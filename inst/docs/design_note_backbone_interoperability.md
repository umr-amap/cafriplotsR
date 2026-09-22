# Design note — multi-backbone interoperability for taxa (NOT IMPLEMENTED)

**Status:** reopened 2026-09-14. First discussion 2026-08-05, no code written.
**Trigger reached:** the third backbone is the **African Plant Database (APD)**,
maintained by the Conservatoire et Jardin botaniques de Genève (CJB). Do the
refactor *before* importing APD, not after.

**Related:** `inst/docs/taxonomic_backbones.md` (describes the current
two-backbone system).

---

## 1. Conclusion in one line

The way the WCVP backbone is stored is right and should be kept: a full copy
of the backbone in `rainbio`, plus a link table that records how each internal
taxon was matched. What doesn't scale is the code, which names WCVP's tables
and columns directly. Adding APD would mean copying all of it and editing 11
functions, and again for every later backbone. Rewrite that code once so it
works for any backbone, then add APD as data.

Implementation plan: `inst/docs/migration_plan_multi_backbone.md`.

## 2. Why the concept is right

`wcvp_idtax_link` is more than an external-ID table:

- the other backbone's **full content is mirrored** (`wcvp_names`, ~350k rows),
  which is necessary — re-expressing our taxa in another backbone needs its
  names, authors, status and synonymy, not just a pointer;
- the bridge carries **match provenance**: `match_type`, `match_score`,
  `verified`, `matched_by`, `matched_on`, `notes`;
- imports are **versioned**: `wcvp_import_metadata.is_current`.

A generalised `wcvp_idtax_link` is therefore the link table for every
backbone.

### Scope

- **Taxa only.** `specimens.id_tropicos` and `specimens.id_brlu` identify
  collections/specimens (keyed on `id_specimen`), not taxa.
  `specimens.id_tropicos` is the Tropicos *collection* ID, one per gathering
  event. They must never become rows in the taxa link table. External IDs for
  specimens are a separate design.
- **Tropicos is not a backbone.** It is only queried live, through `taxize`,
  in step 1 of `R/mod_taxa_add.R` to pre-fill a taxon being added. No Tropicos
  ID is stored for taxa, and none will be: a partial set of Tropicos IDs would
  be of little use.

## 3. Why it will not scale as-is

Inventory re-checked against the code on 2026-09-14.

### 3.1 Backbone selector — `c("internal", "wcvp")` + `match.arg()`

11 signatures in 6 files:

| File | Functions |
|---|---|
| `R/taxonomic_query_functions.R` | `query_taxa()` (l.69), `match_tax()` (l.735), `add_taxa_table_taxa()` (l.1074) |
| `R/functions_manip_db.R` | `query_plots()` (l.250), `.query_plots_impl()` (l.355), `process_individuals()` (l.1042) |
| `R/individual_features_function.R` | `query_individual_features()` (l.1190), `fetch_linked_individuals()` (l.2031) |
| `R/taxa_traits_function.R` | `query_taxa_traits()` (l.146); `enrich_with_taxa_info()` (l.378) passes it through |
| `R/helpers_traits_common.R` | `resolve_taxon_synonyms()` (l.24) |
| `R/taxonomic_update_functions_old.R` | `merge_individuals_taxa()` (l.39) |

### 3.2 WCVP table and column names hardcoded in SQL / R

| Where | What |
|---|---|
| `R/wcvp_integration.R:1174` | `get_wcvp_names()`: `FROM wcvp_idtax_link l JOIN wcvp_names w`, emits `wcvp_*` columns |
| `R/wcvp_integration.R:1321` | `.apply_wcvp_backbone()`: WCVP→internal column mapping, keeps `wcvp_plant_name_id` / `wcvp_accepted_plant_name_id` in user-facing results |
| `R/helpers_traits_common.R:84` | `.resolve_synonyms_wcvp()`: resolves synonymy through the backbone, then back to `idtax_n` via a second join on the link table — the generic version needs `accepted_external_id` in every mirror view |
| `R/wcvp_integration.R` | per-backbone `setup_*`, `import_*`, `match_taxa_to_*`, `save_*_links`, `get_*_status`, `check_*_update` |

### 3.3 Shiny modules

- `R/mod_taxa_add.R` (~60 references): searches `wcvp_names`
  (`.search_wcvp_backbone()`), finds synonymy candidates
  (`.check_wcvp_synonymy_candidates()`), calls `save_wcvp_links()` when a taxon
  is created.
- `R/mod_auto_matching.R`, `R/mod_taxa_search.R`,
  `R/shiny_app_taxonomic_match.R` (`use_wcvp_names` checkbox),
  `R/mod_taxo_match_r_code.R`, `R/mod_results_export.R` (column descriptions),
  `R/utils.R` (`wcvp_*` globals).

### 3.4 Size of the job

`backbone` appears 384 times across 33 files, but ~175 of those
(`taxonomic_matching.R`, `taxonomic_matching_pipeline.R`, `cache_backbone.R`)
are the **cached internal-backbone tibble** used by name matching, not the
selector. The real surface is §3.1–3.3.

**Naming collision to fix while we're there:** in `match_taxa_names()`
(`R/taxonomic_matching.R`) `backbone` is a *cached tibble of the internal
backbone*; in `query_taxa()` it is a *backbone selector string*. Survivable
with two backbones, not with four.

### 3.5 Latent bug: homonym links duplicate rows

The link PK `(idtax_n, plant_name_id)` allows one internal taxon to link to
several WCVP names, but `.apply_wcvp_backbone()` joins on `idtax_n` alone, so
such a taxon comes back as duplicate rows. Found by reading the code; not yet
checked whether such links exist. The generic bridge must define which link
wins per `(idtax_n, id_backbone)` (e.g. a single `verified` / preferred link).

## 4. Proposed design

### 4.1 One bridge table for all backbones (in `rainbio`)

```sql
CREATE TABLE backbone_list (
  id_backbone  serial PRIMARY KEY,
  code         text NOT NULL UNIQUE,   -- 'wcvp', 'apd', later others
  name         text,
  version      text,
  names_table  text NOT NULL,          -- mirror table
  id_column    text,
  url_template text,                   -- resolvable links
  is_current   boolean
);

CREATE TABLE taxa_backbone_link (
  idtax_n      integer NOT NULL REFERENCES table_taxa(idtax_n),
  id_backbone  integer NOT NULL REFERENCES backbone_list(id_backbone),
  external_id  text    NOT NULL,       -- WCVP plant_name_id, APD id…
  match_type   varchar,
  match_score  numeric(4,3),
  matched_on   timestamp,
  matched_by   varchar,
  verified     boolean,
  notes        text,
  PRIMARY KEY (idtax_n, id_backbone, external_id)
);
```

Migration from the existing state is one `INSERT ... SELECT` out of
`wcvp_idtax_link` with `id_backbone = <wcvp>`.

`external_id` is **text**, not integer — backbones other than WCVP and APD may
not use numeric IDs.

The plan refines these tables (preferred link, import history, a flag to
enable a backbone after review): see the implementation plan.

### 4.2 Keep per-backbone `*_names` mirror tables

Their schemas genuinely differ (WCVP has `geographic_area`,
`lifeform_description`; APD has publication citations and its own status
vocabulary). Do **not** force a common schema. Instead give each mirror a
**normalised SQL view** exposing only the canonical fields the code consumes:

```
external_id, accepted_external_id, name, family, genus, species,
infra_rank, infra_epithet, authors, status
```

`get_wcvp_names()` + `.apply_wcvp_backbone()` then collapse into one generic
`get_backbone_names()` / `.apply_backbone()` reading the view. Per new
backbone: an importer, a matcher config, a view — not 1500 lines. APD is the
first backbone to be added this way.

### 4.3 Generic output columns

`wcvp_plant_name_id` / `wcvp_accepted_plant_name_id` →
`backbone_name_id` / `backbone_accepted_id` / `backbone_status`.
`name_source` already generalises (holds `"wcvp"` / `"internal"`, later
`"apd"`).

This is the one genuinely **breaking** change — keep `wcvp_*` aliases for one
release; `get_wcvp_names()` stays as a thin wrapper.

### 4.4 One validator

Replace every `match.arg(c("internal","wcvp"))` with a single
`.validate_backbone(backbone, con_taxa)` reading `backbone_list`.

## 5. Decision (settled 2026-09-14)

**`backbone` stays exclusive and user-selectable; the internal backbone wins
by default.**

- `backbone = "internal"` remains the default everywhere.
- The user may choose any mirrored backbone (`"wcvp"`, `"apd"`) to supply the
  names instead; taxa without a link fall back to internal
  (`name_source = "internal"`).

Not decided: attaching *other* backbones' IDs as extra columns alongside the
winning names (e.g. `include_backbone_ids = c("wcvp", "apd")`). The single
bridge table keeps this cheap to add later without changing the `backbone =`
contract.

## 6. Effort / risk

- Mostly mechanical: §3.1–3.3 (11 selector signatures, ~4 core functions,
  ~7 Shiny/support files).
- Write access to `rainbio`: available.
- Breaking change limited to the `wcvp_*` output column names (aliasable).
- Suggested order:
  1. schema migration (`backbone_list`, `taxa_backbone_link`, WCVP view,
     WCVP link backfill);
  2. generic R core + validator, WCVP aliases, fix §3.5;
  3. Shiny modules (backbone selector instead of the WCVP checkbox);
  4. APD importer, mirror table + view, matcher.
