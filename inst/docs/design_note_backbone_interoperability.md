# Design note — multi-backbone interoperability for taxa (ARCHIVED / NOT IMPLEMENTED)

**Status:** parked. Discussion held 2026-08-05, no code written.
**Trigger to revisit:** when a third taxonomic backbone (POWO, IPNI, WFO, GBIF…)
is about to be added. Do the refactor *before* adding it, not after.

**Related:** `inst/docs/taxonomic_backbones.md` (describes the current
two-backbone system), and the parallel discussion on specimen external IDs
(Tropicos collection ID vs sheet-level SpecimenID).

---

## 1. Conclusion in one line

The existing WCVP system is conceptually right — a *mirror + bridge*, not an
ID store — but its plumbing is hardcoded per backbone and will not survive a
third one. Generalise the plumbing, keep the concept.

## 2. Why the concept is right

`wcvp_idtax_link` is more than an external-ID table:

- the other backbone's **full content is mirrored** (`wcvp_names`, ~350k rows),
  which is necessary — re-expressing our taxa in another backbone needs its
  names, authors, status and synonymy, not just a pointer;
- the bridge carries **match provenance**: `match_type`, `match_score`,
  `verified`, `matched_by`, `matched_on`, `notes`;
- imports are **versioned**: `wcvp_import_metadata.is_current`.

Note the convergence with the specimen work: a generalised
`wcvp_idtax_link` **is** the external-ID table for taxa. `table_taxa.id_tropicos`
and `table_taxa.id_brlu` would become rows in it, gaining the provenance
columns they currently lack — so no separate `taxa_external_ids` table is
needed.

## 3. Why it will not scale as-is

Everything is hardcoded per backbone:

| Where | What |
|---|---|
| `R/taxonomic_query_functions.R:69` | `backbone = c("internal", "wcvp")` + `match.arg()` in `query_taxa()` |
| `R/taxonomic_query_functions.R:735` | same in `match_tax()` |
| `R/taxonomic_query_functions.R:1074` | same in `add_taxa_table_taxa()` |
| also | `taxa_traits_function.R`, `individual_features_function.R`, `helpers_traits_common.R`, `aggregate_individual_traits.R`, several Shiny modules |
| `R/wcvp_integration.R:1229-1231` | table names as SQL literals: `FROM wcvp_idtax_link l JOIN wcvp_names w` |
| `R/wcvp_integration.R:1321` | `.apply_wcvp_backbone()` hardcodes the WCVP→internal column mapping and emits `wcvp_*` columns into user-facing results |

284 occurrences of `backbone` across 28 files. `wcvp_integration.R` is 1503
lines; the copy-paste route duplicates `setup_*`, `import_*`,
`match_taxa_to_*`, `save_*_links`, `get_*_names`, `.apply_*_backbone` per
backbone **and** edits the `match.arg` in 28 files each time.

**Naming collision to fix while we're there:** in `match_taxa_names()`
(`R/taxonomic_matching.R:224`) `backbone` is a *cached tibble of the internal
backbone*; in `query_taxa()` it is a *backbone selector string*. Survivable
with two backbones, not with four.

## 4. Proposed design

### 4.1 One bridge table for all backbones (in `rainbio`)

```sql
CREATE TABLE backbone_list (
  id_backbone  serial PRIMARY KEY,
  code         text NOT NULL UNIQUE,   -- 'wcvp','powo','ipni','wfo','gbif','tropicos'
  name         text,
  version      text,
  names_table  text,                   -- mirror table, NULL if not mirrored
  id_column    text,
  url_template text,                   -- resolvable links
  is_current   boolean
);

CREATE TABLE taxa_backbone_link (
  idtax_n      integer NOT NULL REFERENCES table_taxa(idtax_n),
  id_backbone  integer NOT NULL REFERENCES backbone_list(id_backbone),
  external_id  text    NOT NULL,       -- plant_name_id, IPNI id, GBIF usageKey…
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

`external_id` is **text**, not integer — IPNI ids, GBIF keys and DOIs are not
numeric.

### 4.2 Keep per-backbone `*_names` mirror tables

Their schemas genuinely differ (WCVP has `geographic_area`,
`lifeform_description`; IPNI has publication data). Do **not** force a common
schema. Instead give each mirror a **normalised SQL view** exposing only the
canonical fields the code consumes:

```
external_id, accepted_external_id, name, family, genus, species,
infra_rank, infra_epithet, authors, status
```

`get_wcvp_names()` + `.apply_wcvp_backbone()` then collapse into one generic
`get_backbone_names()` / `.apply_backbone()` reading the view. Per new
backbone: an importer, a matcher config, a view — not 1500 lines.

### 4.3 Generic output columns

`wcvp_plant_name_id` / `wcvp_accepted_plant_name_id` →
`backbone_name_id` / `backbone_accepted_id` / `backbone_status`.
`name_source` already generalises (holds `"wcvp"` / `"internal"`).

This is the one genuinely **breaking** change — keep `wcvp_*` aliases for one
release.

### 4.4 One validator

Replace every `match.arg(c("internal","wcvp"))` with a single
`.validate_backbone(backbone, con_taxa)` reading `backbone_list`.

## 5. Open decision to settle BEFORE implementing

**Is `backbone` exclusive, or can several apply at once?**

Today it is exclusive: one backbone overwrites the name columns. With four
backbones users will plausibly want *WCVP names* plus *POWO and GBIF IDs
attached*.

Recommended: `backbone =` keeps meaning "whose names win", and a new
`include_backbone_ids = c("powo","gbif")` means "attach these as columns".
The single bridge table makes this trivial — but deciding it *after* the
refactor means redoing the output contract a second time.

## 6. Effort / risk

- Mostly mechanical but **wide**: 28 files plus the Shiny modules.
- Needs **write access to `rainbio`** (read-only for most users).
- Breaking change limited to the `wcvp_*` output column names (aliasable).
- Next step when resumed: read every `backbone` call site and produce a
  concrete migration + refactor plan before writing code.
