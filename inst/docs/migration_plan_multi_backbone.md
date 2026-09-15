# Migration plan — any number of taxonomic backbones

**Status:** Phase 1 applied and verified 2026-09-15. Phase 2 written and tested
on a scratch database 2026-09-15, not deployed (§5.4). Later phases not started.
Written 2026-09-15.
**Why:** `inst/docs/design_note_backbone_interoperability.md`.
**First backbone added this way:** African Plant Database (APD, CJB).

Code references were checked against the repository on 2026-09-15. The APD
facts in §7 come from profiling the export `APD export Gilles.txt` the same
day. Nothing here was checked against the live database — Phase 0 lists the
queries that must be run first.

---

## 0. Goals

**In scope**

- Host any number of backbones. Adding one means adding **data** (a mirror
  table, a view, a `backbone_list` row) and **an importer** — no edits to
  `query_taxa()`, `query_plots()`, the trait functions or the Shiny modules.
- `backbone = "internal"` stays the default everywhere. The user can choose any
  other backbone (`"wcvp"`, `"apd"`) to supply the names; taxa without a link
  keep their internal name.

**Out of scope**

- Specimen and collection identifiers (`specimens.id_tropicos`,
  `specimens.id_brlu`).
- Tropicos. It stays a live search in step 1 of `R/mod_taxa_add.R`; no
  Tropicos ID is stored for taxa.
- Offline mode: the local cache stays internal-only, as today.
- Attaching several backbones' IDs as extra columns
  (`include_backbone_ids`) — not decided; the schema below keeps it possible.

## 1. Target state

| Object | Kind | Role |
|---|---|---|
| `backbone_list` | table | one row per external backbone; `internal` is implicit, never a row |
| `backbone_import` | table | import history per backbone (replaces `wcvp_import_metadata`) |
| `taxa_backbone_link` | table | `idtax_n` → external ID, for every backbone (replaces `wcvp_idtax_link`) |
| `wcvp_names`, `apd_names`, … | tables | one mirror per backbone, in its **native** schema |
| `v_backbone_names_wcvp`, `v_backbone_names_apd`, … | views | the same canonical columns for every mirror; the only thing R code reads |

All in `rainbio`.

## 2. Findings that shape the plan

1. **Every WCVP re-import deletes all links, verified ones included.**
   `import_wcvp_names()` runs `TRUNCATE TABLE wcvp_idtax_link`
   (`R/wcvp_integration.R:290`). Manual and verified links are lost at each
   WCVP version bump. The generic importer must keep links and report the ones
   whose external ID disappeared.
2. **Links are not tied to `table_taxa`.** `wcvp_idtax_link` has a foreign key
   to `wcvp_names` but none on `idtax_n`, and taxon deletion
   (`R/delete_functions.R:12`) does not touch links, so orphans are possible.
3. **Homonym links duplicate rows.** The key `(idtax_n, plant_name_id)` allows
   several links per taxon; `.apply_wcvp_backbone()` joins on `idtax_n` alone.
4. **Synonym resolution reads WCVP's status wording in R.** `"Accepted"` is
   compared literally at `R/wcvp_integration.R:1239`, `:1277` and
   `R/helpers_traits_common.R:129`. APD uses other words (§7.1), and for APD
   the accepted-name pointer is the authority whatever the status says. Each
   view therefore decides where a name points, and R follows pointers without
   reading the status (§4.2).
5. **Synonyms are followed one step only** (`R/wcvp_integration.R:1236-1286`).
   In APD, 1,186 synonym pointers land on a record that is itself not accepted
   (§7.1), so the generic resolver must follow chains.
6. **External IDs are cast to integer everywhere** (`as.integer(plant_name_id)`).
   They must become text.
7. **Installed copies of the package keep using the old tables** (user R
   sessions, the SSP Cloud apps) until they are updated. The schema change
   must be additive first.
8. **No test covers the WCVP code paths.** None of the 70 files in
   `tests/testthat/` mentions WCVP.

## 3. Phase 0 — pre-flight (read-only)

Run on `rainbio` and record the answers in §11 before writing the migration.

```sql
-- Volumes
SELECT count(*) FROM wcvp_names;
SELECT match_type, verified, count(*) FROM wcvp_idtax_link GROUP BY 1, 2;
SELECT * FROM wcvp_import_metadata ORDER BY import_date;

-- Homonym links (finding 3)
SELECT idtax_n, count(*) AS n, sum(verified::int) AS n_verified
  FROM wcvp_idtax_link GROUP BY idtax_n HAVING count(*) > 1;

-- Orphan links (finding 2) — the new foreign key cannot be added while any exist
SELECT count(*) FROM wcvp_idtax_link l
 WHERE NOT EXISTS (SELECT 1 FROM table_taxa t WHERE t.idtax_n = l.idtax_n);

-- Status vocabulary to normalise (finding 4)
SELECT taxon_status, count(*) FROM wcvp_names GROUP BY 1 ORDER BY 2 DESC;

-- Accepted names whose pointer goes elsewhere: today they are not followed,
-- and the WCVP view keeps it that way (§4.2)
SELECT count(*) FROM wcvp_names
 WHERE taxon_status = 'Accepted'
   AND accepted_plant_name_id IS NOT NULL
   AND accepted_plant_name_id <> plant_name_id;

-- Who can write links today — the new table gets the same grants
SELECT grantee, privilege_type FROM information_schema.role_table_grants
 WHERE table_name = 'wcvp_idtax_link';

-- Family name convention, for normalising APD's uppercase families (§7.2)
SELECT tax_fam FROM table_taxa WHERE tax_fam IS NOT NULL LIMIT 20;
```

## 4. Phase 1 — schema migration (additive)

File: `inst/migrations/multi_backbone.R` (written), providing
`migrate_multi_backbone(con_taxa, dry_run = TRUE, on_taxon_delete = "cascade")`
and `check_multi_backbone_migration(con_taxa)`. The dry run also prints the
Phase 0 facts it can gather itself (link counts, homonyms, orphans, WCVP status
values, grants). One transaction. Legacy tables are **not** altered, apart from
two expression indexes on `wcvp_names`.

### 4.1 Tables

```sql
CREATE TABLE backbone_list (
  id_backbone    serial PRIMARY KEY,
  code           text NOT NULL UNIQUE
                 CHECK (code ~ '^[a-z][a-z0-9_]*$' AND code <> 'internal'),
  name           text NOT NULL,
  publisher      text,
  names_view     text NOT NULL,           -- v_backbone_names_<code>
  url_template   text,                    -- '{id}' placeholder; NULL if none
  is_name_source boolean NOT NULL DEFAULT false
);

CREATE TABLE backbone_import (
  id_import      serial PRIMARY KEY,
  id_backbone    integer NOT NULL REFERENCES backbone_list(id_backbone),
  version        text NOT NULL,
  import_date    timestamptz DEFAULT CURRENT_TIMESTAMP,
  imported_by    text,
  record_count   integer,
  source_version text,                    -- e.g. rWCVPdata package version
  is_current     boolean NOT NULL DEFAULT true
);
CREATE UNIQUE INDEX backbone_import_one_current
  ON backbone_import (id_backbone) WHERE is_current;

CREATE TABLE taxa_backbone_link (
  idtax_n      integer NOT NULL
               REFERENCES table_taxa(idtax_n) ON DELETE CASCADE,
  id_backbone  integer NOT NULL REFERENCES backbone_list(id_backbone),
  external_id  text    NOT NULL,
  is_preferred boolean NOT NULL DEFAULT false,
  match_type   varchar(20) NOT NULL,      -- exact | fuzzy | manual
  match_score  numeric(4,3),
  matched_on   timestamptz DEFAULT CURRENT_TIMESTAMP,
  matched_by   varchar(100),
  verified     boolean NOT NULL DEFAULT false,
  notes        text,
  PRIMARY KEY (idtax_n, id_backbone, external_id)
);
CREATE UNIQUE INDEX taxa_backbone_link_one_preferred
  ON taxa_backbone_link (idtax_n, id_backbone) WHERE is_preferred;
CREATE INDEX taxa_backbone_link_external
  ON taxa_backbone_link (id_backbone, external_id);
```

- `is_preferred` settles the homonym case (finding 3): **only the preferred
  link supplies names.** A taxon with several links and none preferred keeps
  its internal name and is reported by `check_backbone_links()`.
- `is_name_source` lets a backbone be imported and matched before users are
  offered it.
- `link_count` is not stored; `get_backbone_status()` counts it.

### 4.2 The canonical view

Every mirror exposes exactly these columns:

| Column | Type | Meaning |
|---|---|---|
| `external_id` | text | the backbone's own name ID |
| `accepted_external_id` | text | the name this one resolves to, as the backbone's policy decides; NULL when the name is its own end point |
| `taxon_name` | text | full name without authors |
| `family`, `genus`, `species` | text | |
| `infra_rank`, `infra_epithet` | text | NULL when not infraspecific |
| `authors` | text | authors of the deepest rank of the name |
| `rank` | text | |
| `status` | text | normalised: `accepted` / `synonym` / `other`; display and reporting only |
| `status_raw` | text | the source's own value, for display |

R rule, shared by every backbone: **when `accepted_external_id` is not NULL,
follow it — whatever `status` says — repeatedly until a name without a pointer
is reached** (findings 4 and 5), stopping at a maximum depth or a cycle. A
chain that ends on a missing ID, a cycle or the depth limit keeps the matched
name and is reported by `check_backbone_links()`.

The policy about which names point where lives in each view, not in R:

- WCVP keeps today's behaviour: an `Accepted` name is never followed, so the
  view nulls its pointer.
- APD takes `idtax_good_n` as the authority whatever `taxon_status` says
  (§7.2), so the view passes it through unchanged.

```sql
CREATE VIEW v_backbone_names_wcvp AS
SELECT plant_name_id::text              AS external_id,
       CASE WHEN taxon_status = 'Accepted'
                 OR accepted_plant_name_id = plant_name_id THEN NULL
            ELSE accepted_plant_name_id::text
       END                              AS accepted_external_id,
       taxon_name, family, genus, species,
       NULLIF(infraspecific_rank, '')   AS infra_rank,
       NULLIF(infraspecies, '')         AS infra_epithet,
       taxon_authors                    AS authors,
       taxon_rank                       AS rank,
       CASE taxon_status
            WHEN 'Accepted' THEN 'accepted'
            WHEN 'Synonym'  THEN 'synonym'
            ELSE 'other' END            AS status,     -- finalise from Phase 0
       taxon_status                     AS status_raw
  FROM wcvp_names;

-- Joins go through ::text; without these the view scans 350k rows per query
CREATE INDEX idx_wcvp_names_id_text       ON wcvp_names ((plant_name_id::text));
CREATE INDEX idx_wcvp_names_accepted_text ON wcvp_names ((accepted_plant_name_id::text));
```

### 4.3 Backfill

```sql
INSERT INTO backbone_list (code, name, publisher, names_view, is_name_source)
VALUES ('wcvp', 'World Checklist of Vascular Plants', 'Royal Botanic Gardens, Kew',
        'v_backbone_names_wcvp', true);

INSERT INTO backbone_import
       (id_backbone, version, import_date, imported_by, record_count,
        source_version, is_current)
SELECT b.id_backbone, m.wcvp_version, m.import_date, m.imported_by,
       m.record_count, m.r_package_version, m.is_current
  FROM wcvp_import_metadata m
  JOIN backbone_list b ON b.code = 'wcvp';

INSERT INTO taxa_backbone_link
       (idtax_n, id_backbone, external_id, is_preferred, match_type,
        match_score, matched_on, matched_by, verified, notes)
SELECT l.idtax_n, b.id_backbone, l.plant_name_id::text,
       -- sole link, or the only verified one among several
       (count(*) OVER w = 1)
         OR (l.verified AND sum(l.verified::int) OVER w = 1),
       l.match_type, l.match_score, l.matched_on, l.matched_by,
       l.verified, l.notes
  FROM wcvp_idtax_link l
  JOIN backbone_list b ON b.code = 'wcvp'
WINDOW w AS (PARTITION BY l.idtax_n);
```

The migration **refuses to run** while orphan links exist (finding 2) and
lists them, rather than letting the foreign key fail.

### 4.4 Grants

`GRANT SELECT … TO public` on the three tables and the view, as
`setup_wcvp_schema()` does today. On `taxa_backbone_link`, repeat the write
grants found on `wcvp_idtax_link` in Phase 0 — `mod_taxa_add.R` saves links
with the app user's connection.

### 4.5 What `check_multi_backbone_migration()` verifies

- link count in `taxa_backbone_link` for `wcvp` equals the count in
  `wcvp_idtax_link`;
- no `(idtax_n, id_backbone)` has two preferred links;
- every `wcvp` external ID resolves in `v_backbone_names_wcvp`;
- exactly one current `backbone_import` row for `wcvp`;
- the homonym links left without a preferred link, listed for review.

## 5. Phase 2 — generic R core

New file `R/backbone_core.R`. An unmigrated database degrades to
internal-only: `list_backbones()` returns no external backbone, following the
configuration error-handling pattern in CLAUDE.md. No legacy read path is kept,
so **apply Phase 1 before merging Phase 2**.

### 5.1 New functions

| Function | Replaces | Notes |
|---|---|---|
| `list_backbones(con_taxa, name_sources_only = TRUE)` | — | exported; reads `backbone_list` |
| `.validate_backbone(backbone, con_taxa)` | 11 × `match.arg(c("internal","wcvp"))` | `"internal"` returns immediately, without a query; unknown or disabled codes abort, listing the valid ones |
| `get_backbone_names(idtax_n, backbone, con_taxa, resolve_synonyms = TRUE)` | `get_wcvp_names()` | preferred links only; follows chains (§4.2); returns `backbone_name_id`, `backbone_accepted_id`, `backbone_taxon_name`, `backbone_family`, `backbone_genus`, `backbone_species`, `backbone_authors`, `backbone_status`, `name_source` |
| `.apply_backbone(data, info, id_col = "idtax_n")` | `.apply_wcvp_backbone()` | same overwrite rules; see §5.3 |
| `.resolve_synonyms_backbone(idtax, include_synonyms, con_taxa, backbone)` | `.resolve_synonyms_wcvp()` | same fallback to internal; follows chains |
| `match_taxa_to_backbone(backbone, con_taxa, …)` | `match_taxa_to_wcvp()` | reads the view; matching helpers renamed `.backbone_match_*`, `ipni_id` no longer required |
| `save_backbone_links(matches, backbone, con_taxa, replace = TRUE)` | `save_wcvp_links()` | `replace` removes only that backbone's links; a sole link is saved preferred |
| `get_backbone_status(backbone, con_taxa)` | `get_wcvp_status()` | reads `backbone_import`, counts links |
| `check_backbone_links(backbone, con_taxa)` | — | homonyms without a preferred link, external IDs missing from the view, unresolved synonym chains, unverified fuzzy links |

### 5.2 Existing functions

| Function | Change |
|---|---|
| `import_wcvp_names()` | stays WCVP-specific; **no longer truncates links**; writes `backbone_import`; ends with `check_backbone_links("wcvp")` |
| `check_wcvp_update()` | stays WCVP-specific (uses `rWCVP`); reads `backbone_import` |
| `get_wcvp_names()`, `match_taxa_to_wcvp()`, `save_wcvp_links()`, `get_wcvp_status()` | thin wrappers over the generic functions, with a deprecation warning, like `query_traits_measures()`; `get_wcvp_names()` keeps its `wcvp_*` columns |
| `setup_wcvp_schema()` | deprecated — the schema now lives in the migration |

The 11 selector signatures (design note §3.1) change the same way:

```r
# before
query_taxa <- function(..., backbone = c("internal", "wcvp")) {
  backbone <- match.arg(backbone)

# after
query_taxa <- function(..., backbone = "internal") {
  backbone <- .validate_backbone(backbone, mydb_taxa)
```

The `@param backbone` text is written once in `query_taxa()` and reused with
`@inheritParams`. The branches at `R/taxonomic_query_functions.R:256` and
`:1118` and `R/helpers_traits_common.R:29` become
`if (backbone != "internal")`.

### 5.3 Output contract

| Column | Today | After | Alias kept for one release |
|---|---|---|---|
| `name_source` | `"internal"` / `"wcvp"` | backbone code (`"internal"`, `"wcvp"`, `"apd"`) | — (existing values unchanged) |
| ID of the matched name | `wcvp_plant_name_id` (integer) | `backbone_name_id` (text) | `wcvp_plant_name_id` (integer), only when `backbone = "wcvp"` |
| ID of the accepted name | `wcvp_accepted_plant_name_id` (integer) | `backbone_accepted_id` (text) | `wcvp_accepted_plant_name_id`, only when `backbone = "wcvp"` |
| `alt_taxon_name` | internal name | unchanged | — |

Add the new column names to the globals in `R/utils.R`.

### 5.4 As implemented (2026-09-15)

New file `R/backbone_core.R`; wrappers in `R/wcvp_integration.R`. Differences
from §5.1–5.3:

- **No deprecation warnings yet** on `get_wcvp_names()`,
  `match_taxa_to_wcvp()`, `save_wcvp_links()` and `get_wcvp_status()`: the
  Shiny modules still call them until Phase 3. Their documentation says they
  are superseded.
- `get_backbone_names()` also returns `backbone_status_raw`, which
  `get_wcvp_names()` needs to keep WCVP's own status wording.
- The matching helpers keep their `.wcvp_match_*` names. The backbone view is
  fetched under WCVP's column names, and IDs are text inside the helpers.
- `import_wcvp_names()` still runs `TRUNCATE wcvp_names CASCADE`. That empties
  the legacy `wcvp_idtax_link` but not `taxa_backbone_link`, which has no
  foreign key to `wcvp_names`. It now writes `backbone_import` and ends with
  `check_backbone_links("wcvp")`.
- `setup_wcvp_schema()` is not deprecated; its documentation says to apply the
  migration after it on a new database.
- `.check_wcvp_synonymy_candidates()` (`R/mod_taxa_add.R`) reads
  `taxa_backbone_link`. It was the only Shiny code querying the legacy link
  table directly.

Two existing bugs fixed along the way:

- `query_taxa(ids = )` ignored `backbone`, and so did `match_tax()`, which calls
  it that way: both returned internal names whatever was asked.
- `author_match = "fuzzy"` never worked: the author column was lost before the
  similarity step, the error was caught, and the run returned no exact match.

Tested: 17 unit tests (`tests/testthat/test-backbone-core.R`); 37 integration
checks against a scratch PostgreSQL 17 database built with the migrations
(chains, a cycle, APD's pointer rule, homonyms, wrappers,
`add_taxa_table_taxa()`, `query_taxa(ids = )`, synonym resolution, matching,
saving, status, the follow-up script); the existing taxonomy test files still
pass.

**Before deploying:** run `sync_legacy_wcvp_links()` from
`inst/migrations/multi_backbone_followup.R`, with `mirror_deletions = TRUE`
only at that moment, then deploy (SSP Cloud apps included) straight after.

## 6. Phase 3 — Shiny modules

| File | Change |
|---|---|
| `R/shiny_app_taxonomic_match.R:532-572, 600-621` | WCVP checkbox → "Names in output" selector: Internal (default) plus each `list_backbones()`; hidden when there is none; `use_wcvp_names` → `output_backbone` (a string) |
| `R/mod_auto_matching.R:113, 211, 287-330, 725` | takes `output_backbone`; calls `get_backbone_names()` |
| `R/mod_taxo_match_r_code.R:49, 90, 273-290, 335` | generated code calls `get_backbone_names(backbone = "<code>")` |
| `R/mod_results_export.R:210-213` | column descriptions name the chosen backbone instead of saying WCVP |
| `R/mod_taxa_add.R` | Tropicos search unchanged. `.search_wcvp_backbone()` (l.2076) → `.search_backbone(name, backbone, con)` on the view; `.check_wcvp_synonymy_candidates()` (l.1993) → generic; `rv$wcvp_*` → `rv$backbone_*`; link saved with `save_backbone_links()` (l.1254-1280); a picker for which backbone to search |
| `R/mod_taxa_search.R:600-735` | lists the taxon's link in every backbone, one line each, linked through `url_template` when set |
| `inst/translations/translation.json` | new strings added with the duplicate-checking pattern; WCVP strings removed once unused |

## 7. Phase 4 — APD

### 7.1 What the export contains

Profiled 2026-09-15 from `APD export Gilles.txt`.

**Format**
- 104,117 records, 18 columns, tab-separated, quoted.
- **Encoded Latin-1, not UTF-8** ("Zürich", "Aubrév." are garbled when read
  as UTF-8).
- `ID`: unique integers, 2 to 248,096.
- The export carries **no version tag**.

**Ranks (`tax_level`)**

| species | varietas | genus | subspecies | forma | familia | subvarietas | cultivar | sectio | classis | divisio | regnum | subgenus |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 80,589 | 11,582 | 6,208 | 4,616 | 717 | 306 | 74 | 7 | 6 | 5 | 5 | 1 | 1 |

**Status (`taxon_status`)**

| Value | n |
|---|---|
| `Synonyme` | 55,095 |
| `Accepted` | 42,768 |
| `illeg. name` | 1,593 |
| `invalid name (nom. nud.)` | 1,263 |
| `excl. Name` / `excl. name` | 1,252 / 13 |
| `uncertain` | 1,101 |
| `invalid name` | 652 |
| `nom. nov.` | 150 |
| NULL | 116 |
| `needs further study` | 67 |
| `superfl. name`, `nom.dub.`, `invalid name (ined.)`, `nom.confus.`, `comb. nov. (in press, 2009)` | 21, 12, 7, 5, 2 |

`STATUT_SYN` is a second status code. `Accepted` corresponds exactly to `A`
(39,713) or `A*` (3,055). The other codes are `R` (58,640), NULL (1,565),
`S*` (792), `Z` (344), `R*` (7) and `r` (1). Meanings, from spot checks
against the APD website: `S*` unused autonym, `Z` unresolved, `R*` a synonym;
`A*` is accepted, but how it differs from `A` is unknown. The column adds
nothing to `taxon_status` and `idtax_good_n`, so it is **not imported**.

**Synonymy (`idtax_good_n` — APD's own accepted-name pointer, not ours)**
- 58,766 records have a pointer.
- 326 point to an ID absent from the export.
- 1,186 point to a record that itself has a pointer (chains — finding 5).
- 39 `Accepted` records have a pointer.
- 2,161 `Synonyme` records have none.

Decision (2026-09-15): **`idtax_good_n` is the authority, whatever
`taxon_status` says.** A name with a pointer is followed, even when marked
`Accepted`; a name without one is its own end point, even when marked
`Synonyme`. Why the two disagree in these 2,200 records is not known.

Exports are sent by Cyrille Chatelain, the APD manager.

**Hierarchy (`id_PARENT`)**
- Present on 104,115 records; 1,145 point outside the export.
- Infraspecific → species → genus → familia → classis/divisio → regnum.

**Name fields**
- `taxon_name` never includes authors; `nom_standard` is the name with
  authors.
- `author1` is the species author and `author2` the infraspecific author — the
  same convention as `table_taxa`.
- `taxrank` holds rank and epithet together (`var. abbreviata`); autonyms
  carry the rank alone (`var. `); cultivars read `cv. Sofia`.
- In some forma records `tax_esp` holds rank and epithet too
  (`prostrata f. pedicellata`).
- `fk_famille` is the family **name in uppercase** (`AMARANTHACEAE`), not an
  ID.
- Above genus, `taxon_name` carries a rank prefix (`regn. Plantae`,
  `cla. Liliopsida`, `div. Musci`). One genus record is named `sp.`
  (ID 187654).
- 2,074 `taxon_name` values occur more than once, typically an accepted name
  and an `auct.` misapplication of the same string (`Abelmoschus manihot
  (L.) Medik.`, Accepted / `Abelmoschus manihot auct.`, `excl. Name`).

### 7.2 What that means for the import

1. **Read as Latin-1, store as UTF-8.**
2. **Rename the three ID columns** (`ID` → `apd_id`, `idtax_good_n` →
   `apd_accepted_id`, `id_PARENT` → `apd_parent_id`), so that no column in
   `rainbio` named `idtax_good_n` holds a non-internal ID.
3. **Derive the canonical fields at import**, keeping the raw columns:
   - `family`: `fk_famille` in the case used by `table_taxa.tax_fam` (Phase 0);
   - `species`: first word of `tax_esp`;
   - `infra_rank`, `infra_epithet`: split from `taxrank`, the epithet of an
     autonym being the species epithet;
   - `authors`: `author2` for infraspecific names, `author1` otherwise;
   - `taxon_name`: rank prefix removed above genus.
4. **Version = the date the export file was created** (`YYYY-MM-DD`), since
   the file carries no version tag. Pass it explicitly: the creation time the
   file system reports is reset whenever the file is copied, so it cannot be
   read back reliably. Default to the file's last-modified date, which survives
   copies, and print it so the importer confirms it.
5. **Pointers kept as they are.** `apd_accepted_id` is imported raw and the
   view passes it through (§4.2); `STATUT_SYN` is dropped (§7.1).
6. **Match with authors** (`author_match = "fuzzy"`). A name string alone
   cannot tell an accepted name from its `auct.` homonym; an automatic link to
   an `auct.` record is never marked preferred.
7. **Keep the dump outside the package.** Files under `inst/` are bundled into
   every build; `import_apd_names()` takes a path.

### 7.3 Steps

1. `inst/migrations/apd_backbone.R` (written): `migrate_apd_backbone(con_taxa,
   dry_run = TRUE)` creates `apd_names`, its indexes, `v_backbone_names_apd`,
   and the `backbone_list` row with `is_name_source = false`;
   `check_apd_backbone_migration(con_taxa)` verifies it and reports import,
   links and availability.

   ```sql
   CREATE TABLE apd_names (
     apd_id            integer PRIMARY KEY,
     apd_accepted_id   integer,               -- no FK: 326 dangling in the export
     apd_parent_id     integer,
     taxon_name        text NOT NULL,
     nom_standard      text,
     tax_level         text,
     taxrank           text,
     tax_famclass      text,
     fk_famille        text,
     tax_gen           text,
     tax_esp           text,
     author1           text,
     author2           text,
     taxon_status      text,
     citation          text,
     year_description  integer,
     date_modification timestamp,
     -- derived at import (§7.2)
     family            text,
     species           text,
     infra_rank        text,
     infra_epithet     text,
     authors           text,
     apd_version       text NOT NULL          -- export file date, YYYY-MM-DD
   );
   CREATE INDEX idx_apd_names_id_text       ON apd_names ((apd_id::text));
   CREATE INDEX idx_apd_names_accepted_text ON apd_names ((apd_accepted_id::text));
   CREATE INDEX idx_apd_names_taxon_name    ON apd_names (taxon_name);
   CREATE INDEX idx_apd_names_genus         ON apd_names (tax_gen);

   CREATE VIEW v_backbone_names_apd AS
   SELECT apd_id::text          AS external_id,
          apd_accepted_id::text AS accepted_external_id,  -- authority, whatever the status
          taxon_name, family,
          tax_gen               AS genus,
          species, infra_rank, infra_epithet, authors,
          tax_level             AS rank,
          CASE taxon_status
               WHEN 'Accepted' THEN 'accepted'
               WHEN 'Synonyme' THEN 'synonym'
               ELSE 'other' END AS status,
          taxon_status          AS status_raw
     FROM apd_names;
   ```

2. `import_apd_names(file, version = <file last-modified date>, con_taxa,
   dry_run = TRUE)` in `R/`, since
   it is re-run at each APD export: loads the dump, applies §7.2, writes
   `backbone_import`, leaves links alone, then runs
   `check_backbone_links("apd")`.
3. `match_taxa_to_backbone("apd", author_match = "fuzzy")` → review →
   `save_backbone_links()`.
4. `UPDATE backbone_list SET is_name_source = true WHERE code = 'apd'`.

**Adding any later backbone is the same four steps.** That is the test of the
refactor: if a fourth backbone needs an edit outside a new migration and a new
importer, the generalisation is incomplete.

## 8. Phase 5 — cleanup (next release)

- Rename `wcvp_idtax_link` and `wcvp_import_metadata` to `*_archived`; drop
  them one release later.
- Remove the `wcvp_*` alias columns (breaking — announce in NEWS.md).
- Optional, separate: `backbone` in `match_taxonomic_names()`
  (`R/taxonomic_matching.R:389`) and `standardize_taxonomic_batch()` (l.1819)
  is the cached internal table, not a selector. Rename it `backbone_data` and
  keep `backbone` as a deprecated alias. These are exported arguments, so this
  is breaking and stays out of this migration.

## 9. Tests

- `test-backbone-apply.R` — `.apply_backbone()` on plain data frames: columns
  overwritten only for linked rows; `alt_taxon_name`; `name_source` per row;
  missing input columns tolerated; `wcvp_*` aliases present only for `"wcvp"`;
  row count unchanged.
- `test-backbone-validate.R` — `"internal"` accepted without a connection;
  unknown or disabled code aborts with the valid list.
- `test-backbone-synonyms.R` — accepted maps to itself; a synonym whose
  accepted name is linked maps to that `idtax_n`; a two-step chain is followed;
  a pointer is followed even on a name marked accepted, and a name marked
  synonym without a pointer is its own end point; a cycle and a dangling
  pointer stop and keep the matched name; otherwise falls back to internal.
- The WCVP view: an `Accepted` name gets no pointer, so today's WCVP results
  are unchanged.
- `test-apd-import.R` — the §7.2 derivations on a few rows taken from the
  export: Latin-1 authors, an autonym, a forma with a polluted `tax_esp`, a
  cultivar, a prefixed higher rank, an `auct.` homonym.
- Status normalisation: every value found in Phase 0 and §7.1 maps to
  `accepted` / `synonym` / `other`.
- Database-level: `check_multi_backbone_migration()`, on a restored copy
  before production.

## 10. Order and compatibility

| Step | Deployable alone | Effect on installed older versions |
|---|---|---|
| 1. Migration | yes | none — legacy tables untouched |
| 2. R core | after 1 | new links go only to `taxa_backbone_link`; older versions stop seeing them → update the SSP Cloud apps right after merging |
| 3. Shiny | with 2 | — |
| 4. APD | after 1-3 | — |
| 5. Cleanup | once every client is updated | older versions lose WCVP names |

Documentation moving with the code: `inst/docs/taxonomic_backbones.md` and
`_fr.md` (rewrite for any number of backbones); `vignettes/taxonomic-app.Rmd`
and `-fr`; `vignettes/using-query-plots.Rmd` and `-fr`; `_pkgdown.yml`
reference; `inst/migrations/README.md` status row once applied; the design
note marked implemented. The newsletters are historical and stay as they are.

## 11. Open points

To answer before writing the migrations:

1. Phase 0 results. Known since Phase 1 (2026-09-15): 313,476 WCVP links, no
   orphan, one current import, 1,550 taxa with several links and none
   preferred. Still to read from the dry-run report: WCVP status values,
   accepted names pointing elsewhere, grants. Still to query: `tax_fam`
   capitalisation.
1b. **Until Phase 2 is deployed, links saved by the current package go only to
   `wcvp_idtax_link`.** `check_multi_backbone_migration()` will then report
   legacy links absent from `taxa_backbone_link`; copy them across before
   Phase 2 goes live.
1c. The 1,550 taxa with several links and none preferred keep their internal
   name under Phase 2 until a preferred link is chosen.

For Cyrille Chatelain (APD), about the export:

2. The 326 pointers and 1,145 parents outside the export: is the export
   filtered (by region, by group), and should the missing targets be included?
3. Whether exports can be produced in UTF-8.

Settled 2026-09-15:

- `STATUT_SYN` is not imported (§7.1).
- `idtax_good_n` is the authority for APD synonymy, whatever `taxon_status`
  says (§7.1).
- APD versions are the export file's creation date (§7.2).
- Keeping a full copy of APD in `rainbio` is allowed.

Decisions — recommendation first:

4. `ON DELETE CASCADE` on `taxa_backbone_link.idtax_n` (recommended: a link has
   no meaning without its taxon) vs `RESTRICT` — the migration's
   `on_taxon_delete` argument, defaulting to `"cascade"`.
5. Homonym links with no single verified link stay non-preferred, so the taxon
   keeps its internal name until reviewed (recommended) vs choosing the highest
   `match_score`.
6. Maximum depth when following synonym chains (5 suggested).
7. `include_backbone_ids` — carried over from the design note, not decided.
