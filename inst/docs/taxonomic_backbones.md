# Taxonomic Backbones in CafriplotsR

This document describes the two taxonomic backbones available in the package, their
database structures, and how they are linked together.

---

## Overview

The package maintains an **internal backbone** built from field inventories and
taxonomic revisions specific to Central African flora, stored in the `rainbio`
PostgreSQL database. Since version X.Y it can optionally be enriched with the
**WCVP backbone** (World Checklist of Vascular Plants), imported from the
`rWCVPdata` R package and stored in the same database alongside a bridge table.

The function `query_taxa()` exposes both via its `backbone` argument:

```r
# Default — internal backbone only
query_taxa(genus = "Piptadeniastrum")

# WCVP-enriched — overwrites taxonomy columns with WCVP values where a link exists
query_taxa(genus = "Piptadeniastrum", backbone = "wcvp")
```

---

## 1. Internal backbone — `table_taxa`

**Database:** `rainbio`

Each row represents one taxon entry at any rank (class → infraspecific).
Synonymy and hierarchy are encoded with two self-referential foreign keys.

### Key columns

| Column | Type | Description |
|---|---|---|
| `idtax_n` | integer PK | Unique internal taxon ID |
| `idtax_good_n` | integer FK → `idtax_n` | `NULL` = this is an accepted taxon; non-NULL = this is a synonym pointing to its accepted name |
| `id_parent` | integer FK → `idtax_n` | Parent node in the taxonomic tree (used for recursive hierarchy traversal) |
| `tax_level` | varchar | Rank: `"higher"` / `"class"`, `"order"`, `"family"`, `"genus"`, `"species"`, `"infraspecific"` |
| `tax_famclass` | varchar | Class name |
| `tax_order` | varchar | Order name |
| `tax_fam` | varchar | Family name |
| `tax_gen` | varchar | Genus name |
| `tax_esp` | varchar | Species epithet |
| `tax_rank01` / `tax_nam01` | varchar | First infraspecific rank and epithet (e.g., `"var."` / `"thollonii"`) |
| `tax_rank02` / `tax_nam02` | varchar | Second infraspecific rank and epithet |
| `author1` | varchar | Author of the species epithet (basionym) |
| `author2` | varchar | Author of the first infraspecific taxon |
| `author3` | varchar | Author of the second infraspecific taxon |
| `morpho_species` | boolean | Is this a morphospecies? |
| `id_tax_famclass` | integer FK | → `table_tax_famclass` (class/division lookup) |
| `tax_source` | varchar | Data provenance / source reference |
| `introduced_status` | varchar | Introduction status (native, introduced, etc.) |
| `citation` | varchar | Bibliographic citation |
| `year_description` | integer | Year of taxonomic description |
| `data_modif_d/m/y` | integer | Last modification date (day / month / year) |

### Synonymy

```
idtax_good_n IS NULL  →  accepted taxon
idtax_good_n = 1234   →  synonym; the accepted name has idtax_n = 1234
```

### Hierarchy

Each entry links to its parent via `id_parent`. Recursive CTEs walk up
(ancestors) or down (descendants):

```
class  ←  order  ←  family  ←  genus  ←  species  ←  infraspecific
```

Functions: `get_taxon_children()`, `get_taxon_ancestors()`, `get_taxon_hierarchy()`.

### Full name construction

The name string is built at query time from atomic parts:

```r
# Species
paste(tax_gen, tax_esp)
# → "Piptadeniastrum africanum"

# Infraspecific (one level)
paste(tax_gen, tax_esp, tax_rank01, tax_nam01)
# → "Guibourtia tessmannii var. tessmannii"

# With authors
paste(tax_gen, tax_esp, author1, tax_rank01, tax_nam01, author2)
```

---

## 2. WCVP backbone — `wcvp_names`

**Database:** `rainbio` (imported from `rWCVPdata::wcvp_names`)

The full WCVP dataset (~350 000 records) is imported as-is into the taxa
database. Unlike the internal backbone, the full name string is **pre-built**
in the `taxon_name` column and the acceptance status is stored explicitly in
`taxon_status`.

### Key columns

| Column | Type | Description |
|---|---|---|
| `plant_name_id` | integer PK | WCVP's own unique name identifier |
| `ipni_id` | varchar | IPNI (Index of Plant Names) identifier |
| `accepted_plant_name_id` | integer FK → `plant_name_id` | Accepted name for synonyms (analogue of `idtax_good_n`); set to `NA` in query results when the taxon is already accepted |
| `parent_plant_name_id` | integer | Parent entry in the WCVP hierarchy |
| `taxon_status` | varchar | `"Accepted"`, `"Synonym"`, `"Illegitimate"`, `"Invalid"`, etc. |
| `taxon_name` | varchar | Full pre-built name string (without authors) |
| `taxon_rank` | varchar | `"Species"`, `"Genus"`, `"Family"`, `"Variety"`, etc. |
| `family` | varchar | Family name |
| `genus` | varchar | Genus name |
| `species` | varchar | Species epithet |
| `infraspecific_rank` | varchar | Infraspecific rank abbreviation |
| `infraspecies` | varchar | Infraspecific epithet |
| `taxon_authors` | text | Full author string (basionym + combination) |
| `geographic_area` | text | WGSRPD distribution codes |
| `lifeform_description` | text | Raunkiær life form |
| `first_published` | varchar | Year (or range) of first valid publication |
| `wcvp_version` | varchar | Dataset version tag (tracks import history) |

### Synonymy

```
taxon_status = "Accepted"         →  accepted taxon
taxon_status = "Synonym" (etc.)   →  follow accepted_plant_name_id to reach
                                      the accepted record
```

### Import and version tracking

The `wcvp_import_metadata` table records each import:

| Column | Description |
|---|---|
| `wcvp_version` | Version string from `rWCVPdata` |
| `import_date` | Timestamp of import |
| `imported_by` | System user |
| `record_count` | Number of records imported |
| `link_count` | Number of links currently in `wcvp_idtax_link` |
| `is_current` | Only the most recent import is `TRUE` |

```r
# Check current import
get_wcvp_status()

# Check for a newer version
check_wcvp_update()

# Reimport (if update available)
import_wcvp_names(con_taxa, force = TRUE)
```

---

## 3. The bridge — `wcvp_idtax_link`

**Database:** `rainbio`

This table maps each internal taxon (`idtax_n`) to one or more WCVP names
(`plant_name_id`). It is populated by the matching workflow and can be
supplemented with manually reviewed entries.

### Columns

| Column | Type | Description |
|---|---|---|
| `idtax_n` | integer FK → `table_taxa` | Internal taxon ID |
| `plant_name_id` | integer FK → `wcvp_names` | Matched WCVP name ID |
| `match_type` | varchar | `"exact"` — identical name string; `"fuzzy"` — approximate match |
| `match_score` | numeric(4,3) | Normalised string similarity (0–1) |
| `matched_on` | timestamp | When the match was computed |
| `matched_by` | varchar | User who ran the matching |
| `verified` | boolean | `TRUE` if the match has been manually reviewed |
| `notes` | text | Free-text annotation |
| PK | (idtax_n, plant_name_id) | Composite primary key — one internal taxon may link to multiple WCVP names (homonym cases) |

### Matching workflow

```r
con_taxa <- call.mydb.taxa()

# 1. Run automated matching (exact first, then fuzzy for unmatched)
matches <- match_taxa_to_wcvp(
  con_taxa,
  methods          = c("exact", "fuzzy"),
  fuzzy_threshold  = 0.9,
  author_match     = "fuzzy",   # "none" | "exact" | "fuzzy"
  author_threshold = 0.6,
  n_cores          = 4L         # parallel on Windows (PSOCK) or Unix (fork)
)

# 2. Review the matches tibble manually if needed
# matches has: idtax_n, taxon_name_internal, plant_name_id, wcvp_taxon_name,
#              match_type, match_score

# 3. Persist reviewed matches
save_wcvp_links(matches, con_taxa)
```

---

## 4. Parallel structure at a glance

| Concept | Internal backbone | WCVP backbone |
|---|---|---|
| Primary ID | `idtax_n` | `plant_name_id` |
| Points-to-accepted | `idtax_good_n` (`NULL` if accepted) | `accepted_plant_name_id` (`NA` if already accepted) |
| Hierarchy parent | `id_parent` | `parent_plant_name_id` |
| Acceptance indicator | inferred from `idtax_good_n IS NULL` | explicit `taxon_status = "Accepted"` |
| Full name string | built at query time from atomic columns | pre-built `taxon_name` column |
| Author storage | split across `author1`, `author2`, `author3` | single `taxon_authors` text field |
| Data provenance | `tax_source` | `wcvp_version` (import-level) |

---

## 5. How `query_taxa()` integrates both backbones

```
┌─────────────────────────────────────────────────────────────┐
│  query_taxa(..., backbone = "wcvp")                         │
│                                                             │
│  1. Query table_taxa (internal backbone)                    │
│     • synonym resolution via idtax_good_n                   │
│     • hierarchy filters (only_genus, include_children, …)   │
│                                                             │
│  2. get_wcvp_names(idtax_n)                                 │
│     JOIN wcvp_idtax_link  ON idtax_n                        │
│     JOIN wcvp_names       ON plant_name_id                  │
│     optionally follow accepted_plant_name_id for synonyms   │
│                                                             │
│  3. .apply_wcvp_backbone()                                  │
│     Overwrite: tax_fam, tax_gen, tax_esp,                   │
│                tax_sp_level, tax_infra_level,               │
│                tax_infra_level_auth                         │
│     Add:       wcvp_plant_name_id,                          │
│                wcvp_accepted_plant_name_id,                 │
│                name_source ("wcvp" | "internal"),           │
│                alt_taxon_name (original internal name)      │
│                                                             │
│  Taxa with no WCVP link → name_source = "internal",         │
│  internal columns unchanged.                                │
└─────────────────────────────────────────────────────────────┘
```

The same `backbone` argument is available in `match_tax()` and
`add_taxa_table_taxa()`.

---

## 6. Key functions summary

| Function | Purpose |
|---|---|
| `query_taxa()` | Main query; `backbone = "wcvp"` triggers enrichment |
| `match_tax()` | Synonym resolution + genus-level trait aggregation |
| `add_taxa_table_taxa()` | Lightweight taxon info fetch (no traits) |
| `setup_wcvp_schema()` | Create WCVP tables in the taxa DB |
| `import_wcvp_names()` | Import / refresh the WCVP dataset |
| `match_taxa_to_wcvp()` | Generate automated name matches |
| `save_wcvp_links()` | Persist matches to `wcvp_idtax_link` |
| `get_wcvp_names()` | Lookup WCVP info for a vector of `idtax_n` |
| `get_wcvp_status()` | Report current import version and counts |
| `check_wcvp_update()` | Detect whether a newer WCVP version is available |
| `get_taxon_children()` | Recursive descendant traversal via `id_parent` |
| `get_taxon_ancestors()` | Walk up the hierarchy to root |
| `get_taxon_hierarchy()` | Full class → taxon path as a structured list |
