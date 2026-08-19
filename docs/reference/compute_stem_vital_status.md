# Compute stem vital status for specified individuals

Derives a per-census vital status (`"alive"`, `"dead"`,
`"presumed_dead"`, or `NA`) for each specified stem, applying the same
evidence hierarchy and retroactive correction used in the bulk
`workflow_stem_status.R`. Intended for incremental updates after new
measurements or observations are added, and as a backend for the feature
wizard Shiny app.

## Usage

``` r
compute_stem_vital_status(
  individual_ids,
  add_data = FALSE,
  dry_run = TRUE,
  dead_patterns = c("\\bdead\\b", "\\bmort\\b"),
  missing_patterns = c("\\bmissing\\b", "\\bnon\\s*vu\\b", "\\bdisparu\\b",
    "\\bmanquant\\b", "suppos[eé]\\s*mort"),
  alive_patterns = c("\\balive\\b", "\\bvivant\\b"),
  con = NULL
)
```

## Arguments

- individual_ids:

  Integer vector of individual IDs (`id_n` / `id_data_individuals`).

- add_data:

  Logical. If `TRUE`, upsert the computed statuses into the database:
  existing `stem_status` records for these individuals are deleted, then
  the new statuses are inserted. Default `FALSE`.

- dry_run:

  Logical. When `add_data = TRUE`, preview the upsert without committing
  any changes. Default `TRUE`.

- dead_patterns:

  Character vector of case-insensitive regexes identifying "dead"
  observation text.

- missing_patterns:

  Character vector of case-insensitive regexes identifying "missing"
  observation text.

- alive_patterns:

  Character vector of case-insensitive regexes identifying "alive"
  observation text.

- con:

  Database connection. Defaults to
  [`call.mydb()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md).

## Value

A tibble with one row per individual \\\times\\ census:

- `id_n`:

  Individual ID

- `id_table_liste_plots`:

  Plot ID

- `id_sub_plots`:

  Census subplot ID (used for DB linking)

- `plot_name`:

  Plot name

- `census_name`:

  Census label, e.g. `"census_1"`

- `census_date`:

  Date of census

- `stem_vital_status`:

  `"alive"`, `"dead"`, `"presumed_dead"`, or `NA` (before first
  measurement)

- `missing`:

  Logical — stem was reported missing at this census

- `evidence_source`:

  Human-readable summary of evidence used

Returned invisibly when `add_data = TRUE` and `dry_run = FALSE`.

## Evidence sources (in priority order)

1.  **observations** trait: regex patterns on free-text notes
    (`dead_patterns`, `missing_patterns`, `alive_patterns`)

2.  **flag2_rainfor**: non-`"k"` = dead; `"k"` = presumed_dead / missing

3.  **stem_diameter**: \\\ge 2\\ consecutive missing censuses = dead; 1
    consecutive missing census = presumed_dead

4.  **flag1_rainfor**: presence is an alive indicator (informational;
    does not override stronger dead evidence)

## Retroactive correction

If a stem is classified as `"presumed_dead"` at census *N* but has a
recorded diameter at a later census *N+k*, that earlier status is
corrected to `"alive"` (the stem was just temporarily unmeasured).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Compute status for a few stems (read-only, no DB write)
result <- compute_stem_vital_status(individual_ids = c(101, 102, 103), con = con)
print(result)

# Compute and preview what would be written to the DB
compute_stem_vital_status(
  individual_ids = c(101, 102, 103),
  add_data = TRUE,
  dry_run  = TRUE,
  con      = con
)

# Compute and commit to the DB
compute_stem_vital_status(
  individual_ids = c(101, 102, 103),
  add_data = TRUE,
  dry_run  = FALSE,
  con      = con
)
} # }
```
