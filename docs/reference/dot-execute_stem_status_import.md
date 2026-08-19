# Execute stem vital status upsert

Re-runs
[`compute_stem_vital_status()`](https://umr-amap.github.io/cafriplotsR/reference/compute_stem_vital_status.md)
with `add_data = TRUE` for the individuals stored in
`config$individual_ids`. Existing `stem_status` records for those
individuals are deleted and replaced.

## Usage

``` r
.execute_stem_status_import(data, config, con, dry_run = TRUE, i18n = NULL)
```

## Arguments

- data:

  Status tibble (already computed in step 3; used only for the dry-run
  preview).

- config:

  Feature configuration list; must contain `individual_ids` (integer
  vector).

- con:

  Database connection

- dry_run:

  Logical

- i18n:

  Translator object

## Value

List with import results
