# Prepare specimen list from OpenForis tree data

Filters trees that have a `herbarium_nbe_char` value, applies the
specimen prefix, extracts a numeric collection number (`colnbr`), and
attaches plot coordinates and user-supplied metadata. The output is
ready for
[`add_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/add_specimens.md).
With `deduplicate = TRUE` the result holds one row per unique voucher
rather than one row per individual.

## Usage

``` r
.prepare_openforis_specimens(
  trees,
  census_metadata = NULL,
  specimen_prefix = NULL,
  locality = NULL,
  country = NULL,
  col_month = NULL,
  col_year = NULL,
  collector = NULL,
  additional_people = NULL,
  additional_collector = NULL,
  deduplicate = FALSE,
  description_col = "stem_diameter",
  branch_position_col = "branch_position"
)
```

## Arguments

- trees:

  Full tree data frame (all individuals, already tag-resolved).

- census_metadata:

  Census metadata tibble (used to get plot coordinates if a
  `ddlat`/`ddlon` column is present). Can be NULL.

- specimen_prefix:

  Character prefix (e.g. "PIRD"). NULL to skip.

- locality:

  Character. Locality string (e.g. "Mbalmayo, Centre").

- country:

  Character. Country name.

- col_month:

  Integer. Collection month.

- col_year:

  Integer. Collection year.

- collector:

  Character. Collector code (stored as `colnam`).

- additional_people:

  Character. Additional collectors.

- additional_collector:

  Character scalar applied to every specimen, or a data frame with
  columns `plot_name` and `additional_collector` giving the collecting
  team per plot. Written to an `additional_collector` column.

- deduplicate:

  Logical. If TRUE, keep a single row per unique specimen instead of one
  row per individual carrying it. Among duplicates the row with both a
  `specimen_number` and a `colnbr` wins, ties going to the first
  individual bearing the voucher; the full individual-to-specimen link
  stays available in `trees`.

- description_col:

  Column name in `trees` used to build description (default
  "stem_diameter"). Set to NULL to skip.

- branch_position_col:

  Column name in `trees` giving the origin of the collected branch
  (default "branch_position"). Only the value `"rejet"` is added to the
  description; `"shade_branch"` and `"light_branch"` are ignored.

## Value

Data frame ready for
[`add_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/add_specimens.md),
or NULL if no specimens.
