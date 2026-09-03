# Update non-identification fields of a single specimen

\`r lifecycle::badge("deprecated")\`

Superseded by
[`update_records`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md),
which writes the same columns of `specimens` (`description` included
since 1.9.95) through the generic update path, and additionally offers
batch mode and a `execute = FALSE` dry run. Migrate with:

    ## before
    update_specimen_fields(id_speci = 12345,
                           new_values = list(locality = "Mont Bela"))

    ## after
    update_records(
      data = data.frame(id_specimen = 12345, locality = "Mont Bela"),
      table_type = "specimens",
      execute = TRUE
    )

Updates any of the editable, non-taxonomic columns of the `specimens`
table for one specimen: collection date (`coly`, `colm`, `cold`),
locality information (`locality`, `country`, `ddlat`, `ddlon`),
`description`, `add_col`, `original_tax_name`, `colnbr` and `suffix`.

Only fields whose new value actually differs from the stored value are
written. Supplying `NA` (or an empty string) for a field clears it (sets
it to `NULL` in the database). Fields that are absent from `new_values`
(or set to `NULL`) are left untouched.

Identification fields (`idtax_n` and the `det*` columns) are not handled
here - use
[`update_ident_specimens`](https://umr-amap.github.io/cafriplotsR/reference/update_ident_specimens.md)
for those.

## Usage

``` r
update_specimen_fields(
  id_speci,
  new_values,
  add_backup = TRUE,
  ask_before_update = TRUE,
  show_results = TRUE,
  con = NULL
)
```

## Arguments

- id_speci:

  Integer, id of the specimen to update (single value).

- new_values:

  Named list of new values, one element per field to update. Allowed
  names are given by `names(CafriplotsR:::.specimen_editable_fields())`.

- add_backup:

  Logical, whether the current record should be copied to
  `followup_updates_specimens` before updating. Default `TRUE`.

- ask_before_update:

  Logical, whether to ask for confirmation in the console before
  writing. Default `TRUE`.

- show_results:

  Logical, whether the updated specimen should be queried and printed
  after the update. Default `TRUE`.

- con:

  Database connection or pool. Default `NULL`, in which case
  [`call.mydb`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md)
  is used.

## Value

Invisibly, a tibble with one row per modified field and the columns
`field`, `current` and `new`. A zero-row tibble is returned when nothing
was modified.

## See also

[`update_records`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md),
[`update_ident_specimens`](https://umr-amap.github.io/cafriplotsR/reference/update_ident_specimens.md)

## Author

Gilles Dauby, <gilles.dauby@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
update_specimen_fields(
  id_speci = 12345,
  new_values = list(ddlat = 0.123, ddlon = 11.456, locality = "Mont Bela")
)

## clear a field
update_specimen_fields(id_speci = 12345, new_values = list(description = NA))
} # }
```
