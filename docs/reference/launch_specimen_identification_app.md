# Launch Specimen Identification Update App

Launches an interactive Shiny app for updating specimen identifications
in the \`specimens\` table. The app wraps
[`update_ident_specimens`](https://umr-amap.github.io/cafriplotsR/reference/update_ident_specimens.md)
and provides two workflows:

## Usage

``` r
launch_specimen_identification_app(lang = "fr")
```

## Arguments

- lang:

  Character. Initial UI language: `"en"` or `"fr"`. Default: `"fr"`.

## Value

Launches a Shiny app (does not return until the app closes).

## Details

- **Manual** - search a single specimen by id_specimen, or by
  collector + number; review current values; pick a new accepted taxon
  via an embedded taxonomy search; optionally update determination
  metadata (detd/detm/dety/detby/detvalue) and collector number /
  suffix; optionally edit the other specimen fields (coly/colm/cold,
  add_col, locality, country, ddlat, ddlon, description) which are
  pre-filled with the current values; preview a diff and confirm.

- **Batch** - upload an Excel/CSV file with one row per specimen to
  update. Columns are mapped to specimen fields, collectors are matched
  against `table_colnam` (skipped if `id_specimen` is mapped), all rows
  are validated, then previewed and applied row-by-row.

Batch mode requires that taxonomic names have already been standardized
to `idtax_n` values - typically by first using
[`launch_taxonomic_match_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxonomic_match_app.md).

## See also

[`update_ident_specimens`](https://umr-amap.github.io/cafriplotsR/reference/update_ident_specimens.md),
[`update_records`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md),
[`launch_taxonomic_match_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxonomic_match_app.md),
[`launch_specimen_import_wizard`](https://umr-amap.github.io/cafriplotsR/reference/launch_specimen_import_wizard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
launch_specimen_identification_app()
launch_specimen_identification_app(lang = "en")
} # }
```
