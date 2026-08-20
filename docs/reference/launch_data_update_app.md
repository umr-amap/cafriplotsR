# Launch the Data Update App

Launches an interactive Shiny app for correcting plot metadata and
individual data one record at a time. It is the user-friendly
counterpart to
[`update_records`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md),
which is powerful but expects the caller to already know which table a
value lives in.

## Usage

``` r
launch_data_update_app(lang = "fr")
```

## Arguments

- lang:

  Character. Initial UI language: `"en"` or `"fr"`. Default: `"fr"`.

## Value

Launches a Shiny app (does not return until the app closes).

## Details

The app has two sections:

- **Plot metadata** - pick a plot, edit the columns stored directly in
  `data_liste_plots` (including the `method` and `country` lookups,
  offered as dropdowns), and edit its features.

- **Individual data** - find an individual by plot and tag or by `id_n`,
  edit the columns of `data_individuals`, change its identification
  through an embedded taxonomic search, and edit its trait measurements.

**Why features need care.** Many columns of an extracted plot or
individual table are not columns of that record at all. Plot features
are rows of `data_liste_sub_plots`; individual features are rows of
`data_traits_measures`. Worse, one extracted column can be the
*aggregate* of several such rows - the mean of a trait measured at three
censuses, or the concatenated names of everyone recorded as
`additional_people`. Writing back to that single value is meaningless,
which is why
[`update_records()`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md)
refuses it.

The app therefore never edits an aggregate. For every feature it shows
how many records back it, what the extracted table would display, and
how that display was computed; the editable inputs are the underlying
records, each labelled with its own id and its census or subplot
context.

Only existing records can be edited. Adding or deleting measurements is
done with the feature wizard
([`launch_feature_wizard`](https://umr-amap.github.io/cafriplotsR/reference/launch_feature_wizard.md))
and the `safe_delete_*` functions.

Every write goes through
[`detect_direct_changes()`](https://umr-amap.github.io/cafriplotsR/reference/detect_direct_changes.md)
and `execute_direct_updates()`, so stored values are re-read immediately
before writing, only genuine differences are written, and records are
backed up to their follow-up table where one exists.

## See also

[`update_records`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md),
[`query_plot_features`](https://umr-amap.github.io/cafriplotsR/reference/query_plot_features.md),
[`query_individual_features`](https://umr-amap.github.io/cafriplotsR/reference/query_individual_features.md),
[`launch_specimen_identification_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_specimen_identification_app.md),
[`launch_feature_wizard`](https://umr-amap.github.io/cafriplotsR/reference/launch_feature_wizard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
launch_data_update_app()
launch_data_update_app(lang = "en")
} # }
```
