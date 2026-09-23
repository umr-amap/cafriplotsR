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
  offered as dropdowns), link it to a parent plot, and edit its
  features.

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

**Linking a plot to another plot.** A plot section carries a parent plot
picker: the plot this one sits inside, chosen among the plots already in
the database, together with the relation that says how
(`nested_subsample` - the two overlap on the ground, so their
measurements must never be added; `block_member` - this plot tiles part
of the parent, so summing is correct). The two are written as one
statement, because `chk_plot_parent_relation_paired` refuses a row
holding either one alone, and they are therefore not offered among the
flat columns of section 3. Plots already below this one are left out of
the parent list, so a loop cannot be built from the app. The section is
hidden entirely on a database where `inst/migrations/plot_hierarchy.R`
has not been applied.

A link is stored on the child, so it is changed by loading the child.
The section lists the plots sitting inside the loaded one for reference
only.

**Why an identification is not just `idtax_n`.**
[`merge_individuals_taxa()`](https://umr-amap.github.io/cafriplotsR/reference/merge_individuals_taxa.md)
resolves the individual's `idtax_n` through `table_idtax` synonymy into
`idtax_f`, resolves the identification of the specimen linked to the
individual the same way into `idtax_specimen_f`, and uses
`idtax_individual_f = coalesce(idtax_specimen_f, idtax_f)` everywhere
downstream. The identification section shows that whole cascade. While a
specimen is linked, its identification wins: editing `idtax_n` is stored
but changes nothing an extraction shows, and the app says so both in the
section and in the preview. Re-identifying the specimen is done with
[`launch_specimen_identification_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_specimen_identification_app.md).

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
[`check_plot_hierarchy_consistency`](https://umr-amap.github.io/cafriplotsR/reference/check_plot_hierarchy_consistency.md),
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
