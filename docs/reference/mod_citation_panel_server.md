# Citation Panel Module - Server

Renders a panel listing data sources and citations for trait
measurements.

## Usage

``` r
mod_citation_panel_server(
  id,
  citation_data,
  i18n,
  count_col = "n_taxa",
  context = c("traits", "plots")
)
```

## Arguments

- id:

  Module namespace ID

- citation_data:

  Reactive returning a citations x traits (or, for plots, citations x
  country) pivot table from
  [`build_data_sources_table()`](https://umr-amap.github.io/cafriplotsR/reference/build_data_sources_table.md)
  /
  [`build_plot_data_sources_table()`](https://umr-amap.github.io/cafriplotsR/reference/build_plot_data_sources_table.md)
  with columns: `citation_key`, optionally `citation_authors`,
  `citation_year`, `citation_title`, `citation_dataset_name`, a count
  column (`n_taxa` or `n_plots`, see `count_col`), and one column per
  trait/country containing counts. Returns NULL when no data is
  available.

- i18n:

  Reactive returning a shiny.i18n translator object

- count_col:

  Name of the summary count column to exclude, alongside the citation
  metadata columns, when computing the per-column breakdown total
  (`"n_taxa"` for trait citations, `"n_plots"` for plot citations).
  Defaults to `"n_taxa"`.

- context:

  Either `"traits"` (default) or `"plots"` - selects the wording used in
  the banner, warning message and stat labels, since the same module
  backs both the trait-measurement "Data Sources" tab and the plot-level
  "Plot Data Sources" tab.
