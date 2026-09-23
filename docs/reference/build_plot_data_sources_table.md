# Build a plot data sources summary table (citations × country pivot)

Creates a wide pivot table showing how many plots each data source
contributes per country. Used by
[`query_plots`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md)
to populate the \`plot_sources\` element of its result - the plot-level
counterpart of
[`build_data_sources_table`](https://umr-amap.github.io/cafriplotsR/reference/build_data_sources_table.md),
which does the same thing for taxon-level trait citations.

## Usage

``` r
build_plot_data_sources_table(plots_raw)
```

## Arguments

- plots_raw:

  Data frame of plots enriched with citation info, as returned
  internally by \`query_plots()\` before individual extraction. Must
  contain columns `id_liste_plots` and `citation_key`. `country`, when
  present, becomes the pivoted dimension.

## Value

A data frame with one row per citation (rows) and, when `country` is
available, one column per country (plot counts), preceded by citation
metadata columns and an `n_plots` column. Returns `NULL` when
`plots_raw` is `NULL`, empty, or lacks the required columns.
