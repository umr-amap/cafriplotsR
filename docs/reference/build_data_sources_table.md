# Build a data sources summary table (citations × traits pivot)

Creates a wide pivot table showing how many measurements each data
source contributes per trait. Used by
[`query_plots`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md)
when `extract_traits = TRUE` and by the query plots Shiny app to display
the "Data Sources" panel.

## Usage

``` r
build_data_sources_table(traits_raw)
```

## Arguments

- traits_raw:

  Long-format data frame returned by
  `query_taxa_traits(include_citation = TRUE, format = "long")`. Must
  contain columns `trait`, `citation_key`, and `idtax`.

## Value

A data frame with one row per citation (rows) and one column per trait
(measurement counts), preceded by citation metadata columns and a
`n_taxa` column. Returns `NULL` when `traits_raw` is `NULL`, empty, or
lacks the required columns.
