# Citation Panel Module - Server

Renders a panel listing data sources and citations for trait
measurements.

## Usage

``` r
mod_citation_panel_server(id, citation_data, i18n)
```

## Arguments

- id:

  Module namespace ID

- citation_data:

  Reactive returning a citations x traits pivot table from
  [`build_data_sources_table()`](https://umr-amap.github.io/cafriplotsR/reference/build_data_sources_table.md)
  with columns: `citation_key`, optionally `citation_authors`,
  `citation_year`, `citation_title`, `citation_dataset_name`, `n_taxa`,
  and one column per trait containing measurement counts. Returns NULL
  when no data is available.

- i18n:

  Reactive returning a shiny.i18n translator object
