# Process individuals for query_plots

Extract and process individuals with filters and plot metadata

## Usage

``` r
process_individuals(
  plots_data,
  con,
  con_taxa,
  id_individual = NULL,
  id_tax = NULL,
  tag = NULL,
  include_liana = FALSE,
  census_strategy = c("last", "first", "mean"),
  show_multiple_census = FALSE,
  backbone = c("internal", "wcvp")
)
```

## Arguments

- plots_data:

  Data frame of plots

- con:

  Database connection

- con_taxa:

  Database connection

- id_individual:

  Vector of individual IDs (optional)

- id_tax:

  Vector of taxonomic IDs (optional)

- tag:

  Vector of tags (optional)

- include_liana:

  Include lianas (logical)

## Value

Data frame of processed individuals
