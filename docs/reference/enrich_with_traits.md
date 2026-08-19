# Enrich individuals with all traits

Enrich with individual-level traits, taxonomic traits, and aggregate to
genus level if needed

## Usage

``` r
enrich_with_traits(
  individuals,
  con,
  extract_individual_features = TRUE,
  extract_traits = TRUE,
  traits_to_genera = FALSE,
  wd_fam_level = FALSE,
  show_multiple_census = FALSE,
  issues = c("remove", "include", "ignore"),
  include_measurement_ids = FALSE,
  census_strategy = c("last", "first", "mean"),
  individual_features_format = c("wide", "long", "census_pairs")
)
```

## Arguments

- individuals:

  Data frame of individuals

- con:

  Database connection

- extract_individual_features:

  Extract individual-level traits

- extract_traits:

  Extract taxonomic traits

- traits_to_genera:

  Aggregate traits to genus level

- wd_fam_level:

  Use family-level wood density

- show_multiple_census:

  Show multiple census data

- issues:

  Character. How to handle flagged measurements: "remove", "include", or
  "ignore".

## Value

Data frame enriched with traits
