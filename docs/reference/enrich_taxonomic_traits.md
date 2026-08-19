# Enrich individuals with taxonomic-level traits

Adds trait data at the taxonomic level to individual records. Uses the
new query_taxa_traits() architecture.

## Usage

``` r
enrich_taxonomic_traits(individuals, con)
```

## Arguments

- individuals:

  Data frame with individual data

- con:

  Database connection

## Value

Data frame with added taxonomic traits
