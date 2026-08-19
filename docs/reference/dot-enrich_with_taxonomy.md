# Enrich with Taxonomy (Internal Helper)

Queries taxa database to get taxonomic names for individuals and
specimens. Uses add_taxa_table_taxa() which handles its own database
connection.

## Usage

``` r
.enrich_with_taxonomy(links_found, con_taxa)
```

## Arguments

- links_found:

  Data frame with individual_idtax_n and specimen_idtax_n

- con_taxa:

  Taxa database connection pool (not used, kept for compatibility)

## Value

Data frame enriched with taxonomic names
