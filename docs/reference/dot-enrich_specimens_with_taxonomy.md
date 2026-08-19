# Enrich Specimens with Taxonomy

Internal helper to add taxonomic information to specimens. Uses
table_idtax from main database (con) for synonym resolution, then
fetches full taxonomy from taxa database (con.taxa). Note: table_idtax
must be updated via update_taxa_link_table() first.

## Usage

``` r
.enrich_specimens_with_taxonomy(specimens, con, con.taxa = NULL)
```

## Arguments

- specimens:

  Data frame with specimen data including idtax_n

- con:

  Main database connection (for table_idtax)

- con.taxa:

  Taxa database connection (for table_taxa)

## Value

Data frame with taxonomy columns added
