# Get All Ancestors of a Taxon

Walks up the id_parent chain to return all ancestor taxa from the given
taxon up to the root (class level).

## Usage

``` r
get_taxon_ancestors(idtax_n, con = NULL, include_self = TRUE)
```

## Arguments

- idtax_n:

  The taxon ID to find ancestors for

- con:

  Database connection (optional, will create if NULL)

- include_self:

  If TRUE, include the taxon itself in results

## Value

Data frame of ancestor taxa, ordered from root to taxon
