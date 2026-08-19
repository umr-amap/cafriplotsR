# Get All Children of a Taxon

Returns all taxa that have the specified taxon as their parent (directly
or recursively). Uses the id_parent column for hierarchy traversal.

## Usage

``` r
get_taxon_children(
  idtax_n,
  con = NULL,
  recursive = TRUE,
  include_self = FALSE,
  collect = TRUE
)
```

## Arguments

- idtax_n:

  The taxon ID to find children for

- con:

  Database connection (optional, will create if NULL)

- recursive:

  If TRUE (default), get all descendants; if FALSE, only direct children

- include_self:

  If TRUE, include the taxon itself in results

- collect:

  If TRUE (default), collect results to data frame

## Value

Data frame of child taxa
