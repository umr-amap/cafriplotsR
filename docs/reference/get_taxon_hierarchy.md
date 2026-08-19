# Get Full Taxonomy Hierarchy for a Taxon

Returns a structured list containing the full hierarchical path from
class to the given taxon, with each level's information.

## Usage

``` r
get_taxon_hierarchy(idtax_n, con = NULL)
```

## Arguments

- idtax_n:

  The taxon ID to get hierarchy for

- con:

  Database connection (optional, will create if NULL)

## Value

List with hierarchy levels: class, order, family, genus, species,
infraspecific
