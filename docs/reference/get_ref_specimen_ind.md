# Find Unlinked Individuals with Herbarium Information

Extract individuals that have herbarium specimen references in their
data but no formal links in data_link_specimens table.

## Usage

``` r
get_ref_specimen_ind(collector = NULL, ids = NULL, con = NULL)
```

## Arguments

- collector:

  Character. Optional collector name to filter by.

- ids:

  Integer vector. Optional individual IDs to check.

- con:

  Database connection. If NULL, calls call.mydb()

## Value

List with: - all_herb_not_linked: Individuals with herbarium info but no
links - all_linked_individuals: IDs of individuals that ARE linked

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
