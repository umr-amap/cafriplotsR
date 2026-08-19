# Validate Multi-Stem Grouping (Internal)

Checks the `multi_tiges_id` column, which holds the **tag** of the main
stem a secondary stem belongs to. It is a staging column: the import
resolves it to `data_individuals.stem_grouping`, which stores the
parent's `id_n`. Anything that cannot be resolved becomes a silently
missing grouping after the insert, so it is checked beforehand.

## Usage

``` r
.validate_multi_stem_grouping(data, con)
```

## Arguments

- data:

  Individuals data frame with plot_name, tag and multi_tiges_id

- con:

  Database connection

## Value

List with `errors` and `warnings`

## Details

Checks performed, mirroring the Feature Wizard's multi-stem step:

- values that are not a tag number (error)

- a stem pointing at itself (error)

- a parent tag absent from both the import and the plot in the database
  (error)

- a parent that is itself a secondary stem, which would chain the
  grouping instead of pointing at the main stem (error)

- members of a group carrying different `idtax_n` (warning)

- a parent that already carries a grouping in the database (warning)
