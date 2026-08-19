# Deactivate an aggregation rule

Soft-delete: sets \`is_active = FALSE\`. Use \`hard = TRUE\` to actually
delete the row.

## Usage

``` r
remove_trait_aggregation(id, hard = FALSE, con = NULL)
```

## Arguments

- id:

  Integer. \`id_aggregation\` to deactivate.

- hard:

  If TRUE, DELETE the row instead of flipping \`is_active\`.

- con:

  Connection.

## Value

Invisible TRUE on success.
