# Write flat columns of a plot or an individual

Re-reads the stored values and writes only what actually differs,
backing the records up first via \`execute_direct_updates()\`.

## Usage

``` r
.upd_apply_direct(entity, id, values, con)
```

## Arguments

- entity:

  \`"plot"\` or \`"individual"\`.

- id:

  The record id.

- values:

  Named list of database column -\> new value. \`NA\` clears.

- con:

  A DBI connection.

## Value

The change tibble that was applied (invisibly \`NULL\` if nothing did).
