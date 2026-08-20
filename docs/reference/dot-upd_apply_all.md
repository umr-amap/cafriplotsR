# Write a record's flat columns and its feature records as one unit

Flat columns and features live in different tables, so applying them
separately can leave a record half-updated if the second write fails.
Both go inside one transaction; anything raised rolls the whole edit
back.

## Usage

``` r
.upd_apply_all(entity, id, values, features, con)
```

## Arguments

- entity:

  \`"plot"\` or \`"individual"\`.

- id:

  The record id.

- values:

  Named list of database column -\> new value for the flat table.

- features:

  Named list keyed by feature record id (see \[.upd_apply_feature()\]).

- con:

  A DBI connection.

## Value

A list with \`n_direct\` and \`n_feature\`: how many values were
written.
