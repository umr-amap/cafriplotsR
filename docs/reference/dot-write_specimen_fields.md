# Write specimen field updates (backup + update) in a single transaction

Write specimen field updates (backup + update) in a single transaction

## Usage

``` r
.write_specimen_fields(
  con,
  id_speci,
  coerced,
  current_record,
  modif_type,
  add_backup = TRUE
)
```

## Arguments

- con:

  Database connection or pool.

- id_speci:

  Integer, specimen id.

- coerced:

  Named list of already coerced new values.

- current_record:

  One-row data frame with the current specimen record.

- modif_type:

  Character, value stored in the backup `modif_type`.

- add_backup:

  Logical, whether to write the backup row.

## Value

Invisibly, the number of rows updated.
