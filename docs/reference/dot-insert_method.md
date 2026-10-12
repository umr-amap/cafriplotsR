# Insert a new method into methodslist

Values are bound as parameters, never pasted into the SQL. A blank
description is stored as NULL. A name that already exists, compared
without regard to case or surrounding spaces, is refused rather than
duplicated.

## Usage

``` r
.insert_method(con, method_name, description = NULL)
```

## Arguments

- con:

  DBI connection to the main database.

- method_name:

  Character. Name of the new method.

- description:

  Character or NULL. Protocol description.

## Value

Integer, the `id_method` of the new row.
