# Try to open PostgreSQL table

Access to postgresql database table with repeating if no successful

## Usage

``` r
try_open_postgres_table(table, con)
```

## Arguments

- table:

  A table name.

- con:

  A database connection.

## Value

A \`tbl\` object representing the PostgreSQL table. The function will
error if the connection to the database is lost or if it fails to
connect after 10 attempts.

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
