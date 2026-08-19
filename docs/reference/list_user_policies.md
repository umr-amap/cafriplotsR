# List user policies

Lists row-level security policies from pg_policies system catalog.

## Usage

``` r
list_user_policies(con, user = NULL, table = "data_liste_plots")
```

## Arguments

- con:

  A database connection object.

- user:

  Character. Filter policies by username (optional).

- table:

  Character. Filter by table name. Default is "data_liste_plots". Use
  NULL to see policies on all tables.

## Value

A data frame with policy information (schemaname, tablename, policyname,
roles, cmd, qual).
