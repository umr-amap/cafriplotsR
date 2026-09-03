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
roles, cmd, qual), plus a \`plot_ids\` list-column holding the numeric
IDs parsed out of each row's \`qual\` expression (integer(0) when
\`qual\` has none, e.g. NA or an id-less policy such as a global
\`creator_access\_\*\` policy).
