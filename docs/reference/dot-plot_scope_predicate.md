# The boolean expression a plot-scope policy tests

Built from \[.plot_scope_route()\] and \[.plot_grant_list_sql()\].
Columns of the policy's own table are qualified with the table name,
which is what PostgreSQL accepts inside a policy and what keeps the
\`EXISTS\` branches unambiguous against their alias.

## Usage

``` r
.plot_scope_predicate(table_name, mode = c("read", "write"))
```

## Arguments

- table_name:

  Character of length 1. One of \[.plot_scope_child_tables()\].

- mode:

  \`"read"\` or \`"write"\`.

## Value

Character of length 1.
