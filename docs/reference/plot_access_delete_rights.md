# Who can delete what

Reports both layers side by side: the \`can_delete\` rows in
\`plot_access\`, and who holds the DELETE table privilege. A mismatch is
the interesting case - a table privilege with no \`can_delete\` row is
an account that can delete child rows but no plot.

## Usage

``` r
plot_access_delete_rights(con, tables = .plot_scope_tables())
```

## Arguments

- con:

  A connection to plots_transects.

- tables:

  Character vector of tables to check the privilege on.

## Value

Invisibly a list of two data frames.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
plot_access_delete_rights(con)
} # }
```
