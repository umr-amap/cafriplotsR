# Where plot_access and the row-level security policies disagree

Until the child tables are keyed on \`plot_access\`, the policies on
\`data_liste_plots\` are what enforce access and \`plot_access\` is a
record of them. This compares the two, per account, and names the
difference.

A grant naming a plot that no longer exists is not counted as a
difference: a \`USING\` clause naming a deleted plot matches no row, so
the account has seen nothing for it since the plot went.

## Usage

``` r
plot_access_drift(con)
```

## Arguments

- con:

  A connection to plots_transects, as the owner (the comparison reads
  every account's grants, which only the owner can see).

## Value

Invisibly a data frame with one row per account, and \`TRUE\` in
\`agrees\` where the two records match.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
plot_access_drift(con)
} # }
```
