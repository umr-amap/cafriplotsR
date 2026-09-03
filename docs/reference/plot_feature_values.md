# Values a plot feature actually holds

The distinct stored values of one filterable feature, with the number of
plots carrying each. Meant for discovering what can be asked for before
passing it to \`query_plots(feature_filters = ...)\`, and for populating
a dropdown in the query app.

Lookup features (\`table_colnam\`) are resolved to readable names, so
the values returned are the ones to filter on.

## Usage

``` r
plot_feature_values(feature, con = NULL)
```

## Arguments

- feature:

  A single feature name, as listed by \[plot_feature_filters()\].

- con:

  Optional database connection. If \`NULL\`, \[call.mydb()\] is called.

## Value

A tibble with \`value\` and \`n_plots\`, most widespread value first.

## See also

\[plot_feature_filters()\], \[query_plots()\]

## Examples

``` r
if (FALSE) { # \dontrun{
  plot_feature_values("data_provider", con = mydb)
  plot_feature_values("principal_investigator", con = mydb)
} # }
```
