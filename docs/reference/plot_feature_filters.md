# Plot features that can be used as a filter

Lists the plot features \[query_plots()\] accepts in
\`feature_filters\`: the features whose value reads as text. A plot
feature is a row of \`data_liste_sub_plots\` typed by
\`subplotype_list\`, not a column of \`data_liste_plots\` – see
\[subplot_list()\] for every feature type, filterable or not.

## Usage

``` r
plot_feature_filters(con = NULL)
```

## Arguments

- con:

  Optional database connection. If \`NULL\`, \[call.mydb()\] is called.

## Value

A tibble with one row per filterable feature: \`feature\`, \`valuetype\`
(\`"character"\`, or \`"table_colnam"\` for people), \`category\` and
\`description\`.

## See also

\[plot_feature_values()\] for the values a feature actually holds,
\[query_plots()\] to filter with them, \[subplot_list()\] for all
feature types.

## Examples

``` r
if (FALSE) { # \dontrun{
  plot_feature_filters(con = mydb)
} # }
```
