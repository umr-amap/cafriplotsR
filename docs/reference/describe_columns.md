# Describe columns in query results

Generates a documentation table explaining the content of each column in
[`query_plots()`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md)
or
[`query_individual_features()`](https://umr-amap.github.io/cafriplotsR/reference/query_individual_features.md)
output. Reverse-maps renamed, pivoted, and feature columns back to their
original meaning using output style configurations and database
metadata.

## Usage

``` r
describe_columns(result, con = NULL)
```

## Arguments

- result:

  A `plot_query_list` (from
  [`query_plots()`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md)
  with an output style applied) or a plain `data.frame`.

- con:

  A database connection (DBI connection or pool). If `NULL` (the
  default), uses the active connection from
  [`call.mydb()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md).

## Value

An object of class `"column_documentation"`:

- For a `plot_query_list`: a named list of data.frames (one per table:
  metadata, individuals, etc.)

- For a plain `data.frame`: a single data.frame

Each data.frame has columns: `column_name`, `original_name`,
`description`, `category`, `unit`, `notes`.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- query_plots(output_style = "standard")
docs <- describe_columns(result)
print(docs)
} # }
```
