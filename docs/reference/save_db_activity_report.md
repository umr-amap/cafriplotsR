# Save a database activity report as an HTML file

Generates a self-contained HTML snapshot of a
[`get_db_activity`](https://umr-amap.github.io/cafriplotsR/reference/get_db_activity.md)
result and optionally opens it in the default browser.

## Usage

``` r
save_db_activity_report(activity, output_dir = ".", open = TRUE)
```

## Arguments

- activity:

  A `db_activity` object from
  [`get_db_activity`](https://umr-amap.github.io/cafriplotsR/reference/get_db_activity.md).

- output_dir:

  Directory where the HTML file is saved. Defaults to the current
  working directory.

- open:

  Logical. Open the file in the default browser after saving? Defaults
  to `TRUE`.

## Value

Path to the saved HTML file (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
activity <- get_db_activity(con)
save_db_activity_report(activity, output_dir = "~/db_reports")
} # }
```
