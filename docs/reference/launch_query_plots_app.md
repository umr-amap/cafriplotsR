# Launch Query Plots Interactive App

Launches an interactive Shiny application for querying forest plot data.
This app provides a user-friendly interface to the
[`query_plots`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md)
function with two main stages:

## Usage

``` r
launch_query_plots_app(pool_main = NULL, language = c("fr", "en"), ...)
```

## Arguments

- pool_main:

  Optional database connection pool for the main database. If not
  provided, the app will create one automatically using
  [`create_pool_main`](https://umr-amap.github.io/cafriplotsR/reference/create_pool_main.md).

- language:

  Character string for UI language. Options:

  - "fr" (French, default)

  - "en" (English)

- ...:

  Additional arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.,
  `launch.browser = TRUE`, `port = 3838`)

## Value

NULL (launches the Shiny app)

## Details

1.  **Filter & Discover**: Apply filters to find plots of interest, view
    them on an interactive map with metadata

2.  **Select & Extract**: Choose specific plots and extract detailed
    individual tree data with customizable output styles and options

The app provides an intuitive interface for:

**Filtering**: Use multiple criteria to find plots:

- Country, plot name, locality, method

- Individual tags, taxon IDs

- Advanced filters (plot IDs, specimen IDs)

**Visualization**: Explore plot locations:

- Interactive map with multiple basemaps

- Clickable markers with plot information

- Synchronized table view with selection

**Extraction**: Configure detailed data extraction:

- Choose output style (auto, minimal, standard, permanent_plot, etc.)

- Set census strategy (last, first, mean)

- Toggle traits, features, and additional data

- Configure data organization options

**Export**: Download results in multiple formats:

- Excel (.xlsx) - multi-sheet workbook

- CSV (zipped folder)

- R Object (.rds)

- Shapefile (.zip) - if spatial data included

## Database Connection

The app requires access to the CafriplotsR database. If you don't
provide a connection pool, the app will prompt for credentials or use
stored credentials from
[`setup_db_credentials`](https://umr-amap.github.io/cafriplotsR/reference/setup_db_credentials.md).

## Output Styles

The app supports all output styles from
[`query_plots`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md):

- auto:

  Auto-detect from plot method (default)

- minimal:

  Essential columns only

- standard:

  Common columns for general analysis

- permanent_plot:

  Structured format for single census monitoring

- permanent_plot_multi_census:

  Time-series format preserving all census columns

- transect:

  Simplified format for transect surveys

- full:

  Complete dataset with all columns

## See also

[`query_plots`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md)
for the underlying query function
[`create_pool_main`](https://umr-amap.github.io/cafriplotsR/reference/create_pool_main.md)
for database connection pooling
[`launch_taxonomic_match_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxonomic_match_app.md)
for the taxonomic matching app

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch app with default settings
launch_query_plots_app()

# Launch app in browser on specific port
launch_query_plots_app(launch.browser = TRUE, port = 8080)

# Use existing connection pool
pool <- create_pool_main()
launch_query_plots_app(pool_main = pool)
} # }
```
