# List available output styles

Returns a table summarising the output styles available to
\[query_plots()\]. Each row corresponds to one style and is intended to
help users pick the right value for the \`output_style\` argument.

## Usage

``` r
list_output_styles()
```

## Value

A \[tibble\]\[tibble::tibble-package\] with one row per style and the
columns:

- \`name\`:

  Character. Style identifier to pass as \`output_style = "\<name\>"\`.

- \`description\`:

  Character. Short description of the style.

- \`additional_tables\`:

  Character. Comma-separated list of extra tables the style produces
  (besides \`metadata\` and \`individuals\`), or \`""\` if none.

- \`n_metadata_columns\`:

  Integer. Number of explicitly listed metadata columns, or \`NA\` when
  the style keeps \`"all"\`.

- \`n_individuals_columns\`:

  Integer. Same for the individuals table.

- \`n_keep_patterns\`:

  Integer. Number of regex patterns added on top of the explicit
  columns.

- \`n_remove_patterns\`:

  Integer. Number of regex patterns used to drop columns.

## See also

\[get_output_style()\] to retrieve a single style's full configuration;
\[query_plots()\] to use a style.

## Examples

``` r
list_output_styles()
#> # A tibble: 7 × 7
#>   name    description additional_tables n_metadata_columns n_individuals_columns
#>   <chr>   <chr>       <chr>                          <int>                 <int>
#> 1 minimal Essential … ""                                10                     8
#> 2 standa… Standard o… ""                                15                    11
#> 3 perman… Organized … "censuses, heigh…                 18                    17
#> 4 perman… Organized … "censuses, heigh…                 17                    14
#> 5 transe… Simplified… ""                                11                    10
#> 6 full    Complete e… ""                                NA                    NA
#> 7 census… One row pe… ""                                13                    10
#> # ℹ 2 more variables: n_keep_patterns <int>, n_remove_patterns <int>
```
