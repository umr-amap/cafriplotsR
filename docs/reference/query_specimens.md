# Query Specimens

Modern query interface for specimens with builder pattern support

## Usage

``` r
query_specimens(
  collector = NULL,
  id_colnam = NULL,
  number = NULL,
  number_min = NULL,
  number_max = NULL,
  id_specimen = NULL,
  idtax_n = NULL,
  interactive = TRUE,
  extract_linked_individuals = FALSE,
  subset_columns = TRUE,
  show_html = TRUE,
  html_max = 20,
  con = NULL,
  con.taxa = NULL
)
```

## Arguments

- collector:

  Character vector of collector names (fuzzy match)

- id_colnam:

  Integer vector of collector IDs (exact match)

- number:

  Integer vector of specimen numbers (exact match)

- number_min:

  Minimum specimen number (range query)

- number_max:

  Maximum specimen number (range query)

- id_specimen:

  Integer vector of specimen IDs (direct fetch)

- idtax_n:

  Integer vector of taxonomy IDs

- interactive:

  Logical, use interactive fuzzy matching for collectors

- extract_linked_individuals:

  Logical, also fetch linked individuals

- subset_columns:

  Logical, return subset of columns vs all columns

- show_html:

  Logical. If TRUE (default) and the number of returned specimens is at
  most `html_max`, the results are displayed as a transposed HTML table
  in the RStudio Viewer (one column per specimen) using
  [`print_table()`](https://umr-amap.github.io/cafriplotsR/reference/print_table.md).

- html_max:

  Integer. Maximum number of specimens for which the HTML visualisation
  is triggered automatically (default 20).

- con:

  Database connection (if NULL, creates new connection)

- con.taxa:

  Taxa database connection (if NULL, creates new connection)

## Value

Data frame with specimen records, or list if
extract_linked_individuals=TRUE

## Examples

``` r
if (FALSE) { # \dontrun{
# Query by collector name
query_specimens(collector = "Dauby")

# Query by collector ID and number range
query_specimens(id_colnam = 123, number_min = 1000, number_max = 2000)

# Query by specimen IDs
query_specimens(id_specimen = c(123, 456, 789))

# Query with linked individuals
result <- query_specimens(collector = "Dauby", extract_linked_individuals = TRUE)
result$specimens  # Specimen records
result$linked_individuals  # Linked individual records
} # }
```
