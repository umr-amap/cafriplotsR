# Query citations from table_citations

Returns rows from \`table_citations\`, optionally filtered by ID,
citation key, dataset name, or a free-text pattern matched against
\`citation_key\`, \`authors\`, \`title\`, and \`dataset_name\`.

## Usage

``` r
query_citations(
  con = NULL,
  ids = NULL,
  keys = NULL,
  dataset_names = NULL,
  pattern = NULL
)
```

## Arguments

- con:

  Database connection to \`plots_transects\`. If NULL, calls
  \`call.mydb()\`.

- ids:

  Integer vector of \`id_citation\` values to retrieve.

- keys:

  Character vector of \`citation_key\` values to retrieve.

- dataset_names:

  Character vector of \`dataset_name\` values to filter on.

- pattern:

  Character string. Case-insensitive substring matched against
  \`citation_key\`, \`authors\`, \`title\`, and \`dataset_name\`.

## Value

A data frame of matching rows, or all rows when no filter is supplied.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# All citations
query_citations(con)

# By key
query_citations(con, keys = "TRY_v6")

# Free-text search
query_citations(con, pattern = "TRY")
} # }
```
