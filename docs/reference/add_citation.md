# Add one or more citations to table_citations

Inserts new rows into \`table_citations\`. Rows whose \`citation_key\`
already exists in the database are skipped with a warning.

## Usage

``` r
add_citation(new_data, con = NULL, interactive = TRUE)
```

## Arguments

- new_data:

  Data frame with citation fields. The column \`citation_key\` is
  mandatory. Optional columns: \`authors\`, \`year\`, \`title\`,
  \`journal\`, \`volume\`, \`pages\`, \`doi\`, \`url\`,
  \`dataset_name\`, \`notes\`.

- con:

  Database connection to \`plots_transects\`. If NULL, calls
  \`call.mydb()\`.

- interactive:

  Logical. If TRUE (default), shows a preview and asks for confirmation
  before inserting.

## Value

Invisible data frame of actually inserted rows, or NULL if nothing was
inserted.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

add_citation(
  data.frame(
    citation_key = "TRY_v6",
    authors      = "Kattge et al.",
    year         = 2020,
    title        = "TRY plant trait database - enhanced coverage and open access",
    journal      = "Global Change Biology",
    doi          = "10.1111/gcb.14904",
    dataset_name = "TRY"
  ),
  con = con
)
} # }
```
