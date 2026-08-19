# Update fields of an existing citation

Updates one or more columns of a single row in \`table_citations\`,
identified by its \`id_citation\`.

## Usage

``` r
update_citation(id_citation, fields, con = NULL, execute = FALSE)
```

## Arguments

- id_citation:

  Integer. The \`id_citation\` of the row to update.

- fields:

  Named list of field-value pairs to update. Names must be valid column
  names of \`table_citations\` (excluding \`id_citation\`).

- con:

  Database connection to \`plots_transects\`. If NULL, calls
  \`call.mydb()\`.

- execute:

  Logical. If FALSE (default), shows changes without applying them. Set
  to TRUE to apply.

## Value

Invisible TRUE on success, or invisible FALSE if not executed.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Preview
update_citation(1, list(doi = "10.1111/gcb.14904", url = "https://..."), con)

# Apply
update_citation(1, list(doi = "10.1111/gcb.14904"), con, execute = TRUE)
} # }
```
