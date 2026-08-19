# Apply citation backfill from a manually filled data frame

Takes a data frame (typically the output of
\`export_taxa_traits_for_citation_backfill()\` after manual editing) and
updates \`id_citation\` in \`taxa_traits_measures\` for each row where
\`id_citation\` is not NA. Rows with NA are skipped.

## Usage

``` r
apply_citation_backfill(data, con = NULL, execute = FALSE, batch_size = 1000L)
```

## Arguments

- data:

  Data frame with at minimum two columns: \`id_trait_measures\`
  (integer, primary key) and \`id_citation\` (integer, FK to
  \`table_citations\`). Additional columns are ignored.

- con:

  Database connection to \`plots_transects\`. If NULL, calls
  \`call.mydb()\`.

- execute:

  Logical. If FALSE (default), shows a preview of what would be updated
  without modifying the database.

- batch_size:

  Integer. Number of rows per UPDATE batch (default 1000).

## Value

Invisible integer: number of rows updated.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Export, fill manually, apply
df <- export_taxa_traits_for_citation_backfill(con)
# ... fill df$id_citation ...
apply_citation_backfill(df, con = con)              # dry run
apply_citation_backfill(df, con = con, execute = TRUE)

# Or from Excel
df <- readxl::read_excel("traits_to_cite.xlsx", sheet = "traits")
apply_citation_backfill(df, con = con, execute = TRUE)
} # }
```
