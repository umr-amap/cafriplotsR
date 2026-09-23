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
  pattern = NULL,
  backbones = TRUE,
  con_taxa = NULL,
  language = c("en", "fr")
)
```

## Arguments

- con:

  Database connection to \`plots_transects\`. If NULL, calls
  \`call.mydb()\`.

- ids:

  Integer vector of \`id_citation\` values to retrieve. Backbones have
  no \`id_citation\`, so supplying this drops them.

- keys:

  Character vector of \`citation_key\` values to retrieve. A backbone's
  key is its code in capitals, e.g. \`"WCVP"\`.

- dataset_names:

  Character vector of \`dataset_name\` values to filter on.

- pattern:

  Character string. Case-insensitive substring matched against
  \`citation_key\`, \`authors\`, \`title\`, and \`dataset_name\`.

- backbones:

  Logical. Include the taxonomic backbones. Default TRUE.

- con_taxa:

  Connection or pool to the taxa database. If NULL, one that is already
  open is used, and otherwise the backbones are skipped.

- language:

  \`"en"\` (default) or \`"fr"\`, for the wording of a backbone's access
  date.

## Value

A data frame of matching rows, or all rows when no filter is supplied,
with a \`source\` column saying where each row came from
(\`"table_citations"\` or \`"backbone"\`).

## Taxonomic backbones

The taxonomic backbones - APD, WCVP, any other the taxa database
registers - are not rows of \`table_citations\`. They are described in
the taxa database, where their version and access date are updated by
each import, and they are listed here so that one call answers "what do
I have to cite?" whether the answer is a trait dataset or a name source.

Their rows carry \`source = "backbone"\` and no \`id_citation\`: nothing
points at them with a foreign key, and they must not be used as one. The
sentence to paste into a methods section is in \`notes\`.
\[backbone_reference()\] returns the same thing with the version and
access date in columns of their own.

Backbones are included only when a taxa connection is already open or is
passed as \`con_taxa\`. Reading the main database never prompts for a
second password on its own.

## See also

\[backbone_reference()\] for the backbones alone, with their version and
access date.

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

# What to cite for the names themselves
query_citations(con, pattern = "WCVP")$notes
} # }
```
