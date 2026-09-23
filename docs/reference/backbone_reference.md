# What to cite for a taxonomic backbone, as a table

The same citation \[backbone_citation()\] returns, with the parts it was
built from alongside it: publisher, site, version and access date. Meant
for anything that needs more than the sentence - a table to show, a
sheet to write next to exported names, a row to list among other
citations.

The version and date are those of the import currently in the database,
so the reference describes the names actually served, not the
publisher's latest release.

## Usage

``` r
backbone_reference(backbone = NULL, con_taxa = NULL, language = c("en", "fr"))
```

## Arguments

- backbone:

  Character. One or more backbone codes. `NULL` (the default) returns
  every backbone registered in `backbone_list`. `"internal"` has no
  external reference and is dropped.

- con_taxa:

  Connection or pool to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- language:

  `"en"` (default) or `"fr"`, for the wording of the access date.

## Value

A tibble with one row per backbone and columns `code`, `name`,
`publisher`, `homepage`, `version`, `access_date` and `citation`. Zero
rows when the registry cannot be read or nothing matches; never an
error.

## See also

\[backbone_citation()\] for the sentence alone, \[query_citations()\] to
list these beside the trait citations.

## Examples

``` r
if (FALSE) { # \dontrun{
backbone_reference()
backbone_reference("wcvp")
backbone_reference("apd", language = "fr")
} # }
```
