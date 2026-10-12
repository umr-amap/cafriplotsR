# Fetch names and precision for a set of taxon ids

Reads the \*\*taxa\*\* database, which is a separate connection from the
one holding the plots. Showing bare \`idtax_n\` integers in the review
step would make it unusable, so this is not optional decoration.

## Usage

``` r
.fetch_taxon_names(idtax, con_taxa)
```

## Arguments

- idtax:

  Integer vector of \`idtax_n\` values.

- con_taxa:

  Connection or pool for the taxa database.

## Value

Data frame with \`idtax_n\`, \`taxon_name\`, \`tax_level\`,
\`precision\` and \`idtax_accepted\` — the id of the accepted taxon, so
the caller can tell a renaming from a redetermination without a second
query. An empty frame if nothing can be fetched — the caller degrades to
ids.
