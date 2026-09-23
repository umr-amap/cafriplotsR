# Key matched taxa the way trait measurements come back (internal helper)

\`query_taxa_traits()\` rewrites \`idtax\` to the accepted id of the
group (\`mutate(idtax = idtax_good)\`), so measurements always come back
keyed by the accepted taxon - never by the synonym that was matched.
Joining them back on \`idtax_n\` silently dropped every synonym. This
adds the key they actually carry, using the same rule as
\`resolve_taxon_synonyms()\`.

## Usage

``` r
.with_accepted_idtax(matched_taxa)
```

## Arguments

- matched_taxa:

  Data frame carrying \`idtax_n\` and \`idtax_good_n\`

## Value

\`matched_taxa\` with an \`idtax_resolved\` column added
