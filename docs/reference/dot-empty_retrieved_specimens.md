# Empty specimen table with the columns the retriever needs

\`query_specimens()\` returns the raw, un-enriched (and possibly
zero-column) table when nothing matches, which breaks the downstream
join and mutate. This gives a zero-row table carrying the columns the
retriever relies on.

## Usage

``` r
.empty_retrieved_specimens()
```

## Value

A zero-row tibble with columns \`id_specimen\`, \`id_colnam\`,
\`colnbr\`, \`idtax_n\`, \`idtax_f\` and \`colnam\`.
