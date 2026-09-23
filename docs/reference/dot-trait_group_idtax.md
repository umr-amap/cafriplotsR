# Taxon ids to ask the trait table for (internal helper)

Trait measurements are keyed by the taxon they were recorded on, which
may be any member of a synonym group: a name matched to a synonym often
has its traits stored on the accepted taxon, or on a sibling synonym.

\`query_taxa()\` handles this by widening the id set \*before\*
fetching - \`.resolve_synonyms()\` substitutes the accepted id and binds
every synonym of the group into the result, and that whole vector is
what reaches \`query_taxa_traits()\`. \`include_synonyms = TRUE\` does
not do this on its own: inside \`query_taxa_traits()\` the fetch runs
first, on the ids given verbatim, and synonyms are resolved only
afterwards - which regroups what came back but can no longer widen the
search. Asking for the matched id alone therefore returned nothing at
all for any name matched to a synonym.

## Usage

``` r
.trait_group_idtax(matched_taxa, con_taxa = NULL)
```

## Arguments

- matched_taxa:

  Data frame carrying \`idtax_n\` and \`idtax_good_n\`

- con_taxa:

  Connection to the taxa database, or NULL to open one

## Value

Integer vector of taxon ids covering the full synonym groups
