# Resolve taxon ids to the accepted taxon they name

\`table_taxa\` keeps a synonym as a row of its own, pointing at the
accepted name through \`idtax_good_n\`. Two ids that resolve to the same
accepted taxon name the same plant, so comparing the raw ids reports a
revision where nothing was revised — which is how a census that renamed
nothing turns into hundreds of identifications to review.

## Usage

``` r
.resolve_idtax(idtax, map = NULL)
```

## Arguments

- idtax:

  Vector of taxon ids, character or numeric.

- map:

  Frame with \`idtax_n\` and \`idtax_accepted\`, from
  \[.fetch_accepted_idtax()\], or \`NULL\`.

## Value

Integer vector: the accepted id where the map knows one, the id itself
otherwise. An absent or incomplete map therefore degrades to the raw
comparison rather than silently merging unrelated taxa.

## Details

One hop, which is the convention everywhere else in the package
(\`ifelse(is.na(idtax_good_n), idtax_n, idtax_good_n)\`): an accepted
name points at itself or at nothing.
