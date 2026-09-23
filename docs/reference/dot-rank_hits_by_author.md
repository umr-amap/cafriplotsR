# Order exact-match candidates by how well their authorship fits

The name already matched exactly; several backbone rows can share it
(homonyms, or the same name under different authors). When the input
carries an author, it decides which row comes first - the whole point of
matching with authors. Without one, the order is left as it was.

## Usage

``` r
.rank_hits_by_author(hits_idx, backbone, parsed, include_authors)
```

## Arguments

- hits_idx:

  Integer positions into \`backbone\`

- backbone:

  The cached backbone tibble

- parsed:

  Parsed input name

- include_authors:

  Whether authorship was requested

## Value

\`hits_idx\`, reordered
