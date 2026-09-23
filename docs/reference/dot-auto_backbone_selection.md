# The unambiguous hit of each backbone, if it has one

Picks, per backbone, the row that can be linked without a person looking
at it: an exact match whose name is the one that was searched for, and
the only such row in that backbone. Two rows carrying the same name
(homonyms, or a name both accepted and synonymised) are ambiguous and
left for the user.

## Usage

``` r
.auto_backbone_selection(results, name)
```

## Arguments

- results:

  A data frame from \[search_all_backbones()\].

- name:

  Character. The name that was searched for.

## Value

A named character vector of external identifiers, named by backbone
code. Empty when no backbone has an unambiguous hit.
