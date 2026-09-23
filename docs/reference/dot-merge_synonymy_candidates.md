# Merge the synonymy candidates suggested by several backbones

Each backbone is asked separately which internal taxa share the accepted
name of the identifier selected in it, so the same taxon can come back
from two backbones. This keeps one row per internal taxon and collapses
what each backbone said about it into a single \`sources\` string, so
the user is offered each candidate once, with the evidence for it.

## Usage

``` r
.merge_synonymy_candidates(frames)
```

## Arguments

- frames:

  List of data frames from \[.backbone_synonymy_candidates()\], each
  with the extra columns \`backbone\` and \`backbone_label\`. \`NULL\`
  entries and empty frames are ignored.

## Value

A data frame with one row per \`idtax_n\` and an added \`sources\`
column, or \`NULL\` when no backbone suggested anything.
