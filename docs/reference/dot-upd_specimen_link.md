# The specimen whose identification governs an individual

Highest link priority first, then the most recent determination date -
the order \`merge_individuals_taxa()\` sorts by before taking one link
per individual.

## Usage

``` r
.upd_specimen_link(id_ind, con)
```

## Value

A one-row data frame, or \`NULL\` when nothing is linked.
