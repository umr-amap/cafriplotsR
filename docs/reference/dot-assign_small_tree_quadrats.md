# Assign each small-tree stem to a sampled quadrat by tag range

Within a parent plot the quadrats were tagged one after another, so
`firsttag` cuts the tag sequence into disjoint ranges: a stem belongs to
the quadrat with the largest `firsttag` not above its tag. This is the
only key the two files genuinely share — `plot_plot_name_old`
distinguishes quadrats by a hand-typed trailing underscore, which the
teams do not apply consistently.

## Usage

``` r
.assign_small_tree_quadrats(trees, quadrats)
```

## Arguments

- trees:

  Normalised tree data frame (parent_plot_name, tag, plot_name_raw).

- quadrats:

  Quadrat units from
  [`.prepare_openforis_small_tree_quadrats()`](https://umr-amap.github.io/cafriplotsR/reference/dot-prepare_openforis_small_tree_quadrats.md).

## Value

List with `trees` (assignable rows, with `plot_name` set), `unassigned`
(rows dropped, or NULL) and `mismatches` (rows the raw spelling
disagrees about, or NULL).

## Details

That column is still worth something as a cross-check: every stem
sharing a raw spelling ought to land in the same quadrat. Where they do
not, the minority rows are reported and the tag is believed.
