# Apply the processor's decisions on quadrat mismatches

Moves the stems marked `keep = "name"` to the quadrat given by
`quadrat_by_name`, the one taken from the mismatch table unless the
decision row names another. A decision is matched to a stem on parent
plot and tag, so it survives a re-export as long as those do.

## Usage

``` r
.apply_small_tree_quadrat_decisions(
  trees,
  mismatches,
  decisions,
  quadrats,
  data_dir = NULL
)
```

## Arguments

- trees:

  Assigned tree data frame (parent_plot_name, tag, quadrat_unit,
  quadrat_label).

- mismatches:

  Output of
  [`.flag_small_tree_name_mismatches()`](https://umr-amap.github.io/cafriplotsR/reference/dot-flag_small_tree_name_mismatches.md),
  or NULL.

- decisions:

  Data frame, or path of an xlsx, with parent_plot_name, tag, keep and
  optionally quadrat_by_name.

- quadrats:

  Quadrat units from
  [`.prepare_openforis_small_tree_quadrats()`](https://umr-amap.github.io/cafriplotsR/reference/dot-prepare_openforis_small_tree_quadrats.md).

- data_dir:

  Directory a bare filename in `decisions` is looked up in, or NULL.

## Value

List with `trees` (stems moved) and `mismatches` (keep and
quadrat_by_name set to what was applied; stems moved by a decision but
not flagged are appended).

## Details

Anything that cannot be applied as written is an error, not a skip: a
decision for a stem that is not there, a quadrat that was not sampled in
that parent plot, a `"name"` without a quadrat, or two decisions for one
stem. A silently dropped decision would put a stem back in the quadrat
someone had taken it out of.
