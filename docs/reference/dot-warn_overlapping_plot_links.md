# Warn when an extraction holds both ends of a plot link

Not gated on \`extract_plot_links\`. A parent and its child in the same
result describe overlapping ground under either relation - a
\`block_member\` child tiles its parent, a \`nested_subsample\` child
overlaps it - so any total taken across those rows counts the same stems
twice. Nothing in the numbers says so, which is the whole reason to say
it here.

## Usage

``` r
.warn_overlapping_plot_links(edges, max_show = 5L)
```

## Arguments

- edges:

  The result of \[.plot_link_edges()\].

- max_show:

  Integer, how many pairs to name before summarising.

## Value

Invisibly, the pairs that were reported.
