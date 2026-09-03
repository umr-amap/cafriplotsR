# Per-plot count of rows whose tag could not be matched

Only the first ten unmatched rows are listed individually, and on a long
upload those ten can all belong to one plot while other plots go
unmentioned. This adds one line per plot so the whole picture is
visible.

## Usage

``` r
.unmatched_tag_plot_summary(plot_labels, i18n = NULL)
```

## Arguments

- plot_labels:

  Character vector of plot labels, one per unmatched row. Names or
  \`#\<id\>\` labels both work; see \[.plot_labels_for_ids()\].

- i18n:

  Translator object (not the reactive), or NULL for no translation.

## Value

A data frame of warning rows (\`row\`, \`column\`, \`warning\`), empty
when nothing is unmatched.
