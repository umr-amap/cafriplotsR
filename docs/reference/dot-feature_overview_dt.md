# The feature overview table

The feature overview table

## Usage

``` r
.feature_overview_dt(summary, i18n, page_length = 10)
```

## Arguments

- summary:

  A \`.upd_feature_summary()\` result. A \`plot_name\` column, when
  present, becomes the first column so several plots can be shown at
  once.

- i18n:

  A \`shiny.i18n\` translator (already resolved).

- page_length:

  Rows per page.

## Value

A \`DT::datatable\`, with rows backed by several records highlighted.
