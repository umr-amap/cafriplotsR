# What linking a measurement to a census actually does

The census link is not a label: \`id_sub_plots\` decides how every later
query reports the value. Collapsed by default, because the answer only
matters when the user doubts the pre-selection.

## Usage

``` r
.census_link_explainer(i18n)
```

## Arguments

- i18n:

  Translator object (already resolved, not the reactive).

## Value

A \`shiny::tags\$details\` block.

## Details

The three consequences named here are the ones in the code:
\`aggregate_numeric_features_dt()\` pivots census-linked features to
\`\<trait\>\_census\_\<n\>\` and averages unlinked ones into a single
\`\<trait\>\` column, \`enrich_census_info()\` dates a measurement from
its census subplot, and \`compute_growth()\` reads
\`stem_diameter_census\_\<n\>\` and needs two of them.
