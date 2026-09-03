# Annotate feature records with what the extracted table shows for them

The extraction does not treat every feature the same way, so neither can
this. \`aggregate_plot_features()\` averages numeric subplot features,
joins character and table-referenced ones, and hands \`census\` rows to
\`extract_census_dates()\` instead - a plot table carries \`n_census\`,
\`first_census\`, \`last_census\` and one \`date_census_N\` column per
census, never the mean of the census numbers. On the individual side
\`aggregate_numeric_features_dt()\` /
\`aggregate_character_features_dt()\` aggregate \*within\* each census,
so a census-linked trait becomes one column per census rather than one
value.

## Usage

``` r
.upd_annotate_aggregation(records, entity = c("plot", "individual"))
```

## Arguments

- records:

  Feature records from one of the two resolvers.

- entity:

  Either \`"plot"\` or \`"individual"\`.

## Value

\`records\` with \`n_records\`, \`agg_rule\` and \`aggregate_display\`.

## Details

\`agg_rule\` records which of those happens; \`aggregate_display\` is
what the extracted table would show, and is \`NA\` when there is no
single value to show.
