# Create one census record per plot

Refuses to create a census number a plot already carries — a duplicate
census record silently splits a campaign's measurements in two.

## Usage

``` r
.create_census_subplots(
  plot_ids,
  census_number,
  year,
  month = NA,
  day = NA,
  con
)
```

## Arguments

- plot_ids:

  Integer vector of plot ids.

- census_number:

  Census number to create.

- year, month, day:

  Census date parts; \`month\` and \`day\` may be \`NA\`.

- con:

  Database connection.

## Value

Data frame with \`id_table_liste_plots\` and \`id_sub_plots\`.
