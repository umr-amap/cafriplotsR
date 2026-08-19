# Rows that repeat a measurement the database already holds

Compares each row of prepared measurement data against what is already
recorded, and answers on the terms the row itself sets:

## Usage

``` r
.existing_measurement_rows(data, individuals, measures)
```

## Arguments

- data:

  Data frame of prepared measurements, with \`tag\`, \`traitid\` and
  \`id_liste_plots\`, and optionally \`id_sub_plots\`.

- individuals:

  Data frame of the individuals recorded for the selected plots, with
  \`tag\`, \`id_table_liste_plots_n\` and \`id_n\`.

- measures:

  Data frame of the measurements already recorded for those individuals,
  with \`id_data_individuals\`, \`traitid\` and \`id_sub_plots\`.

## Value

List of two integer vectors of row numbers into \`data\`:
\`with_census\` (matched on individual, feature and census) and
\`without_census\` (matched on individual and feature alone).

## Details

\* A row carrying a census is a repeat only of a measurement recorded
\*\*during that same census\*\*. The same feature measured during
another campaign is a new measurement, not a duplicate, which is the
whole point of a census. \* A row carrying no census - a position, a
quadrat, anything the census link policy keeps off a campaign - is a
repeat of \*\*any\*\* recorded value of that feature for that
individual. There is no campaign to narrow the comparison to, and the
tree has one position, not one per census.

The two are returned separately because they are not the same claim, and
a single count would misreport one of them.

Values are not compared, only the existence of a measurement: whether
re-recording the same feature is a mistake or a deliberate second
reading is the user's call, not this function's.
