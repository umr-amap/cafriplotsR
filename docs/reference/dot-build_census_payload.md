# Turn a classified census table into measurements plus recruits

The wide table becomes one measurement row per stem per trait, and the
recruit rows become a frame shaped for \`data_individuals\`. Rows held
for review are left out unless \`include_review\` says otherwise.

## Usage

``` r
.build_census_payload(
  split,
  plots,
  traits,
  trait_mapping,
  include_review = FALSE,
  census_mode = "create",
  census_number = NA,
  census_year = NA,
  census_month = NA,
  census_day = NA,
  census_map = NULL
)
```

## Arguments

- split:

  A \`census_split\` from \[split_census_table()\].

- plots:

  Selected plots, with \`plot_name\` and \`id_liste_plots\`.

- traits:

  Trait table from \`traitlist\`.

- trait_mapping:

  Named character vector, column to trait name.

- include_review:

  Treat reviewed rows as recruits?

- census_mode:

  \`"create"\` or \`"existing"\`.

- census_number, census_year, census_month, census_day:

  Census identity when creating one.

- census_map:

  Census map when reusing one.

## Value

List with \`data\` (long measurements) and \`config\`.
