# Import a full census in one transaction

Creates or reuses the census record, inserts the recruits, resolves
their multi-stem grouping, then writes the measurements for every stem —
all inside a single transaction, so a failure at any point leaves the
database as it was rather than half-populated.

## Usage

``` r
.execute_census_import(data, config, con, dry_run = TRUE, i18n = NULL)
```

## Arguments

- data:

  Long measurement rows carrying \`plot_name\`, \`tag\`,
  \`id_liste_plots\`, \`traitid\` and the value columns.

- config:

  Configuration list from the step 3 module. Uses \`recruits\`,
  \`census_mode\`, \`census_map\`, \`census_number\`, \`census_year\`,
  \`census_month\`, \`census_day\`, \`features_field\` and
  \`features_field_mappings\`.

- con:

  Database connection or pool.

- dry_run:

  Report what would happen without writing.

- i18n:

  Optional translator.

## Value

List with \`success\`, \`dry_run\`, counts (\`n_census_records\`,
\`n_recruits\`, \`n_stem_grouping\`, \`n_measurements\`) and a
\`message\`.
