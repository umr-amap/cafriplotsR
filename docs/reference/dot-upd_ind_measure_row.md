# Build the data_traits_measures row for a new measurement

The value goes where the feature wizard puts it: \`traitvalue\` for
\`numeric\`, \`integer\` and \`table_colnam\` features,
\`traitvalue_char\` for the others. The plot, taxon and specimen links
are copied from the individual, as the wizard does.

## Usage

``` r
.upd_ind_measure_row(
  individual,
  trait,
  value,
  id_sub_plots = NA_integer_,
  year = NA_integer_,
  month = NA_integer_,
  day = NA_integer_,
  issue = NA_character_,
  plot_name = NA_character_
)
```

## Arguments

- individual:

  One-row data frame of \`data_individuals\` (\`id_n\`,
  \`id_table_liste_plots_n\`, and \`id_diconame_n\`, \`id_specimen\`
  when present).

- trait:

  One-row data frame of \`traitlist\` (\`id_trait\`, \`valuetype\`).

- value:

  The value, as typed.

- id_sub_plots:

  Integer(1) or \`NA\`. The census the value belongs to.

- year, month, day:

  Integer(1) or \`NA\`. Measurement date, when it differs from the
  census's.

- issue:

  Character(1) or \`NA\`.

- plot_name:

  Character(1) or \`NA\`, kept as \`original_plot_name\`.

## Value

A one-row data frame. Stops on an empty value, or on a value a numeric
feature cannot hold.
