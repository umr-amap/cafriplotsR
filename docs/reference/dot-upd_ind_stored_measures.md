# Measurements already stored for an individual

The ids the checks need, which the feature records of section 4 do not
carry: the feature id and the census of each measurement.

## Usage

``` r
.upd_ind_stored_measures(id_ind, con)
```

## Arguments

- id_ind:

  Integer(1), \`data_individuals.id_n\`.

- con:

  A DBI connection.

## Value

A data frame with \`id_trait_measures\`, \`traitid\`, \`id_sub_plots\`,
\`traitvalue\` and \`traitvalue_char\`.
