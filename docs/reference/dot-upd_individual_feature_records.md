# Individual feature records behind the columns of an extracted individual table

Each row of \`data_traits_measures\` for the individual, with the census
it belongs to and the same aggregation annotation as the plot resolver.
A trait measured at several censuses has \`n_records \> 1\`, which is
exactly the case \`detect_feature_changes()\` refuses.

## Usage

``` r
.upd_individual_feature_records(id_ind, con)
```

## Arguments

- id_ind:

  Integer, \`data_individuals.id_n\`.

- con:

  A DBI connection.

## Value

A tibble, one row per measurement; zero rows if there are none.
