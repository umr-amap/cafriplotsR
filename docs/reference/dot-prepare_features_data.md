# Prepare Features Data for Insert (Internal)

Prepares trait/feature data for insertion into data_ind_measures_feat
table. Links features to individuals via id_individuals.

## Usage

``` r
.prepare_features_data(
  features_data,
  individuals_id_data,
  con,
  progress = TRUE
)
```

## Arguments

- features_data:

  Data frame with feature data

- individuals_id_data:

  Data frame with id_individuals, plot_name, tag

- con:

  Database connection

- progress:

  Show progress

## Value

Data frame ready for insert into data_ind_measures_feat
