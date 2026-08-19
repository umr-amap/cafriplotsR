# Feature Wizard Step 3: Standardize Observations - Server

Feature Wizard Step 3: Standardize Observations - Server

## Usage

``` r
mod_feat_step3_standardize_obs_server(id, selected_plots, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- selected_plots:

  Reactive containing selected plots data frame

- con:

  Reactive returning the database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive returning list with `data` (standardized tibble) and `config`
(mode + individual_ids), or NULL if not yet confirmed
