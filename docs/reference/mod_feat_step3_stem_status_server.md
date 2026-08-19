# Feature Wizard Step 3: Compute Stem Status - Server

Feature Wizard Step 3: Compute Stem Status - Server

## Usage

``` r
mod_feat_step3_stem_status_server(id, selected_plots, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- selected_plots:

  Reactive containing selected plots data frame

- con:

  Reactive containing database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive returning list with `data` (status tibble) and `config` (list
with mode and individual_ids), or NULL if not yet confirmed
