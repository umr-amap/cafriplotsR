# Feature Wizard Step 3: Full Census Import - Server

Feature Wizard Step 3: Full Census Import - Server

## Usage

``` r
mod_feat_step3_census_import_server(id, selected_plots, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- selected_plots:

  Reactive containing data.frame of selected plots

- con:

  Reactive containing database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive containing list(data, config)
