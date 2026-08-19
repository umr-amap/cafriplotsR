# Feature Wizard Step 3: Individual Measurements - Server

Feature Wizard Step 3: Individual Measurements - Server

## Usage

``` r
mod_feat_step3_measurements_server(
  id,
  selected_plots,
  operation_mode,
  con,
  i18n
)
```

## Arguments

- id:

  Module namespace ID

- selected_plots:

  Reactive containing data.frame of selected plots

- operation_mode:

  Reactive containing mode string

- con:

  Reactive containing database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive containing list(data, config)
