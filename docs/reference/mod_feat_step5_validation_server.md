# Feature Wizard Step 5: Validation - Server

Feature Wizard Step 5: Validation - Server

## Usage

``` r
mod_feat_step5_validation_server(
  id,
  matched_data,
  feature_config,
  selected_plots,
  operation_mode,
  con,
  i18n
)
```

## Arguments

- id:

  Module namespace ID

- matched_data:

  Reactive containing the matched feature data

- feature_config:

  Reactive containing the feature configuration

- selected_plots:

  Reactive containing selected plots data

- operation_mode:

  Reactive containing operation mode string

- con:

  Reactive containing database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive containing validation result
