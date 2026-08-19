# Feature Wizard Step 6: Import - Server

Feature Wizard Step 6: Import - Server

## Usage

``` r
mod_feat_step6_import_server(
  id,
  matched_data,
  feature_config,
  selected_plots,
  operation_mode,
  validation_result,
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

- validation_result:

  Reactive containing validation result

- con:

  Reactive containing database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive containing import result
