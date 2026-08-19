# Feature Wizard Step 3: Multi-Stems - Server

Feature Wizard Step 3: Multi-Stems - Server

## Usage

``` r
mod_feat_step3_multi_stems_server(
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

  Reactive containing selected plots data

- operation_mode:

  Reactive containing operation mode string

- con:

  Reactive returning database connection/pool

- i18n:

  Reactive returning translator object

## Value

Reactive containing list(data, config) for downstream steps
