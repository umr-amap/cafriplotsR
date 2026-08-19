# Feature Wizard Step 4: Lookup Matching - Server

Feature Wizard Step 4: Lookup Matching - Server

## Usage

``` r
mod_feat_step4_lookup_server(id, feature_data, feature_config, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- feature_data:

  Reactive containing the prepared feature data

- feature_config:

  Reactive containing the feature configuration

- con:

  Reactive containing database connection pool

- i18n:

  Reactive returning translator object

## Value

List of reactives: data (matched data), complete (boolean)
