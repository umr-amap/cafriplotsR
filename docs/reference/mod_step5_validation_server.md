# Step 5 Module: Data Validation - Server

Step 5 Module: Data Validation - Server

## Usage

``` r
mod_step5_validation_server(id, data, mappings, config, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- data:

  Reactive containing uploaded user data (should be matched data from
  Step 4)

- mappings:

  Reactive containing column mappings

- config:

  Reactive containing import configuration

- con:

  Reactive containing database connection pool

- i18n:

  Reactive returning translator object from shiny.i18n

## Value

Reactive containing validation results
