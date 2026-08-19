# Step 7 Module: Execute Import - Server

Step 7 Module: Execute Import - Server

## Usage

``` r
mod_step7_import_server(id, validation_result, mappings, config, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- validation_result:

  Reactive containing validation results

- mappings:

  Reactive containing column mappings

- config:

  Reactive containing import configuration

- con:

  Reactive containing database connection pool

- i18n:

  Reactive translator object from shiny.i18n

## Value

Reactive containing import result
