# Step 3 Module: Column Mapping - Server

Step 3 Module: Column Mapping - Server

## Usage

``` r
mod_step3_mapping_server(id, data, config, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- data:

  Reactive containing uploaded user data

- config:

  Reactive containing import configuration

- con:

  Reactive containing database connection pool

- i18n:

  Translator object from shiny.i18n

## Value

Reactive list containing mappings and validation status
