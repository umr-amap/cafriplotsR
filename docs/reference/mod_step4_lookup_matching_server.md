# Step 4 Module: Lookup Matching - Server

Step 4 Module: Lookup Matching - Server

## Usage

``` r
mod_step4_lookup_matching_server(id, data, mappings, config, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- data:

  Reactive containing uploaded user data

- mappings:

  Reactive containing column mappings

- config:

  Reactive containing import configuration

- con:

  Reactive containing database connection pool

- i18n:

  Reactive returning translator object from shiny.i18n

## Value

Reactive containing matched/updated data
