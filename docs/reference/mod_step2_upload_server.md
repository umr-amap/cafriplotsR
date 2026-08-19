# Step 2 Module: Upload Data - Server

Step 2 Module: Upload Data - Server

## Usage

``` r
mod_step2_upload_server(id, import_type, config, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- import_type:

  Reactive value containing import type ("plots" or "individuals")

- config:

  Reactive value containing import configuration

- con:

  Reactive database connection pool

- i18n:

  Translator object from shiny.i18n

## Value

Reactive value containing uploaded data (data frame)
