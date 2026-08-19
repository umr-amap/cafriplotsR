# Plot Filters Module - Server

Server logic for plot query filters

## Usage

``` r
mod_plot_filters_server(id, pool, i18n)
```

## Arguments

- id:

  Module namespace ID

- pool:

  Database connection pool (reactive or static)

- i18n:

  Reactive returning shiny.i18n translator

## Value

A reactive list containing: - filters: Named list of filter values -
execute_trigger: Reactive counter that increments on execute
