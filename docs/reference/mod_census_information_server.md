# Census Information Module - Server

Census Information Module - Server

## Usage

``` r
mod_census_information_server(id, imported_plots, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- imported_plots:

  Reactive containing the imported plot data with plot_name and
  id_liste_plots

- con:

  Reactive containing database connection pool

- i18n:

  Reactive returning translator object from shiny.i18n

## Value

Reactive containing census addition result
