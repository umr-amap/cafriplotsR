# Plot Metadata Viewer Module - Server

Server logic for plot metadata display and selection

## Usage

``` r
mod_plot_metadata_viewer_server(id, metadata, i18n)
```

## Arguments

- id:

  Module namespace ID

- metadata:

  Reactive containing plot metadata from query_plots()

- i18n:

  Reactive returning shiny.i18n translator

## Value

A reactive containing selected plot IDs
