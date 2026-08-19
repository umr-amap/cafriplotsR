# Specimen Retriever Module - Server

Specimen Retriever Module - Server

## Usage

``` r
mod_specimen_retriever_server(id, parsed_data, collector_matches, con, i18n)
```

## Arguments

- id:

  Module namespace ID

- parsed_data:

  Reactive containing parsed herbarium data from Step 2

- collector_matches:

  Reactive containing collector ID matches from Step 3

- con:

  Reactive database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive list containing retrieved specimens
