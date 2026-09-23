# Trait Citation Module - Server

Trait Citation Module - Server

## Usage

``` r
mod_trait_citation_server(id, pool, i18n)
```

## Arguments

- id:

  Module namespace ID

- pool:

  Reactive returning database connection pool

- i18n:

  Reactive returning translator

## Value

Reactive list: \`id_citation\` (integer, NA when none is selected),
\`citation\` (the selected row of \`table_citations\`, or NULL).
