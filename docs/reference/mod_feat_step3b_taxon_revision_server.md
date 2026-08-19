# Server for the taxon revision step

Server for the taxon revision step

## Usage

``` r
mod_feat_step3b_taxon_revision_server(id, split_result, con, con_taxa, i18n)
```

## Arguments

- id:

  Module id.

- split_result:

  Reactive returning the \`census_split\`.

- con, con_taxa:

  Reactives returning the two connections.

- i18n:

  Reactive translator.

## Value

Reactive returning the accepted revisions, or \`NULL\`.
