# List the taxonomic backbones available in the taxa database

The internal backbone (`table_taxa`) is always available and is not
listed. Other backbones are registered in `backbone_list`, each with a
mirror of its names exposed through a view.

If `backbone_list` cannot be read (database not migrated, no
permission), an empty tibble is returned and only the internal backbone
is usable.

## Usage

``` r
list_backbones(con_taxa = NULL, name_sources_only = TRUE)
```

## Arguments

- con_taxa:

  Connection or pool to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- name_sources_only:

  Logical. If `TRUE` (default), only backbones offered to users as a
  source of names (`is_name_source`).

## Value

A tibble with columns `id_backbone`, `code`, `name`, `publisher`,
`names_view`, `url_template`, `is_name_source`.

## Examples

``` r
if (FALSE) { # \dontrun{
list_backbones()
} # }
```
