# Legacy table_idtax Update Method

Original method for updating table_idtax using dbWriteTable. Requires
admin/write permissions. Used as fallback when materialized view
approach is not available.

## Usage

``` r
legacy_update_taxa_link_table(con = NULL, con_taxa = NULL)
```

## Arguments

- con:

  Main database connection

- con_taxa:

  Taxa database connection

## Value

List with success status
