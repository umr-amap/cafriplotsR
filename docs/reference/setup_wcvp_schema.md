# Setup WCVP Database Schema

Creates the WCVP-related tables in the taxa database:

- `wcvp_names`: Full WCVP dataset

- `wcvp_idtax_link`: Bridge between internal `idtax_n` and WCVP
  `plant_name_id`

- `wcvp_import_metadata`: Version tracking

## Usage

``` r
setup_wcvp_schema(con_taxa = NULL, dry_run = FALSE)
```

## Arguments

- con_taxa:

  Connection to the taxa database. If NULL, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- dry_run:

  Logical. If TRUE, prints SQL without executing. Default FALSE.

## Value

Invisible list with success status and steps completed.

## Examples

``` r
if (FALSE) { # \dontrun{
con_taxa <- call.mydb.taxa()
# Preview SQL
setup_wcvp_schema(con_taxa, dry_run = TRUE)
# Execute
setup_wcvp_schema(con_taxa)
} # }
```
