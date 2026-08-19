# Import WCVP Names into Database

Imports the WCVP dataset from the `rWCVPdata` package into the
`wcvp_names` table. Requires `rWCVPdata` and `rWCVP` packages.

## Usage

``` r
import_wcvp_names(
  con_taxa = NULL,
  batch_size = 50000,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- con_taxa:

  Connection to the taxa database. If NULL, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- batch_size:

  Number of rows to insert per batch. Default 50000.

- force:

  Logical. If TRUE, reimports even if the same version is already
  present.

- verbose:

  Logical. Show progress messages. Default TRUE.

## Value

Invisible list with import results (version, record_count).

## Examples

``` r
if (FALSE) { # \dontrun{
con_taxa <- call.mydb.taxa()
import_wcvp_names(con_taxa)
} # }
```
