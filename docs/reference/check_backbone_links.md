# Check the links between internal taxa and a backbone

Read-only. Counts what keeps taxa from getting a backbone's names:

- taxa with several links and none preferred (they keep their internal
  name);

- links to an ID absent from the backbone, e.g. after an import removed
  it;

- preferred links whose synonym chain cannot be completed (missing
  target, cycle, more than `max_depth` steps);

- fuzzy links not yet verified.

## Usage

``` r
check_backbone_links(backbone, con_taxa = NULL, max_depth = 5L, verbose = TRUE)
```

## Arguments

- backbone:

  Character. Backbone code.

- con_taxa:

  Connection or pool to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- max_depth:

  Integer. Maximum number of synonym steps followed. Default 5.

- verbose:

  Logical. Print the counts. Default `TRUE`.

## Value

Invisibly, a tibble with columns `check` and `n`.

## Examples

``` r
if (FALSE) { # \dontrun{
check_backbone_links("wcvp")
} # }
```
