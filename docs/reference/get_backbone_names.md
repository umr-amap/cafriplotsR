# Get a backbone's names for internal taxa

Looks up, for each internal taxon, the name its \*\*preferred\*\* link
points to in another backbone. With `resolve_synonyms = TRUE`, synonym
pointers are followed until a name without a pointer is reached,
whatever the names' status says.

A taxon keeps `name_source = "internal"` when it has no preferred link
or when the linked ID is absent from the backbone (e.g. removed by a
newer import). When a chain cannot be completed (missing target, cycle,
more than `max_depth` steps), the matched name is returned; see
\[check_backbone_links()\].

## Usage

``` r
get_backbone_names(
  idtax_n,
  backbone,
  con_taxa = NULL,
  resolve_synonyms = TRUE,
  max_depth = 5L
)
```

## Arguments

- idtax_n:

  Integer vector of internal taxon IDs.

- backbone:

  Character. Code of a backbone registered in the taxa database, e.g.
  `"wcvp"`.

- con_taxa:

  Connection or pool to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- resolve_synonyms:

  Logical. Follow synonym pointers to the accepted name. Default `TRUE`.

- max_depth:

  Integer. Maximum number of synonym steps followed. Default 5.

## Value

A tibble with one row per unique `idtax_n`:

- `backbone_name_id`:

  ID of the linked name in the backbone.

- `backbone_accepted_id`:

  ID of the accepted name when the linked name was followed to it, `NA`
  otherwise. With `resolve_synonyms = FALSE`, the raw pointer.

- `backbone_taxon_name`, `backbone_family`, `backbone_genus`,
  `backbone_species`, `backbone_authors`:

  Name parts of the returned name.

- `backbone_status`:

  `"accepted"`, `"synonym"` or `"other"`.

- `backbone_status_raw`:

  The backbone's own status value.

- `name_source`:

  `backbone` or `"internal"`.

## Examples

``` r
if (FALSE) { # \dontrun{
get_backbone_names(c(123, 456), "wcvp")
} # }
```
