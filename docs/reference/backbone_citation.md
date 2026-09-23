# How to cite a taxonomic backbone

Builds the citation from what the database records about the backbone:
the formula its publisher asks for (`backbone_list.citation_template`),
filled with the name, publisher and site from `backbone_list` and the
version and date of the current import from `backbone_import`. The date
stated is the one the names in the database were taken from, not
today's, so two people citing the same query cite the same thing.

Publishers word their citations differently, so the wording is data, not
code. APD:

*African Plant Database (version 4.0.0). Conservatoire et Jardin
botaniques de la Ville de Genève and South African National Biodiversity
Institute, Pretoria, accessed September 2026, from
\<http://africanplantdatabase.ch\>.*

WCVP, in Kew's own formula:

*Govaerts R. (ed.) (2026). WCVP: World Checklist of Vascular Plants,
version 13. Facilitated by the Royal Botanic Gardens, Kew. Published on
the Internet; http://sftp.kew.org/pub/data-repositories/WCVP/ Retrieved
8 January 2026.*

A backbone with no formula recorded gets a plain one built from its
name, publisher, version, date and site.

## Usage

``` r
backbone_citation(backbone, con_taxa = NULL, language = c("en", "fr"))
```

## Arguments

- backbone:

  Character. Backbone code, e.g. `"apd"` or `"wcvp"`. `"internal"` has
  no external citation.

- con_taxa:

  Connection or pool to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- language:

  `"en"` (default) or `"fr"`, for the wording of the access date.

## Value

A single string, or `NA_character_` when the backbone is unknown or has
no import.

## Examples

``` r
if (FALSE) { # \dontrun{
backbone_citation("apd")
backbone_citation("apd", language = "fr")
} # }
```
