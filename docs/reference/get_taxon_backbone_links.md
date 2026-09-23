# Every backbone link of a few taxa, preferred or not

Where \[get_backbone_names()\] answers "what name does this backbone
give me", this answers "what is this taxon linked to, and why does a
link stay silent". It returns one row per link, in every backbone, with
the match details (`match_type`, `match_score`, `verified`,
`is_preferred`) beside the name the link points at. A link that is not
preferred supplies no name until it is reviewed.

Meant for inspecting a handful of taxa - the taxon panel of
[`launch_taxo_backbone_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxo_backbone_app.md)
uses it - not for bulk work.

## Usage

``` r
get_taxon_backbone_links(idtax_n, con_taxa = NULL, backbones = NULL)
```

## Arguments

- idtax_n:

  Integer vector of internal taxon identifiers.

- con_taxa:

  Connection or pool to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- backbones:

  Character vector of backbone codes. `NULL` (default) means every
  registered backbone, including those not yet offered as a source of
  names.

## Value

A tibble with one row per link, ordered by taxon then backbone:
`idtax_n`, `backbone`, `backbone_name` (the publisher's name for it),
`is_name_source`, `external_id`, `is_preferred`, `match_type`,
`match_score`, `verified`, `in_view` (`FALSE` when the linked ID is
absent from the current import), `taxon_name`, `authors`, `status`,
`status_raw`, `accepted_external_id` and `url` (built from
`backbone_list.url_template`, `NA` when the backbone has none).

## Examples

``` r
if (FALSE) { # \dontrun{
get_taxon_backbone_links(41234)
} # }
```
