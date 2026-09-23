# Render one backbone's links for the selected taxon

One line per link, with what the link is worth: a preferred link
supplies the taxon's name under that backbone, an unreviewed fuzzy or
author-mismatch link supplies nothing until someone accepts it. A
backbone the taxon is not linked to says so rather than being left out,
so the panel shows the same backbones for every taxon.

## Usage

``` r
.render_backbone_link_block(bb, rows, i18n)
```

## Arguments

- bb:

  One row of \[list_backbones()\].

- rows:

  The rows of \[get_taxon_backbone_links()\] for that backbone.

- i18n:

  A shiny.i18n translator (already un-reactived).

## Value

A shiny tag.
