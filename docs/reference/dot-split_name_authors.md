# Separate the name part of a taxonomic string from its authorship

Takes the tokens that follow "Genus epithet" and decides, one by one,
which belong to the name (infraspecific ranks and their epithets) and
which are authorship. Authors and infraspecific ranks interleave -
"Anthonotha macrophylla P.Beauv. var. oblongifolia (Baker f.)
J.Leonard" - so the scan cannot simply stop at the first author it
meets.

A bare lowercase word that is not introduced by a rank marker stays in
the name: it is far more likely a sloppy infraspecific epithet than an
author.

## Usage

``` r
.split_name_authors(parts)
```

## Arguments

- parts:

  Character vector of whitespace-separated tokens

## Value

A list with \`name\` and \`authors\`, both character vectors, each in
the order the tokens appeared
