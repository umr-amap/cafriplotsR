# Add a trait in species trait list

See https://terminologies.gfbio.org/terms/ets/pages/index.html for
description of each field

## Usage

``` r
add_trait_taxa(
  new_trait = NULL,
  new_relatedterm = NULL,
  new_valuetype = NULL,
  new_maxallowedvalue = NULL,
  new_minallowedvalue = NULL,
  new_traitdescription = NULL,
  new_factorlevels = NULL,
  new_expectedunit = NULL,
  new_comments = NULL,
  con = NULL,
  interactive = TRUE
)
```

## Arguments

- new_trait:

  string value with new trait descritors - try to avoid space

- new_relatedterm:

  string related trait to new trait

- new_valuetype:

  string one of following 'numeric', 'integer', 'categorical',
  'ordinal', 'logical', 'character'

- new_maxallowedvalue:

  numeric if valuetype is numeric, indicate the maximum allowed value

- new_minallowedvalue:

  numeric if valuetype is numeric, indicate the minimum allowed value

- new_traitdescription:

  string full description of trait

- new_factorlevels:

  string a vector of all possible value if valuetype is categorical or
  ordinal

- new_expectedunit:

  string expected unit (unitless if none)

- new_comments:

  string any comments

## Value

nothing

## Details

Add trait and associated descriptors in trait list table

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
