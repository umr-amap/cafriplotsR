# Add a type in subplot table

Add feature and associated descriptors in subplot list table

## Usage

``` r
add_subplottype(
  new_type = NULL,
  new_valuetype = NULL,
  new_maxallowedvalue = NULL,
  new_minallowedvalue = NULL,
  new_typedescription = NULL,
  new_factorlevels = NULL,
  new_expectedunit = NULL,
  new_comments = NULL,
  new_category = NULL,
  con = NULL,
  interactive = TRUE
)
```

## Arguments

- new_type:

  string value with new type descriptors - try to avoid space

- new_valuetype:

  string one of following 'numeric', 'integer', 'categorical',
  'ordinal', 'logical', 'character', 'table_colnam'

- new_maxallowedvalue:

  numeric if valuetype is numeric, indicate the maximum allowed value

- new_minallowedvalue:

  numeric if valuetype is numeric, indicate the minimum allowed value

- new_typedescription:

  string full description of type/feature

- new_factorlevels:

  string a vector of all possible value if valuetype is categorical or
  ordinal

- new_expectedunit:

  string expected unit (unitless if none)

- new_comments:

  string any comments

- new_category:

  string category for grouping features in the UI (e.g., "People",
  "Sampling", "Environment", "Observation"). Defaults to "Other".

- con:

  Database connection (optional). If NULL, will create a new connection.

- interactive:

  Logical. If TRUE (default), will ask for confirmation. Set to FALSE
  when calling from Shiny.

## Value

Invisibly returns the new feature data (tibble)

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
