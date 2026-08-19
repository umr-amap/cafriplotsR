# Validate a prepared full-census payload

The step 3 split already decided which rows are recruits, so a tag
absent from the database is expected here rather than an error. What is
checked is what the split deliberately left to a human: the census
identity, the recruits' taxonomy, and the multi-stem grouping.

## Usage

``` r
.validate_census_import(data, config, con)
```

## Arguments

- data:

  Long measurement rows from the step 3 module.

- config:

  Configuration list from the step 3 module.

- con:

  Database connection or pool.

## Value

List with \`errors\` and \`warnings\`, both lists of strings.
