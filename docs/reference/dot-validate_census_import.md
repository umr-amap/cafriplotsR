# Validate a prepared full-census payload

The step 3 split already decided which rows are recruits, so a tag
absent from the database is expected here rather than an error. What is
checked is what the split deliberately left to a human: the census
identity, the recruits' taxonomy, and the multi-stem grouping.

## Usage

``` r
.validate_census_import(data, config, con, i18n = NULL)
```

## Arguments

- data:

  Long measurement rows from the step 3 module.

- config:

  Configuration list from the step 3 module.

- con:

  Database connection or pool.

- i18n:

  Translator object from shiny.i18n, or \`NULL\` to leave the messages
  in English. The step renders what this returns straight into the
  validation table, so a message left untranslated here reaches a French
  user in English.

## Value

List with \`errors\` and \`warnings\`, both lists of strings.
