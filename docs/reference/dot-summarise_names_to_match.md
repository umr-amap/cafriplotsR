# Summarise the names a matching run would look up

Describes the column the user selected the way the matching pipeline
will read it: distinct values, how often each occurs, the normalised
form that is actually searched, and the rank the parser detects. This is
what lets a user notice, before starting, that they pointed the app at
the wrong column.

Counting follows the pipeline: distinct \*raw\* values, because that is
what \`mod_auto_matching_server()\` iterates over. Two spellings that
normalise to the same string are two lookups, and are reported as two
names.

## Usage

``` r
.summarise_names_to_match(values, max_parse = 5000L)
```

## Arguments

- values:

  Character vector, the selected column's contents.

- max_parse:

  Integer, above this many distinct names the per-name rank detection is
  skipped (it is only a display aid, and the parse is the one part of
  this that grows with the list). Default 5000.

## Value

A list with:

- `n_rows`: number of rows in the column

- `n_missing`: rows with no usable name (NA or blank)

- `n_unique`: distinct names that would be looked up

- `rank_counts`: named integer vector, distinct names per detected rank
  (empty when the parse was skipped)

- `parsed`: logical, whether rank detection ran

- `names`: data.frame of `name`, `searched`, `n` and `rank`, most
  frequent first
