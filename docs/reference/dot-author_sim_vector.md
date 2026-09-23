# Similarity between the authorship asked for and the one each row carries

Returns zeros when authorship was not requested, when the input carries
none, or when the backbone has no author column - so callers can always
add it to an ordering key without branching.

## Usage

``` r
.author_sim_vector(rows, parsed, include_authors)
```

## Arguments

- rows:

  A slice of the backbone

- parsed:

  Parsed input name

- include_authors:

  Whether authorship was requested

## Value

Numeric vector, one score per row of \`rows\`
