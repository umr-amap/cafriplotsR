# What the recorded data says about census links, feature by feature

Reports how often each feature is already attached to a census, so the
policy can be settled against the data rather than from memory. A
feature at 0 has; anything in between is usually old data loaded before
censuses were recorded rather than a genuine ambiguity.

## Usage

``` r
census_link_evidence(con)
```

## Arguments

- con:

  Database connection or pool.

## Value

Data frame with \`trait\`, \`category\`, \`n\`, \`n_linked\`,
\`pct_linked\` and the \`policy\` currently in force, ordered by volume.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
census_link_evidence(con)
} # }
```
