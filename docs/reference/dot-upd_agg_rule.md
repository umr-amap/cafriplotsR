# How the extraction treats one feature's records

The rule is what the extraction \*does\*, not how many records happen to
be there: a single record still goes through the same mean or join, and
the caller has \`n_records\` when it wants to say "one record".

## Usage

``` r
.upd_agg_rule(entity, grp)
```

## Value

One of \`"census"\`, \`"per_census"\`, \`"mean"\`, \`"concat"\`,
\`"not_extracted"\` or \`"other"\`.
