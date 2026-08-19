# Features that are never attached to a census

Where a stem sits, what kind of plant it is and what was determined from
it in a laboratory are properties of the tree, not of the campaign that
measured it. This is the built-in default used when the database does
not state a policy of its own.

## Usage

``` r
.default_census_link_policy()
```

## Value

Named character vector, feature name to \`"never"\`.

## Details

Every feature listed is also unlinked in every row already recorded —
\`quadrat\`, for instance, 0 of 147,894 — and the classification was
confirmed against \[census_link_evidence()\] rather than inferred from
the counts alone. Anything absent from this list is attached to the
census.
