# Default minimum similarity for the taxonomic matching app

The threshold the app starts from, shared by the automatic matching
field on the Auto Match tab and the suggestions slider on the Review tab
so the two cannot drift apart.

It lives here, and not in an argument to
\`launch_taxonomic_match_app()\`, because the threshold is a per-run
decision made against the list in front of you: the user raises or
lowers the field, sees how many names fall to manual review, and runs
again. An argument fixed at launch was answering the question too
early - and, because the field always won, was silently ignored.

## Usage

``` r
.default_min_similarity()
```

## Value

Numeric scalar between 0 and 1.
