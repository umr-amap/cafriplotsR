# Build the suffix appended to a source trait name to form the derived name.

Encodes the aggregation method (and parameter where relevant) into a
short tag. Example: \`"percentile"\` with \`method_param = 95\` -\>
\`"p95"\`. All other methods use the method name as the suffix (e.g.
\`"mean"\` -\> \`"mean"\`).

## Usage

``` r
.derived_trait_suffix(method, method_param = NULL)
```
