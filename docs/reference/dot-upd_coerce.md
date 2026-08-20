# Coerce a form value to the R type matching a PostgreSQL column

Comparison against the stored value happens in R, so a numeric column
must be compared as a number - otherwise \`12.50\` and \`12.5\` read as
a change.

## Usage

``` r
.upd_coerce(value, pg_type)
```
