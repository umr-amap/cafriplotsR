# Fetch subplot observation features, one row per record

Unlike \[fetch_subplot_obs_features()\], the stored columns are returned
untouched: the person's name of a people feature goes in its own
\`colnam\` column instead of overwriting \`typevalue_char\`, so the
table can be edited and written back.

## Usage

``` r
fetch_subplot_obs_features_long(subplot_ids, con)
```
