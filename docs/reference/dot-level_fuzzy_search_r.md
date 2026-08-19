# R-side per-level fuzzy search on cached backbone

Mirrors the per-level SQL queries in \`mod_fuzzy_suggestions_server\`
for the offline/cached-backbone code path. Returns a tibble with the
same columns the SQL path produces so downstream rendering doesn't
change.

## Usage

``` r
.level_fuzzy_search_r(backbone, name, level_filter, min_sim, max_results = 50L)
```
