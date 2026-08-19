# Specimen Column Synonym Dictionary (Internal)

Maps each specimen database field to a vector of common column-name
variations (including French equivalents). Used to feed the shared
[`map_user_columns`](https://umr-amap.github.io/cafriplotsR/reference/map_user_columns.md)
engine so specimen auto-detection matches the quality of the
plot/individual import wizard.

## Usage

``` r
.get_specimen_column_synonyms()
```

## Value

Named list: names are specimen database fields, values are character
vectors of synonyms.
