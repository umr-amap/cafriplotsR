# Get Column Synonym Dictionary

Returns a comprehensive dictionary mapping common column name variations
to standard database column names. Includes both textual variations and
domain-specific semantic equivalents (e.g., dbh = stem_diameter).

## Usage

``` r
.get_column_synonyms()
```

## Value

Named list where names are standard columns and values are character
vectors of synonyms
