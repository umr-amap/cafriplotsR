# Load backbone from cache with validation

Loads the taxonomic backbone from cache and validates its structure.
Returns NULL if cache is corrupted or missing required columns.

## Usage

``` r
load_backbone_cache()
```

## Value

Tibble with backbone data, or NULL if invalid/unavailable
