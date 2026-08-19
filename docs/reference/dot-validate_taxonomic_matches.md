# Validate Taxonomic Matches (Internal Helper)

Compares individual and specimen taxonomy and assigns validation status.
Creates detailed difference indicators with priority on family-level
differences.

## Usage

``` r
.validate_taxonomic_matches(links_with_taxonomy)
```

## Arguments

- links_with_taxonomy:

  Data frame with taxonomic information

## Value

Data frame with taxonomic_match, validation_status, and
difference_indicator columns
