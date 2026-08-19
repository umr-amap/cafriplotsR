# Clean and normalize taxonomic name

Clean taxonomic names by removing common botanical annotation patterns
that interfere with matching: - "sp.", "sp", "spp.", "spp" after genus
(e.g., "Garcinia sp." → "Garcinia") - "cf.", "cf", "aff.", "?" between
genus and species (e.g., "Garcinia cf. kola" → "Garcinia kola") - Extra
whitespace and punctuation

## Usage

``` r
clean_taxonomic_name(name)
```

## Arguments

- name:

  Character string of taxonomic name

## Value

Cleaned taxonomic name (character string)

## Author

Claude Code Assistant

## Examples

``` r
clean_taxonomic_name("Fabaceae sp.")        # → "Fabaceae"
#> [1] "Fabaceae"
clean_taxonomic_name("Garcinia cf. kola")   # → "Garcinia kola"
#> [1] "Garcinia kola"
clean_taxonomic_name("Brachystegia spp")    # → "Brachystegia"
#> [1] "Brachystegia"
clean_taxonomic_name("Gilbertiodendron  ?  dewevrei")  # → "Gilbertiodendron dewevrei"
#> [1] "Gilbertiodendron dewevrei"
```
