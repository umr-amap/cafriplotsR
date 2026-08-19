# Parse taxonomic name into components

Parse a taxonomic name string into its components: genus, species
epithet, infraspecific ranks/names, and author names.

## Usage

``` r
parse_taxonomic_name(name)
```

## Arguments

- name:

  Character string of taxonomic name

## Value

A list with components: - rank: Detected rank ("family", "order",
"genus", "species", or "unknown") - genus: Genus name (first word, or NA
if family/order detected) - species: Species epithet (second word if
present) - infraspecific: Full infraspecific part (everything after
species) - full_name_no_auth: Genus + species + infraspecific without
authors - input_name: Original input

## Author

Claude Code Assistant

## Examples

``` r
parse_taxonomic_name("Gilbertiodendron dewevrei")
#> $rank
#> [1] "species"
#> 
#> $genus
#> [1] "Gilbertiodendron"
#> 
#> $species
#> [1] "dewevrei"
#> 
#> $infraspecific
#> [1] NA
#> 
#> $full_name_no_auth
#> [1] "Gilbertiodendron dewevrei"
#> 
#> $input_name
#> [1] "Gilbertiodendron dewevrei"
#> 
parse_taxonomic_name("Anthonotha macrophylla var. oblongifolia")
#> $rank
#> [1] "species"
#> 
#> $genus
#> [1] "Anthonotha"
#> 
#> $species
#> [1] "macrophylla"
#> 
#> $infraspecific
#> [1] "var. oblongifolia"
#> 
#> $full_name_no_auth
#> [1] "Anthonotha macrophylla var. oblongifolia"
#> 
#> $input_name
#> [1] "Anthonotha macrophylla var. oblongifolia"
#> 
parse_taxonomic_name("Brachystegia")
#> $rank
#> [1] "genus"
#> 
#> $genus
#> [1] "Brachystegia"
#> 
#> $species
#> [1] NA
#> 
#> $infraspecific
#> [1] NA
#> 
#> $full_name_no_auth
#> [1] "Brachystegia"
#> 
#> $input_name
#> [1] "Brachystegia"
#> 
```
