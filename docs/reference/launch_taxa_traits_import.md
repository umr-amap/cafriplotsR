# Launch Taxa Traits Import App

Opens an interactive Shiny app that guides users through importing
taxa-level trait measurements: upload data, map columns to traits from
the trait list, preview, and execute the import.

## Usage

``` r
launch_taxa_traits_import(launch_browser = TRUE, language = "fr")
```

## Arguments

- launch_browser:

  Logical: Open in external browser? (default TRUE)

- language:

  Character, initial language ("en" or "fr"), default: "fr"

## Value

Invisibly returns the Shiny app object

## Details

The wizard consists of 5 steps:

1.  Upload data (xlsx or csv with idtax column)

2.  Map trait columns (select which columns contain trait observations)

3.  Map metadata columns (taxon ID, flat metadata, and trait features)

4.  Validate (check types, ranges, NAs, duplicates; auto-fix type
    mismatches)

5.  Preview & import (dry run or live)

Prerequisites:

- Data must contain an `idtax` column with valid taxon IDs

- Use the taxonomic matching app first to standardize names

- Trait columns should match existing traits in `traitlist`

## Examples

``` r
if (FALSE) { # \dontrun{
launch_taxa_traits_import()
} # }
```
