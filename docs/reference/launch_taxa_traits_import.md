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

The wizard consists of 6 steps:

1.  Upload data (xlsx or csv with idtax column; choose the sheet of a
    multi-sheet workbook)

2.  Map trait columns. Wide format: one column per trait, each mapped to
    a trait. Long format: one row per measurement, with a trait-name
    column and a numeric and/or character value column; each trait name
    is mapped to a trait

3.  Map metadata columns (taxon ID, flat metadata, and trait features)

4.  Citation: link the import to an existing entry of `table_citations`,
    or create one (written to the database there and then, before the
    import). Optional

5.  Validate (check types, ranges, NAs, duplicates; auto-fix type
    mismatches)

6.  Preview & import (dry run or live)

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
