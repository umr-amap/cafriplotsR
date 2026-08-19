# Prepare Growth Form Data for Database Insert

Converts hierarchical growth form selections into a data frame suitable
for add_sp_traits_measures()

## Usage

``` r
.prepare_growth_form_data(growth_form_selections, idtax)
```

## Arguments

- growth_form_selections:

  List of growth form paths

- idtax:

  Taxon ID

## Value

Data frame with one row per path, traits as columns
