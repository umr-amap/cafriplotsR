# Add new entry to taxonomic table

Add new entry to taxonomic table

## Usage

``` r
add_entry_taxa(
  search_name_tps = NULL,
  tax_gen = NULL,
  tax_esp = NULL,
  tax_fam = NULL,
  tax_order = NULL,
  tax_famclass = NULL,
  tax_rank1 = NULL,
  tax_name1 = NULL,
  tax_rank2 = NULL,
  tax_name2 = NULL,
  author1 = NULL,
  author2 = NULL,
  author3 = NULL,
  year_description = NULL,
  synonym_of = NULL,
  morpho_species = FALSE,
  TPS_KEY = "15ad0b4c-f0d3-46ab-b649-178f2c75724f",
  tax_tax = NULL
)
```

## Arguments

- tax_gen:

  string genus name

- tax_esp:

  string species name

- tax_fam:

  string family name

- tax_rank1:

  string tax_rank1 name

- tax_name1:

  string tax_name1 name

- synonym_of:

  list if the new entry should be put in synonymy with an existing taxa,
  add in a list at least one values to identify to which taxa it will be
  put in synonymy: genus, species or id

- detvalue:

  integer detvalue code

- morphocat:

  integer morphocat code

- full_name:

  string full name : genus + species + authors

## Value

A tibble

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
