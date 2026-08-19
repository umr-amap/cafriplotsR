# Add taxonomic entry non-interactively (for Shiny apps)

Non-interactive version of add_entry_taxa for use in Shiny applications.
Does not prompt for user input or growth forms.

## Usage

``` r
.add_taxa_noninteractive(
  tax_gen = NULL,
  tax_esp = NULL,
  tax_fam = NULL,
  tax_order = NULL,
  tax_famclass = NULL,
  tax_rank1 = NULL,
  tax_name1 = NULL,
  author1 = NULL,
  author2 = NULL,
  author3 = NULL,
  year_description = NULL,
  morpho_species = FALSE,
  tax_tax = NULL,
  con = NULL
)
```

## Arguments

- tax_gen:

  Character, genus name

- tax_esp:

  Character, species epithet

- tax_fam:

  Character, family name

- tax_order:

  Character, order name

- tax_famclass:

  Character, class name

- tax_rank1:

  Character, infraspecific rank ("var." or "subsp.")

- tax_name1:

  Character, infraspecific name

- author1:

  Character, species author

- author2:

  Character, infraspecific author

- author3:

  Character, additional author

- year_description:

  Numeric, year of description

- morpho_species:

  Logical, whether this is a morphospecies

- tax_tax:

  Character, full taxonomic name with authors

- con:

  Database connection (pool or connection object)

## Value

Integer, the new idtax_n value
