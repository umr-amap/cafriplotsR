# Find or Create Parent Entry

Finds the parent entry for a taxon, creating it if it doesn't exist.
Recursively ensures the full hierarchy exists.

## Usage

``` r
.find_or_create_parent_entry(
  con,
  tax_gen = NULL,
  tax_fam = NULL,
  tax_order = NULL,
  tax_famclass = NULL,
  tax_esp = NULL,
  level = "species"
)
```

## Arguments

- con:

  Database connection

- tax_gen:

  Genus name

- tax_fam:

  Family name

- tax_order:

  Order name

- tax_famclass:

  Class name

- tax_esp:

  Species epithet (for infraspecific taxa)

- level:

  The level of the taxon being inserted

## Value

Parent taxon ID (idtax_n)
