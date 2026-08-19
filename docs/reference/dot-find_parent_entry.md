# Find Parent Entry for a Given Taxonomic Level

Finds the parent entry for a taxon at a specific level. Used when
inserting new taxa to find or create the appropriate parent.

## Usage

``` r
.find_parent_entry(
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

  The level of the taxon being inserted ("species", "genus", "family",
  "order")

## Value

Parent taxon ID (idtax_n) or NULL if not found

## Details

Uses the tax_level column to identify parent entries.
