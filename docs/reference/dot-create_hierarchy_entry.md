# Create a hierarchy entry (internal helper)

Creates a new entry in table_taxa for a given taxonomic level. Uses
tax_level column to indicate the taxonomic rank.

## Usage

``` r
.create_hierarchy_entry(
  con,
  tax_gen = NA,
  tax_fam = NA,
  tax_order = NA,
  tax_famclass = NA,
  tax_level = NA,
  id_tax_famclass = NA
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

- tax_level:

  Taxonomic level: "class", "order", "family", "genus"

- id_tax_famclass:

  (deprecated) ID in table_tax_famclass for backward compatibility

## Details

Note: id_tax_famclass is deprecated in the new hierarchy system. Classes
are now stored directly in table_taxa with id_parent = NULL.
