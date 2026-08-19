# Create a Hierarchy Entry for Parent (internal helper)

Creates a new entry at the specified taxonomic level with proper
id_parent. Uses tax_level column to indicate the taxonomic rank.

## Usage

``` r
.create_hierarchy_entry_for_parent(
  con,
  tax_gen = NA,
  tax_esp = NA,
  tax_fam = NA,
  tax_order = NA,
  tax_famclass = NA,
  tax_level = NA,
  id_parent = NA
)
```

## Arguments

- con:

  Database connection

- tax_gen:

  Genus name

- tax_esp:

  Species epithet

- tax_fam:

  Family name

- tax_order:

  Order name

- tax_famclass:

  Class name

- tax_level:

  Taxonomic level: "class", "order", "family", "genus", "species"

- id_parent:

  Parent taxon ID
