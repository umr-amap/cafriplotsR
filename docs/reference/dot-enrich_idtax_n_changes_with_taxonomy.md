# Enrich idtax_n changes with taxonomic names

Adds genus, species, and family columns for both old and new idtax_n
values Uses the existing add_taxa_table_taxa() function to fetch
taxonomy

## Usage

``` r
.enrich_idtax_n_changes_with_taxonomy(changes_df)
```

## Arguments

- changes_df:

  Dataframe with columns: column, old_value, new_value where column may
  contain "idtax_n" and values are taxon IDs

## Value

changes_df with added columns: old_genus, old_species, old_family,
new_genus, new_species, new_family (for idtax_n rows only)
