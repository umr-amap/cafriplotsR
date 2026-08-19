# Restrict raw individual values to taxa identified at the requested ranks.

Aggregating to a taxa-level trait only makes sense when the underlying
identification is at species or finer rank — a measurement on an
individual identified to "Combretum sp." should not feed a species-level
aggregate. This filter is applied to the \*source\* idtax (the one
attached to \`data_individuals\`, before synonym resolution), since that
is the rank the field identification was made at.

## Usage

``` r
.filter_to_allowed_tax_levels(raw, con_taxa, allowed_tax_levels)
```

## Arguments

- raw:

  Tibble returned by \[\`.fetch_individual_values()\`\].

- con_taxa:

  Connection to the taxa DB.

- allowed_tax_levels:

  Character vector of accepted \`tax_level\` values.

## Details

Cross-database: \`tax_level\` lives in \`table_taxa\` on the taxa DB, so
this issues one extra SELECT against \`con_taxa\`.
