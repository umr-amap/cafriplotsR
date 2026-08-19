# Rebuild aggregated taxa-level trait rows from individual measurements

Reads the active rules in \`trait_aggregation_config\`, computes
per-taxon aggregates from individual-level measurements, and writes them
back to \`taxa_traits_measures\` tagged with the supplied (or default)
citation key.

## Usage

``` r
rebuild_aggregated_taxa_traits(
  con = NULL,
  con_taxa = NULL,
  rule_ids = NULL,
  citation_key = "CafriplotsR_aggregated",
  allowed_tax_levels = c("species", "infraspecific"),
  execute = FALSE
)
```

## Arguments

- con:

  Connection to \`plots_transects\` (write privileges).

- con_taxa:

  Connection to \`rainbio\` (for synonym resolution).

- rule_ids:

  Optional integer vector. Restrict the rebuild to these
  \`id_aggregation\` rows; otherwise all \`is_active\` rules are
  processed.

- citation_key:

  Stable key under which aggregated rows are tagged. Created on first
  use with \`is_public = FALSE\`.

- allowed_tax_levels:

  Character vector of \`tax_level\` values (\`"species"\`,
  \`"infraspecific"\`, \`"genus"\`, \`"family"\`, \`"order"\`,
  \`"class"\`, \`"higher"\`) for which the source individual
  identification is allowed to feed the aggregate. Defaults to
  \`c("species", "infraspecific")\` so that, for example, a measurement
  on a "Combretum sp." stem does not contribute to a species-level
  aggregate.

- execute:

  If FALSE (default), a DRY RUN: rows are computed and shown but no
  DELETE/INSERT runs. Set to TRUE to apply.

## Value

Invisible tibble of all rows that were (or would be) inserted.

## Details

Old aggregated rows tied to the same citation \*\*and\*\* to one of the
touched output trait ids are deleted in the same transaction before the
new rows are inserted, so the table always reflects the latest rebuild.
