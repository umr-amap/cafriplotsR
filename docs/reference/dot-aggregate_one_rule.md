# Aggregate a single rule, return rows ready to insert into taxa_traits_measures

Aggregate a single rule, return rows ready to insert into
taxa_traits_measures

## Usage

``` r
.aggregate_one_rule(
  rule,
  citation_id,
  con,
  con_taxa,
  allowed_tax_levels = c("species", "infraspecific")
)
```
