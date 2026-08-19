# Add an aggregation rule

Add an aggregation rule

## Usage

``` r
add_trait_aggregation(
  source_trait_id,
  method,
  target_trait_id = NULL,
  method_param = NULL,
  min_n = 1L,
  notes = NULL,
  con = NULL
)
```

## Arguments

- source_trait_id:

  Integer. Trait id whose individual-level rows (in
  \`data_traits_measures.traitid\`) feed the aggregate.

- method:

  Character. One of the supported methods (see
  \`.compute_aggregate()\`).

- target_trait_id:

  Integer or NULL. Trait id stamped on the aggregated output rows in
  \`taxa_traits_measures.fk_id_trait\`. Three usage modes:

  \* \*\*\`NULL\` (default) — auto-derive.\*\* Creates (or reuses) a
  derived \`traitlist\` entry named \`\<source_trait\>\_\<suffix\>\`
  (e.g. \`stem_diameter_p95\`, \`stem_diameter_max\`) with the source's
  \`valuetype\`, \`expectedunit\`, and value bounds copied over. The
  derived trait's id is stored as \`target_trait_id\`. The method is
  preserved in the trait name and multiple aggregations of the same
  source coexist as distinct columns in \`query_taxa_traits()\`. \*
  \*\*\`source_trait_id\` — keep the original trait name.\*\* The
  aggregated row is written under the source trait id, so wide pivots
  show it as \`taxa\_\<source_trait\>\` and there is no name suffix.
  Trade-offs: (a) the aggregation method is no longer visible at the
  trait-id level — it survives only in \`measurementremarks\` (e.g.
  \`"percentile(n=12) param=95"\`) and \`basisofrecord =
  "AggregatedFromIndividual"\`; (b) you can only have one such rule per
  source trait — two rules pointing to the same id collide in \`(idtax,
  fk_id_trait)\` and the wide pivot averages them; (c) any
  directly-measured taxa-level rows for that trait are mixed in. \*
  \*\*An explicit integer\*\* — use that trait id (e.g. a pre-existing
  custom entry in \`traitlist\`) without auto-creation.

- method_param:

  Numeric or NULL. Required for \`"percentile"\`.

- min_n:

  Integer. Minimum non-NA values per taxon to emit a row (default 1).

- notes:

  Character. Free-text description.

- con:

  Connection (admin/write privileges).

## Value

Invisible integer: the new \`id_aggregation\`.
