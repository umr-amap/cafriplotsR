# List active trait aggregation rules

Returns one row per rule with both \`source_trait_id\` /
\`source_trait\` and \`target_trait_id\` / \`target_trait\`. When
\`target_trait_id\` is NULL in the config (meaning "same as source"),
the returned \`effective_target_trait_id\` is filled with
\`source_trait_id\` to make downstream filtering trivial.

## Usage

``` r
list_trait_aggregations(con = NULL, include_inactive = FALSE)
```

## Arguments

- con:

  Connection to \`plots_transects\` (or NULL to use \`call.mydb()\`).

- include_inactive:

  Logical. If TRUE, also return rules with \`is_active = FALSE\`.

## Value

Tibble with one row per rule, joined to trait names.
