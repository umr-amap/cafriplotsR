# Assert that \`trait_aggregation_config\` exists with the expected schema.

Throws a clear error pointing at \`migrate_aggregated_traits_all()\` if
the table is missing or has been re-created with a wrong shape (e.g. by
an accidental \`dbWriteTable(append = TRUE)\` against a dropped table).

## Usage

``` r
.assert_aggregation_config_schema(con)
```
