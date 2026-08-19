# Insert aggregated rows into \`taxa_traits_measures\` via plain INSERT.

\`DBI::dbWriteTable()\` falls back to \`COPY\`, which PostgreSQL refuses
on tables protected by row-level security (it raises \*"Use INSERT
statements instead."\*). This helper runs one parametrised INSERT per
row instead.

## Usage

``` r
.insert_aggregated_rows(con, rows)
```
