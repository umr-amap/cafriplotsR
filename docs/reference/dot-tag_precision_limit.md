# Largest tag the database can store exactly (Internal)

\`data_individuals.tag\` was created as a PostgreSQL \`real\` — single
precision, exact for integers only to 2^24. The \`tag_to_numeric\`
migration (\`inst/migrations/\`) widened it, after which any tag R can
hold is safe. Reading the type rather than assuming it means the ceiling
is right both before and after that migration, with no edit needed in
between.

## Usage

``` r
.tag_precision_limit(con = NULL)
```

## Arguments

- con:

  Database connection, or \`NULL\`.

## Value

\`2^24\` while the column is single precision, otherwise \`2^53\` — the
limit R's own doubles impose. Also \`2^53\` when the type cannot be
read, so a permissions problem never rejects a legitimate tag.
