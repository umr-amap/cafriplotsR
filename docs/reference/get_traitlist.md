# Return the \`traitlist\` lookup table, cached per session

\`traitlist\` is a small lookup table (~50 rows) that the package
re-fetches from the database in many places to find the \`valuetype\` of
a trait or to join trait definitions to measurements. Re-fetching ~50
rows on every call adds up to thousands of round trips during a typical
workflow, even though the table changes very rarely (only when
\[add_trait_taxa()\] inserts a new row).

## Usage

``` r
get_traitlist(con, refresh = FALSE)
```

## Arguments

- con:

  A database connection from \[call.mydb()\] (or a \[pool::pool()\]).

- refresh:

  Logical. If \`TRUE\`, drop the cache and re-fetch from the database.
  Default \`FALSE\`.

## Value

A \`data.frame\` with the full contents of \`traitlist\`.

## Details

\`get_traitlist()\` fetches the table once into an internal cache
(\`.db_env\$traitlist_cache\`) and returns the cached \`data.frame\` on
subsequent calls. The cache is invalidated automatically when a new
trait is added via \[add_trait_taxa()\] and when
\[cleanup_connections()\] is called. Use \`refresh = TRUE\` to force a
re-fetch (e.g. after an external update).
