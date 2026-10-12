# Append rows to a table without using COPY

Drop-in replacement for \`DBI::dbWriteTable(..., append = TRUE)\` and
\`DBI::dbAppendTable()\` that issues parametrised multi-row \`INSERT\`
statements instead.

## Usage

``` r
.db_append_table(con, table, data, chunk_size = NULL)
```

## Arguments

- con:

  A DBI connection or a \`pool\` object.

- table:

  Character. Name of the target table.

- data:

  A data frame or tibble. Zero rows is a no-op.

- chunk_size:

  Integer or NULL. Rows per \`INSERT\`. When NULL (the default) it is
  derived from the column count so a chunk stays under PostgreSQL's
  limit of 65535 bound parameters per statement.

## Value

Invisibly, the number of rows inserted.

## Details

RPostgres implements \`dbWriteTable()\` and \`dbAppendTable()\` with
\`COPY ... FROM STDIN\`, and PostgreSQL refuses \`COPY FROM\` on any
table where row-level security applies to the current user:

      ERROR:  COPY FROM not supported with row level security
      HINT:   Use INSERT statements instead.

The table owner is exempt, which is why this has not bitten yet — almost
every write to this database is made by \`dauby\`, who owns all 38
tables. It would bite the moment RLS reached the child tables: every
account in \`plots_transects-rw\` would stop being able to import, with
that error. This function is the "use INSERT statements instead".

Rows are sent in chunks of one statement each rather than one statement
per row, because one round trip per row against a remote managed
instance turns a ten-thousand-row import into minutes of latency. Within
a chunk the placeholders are laid out column-major, so the parameter
list can be built with one \`as.list()\` per column instead of a
per-cell loop.

Columns are matched to the table by name, as \`dbWriteTable()\` does, so
the order of columns in \`data\` does not matter. Nothing is quoted by
hand: values travel as bound parameters, which also removes the
injection surface that string-built inserts carry.

## Transactions

A caller that needs all the rows to arrive or none should open its own
transaction, as \`mod_feat_step6_import.R\` and
\`import_individual_data()\` already do. Within one chunk the \`INSERT\`
is atomic by itself; across chunks it is not, which is the one way this
differs from a single \`COPY\`. Passing a pool is safe — one connection
is held for the whole call — but a pool cannot join a transaction the
caller opened on a different connection.

## See also

\[DBI::dbAppendTable()\], which this replaces at the call sites that
write to row-level-security tables.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
.db_append_table(con, "data_ind_measures_feat", feat_records)
} # }
```
