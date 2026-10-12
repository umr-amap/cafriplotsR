# The statements that undo step 5 for one child table

\`DISABLE ROW LEVEL SECURITY\` is immediate and total, so the first
statement alone restores the previous behaviour; the drops are there so
a re-run of the migration starts clean.

## Usage

``` r
.plot_scope_rollback_statements(table_name)
```

## Arguments

- table_name:

  Character of length 1.

## Value

Character vector of SQL statements.
