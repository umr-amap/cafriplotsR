# Config for \`detect_direct_changes()\` / \`execute_direct_updates()\`

\`get_column_routing()\` is the single source for table, id column and
backup table. Its \`direct_columns\` are tuned for the import wizard
(friendly names such as \`method\`, and \`plot_name\` on individuals
where no such column exists), so the app supplies its own list, derived
from the live schema.

## Usage

``` r
.upd_routing(table_type, columns, con)
```
