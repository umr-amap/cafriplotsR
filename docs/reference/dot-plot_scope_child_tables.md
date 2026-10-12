# The six child tables step 5 puts under row-level security

\`data_liste_plots\` is deliberately absent. It already has row-level
security and ~117 per-account policies that \`plot_access_seed.R\`
proved agree with \`plot_access\` plot-for-plot, plus \`insert_own\` and
the three \`creator_access\_\*\` policies the import wizard depends on.
Swapping it over to \`plot_access\` is a separate migration with a
different risk profile, and leaving it alone here means step 5 cannot
change how a plot is created.

## Usage

``` r
.plot_scope_child_tables()
```

## Value

Character vector of table names.

## See also

\[.plot_scope_tables()\], which is all seven.
