# The grant list a policy compares against

\`read\` is every row in \`plot_access\` for the calling account;
\`write\` is the subset carrying \`can_write\`. There is no \`delete\`
mode: see decision 2 in the header of this file.

Kept as a bare subquery rather than a call to \`accessible_plots()\` on
purpose. That function carries a \`SET search_path\`, which stops the
planner inlining it, and an opaque call is exactly what the measured
shape avoids.

## Usage

``` r
.plot_grant_list_sql(mode = c("read", "write"))
```

## Arguments

- mode:

  \`"read"\` or \`"write"\`.

## Value

Character of length 1: an SQL \`ARRAY(SELECT ...)\` expression.
