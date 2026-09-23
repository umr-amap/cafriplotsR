# Repair the unambiguous plot hierarchy issues

Each repair here has exactly one sensible outcome, which is why it is
allowed to run unattended:

## Usage

``` r
.fix_plot_hierarchy_issues(con, issues, force = FALSE)
```

## Arguments

- con:

  Raw database connection (not a pool)

- issues:

  Named list of data frames, fixable types only

- force:

  Logical, skip the confirmation prompt

## Value

Named list of row counts, or NULL if the user declined

## Details

\- a self-parent and a parent that does not exist are both links that
say nothing, so the link is cleared (both columns, to keep the
pairing); - a relation with no parent describes a relationship that is
not there, so the relation is cleared.

Everything else - a parent with no relation, an unknown relation, a
cycle - needs someone who knows the plots.
