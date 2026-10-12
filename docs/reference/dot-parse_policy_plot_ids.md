# Parse the plot IDs out of a row-level security policy expression

Reads the \`qual\` (USING expression) of a policy on
\`data_liste_plots\` and returns the plot IDs it grants access to.

\`define_user_policy()\` writes \`USING (id_liste_plots IN (...))\`,
which PostgreSQL stores back in one of three deparsed forms depending on
how many IDs were given. The creator policies from
\`inst/migrations/add_created_by.R\` instead compare \`created_by\` to
\`current_user\` and carry no ID list at all.

Anything else is reported as \`"unparseable"\`. This is deliberate: a
loose parser that scraped digits out of an unexpected expression would
produce a plausible-looking, wrong grant set.

## Usage

``` r
.parse_policy_plot_ids(qual)
```

## Arguments

- qual:

  Character of length 1, or \`NA\`. The \`qual\` column of
  \`pg_policies\`.

## Value

A list with two elements:

- kind:

  One of \`"ids"\` (an explicit plot list), \`"creator"\` (access
  derived from \`created_by\`), \`"none"\` (no USING expression, e.g. an
  INSERT policy), or \`"unparseable"\`.

- ids:

  Integer vector of plot IDs, sorted and unique. Empty unless \`kind\`
  is \`"ids"\`.

## Examples

``` r
.parse_policy_plot_ids("(id_liste_plots = ANY (ARRAY[179, 180]))")
#> $kind
#> [1] "ids"
#> 
#> $ids
#> [1] 179 180
#> 
.parse_policy_plot_ids("(created_by = (CURRENT_USER)::text)")
#> $kind
#> [1] "creator"
#> 
#> $ids
#> integer(0)
#> 
.parse_policy_plot_ids("(ddlat > (0)::double precision)")
#> $kind
#> [1] "unparseable"
#> 
#> $ids
#> integer(0)
#> 
```
