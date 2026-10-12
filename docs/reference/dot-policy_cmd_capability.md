# What capability does a policy command confer?

Maps the \`cmd\` column of \`pg_policies\` onto the capability flags of
\`plot_access\`.

DELETE is kept separate from UPDATE rather than folded into one
\`can_write\` flag. It is the destructive one, it cascades through six
child tables, and \`safe_delete_plot()\` is not atomic, so it is the one
capability that should be handed out deliberately rather than inherited.

Every command implies read, because a policy's USING expression is
evaluated against rows the account must already be able to see - which
is why \`plot_access\` has no \`can_read\` column: a row in it \*is\*
read access, and \`can_write\` and \`can_delete\` are the escalations.

INSERT returns \`NA\`: insertion on \`data_liste_plots\` is governed by
a single global policy with no plot list, so it contributes nothing to a
per-plot grant and the caller skips it.

## Usage

``` r
.policy_cmd_capability(cmd)
```

## Arguments

- cmd:

  Character of length 1. One of \`"SELECT"\`, \`"INSERT"\`,
  \`"UPDATE"\`, \`"DELETE"\`, \`"ALL"\`.

## Value

\`"read"\`, \`"write"\`, \`"delete"\`, \`"all"\`, or \`NA_character\_\`
for INSERT. Errors on anything else.

## Examples

``` r
.policy_cmd_capability("SELECT")
#> [1] "read"
.policy_cmd_capability("DELETE")
#> [1] "delete"
.policy_cmd_capability("ALL")
#> [1] "all"
```
