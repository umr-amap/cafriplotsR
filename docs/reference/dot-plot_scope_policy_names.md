# Policy names step 5 creates on a child table

One per command. Named so they are greppable and so nothing collides
with the \`policy\_\<account\>\_\<command\>\` names
\`define_user_policy()\` writes or the \`creator_access\_\*\` names from
\`inst/migrations/add_created_by.R\`.

## Usage

``` r
.plot_scope_policy_names(table_name)
```

## Arguments

- table_name:

  Character of length 1.

## Value

Named character vector, one entry per SQL command.
