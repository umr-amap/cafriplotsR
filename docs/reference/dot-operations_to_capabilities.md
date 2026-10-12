# What capabilities does an \`operations\` argument confer?

Maps the \`operations\` argument of \[define_user_policy()\] onto the
\`can_write\` and \`can_delete\` flags of \`plot_access\`.

\`"ALL"\` means SELECT, INSERT and UPDATE - not DELETE. It used to mean
all four, and since \`define_full_access_policy()\` is the usual way a
colleague gets access, that is how 13,913 of 14,018 plot grants came to
carry the right to delete other people's plots. DELETE is now named
explicitly or handed out per plot with \[grant_delete_right()\].

## Usage

``` r
.operations_to_capabilities(operations)
```

## Arguments

- operations:

  Character vector, as passed to \[define_user_policy()\].

## Value

A list with \`can_write\` and \`can_delete\`, both logical.

## Examples

``` r
.operations_to_capabilities("SELECT")
#> $can_write
#> [1] FALSE
#> 
#> $can_delete
#> [1] FALSE
#> 
.operations_to_capabilities("ALL")
#> $can_write
#> [1] TRUE
#> 
#> $can_delete
#> [1] FALSE
#> 
.operations_to_capabilities(c("SELECT", "DELETE"))
#> $can_write
#> [1] FALSE
#> 
#> $can_delete
#> [1] TRUE
#> 
```
