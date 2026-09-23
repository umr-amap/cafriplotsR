# Index of the next name still waiting for review

Looks forward from \`from\`, then wraps round to the start. \`from\`
itself is never returned: skipping a name must move away from it.

## Usage

``` r
.next_pending_review_index(pending, from)
```

## Arguments

- pending:

  Logical vector, \`TRUE\` for names with no decision yet.

- from:

  Integer, the current position.

## Value

Integer index, or \`NA_integer\_\` when no other name is waiting.
