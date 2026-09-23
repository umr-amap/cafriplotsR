# What is wrong with a proposed parent link

Returns problem codes rather than sentences, so the app can say them in
the user's language and the console callers can say them in English.

## Usage

``` r
.upd_validate_plot_link(id, parent_id, relation, con = NULL)
```

## Arguments

- id:

  Integer plot id being edited.

- parent_id:

  Proposed parent id, \`NA\` for none.

- relation:

  Proposed relation, \`NA\` for none.

- con:

  Optional DBI connection.

## Value

Character vector of codes, empty when the link is sound:
\`"relation_missing"\`, \`"parent_missing"\`, \`"unknown_relation"\`,
\`"self_parent"\`, \`"parent_not_found"\`, \`"cycle"\`.

## Details

Without \`con\` only the checks that need no database run - the pairing
rule and the vocabulary - which is what the form uses for live feedback
as the user types. With \`con\` the link is also checked against the
stored hierarchy, which is the gate \[.upd_apply_plot_link()\] will not
write past.
