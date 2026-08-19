# Fetch the herbarium evidence behind a set of individuals

Individuals link to herbarium material through \`data_link_specimens\`,
with \`linktype\` distinguishing a specimen collected \*\*from this
tree\*\* (\`type_individual\`) from one used as a \*\*comparison\*\*
when determining it (\`referenced_individual\`). The two carry very
different weight against a proposed revision, so both are counted.

## Usage

``` r
.fetch_specimen_evidence(id_n, con)
```

## Arguments

- id_n:

  Integer vector of individual ids.

- con:

  Connection or pool for the main database.

## Value

Data frame with \`id_n\`, \`n_voucher\`, \`n_reference\`. Empty on
failure — evidence is context, never a reason to block the step.
