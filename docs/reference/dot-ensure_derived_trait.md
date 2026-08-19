# Look up (or create) the derived \`traitlist\` row for an aggregation rule.

For a \`(source_trait_id, method, method_param)\` tuple, returns the
\`id_trait\` of a \`traitlist\` row whose name is
\`\<source_trait\>\_\<suffix\>\` (see \[\`.derived_trait_suffix()\`\]).
Creates the row on the fly if it does not exist, copying \`valuetype\`,
\`expectedunit\`, \`minallowedvalue\` and \`maxallowedvalue\` from the
source trait.

## Usage

``` r
.ensure_derived_trait(source_trait_id, method, method_param, con)
```

## Details

Note: the lookup is by name only, so if a \`traitlist\` row with the
same computed name already exists for a different reason it will be
reused.
