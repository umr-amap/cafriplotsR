# Turn tracked field changes into update_dico_name() arguments

Both the simple and the cascade update path need the same translation
from the module's \`modified_fields\` list to named
\`update_dico_name()\` arguments, so it lives here rather than twice
inline. An emptied field becomes \`NA\`, which is how it reaches the
database as NULL; fields update_dico_name() cannot write are dropped and
handled by the caller.

## Usage

``` r
.taxa_update_dico_params(modified_fields)
```

## Arguments

- modified_fields:

  Named list of \`list(old =, new =)\` entries, as accumulated by the
  module.

## Value

Named list of arguments, empty if nothing update_dico_name() handles was
modified.
