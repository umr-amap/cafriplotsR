# Get Template Examples (Internal Helper)

Generate example data rows for templates. Real \`method\` / \`country\`
values are used when the database context supplies them.

## Usage

``` r
.get_template_examples(template_type, n_examples = 3, ctx = NULL)
```

## Arguments

- template_type:

  Character: Type of template

- n_examples:

  Integer: Number of examples (1 to 3)

- ctx:

  List from \`.get_template_context()\` (optional)

## Value

Tibble with example rows (a superset of the template columns; the caller
keeps the ones it needs)
