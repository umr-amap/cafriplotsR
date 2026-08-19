# Normalise a tag for matching

Tags travel between Excel (often numeric), the database (often integer
or text) and free typing, so they have to be compared on a common form.
Numeric tags are formatted without scientific notation —
\`as.character()\` would turn a six-digit tag into \`"1e+05"\` and break
every match.

## Usage

``` r
.normalize_tag(x)
```

## Arguments

- x:

  Vector of tags.

## Value

Character vector, blanks returned as \`NA\`.

## Details

A tag column read from a spreadsheet as text carries whatever the typist
wrote — \`"0101"\`, \`"22.10"\`, \`"45.0"\` — while the database returns
the number itself. Purely numeric strings are therefore put through the
same formatting as numeric input, so both sides reach one string.
Anything with a letter or a separator in it is left exactly as written.
