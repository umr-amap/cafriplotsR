# Normalise a plot name to the spelling the database uses

The raw exports spell one plot several ways — `"Mbalmayo1__"`,
`"Mbalmayo-09"`, `"mbalmayo010_"` — because the field entry is free text
and the tidied column pads its numbers inconsistently. Stripping every
separator and zero-padding the trailing number to a fixed width
collapses them onto the database spelling (`"mbalmayo001"`,
`"mbalmayo009"`, `"mbalmayo010"`).

## Usage

``` r
.normalise_plot_name(x, digits = 3L, map = NULL)
```

## Arguments

- x:

  Character vector of raw plot names.

- digits:

  Width the trailing number is padded to. NULL disables padding.

- map:

  Named character vector of explicit replacements, applied to the raw
  value and winning over the derived one. NULL for none.

## Value

Character vector of normalised names.

## Details

A number already at or above `digits` wide is left alone, so a plot
whose name genuinely carries more digits is not mangled.
