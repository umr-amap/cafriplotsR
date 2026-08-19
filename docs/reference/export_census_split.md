# Write a census split out for the import wizards

Produces the files the existing wizards already accept, so the split can
be used today without waiting on a dedicated import mode: the recruit
file goes to \`launch_import_wizard()\`, the measurement file to
\`launch_feature_wizard()\`'s \*Add Individual Measurements\* mode.

## Usage

``` r
export_census_split(x, dir = ".", prefix = NULL, overwrite = FALSE)
```

## Arguments

- x:

  A \`census_split\` object from \[split_census_table()\].

- dir:

  Directory to write into. Created if absent.

- prefix:

  Optional file name prefix.

- overwrite:

  Overwrite existing files?

## Value

Invisibly, the character vector of paths written.

## Details

Review rows are written to their own file and are deliberately absent
from the recruit file — importing them unchecked is what the split
exists to prevent.

## Examples

``` r
if (FALSE) { # \dontrun{
split <- split_census_table(census, plot_names = "P1", con = con)
export_census_split(split, dir = "census_2026")
} # }
```
