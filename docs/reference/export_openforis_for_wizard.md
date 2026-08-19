# Write the processed tables as Import Wizard uploads

Each wizard step reads the first sheet of a single file, so every table
goes to its own xlsx. Upload them in the order they are numbered: plots
first (individuals are linked to plots by `plot_name`), then the flat
individuals table.

## Usage

``` r
export_openforis_for_wizard(x, dir = ".", prefix = NULL, overwrite = FALSE)
```

## Arguments

- x:

  List returned by
  [`process_openforis_new_plot()`](https://umr-amap.github.io/cafriplotsR/reference/process_openforis_new_plot.md).

- dir:

  Directory to write into. Created when missing.

- prefix:

  Optional file-name prefix, e.g. a mission name.

- overwrite:

  Logical. Replace existing files? Default FALSE.

## Value

Character vector of the files written, invisibly.
