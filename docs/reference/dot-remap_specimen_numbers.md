# Renumber the specimens of an OpenForis export from a correspondence table

A field team numbers its vouchers from 1 and only later learns which
block of the herbarium series it was allotted, so a whole mission is
renumbered in one go from a two-column table. Both the specimen number
and the herbarium code are substituted, and the values as recorded are
kept in `specimen_number_original` and `herbarium_nbe_char_original`.

## Usage

``` r
.remap_specimen_numbers(trees, remap_file, data_dir = NULL)
```

## Arguments

- trees:

  Normalised tree data frame.

- remap_file:

  Path to the xlsx, or a bare filename to look up in `data_dir`. The
  first column holds the number as recorded, the second its replacement;
  any further columns are ignored.

- data_dir:

  Directory a bare filename is resolved against. NULL to require a full
  path.

## Value

`trees` with the substitutions applied.
