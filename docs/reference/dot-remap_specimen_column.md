# Substitute specimen numbers from a remap table

Matches on the number as written first. Anything left over is matched on
its digits alone, keeping whatever preceded them — so a table keyed on
bare numbers (`1 -> 4530`) reaches `"Pird 1"` as well as `1`, and turns
it into `"Pird 4530"`.

## Usage

``` r
.remap_specimen_column(x, old, new)
```

## Arguments

- x:

  Vector of specimen numbers to remap.

- old, new:

  The two columns of the remap table.

## Value

List with `value` (the remapped vector, character) and `matched` /
`by_digits` (logical, which rows were substituted and which of them
needed the second pass).

## Details

That second pass is what makes a remap safe to apply to both the
specimen number and the herbarium code. The two columns of an OpenForis
export do not agree on their form — `specimen_nbr` holds `107` while the
calculated `specimen_name` holds `"Pird 107"` — so a literal-only match
renumbers one and not the other, and specimens are deduplicated on the
herbarium code while their collection number is parsed out of the
specimen number. The pair would disagree about which collection it is.
