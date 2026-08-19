# Link each secondary stem to the tag of its main stem

`data_individuals.multi_tiges_id` holds the tag of the individual a stem
belongs to, so the leader of every group stays NA and only the secondary
stems carry a value.

## Usage

``` r
.build_multi_tiges_id(individuals, multi_stems)
```

## Arguments

- individuals:

  Individuals table (plot_name, tag).

- multi_stems:

  Groupings from
  [`.build_openforis_multi_stems()`](https://umr-amap.github.io/cafriplotsR/reference/dot-build_openforis_multi_stems.md).

## Value

Vector of parent tags, aligned with the rows of `individuals`.
