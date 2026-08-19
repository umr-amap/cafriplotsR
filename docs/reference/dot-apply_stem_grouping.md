# Resolve multi_tiges_id into stem_grouping (Internal)

`multi_tiges_id` is a staging column holding the **tag** of the main
stem, while `data_individuals.stem_grouping` stores that stem's `id_n`.
Once the batch is inserted every `id_n` is known, so the tags are
resolved here — against this batch first, then against individuals
already in the plot — and written in a single UPDATE inside the same
transaction.

## Usage

``` r
.apply_stem_grouping(
  individuals_data,
  individuals_id_data,
  con,
  progress = TRUE
)
```

## Arguments

- individuals_data:

  Data frame carrying plot_name and multi_tiges_id, in the order it was
  inserted.

- individuals_id_data:

  Data frame returned by the INSERT, with id_individuals, tag and
  plot_name.

- con:

  Database connection (inside the open transaction).

- progress:

  Show progress messages.

## Value

Number of stems whose stem_grouping was set.

## Details

Rows whose parent cannot be resolved are left ungrouped and reported;
[`validate_individual_data`](https://umr-amap.github.io/cafriplotsR/reference/validate_individual_data.md)
flags them as errors beforehand.
