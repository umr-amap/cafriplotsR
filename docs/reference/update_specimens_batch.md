# Update specimens data data

\`r lifecycle::badge("deprecated")\`

Superseded by
[`update_records`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md)
with `table_type = "specimens"` and `method = "batch"`. Rename the
columns of `new_data` to their database names beforehand (the
`col_names_select` / `col_names_corresp` pair has no equivalent), then:

    update_records(data = new_data, table_type = "specimens",
                   method = "batch", execute = TRUE)

Update specimens data plot \_ at a time

## Usage

``` r
update_specimens_batch(
  new_data,
  col_names_select = NULL,
  col_names_corresp = NULL,
  id_col = 1,
  launch_update = FALSE,
  add_backup = TRUE
)
```

## Arguments

- new_data:

  data frame data containing id and new values

- col_names_select:

  string plot name of the selected plots

- col_names_corresp:

  string of the selected plots

- id_col:

  integer indicate which name of col_names_select is the id for matching
  data

- launch_update:

  logical if TRUE updates are performed

- add_backup:

  logical whether backup of modified data should be recorded

## Value

No return value individuals updated

## See also

[`update_records`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md)

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
