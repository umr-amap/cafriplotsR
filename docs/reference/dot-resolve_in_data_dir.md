# Resolve a relative path against the export directory

The review files of a mission belong next to its export, not wherever R
happens to be running, so a relative path is taken inside `data_dir`. An
absolute path, or any path when `data_dir` is NULL, is returned as
given.

## Usage

``` r
.resolve_in_data_dir(path, data_dir = NULL)
```

## Arguments

- path:

  Character path.

- data_dir:

  Export directory, or NULL.

## Value

Character path.
