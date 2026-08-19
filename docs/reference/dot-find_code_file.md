# Find a code-list CSV, trying several glob patterns in turn

Unlike
[`.find_file_in_dir()`](https://umr-amap.github.io/cafriplotsR/reference/dot-find_file_in_dir.md)
this only reports once, after every pattern has failed, so trying
alternative names stays quiet.

## Usage

``` r
.find_code_file(dir, patterns, label)
```

## Arguments

- dir:

  Directory to search.

- patterns:

  Character vector of glob patterns, tried in order.

- label:

  Human-readable label used in messages.

## Value

File path, or NULL when none matched.
