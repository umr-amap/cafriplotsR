# Validate Tag Values (Internal)

Tags must be numeric and valid (not 0, not NA).

## Usage

``` r
.validate_tag_values(data, max_exact = 2^53)
```

## Arguments

- data:

  Data frame with tag column

- max_exact:

  Largest integer the tag column can hold exactly. Defaults to 2^53, the
  limit R itself imposes on a double.

## Value

List with errors and warnings

## Details

A tag too large for the column is stored rounded, with nothing
downstream to notice, so the ceiling is checked here rather than left to
the insert. Where that ceiling sits depends on the column type — see
\[.tag_precision_limit()\] — which is why it arrives as an argument
instead of being hard-coded.

Every check runs on the parsed values, not on `is.numeric(data$tag)`: a
tag column read from a spreadsheet as text used to skip the zero and
negative checks entirely.
