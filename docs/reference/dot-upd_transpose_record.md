# One row per column of a record, values rendered as text

A record read across is unreadable once it has a hundred columns, so it
is turned on its side: one row per column, one value column per record
row (an individual can come back as several rows, one per stem or
census).

## Usage

``` r
.upd_transpose_record(tbl)
```
