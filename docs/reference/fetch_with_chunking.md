# Generic chunking function for large queries

Generic chunking function for large queries

## Usage

``` r
fetch_with_chunking(
  ids,
  query_fun,
  chunk_size,
  con,
  desc = "data",
  trait_ids = NULL
)
```
