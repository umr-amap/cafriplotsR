# Build a debug header string with package/R version and timestamp

Used internally to prefix error and diagnostic messages with
reproducible version context, making it easier to correlate
user-reported errors to specific package releases.

## Usage

``` r
.get_debug_header()
```

## Value

A character string of the form \`\[CafriplotsR vX.Y.Z \| R M.m \|
YYYY-MM-DD HH:MM:SS\]\`
