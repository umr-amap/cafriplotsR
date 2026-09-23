# Can matching run in a background process here?

Two things have to hold. \`callr\` must be installed, and CafriplotsR
must be \*installed\* rather than merely loaded from source: the worker
is a fresh R session that resolves \`CafriplotsR::.matching_worker\`,
which a \`devtools::load_all()\` source tree cannot satisfy. The
presence of \`Meta/package.rds\` is what separates the two.

## Usage

``` r
.async_matching_available()
```

## Value

Logical scalar.

## Details

When this returns FALSE the caller runs the pipeline in-process, which
is correct but blocks — fine for a developer at a console, not for a
server.
