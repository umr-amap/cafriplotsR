# Explain why a worker died without producing a result

The logs first — a crashed R session says why there. An exit status
alone is what a killed process leaves behind, so it is the fallback, not
the headline. Both streams are read: a worker that dies mapping a DLL
writes to stderr, but an allocation failure R catches on the way down
can land on either.

## Usage

``` r
.matching_job_error(job)
```

## Arguments

- job:

  A job list from \`.start_matching_job()\`.

## Value

A character scalar.

## Details

A recognised memory failure is explained rather than quoted, with the
raw tail kept underneath so nothing is lost for whoever is debugging it.
