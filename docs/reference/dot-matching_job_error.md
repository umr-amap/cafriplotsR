# Explain why a worker died without producing a result

stderr first — a crashed R session says why there. An exit status alone
is what a killed process leaves behind, so it is the fallback, not the
headline.

## Usage

``` r
.matching_job_error(job)
```

## Arguments

- job:

  A job list from \`.start_matching_job()\`.

## Value

A character scalar.
