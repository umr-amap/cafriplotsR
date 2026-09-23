# Entry point executed inside the worker process

Reads one input file, runs the pipeline, writes one result file.
Exported only so a fresh R session can reach it as
\`CafriplotsR::.matching_worker()\`; it is not meant to be called by
hand.

## Usage

``` r
.matching_worker(input_file, result_file)
```

## Arguments

- input_file:

  Character path to the RDS written by \`.start_matching_job()\`.

- result_file:

  Character path the result RDS is written to.

## Value

Invisibly NULL. The result travels through \`result_file\`.
