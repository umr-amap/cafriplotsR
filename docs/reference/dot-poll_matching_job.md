# Check on a running matching job

Check on a running matching job

## Usage

``` r
.poll_matching_job(job)
```

## Arguments

- job:

  A job list from \`.start_matching_job()\`.

## Value

A list with \`state\`, one of: \* \`"running"\` — plus \`progress\`,
whatever the worker last published \* \`"done"\` — plus \`result\`, the
pipeline's return value \* \`"failed"\` — plus \`message\`, including
the worker's stderr if any
