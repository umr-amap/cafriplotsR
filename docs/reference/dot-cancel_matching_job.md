# Ask a running matching job to stop, then clean up after it

Cancellation is cooperative first: the worker notices \`cancel_file\`
between two names and saves its checkpoint, so the run can be resumed
later. \`kill()\` is the backstop for a worker stuck inside a single
long name.

## Usage

``` r
.cancel_matching_job(job, wait = 2)
```

## Arguments

- job:

  A job list from \`.start_matching_job()\`, or NULL.

- wait:

  Numeric seconds to allow for a clean stop before killing.

## Value

Invisibly NULL.
