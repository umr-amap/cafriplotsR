# Size OpenMP thread pools to the container's CPU quota

Caps stringdist and data.table at the number of CPUs the container may
use, and exports \`OMP_THREAD_LIMIT\` so child processes inherit the
same cap: a \`callr\` worker is a fresh R session, and stringdist reads
that variable when it loads.

## Usage

``` r
.apply_container_thread_limits(n = .container_cpu_limit())
```

## Arguments

- n:

  Integer thread cap. Defaults to the detected container quota.

## Value

Invisibly, the cap applied, or \`NA_integer\_\` if none was.

## Details

A no-op when no CPU limit is detected, so it is safe to call anywhere;
it is called from the served app entry points and from
\`.matching_worker()\`.
