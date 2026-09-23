# CPU quota enforced on this process, in whole CPUs

Reads the cgroup CPU bandwidth limit: \`cpu.max\` under cgroup v2, or
\`cpu.cfs_quota_us\` / \`cpu.cfs_period_us\` under cgroup v1. A
fractional quota is rounded up, so a 1.5-CPU limit yields 2 threads
rather than 1.

## Usage

``` r
.container_cpu_limit(root = "/sys/fs/cgroup")
```

## Arguments

- root:

  Character. The cgroup mount point; a parameter only so tests can point
  it at a fixture directory.

## Value

Integer number of CPUs, or \`NA_integer\_\` when no limit is set or the
cgroup files are absent (Windows, macOS, an unconstrained Linux host).
