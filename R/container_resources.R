# Container resource awareness
#
# Inside a container, parallel::detectCores() reports the *node's* cores, not
# the CPU quota the container is allowed to use. On SSP Cloud that is 128
# cores reported against a quota of 2. stringdist sizes its OpenMP pool as
# detectCores() - 1 and data.table as half of it, so every trigram scan and
# every data.table operation launched 127 (or 64) threads onto 2 cores' worth
# of quota. The kernel's CFS scheduler then throttles the whole container for
# most of each period, which can make the "parallel" code slower than running
# single-threaded.
#
# The fix is to size the pools from the quota the kernel actually enforces,
# read from the cgroup filesystem. Outside a container, or on a machine with no
# CPU limit, nothing is found and nothing is changed.

#' CPU quota enforced on this process, in whole CPUs
#'
#' Reads the cgroup CPU bandwidth limit: `cpu.max` under cgroup v2, or
#' `cpu.cfs_quota_us` / `cpu.cfs_period_us` under cgroup v1. A fractional quota
#' is rounded up, so a 1.5-CPU limit yields 2 threads rather than 1.
#'
#' @param root Character. The cgroup mount point; a parameter only so tests can
#'   point it at a fixture directory.
#' @return Integer number of CPUs, or `NA_integer_` when no limit is set or the
#'   cgroup files are absent (Windows, macOS, an unconstrained Linux host).
#' @keywords internal
.container_cpu_limit <- function(root = "/sys/fs/cgroup") {

  as_cpus <- function(quota, period) {
    quota  <- suppressWarnings(as.numeric(quota))
    period <- suppressWarnings(as.numeric(period))
    if (length(quota) != 1 || length(period) != 1 ||
        is.na(quota) || is.na(period) || quota <= 0 || period <= 0) {
      return(NA_integer_)
    }
    max(1L, as.integer(ceiling(quota / period)))
  }

  read_first <- function(path) {
    tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) character(0))
  }

  # cgroup v2: one file, "<quota> <period>", or "max <period>" when unlimited.
  v2 <- file.path(root, "cpu.max")
  if (file.exists(v2)) {
    fields <- strsplit(trimws(read_first(v2)), "\\s+")[[1]]
    if (length(fields) != 2 || identical(fields[1], "max")) return(NA_integer_)
    return(as_cpus(fields[1], fields[2]))
  }

  # cgroup v1: quota and period in separate files; quota -1 means unlimited.
  v1_quota  <- file.path(root, "cpu", "cpu.cfs_quota_us")
  v1_period <- file.path(root, "cpu", "cpu.cfs_period_us")
  if (file.exists(v1_quota) && file.exists(v1_period)) {
    return(as_cpus(read_first(v1_quota), read_first(v1_period)))
  }

  NA_integer_
}

#' Size OpenMP thread pools to the container's CPU quota
#'
#' Caps stringdist and data.table at the number of CPUs the container may use,
#' and exports `OMP_THREAD_LIMIT` so child processes inherit the same cap: a
#' `callr` worker is a fresh R session, and stringdist reads that variable when
#' it loads.
#'
#' A no-op when no CPU limit is detected, so it is safe to call anywhere; it is
#' called from the served app entry points and from `.matching_worker()`.
#'
#' @param n Integer thread cap. Defaults to the detected container quota.
#' @return Invisibly, the cap applied, or `NA_integer_` if none was.
#' @keywords internal
.apply_container_thread_limits <- function(n = .container_cpu_limit()) {
  if (length(n) != 1 || is.na(n) || n < 1) return(invisible(NA_integer_))
  n <- as.integer(n)

  Sys.setenv(OMP_THREAD_LIMIT = n)
  options(sd_num_thread = n)
  data.table::setDTthreads(n)

  invisible(n)
}
