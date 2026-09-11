# Tests for container CPU-quota detection and OpenMP thread capping.
#
# The cgroup readers take a `root` argument so these run against fixture
# directories, on any OS, without a container.

.cgroup_v2 <- function(cpu_max) {
  root <- tempfile("cgroup_v2_")
  dir.create(root)
  writeLines(cpu_max, file.path(root, "cpu.max"))
  root
}

.cgroup_v1 <- function(quota, period) {
  root <- tempfile("cgroup_v1_")
  dir.create(file.path(root, "cpu"), recursive = TRUE)
  writeLines(quota,  file.path(root, "cpu", "cpu.cfs_quota_us"))
  writeLines(period, file.path(root, "cpu", "cpu.cfs_period_us"))
  root
}

test_that(".container_cpu_limit() reads a cgroup v2 quota", {
  # The exact value observed on the SSP Cloud taxonomic-match pod.
  expect_identical(.container_cpu_limit(.cgroup_v2("200000 100000")), 2L)
  expect_identical(.container_cpu_limit(.cgroup_v2("400000 100000")), 4L)
})

test_that(".container_cpu_limit() rounds a fractional quota up", {
  expect_identical(.container_cpu_limit(.cgroup_v2("150000 100000")), 2L)
  # Below one CPU is still one thread, never zero.
  expect_identical(.container_cpu_limit(.cgroup_v2("50000 100000")), 1L)
})

test_that(".container_cpu_limit() reports no limit as NA", {
  expect_identical(.container_cpu_limit(.cgroup_v2("max 100000")), NA_integer_)
  expect_identical(.container_cpu_limit(.cgroup_v1("-1", "100000")), NA_integer_)
  expect_identical(.container_cpu_limit(tempfile("no_cgroup_")), NA_integer_)
})

test_that(".container_cpu_limit() reads a cgroup v1 quota", {
  expect_identical(.container_cpu_limit(.cgroup_v1("300000", "100000")), 3L)
})

test_that(".container_cpu_limit() tolerates a malformed file", {
  expect_identical(.container_cpu_limit(.cgroup_v2("garbage")), NA_integer_)
  expect_identical(.container_cpu_limit(.cgroup_v2("abc 100000")), NA_integer_)
  expect_identical(.container_cpu_limit(.cgroup_v2("")), NA_integer_)
})

test_that(".apply_container_thread_limits() does nothing without a limit", {
  withr::local_options(sd_num_thread = 17L)
  withr::local_envvar(OMP_THREAD_LIMIT = NA)

  expect_identical(.apply_container_thread_limits(NA_integer_), NA_integer_)
  expect_identical(getOption("sd_num_thread"), 17L)
  expect_identical(Sys.getenv("OMP_THREAD_LIMIT"), "")
})

test_that(".apply_container_thread_limits() caps stringdist, data.table and children", {
  withr::local_options(sd_num_thread = getOption("sd_num_thread"))
  withr::local_envvar(OMP_THREAD_LIMIT = NA)
  old_dt <- data.table::getDTthreads()
  withr::defer(data.table::setDTthreads(old_dt))

  expect_identical(.apply_container_thread_limits(2L), 2L)

  expect_identical(getOption("sd_num_thread"), 2L)
  expect_identical(data.table::getDTthreads(), 2L)
  # Exported so a callr worker inherits the cap when stringdist loads there.
  expect_identical(Sys.getenv("OMP_THREAD_LIMIT"), "2")
})
