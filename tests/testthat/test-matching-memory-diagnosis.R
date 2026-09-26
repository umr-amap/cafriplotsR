# Tests for .diagnose_matching_memory() in R/taxonomic_matching_pipeline.R
#
# One condition -- the machine out of memory -- reaches the user as three
# unrelated-looking messages. These pin down which ones are recognised, and
# just as importantly that an ordinary error is still reported as itself.

test_that('.diagnose_matching_memory recognises the Windows commit-limit failure', {
  logs <- paste(
    'LoadLibrary failure:  The paging file is too small for this operation to complete.',
    "Error: package or namespace load failed for 'CafriplotsR'",
    sep = '\n'
  )

  msg <- .diagnose_matching_memory(logs)

  expect_type(msg, 'character')
  expect_match(msg, 'ran out of memory')
  expect_match(msg, 'Restart R')
})

test_that('.diagnose_matching_memory recognises a failed allocation whatever its size', {
  for (size in c('2.6 Mb', '1.4 Gb', '500 Kb')) {
    msg <- .diagnose_matching_memory(paste0('Error: cannot allocate vector of size ', size))
    expect_match(msg, 'ran out of memory')
    # The size is the wrong thing to act on, so the message has to say so.
    expect_match(msg, 'not the size of your file')
  }
})

test_that('.diagnose_matching_memory recognises a C++ allocation failure', {
  msg <- .diagnose_matching_memory("terminate called after throwing an instance of 'std::bad_alloc'")
  expect_match(msg, 'ran out of memory')
})

test_that('.diagnose_matching_memory reports an OOM kill from the exit status alone', {
  msg <- .diagnose_matching_memory('', status = 137L)

  expect_match(msg, 'exit 137')
  expect_match(msg, 'Restart R')
})

test_that('.diagnose_matching_memory passes ordinary failures through', {
  expect_null(.diagnose_matching_memory('Error in dbConnect(): could not connect to server'))
  expect_null(.diagnose_matching_memory(''))
  expect_null(.diagnose_matching_memory(character(0)))
  expect_null(.diagnose_matching_memory(NA_character_))
  expect_null(.diagnose_matching_memory('', status = NA_integer_))
  expect_null(.diagnose_matching_memory('', status = 1L))
})

test_that('.diagnose_matching_memory matches regardless of case', {
  expect_false(is.null(.diagnose_matching_memory('CANNOT ALLOCATE VECTOR of size 3 Mb')))
})

test_that('.matching_job_error explains a memory failure and keeps the raw output', {
  dir <- file.path(tempdir(), paste0('cafri-joberr-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines(
    c('loading namespace', 'LoadLibrary failure:  The paging file is too small for this operation to complete.'),
    file.path(dir, 'stderr.log')
  )

  job <- list(dir = dir, proc = list(get_exit_status = function() 1L))

  msg <- .matching_job_error(job)

  expect_match(msg, 'ran out of memory')
  # Explained for the user, but the original is still there for whoever has to
  # debug it -- losing it is what left us guessing from a half-quoted report.
  expect_match(msg, 'paging file is too small')
})

test_that('.matching_job_error reads stdout when stderr is empty', {
  dir <- file.path(tempdir(), paste0('cafri-joberr-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines('something went wrong in the worker', file.path(dir, 'stdout.log'))

  job <- list(dir = dir, proc = list(get_exit_status = function() 1L))

  expect_match(.matching_job_error(job), 'something went wrong in the worker')
})

test_that('.matching_job_error falls back to the exit status with no logs', {
  dir <- file.path(tempdir(), paste0('cafri-joberr-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  job <- list(dir = dir, proc = list(get_exit_status = function() 3L))
  expect_match(.matching_job_error(job), 'exited with status 3')

  job0 <- list(dir = dir, proc = list(get_exit_status = function() 0L))
  expect_match(.matching_job_error(job0), 'without producing a result')
})
