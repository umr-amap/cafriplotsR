# Tests for R/cache_backbone.R

make_backbone_fixture <- function() {
  tibble::tibble(
    idtax_n = 1:2,
    idtax_good_n = 1:2,
    tax_fam = c('Fabaceae', 'Clusiaceae'),
    tax_gen = c('Gilbertiodendron', 'Garcinia'),
    tax_esp = c('dewevrei', 'kola'),
    tax_sp_level = c('Gilbertiodendron dewevrei', 'Garcinia kola'),
    tax_gen_level = c('Gilbertiodendron', 'Garcinia'),
    tax_fam_level = c('Fabaceae', 'Clusiaceae'),
    tax_class_level = c('Magnoliopsida', 'Magnoliopsida')
  )
}

test_that('get_backbone_cache_path creates the cache directory when missing', {
  cache_dir <- file.path(tempdir(), paste0('cafriplots-cache-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .package = 'tools',
    R_user_dir = function(package, which) {
      expect_equal(package, 'CafriplotsR')
      expect_equal(which, 'cache')
      cache_dir
    }
  )

  result <- get_backbone_cache_path()

  expect_equal(result, cache_dir)
  expect_true(dir.exists(cache_dir))
})

test_that('cache helpers detect, save, load, and delete cache files', {
  cache_dir <- file.path(tempdir(), paste0('cafriplots-cache-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .package = 'tools',
    R_user_dir = function(package, which) cache_dir
  )

  expect_false(cache_exists())

  backbone <- make_backbone_fixture()
  expect_true(save_backbone_cache(backbone))
  expect_true(cache_exists())

  loaded <- load_backbone_cache()
  expect_s3_class(loaded, 'tbl_df')
  expect_equal(names(loaded), names(backbone))
  expect_equal(nrow(loaded), 2)

  expect_true(isTRUE(delete_backbone_cache()))
  expect_false(cache_exists())
  expect_false(isTRUE(delete_backbone_cache()))
})

test_that('get_cache_metadata formats age display and returns NULL for invalid metadata', {
  cache_dir <- file.path(tempdir(), paste0('cafriplots-cache-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  cache_file <- file.path(cache_dir, 'backbone_cache.rds')
  metadata_file <- file.path(cache_dir, 'backbone_metadata.rds')
  saveRDS(make_backbone_fixture(), cache_file)

  testthat::local_mocked_bindings(
    .package = 'tools',
    R_user_dir = function(package, which) cache_dir
  )

  saveRDS(list(download_date = Sys.Date(), n_records = 2, file_size_bytes = 2048, columns = 'idtax_n', cache_version = '1.0'), metadata_file)
  meta_today <- get_cache_metadata()
  expect_equal(meta_today$age_display, 'Today')
  expect_true(is.character(meta_today$size_display))
  expect_true(nzchar(meta_today$size_display))

  saveRDS(list(download_date = Sys.Date() - 1, n_records = 2, file_size_bytes = 2048, columns = 'idtax_n', cache_version = '1.0'), metadata_file)
  expect_equal(get_cache_metadata()$age_display, 'Yesterday')

  saveRDS(list(download_date = Sys.Date() - 10, n_records = 2, file_size_bytes = 2048, columns = 'idtax_n', cache_version = '1.0'), metadata_file)
  expect_match(get_cache_metadata()$age_display, 'weeks ago')

  saveRDS('bad metadata', metadata_file)
  expect_null(get_cache_metadata())
})

test_that('load_backbone_cache returns NULL for invalid cached structure', {
  cache_dir <- file.path(tempdir(), paste0('cafriplots-cache-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .package = 'tools',
    R_user_dir = function(package, which) cache_dir
  )

  saveRDS(tibble::tibble(idtax_n = 1L), file.path(cache_dir, 'backbone_cache.rds'))
  # Current version, so it is the structure check that rejects this and not
  # the version check added alongside it.
  saveRDS(list(download_date = Sys.Date(), n_records = 1, file_size_bytes = 1, columns = 'idtax_n', cache_version = CafriplotsR:::.backbone_cache_version), file.path(cache_dir, 'backbone_metadata.rds'))

  expect_null(load_backbone_cache())
})

test_that('load_backbone_cache discards a cache written by an older version', {
  # A cache from before a content fix loads fine and looks complete; the only
  # symptom is names that quietly stop matching. So it must be refused, not
  # trusted, and callers already read NULL as "download again".
  cache_dir <- file.path(tempdir(), paste0('cafriplots-cache-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .package = 'tools',
    R_user_dir = function(package, which) cache_dir
  )

  saveRDS(make_backbone_fixture(), file.path(cache_dir, 'backbone_cache.rds'))
  saveRDS(list(download_date = Sys.Date(), n_records = 2, file_size_bytes = 1, columns = 'idtax_n', cache_version = '1.0'), file.path(cache_dir, 'backbone_metadata.rds'))

  expect_warning(expect_null(load_backbone_cache()), NA)  # cli, not warning()

  # A cache saved by the current code is accepted again
  expect_true(save_backbone_cache(make_backbone_fixture()))
  expect_equal(nrow(load_backbone_cache()), 2)
})


test_that('load_backbone_cache reuses one expanded copy per process', {
  cache_dir <- file.path(tempdir(), paste0('cafriplots-memo-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(cache_dir, recursive = TRUE, force = TRUE)
    release_backbone_memory()
  }, add = TRUE)

  testthat::local_mocked_bindings(
    .package = 'tools',
    R_user_dir = function(package, which) cache_dir
  )

  release_backbone_memory()
  expect_true(save_backbone_cache(make_backbone_fixture()))

  # Pin the mtime to a whole second before the first load: Sys.setFileTime()
  # cannot restore the sub-millisecond part on Windows, so a stamp written at
  # full precision could not be put back and the memo would rightly miss.
  cache_file <- file.path(cache_dir, 'backbone_cache.rds')
  stamp <- as.POSIXct('2020-01-01 00:00:00', tz = 'UTC')
  Sys.setFileTime(cache_file, stamp)

  first <- load_backbone_cache()
  expect_s3_class(first, 'tbl_df')

  # Make the file unreadable while leaving its size and mtime alone, which is
  # all the memo keys on. A second call that still succeeds can only have come
  # from memory; deleting the file instead would prove nothing, because
  # cache_exists() refuses before the memo is ever consulted.
  size <- file.info(cache_file)$size
  writeBin(as.raw(rep(0L, size)), cache_file)
  Sys.setFileTime(cache_file, stamp)
  expect_equal(file.info(cache_file)$size, size)

  second <- load_backbone_cache()
  expect_identical(second, first)

  # ...and with the copy dropped, that same file is unreadable, which is what
  # makes the assertion above mean something.
  release_backbone_memory()
  expect_null(load_backbone_cache())
})

test_that('release_backbone_memory drops the held copy', {
  cache_dir <- file.path(tempdir(), paste0('cafriplots-memo-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(cache_dir, recursive = TRUE, force = TRUE)
    release_backbone_memory()
  }, add = TRUE)

  testthat::local_mocked_bindings(
    .package = 'tools',
    R_user_dir = function(package, which) cache_dir
  )

  release_backbone_memory()
  save_backbone_cache(make_backbone_fixture())
  load_backbone_cache()

  expect_true(release_backbone_memory())
  expect_false(release_backbone_memory())
})

test_that('a rewritten cache file invalidates the held copy', {
  cache_dir <- file.path(tempdir(), paste0('cafriplots-memo-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(cache_dir, recursive = TRUE, force = TRUE)
    release_backbone_memory()
  }, add = TRUE)

  testthat::local_mocked_bindings(
    .package = 'tools',
    R_user_dir = function(package, which) cache_dir
  )

  release_backbone_memory()
  save_backbone_cache(make_backbone_fixture())
  expect_equal(nrow(load_backbone_cache()), 2L)

  bigger <- dplyr::bind_rows(make_backbone_fixture(), make_backbone_fixture())
  save_backbone_cache(bigger)

  expect_equal(nrow(load_backbone_cache()), 4L)
})

test_that('delete_backbone_cache also forgets the held copy', {
  cache_dir <- file.path(tempdir(), paste0('cafriplots-memo-', Sys.getpid(), '-', as.integer(stats::runif(1, 1, 1e6))))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(cache_dir, recursive = TRUE, force = TRUE)
    release_backbone_memory()
  }, add = TRUE)

  testthat::local_mocked_bindings(
    .package = 'tools',
    R_user_dir = function(package, which) cache_dir
  )

  release_backbone_memory()
  save_backbone_cache(make_backbone_fixture())
  load_backbone_cache()

  delete_backbone_cache()

  expect_false(release_backbone_memory())
  expect_null(load_backbone_cache())
})
