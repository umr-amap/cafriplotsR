# Tests for the taxonomic matching pipeline extracted from mod_auto_matching.
#
# The point of the extraction is that the computation no longer needs a Shiny
# session, a database or a reactive context — so it can be tested directly, and
# run in a background process. These tests exercise it as a plain function.

# Minimal backbone with the columns .run_matching_pipeline() reads, shaped the
# way save_backbone_cache() / .shape_backbone() leave it.
.pipeline_backbone <- function() {
  bb <- tibble::tibble(
    idtax_n      = 1:6,
    idtax_good_n = c(1L, 2L, 3L, 1L, 5L, 6L),
    tax_fam      = c("Clusiaceae", "Clusiaceae", "Fabaceae",
                     "Clusiaceae", "Fabaceae", NA),
    tax_famclass = c(rep("Magnoliopsida", 5), NA),
    tax_gen      = c("Garcinia", "Garcinia", "Brachystegia",
                     "Garcinia", NA, NA),
    tax_esp      = c("kola", "punctata", "laurentii", "cola", NA, NA),
    tax_rank01   = NA_character_,
    tax_nam01    = NA_character_,
    tax_rank02   = NA_character_,
    tax_nam02    = NA_character_,
    tax_level    = c("species", "species", "species", "species",
                     "family", "genus"),
    author1      = NA_character_
  )
  bb$tax_sp_level    <- ifelse(!is.na(bb$tax_esp),
                               paste(bb$tax_gen, bb$tax_esp), NA_character_)
  bb$tax_gen_level   <- bb$tax_gen
  bb$tax_fam_level   <- bb$tax_fam
  bb$tax_class_level <- bb$tax_famclass
  bb
}

.pipeline_args <- function(names, ...) {
  utils::modifyList(
    list(
      user_df         = data.frame(taxa = names, stringsAsFactors = FALSE),
      col_name        = "taxa",
      backbone        = .pipeline_backbone(),
      min_similarity  = 0.6,
      include_authors = FALSE,
      input_hash      = "test_hash",
      rm_mode         = "fresh",
      checkpoint_file = tempfile(fileext = ".rds"),
      cancel_file     = NULL,
      progress        = NULL
    ),
    list(...)
  )
}

test_that(".run_matching_pipeline() exact-matches species names", {
  res <- do.call(.run_matching_pipeline,
                 .pipeline_args(c("Garcinia kola", "Garcinia punctata")))

  expect_identical(res$status, "ok")
  expect_equal(nrow(res$updated_data), 2)
  expect_identical(res$updated_data$idtax_n, c(1L, 2L))
  expect_true(all(res$updated_data$match_method == "exact"))
  expect_identical(res$stats$n_exact, 2L)
  expect_identical(res$stats$n_unmatched, 0L)
})

test_that(".run_matching_pipeline() resolves a synonym to its accepted name", {
  # idtax_n 4 (Garcinia cola) points at idtax_good_n 1 (Garcinia kola).
  res <- do.call(.run_matching_pipeline, .pipeline_args("Garcinia cola"))

  expect_identical(res$status, "ok")
  expect_true(res$updated_data$is_synonym)
  expect_identical(res$updated_data$corrected_name, "Garcinia kola")
})

test_that(".run_matching_pipeline() returns every input row, matched or not", {
  res <- do.call(
    .run_matching_pipeline,
    .pipeline_args(c("Garcinia kola", "Zzzzzz qqqqqq"), min_similarity = 0.95)
  )

  expect_identical(res$status, "ok")
  expect_equal(nrow(res$updated_data), 2)
  expect_true(is.na(res$updated_data$idtax_n[2]))
  expect_identical(res$stats$total_names, 2L)
})

test_that(".run_matching_pipeline() reports an empty column rather than failing", {
  res <- do.call(.run_matching_pipeline,
                 .pipeline_args(character(0)))

  expect_identical(res$status, "empty")
})

test_that(".run_matching_pipeline() rejects a column that is not in the data", {
  expect_error(
    do.call(.run_matching_pipeline, .pipeline_args("Garcinia kola",
                                                   col_name = "absent")),
    "not in the supplied data"
  )
})

test_that(".run_matching_pipeline() stops when the cancel file appears", {
  cancel <- tempfile()
  file.create(cancel)
  on.exit(unlink(cancel), add = TRUE)

  # A name no exact stage can match, so the run reaches the fuzzy loop, which
  # is where cancellation is checked.
  res <- do.call(
    .run_matching_pipeline,
    .pipeline_args("Zzzzzz qqqqqq", cancel_file = cancel)
  )

  expect_identical(res$status, "cancelled")
})

test_that(".run_matching_pipeline() writes its checkpoint where it is told", {
  chk <- tempfile(fileext = ".rds")
  on.exit(unlink(chk), add = TRUE)

  # 30 unmatchable names: past the 25-name checkpoint throttle, so at least one
  # checkpoint is written before the run ends.
  names <- paste0("Zzzzzz qqqqq", sprintf("%02d", 1:30))
  res <- do.call(
    .run_matching_pipeline,
    .pipeline_args(names, checkpoint_file = chk, min_similarity = 0.95)
  )

  expect_identical(res$status, "ok")
  # Deleted on success — the point is that nothing was written to the default
  # tempdir() path instead.
  expect_false(file.exists(chk))
  expect_false(file.exists(.checkpoint_path("test_hash")))
})

test_that("progress round-trips through the progress file", {
  f <- tempfile(fileext = ".json")
  on.exit(unlink(c(f, paste0(f, ".tmp"))), add = TRUE)

  .write_matching_progress(f, "fuzzy", i = 7L, n = 42L, name = "Garcinia kola")
  p <- .read_matching_progress(f)

  expect_identical(p$stage, "fuzzy")
  expect_identical(as.integer(p$i), 7L)
  expect_identical(as.integer(p$n), 42L)
  expect_identical(p$name, "Garcinia kola")
})

test_that("progress helpers tolerate a missing or NULL file", {
  expect_null(.read_matching_progress(NULL))
  expect_null(.read_matching_progress(tempfile()))
  expect_silent(.write_matching_progress(NULL, "fuzzy"))
})

test_that(".matching_cancelled() is FALSE without a cancel file", {
  expect_false(.matching_cancelled(NULL))
  expect_false(.matching_cancelled(tempfile()))

  f <- tempfile()
  file.create(f)
  on.exit(unlink(f), add = TRUE)
  expect_true(.matching_cancelled(f))
})

test_that(".async_matching_available() honours the opt-out", {
  withr::with_options(list(cafri.async_matching = FALSE), {
    expect_false(.async_matching_available())
  })
})

test_that("checkpoint helpers honour an explicit path", {
  chk <- tempfile(fileext = ".rds")
  on.exit(unlink(chk), add = TRUE)

  .save_matching_checkpoint(
    input_hash = "explicit_path_hash",
    best_matches = data.frame(input_name = "a"),
    fuzzy_results = list(),
    still_unmatched = "a",
    current_index = 1L,
    total_names = 1L,
    path = chk
  )

  expect_true(file.exists(chk))
  expect_false(file.exists(.checkpoint_path("explicit_path_hash")))

  loaded <- .load_matching_checkpoint("explicit_path_hash", path = chk)
  expect_identical(loaded$current_index, 1L)
  expect_identical(loaded$still_unmatched, "a")

  .delete_matching_checkpoint("explicit_path_hash", path = chk)
  expect_false(file.exists(chk))
})
