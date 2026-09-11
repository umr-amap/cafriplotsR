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

# ---------------------------------------------------------------------------
# Backbone name index (.prepare_backbone_for_matching)
# ---------------------------------------------------------------------------

test_that("the name index holds exactly what the matchers used to rebuild", {
  bb  <- .pipeline_backbone()
  idx <- attr(.prepare_backbone_for_matching(bb), "cafri_name_index")

  expect_identical(idx$plain, .build_backbone_name_field(bb, include_authors = FALSE))
  expect_identical(idx$auth,  .build_backbone_name_field(bb, include_authors = TRUE))
  expect_identical(idx$plain_lc, tolower(idx$plain))
  expect_identical(idx$auth_lc,  tolower(idx$auth))
  expect_identical(
    idx$genera,
    bb %>% dplyr::filter(!is.na(tax_gen)) %>% dplyr::distinct(tax_gen) %>%
      dplyr::pull(tax_gen)
  )
})

test_that("preparing twice is a no-op", {
  once  <- .prepare_backbone_for_matching(.pipeline_backbone())
  twice <- .prepare_backbone_for_matching(once)
  expect_identical(attr(twice, "cafri_name_index"), attr(once, "cafri_name_index"))
})

test_that("a filtered backbone does not read a stale index", {
  prepared <- .prepare_backbone_for_matching(.pipeline_backbone())
  filtered <- prepared[prepared$tax_level == "species", ]

  expect_null(.valid_name_index(filtered))
  # Recomputed for the rows actually present, not read from the parent.
  expect_identical(.backbone_name_index(filtered)$plain,
                   .build_backbone_name_field(filtered, include_authors = FALSE))
})

test_that("matching gives identical results with and without the index", {
  bb       <- .pipeline_backbone()
  prepared <- .prepare_backbone_for_matching(bb)
  names    <- c("Garcinia kola", "Garcinia kolla", "Garcina kola",
                "Brachystegia laurenti", "Garcinia", "Clusiaceae",
                "Zzzzzz qqqqqq")

  for (ia in c(FALSE, TRUE)) for (ms in c(0.3, 0.6)) {
    run <- function(b) {
      match_taxonomic_names(names, method = "hierarchical", max_matches = 5,
                            min_similarity = ms, include_synonyms = TRUE,
                            include_authors = ia, backbone = b, verbose = FALSE)
    }
    # The raw backbone takes the prepare-on-entry path, the prepared one the
    # no-op path: both must agree. Equivalence with the pre-index code rests on
    # the index-content test above: the matchers read exactly the vectors they
    # used to rebuild.
    expect_identical(run(prepared), run(bb))
  }
})

test_that(".trigram_sim_lc() equals .trigram_sim() on lowercase input", {
  x <- c("Garcinia kola", "brachystegia", NA, "")
  expect_identical(.trigram_sim_lc(tolower(x), "Garcina Kola"),
                   .trigram_sim(x, "Garcina Kola"))
  expect_identical(.trigram_sim_lc(character(0), "x"), numeric(0))
})
