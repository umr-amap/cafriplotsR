# Reviewing uncertain backbone matches (R/backbone_review.R).


.review_matches <- function() {
  data.frame(
    idtax_n             = c(1L, 2L, 3L, 3L, 4L),
    taxon_name_internal = c("Psychotria pleuropoda", "Diospyros macrophylla",
                            "Vigna mungo", "Vigna mungo", "Cyathula lanceolata"),
    authors_internal    = c("Hiern", "Blume", NA, NA, "Schinz"),
    external_id         = c("11", "21", "31", "32", "41"),
    backbone_taxon_name = c("Psychotria leucopoda", "Diospyros macrophylla",
                            "Vigna mungo", "Vigna mungo", "Cyathula lanceolata"),
    backbone_authors    = c("Petit", "A.Chev.", "(L.) Hepper", "L.", "Schinz"),
    backbone_status     = c("Accepted", "Synonym", "Accepted", "Synonym", "Accepted"),
    match_type          = c("fuzzy", "author_mismatch", "exact", "exact", "exact"),
    match_score         = c(0.905, 1, 1, 1, 1),
    author_score        = c(0.3, 0.45, NA, NA, 1),
    stringsAsFactors = FALSE
  )
}


test_that(".review_rows() sorts rows into review kinds", {
  d <- .review_rows(.review_matches())
  expect_equal(d$review_kind, c("fuzzy", "author_mismatch", "several", "several", NA))
  expect_equal(d$.key[1], "1|11")
  expect_true(all(is.na(d$decision)))
})

test_that(".review_rows() tolerates matches without the optional columns", {
  d <- .review_rows(data.frame(idtax_n = 1L, external_id = "1", match_type = "fuzzy"))
  expect_true(all(c("taxon_name_internal", "backbone_authors", "author_score") %in% names(d)))
  expect_error(.review_rows(data.frame(idtax_n = 1L)), "external_id")
})

test_that(".mark_word_diff() highlights differing words and escapes HTML", {
  res <- .mark_word_diff(c("Psychotria pleuropoda", "A <b>", NA),
                         c("Psychotria leucopoda", "A <b>", "x"))
  expect_equal(res$x[1], "Psychotria <mark>pleuropoda</mark>")
  expect_equal(res$y[1], "Psychotria <mark>leucopoda</mark>")
  expect_equal(res$x[2], "A &lt;b&gt;")
  expect_equal(res$x[3], "")
  expect_equal(res$y[3], "<mark>x</mark>")
})

test_that(".set_decision() accepting a candidate rejects the taxon's other undecided ones", {
  d <- .review_rows(.review_matches())
  d <- .set_decision(d, "3|31", "accepted")
  expect_equal(d$decision[d$.key == "3|31"], "accepted")
  expect_equal(d$decision[d$.key == "3|32"], "rejected")
  expect_true(is.na(d$decision[d$.key == "1|11"]))

  d <- .set_decision(d, "3|31", NA_character_)
  expect_true(is.na(d$decision[d$.key == "3|31"]))
  expect_equal(d$decision[d$.key == "3|32"], "rejected")

  expect_identical(.set_decision(d, "no|key", "accepted"), d)
})

test_that("decisions survive a round trip through the review file", {
  f <- withr::local_tempfile(fileext = ".rds")
  d <- .review_rows(.review_matches())
  d <- .set_decision(d, c("1|11", "2|21"), "rejected")
  d <- .set_decision(d, "3|32", "accepted")
  .save_review_file(d, f)

  fresh <- .load_review_file(.review_rows(.review_matches()), f)
  expect_equal(fresh$decision, d$decision)
  expect_identical(.load_review_file(d, NULL), d)
})

test_that(".review_result() marks accepted rows verified", {
  d <- .set_decision(.review_rows(.review_matches()), "1|11", "accepted")
  res <- .review_result(d)
  expect_equal(res$verified, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_false(".key" %in% names(res))
})

test_that(".review_display() shows one row per match with badges", {
  d <- .review_rows(.review_matches())[1:2, ]
  d$decision[2] <- "rejected"
  out <- .review_display(d)
  expect_equal(nrow(out), 2)
  expect_match(out$Decision[1], "rv-undecided")
  expect_match(out$Decision[2], "rv-rejected")
  expect_equal(out$Kind, c("Fuzzy name", "Authors differ"))
})

test_that("review_backbone_matches(open = FALSE) applies the saved decisions", {
  f <- withr::local_tempfile(fileext = ".rds")
  d <- .set_decision(.review_rows(.review_matches()), "1|11", "accepted")
  d <- .set_decision(d, "2|21", "rejected")
  .save_review_file(d, f)

  res <- review_backbone_matches(.review_matches(), review_file = f, open = FALSE)
  expect_equal(res$decision, c("accepted", "rejected", NA, NA, NA))
  expect_equal(res$verified, c(TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("review_backbone_matches() returns at once when nothing needs review", {
  m <- .review_matches()[5, ]
  res <- review_backbone_matches(m)
  expect_equal(nrow(res), 1)
  expect_false(res$verified)
})

test_that("the review app records keyboard and bulk decisions", {
  skip_if_not_installed("DT")
  f <- withr::local_tempfile(fileext = ".rds")
  app <- .review_app(.review_matches(), review_file = f)

  shiny::testServer(app, {
    session$setInputs(kinds = names(.review_kind_labels), show = "undecided",
                      score = c(0, 1))
    expect_equal(length(shown()), 4)

    # the first row shown is the fuzzy one
    session$setInputs(table_rows_selected = 1)
    session$setInputs(key_action = list(action = "reject", nonce = 1))
    expect_equal(rv$data$decision[rv$data$.key == "1|11"], "rejected")
    expect_true(file.exists(f))

    session$setInputs(bulk_name = 0.9, bulk_author = 0.4, bulk_author_na = FALSE)
    session$setInputs(bulk = 1)
    expect_equal(rv$data$decision[rv$data$.key == "2|21"], "accepted")
    expect_true(all(is.na(rv$data$decision[rv$data$review_kind %in% "several"])))
  })
})
