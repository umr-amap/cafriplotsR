# Recruit tag labels: reading, ordering and resolving them
#
# 22.10 and 22.1 are two recruits. A CSV read with type guessing turns both
# into the same number before any module sees them.

write_csv_upload <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  list(name = "census.csv", datapath = path)
}

test_that(".type_convert_keep_labels keeps a column text when numbers would merge", {
  df <- data.frame(tag = c("22.1", "22.10", "23"),
                   dbh = c("10.5", "12", "15.25"),
                   stringsAsFactors = FALSE)
  res <- .type_convert_keep_labels(df)

  expect_type(res$tag, "character")
  expect_equal(res$tag, c("22.1", "22.10", "23"))
  expect_type(res$dbh, "double")
})

test_that(".type_convert_keep_labels converts like read.csv otherwise", {
  df <- data.frame(n = c("1", "2", NA), x = c("a", "b", "c"),
                   ok = c("TRUE", "FALSE", "TRUE"), stringsAsFactors = FALSE)
  res <- .type_convert_keep_labels(df)

  expect_type(res$n, "integer")
  expect_type(res$x, "character")
  expect_type(res$ok, "logical")
})

test_that(".type_convert_keep_labels treats repeats of one value as no merge", {
  df <- data.frame(tag = c("22.1", "22.1", "5"), stringsAsFactors = FALSE)
  expect_type(.type_convert_keep_labels(df)$tag, "double")
})

test_that(".read_uploaded_table keeps 22.10 in a CSV", {
  f <- write_csv_upload(c("plot_name,tag,dbh",
                          "P1,22.1,10",
                          "P1,22.10,11",
                          "P1,22.11,12"))
  res <- .read_uploaded_table(f)

  expect_equal(res$tag, c("22.1", "22.10", "22.11"))
  expect_equal(.normalize_tag(res$tag), c("22.1", "22.10", "22.11"))
  expect_type(res$dbh, "integer")
})


# =============================================================================
# Ordering and resolving labels
# =============================================================================

test_that(".tag_order counts recruits the way the field does", {
  x <- c("22.10", "100", "22.2", "22", "A12", "22.1", "3", NA)
  expect_equal(x[.tag_order(x)],
               c("3", "22", "22.1", "22.2", "22.10", "100", "A12", NA))
})

test_that(".tag_order accepts numbers", {
  expect_equal(.tag_order(c(10, 2, 2.5)), c(2L, 3L, 1L))
  expect_equal(.tag_order(character(0)), integer(0))
})

test_that(".compute_stem_order puts 22.2 before 22.10", {
  grp <- data.frame(plot_name = "P1",
                    tag = c("22.10", "22", "22.2"),
                    group_tag = "22", stringsAsFactors = FALSE)
  expect_equal(.compute_stem_order(grp)$stem_order, c(3L, 1L, 2L))
})

test_that(".match_tag_to_individual resolves 22.10 and 22.1 to their own stems", {
  existing <- data.frame(id_n = c(11L, 12L, 13L), plot = c(1L, 1L, 2L),
                         tag = c(22.1, 22.1, 22.1),
                         tag_label = c("22.1", "22.10", "22.1"),
                         stringsAsFactors = FALSE)
  hit <- .match_tag_to_individual(
    plot = c(1L, 1L, 2L, 2L, NA, 1L),
    tag  = c("22.10", "22.1", "22.1", "22.10", "22.1", NA),
    existing = existing, existing_plot = existing$plot
  )
  expect_equal(existing$id_n[hit], c(12L, 11L, 13L, NA, NA, NA))
})

test_that(".match_tag_to_individual falls back to tag on legacy rows", {
  existing <- data.frame(id_n = 1:2, tag = c(101, 102),
                         tag_label = c(NA, NA))
  expect_equal(.match_tag_to_individual(c("P", "P"), c("102", "0101"),
                                        existing, c("P", "P")),
               c(2L, 1L))
})

test_that(".match_tag_to_individual never matches a missing tag to a missing tag", {
  existing <- data.frame(id_n = 1L, tag = NA_real_, tag_label = NA_character_)
  expect_equal(.match_tag_to_individual("P", NA, existing, "P"), NA_integer_)
})


# =============================================================================
# Import wizard (launch_import_wizard)
# =============================================================================

make_import_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "data_liste_plots", data.frame(
    id_liste_plots = 1L, plot_name = "P1", stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "data_individuals", data.frame(
    id_n = c(11L, 12L), id_table_liste_plots_n = 1L,
    tag = c(22, 22.1), tag_label = c("22", "22.1"),
    stem_grouping = NA_integer_, stringsAsFactors = FALSE))
  con
}

test_that(".prepare_individuals_data writes the tag as written to tag_label", {
  res <- .prepare_individuals_data(
    data.frame(id_table_liste_plots_n = 1L, tag = c("22.1", "22.10"),
               idtax_n = 1L, stringsAsFactors = FALSE),
    progress = FALSE
  )
  expect_equal(res$tag_label, c("22.1", "22.10"))
})

test_that("the database conflict check sees 22.1 but not 22.10", {
  skip_if_not_installed("RSQLite")
  con <- make_import_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- .validate_tag_conflicts_database(
    data.frame(plot_name = "P1", tag = c("22.1", "22.10"),
               stringsAsFactors = FALSE), con)

  expect_length(res, 1)
  expect_match(res[[1]], "22.1")
  expect_no_match(res[[1]], "22.10")
})

test_that("a multi-stem can hang off recruit 22.10 in the same import", {
  skip_if_not_installed("RSQLite")
  con <- make_import_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  data <- data.frame(plot_name = "P1",
                     tag = c("22.10", "22.11"),
                     multi_tiges_id = c(NA, "22.10"),
                     stringsAsFactors = FALSE)
  res <- .validate_multi_stem_grouping(data, con)
  expect_length(res$errors, 0)

  # Pointing at 22.20, which exists nowhere, is still an error
  data$multi_tiges_id[2] <- "22.20"
  expect_length(.validate_multi_stem_grouping(data, con)$errors, 1)
})

test_that("features link to individuals on the label", {
  ind  <- data.frame(plot_name = "P1", tag = c("22.1", "22.10"),
                     stringsAsFactors = FALSE)
  feat <- data.frame(plot_name = "P1", tag = c("22.10", "22.20"),
                     stringsAsFactors = FALSE)
  res <- .validate_feature_individual_linkage(feat, ind)
  expect_length(res$errors, 1)
  expect_match(res$errors[[1]], "1 row")
})
