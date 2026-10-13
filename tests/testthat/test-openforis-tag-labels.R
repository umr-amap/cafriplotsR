# process_openforis_census(): recruit labels survive the tree workbook
#
# OpenForis writes recruits' labels to label_recrut. A team numbering recruits
# off their nearest stem reaches 22.10 after 22.9; read as a number that is
# 22.1, which used to be reported as a duplicated stem.

write_tree_workbook <- function(trees) {
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(trees, path)
  path
}

test_that(".read_excel_text_cols reads the named columns as text only", {
  skip_if_not_installed("writexl")
  path <- write_tree_workbook(data.frame(
    arbre = c(22, 22.1), label = c("22.10", "22.1"), dbh = c(10.5, 12),
    stringsAsFactors = FALSE
  ))
  res <- .read_excel_text_cols(path, c("arbre", "label", "absent"))

  expect_type(res$arbre, "character")
  expect_equal(res$arbre, c("22", "22.1"))
  expect_equal(res$label, c("22.10", "22.1"))
  expect_type(res$dbh, "double")
})

test_that("process_openforis_census keeps 22.10 apart from 22.1", {
  skip_if_not_installed("writexl")
  path <- write_tree_workbook(data.frame(
    plot_plot_name = "alpicam001",
    arbre          = c(22, NA, NA, NA),
    label_recrut   = c(NA, "22.1", "22.10", "22.11"),
    state          = c("alive", "recruted", "recruted", "recruted"),
    stem_diameter  = c(30, 10, 11, 12),
    herbarium_nbe_char = NA_character_,
    stringsAsFactors = FALSE
  ))

  res <- suppressWarnings(suppressMessages(
    process_openforis_census(tree_file = path)
  ))

  expect_null(res$duplicated_stems)
  expect_equal(nrow(res$lost_tag_zeros), 0)
  expect_setequal(unique(res$measurements$tag), c("22", "22.1", "22.10", "22.11"))
})

test_that("process_openforis_census reports a .10 lost as a number", {
  skip_if_not_installed("writexl")
  # The workbook stored the label as a number: 22.10 is already 22.1
  path <- write_tree_workbook(data.frame(
    plot_plot_name = "alpicam001",
    arbre          = c(22, NA, NA, NA),
    label_recrut   = c(NA, 22.1, 22.1, 22.11),
    state          = c("alive", "recruted", "recruted", "recruted"),
    stem_diameter  = c(30, 10, 11, 12),
    herbarium_nbe_char = NA_character_,
    stringsAsFactors = FALSE
  ))

  res <- suppressWarnings(suppressMessages(
    process_openforis_census(tree_file = path)
  ))

  expect_equal(nrow(res$duplicated_stems), 2)
  expect_equal(res$lost_tag_zeros$missing_suffix, ".10")
  expect_equal(res$lost_tag_zeros$candidates, "22.1")
})

test_that(".build_openforis_multi_stems orders labels and checks consecutive tags", {
  trees <- data.frame(
    plot_name = "P1",
    tag = c("100", "22", "23", "22.10", "22.2"),
    multi_stem = c(NA, "yes", NA, NA, NA),
    number_multi_stem = c(NA, 2, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  res <- .build_openforis_multi_stems(trees)

  # Field order puts 22.2 and 22.10 right after 22, before 23
  expect_equal(res$tag, c("22", "22.2"))
  expect_equal(res$group_tag, c("22", "22"))
  expect_equal(res$flag, c(NA, "tag_gap"))
})


# =============================================================================
# Small-tree and new-plot forms
# =============================================================================

test_that("the small-tree tree list keeps tags as labels", {
  raw <- data.frame(plot_plot_name_old = "Mbalmayo1",
                    tag = c("5.1", "5.10", "6"),
                    stringsAsFactors = FALSE)
  trees <- .normalise_openforis_small_tree_trees(raw)
  expect_equal(trees$tag, c("5.1", "5.10", "6"))
})

test_that("small-tree quadrats are assigned on the tag's numeric value", {
  quadrats <- data.frame(
    parent_plot_name = "mbalmayo001",
    quadrat_unit = c("mbalmayo001_20_20", "mbalmayo001_40_40"),
    quadrat = c("20_20", "40_40"),
    plot_name = "mbalmayo001_smalltree",
    firsttag = c(1, 105),
    stringsAsFactors = FALSE
  )
  trees <- data.frame(plot_name_raw = "Mbalmayo1",
                      parent_plot_name = "mbalmayo001",
                      tag = c("104.10", "104.1", "105", "A12"),
                      stringsAsFactors = FALSE)
  res <- suppressWarnings(suppressMessages(
    .assign_small_tree_quadrats(trees, quadrats)
  ))

  expect_equal(res$trees$tag, c("104.10", "104.1", "105"))
  expect_equal(res$trees$quadrat_unit,
               c("mbalmayo001_20_20", "mbalmayo001_20_20", "mbalmayo001_40_40"))
  expect_equal(res$unassigned$tag, "A12")
})

test_that("the new-plot tree list keeps tags as labels", {
  raw <- data.frame(plot_name = "P1", tag = c("22.1", "22.10"),
                    stringsAsFactors = FALSE)
  trees <- .normalise_openforis_new_plot_trees(raw, "plot_name", "tag")
  expect_equal(trees$tag, c("22.1", "22.10"))
})

test_that("new-plot individuals come out in field order", {
  trees <- data.frame(plot_name = "P1",
                      tag = c("22.10", "100", "22.2", "22"),
                      stringsAsFactors = FALSE)
  ind <- .prepare_openforis_new_plot_individuals(trees)
  expect_equal(ind$tag, c("22", "22.2", "22.10", "100"))
})

test_that(".plot_tag_order sorts by plot, then field order", {
  plot <- c("B", "A", "A", "A")
  tag  <- c("1", "22.10", "22.2", "3")
  expect_equal(.plot_tag_order(plot, tag), c(4L, 3L, 2L, 1L))
})


# =============================================================================
# Consecutive stems and number cells read as text
# =============================================================================

test_that(".tags_consecutive steps by the label's own shape", {
  expect_equal(.tags_consecutive(c("13.2", "13.3")), c(TRUE, TRUE))
  expect_equal(.tags_consecutive(c("120.8", "120.9", "120.10")),
               c(TRUE, TRUE, TRUE))
  expect_equal(.tags_consecutive(c("72", "73", "74")), c(TRUE, TRUE, TRUE))
  expect_equal(.tags_consecutive(c("373", "373.1")), c(TRUE, TRUE))
})

test_that(".tags_consecutive flags the stem after a break", {
  expect_equal(.tags_consecutive(c("50.2", "50.4")), c(TRUE, FALSE))
  expect_equal(.tags_consecutive(c("22", "22.2")), c(TRUE, FALSE))
  expect_equal(.tags_consecutive(c("111.1", "112")), c(TRUE, FALSE))
  expect_equal(.tags_consecutive(c("13.2", "14.2")), c(TRUE, FALSE))
  expect_equal(.tags_consecutive(c("12", "A13")), c(TRUE, FALSE))
  expect_equal(.tags_consecutive(c("12", NA)), c(TRUE, FALSE))
  expect_equal(.tags_consecutive("12"), TRUE)
  expect_equal(.tags_consecutive(character(0)), logical(0))
})

test_that("decimal multi-stems in sequence raise no tag_gap", {
  trees <- data.frame(
    plot_name = "P1",
    tag = c("13.2", "13.3", "14"),
    multi_stem = c("yes", NA, NA),
    number_multi_stem = c(2, NA, NA),
    stringsAsFactors = FALSE
  )
  res <- .build_openforis_multi_stems(trees)
  expect_equal(res$tag, c("13.2", "13.3"))
  expect_true(all(is.na(res$flag)))
})

test_that("a number cell read as text loses its float noise", {
  expect_equal(.normalize_tag(c("8.1999999999999993", "142.19999999999999",
                                "303.10000000000002", "21.10", "8.2")),
               c("8.2", "142.2", "303.1", "21.10", "8.2"))
})

test_that("long whole-number labels are left alone", {
  expect_equal(.normalize_tag("1234567890123456789"), "1234567890123456789")
})
