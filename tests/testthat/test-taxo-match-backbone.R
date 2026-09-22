# The name standardization app used to offer one foreign backbone, WCVP, behind
# a checkbox. It now offers whichever backbones the database registers as a
# source of names. These tests pin the parts that decide what the output
# columns are called and which names end up in `corrected_name` — none of them
# needs a database or a Shiny session.

# ---- Which backbone was asked for ------------------------------------------

test_that("no selector at all means the internal backbone", {
  expect_equal(.chosen_name_backbone(NULL), "internal")
})

test_that("a reactive returning a code is taken at its word", {
  expect_equal(.chosen_name_backbone(function() "apd"), "apd")
  expect_equal(.chosen_name_backbone("wcvp"), "wcvp")
})

test_that("an empty or missing choice falls back to the internal backbone", {
  expect_equal(.chosen_name_backbone(function() NULL), "internal")
  expect_equal(.chosen_name_backbone(function() ""), "internal")
  expect_equal(.chosen_name_backbone(function() NA_character_), "internal")
  expect_equal(.chosen_name_backbone(character(0)), "internal")
})

test_that("a selector that errors does not take the run down with it", {
  expect_equal(.chosen_name_backbone(function() stop("no session")), "internal")
})

# ---- Joining a backbone's names onto the match output -----------------------

match_output <- function() {
  dplyr::tibble(
    input_name     = c("Cola nitida", "Garcinia kola", "Nowhere name"),
    idtax_n        = c(1L, 2L, NA_integer_),
    corrected_name = c("Cola nitida", "Garcinia kola", NA_character_)
  )
}

backbone_info <- function() {
  dplyr::tibble(
    idtax_n             = c(1L, 2L),
    backbone_name_id    = c("100", "200"),
    backbone_taxon_name = c("Cola nitida (Vent.) Schott & Endl.", NA_character_),
    backbone_family     = c("Malvaceae", NA_character_),
    backbone_authors    = c("(Vent.) Schott & Endl.", NA_character_),
    backbone_status_raw = c("Accepted", NA_character_),
    name_source         = c("apd", "internal")
  )
}

test_that("a backbone name replaces the standardized one where there is one", {
  out <- .apply_backbone_names(match_output(), backbone_info(), "apd")

  expect_equal(out$corrected_name[1], "Cola nitida (Vent.) Schott & Endl.")
  expect_equal(out$backbone_family[1], "Malvaceae")
  expect_equal(out$name_source[1], "apd")
})

test_that("a taxon the backbone does not have keeps its internal name", {
  out <- .apply_backbone_names(match_output(), backbone_info(), "apd")

  expect_equal(out$corrected_name[2], "Garcinia kola")
  expect_equal(out$name_source[2], "internal")
})

test_that("a name that matched nothing is left alone, not blamed on a backbone", {
  out <- .apply_backbone_names(match_output(), backbone_info(), "apd")

  expect_true(is.na(out$corrected_name[3]))
  expect_equal(out$name_source[3], "internal")
})

test_that("row count and user columns survive the join", {
  out <- .apply_backbone_names(match_output(), backbone_info(), "apd")

  expect_equal(nrow(out), 3)
  expect_equal(out$input_name, match_output()$input_name)
})

test_that("WCVP keeps the column names it had before other backbones existed", {
  out <- .apply_backbone_names(match_output(), backbone_info(), "wcvp")

  expect_equal(out$wcvp_taxon_name, out$backbone_taxon_name)
  expect_equal(out$wcvp_family, out$backbone_family)
  expect_equal(out$wcvp_taxon_authors, out$backbone_authors)
  expect_equal(out$wcvp_taxon_status, out$backbone_status_raw)
})

test_that("another backbone does not write itself into the WCVP columns", {
  out <- .apply_backbone_names(match_output(), backbone_info(), "apd")

  expect_false("wcvp_taxon_name" %in% names(out))
})

test_that("running a second time overwrites rather than suffixes", {
  once  <- .apply_backbone_names(match_output(), backbone_info(), "wcvp")
  twice <- .apply_backbone_names(once, backbone_info(), "wcvp")

  expect_false(any(grepl("\\.x$|\\.y$", names(twice))))
  expect_equal(sum(names(twice) == "backbone_taxon_name"), 1L)
  expect_equal(twice$corrected_name, once$corrected_name)
})

test_that("switching backbone clears the previous one's columns", {
  wcvp_first <- .apply_backbone_names(match_output(), backbone_info(), "wcvp")
  then_apd   <- .apply_backbone_names(wcvp_first, backbone_info(), "apd")

  expect_false("wcvp_taxon_name" %in% names(then_apd))
})

test_that("an empty lookup leaves the data untouched", {
  data <- match_output()
  empty <- backbone_info()[0, ]

  expect_equal(.apply_backbone_names(data, empty, "apd"), data)
  expect_equal(.apply_backbone_names(data, NULL, "apd"), data)
})

test_that("data without idtax_n is returned rather than joined blindly", {
  data <- dplyr::tibble(input_name = "Cola nitida")

  expect_equal(.apply_backbone_names(data, backbone_info(), "apd"), data)
})

test_that("a duplicated taxon in the lookup does not duplicate rows", {
  doubled <- dplyr::bind_rows(backbone_info(), backbone_info()[1, ])
  out <- .apply_backbone_names(match_output(), doubled, "apd")

  expect_equal(nrow(out), 3)
})

# ---- The columns the pipeline claims ----------------------------------------

test_that("the output columns cover both the generic and the WCVP names", {
  cols <- .taxo_match_output_columns()

  expect_true(all(c("backbone_taxon_name", "backbone_family",
                    "backbone_authors", "backbone_status_raw") %in% cols))
  # Reserved whatever the chosen backbone, so an uploaded wcvp_* column is
  # parked either way
  expect_true(all(c("wcvp_taxon_name", "wcvp_taxon_status") %in% cols))
  expect_true("name_source" %in% cols)
})

test_that("a user column named after a backbone column is parked, not lost", {
  df <- dplyr::tibble(name = "Cola nitida", backbone_taxon_name = "mine")
  parked <- .rename_conflicting_columns(df)

  expect_false("backbone_taxon_name" %in% names(parked$data))
  expect_true("backbone_taxon_name_input" %in% names(parked$data))
  expect_equal(parked$data$backbone_taxon_name_input, "mine")
})

# ---- The generated R code ---------------------------------------------------

test_that("the internal backbone needs no extra step in the script", {
  expect_null(.taxo_match_backbone_code("internal"))
  expect_null(.taxo_match_backbone_code(NULL))
})

test_that("the generated code names the backbone that was chosen", {
  code <- .taxo_match_backbone_code("apd")

  expect_match(code, 'get_backbone_names(', fixed = TRUE)
  expect_match(code, '"apd"', fixed = TRUE)
  expect_match(code, "backbone_taxon_name", fixed = TRUE)
  expect_false(grepl("get_wcvp_names", code, fixed = TRUE))
})

test_that("the generated code parses as R", {
  code <- .taxo_match_backbone_code("wcvp")
  expect_silent(parse(text = code))
})

test_that("the workflow script includes the backbone step only when there is one", {
  with_bb <- .taxo_match_combined_code(NULL, "match_code()",
                                       .taxo_match_backbone_code("apd"))
  without <- .taxo_match_combined_code(NULL, "match_code()", NULL)

  expect_match(with_bb, "Step 4", fixed = TRUE)
  expect_false(grepl("Step 4", without, fixed = TRUE))
})

# ---- Naming a backbone to the user ------------------------------------------

test_that("the internal backbone is named without asking the database", {
  expect_equal(.backbone_display_name("internal"), "internal")
})

test_that("an undescribable backbone is named by its own code", {
  # A connection object the query cannot use: the lookup fails, and the label
  # has to come from somewhere rather than blow up a notification.
  broken <- structure(list(), class = "not_a_connection")

  expect_equal(.backbone_display_name("apd", con_taxa = broken), "APD")
})

test_that("no backbone at all has no name", {
  expect_equal(.backbone_display_name(NULL), "")
  expect_equal(.backbone_display_name(NA_character_), "")
  expect_equal(.backbone_display_name(""), "")
})
