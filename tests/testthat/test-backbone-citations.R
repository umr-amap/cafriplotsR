# What a user has to cite when names come from APD, WCVP or any other
# registered backbone: the table the app, the export and query_citations()
# all read from, and the rules that put it in front of the right people.

fake_refs <- function() {
  dplyr::tibble(
    code        = c("apd", "wcvp"),
    name        = c("African Plant Database", "World Checklist of Vascular Plants"),
    publisher   = c("CJB", "Royal Botanic Gardens, Kew"),
    homepage    = c("http://africanplantdatabase.ch",
                    "http://sftp.kew.org/pub/data-repositories/WCVP/"),
    version     = c("4.0.0", "13"),
    access_date = as.Date(c("2026-09-01", "2026-01-08")),
    citation    = c("African Plant Database (version 4.0.0). CJB, accessed September 2026.",
                    "Govaerts R. (ed.) (2026). WCVP, version 13.")
  )
}


# ---- backbone_reference() --------------------------------------------------

test_that("backbone_reference() has nothing to say about the internal backbone", {
  ref <- backbone_reference("internal", con_taxa = structure(list(), class = "fake"))
  expect_s3_class(ref, "tbl_df")
  expect_equal(nrow(ref), 0)
  expect_named(ref, c("code", "name", "publisher", "homepage", "version",
                      "access_date", "citation"))
})

test_that("backbone_reference() returns empty rather than failing when unreadable", {
  testthat::local_mocked_bindings(
    .backbone_citation_meta = function(...) data.frame()
  )
  ref <- backbone_reference(con_taxa = structure(list(), class = "fake"))
  expect_equal(nrow(ref), 0)
})

test_that("backbone_reference() builds one row per backbone and filters by code", {
  meta <- data.frame(
    code = c("apd", "wcvp"),
    name = c("African Plant Database", "World Checklist of Vascular Plants"),
    publisher = c("CJB", "Royal Botanic Gardens, Kew"),
    homepage = c("http://africanplantdatabase.ch", "http://kew.org"),
    citation_template = c(NA_character_, NA_character_),
    version = c("4.0.0", "13"),
    import_date = as.Date(c("2026-09-01", "2026-01-08")),
    source_version = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    .backbone_citation_meta = function(...) meta
  )

  all_refs <- backbone_reference(con_taxa = structure(list(), class = "fake"))
  expect_equal(nrow(all_refs), 2)
  expect_equal(all_refs$code, c("apd", "wcvp"))
  expect_equal(all_refs$version, c("4.0.0", "13"))
  expect_s3_class(all_refs$access_date, "Date")
  # every row gets a sentence, one per row and not the first one recycled
  expect_true(all(nzchar(all_refs$citation)))
  expect_false(all_refs$citation[1] == all_refs$citation[2])

  one <- backbone_reference("wcvp", con_taxa = structure(list(), class = "fake"))
  expect_equal(nrow(one), 1)
  expect_equal(one$code, "wcvp")

  # a code the database does not know is empty, not an error
  expect_equal(
    nrow(backbone_reference("nope", con_taxa = structure(list(), class = "fake"))),
    0
  )
})


# ---- the once-per-session notice -------------------------------------------

test_that(".backbone_notice_key() keys on the backbone alone outside Shiny", {
  expect_equal(.backbone_notice_key("apd"), "apd")
})

test_that(".backbone_notice_key() keys on the session so one visitor cannot silence another", {
  # A hosted app serves every visitor from one R process, so a key on the
  # backbone alone would let the first visitor's notice suppress everyone's.
  session_a <- list(token = "aaa", onSessionEnded = function(f) invisible(NULL))
  session_b <- list(token = "bbb", onSessionEnded = function(f) invisible(NULL))

  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() session_a, .package = "shiny"
  )
  key_a <- .backbone_notice_key("wcvp")

  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() session_b, .package = "shiny"
  )
  key_b <- .backbone_notice_key("wcvp")

  expect_false(identical(key_a, key_b))
  expect_true(startsWith(key_a, "aaa|"))
  expect_true(startsWith(key_b, "bbb|"))
})


# ---- the sheet written beside exported names -------------------------------

test_that(".export_citation_sheet() carries the parts a reader needs", {
  sheet <- .export_citation_sheet(fake_refs()[2, ])

  expect_s3_class(sheet, "data.frame")
  expect_equal(nrow(sheet), 1)
  expect_named(sheet, c("reference", "code", "publisher", "version",
                        "accessed", "url", "citation"))
  expect_equal(sheet$code, "wcvp")
  expect_equal(sheet$accessed, "2026-01-08")
  # headers stay English whatever the interface language, so a collaborator
  # opening the file finds the same columns
  expect_equal(sheet$citation, "Govaerts R. (ed.) (2026). WCVP, version 13.")
})


# ---- the export module -----------------------------------------------------

test_that("the export module asks the database nothing when names are internal", {
  # The internal backbone is the database's own, so there is no second
  # reference to cite and no reason to open a taxa connection for one.
  testthat::local_mocked_bindings(
    call.mydb.taxa = function(...) stop("must not be called")
  )

  shiny::testServer(
    mod_results_export_server,
    args = list(
      results = shiny::reactive(list(data = data.frame(id_data = 1L))),
      original_data = shiny::reactive(data.frame(id_data = 1L)),
      i18n = shiny::reactive(shiny.i18n::Translator$new(
        translation_json_path = system.file("translations", "translation.json",
                                            package = "CafriplotsR")
      )),
      name_backbone = shiny::reactive("internal"),
      language = shiny::reactive("en")
    ),
    {
      expect_null(backbone_ref())
    }
  )
})


# ---- query_citations() -----------------------------------------------------

test_that(".append_backbone_citations() skips the backbones without a taxa connection", {
  base <- data.frame(id_citation = 1L, citation_key = "TRY_v6",
                     authors = "Kattge et al.", year = 2020L,
                     title = "TRY", url = NA_character_,
                     dataset_name = "TRY", notes = NA_character_,
                     source = "table_citations", stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(.taxa_connection_if_open = function() NULL)

  expect_message(
    out <- .append_backbone_citations(base),
    "no taxa connection"
  )
  expect_equal(out, base)
})

test_that(".append_backbone_citations() appends backbones with no id_citation", {
  base <- data.frame(id_citation = 1L, citation_key = "TRY_v6",
                     authors = "Kattge et al.", year = 2020L,
                     title = "TRY", url = NA_character_,
                     dataset_name = "TRY", notes = NA_character_,
                     source = "table_citations", stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    backbone_reference = function(...) fake_refs()
  )

  out <- .append_backbone_citations(base, con_taxa = structure(list(), class = "fake"))

  expect_equal(nrow(out), 3)
  expect_equal(out$source, c("table_citations", "backbone", "backbone"))
  # nothing points at a backbone with a foreign key, so it must not carry one
  expect_true(all(is.na(out$id_citation[out$source == "backbone"])))
  expect_equal(out$citation_key[out$source == "backbone"], c("APD", "WCVP"))
  # the sentence to paste is the one the publisher asks for
  expect_match(out$notes[out$citation_key == "WCVP"], "Govaerts")
  expect_equal(out$year[out$citation_key == "WCVP"], 2026L)
})

test_that(".append_backbone_citations() applies the same filters as the SQL", {
  base <- data.frame(id_citation = integer(), citation_key = character(),
                     authors = character(), year = integer(),
                     title = character(), url = character(),
                     dataset_name = character(), notes = character(),
                     source = character(), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    backbone_reference = function(...) fake_refs()
  )
  fake_con <- structure(list(), class = "fake")

  by_key <- .append_backbone_citations(base, con_taxa = fake_con, keys = "WCVP")
  expect_equal(by_key$citation_key, "WCVP")

  # ILIKE '%...%' is a literal substring, case-insensitive
  by_pattern <- .append_backbone_citations(base, con_taxa = fake_con,
                                           pattern = "kew")
  expect_equal(by_pattern$citation_key, "WCVP")

  # a pattern with regex metacharacters matches literally, as ILIKE would
  expect_equal(
    nrow(.append_backbone_citations(base, con_taxa = fake_con, pattern = "K.w")),
    0
  )

  expect_equal(
    nrow(.append_backbone_citations(base, con_taxa = fake_con, pattern = "TRY")),
    0
  )
})
