# Generic taxonomic backbone layer (R/backbone_core.R).
#
# Only the pure parts are tested here: validating the argument, shaping the
# rows fetched from a backbone view, and writing a backbone's names into a
# taxonomy table. The SQL is exercised against a database.


# ---------------------------------------------------------------------------
# .pg_array_literal()
# ---------------------------------------------------------------------------

test_that(".pg_array_literal() quotes elements and drops NA", {
  expect_equal(.pg_array_literal(c(1L, NA, 3L)), "{\"1\",\"3\"}")
  expect_equal(.pg_array_literal(character(0)), "{}")
  expect_equal(.pg_array_literal(c("a\"b", "c\\d")), "{\"a\\\"b\",\"c\\\\d\"}")
})


# ---------------------------------------------------------------------------
# .validate_backbone()
# ---------------------------------------------------------------------------

test_that(".validate_backbone() accepts 'internal' without a database", {
  local_mocked_bindings(list_backbones = function(...) stop("must not be called"))
  expect_equal(.validate_backbone("internal"), "internal")
})

test_that(".validate_backbone() rejects anything but a single code", {
  expect_error(.validate_backbone(c("internal", "wcvp")), "single backbone code")
  expect_error(.validate_backbone(NA_character_), "single backbone code")
  expect_error(.validate_backbone(1), "single backbone code")
  expect_error(.validate_backbone(""), "single backbone code")
})

test_that(".validate_backbone() accepts only backbones offered to users", {
  local_mocked_bindings(
    list_backbones = function(...) dplyr::tibble(code = "wcvp", is_name_source = TRUE)
  )
  expect_equal(.validate_backbone("wcvp"), "wcvp")
  expect_error(.validate_backbone("apd"), "unknown or not available")
})

test_that(".validate_backbone() says when a backbone is registered but not offered", {
  local_mocked_bindings(
    list_backbones = function(...) {
      dplyr::tibble(code = c("apd", "wcvp"), is_name_source = c(FALSE, TRUE))
    }
  )
  expect_equal(.validate_backbone("wcvp"), "wcvp")
  expect_error(.validate_backbone("apd"), "registered but not offered")
})


# ---------------------------------------------------------------------------
# .shape_backbone_names()
# ---------------------------------------------------------------------------

# One row per preferred link, as get_backbone_names() fetches them
.raw_backbone_rows <- function(...) {
  base <- list(
    idtax_n         = c(1L, 2L, 3L, 4L),
    external_id     = c("101", "102", "103", "999"),
    in_view         = c(TRUE, TRUE, TRUE, FALSE),
    raw_accepted_id = c(NA, "101", "900", NA),
    end_id          = c("101", "101", "103", NA),
    resolved        = c(TRUE, TRUE, FALSE, FALSE),
    depth           = c(0L, 1L, 0L, 0L),
    m_taxon_name = c("A a", "A b", "C c", NA),
    m_family     = c("Fam", "Fam", "Fam", NA),
    m_genus      = c("A", "A", "C", NA),
    m_species    = c("a", "b", "c", NA),
    m_authors    = c("L.", "Mill.", "DC.", NA),
    m_status     = c("accepted", "synonym", "synonym", NA),
    m_status_raw = c("Accepted", "Synonym", "Synonym", NA),
    x_taxon_name = c("A a", "A a", "C c", NA),
    x_family     = c("Fam", "Fam", "Fam", NA),
    x_genus      = c("A", "A", "C", NA),
    x_species    = c("a", "a", "c", NA),
    x_authors    = c("L.", "L.", "DC.", NA),
    x_status     = c("accepted", "accepted", "synonym", NA),
    x_status_raw = c("Accepted", "Accepted", "Synonym", NA)
  )
  overrides <- list(...)
  base[names(overrides)] <- overrides
  as.data.frame(base, stringsAsFactors = FALSE)
}

test_that(".shape_backbone_names() follows a completed chain", {
  res <- .shape_backbone_names(.raw_backbone_rows(), 1:4, "wcvp")

  # 2 is a synonym of 101: accepted name, accepted id, original link kept
  expect_equal(res$backbone_taxon_name[2], "A a")
  expect_equal(res$backbone_accepted_id[2], "101")
  expect_equal(res$backbone_name_id[2], "102")
  expect_equal(res$name_source[2], "wcvp")
})

test_that(".shape_backbone_names() gives an end point no accepted id", {
  res <- .shape_backbone_names(.raw_backbone_rows(), 1:4, "wcvp")
  expect_equal(res$backbone_taxon_name[1], "A a")
  expect_true(is.na(res$backbone_accepted_id[1]))
})

test_that(".shape_backbone_names() keeps the matched name when the chain fails", {
  res <- .shape_backbone_names(.raw_backbone_rows(), 1:4, "apd")
  expect_equal(res$backbone_taxon_name[3], "C c")
  expect_equal(res$backbone_status_raw[3], "Synonym")
  expect_true(is.na(res$backbone_accepted_id[3]))
  expect_equal(res$name_source[3], "apd")
})

test_that(".shape_backbone_names() treats an ID absent from the view as internal", {
  res <- .shape_backbone_names(.raw_backbone_rows(), 1:4, "wcvp")
  expect_equal(res$name_source[4], "internal")
  expect_true(is.na(res$backbone_taxon_name[4]))
})

test_that(".shape_backbone_names() returns every requested taxon, unlinked ones internal", {
  res <- .shape_backbone_names(.raw_backbone_rows(), c(1L, 2L, 3L, 4L, 5L), "wcvp")
  expect_equal(nrow(res), 5)
  expect_equal(res$name_source[5], "internal")
  expect_true(all(is.na(res[5, c("backbone_name_id", "backbone_taxon_name")])))
})

test_that(".shape_backbone_names() without resolution keeps names and raw pointers", {
  res <- .shape_backbone_names(.raw_backbone_rows(), 1:4, "wcvp", resolve_synonyms = FALSE)
  expect_equal(res$backbone_taxon_name[2], "A b")
  expect_equal(res$backbone_accepted_id[2], "101")
  expect_equal(res$backbone_accepted_id[3], "900")
})

test_that(".shape_backbone_names() with no rows returns internal for all", {
  res <- .shape_backbone_names(NULL, c(7L, 8L), "wcvp")
  expect_equal(res$idtax_n, c(7L, 8L))
  expect_equal(res$name_source, c("internal", "internal"))
  expect_true(all(is.na(res$backbone_name_id)))

  empty <- .shape_backbone_names(NULL, integer(0), "wcvp")
  expect_equal(nrow(empty), 0)
  expect_true("backbone_taxon_name" %in% names(empty))
})


# ---------------------------------------------------------------------------
# .apply_backbone()
# ---------------------------------------------------------------------------

.taxa_table <- function() {
  data.frame(
    idtax_n              = c(1L, 2L, 3L),
    tax_fam              = c("OldFam", "OldFam", "OtherFam"),
    tax_gen              = c("Old", "Old", "Other"),
    tax_esp              = c("one", "two", "three"),
    tax_sp_level         = c("Old one", "Old two", "Other three"),
    tax_infra_level      = c("Old one", "Old two", "Other three"),
    tax_infra_level_auth = c("Old one X", "Old two Y", "Other three Z"),
    stringsAsFactors     = FALSE
  )
}

.backbone_info_rows <- function() {
  dplyr::tibble(
    idtax_n              = c(1L, 2L, 3L),
    backbone_name_id     = c("101", "102", NA),
    backbone_accepted_id = c(NA, "101", NA),
    backbone_taxon_name  = c("New one", "New one", NA),
    backbone_family      = c("NewFam", "NewFam", NA),
    backbone_genus       = c("New", "New", NA),
    backbone_species     = c("one", "one", NA),
    backbone_authors     = c("L.", NA, NA),
    backbone_status      = c("accepted", "accepted", NA),
    backbone_status_raw  = c("Accepted", "Accepted", NA),
    name_source          = c("wcvp", "wcvp", "internal")
  )
}

test_that(".apply_backbone() replaces names of linked rows only", {
  res <- .apply_backbone(.taxa_table(), .backbone_info_rows(), "wcvp")

  expect_equal(res$tax_fam, c("NewFam", "NewFam", "OtherFam"))
  expect_equal(res$tax_sp_level, c("New one", "New one", "Other three"))
  expect_equal(res$tax_infra_level_auth, c("New one L.", "New one", "Other three Z"))
  expect_equal(res$name_source, c("wcvp", "wcvp", "internal"))
})

test_that(".apply_backbone() keeps the internal name and the row count", {
  input <- .taxa_table()
  res <- .apply_backbone(input, .backbone_info_rows(), "wcvp")

  expect_equal(nrow(res), nrow(input))
  expect_equal(res$alt_taxon_name, input$tax_infra_level)
  expect_false(any(c("backbone_taxon_name", "backbone_family") %in% names(res)))
})

test_that(".apply_backbone() adds the wcvp_* id columns for WCVP only", {
  wcvp <- .apply_backbone(.taxa_table(), .backbone_info_rows(), "wcvp")
  expect_equal(wcvp$wcvp_plant_name_id, c(101L, 102L, NA))
  expect_equal(wcvp$wcvp_accepted_plant_name_id, c(NA, 101L, NA))

  info_apd <- .backbone_info_rows()
  info_apd$name_source <- c("apd", "apd", "internal")
  apd <- .apply_backbone(.taxa_table(), info_apd, "apd")
  expect_false("wcvp_plant_name_id" %in% names(apd))
  expect_equal(apd$backbone_name_id, c("101", "102", NA))
  expect_equal(apd$tax_gen, c("New", "New", "Other"))
})

test_that(".apply_backbone() does not replace rows linked to another backbone", {
  # info produced for "apd" applied as "wcvp": nothing matches the code
  info <- .backbone_info_rows()
  info$name_source <- c("apd", "apd", "internal")
  res <- .apply_backbone(.taxa_table(), info, "wcvp")
  expect_equal(res$tax_gen, .taxa_table()$tax_gen)
})

test_that(".apply_backbone() can be applied twice without duplicating columns", {
  once <- .apply_backbone(.taxa_table(), .backbone_info_rows(), "wcvp")
  twice <- .apply_backbone(once, .backbone_info_rows(), "wcvp")
  expect_equal(names(twice), names(once))
  expect_equal(nrow(twice), 3)
})

test_that(".apply_backbone() tolerates missing name columns and another id column", {
  input <- data.frame(idtax_individual_f = c(1L, 3L), tax_gen = c("Old", "Other"),
                      stringsAsFactors = FALSE)
  res <- .apply_backbone(input, .backbone_info_rows(), "wcvp", id_col = "idtax_individual_f")
  expect_equal(res$tax_gen, c("New", "Other"))
  expect_false("alt_taxon_name" %in% names(res))
})


# ---- Citation ----------------------------------------------------------------

test_that(".backbone_access_date() states the month of the version, not today", {
  expect_equal(.backbone_access_date("2026-09-16", NULL), "September 2026")
  expect_equal(.backbone_access_date("2026-09-16", NULL, "fr"), "septembre 2026")
  # WCVP versions are not dates: the import date answers instead
  expect_equal(.backbone_access_date("v13", as.POSIXct("2026-01-08 10:00:00", tz = "UTC")),
               "January 2026")
  expect_true(is.na(.backbone_access_date(NA_character_, NULL)))
})

test_that(".backbone_cited_version() prefers the publisher's version to a file name", {
  expect_equal(.backbone_cited_version("4.0.0", "2026-09-16"), "4.0.0")
  expect_equal(.backbone_cited_version("APD export Gilles.txt", "2026-09-16"), "2026-09-16")
  expect_equal(.backbone_cited_version(NA_character_, "2026-09-16"), "2026-09-16")
  expect_true(is.na(.backbone_cited_version(NA_character_, NA_character_)))
})

test_that(".format_backbone_citation() writes the citation APD asks for", {
  apd <- .format_backbone_citation(
    "African Plant Database",
    "Conservatoire et Jardin botaniques de la Ville de Gen\u00e8ve and South African National Biodiversity Institute, Pretoria",
    "4.0.0", "September 2026", "http://africanplantdatabase.ch")
  expect_equal(
    apd,
    paste0("African Plant Database (version 4.0.0). Conservatoire et Jardin ",
           "botaniques de la Ville de Gen\u00e8ve and South African National ",
           "Biodiversity Institute, Pretoria, accessed September 2026, from ",
           "<http://africanplantdatabase.ch>.")
  )
  expect_match(
    .format_backbone_citation("African Plant Database", "CJB", "4.0.0",
                              "septembre 2026", "http://x.ch", language = "fr"),
    "acc\u00e8s septembre 2026, de <http://x[.]ch>[.]$"
  )
})

test_that(".format_backbone_citation() ends with a full stop whatever is missing", {
  expect_equal(.format_backbone_citation("A backbone", NA, NA, NA, NA), "A backbone.")
  expect_equal(.format_backbone_citation("A backbone", "Kew", NA, NA, NA),
               "A backbone. Kew.")
  expect_equal(.format_backbone_citation("A backbone", "Kew", "v13", "May 2026", NA),
               "A backbone (version v13). Kew, accessed May 2026.")
})

test_that(".backbone_citation_notice() speaks once per session and can be silenced", {
  rm(list = ls(envir = .backbone_cited), envir = .backbone_cited)
  withr::local_options(CafriplotsR.backbone_citation = FALSE)
  expect_null(.backbone_citation_notice("apd"))
  expect_equal(length(ls(envir = .backbone_cited)), 0L)

  expect_null(.backbone_citation_notice("internal"))
})


test_that(".render_citation_template() fills Kew's formula", {
  template <- paste("Govaerts R. (ed.) ({year}). WCVP: World Checklist of",
                    "Vascular Plants, version {version}. Facilitated by the",
                    "Royal Botanic Gardens, Kew. Published on the Internet;",
                    "{url} {retrieved} {date}.")
  out <- .render_citation_template(
    template,
    list(version = "13", year = "2026", date = "8 January 2026",
         url = "http://sftp.kew.org/pub/data-repositories/WCVP/")
  )
  expect_equal(
    out,
    paste("Govaerts R. (ed.) (2026). WCVP: World Checklist of Vascular Plants,",
          "version 13. Facilitated by the Royal Botanic Gardens, Kew. Published",
          "on the Internet; http://sftp.kew.org/pub/data-repositories/WCVP/",
          "Retrieved 8 January 2026.")
  )
})

test_that(".render_citation_template() translates its words and drops empty parts", {
  template <- "{name} (version {version}). {publisher}, {accessed} {access}, {from} <{url}>."
  values <- list(name = "African Plant Database", publisher = "CJB",
                 version = "4.0.0", access = "septembre 2026",
                 url = "http://africanplantdatabase.ch")
  expect_equal(
    .render_citation_template(template, values, "fr"),
    "African Plant Database (version 4.0.0). CJB, acc\u00e8s septembre 2026, de <http://africanplantdatabase.ch>."
  )
  # nothing to fill: no empty brackets, no dangling words, no stray commas
  bare <- .render_citation_template(
    template, list(name = "A backbone", publisher = NA, version = NA,
                   access = NA, url = NA))
  expect_equal(bare, "A backbone.")

  # a site but no version: "from" stays, "accessed" goes with its date
  partial <- .render_citation_template(
    template, list(name = "A backbone", publisher = "Kew", version = NA,
                   access = NA, url = "https://example.org"))
  expect_equal(partial, "A backbone. Kew, from <https://example.org>.")
})

test_that(".backbone_cited_version() drops the v of a version like v13", {
  expect_equal(.backbone_cited_version(NA_character_, "v13"), "13")
  expect_equal(.backbone_cited_version("4.0.0", "2026-09-16"), "4.0.0")
  expect_equal(.backbone_cited_version(NA_character_, "version 13"), "version 13")
})

test_that(".backbone_access_full_date() gives the day Kew's formula needs", {
  expect_equal(.backbone_access_full_date("2026-09-16", NULL), "16 September 2026")
  expect_equal(.backbone_access_full_date("v13", as.POSIXct("2026-01-08", tz = "UTC"), "fr"),
               "8 janvier 2026")
})
