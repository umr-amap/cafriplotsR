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
