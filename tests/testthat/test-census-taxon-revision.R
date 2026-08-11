# Tests for R/census_taxon_revision.R
#
# A census import overwrites nothing about an individual — except here. The
# decision of whether to accept a revised identification turns on the herbarium
# evidence behind the one being replaced, so the classifier is kept pure and
# tested without a connection.

ctr_taxa <- function() {
  data.frame(
    idtax_n    = c(351190L, 500L, 600L, 700L),
    taxon_name = c("unidentified", "Baphia sp1", "Memecylon sp1", "Fabaceae"),
    tax_level  = c("higher", NA, NA, "family"),
    precision  = c(1L, 6L, 6L, 4L),
    stringsAsFactors = FALSE
  )
}

ctr_drift <- function(...) {
  base <- data.frame(
    row_id     = 1L,
    plot_name  = "P1",
    tag        = "101",
    idtax_file = "600",
    idtax_db   = "500",
    stringsAsFactors = FALSE
  )
  over <- list(...)
  for (nm in names(over)) base[[nm]] <- over[[nm]]
  base
}

ctr_data <- function(herbarium = NA_character_, id_n = 11L) {
  data.frame(
    row_id             = 1L,
    id_n               = id_n,
    herbarium_nbe_char = herbarium,
    stringsAsFactors   = FALSE
  )
}

# =============================================================================
# .taxon_precision()
# =============================================================================

test_that(".taxon_precision orders the ladder", {
  p <- .taxon_precision(c("higher", "family", "genus", "species", "infraspecific"))
  expect_true(all(diff(p) > 0))
})

test_that(".taxon_precision is case and whitespace tolerant", {
  expect_equal(.taxon_precision(" Species "), .taxon_precision("species"))
})

test_that(".taxon_precision ranks a morphospecies with species", {
  # Baphia sp1 carries no tax_level but does carry a genus and an epithet
  expect_equal(.taxon_precision(NA, morpho = TRUE), .taxon_precision("species"))
  expect_true(is.na(.taxon_precision(NA, morpho = FALSE)))
})

test_that(".taxon_precision returns NA for an unknown level", {
  expect_true(is.na(.taxon_precision("kingdom")))
})

# =============================================================================
# .assemble_taxon_name()
# =============================================================================

test_that(".assemble_taxon_name builds a binomial", {
  x <- data.frame(tax_fam = "Fabaceae", tax_gen = "Baphia", tax_esp = "sp1",
                  tax_rank01 = NA, tax_nam01 = NA, stringsAsFactors = FALSE)
  expect_equal(.assemble_taxon_name(x), "Baphia sp1")
})

test_that(".assemble_taxon_name includes an infraspecific rank", {
  x <- data.frame(tax_fam = "F", tax_gen = "Genus", tax_esp = "sp",
                  tax_rank01 = "var.", tax_nam01 = "minor",
                  stringsAsFactors = FALSE)
  expect_equal(.assemble_taxon_name(x), "Genus sp var. minor")
})

test_that(".assemble_taxon_name falls back to family then to unidentified", {
  x <- data.frame(tax_fam = c("Olacaceae", NA), tax_gen = c(NA, NA),
                  tax_esp = c(NA, NA), tax_rank01 = c(NA, NA),
                  tax_nam01 = c(NA, NA), stringsAsFactors = FALSE)
  expect_equal(.assemble_taxon_name(x), c("Olacaceae", "unidentified"))
})

# =============================================================================
# .classify_taxon_revisions() — evidence
# =============================================================================

test_that("no drift gives an empty frame with the full shape", {
  res <- .classify_taxon_revisions(NULL)
  expect_equal(nrow(res), 0)
  expect_true(all(c("evidence", "category", "decision", "n_voucher") %in% names(res)))
})

test_that("a specimen of this tree defaults to keeping the database value", {
  evidence <- data.frame(id_n = 11L, n_voucher = 1L, n_reference = 0L)
  res <- .classify_taxon_revisions(ctr_drift(), ctr_data(), evidence, ctr_taxa())

  expect_equal(res$evidence, "voucher")
  expect_equal(res$decision, "keep_db")
})

test_that("a reference specimen does not block the revision", {
  evidence <- data.frame(id_n = 11L, n_voucher = 0L, n_reference = 3L)
  res <- .classify_taxon_revisions(ctr_drift(), ctr_data(), evidence, ctr_taxa())

  expect_equal(res$evidence, "reference")
  expect_equal(res$decision, "accept_file")
})

test_that("a herbarium number in the file with no link is its own state", {
  # The specimen is in a press, not in the database — detectable only here
  res <- .classify_taxon_revisions(
    ctr_drift(), ctr_data(herbarium = "PIRD 3080"), NULL, ctr_taxa()
  )

  expect_equal(res$evidence, "collected_this_census")
  expect_equal(res$decision, "accept_file")
  expect_equal(res$herbarium_nbe_char, "PIRD 3080")
})

test_that("a formal voucher outranks a field number on the same row", {
  evidence <- data.frame(id_n = 11L, n_voucher = 2L, n_reference = 0L)
  res <- .classify_taxon_revisions(
    ctr_drift(), ctr_data(herbarium = "PIRD 3080"), evidence, ctr_taxa()
  )
  expect_equal(res$evidence, "voucher")
})

test_that("no evidence at all is field_only and accepted", {
  res <- .classify_taxon_revisions(ctr_drift(), ctr_data(), NULL, ctr_taxa())

  expect_equal(res$evidence, "field_only")
  expect_equal(res$decision, "accept_file")
  expect_equal(res$n_voucher, 0L)
})

# =============================================================================
# .classify_taxon_revisions() — categories that override the evidence
# =============================================================================

test_that("gaining an identification is accepted even against a voucher", {
  evidence <- data.frame(id_n = 11L, n_voucher = 1L, n_reference = 0L)
  res <- .classify_taxon_revisions(
    ctr_drift(idtax_db = "351190", idtax_file = "600"),
    ctr_data(), evidence, ctr_taxa()
  )

  expect_equal(res$category, "identification_gained")
  expect_equal(res$decision, "accept_file")
})

test_that("losing precision defaults to keeping the database value", {
  # Fabaceae (family) proposed where Baphia sp1 (species) is recorded
  res <- .classify_taxon_revisions(
    ctr_drift(idtax_db = "500", idtax_file = "700"),
    ctr_data(), NULL, ctr_taxa()
  )

  expect_equal(res$category, "precision_lost")
  expect_equal(res$decision, "keep_db")
})

test_that("gaining beats losing when the database value was unidentified", {
  # 351190 ranks as `higher`, so anything is more precise — never a loss
  res <- .classify_taxon_revisions(
    ctr_drift(idtax_db = "351190", idtax_file = "700"),
    ctr_data(), NULL, ctr_taxa()
  )
  expect_equal(res$category, "identification_gained")
})

test_that("an ordinary revision between two species is just a revision", {
  res <- .classify_taxon_revisions(ctr_drift(), ctr_data(), NULL, ctr_taxa())
  expect_equal(res$category, "revision")
})

# =============================================================================
# .classify_taxon_revisions() — degradation
# =============================================================================

test_that("names fall back to the id when the taxa lookup failed", {
  res <- .classify_taxon_revisions(ctr_drift(), ctr_data(), NULL, NULL)

  expect_equal(res$name_db, "idtax 500")
  expect_equal(res$name_file, "idtax 600")
  # with no precision known, nothing can be called a regression
  expect_equal(res$category, "revision")
})

test_that("a missing id_n is carried through rather than dropped", {
  res <- .classify_taxon_revisions(ctr_drift(), NULL, NULL, ctr_taxa())
  expect_true(is.na(res$id_n))
})

test_that("several revisions are classified independently", {
  drift <- data.frame(
    row_id     = 1:3,
    plot_name  = rep("P1", 3),
    tag        = c("1", "2", "3"),
    idtax_db   = c("500", "351190", "500"),
    idtax_file = c("600", "600", "700"),
    stringsAsFactors = FALSE
  )
  data <- data.frame(row_id = 1:3, id_n = c(11L, 12L, 13L),
                     herbarium_nbe_char = NA_character_)
  evidence <- data.frame(id_n = 11L, n_voucher = 1L, n_reference = 0L)

  res <- .classify_taxon_revisions(drift, data, evidence, ctr_taxa())

  expect_equal(res$evidence, c("voucher", "field_only", "field_only"))
  expect_equal(res$category, c("revision", "identification_gained", "precision_lost"))
  expect_equal(res$decision, c("keep_db", "accept_file", "keep_db"))
})

# =============================================================================
# .census_revision_display()
# =============================================================================

test_that("the display table flags a precision loss in the proposed name", {
  res <- .classify_taxon_revisions(
    ctr_drift(idtax_db = "500", idtax_file = "700"), ctr_data(), NULL, ctr_taxa()
  )
  disp <- .census_revision_display(res)

  expect_match(disp$proposed, "less precise")
  expect_equal(disp$decision, "keep database")
})

test_that("the display table is empty-safe", {
  expect_equal(nrow(.census_revision_display(.classify_taxon_revisions(NULL))), 0)
})

test_that("the display table honours an overriding decision vector", {
  res <- .classify_taxon_revisions(ctr_drift(), ctr_data(), NULL, ctr_taxa())
  disp <- .census_revision_display(res, decisions = "keep_db")
  expect_equal(disp$decision, "keep database")
})

# =============================================================================
# .apply_taxon_revisions() — against a real SQL engine
# =============================================================================

make_revision_db <- function(with_audit_cols = TRUE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(con, "data_individuals", data.frame(
    id_n = c(11L, 12L), idtax_n = c(500L, 500L),
    tag = c("101", "102"), sous_plot_name = NA_character_,
    id_table_liste_plots_n = c(1L, 1L),
    herbarium_nbe_char = NA_character_, multi_tiges_id = NA_character_,
    stringsAsFactors = FALSE
  ))

  cols <- c("id_n INTEGER", "tag TEXT", "sous_plot_name TEXT",
            "id_table_liste_plots_n INTEGER", "herbarium_nbe_char TEXT",
            "multi_tiges_id TEXT", "modif_type TEXT", "date_modified TEXT",
            "data_modif_y INTEGER", "data_modif_m INTEGER", "data_modif_d INTEGER")
  if (with_audit_cols) {
    cols <- c(cols, "idtax_n INTEGER", "idtax_n_new INTEGER")
  }
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE followup_updates_individuals (%s)", paste(cols, collapse = ", ")))
  con
}

test_that(".apply_taxon_revisions updates the taxon and logs where it moved", {
  skip_if_not_installed("RSQLite")

  con <- make_revision_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rev <- data.frame(id_n = 11L, idtax_file = 600L, stringsAsFactors = FALSE)
  expect_equal(.apply_taxon_revisions(rev, con), 1L)

  after <- DBI::dbGetQuery(con, "SELECT id_n, idtax_n FROM data_individuals ORDER BY id_n")
  expect_equal(after$idtax_n, c(600L, 500L))

  audit <- DBI::dbGetQuery(con, "SELECT * FROM followup_updates_individuals")
  expect_equal(nrow(audit), 1)
  expect_equal(audit$idtax_n, 500L)      # where it came from
  expect_equal(audit$idtax_n_new, 600L)  # where it went
  expect_equal(audit$modif_type, "idtax_n")
  expect_equal(audit$id_n, 11L)
})

test_that(".apply_taxon_revisions refuses without the audit columns", {
  skip_if_not_installed("RSQLite")

  con <- make_revision_db(with_audit_cols = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rev <- data.frame(id_n = 11L, idtax_file = 600L)
  expect_error(.apply_taxon_revisions(rev, con), "migrate_followup_idtax")

  # and nothing was written
  expect_equal(DBI::dbGetQuery(con, "SELECT idtax_n FROM data_individuals WHERE id_n=11")$idtax_n,
               500L)
})

test_that(".apply_taxon_revisions skips a stem already carrying that taxon", {
  skip_if_not_installed("RSQLite")

  con <- make_revision_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rev <- data.frame(id_n = 11L, idtax_file = 500L)
  expect_equal(.apply_taxon_revisions(rev, con), 0L)
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM followup_updates_individuals")), 0)
})

test_that(".apply_taxon_revisions ignores rows with no individual", {
  skip_if_not_installed("RSQLite")

  con <- make_revision_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rev <- data.frame(id_n = c(NA_integer_, 999L), idtax_file = c(600L, 600L))
  expect_equal(.apply_taxon_revisions(rev, con), 0L)
})

test_that(".apply_taxon_revisions handles nothing to do", {
  expect_equal(.apply_taxon_revisions(NULL, NULL), 0L)
  expect_equal(.apply_taxon_revisions(data.frame(), NULL), 0L)
})
