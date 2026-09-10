# The backbone app's update form could edit every taxonomic rank but not the
# authorship columns, and `update_dico_name()` had no parameter for them either,
# so an author recorded wrongly (or, as in the Wilczekra case, not recorded at
# all) could only be repaired with hand-written SQL. These tests cover the two
# halves of the fix: the form now tracks author1/2/3, and the tracked changes
# are translated into the arguments that write them.

.update_i18n <- function() {
  shiny::reactive(list(t = function(x) x))
}

# One row shaped like query_taxa() output - which is what the search module
# hands over, so the column names here are the ones the form must read.
.update_taxon_fixture <- function(...) {
  taxon <- data.frame(
    idtax_n      = 42L,
    idtax_good_n = NA_integer_,
    tax_famclass = "Magnoliopsida",
    tax_order    = "Celastrales",
    tax_fam      = "Celastraceae",
    tax_gen      = "Wilczekra",
    tax_esp      = "congolensis",
    tax_rank01   = NA_character_,
    tax_nam01    = NA_character_,
    author1      = NA_character_,
    author2      = NA_character_,
    author3      = NA_character_,
    morpho_species = FALSE,
    stringsAsFactors = FALSE
  )
  overrides <- list(...)
  for (nm in names(overrides)) taxon[[nm]] <- overrides[[nm]]
  taxon
}

test_that("an author typed into the form is tracked as a modified field", {
  taxon <- .update_taxon_fixture()

  shiny::testServer(
    mod_taxa_update_server,
    args = list(
      pool = shiny::reactive(NULL),
      selected_taxon = shiny::reactive(taxon),
      has_write_permission = shiny::reactive(TRUE),
      i18n = .update_i18n()
    ),
    {
      session$setInputs(btn_start_edit = 1)
      session$setInputs(new_author1 = "(R. Wilczek) M.P. Simmons")

      expect_equal(rv$modified_fields$author1$new, "(R. Wilczek) M.P. Simmons")
      expect_true(is.na(rv$modified_fields$author1$old))
    }
  )
})


test_that("all three author columns are editable", {
  taxon <- .update_taxon_fixture(
    tax_rank01 = "var.", tax_nam01 = "oblongifolia",
    author1 = "Benth.", author2 = "Hook.f.", author3 = NA_character_
  )

  shiny::testServer(
    mod_taxa_update_server,
    args = list(
      pool = shiny::reactive(NULL),
      selected_taxon = shiny::reactive(taxon),
      has_write_permission = shiny::reactive(TRUE),
      i18n = .update_i18n()
    ),
    {
      session$setInputs(btn_start_edit = 1)
      session$setInputs(
        new_author1 = "Benth.",       # unchanged
        new_author2 = "P.Beauv.",     # changed
        new_author3 = "Aubrev."       # added
      )

      expect_null(rv$modified_fields$author1)
      expect_equal(rv$modified_fields$author2$new, "P.Beauv.")
      expect_equal(rv$modified_fields$author3$new, "Aubrev.")
    }
  )
})


test_that("clearing an author is a change, not a no-op", {
  taxon <- .update_taxon_fixture(author1 = "Benth.")

  shiny::testServer(
    mod_taxa_update_server,
    args = list(
      pool = shiny::reactive(NULL),
      selected_taxon = shiny::reactive(taxon),
      has_write_permission = shiny::reactive(TRUE),
      i18n = .update_i18n()
    ),
    {
      session$setInputs(btn_start_edit = 1)
      session$setInputs(new_author1 = "")

      expect_equal(rv$modified_fields$author1$new, "")
    }
  )
})


test_that("the infraspecific fields read the columns query_taxa returns", {
  # tax_rank01 / tax_nam01, not tax_rank1 / tax_name1: reading the wrong name
  # gave NULL, so an existing epithet looked like an addition and clearing one
  # registered no change at all.
  taxon <- .update_taxon_fixture(tax_rank01 = "var.", tax_nam01 = "oblongifolia")

  shiny::testServer(
    mod_taxa_update_server,
    args = list(
      pool = shiny::reactive(NULL),
      selected_taxon = shiny::reactive(taxon),
      has_write_permission = shiny::reactive(TRUE),
      i18n = .update_i18n()
    ),
    {
      session$setInputs(btn_start_edit = 1)

      # Retyping the current values is not a change
      session$setInputs(new_tax_rank1 = "var.", new_tax_name1 = "oblongifolia")
      expect_null(rv$modified_fields$tax_rank01)
      expect_null(rv$modified_fields$tax_nam01)

      # Clearing them is
      session$setInputs(new_tax_name1 = "")
      expect_equal(rv$modified_fields$tax_nam01$old, "oblongifolia")
      expect_equal(rv$modified_fields$tax_nam01$new, "")
    }
  )
})


test_that(".taxa_update_dico_params() names the arguments update_dico_name takes", {
  params <- .taxa_update_dico_params(list(
    author1    = list(old = NA, new = "Benth."),
    tax_esp    = list(old = "kola", new = "mangostana"),
    tax_nam01  = list(old = NA, new = "oblongifolia")
  ))

  expect_setequal(names(params), c("new_author1", "new_tax_esp", "new_tax_name1"))
  expect_equal(params$new_author1, "Benth.")
  expect_equal(params$new_tax_name1, "oblongifolia")
})


test_that(".taxa_update_dico_params() turns an emptied field into NA", {
  # NA is what reaches the database as NULL. Genus and family are exempt: the
  # database will not take them empty, so a blank is passed through untouched
  # and rejected there rather than silently blanking the record.
  params <- .taxa_update_dico_params(list(
    author2 = list(old = "Hook.f.", new = ""),
    tax_gen = list(old = "Garcinia", new = "")
  ))

  expect_true(is.na(params$new_author2))
  # Typed, not a bare logical NA: it is bound to a text column.
  expect_type(params$new_author2, "character")
  expect_equal(params$new_tax_gen, "")
})


test_that(".taxa_update_dico_params() drops what update_dico_name cannot write", {
  # morpho_species has no parameter; the module applies it with its own SQL.
  params <- .taxa_update_dico_params(list(
    morpho_species = list(old = FALSE, new = TRUE)
  ))

  expect_length(params, 0)
})


test_that("update_dico_name() accepts the three author arguments", {
  args <- names(formals(update_dico_name))

  expect_true(all(c("new_author1", "new_author2", "new_author3") %in% args))
})
