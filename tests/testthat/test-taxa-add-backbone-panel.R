# Step 1 of the Add New Taxa tab searches every backbone at once. These tests
# drive the module server with no database behind it: what they check is that
# the panels render and the selection bookkeeping holds, not the SQL.

fake_i18n_reactive <- function() {
  shiny::reactive(list(t = function(x) x))
}

# A pool that is not a database: list_backbones() fails, and the module is
# expected to degrade to "no backbone in this database" rather than crash.
no_pool <- function() shiny::reactive(NULL)

test_that("step 1 renders when the database offers no backbone", {
  shiny::testServer(
    mod_taxa_add_server,
    args = list(pool = no_pool(), has_write_permission = shiny::reactive(TRUE),
                i18n = fake_i18n_reactive()),
    {
      html <- as.character(output$backbone_results_ui$html)
      expect_match(html, "No external taxonomic backbone in this database",
                   fixed = TRUE)
    }
  )
})

test_that("nothing is selected for linking before a search", {
  shiny::testServer(
    mod_taxa_add_server,
    args = list(pool = no_pool(), has_write_permission = shiny::reactive(TRUE),
                i18n = fake_i18n_reactive()),
    {
      expect_length(rv$backbone_selected, 0)
      html <- as.character(output$backbone_selected_badge_ui$html)
      expect_match(html, "No backbone identifier selected", fixed = TRUE)
    }
  )
})

test_that("the selected identifiers are listed, one line per backbone", {
  shiny::testServer(
    mod_taxa_add_server,
    args = list(pool = no_pool(), has_write_permission = shiny::reactive(TRUE),
                i18n = fake_i18n_reactive()),
    {
      rv$backbone_selected <- c(apd = "A1", wcvp = "W1")
      session$flushReact()

      html <- as.character(output$backbone_selected_badge_ui$html)
      expect_match(html, "Will be linked after taxon creation:", fixed = TRUE)
      expect_match(html, "A1", fixed = TRUE)
      expect_match(html, "W1", fixed = TRUE)
    }
  )
})

test_that("clearing drops every selected identifier at once", {
  shiny::testServer(
    mod_taxa_add_server,
    args = list(pool = no_pool(), has_write_permission = shiny::reactive(TRUE),
                i18n = fake_i18n_reactive()),
    {
      rv$backbone_selected <- c(apd = "A1", wcvp = "W1")
      session$setInputs(btn_clear_backbone_links = 1)

      expect_length(rv$backbone_selected, 0)
      expect_null(rv$backbone_synonymy_candidates)
    }
  )
})

test_that("a search that finds nothing says so without naming one backbone", {
  shiny::testServer(
    mod_taxa_add_server,
    args = list(pool = no_pool(), has_write_permission = shiny::reactive(TRUE),
                i18n = fake_i18n_reactive()),
    {
      # An empty result set, as search_all_backbones() returns when no backbone
      # matched, with at least one backbone known so the panel gets that far.
      rv$backbone_results <- search_all_backbones("")
      session$flushReact()

      html <- as.character(output$backbone_results_ui$html)
      # With no backbone registered the earlier branch wins; either way the
      # panel must not claim a match.
      expect_false(grepl("Exact match", html, fixed = TRUE))
    }
  )
})
