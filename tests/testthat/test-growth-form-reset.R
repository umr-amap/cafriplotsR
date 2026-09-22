# The growth form selector deliberately keeps its choices when its UI is
# destroyed, so that stepping back and forth in the Add New Taxa wizard does
# not lose them. That same persistence made the choices outlive the taxon:
# adding a taxon and then another saved the first one's growth forms against
# the second. These tests pin the reset that ends a taxon, and the removal of
# a single growth form from the list.

gf_args <- function() {
  list(pool = shiny::reactive(NULL),
       i18n = shiny::reactive(list(t = function(x) x)))
}

gf_path <- function(value) {
  list(list(id_trait = 1L, trait = "growth_form_level_1", value = value))
}

test_that("the module offers a reset to whoever drives it", {
  shiny::testServer(mod_growth_form_selector_server, args = gf_args(), {
    expect_true(is.function(session$returned$reset))
  })
})

test_that("reset forgets the growth forms of the taxon just added", {
  shiny::testServer(mod_growth_form_selector_server, args = gf_args(), {
    rv$all_paths <- list(gf_path("tree"), gf_path("liana"))
    rv$saved_basisofrecord <- "expertKnowledge"
    rv$saved_measurementremarks <- "from the field notebook"

    expect_length(session$returned$growth_form_selections(), 2)

    session$returned$reset()

    expect_length(session$returned$growth_form_selections(), 0)
    expect_equal(session$returned$basisofrecord(), "")
    expect_equal(session$returned$measurementremarks(), "")
    expect_false(session$returned$is_valid())
  })
})

test_that("reset survives being called on an untouched selector", {
  shiny::testServer(mod_growth_form_selector_server, args = gf_args(), {
    session$returned$reset()
    expect_length(session$returned$growth_form_selections(), 0)
  })
})

test_that("blanking the inputs does not resurrect the saved basis of record", {
  # The observer that saves the basis of record ignores empty values, so that
  # a destroyed UI cannot wipe it. reset() has to clear the stored value
  # itself, and the "" it writes to the input must not be saved back.
  shiny::testServer(mod_growth_form_selector_server, args = gf_args(), {
    rv$saved_basisofrecord <- "LivingSpecimen"
    session$returned$reset()
    session$setInputs(basisofrecord = "")
    session$flushReact()

    expect_equal(session$returned$basisofrecord(), "")
  })
})

test_that("removing one growth form removes exactly one", {
  # Each position gets a remover observer, and the list is rebuilt whenever it
  # changes. Registering a second observer for a position it already has makes
  # one click delete several growth forms.
  shiny::testServer(mod_growth_form_selector_server, args = gf_args(), {
    rv$all_paths <- list(gf_path("tree"), gf_path("liana"), gf_path("herb"))
    session$flushReact()

    # The list changing is what used to register the duplicates
    rv$all_paths <- c(rv$all_paths, list(gf_path("epiphyte")))
    session$flushReact()

    session$setInputs(remove_path_2 = 1)

    expect_length(rv$all_paths, 3)
    values <- vapply(rv$all_paths, function(p) p[[1]]$value, character(1))
    expect_equal(values, c("tree", "herb", "epiphyte"))
  })
})

test_that("a stale removal click after a reset does nothing", {
  shiny::testServer(mod_growth_form_selector_server, args = gf_args(), {
    rv$all_paths <- list(gf_path("tree"), gf_path("liana"))
    session$flushReact()

    session$returned$reset()
    session$setInputs(remove_path_2 = 1)

    expect_length(rv$all_paths, 0)
  })
})
