# Tests for R/mod_growth_form_selector.R
#
# With the growth form traits collapsed to three flat levels, the selector used
# to offer level 1 alone: it looked for the parent in each trait's description,
# where it no longer is. traitlist is served from the session cache, so no
# database is needed.

local_growth_form_traitlist <- function(env = parent.frame()) {
  old_cache <- .db_env$traitlist_cache
  old_pool <- .db_env$pool_main

  .db_env$traitlist_cache <- data.frame(
    id_trait = c(41L, 120L, 121L, 5L),
    trait = c("growth_form_level_1", "growth_form_level_2",
              "growth_form_level_3", "wood_density"),
    traitdescription = c("First hierarchical level of growth form.",
                         "Second hierarchical level of growth form.",
                         "Third hierarchical level of growth form.",
                         "Wood density"),
    factorlevels = c(
      "terrestrial_self_supporting, not_self_supporting",
      paste("herbaceous, semi_woody, woody, epiphyte, lithophyte, climber,",
            "hydrophyte, saprophyte_parasite"),
      paste("rosette, elongated_leaf_bearing_rhizomatous, cushion,",
            "extensive_stemmmed, tussock, palmoid, bambusoid, succulent_stem,",
            "prostrate_shrub, dwarf_shrub, shrub, tree, dwarf_tree,",
            "herbaceous_vine, woody_vine_liana, scrambler, strangler"),
      NA),
    stringsAsFactors = FALSE
  )
  .db_env$pool_main <- NULL

  withr::defer({
    .db_env$traitlist_cache <- old_cache
    .db_env$pool_main <- old_pool
  }, envir = env)
}

selector_args <- function() {
  list(pool = shiny::reactive("con"),
       i18n = shiny::reactive(list(t = function(x) x)))
}

path_values <- function(path) vapply(path, function(x) x$value, character(1))

test_that("choosing a level 1 form offers its level 2 forms, and only those", {
  local_growth_form_traitlist()

  shiny::testServer(mod_growth_form_selector_server, args = selector_args(), {
    session$setInputs(level_1 = "terrestrial_self_supporting")
    html <- output$level_2_ui$html

    expect_match(html, "semi_woody")
    expect_match(html, "woody")
    expect_no_match(html, "epiphyte")
  })
})

test_that("choosing a level 2 form offers its level 3 forms", {
  local_growth_form_traitlist()

  shiny::testServer(mod_growth_form_selector_server, args = selector_args(), {
    session$setInputs(level_1 = "not_self_supporting", level_2 = "climber")
    html <- output$level_3_ui$html

    expect_match(html, "woody_vine_liana")
    expect_no_match(html, "tree")
  })
})

test_that("a form with nothing finer offers no level 3", {
  local_growth_form_traitlist()

  shiny::testServer(mod_growth_form_selector_server, args = selector_args(), {
    session$setInputs(level_1 = "not_self_supporting", level_2 = "epiphyte")

    expect_false(grepl("level_3", paste(output$level_3_ui$html, collapse = "")))
  })
})

test_that("a full path is recorded against the three level traits", {
  local_growth_form_traitlist()

  shiny::testServer(mod_growth_form_selector_server, args = selector_args(), {
    session$setInputs(level_1 = "terrestrial_self_supporting",
                      level_2 = "woody", level_3 = "tree")
    session$setInputs(btn_add_path = 1)

    path <- session$returned$growth_form_selections()[[1]]
    expect_equal(path_values(path),
                 c("terrestrial_self_supporting", "woody", "tree"))
    expect_equal(vapply(path, function(x) as.numeric(x$id_trait), numeric(1)),
                 c(41, 120, 121))
    expect_equal(vapply(path, function(x) x$trait, character(1)),
                 paste0("growth_form_level_", 1:3))
  })
})

test_that("a level 2 choice does not survive a change of level 1", {
  local_growth_form_traitlist()

  shiny::testServer(mod_growth_form_selector_server, args = selector_args(), {
    session$setInputs(level_1 = "terrestrial_self_supporting",
                      level_2 = "woody", level_3 = "tree")
    # The level 2 and 3 inputs keep their values on the server once removed
    session$setInputs(level_1 = "not_self_supporting")
    session$setInputs(btn_add_path = 1)

    path <- session$returned$growth_form_selections()[[1]]
    expect_equal(path_values(path), "not_self_supporting")
  })
})
