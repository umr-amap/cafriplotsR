# The parent-plot section of launch_data_update_app().
#
# `data_liste_plots.id_parent_plot` and `parent_relation` are the one pair of
# columns the app's generic form cannot write, because the database refuses a
# row holding either alone and the generic path writes one column per
# statement. These tests cover the dedicated path that replaces it: what it
# offers, what it refuses, and that it moves both columns at once.

link_i18n <- function() list(t = function(x) x)

# A mock database. Dispatches on a distinctive fragment of each query the
# functions under test issue; anything unnamed comes back empty.
#
# The connection underneath is a real (empty) SQLite one, because `glue_sql()`
# asks it how to quote a literal and no query ever reaches it.
mock_link_con <- function(...,
                          hierarchy = TRUE,
                          executed = NULL,
                          env = parent.frame()) {
  testthat::skip_if_not_installed("RSQLite")
  results <- list(...)

  pick <- function(name, fallback = data.frame()) {
    if (!is.null(results[[name]])) results[[name]] else fallback
  }

  testthat::local_mocked_bindings(
    .package = "DBI",
    dbListFields = function(conn, name, ...) {
      if (hierarchy) {
        c("id_liste_plots", "plot_name", "id_parent_plot", "parent_relation")
      } else {
        c("id_liste_plots", "plot_name")
      }
    },
    dbGetQuery = function(con, sql, ...) {
      sql_chr <- gsub("\\s+", " ", as.character(sql))
      if (grepl("WITH RECURSIVE descendants", sql_chr, fixed = TRUE)) {
        return(pick("descendants", CafriplotsR:::.upd_empty_plot_rows()))
      }
      if (grepl("WITH RECURSIVE anc", sql_chr, fixed = TRUE)) {
        return(pick("chain", CafriplotsR:::.upd_empty_plot_rows()))
      }
      if (grepl("LEFT JOIN data_liste_plots p", sql_chr, fixed = TRUE)) {
        return(pick("self"))
      }
      if (grepl("WHERE id_parent_plot =", sql_chr, fixed = TRUE)) {
        return(pick("children"))
      }
      if (grepl("SELECT COUNT(*) AS n", sql_chr, fixed = TRUE)) {
        return(pick("parent_exists", data.frame(n = 1L)))
      }
      if (grepl("SELECT id_parent_plot, parent_relation", sql_chr, fixed = TRUE)) {
        return(pick("current"))
      }
      if (grepl("SELECT id_liste_plots, plot_name FROM data_liste_plots",
                sql_chr, fixed = TRUE)) {
        return(pick("all_plots"))
      }
      stop(sprintf("Unexpected query: %s", sql_chr))
    },
    .env = env
  )

  if (!is.null(executed)) {
    testthat::local_mocked_bindings(
      .package = "DBI",
      dbExecute = function(conn, statement, ...) {
        assign(executed, c(get(executed, envir = env),
                           gsub("\\s+", " ", as.character(statement))),
               envir = env)
        1L
      },
      .env = env
    )
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

all_plots_df <- function() {
  data.frame(
    id_liste_plots = c(1L, 2L, 7L, 9L),
    plot_name      = c("P1", "P2", "P1_regen", "P1_regen_deep"),
    stringsAsFactors = FALSE
  )
}


# ── The pair cannot be reached through the generic form ──────────────────────

test_that("the parent link is kept out of the editable flat columns", {
  # chk_plot_parent_relation_paired plus one-UPDATE-per-column is a guaranteed
  # constraint violation, so the form must never offer these two.
  exclude <- CafriplotsR:::.upd_entity_spec("plot")$exclude

  expect_true("id_parent_plot" %in% exclude)
  expect_true("parent_relation" %in% exclude)
})


# ── Validation without a database: the two CHECK constraints ─────────────────

test_that("a parent without a relation is refused, and the other way round", {
  expect_equal(
    CafriplotsR:::.upd_validate_plot_link(7L, 1L, NA),
    "relation_missing"
  )
  expect_equal(
    CafriplotsR:::.upd_validate_plot_link(7L, NA, "block_member"),
    "parent_missing"
  )
})

test_that("a complete link and an empty link are both sound", {
  expect_length(
    CafriplotsR:::.upd_validate_plot_link(7L, 1L, "nested_subsample"), 0
  )
  expect_length(CafriplotsR:::.upd_validate_plot_link(7L, NA, NA), 0)
  # An empty select posts "" rather than NA, and that is a detach, not a value.
  expect_length(CafriplotsR:::.upd_validate_plot_link(7L, "", ""), 0)
})

test_that("a relation outside the vocabulary is refused", {
  expect_true("unknown_relation" %in%
    CafriplotsR:::.upd_validate_plot_link(7L, 1L, "sub_placette"))
})

test_that("a plot cannot be its own parent", {
  expect_true("self_parent" %in%
    CafriplotsR:::.upd_validate_plot_link(7L, 7L, "block_member"))
})


# ── Validation against the stored hierarchy ──────────────────────────────────

test_that("a parent that is already a descendant is refused as a loop", {
  # Nothing in the schema stops A -> B -> A; this check is the only thing that
  # does once the value has left the select.
  con <- mock_link_con(
    descendants = data.frame(
      id_liste_plots = 1L, plot_name = "P1",
      parent_relation = "block_member", depth = 1L,
      stringsAsFactors = FALSE
    )
  )

  expect_true("cycle" %in%
    CafriplotsR:::.upd_validate_plot_link(7L, 1L, "block_member", con))
})

test_that("a parent that no longer exists is refused", {
  con <- mock_link_con(parent_exists = data.frame(n = 0L))

  expect_equal(
    CafriplotsR:::.upd_validate_plot_link(7L, 999L, "block_member", con),
    "parent_not_found"
  )
})


# ── Candidate parents ────────────────────────────────────────────────────────

test_that("the plot itself and everything under it are not offered as parents", {
  con <- mock_link_con(
    all_plots = all_plots_df(),
    descendants = data.frame(
      id_liste_plots = 9L, plot_name = "P1_regen_deep",
      parent_relation = "nested_subsample", depth = 1L,
      stringsAsFactors = FALSE
    )
  )

  choices <- CafriplotsR:::.upd_plot_parent_choices(7L, con)

  expect_setequal(names(choices), c("P1", "P2"))
  expect_false("7" %in% choices)
  expect_false("9" %in% choices)
})


# ── Reading one plot's link ──────────────────────────────────────────────────

test_that(".upd_plot_link reports nothing to edit on an unmigrated database", {
  con <- mock_link_con(hierarchy = FALSE)

  link <- CafriplotsR:::.upd_plot_link(7L, con)

  expect_false(link$available)
  expect_length(link$candidates, 0)
})

test_that(".upd_plot_link returns the parent, the children and the candidates", {
  con <- mock_link_con(
    self = data.frame(
      id_liste_plots = 7L, id_parent_plot = 1L,
      parent_relation = "nested_subsample", parent_name = "P1",
      stringsAsFactors = FALSE
    ),
    children = data.frame(
      id_liste_plots = 9L, plot_name = "P1_regen_deep",
      parent_relation = "block_member", stringsAsFactors = FALSE
    ),
    chain = data.frame(
      id_liste_plots = c(7L, 1L), plot_name = c("P1_regen", "P1"),
      parent_relation = c("nested_subsample", NA), depth = c(1L, 2L),
      stringsAsFactors = FALSE
    ),
    all_plots = all_plots_df()
  )

  link <- CafriplotsR:::.upd_plot_link(7L, con)

  expect_true(link$available)
  expect_equal(link$id_parent_plot, 1L)
  expect_equal(link$parent_name, "P1")
  expect_equal(link$parent_relation, "nested_subsample")
  expect_equal(nrow(link$children), 1L)
  expect_equal(nrow(link$chain), 2L)
  expect_true("1" %in% link$candidates)
})

test_that("the current parent stays selectable even when the hierarchy loops", {
  # A stored cycle would put the parent among this plot's descendants and drop
  # it from the candidates. A blank select reads as "no parent", which would
  # offer to detach a link the user never touched.
  con <- mock_link_con(
    self = data.frame(
      id_liste_plots = 7L, id_parent_plot = 1L,
      parent_relation = "block_member", parent_name = "P1",
      stringsAsFactors = FALSE
    ),
    chain = data.frame(
      id_liste_plots = 7L, plot_name = "P1_regen",
      parent_relation = "block_member", depth = 1L, stringsAsFactors = FALSE
    ),
    all_plots = all_plots_df(),
    descendants = data.frame(
      id_liste_plots = 1L, plot_name = "P1",
      parent_relation = "block_member", depth = 1L, stringsAsFactors = FALSE
    )
  )

  link <- CafriplotsR:::.upd_plot_link(7L, con)

  expect_true("1" %in% link$candidates)
  expect_true("P1" %in% names(link$candidates))
})


# ── Writing the link ─────────────────────────────────────────────────────────

test_that("attaching a parent moves both columns in one statement", {
  # Two statements would leave the row holding a parent with no relation, which
  # chk_plot_parent_relation_paired rejects on the spot.
  executed <- character()
  con <- mock_link_con(
    executed = "executed",
    current = data.frame(id_parent_plot = NA_integer_,
                         parent_relation = NA_character_)
  )
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    backup_direct_records = function(changes, config, con) invisible(NULL),
    .upd_routing = function(table_type, columns, con) list()
  )

  n <- CafriplotsR:::.upd_apply_plot_link(7L, 1L, "block_member", con)

  expect_equal(n, 1L)
  expect_length(executed, 1L)
  expect_match(executed, "id_parent_plot = 1")
  expect_match(executed, "parent_relation = 'block_member'")
})

test_that("detaching clears both columns, not just the parent", {
  executed <- character()
  con <- mock_link_con(
    executed = "executed",
    current = data.frame(id_parent_plot = 1L, parent_relation = "block_member",
                         stringsAsFactors = FALSE)
  )
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    backup_direct_records = function(changes, config, con) invisible(NULL),
    .upd_routing = function(table_type, columns, con) list()
  )

  n <- CafriplotsR:::.upd_apply_plot_link(7L, NA, NA, con)

  expect_equal(n, 1L)
  expect_match(executed, "id_parent_plot = NULL")
  expect_match(executed, "parent_relation = NULL")
})

test_that("a link that already matches what is stored writes nothing", {
  executed <- character()
  con <- mock_link_con(
    executed = "executed",
    current = data.frame(id_parent_plot = 1L, parent_relation = "block_member",
                         stringsAsFactors = FALSE)
  )

  n <- CafriplotsR:::.upd_apply_plot_link(7L, 1L, "block_member", con)

  expect_equal(n, 0L)
  expect_length(executed, 0L)
})

test_that("an invalid link is refused before anything is written", {
  executed <- character()
  con <- mock_link_con(
    executed = "executed",
    current = data.frame(id_parent_plot = NA_integer_,
                         parent_relation = NA_character_)
  )

  expect_error(
    CafriplotsR:::.upd_apply_plot_link(7L, 1L, NA, con),
    "relation_missing"
  )
  expect_length(executed, 0L)
})

test_that("nothing is written on a database without the hierarchy columns", {
  executed <- character()
  con <- mock_link_con(hierarchy = FALSE, executed = "executed")

  expect_equal(CafriplotsR:::.upd_apply_plot_link(7L, 1L, "block_member", con), 0L)
  expect_length(executed, 0L)
})


# ── What the form says ───────────────────────────────────────────────────────

test_that("the relation select offers exactly the CHECK vocabulary, plus none", {
  choices <- CafriplotsR:::.upd_relation_choices(link_i18n())

  expect_setequal(unname(choices), c("", CafriplotsR:::.plot_parent_relations()))
  # The labels have to carry the arithmetic; the bare value does not say it.
  expect_true(any(grepl("Never sum", names(choices))))
  expect_true(any(grepl("Summing", names(choices))))
})

test_that("every problem the validator can report has a sentence", {
  codes <- c("relation_missing", "parent_missing", "unknown_relation",
             "self_parent", "parent_not_found", "cycle")
  i18n <- link_i18n()

  for (code in codes) {
    expect_false(identical(CafriplotsR:::.upd_link_problem_text(code, i18n), code),
                 info = code)
  }
})

link_translator <- function(lang) {
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file("translations/translation.json",
                                        package = "CafriplotsR")
  )
  i18n$set_translation_language(lang)
  i18n
}

test_that("the parent-plot wording is in the translation file", {
  # shiny.i18n echoes a string it does not know, so a French rendering that
  # equals the English one means the entry is missing.
  en <- link_translator("en")
  fr <- link_translator("fr")

  codes <- c("relation_missing", "parent_missing", "unknown_relation",
             "self_parent", "parent_not_found", "cycle")
  for (code in codes) {
    expect_false(
      identical(CafriplotsR:::.upd_link_problem_text(code, en),
                CafriplotsR:::.upd_link_problem_text(code, fr)),
      info = code
    )
  }

  expect_false(identical(names(CafriplotsR:::.upd_relation_choices(en)),
                         names(CafriplotsR:::.upd_relation_choices(fr))))
})

test_that("only the plot section carries the parent-plot inputs", {
  i18n <- link_translator("fr")

  ui_plot <- as.character(mod_update_record_ui("p", "plot", i18n))
  ui_ind  <- as.character(mod_update_record_ui("i", "individual", i18n))

  expect_true(grepl("p-parent_plot", ui_plot, fixed = TRUE))
  expect_true(grepl("p-parent_relation", ui_plot, fixed = TRUE))
  expect_true(grepl("p-has_hierarchy", ui_plot, fixed = TRUE))
  expect_false(grepl("parent_plot", ui_ind, fixed = TRUE))
})
