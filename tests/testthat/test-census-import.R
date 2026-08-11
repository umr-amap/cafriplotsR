# Tests for R/census_import_transaction.R and the pure helpers of
# R/mod_feat_step3_census_import.R

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

ci_existing <- function() {
  data.frame(
    plot_name   = rep("P1", 2),
    tag         = c("101", "102"),
    id_n        = c(11L, 12L),
    idtax_n     = c(500L, 500L),
    last_status = c("alive", "alive"),
    stringsAsFactors = FALSE
  )
}

ci_plots <- function() {
  data.frame(plot_name = "P1", id_liste_plots = 1L, stringsAsFactors = FALSE)
}

ci_traits <- function() {
  data.frame(
    id_trait  = c(1L, 2L),
    trait     = c("stem_diameter", "observations"),
    valuetype = c("numeric", "character"),
    stringsAsFactors = FALSE
  )
}

ci_split <- function(census = NULL) {
  if (is.null(census)) {
    census <- data.frame(
      plot_name = rep("P1", 3),
      tag       = c("101", "102", "201"),
      dbh       = c(21.1, 33.4, 10.2),
      note      = c("ok", NA, "leaning"),
      idtax_n   = c(500L, 500L, 700L),
      stringsAsFactors = FALSE
    )
  }
  split_census_table(census, existing = ci_existing())
}

ci_payload <- function(...) {
  args <- list(
    split         = ci_split(),
    plots         = ci_plots(),
    traits        = ci_traits(),
    trait_mapping = c(dbh = "stem_diameter"),
    census_mode   = "create",
    census_number = 3,
    census_year   = 2026
  )
  # Plain replacement: modifyList() would recurse into `split` and merge its
  # data frames element by element instead of swapping the whole object
  overrides <- list(...)
  for (nm in names(overrides)) args[[nm]] <- overrides[[nm]]
  do.call(.build_census_payload, args)
}

# =============================================================================
# .sql_values_rows()
# =============================================================================

test_that(".sql_values_rows renders a row per record", {
  df <- data.frame(a = c(1L, 2L), b = c("x", "y"), stringsAsFactors = FALSE)
  expect_equal(.sql_values_rows(df), "(1, 'x'), (2, 'y')")
})

test_that(".sql_values_rows keeps large numbers out of scientific notation", {
  df <- data.frame(a = c(1L, 100000L), stringsAsFactors = FALSE)
  expect_equal(.sql_values_rows(df), "(1), (100000)")
})

test_that(".sql_values_rows turns NA and blanks into NULL", {
  df <- data.frame(a = c(1, NA), b = c(NA, ""), stringsAsFactors = FALSE)
  expect_equal(.sql_values_rows(df), "(1, NULL), (NULL, NULL)")
})

test_that(".sql_values_rows escapes single quotes", {
  df <- data.frame(a = "O'Brien", stringsAsFactors = FALSE)
  expect_equal(.sql_values_rows(df), "('O''Brien')")
})

# =============================================================================
# .build_census_payload()
# =============================================================================

test_that(".build_census_payload pivots mapped columns into measurement rows", {
  res <- ci_payload()

  expect_equal(nrow(res$data), 3)
  expect_equal(res$data$trait_name, rep("stem_diameter", 3))
  expect_equal(res$data$traitid, rep(1L, 3))
  expect_equal(res$data$traitvalue, c(21.1, 33.4, 10.2))
  expect_true(all(is.na(res$data$traitvalue_char)))
  expect_equal(res$data$id_liste_plots, rep(1L, 3))
})

test_that(".build_census_payload routes character traits to traitvalue_char", {
  res <- ci_payload(trait_mapping = c(dbh = "stem_diameter", note = "observations"))

  chars <- res$data[res$data$trait_name == "observations", ]
  expect_equal(nrow(chars), 2)   # the NA note is dropped
  expect_equal(chars$traitvalue_char, c("ok", "leaning"))
  expect_true(all(is.na(chars$traitvalue)))
})

test_that(".build_census_payload skips rows with no value for a trait", {
  res <- ci_payload(trait_mapping = c(note = "observations"))
  expect_equal(nrow(res$data), 2)
})

test_that(".build_census_payload returns no measurements when nothing is mapped", {
  res <- ci_payload(trait_mapping = character(0))
  expect_equal(nrow(res$data), 0)
})

test_that(".build_census_payload separates recruits from remeasures", {
  res <- ci_payload()

  expect_equal(nrow(res$config$recruits), 1)
  expect_equal(res$config$recruits$tag, "201")
  expect_equal(res$config$recruits$id_table_liste_plots_n, 1L)
  expect_equal(sort(res$data$row_role), c("recruit", "remeasure", "remeasure"))
})

test_that(".build_census_payload copes with a census that has no recruits", {
  # Every stem already known — the ordinary case for a mature plot
  census <- data.frame(
    plot_name = c("P1", "P1"), tag = c("101", "102"),
    dbh = c(21.1, 33.4), stringsAsFactors = FALSE
  )
  res <- ci_payload(split = split_census_table(census, existing = ci_existing()))

  expect_equal(nrow(res$config$recruits), 0)
  expect_equal(res$config$n_unidentified_recruits, 0L)
  expect_equal(nrow(res$data), 2)
  expect_true(all(res$data$row_role == "remeasure"))
})

test_that(".execute_census_import dry run handles a census with no recruits", {
  census <- data.frame(
    plot_name = c("P1", "P1"), tag = c("101", "102"),
    dbh = c(21.1, 33.4), stringsAsFactors = FALSE
  )
  res <- ci_payload(split = split_census_table(census, existing = ci_existing()))
  out <- .execute_census_import(res$data, res$config,
                                structure(list(), class = "not_a_connection"),
                                dry_run = TRUE)

  expect_true(out$success)
  expect_equal(out$n_recruits, 0L)
  expect_equal(out$n_measurements, 2L)
})

test_that(".build_census_payload never promotes an ambiguous row to a recruit", {
  # Confirming reviewed tags turns them into recruits. A row matching several
  # recorded stems is the opposite of a new stem — promoting it would add a
  # second copy of a tree that is already there.
  existing <- data.frame(
    plot_name   = rep("P1", 3),
    tag         = c("5", "5", "101"),
    id_n        = c(11L, 12L, 13L),
    idtax_n     = rep(500L, 3),
    last_status = rep("alive", 3),
    stringsAsFactors = FALSE
  )
  census <- data.frame(plot_name = c("P1", "P1"), tag = c("5", "101"),
                       dbh = c(21.1, 30.2), stringsAsFactors = FALSE)
  split <- split_census_table(census, existing = existing)
  expect_equal(nrow(split$ambiguous), 1)

  res <- ci_payload(split = split, include_review = TRUE)

  # the unambiguous stem still goes through; the ambiguous one does not
  expect_equal(nrow(res$config$recruits), 0)
  expect_equal(nrow(res$data), 1)
  expect_equal(res$data$tag, "101")
  expect_equal(res$config$n_review_included, 0L)
})

test_that(".build_census_payload returns nothing when every row is ambiguous", {
  existing <- data.frame(
    plot_name   = rep("P1", 2),
    tag         = c("5", "5"),
    id_n        = c(11L, 12L),
    idtax_n     = c(500L, 500L),
    last_status = c("alive", "alive"),
    stringsAsFactors = FALSE
  )
  census <- data.frame(plot_name = "P1", tag = "5", dbh = 21.1,
                       stringsAsFactors = FALSE)
  split <- split_census_table(census, existing = existing)

  res <- ci_payload(split = split, include_review = TRUE)

  expect_equal(nrow(res$data), 0)
  expect_null(res$config)
})

test_that(".validate_census_import reports ambiguous rows on their own", {
  existing <- data.frame(
    plot_name   = rep("P1", 2),
    tag         = c("5", "5"),
    id_n        = c(11L, 12L),
    idtax_n     = c(500L, 500L),
    last_status = c("alive", "alive"),
    stringsAsFactors = FALSE
  )
  census <- data.frame(plot_name = c("P1", "P1"), tag = c("5", "102"),
                       dbh = c(21.1, 30),
                       stringsAsFactors = FALSE)
  split <- split_census_table(census, existing = existing)

  out <- .validate_census_import(
    data = data.frame(id_liste_plots = 1L),
    config = list(split = split, census_mode = "create", census_number = 3,
                  census_year = 2026, recruits = NULL, n_review_included = 0L),
    con = NULL
  )
  msgs <- paste(unlist(out$warnings), collapse = " | ")

  expect_match(msgs, "match more than one recorded stem")
  expect_false(grepl("held for review", msgs))
})

test_that(".build_census_payload fills missing taxonomy and counts it", {
  census <- data.frame(
    plot_name = c("P1", "P1"),
    tag       = c("101", "201"),
    dbh       = c(21.1, 10.2),
    idtax_n   = c(500L, NA),
    stringsAsFactors = FALSE
  )
  res <- ci_payload(split = split_census_table(census, existing = ci_existing()))

  expect_equal(res$config$recruits$idtax_n, 351190L)
  expect_equal(res$config$n_unidentified_recruits, 1L)
})

test_that(".build_census_payload carries multi_tiges_id onto recruits", {
  census <- data.frame(
    plot_name      = c("P1", "P1"),
    tag            = c("201", "202"),
    dbh            = c(10.2, 8.1),
    multi_tiges_id = c(NA, "201"),
    stringsAsFactors = FALSE
  )
  res <- ci_payload(split = split_census_table(census, existing = ci_existing()))

  expect_true("multi_tiges_id" %in% names(res$config$recruits))
  expect_equal(res$config$recruits$multi_tiges_id, c(NA, "201"))
})

test_that(".build_census_payload excludes review rows unless confirmed", {
  census <- data.frame(
    plot_name = c("P1", "P1"),
    tag       = c("101", "1O2"),   # 1O2 resembles the existing 102
    dbh       = c(21.1, 10.2),
    stringsAsFactors = FALSE
  )
  split <- split_census_table(census, existing = ci_existing())
  expect_equal(sum(split$data$row_role == "review"), 1)

  excluded <- ci_payload(split = split)
  expect_equal(nrow(excluded$data), 1)
  expect_equal(nrow(excluded$config$recruits), 0)
  expect_equal(excluded$config$n_review_included, 0L)

  included <- ci_payload(split = split, include_review = TRUE)
  expect_equal(nrow(included$data), 2)
  expect_equal(included$config$recruits$tag, "1O2")
  expect_equal(included$config$n_review_included, 1L)
})

test_that(".build_census_payload records the census identity in the config", {
  res <- ci_payload(census_mode = "create", census_number = 4, census_year = 2027,
                    census_month = 3, census_day = 15)

  expect_equal(res$config$mode, "import_census")
  expect_equal(res$config$census_mode, "create")
  expect_equal(res$config$census_number, 4)
  expect_equal(res$config$census_year, 2027)
  expect_equal(res$config$census_month, 3)
})

# =============================================================================
# .census_selected_map()
# =============================================================================

test_that(".census_selected_map keeps only the chosen censuses", {
  censuses <- data.frame(
    id_sub_plots = c(10L, 11L, 12L),
    id_table_liste_plots = c(1L, 1L, 2L),
    census_num = c(1, 2, 1),
    stringsAsFactors = FALSE
  )
  res <- .census_selected_map(c(11L, 12L), censuses)

  expect_equal(nrow(res), 2)
  expect_equal(sort(res$id_sub_plots), c(11L, 12L))
  expect_equal(names(res), c("id_table_liste_plots", "id_sub_plots"))
})

test_that(".census_selected_map returns NULL when nothing is selected", {
  expect_null(.census_selected_map(NULL, data.frame(id_sub_plots = 1L)))
  expect_null(.census_selected_map(integer(0), data.frame(id_sub_plots = 1L)))
  expect_null(.census_selected_map(1L, NULL))
})

# =============================================================================
# .census_mapped_columns() / .census_trait_mapping()
# =============================================================================

test_that(".census_mapped_columns collects the claimed columns", {
  input <- list(map_plot_name = "parcelle", map_tag = "no_arbre",
                map_idtax_n = "", map_multi_tiges_id = "multi")
  expect_equal(sort(.census_mapped_columns(input)),
               c("multi", "no_arbre", "parcelle"))
})

test_that(".census_trait_mapping only keeps columns pointed at a real trait", {
  data <- data.frame(parcelle = "P1", no_arbre = "1", dbh = 1, junk = 1,
                     stringsAsFactors = FALSE)
  input <- list(
    map_plot_name = "parcelle", map_tag = "no_arbre",
    trait_map_dbh = "stem_diameter",
    trait_map_junk = "not_a_real_trait"
  )
  res <- .census_trait_mapping(input, data, ci_traits())

  expect_equal(res, c(dbh = "stem_diameter"))
})

# =============================================================================
# .validate_census_import()
# =============================================================================

ci_con <- structure(list(), class = "not_a_connection")

test_that(".validate_census_import passes a well-formed payload", {
  res <- ci_payload()
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_equal(length(v$errors), 0)
})

test_that(".validate_census_import rejects a missing configuration", {
  v <- .validate_census_import(data.frame(), NULL, ci_con)
  expect_true(length(v$errors) > 0)
})

test_that(".validate_census_import requires measurements", {
  res <- ci_payload(trait_mapping = character(0))
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_true(any(grepl("No measurement", unlist(v$errors))))
})

test_that(".validate_census_import requires a census number and year when creating", {
  res <- ci_payload(census_number = NA, census_year = NA)
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_true(any(grepl("census number", unlist(v$errors))))
  expect_true(any(grepl("census year", unlist(v$errors))))
})

test_that(".validate_census_import requires a census when reusing one", {
  res <- ci_payload(census_mode = "existing", census_map = NULL)
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_true(any(grepl("Select the census", unlist(v$errors))))
})

test_that(".validate_census_import catches plots with no census selected", {
  res <- ci_payload(
    census_mode = "existing",
    census_map = data.frame(id_table_liste_plots = 99L, id_sub_plots = 5L)
  )
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_true(any(grepl("no census selected", unlist(v$errors))))
})

test_that(".validate_census_import accepts a census map covering every plot", {
  res <- ci_payload(
    census_mode = "existing",
    census_map = data.frame(id_table_liste_plots = 1L, id_sub_plots = 5L)
  )
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_equal(length(v$errors), 0)
})

test_that(".validate_census_import warns about unidentified recruits", {
  census <- data.frame(
    plot_name = c("P1", "P1"), tag = c("101", "201"),
    dbh = c(21.1, 10.2), idtax_n = c(500L, NA),
    stringsAsFactors = FALSE
  )
  res <- ci_payload(split = split_census_table(census, existing = ci_existing()))
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_true(any(grepl("unidentified", unlist(v$warnings))))
})

test_that(".validate_census_import rejects a file with repeated stems", {
  census <- data.frame(
    plot_name = rep("P1", 3), tag = c("101", "101", "201"),
    dbh = c(21.1, 22.0, 10.2), stringsAsFactors = FALSE
  )
  res <- ci_payload(split = split_census_table(census, existing = ci_existing()))
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_true(any(grepl("more than once", unlist(v$errors))))
})

test_that(".validate_census_import warns when review rows are being dropped", {
  census <- data.frame(
    plot_name = c("P1", "P1"), tag = c("101", "1O2"),
    dbh = c(21.1, 10.2), stringsAsFactors = FALSE
  )
  res <- ci_payload(split = split_census_table(census, existing = ci_existing()))
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_true(any(grepl("held for review", unlist(v$warnings))))
})

test_that(".validate_census_import reports stems missing from the file", {
  # the census only covers 101, so 102 is unaccounted for
  census <- data.frame(plot_name = "P1", tag = "101", dbh = 21.1,
                       stringsAsFactors = FALSE)
  res <- ci_payload(split = split_census_table(census, existing = ci_existing()))
  v <- .validate_census_import(res$data, res$config, ci_con)

  expect_true(any(grepl("no row in the file", unlist(v$warnings))))
})

# =============================================================================
# .execute_census_import() — dry run needs no database
# =============================================================================

test_that(".execute_census_import dry run reports the three record counts", {
  res <- ci_payload()
  out <- .execute_census_import(res$data, res$config, ci_con, dry_run = TRUE)

  expect_true(out$success)
  expect_true(out$dry_run)
  expect_equal(out$n_census_records, 1L)
  expect_equal(out$n_recruits, 1L)
  expect_equal(out$n_measurements, 3L)
  expect_match(out$message, "Dry run")
})

test_that(".execute_census_import dry run counts no census when reusing one", {
  res <- ci_payload(
    census_mode = "existing",
    census_map = data.frame(id_table_liste_plots = 1L, id_sub_plots = 5L)
  )
  out <- .execute_census_import(res$data, res$config, ci_con, dry_run = TRUE)

  expect_equal(out$n_census_records, 0L)
  expect_equal(out$n_recruits, 1L)
})

# =============================================================================
# Database-facing pieces, against SQLite
# =============================================================================

make_census_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(con, "data_liste_plots", data.frame(
    id_liste_plots = c(1L, 2L), plot_name = c("P1", "P2"),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "subplotype_list", data.frame(
    id_subplotype = c(7L, 8L), type = c("census", "soil_sample"),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "data_liste_sub_plots", data.frame(
    id_sub_plots         = c(100L, 101L),
    id_table_liste_plots = c(1L, 1L),
    id_type_sub_plot     = c(7L, 8L),
    typevalue            = c(1, NA),
    year                 = c(2020L, NA),
    month                = c(NA_integer_, NA_integer_),
    day                  = c(NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  ))
  con
}

test_that(".census_subplot_type_id finds the census type", {
  skip_if_not_installed("RSQLite")
  con <- make_census_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(.census_subplot_type_id(con), 7L)
})

test_that(".census_subplot_type_id errors when the type is absent", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "subplotype_list",
                    data.frame(id_subplotype = 1L, type = "soil_sample"))

  expect_error(.census_subplot_type_id(con), "subplotype_list")
})

test_that(".fetch_census_subplots returns only census rows", {
  skip_if_not_installed("RSQLite")
  con <- make_census_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- .fetch_census_subplots(c(1L, 2L), con)

  expect_equal(nrow(res), 1)          # the soil_sample subplot is excluded
  expect_equal(res$id_sub_plots, 100L)
  expect_equal(res$census_num, 1)
})

test_that(".fetch_census_subplots handles plots with nothing recorded", {
  skip_if_not_installed("RSQLite")
  con <- make_census_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(nrow(.fetch_census_subplots(2L, con)), 0)
  expect_equal(nrow(.fetch_census_subplots(integer(0), con)), 0)
})

test_that(".create_census_subplots refuses to duplicate an existing census", {
  skip_if_not_installed("RSQLite")
  con <- make_census_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # plot 1 already carries census 1
  expect_error(
    .create_census_subplots(plot_ids = 1L, census_number = 1, year = 2026, con = con),
    "already exists"
  )
})

test_that(".create_census_subplots requires plots and a number", {
  skip_if_not_installed("RSQLite")
  con <- make_census_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_error(.create_census_subplots(integer(0), 2, 2026, con = con), "No plots")
  expect_error(.create_census_subplots(1L, NA, 2026, con = con), "census number")
})

test_that(".resolve_census_individuals matches both existing and new stems", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "data_liste_plots", data.frame(
    id_liste_plots = 1L, plot_name = "P1", stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "data_individuals", data.frame(
    id_n = c(11L, 12L), id_table_liste_plots_n = c(1L, 1L),
    tag = c("101", "102"), idtax_n = c(500L, 500L),
    stem_grouping = c(NA_integer_, NA_integer_), stringsAsFactors = FALSE))

  data <- data.frame(
    plot_name = rep("P1", 3), tag = c("101", "102", "201"),
    stringsAsFactors = FALSE
  )
  new_individuals <- data.frame(
    id_individuals = 99L, tag = "201", plot_name = "P1",
    stringsAsFactors = FALSE
  )

  res <- suppressMessages(
    .resolve_census_individuals(data, 1L, new_individuals, con)
  )

  expect_equal(res$id_data_individuals, c(11L, 12L, 99L))
})

test_that(".resolve_census_individuals leaves unknown stems unresolved", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "data_liste_plots", data.frame(
    id_liste_plots = 1L, plot_name = "P1", stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "data_individuals", data.frame(
    id_n = 11L, id_table_liste_plots_n = 1L, tag = "101",
    idtax_n = 500L, stem_grouping = NA_integer_, stringsAsFactors = FALSE))

  data <- data.frame(plot_name = c("P1", "P1"), tag = c("101", "999"),
                     stringsAsFactors = FALSE)
  res <- suppressMessages(.resolve_census_individuals(data, 1L, NULL, con))

  expect_equal(res$id_data_individuals, c(11L, NA_integer_))
})

test_that(".insert_census_recruits returns an empty frame for no recruits", {
  res <- .insert_census_recruits(NULL, con = NULL)
  expect_equal(nrow(res), 0)
  expect_true(all(c("id_individuals", "tag", "plot_name") %in% names(res)))

  res2 <- .insert_census_recruits(data.frame(), con = NULL)
  expect_equal(nrow(res2), 0)
})

test_that(".insert_census_recruits requires the plot id and tag columns", {
  bad <- data.frame(plot_name = "P1", idtax_n = 1L, stringsAsFactors = FALSE)
  expect_error(.insert_census_recruits(bad, con = NULL),
               "id_table_liste_plots_n")
})
