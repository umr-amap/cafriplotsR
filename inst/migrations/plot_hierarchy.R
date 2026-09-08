# ARCHIVED MIGRATION - applied 2026-09-08, kept for the record
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
# To run one (should not be necessary - these are one-shot):
#   source(system.file("migrations", "plot_hierarchy.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#   migrate_plot_hierarchy(con)                   # rehearsal: prints, changes nothing
#   migrate_plot_hierarchy(con, dry_run = FALSE)  # apply
#
#
# WHAT THIS ADDS
#
#   1. `data_liste_plots.id_parent_plot` - self-referencing FK. The plot this
#      plot sits inside. NULL for every plot that exists today.
#   2. `data_liste_plots.parent_relation` - how it sits inside, from a closed
#      vocabulary. NOT optional when a parent is set; see below.
#
#
# WHY THE RELATION TYPE IS NOT OPTIONAL
#
# `table_taxa.id_parent` (see taxa_hierarchy.R) gets away with a bare parent
# column because `tax_level` already says what each node is - species under
# genus under family, one canonical ladder. Plots have no such ladder, so a
# bare `id_parent_plot` would say "related to" without saying how. That is not
# a cosmetic gap: the aggregation rule inverts between the two cases.
#
#   nested_subsample  a regeneration inventory (2-10 cm stems, ~0.12 ha across
#                     3 quadrats) inside a 1 ha plot. Parent and child OVERLAP
#                     GROUND and use different diameter thresholds. Summing
#                     them double-counts the surface and mixes protocols.
#
#   block_member      1 ha plots partitioning a larger block. Children TILE the
#                     parent, no overlap. Summing them is correct.
#
# Same edge shape, opposite arithmetic. So the paired CHECK below refuses a
# parent without a relation - an untyped edge is worse than no edge, because it
# invites traversal while withholding the fact that makes traversal safe.
#
#
# WHY NOT `linktypelist`
#
# Reusing the existing link-type lookup was considered and rejected. Its
# `scope` column carries `CHECK (scope IN ('individual', 'plot'))`, so a third
# value means constraint surgery on a table that governs specimen links; and
# its `priority` column means "which specimen governs an individual's
# determination", which is meaningless for a plot relation. A closed VARCHAR
# vocabulary with a CHECK is both simpler and already the house precedent -
# `linktypelist.scope` itself was added exactly that way by
# reference_plot_linktype.R.
#
#
# SAFETY
#
# Two nullable columns, no backfill, no data touched. All ~2,166 existing plots
# keep `id_parent_plot IS NULL` and behave exactly as before. Nothing in the
# package reads either column until the import-path changes ship alongside.
# This is additive in the strict sense: the migration and the code that uses it
# can be applied in either order.
#
# Still missing after this runs, and deliberately not part of it:
# `check_plot_hierarchy_consistency()` - the ongoing cycle probe. Per the
# README convention, an ongoing consistency check belongs in R/, not here. The
# CHECK constraint below stops a plot being its own parent; it does not stop
# A -> B -> A, which needs a recursive CTE at write time.


#' Add id_parent_plot Column to data_liste_plots
#'
#' Adds the self-referencing parent column, its foreign key, its index, and a
#' constraint forbidding a plot from being its own parent. Phase 1 of the plot
#' hierarchy migration.
#'
#' `ON DELETE SET NULL` mirrors `fk_table_taxa_id_parent`. It is a backstop
#' only: `safe_delete_plot()` should refuse to delete a plot that still has
#' children rather than let them be silently orphaned.
#'
#' @param con Database connection to the main database
#' @param dry_run If TRUE, only print SQL without executing
#'
#' @return TRUE if successful
#' @keywords internal
migration_add_plot_parent_column <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) {
    con <- call.mydb()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Migration: add data_liste_plots.id_parent_plot")

  existing_cols <- DBI::dbListFields(actual_con, "data_liste_plots")
  if ("id_parent_plot" %in% existing_cols) {
    cli::cli_alert_success("Column 'id_parent_plot' already exists. Nothing to do.")
    return(TRUE)
  }

  sql_add_column <-
    "ALTER TABLE data_liste_plots ADD COLUMN id_parent_plot INTEGER;"

  sql_add_fk <- paste(
    "ALTER TABLE data_liste_plots",
    "ADD CONSTRAINT fk_data_liste_plots_id_parent_plot",
    "FOREIGN KEY (id_parent_plot) REFERENCES data_liste_plots(id_liste_plots)",
    "ON DELETE SET NULL;"
  )

  sql_add_index <- paste(
    "CREATE INDEX IF NOT EXISTS idx_data_liste_plots_id_parent_plot",
    "ON data_liste_plots(id_parent_plot);"
  )

  # A plot cannot be its own parent. Cheap, and it catches the commonest
  # data-entry error. It does NOT catch longer cycles.
  sql_add_self_check <- paste(
    "ALTER TABLE data_liste_plots",
    "ADD CONSTRAINT chk_plot_not_own_parent",
    "CHECK (id_parent_plot IS DISTINCT FROM id_liste_plots);"
  )

  if (dry_run) {
    cli::cli_h2("Dry run - SQL statements that would be executed:")
    cli::cli_code(sql_add_column)
    cli::cli_code(sql_add_fk)
    cli::cli_code(sql_add_index)
    cli::cli_code(sql_add_self_check)
    return(TRUE)
  }

  tryCatch({
    cli::cli_alert_info("Adding id_parent_plot column...")
    DBI::dbExecute(actual_con, sql_add_column)
    cli::cli_alert_success("Column added")

    cli::cli_alert_info("Adding foreign key constraint...")
    DBI::dbExecute(actual_con, sql_add_fk)
    cli::cli_alert_success("Foreign key constraint added")

    cli::cli_alert_info("Creating index...")
    DBI::dbExecute(actual_con, sql_add_index)
    cli::cli_alert_success("Index created")

    cli::cli_alert_info("Adding self-parent check constraint...")
    DBI::dbExecute(actual_con, sql_add_self_check)
    cli::cli_alert_success("Check constraint chk_plot_not_own_parent added")

    cli::cli_alert_success("Phase 1 complete: id_parent_plot added")
    return(TRUE)

  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Add parent_relation Column to data_liste_plots
#'
#' Adds the relation vocabulary and the paired constraint tying it to
#' `id_parent_plot`. Phase 2 of the plot hierarchy migration.
#'
#' Extending the vocabulary later means dropping and re-adding
#' `chk_plot_parent_relation` with the new value included - a second migration,
#' deliberately, so that a new relation type is a decision someone records
#' rather than a string someone typed.
#'
#' @param con Database connection to the main database
#' @param dry_run If TRUE, only print SQL without executing
#'
#' @return TRUE if successful
#' @keywords internal
migration_add_plot_relation_column <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) {
    con <- call.mydb()
  }

  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Migration: add data_liste_plots.parent_relation")

  existing_cols <- DBI::dbListFields(actual_con, "data_liste_plots")

  if (!"id_parent_plot" %in% existing_cols && !dry_run) {
    cli::cli_alert_danger(
      "id_parent_plot missing. Run migration_add_plot_parent_column() first."
    )
    stop("id_parent_plot required")
  }

  if ("parent_relation" %in% existing_cols) {
    cli::cli_alert_success("Column 'parent_relation' already exists. Nothing to do.")
    return(TRUE)
  }

  sql_add_column <-
    "ALTER TABLE data_liste_plots ADD COLUMN parent_relation VARCHAR(30);"

  sql_add_vocab_check <- paste(
    "ALTER TABLE data_liste_plots",
    "ADD CONSTRAINT chk_plot_parent_relation",
    "CHECK (parent_relation IS NULL",
    "       OR parent_relation IN ('nested_subsample', 'block_member'));"
  )

  # A parent without a relation is the failure mode this whole migration
  # exists to prevent, so the database refuses it outright.
  sql_add_paired_check <- paste(
    "ALTER TABLE data_liste_plots",
    "ADD CONSTRAINT chk_plot_parent_relation_paired",
    "CHECK ((id_parent_plot IS NULL     AND parent_relation IS NULL)",
    "    OR (id_parent_plot IS NOT NULL AND parent_relation IS NOT NULL));"
  )

  if (dry_run) {
    cli::cli_h2("Dry run - SQL statements that would be executed:")
    cli::cli_code(sql_add_column)
    cli::cli_code(sql_add_vocab_check)
    cli::cli_code(sql_add_paired_check)
    return(TRUE)
  }

  tryCatch({
    cli::cli_alert_info("Adding parent_relation column...")
    DBI::dbExecute(actual_con, sql_add_column)
    cli::cli_alert_success("Column added")

    cli::cli_alert_info("Adding vocabulary check constraint...")
    DBI::dbExecute(actual_con, sql_add_vocab_check)
    cli::cli_alert_success("Check constraint chk_plot_parent_relation added")

    cli::cli_alert_info("Adding paired check constraint...")
    DBI::dbExecute(actual_con, sql_add_paired_check)
    cli::cli_alert_success("Check constraint chk_plot_parent_relation_paired added")

    cli::cli_alert_success("Phase 2 complete: parent_relation added")
    return(TRUE)

  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Run the Plot Hierarchy Migration
#'
#' Runs both phases in order.
#'
#' @param con Database connection to the main database
#' @param dry_run If TRUE (the default), print the SQL and change nothing
#'
#' @return Named list of phase results, invisibly
#' @keywords internal
migrate_plot_hierarchy <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) {
    con <- call.mydb()
  }

  cli::cli_h1("Plot hierarchy migration")
  if (dry_run) {
    cli::cli_alert_info("Dry run. Nothing will be changed.")
  }

  results <- list()

  cli::cli_h2("Phase 1: id_parent_plot")
  results$parent_column <- migration_add_plot_parent_column(con, dry_run = dry_run)

  cli::cli_h2("Phase 2: parent_relation")
  results$relation_column <- migration_add_plot_relation_column(con, dry_run = dry_run)

  if (!dry_run) {
    cli::cli_h2("Verification")
    check_plot_hierarchy_migration(con)
  }

  invisible(results)
}


#' Check Whether the Plot Hierarchy Migration Has Run
#'
#' Inspects the schema rather than trusting the code, and reports how the
#' columns are being used.
#'
#' @param con Database connection to the main database
#'
#' @return Named list of check results, invisibly
#' @keywords internal
check_plot_hierarchy_migration <- function(con = NULL) {
  if (is.null(con)) {
    con <- call.mydb()
  }

  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Plot hierarchy migration status")

  checks <- list()
  existing_cols <- DBI::dbListFields(actual_con, "data_liste_plots")

  checks$parent_column <- "id_parent_plot" %in% existing_cols
  checks$relation_column <- "parent_relation" %in% existing_cols

  if (checks$parent_column) {
    cli::cli_alert_success("data_liste_plots.id_parent_plot exists")
  } else {
    cli::cli_alert_danger("data_liste_plots.id_parent_plot missing")
  }

  if (checks$relation_column) {
    cli::cli_alert_success("data_liste_plots.parent_relation exists")
  } else {
    cli::cli_alert_danger("data_liste_plots.parent_relation missing")
  }

  # Constraints, read from the catalogue
  constraint_names <- c(
    "fk_data_liste_plots_id_parent_plot",
    "chk_plot_not_own_parent",
    "chk_plot_parent_relation",
    "chk_plot_parent_relation_paired"
  )

  present <- DBI::dbGetQuery(actual_con, paste(
    "SELECT conname FROM pg_constraint",
    "WHERE conrelid = 'data_liste_plots'::regclass"
  ))$conname

  for (cname in constraint_names) {
    ok <- cname %in% present
    checks[[cname]] <- ok
    if (ok) {
      cli::cli_alert_success("constraint {cname} present")
    } else {
      cli::cli_alert_danger("constraint {cname} missing")
    }
  }

  # Usage
  if (checks$parent_column && checks$relation_column) {
    usage <- DBI::dbGetQuery(actual_con, paste(
      "SELECT parent_relation, COUNT(*) AS n",
      "FROM data_liste_plots",
      "WHERE id_parent_plot IS NOT NULL",
      "GROUP BY parent_relation ORDER BY n DESC"
    ))

    if (nrow(usage) == 0) {
      cli::cli_alert_info("No plot has a parent yet.")
    } else {
      cli::cli_h3("Plots with a parent")
      for (i in seq_len(nrow(usage))) {
        cli::cli_li("{usage$parent_relation[i]}: {usage$n[i]}")
      }
    }
    checks$linked_plots <- sum(usage$n)
  }

  checks$complete <- all(vapply(
    checks[c("parent_column", "relation_column", constraint_names)],
    isTRUE, logical(1)
  ))

  if (checks$complete) {
    cli::cli_alert_success("Migration complete")
  } else {
    cli::cli_alert_warning("Migration incomplete")
  }

  invisible(checks)
}
