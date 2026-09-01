# MIGRATION - formalises the `reference_plot` link type
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
# To run it:
#   source(system.file("migrations", "reference_plot_linktype.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#   run_reference_plot_linktype_migration(con)                  # rehearsal
#   run_reference_plot_linktype_migration(con, dry_run = FALSE) # apply
#
# Background
# ----------
# `data_link_specimens` has always carried an `id_liste_plots` column alongside
# `id_n`, but nothing in the package ever wrote it. 74 rows use it: they were
# written straight to the table on 2026-01-06, with `id_n` NULL,
# `id_liste_plots` set, `id_linktype` NULL and the free-text `type` column
# reading 'reference_plot'. They record a specimen collected somewhere in a
# plot, where the individual tree is unknown.
#
# That convention lived only in those 74 rows. This migration turns it into
# something the schema states:
#
#   1. `linktypelist.scope` - 'individual' or 'plot'. Which end of the link a
#      type points at. Every pre-existing type is 'individual'.
#   2. a `reference_plot` row in `linktypelist`, scope 'plot', priority 10.
#   3. `id_linktype` backfilled on the rows whose `type` is 'reference_plot'.
#   4. a foreign key on `id_liste_plots`, which had none.
#
# Priority 10 sits below `referenced_individual` (50) on purpose. Priority
# orders the specimen that governs an individual's determination
# (`idtax_individual_f = coalesce(idtax_specimen_f, idtax_f)`), and every one
# of those sorts filters on `id_n`, which a plot link does not have - so the
# value is inert there. It is not inert in the UI: `mod_link_preview.R`
# preselects the highest-priority type, and a plot type must never become the
# default for pairing a specimen with a tree.


.rpl_checkout <- function(con) {
  if (inherits(con, "Pool")) pool::poolCheckout(con) else con
}

.rpl_return <- function(con, actual_con) {
  if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
}


#' Add scope Column to linktypelist
#'
#' Adds `scope` ('individual' or 'plot') so that a link type declares which end
#' of the link it points at, instead of leaving it to convention.
#'
#' @param con Database connection (needs ALTER privileges on linktypelist)
#' @param dry_run If TRUE, print the SQL and change nothing
#'
#' @return TRUE if successful
#' @keywords internal
migration_add_linktype_scope <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) con <- call.mydb()
  actual_con <- .rpl_checkout(con)
  on.exit(.rpl_return(con, actual_con), add = TRUE)

  cli::cli_h1("Migration: add linktypelist.scope")

  if (!"linktypelist" %in% DBI::dbListTables(actual_con)) {
    cli::cli_alert_danger("linktypelist does not exist. Run specimen_links.R first.")
    stop("linktypelist table required")
  }

  if ("scope" %in% DBI::dbListFields(actual_con, "linktypelist")) {
    cli::cli_alert_success("Column 'scope' already exists. Nothing to do.")
    return(TRUE)
  }

  sql_add_column <- paste(
    "ALTER TABLE linktypelist",
    "ADD COLUMN IF NOT EXISTS scope VARCHAR(20) NOT NULL DEFAULT 'individual';"
  )
  sql_add_check <- paste(
    "ALTER TABLE linktypelist",
    "ADD CONSTRAINT chk_linktype_scope CHECK (scope IN ('individual', 'plot'));"
  )

  if (dry_run) {
    cli::cli_h2("Dry run - SQL that would be executed:")
    cli::cli_code(sql_add_column)
    cli::cli_code(sql_add_check)
    cli::cli_alert_info(
      "Every existing link type would take the 'individual' default, which is what they are."
    )
    return(TRUE)
  }

  tryCatch({
    DBI::dbExecute(actual_con, sql_add_column)
    cli::cli_alert_success("Column 'scope' added (default 'individual')")

    DBI::dbExecute(
      actual_con,
      "ALTER TABLE linktypelist DROP CONSTRAINT IF EXISTS chk_linktype_scope;"
    )
    DBI::dbExecute(actual_con, sql_add_check)
    cli::cli_alert_success("Check constraint chk_linktype_scope added")

    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Seed the reference_plot Link Type
#'
#' Inserts `reference_plot` into `linktypelist` (scope 'plot', priority 10).
#'
#' @param con Database connection
#' @param dry_run If TRUE, print the SQL and change nothing
#'
#' @return TRUE if successful
#' @keywords internal
migration_seed_reference_plot <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) con <- call.mydb()
  actual_con <- .rpl_checkout(con)
  on.exit(.rpl_return(con, actual_con), add = TRUE)

  cli::cli_h1("Migration: seed reference_plot link type")

  has_scope <- "scope" %in% DBI::dbListFields(actual_con, "linktypelist")
  if (!has_scope && !dry_run) {
    cli::cli_alert_danger("linktypelist.scope missing. Run migration_add_linktype_scope() first.")
    stop("linktypelist.scope required")
  }

  existing <- DBI::dbGetQuery(
    actual_con,
    "SELECT * FROM linktypelist WHERE linktype = 'reference_plot'"
  )
  if (nrow(existing) > 0) {
    cli::cli_alert_success(
      "'reference_plot' already present (id_linktype {existing$id_linktype[1]}, priority {existing$priority[1]}). Nothing to do."
    )
    return(TRUE)
  }

  sql_insert <- paste(
    "INSERT INTO linktypelist (linktype, description, priority, scope) VALUES",
    "('reference_plot',",
    "'Specimen collected within this plot; the individual tree is not identified',",
    "10, 'plot');"
  )

  if (dry_run) {
    cli::cli_h2("Dry run - SQL that would be executed:")
    cli::cli_code(sql_insert)
    return(TRUE)
  }

  tryCatch({
    DBI::dbExecute(actual_con, sql_insert)
    seeded <- DBI::dbGetQuery(
      actual_con,
      "SELECT id_linktype, linktype, priority, scope FROM linktypelist WHERE linktype = 'reference_plot'"
    )
    cli::cli_alert_success(
      "Seeded 'reference_plot' (id_linktype {seeded$id_linktype[1]}, priority {seeded$priority[1]}, scope {seeded$scope[1]})"
    )
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Backfill id_linktype on reference_plot Links
#'
#' The 74 pre-existing plot links carry the string 'reference_plot' in the
#' legacy free-text `type` column and a NULL `id_linktype`. This points them at
#' the seeded lookup row.
#'
#' @param con Database connection
#' @param dry_run If TRUE, report what would change and change nothing
#'
#' @return TRUE if successful
#' @keywords internal
migration_backfill_reference_plot_links <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) con <- call.mydb()
  actual_con <- .rpl_checkout(con)
  on.exit(.rpl_return(con, actual_con), add = TRUE)

  cli::cli_h1("Migration: backfill id_linktype on reference_plot links")

  candidates <- DBI::dbGetQuery(actual_con, "
    SELECT
      COUNT(*)                                    AS n_total,
      COUNT(*) FILTER (WHERE id_n IS NULL)        AS n_sans_individu,
      COUNT(id_liste_plots)                       AS n_avec_plot,
      COUNT(*) FILTER (WHERE id_linktype IS NULL) AS n_sans_linktype
    FROM data_link_specimens
    WHERE type = 'reference_plot'
  ")

  cli::cli_alert_info("Rows with type = 'reference_plot': {candidates$n_total[1]}")
  cli::cli_li("without an individual: {candidates$n_sans_individu[1]}")
  cli::cli_li("with a plot: {candidates$n_avec_plot[1]}")
  cli::cli_li("without id_linktype: {candidates$n_sans_linktype[1]}")

  # A reference_plot row that carries an individual, or that carries no plot,
  # is not what the type means.
  #
  # This used to warn and stamp them anyway, and on 2026-09-01 that is exactly
  # what it did: 443 of the 517 rows labelled 'reference_plot' carried an id_n
  # and no plot - one specimen serving as the identification reference for
  # several trees of a plot, which is `referenced_individual`, not a plot link.
  # They were given a plot-scope type while holding an id_n, the combination
  # `.check_link_scope()` rejects. `reference_plot_mistyped_links.R` repaired
  # it. The phase now refuses rather than repeating the mistake on a restored
  # backup.
  anomalies <- DBI::dbGetQuery(actual_con, "
    SELECT id_link_specimens, id_specimen, id_n, id_liste_plots
    FROM data_link_specimens
    WHERE type = 'reference_plot'
      AND (id_n IS NOT NULL OR id_liste_plots IS NULL)
    ORDER BY id_link_specimens
  ")
  if (nrow(anomalies) > 0) {
    cli::cli_alert_danger(
      "{nrow(anomalies)} 'reference_plot' row(s) carry an individual or no plot:"
    )
    print(utils::head(anomalies, 20))
    cli::cli_alert_info(
      "These are not plot links. Sort them out first - see reference_plot_mistyped_links.R - then run this phase again."
    )
    return(FALSE)
  }

  if (candidates$n_sans_linktype[1] == 0) {
    cli::cli_alert_success("Every reference_plot row already has id_linktype. Nothing to do.")
    return(TRUE)
  }

  sql_backfill <- "
    UPDATE data_link_specimens
    SET id_linktype = (SELECT id_linktype FROM linktypelist WHERE linktype = 'reference_plot')
    WHERE type = 'reference_plot' AND id_linktype IS NULL;
  "

  if (dry_run) {
    cli::cli_h2("Dry run - SQL that would be executed:")
    cli::cli_code(sql_backfill)
    cli::cli_alert_info("{candidates$n_sans_linktype[1]} row(s) would be updated.")

    remaining <- DBI::dbGetQuery(actual_con, "
      SELECT type, COUNT(*) AS n
      FROM data_link_specimens
      WHERE id_linktype IS NULL
      GROUP BY type ORDER BY n DESC
    ")
    if (nrow(remaining) > 0) {
      cli::cli_alert_info("Rows that would still have a NULL id_linktype afterwards:")
      print(remaining)
    }
    return(TRUE)
  }

  tryCatch({
    seeded <- DBI::dbGetQuery(
      actual_con,
      "SELECT id_linktype FROM linktypelist WHERE linktype = 'reference_plot'"
    )
    if (nrow(seeded) == 0) {
      cli::cli_alert_danger(
        "'reference_plot' not in linktypelist. Run migration_seed_reference_plot() first."
      )
      stop("reference_plot link type required")
    }

    n <- DBI::dbExecute(actual_con, sql_backfill)
    cli::cli_alert_success("Backfilled id_linktype on {n} row(s)")
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Add the Missing Foreign Key on id_liste_plots
#'
#' `data_link_specimens.id_liste_plots` had no constraint - only `fk_id_n` and
#' `fk_linktype` existed - so nothing guaranteed it pointed at a real plot.
#' Refuses to add the key while orphan rows exist.
#'
#' @param con Database connection
#' @param dry_run If TRUE, report orphans and change nothing
#'
#' @return TRUE if the key was added (or already present), FALSE if orphans block it
#' @keywords internal
migration_add_liste_plots_fk <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) con <- call.mydb()
  actual_con <- .rpl_checkout(con)
  on.exit(.rpl_return(con, actual_con), add = TRUE)

  cli::cli_h1("Migration: foreign key on data_link_specimens.id_liste_plots")

  already <- DBI::dbGetQuery(actual_con, "
    SELECT conname FROM pg_constraint
    WHERE conrelid = 'data_link_specimens'::regclass AND conname = 'fk_id_liste_plots'
  ")
  if (nrow(already) > 0) {
    cli::cli_alert_success("Constraint fk_id_liste_plots already exists. Nothing to do.")
    return(TRUE)
  }

  orphans <- DBI::dbGetQuery(actual_con, "
    SELECT ls.id_link_specimens, ls.id_specimen, ls.id_liste_plots
    FROM data_link_specimens ls
    LEFT JOIN data_liste_plots p ON ls.id_liste_plots = p.id_liste_plots
    WHERE ls.id_liste_plots IS NOT NULL AND p.id_liste_plots IS NULL
    ORDER BY ls.id_link_specimens
  ")

  if (nrow(orphans) > 0) {
    cli::cli_alert_danger(
      "{nrow(orphans)} link(s) point at a plot that does not exist - the constraint would fail:"
    )
    print(utils::head(orphans, 20))
    cli::cli_alert_info("Resolve these before adding the key.")
    return(FALSE)
  }
  cli::cli_alert_success("No orphan plot references")

  sql_add_fk <- paste(
    "ALTER TABLE data_link_specimens",
    "ADD CONSTRAINT fk_id_liste_plots",
    "FOREIGN KEY (id_liste_plots) REFERENCES data_liste_plots(id_liste_plots);"
  )
  sql_add_index <- paste(
    "CREATE INDEX IF NOT EXISTS idx_data_link_specimens_liste_plots",
    "ON data_link_specimens(id_liste_plots);"
  )

  if (dry_run) {
    cli::cli_h2("Dry run - SQL that would be executed:")
    cli::cli_code(sql_add_fk)
    cli::cli_code(sql_add_index)
    return(TRUE)
  }

  tryCatch({
    DBI::dbExecute(actual_con, sql_add_fk)
    cli::cli_alert_success("Constraint fk_id_liste_plots added")
    DBI::dbExecute(actual_con, sql_add_index)
    cli::cli_alert_success("Index idx_data_link_specimens_liste_plots created")
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Run the Full reference_plot Migration
#'
#' Runs all four phases in order.
#'
#' @param con Database connection
#' @param dry_run If TRUE (the default), rehearse without changing anything
#'
#' @return List with the result of each phase
#' @keywords internal
run_reference_plot_linktype_migration <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) con <- call.mydb()

  cli::cli_h1("Full reference_plot link type migration")
  if (dry_run) cli::cli_alert_warning("DRY RUN - no changes will be made")

  results <- list()
  results$scope    <- migration_add_linktype_scope(con, dry_run = dry_run)
  results$seed     <- migration_seed_reference_plot(con, dry_run = dry_run)
  results$backfill <- migration_backfill_reference_plot_links(con, dry_run = dry_run)
  results$fk       <- migration_add_liste_plots_fk(con, dry_run = dry_run)

  cli::cli_h1("Migration complete")
  if (isFALSE(results$backfill)) {
    cli::cli_alert_warning(
      "The backfill was refused - rows labelled 'reference_plot' are not all plot links."
    )
  }
  if (isFALSE(results$fk)) {
    cli::cli_alert_warning(
      "The foreign key was not added - orphan plot references need resolving first."
    )
  }

  results
}


#' Verify the reference_plot Migration
#'
#' Checks the state of the schema and of the plot-level links.
#'
#' @param con Database connection
#'
#' @return A one-row data frame of checks
#' @keywords internal
verify_reference_plot_linktype_migration <- function(con = NULL) {
  if (is.null(con)) con <- call.mydb()
  actual_con <- .rpl_checkout(con)
  on.exit(.rpl_return(con, actual_con), add = TRUE)

  cli::cli_h1("Verifying reference_plot link type migration")
  checks <- list()

  checks$scope_column <- "scope" %in% DBI::dbListFields(actual_con, "linktypelist")
  if (checks$scope_column) {
    cli::cli_alert_success("linktypelist.scope exists")
    print(DBI::dbGetQuery(
      actual_con,
      "SELECT id_linktype, linktype, priority, scope FROM linktypelist ORDER BY priority DESC"
    ))
  } else {
    cli::cli_alert_danger("linktypelist.scope missing")
  }

  seeded <- DBI::dbGetQuery(
    actual_con,
    "SELECT id_linktype, priority, scope FROM linktypelist WHERE linktype = 'reference_plot'"
  )
  checks$reference_plot_seeded <- nrow(seeded) > 0
  if (checks$reference_plot_seeded) {
    cli::cli_alert_success(
      "'reference_plot' present (priority {seeded$priority[1]}, scope {seeded$scope[1]})"
    )
  } else {
    cli::cli_alert_danger("'reference_plot' not in linktypelist")
  }

  stats <- DBI::dbGetQuery(actual_con, "
    SELECT
      COUNT(*)                                    AS n_plot_links,
      COUNT(*) FILTER (WHERE id_linktype IS NULL) AS n_sans_linktype
    FROM data_link_specimens
    WHERE type = 'reference_plot'
  ")
  checks$n_plot_links <- stats$n_plot_links[1]
  checks$n_missing_linktype <- stats$n_sans_linktype[1]
  cli::cli_alert_info("reference_plot links: {checks$n_plot_links}")
  if (checks$n_missing_linktype > 0) {
    cli::cli_alert_warning("{checks$n_missing_linktype} still without id_linktype")
  } else {
    cli::cli_alert_success("All reference_plot links carry id_linktype")
  }

  fk <- DBI::dbGetQuery(actual_con, "
    SELECT conname FROM pg_constraint
    WHERE conrelid = 'data_link_specimens'::regclass AND conname = 'fk_id_liste_plots'
  ")
  checks$fk_liste_plots <- nrow(fk) > 0
  if (checks$fk_liste_plots) {
    cli::cli_alert_success("fk_id_liste_plots present")
  } else {
    cli::cli_alert_warning("fk_id_liste_plots absent")
  }

  checks$migration_complete <- all(c(
    checks$scope_column, checks$reference_plot_seeded,
    checks$n_missing_linktype == 0, checks$fk_liste_plots
  ))
  if (checks$migration_complete) {
    cli::cli_alert_success("Migration is complete")
  } else {
    cli::cli_alert_warning("Migration is incomplete")
  }

  as.data.frame(checks)
}
