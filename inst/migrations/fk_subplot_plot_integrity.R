# MIGRATION - not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
#   source(system.file("migrations", "fk_subplot_plot_integrity.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#   migrate_fk_subplot_plot(con)                   # rehearsal: prints, changes nothing
#   migrate_fk_subplot_plot(con, dry_run = FALSE)  # apply


#' Migration: constrain the two plot columns that have no foreign key
#'
#' `data_liste_sub_plots.id_table_liste_plots` and
#' `data_traits_measures.id_table_liste_plots` both name a plot and neither has
#' a foreign key. Nothing has been stopping either from holding an id that does
#' not exist, and on 2026-09-28 the first held **867** such rows across 193
#' missing plot ids, plus 23 more with no id at all.
#'
#' This is the fix that makes the cause moot. Whatever produced those rows — and
#' reading the code did not identify it, see below — a constrained column refuses
#' the write at the moment it happens instead of leaving a row that no query,
#' policy or person can attribute.
#'
#' @details
#' **What was ruled out, so the next person does not repeat it.** Plot deletion
#' is not the cause: `safe_delete_plot()` is the only path that deletes from
#' `data_liste_plots`, and it removes subplots *before* plots, so a failure part
#' way strands plots without subplots — the opposite of what was found. (It is
#' worth knowing that it is not atomic: `batch_delete_ids()` commits per batch,
#' so that opposite orphan is reachable.) The plot import is not the cause
#' either: `import_plot_metadata()` holds one transaction over both the plot
#' insert and the `add_subplot_features()` calls, and passes its checked-out
#' connection down, so a rollback takes both.
#'
#' That leaves a path not visible in the package: most likely
#' `add_subplot_features()` called directly with plot ids from a spreadsheet that
#' did not match the database. The 867 rows carry no `original_subplot_name` and
#' their ids fall in contiguous blocks — 1998-2000 with 44 subplots each,
#' 2453-2473 with 10 — which reads as one bulk operation in 2025. The
#' `date_modif_*` and `id_colnam` columns on those rows would date it and name
#' who ran it, if that matters after the constraint is in place.
#'
#' @section Why NO ACTION and not CASCADE:
#' `ON DELETE NO ACTION` matches `fk_id_table_liste_plots_n` on
#' `data_individuals` and every other plot foreign key here. It means deleting a
#' plot that still has subplots *fails*, which is the point: the cascade should
#' stay explicit in `safe_delete_plot()`, where it is visible and counted, rather
#' than becoming a silent side effect of a `DELETE`.
#'
#' @section Locking:
#' Added `NOT VALID` first, then validated in a second statement.
#' `ADD CONSTRAINT` alone takes `SHARE ROW EXCLUSIVE` on both tables and scans
#' the referencing table to validate, which on the 2 M-row
#' `data_traits_measures` would block writes for the duration. `NOT VALID` skips
#' the scan and takes the lock only briefly; `VALIDATE CONSTRAINT` then scans
#' under `SHARE UPDATE EXCLUSIVE`, which does not block reads or writes.
#'
#' Both referencing columns were indexed by `fk_indexes_plot_scope.R`. That
#' matters here too: without an index on the referencing side, every plot
#' deletion would scan the child table to check the constraint.
#'
#' @param con Database connection to `plots_transects`, as the table owner.
#' @param set_not_null If TRUE (the default), also make
#'   `data_liste_sub_plots.id_table_liste_plots` `NOT NULL`. A subplot without a
#'   plot is meaningless, and this closes the other half of the orphan problem —
#'   a foreign key alone still permits NULL.
#' @param dry_run If TRUE (the default), check the preconditions, print the
#'   statements and change nothing.
#' @return Invisibly, a list with the precondition counts and what was applied.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' migrate_fk_subplot_plot(con)                   # rehearse
#' migrate_fk_subplot_plot(con, dry_run = FALSE)  # apply
#' }
#'
#' @seealso `delete_orphan_plot_rows.R`, which must run first — this migration
#'   refuses while any unattributable row remains.
#' @keywords internal
migrate_fk_subplot_plot <- function(con, set_not_null = TRUE, dry_run = TRUE) {

  cli::cli_h1("Migration: foreign keys on the plot columns that lack them")

  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  targets <- data.frame(
    table_name  = c("data_liste_sub_plots", "data_traits_measures"),
    column_name = c("id_table_liste_plots", "id_table_liste_plots"),
    constraint  = c("fk_sub_plots_id_table_liste_plots",
                    "fk_traits_measures_id_table_liste_plots"),
    stringsAsFactors = FALSE
  )

  # -- Step 1: preflight -----------------------------------------------------
  cli::cli_h2("Step 1: Preflight")

  whoami <- DBI::dbGetQuery(con, "SELECT current_database() AS db, current_user AS usr")
  if (whoami$db != "plots_transects") {
    cli::cli_abort("This migration belongs to plots_transects, not {.val {whoami$db}}")
  }
  cli::cli_alert_info("Connected to {.val {whoami$db}} as {.val {whoami$usr}}")

  existing <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT con.conname, src.relname AS table_name, con.convalidated
      FROM pg_constraint con
      JOIN pg_class src ON src.oid = con.conrelid
     WHERE con.contype = 'f' AND con.conname IN ({targets$constraint*})",
    .con = con))
  if (nrow(existing) > 0) {
    print(existing, row.names = FALSE)
    if (all(existing$convalidated)) {
      cli::cli_alert_success("Already present and validated - nothing to do")
      return(invisible(list(existing = existing, applied = NULL)))
    }
    cli::cli_alert_warning(
      "Present but not validated - this run will validate {?it/them}")
  }

  # -- Step 2: preconditions -------------------------------------------------
  # A constraint cannot be validated over rows that violate it, so this is the
  # gate: delete_orphan_plot_rows.R must have run.
  cli::cli_h2("Step 2: Preconditions")

  counts <- DBI::dbGetQuery(con, "
    SELECT (SELECT count(*)::int FROM data_liste_sub_plots sp
             WHERE sp.id_table_liste_plots IS NOT NULL
               AND NOT EXISTS (SELECT 1 FROM data_liste_plots p
                                WHERE p.id_liste_plots = sp.id_table_liste_plots))
             AS sub_plots_dangling,
           (SELECT count(*)::int FROM data_liste_sub_plots
             WHERE id_table_liste_plots IS NULL) AS sub_plots_null,
           (SELECT count(*)::int FROM data_traits_measures m
             WHERE m.id_table_liste_plots IS NOT NULL
               AND NOT EXISTS (SELECT 1 FROM data_liste_plots p
                                WHERE p.id_liste_plots = m.id_table_liste_plots))
             AS measures_dangling")
  print(counts, row.names = FALSE)

  blocked <- character()
  if (counts$sub_plots_dangling > 0) {
    blocked <- c(blocked, sprintf(
      "%d data_liste_sub_plots row(s) name a plot that does not exist",
      counts$sub_plots_dangling))
  }
  if (counts$measures_dangling > 0) {
    blocked <- c(blocked, sprintf(
      "%d data_traits_measures row(s) name a plot that does not exist",
      counts$measures_dangling))
  }
  if (set_not_null && counts$sub_plots_null > 0) {
    blocked <- c(blocked, sprintf(
      "%d data_liste_sub_plots row(s) have no plot id, so NOT NULL would be rejected",
      counts$sub_plots_null))
  }

  if (length(blocked) > 0) {
    cli::cli_abort(c(
      "Preconditions not met:",
      stats::setNames(blocked, rep("x", length(blocked))),
      i = "Run {.path inst/migrations/delete_orphan_plot_rows.R} first.",
      i = "Or pass {.code set_not_null = FALSE} to add the keys and leave NULLs."
    ))
  }
  cli::cli_alert_success("No violating row - the constraints will validate")

  # The index on the referencing side is what keeps plot deletion cheap once a
  # foreign key exists; without it every delete scans the child table.
  missing_idx <- DBI::dbGetQuery(con, "
    SELECT t.relname AS table_name
      FROM (VALUES ('data_liste_sub_plots'), ('data_traits_measures')) v(n)
      JOIN pg_class t ON t.relname = v.n
      JOIN pg_namespace ns ON ns.oid = t.relnamespace AND ns.nspname = 'public'
     WHERE NOT EXISTS (
       SELECT 1 FROM pg_index i
        WHERE i.indrelid = t.oid
          AND pg_get_indexdef(i.indexrelid) LIKE '%(id_table_liste_plots)')")
  if (nrow(missing_idx) > 0) {
    cli::cli_alert_warning(
      "No index on id_table_liste_plots for: {.field {missing_idx$table_name}}. \\
       Plot deletion will scan {?it/them} to check the constraint - consider \\
       indexing before applying.")
  } else {
    cli::cli_alert_success("Both referencing columns are indexed")
  }

  # -- Step 3: the statements ------------------------------------------------
  cli::cli_h2("Step 3: Applying")

  qi <- function(x) DBI::dbQuoteIdentifier(con, x)

  add_stmts <- sprintf(
    "ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s (%s) ON DELETE NO ACTION NOT VALID",
    qi(targets$table_name), qi(targets$constraint), qi(targets$column_name),
    qi("data_liste_plots"), qi("id_liste_plots"))
  val_stmts <- sprintf("ALTER TABLE %s VALIDATE CONSTRAINT %s",
                       qi(targets$table_name), qi(targets$constraint))
  nn_stmt <- sprintf("ALTER TABLE %s ALTER COLUMN %s SET NOT NULL",
                     qi("data_liste_sub_plots"), qi("id_table_liste_plots"))

  # Skip whatever already exists, so a partial earlier run finishes cleanly.
  present <- existing$conname
  keep <- !targets$constraint %in% present
  add_stmts <- add_stmts[keep]

  statements <- c(add_stmts, val_stmts, if (set_not_null) nn_stmt)

  if (dry_run) {
    for (s in statements) cli::cli_alert_info("Would execute: {.code {s}}")
    cli::cli_alert_info(
      "Dry run - nothing was altered. Re-run with {.code dry_run = FALSE}.")
    return(invisible(list(counts = counts, applied = NULL)))
  }

  DBI::dbExecute(con, "SET lock_timeout = '30s'")

  # The ADD statements and NOT NULL go in one transaction; VALIDATE runs outside
  # it, because holding a transaction open across the validating scan of a 2 M
  # row table is what this design is trying to avoid.
  DBI::dbBegin(con)
  ok <- tryCatch({
    for (s in c(add_stmts, if (set_not_null) nn_stmt)) {
      cli::cli_alert_info("Executing: {.code {s}}")
      DBI::dbExecute(con, s)
    }
    DBI::dbCommit(con)
    TRUE
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Rolled back: {e$message}")
    FALSE
  })
  if (!ok) stop("Migration failed - no constraint was added.", call. = FALSE)
  cli::cli_alert_success("Constraints added (NOT VALID){if (set_not_null) ' and NOT NULL set' else ''}")

  for (s in val_stmts) {
    cli::cli_alert_info("Executing: {.code {s}}")
    t0 <- Sys.time()
    tryCatch({
      DBI::dbExecute(con, s)
      cli::cli_alert_success(
        "Validated in {round(as.numeric(difftime(Sys.time(), t0, units = 'secs')), 1)}s")
    }, error = function(e) {
      cli::cli_alert_danger("Validation failed: {e$message}")
      cli::cli_alert_info(
        "The constraint is in place but NOT VALID: it is enforced on new and \\
         changed rows, and existing rows were not checked. Fix the violations \\
         and re-run to validate.")
    })
  }

  # -- Step 4: verify --------------------------------------------------------
  cli::cli_h2("Step 4: Verifying")

  after <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT con.conname, src.relname AS table_name, con.convalidated,
           con.confdeltype AS on_delete
      FROM pg_constraint con
      JOIN pg_class src ON src.oid = con.conrelid
     WHERE con.contype = 'f' AND con.conname IN ({targets$constraint*})
     ORDER BY 2", .con = con))
  print(after, row.names = FALSE)

  nn <- DBI::dbGetQuery(con, "
    SELECT is_nullable FROM information_schema.columns
     WHERE table_schema='public' AND table_name='data_liste_sub_plots'
       AND column_name='id_table_liste_plots'")$is_nullable
  cli::cli_alert_info("data_liste_sub_plots.id_table_liste_plots is_nullable = {nn}")

  if (nrow(after) == nrow(targets) && all(after$convalidated)) {
    cli::cli_alert_success(
      "Both foreign keys present and validated - an orphan subplot can no \\
       longer be created, whatever writes it")
  } else {
    cli::cli_alert_warning("Not all constraints are present and validated - see above")
  }

  invisible(list(counts = counts, applied = statements, after = after))
}
