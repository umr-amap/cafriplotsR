#' Migration: Record which taxon a revised identification moved between
#'
#' `followup_updates_individuals` already carries 4,440 rows with
#' `modif_type = 'idtax_n'`, the most recent from 2026-06-19 — so identification
#' changes have been logged for years. What has never been logged is *what the
#' identification changed from and to*: the table is a mirror of the old
#' `data_individuals` schema, dating from before `idtax_n` existed, and it has
#' no column able to hold a taxon id. Those 4,440 rows record that a
#' determination moved without recording where it moved.
#'
#' @details
#' Three columns are added:
#'
#' \describe{
#'   \item{`idtax_n`}{the taxon **before** the change, matching the table's
#'     snapshot-of-previous-state convention.}
#'   \item{`idtax_n_new`}{the taxon **after** it.}
#'   \item{`created_by`}{who made the change, defaulting to `current_user`, as
#'     `data_link_specimens` already does.}
#' }
#'
#' A pure mirror can express a state but not a transition, which is exactly why
#' the existing rows are unreadable. Holding both ends makes each row
#' self-describing: the trail can be read without joining back to
#' `data_individuals`, and it survives the individual being merged or deleted.
#'
#' Existing rows keep `NULL` in all three. Their information is not recoverable
#' and inventing it would be worse than leaving the gap visible.
#'
#' @param con Database connection with rights to ALTER the table.
#' @param dry_run If TRUE (the default), report what would happen without
#'   altering anything.
#' @return Invisibly TRUE.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' migrate_followup_idtax(con)                   # checks only
#' migrate_followup_idtax(con, dry_run = FALSE)  # apply
#' }
#' @keywords internal
migrate_followup_idtax <- function(con, dry_run = TRUE) {

  cli::cli_h1("Migration: taxon columns on followup_updates_individuals")

  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  existing <- DBI::dbGetQuery(con, "
    SELECT column_name FROM information_schema.columns
     WHERE table_schema = 'public'
       AND table_name = 'followup_updates_individuals'")$column_name

  wanted <- c("idtax_n", "idtax_n_new", "created_by")
  todo <- setdiff(wanted, existing)

  if (length(todo) == 0) {
    cli::cli_alert_success("All three columns are already present — nothing to do")
    return(invisible(TRUE))
  }
  cli::cli_alert_info("Columns to add: {todo}")

  n_idtax_rows <- DBI::dbGetQuery(con, "
    SELECT count(*) AS n FROM followup_updates_individuals
     WHERE modif_type = 'idtax_n'")$n
  cli::cli_alert_info(
    "{n_idtax_rows} existing identification change{?s} will keep NULL — their taxa were never recorded"
  )

  statements <- c(
    idtax_n     = "ALTER TABLE followup_updates_individuals ADD COLUMN IF NOT EXISTS idtax_n INTEGER",
    idtax_n_new = "ALTER TABLE followup_updates_individuals ADD COLUMN IF NOT EXISTS idtax_n_new INTEGER",
    created_by  = "ALTER TABLE followup_updates_individuals ADD COLUMN IF NOT EXISTS created_by TEXT DEFAULT current_user"
  )[todo]

  if (dry_run) {
    for (s in statements) cli::cli_alert_info("Would execute: {.code {s}}")
    cli::cli_alert_info("Dry run — nothing was altered. Re-run with {.code dry_run = FALSE}.")
    return(invisible(TRUE))
  }

  DBI::dbBegin(con)
  ok <- tryCatch({
    for (s in statements) {
      cli::cli_alert_info("Executing: {.code {s}}")
      DBI::dbExecute(con, s)
    }
    DBI::dbCommit(con)
    TRUE
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Migration rolled back: {e$message}")
    FALSE
  })
  if (!ok) stop("Migration failed — no change was committed.", call. = FALSE)

  print(DBI::dbGetQuery(con, "
    SELECT column_name, data_type FROM information_schema.columns
     WHERE table_schema = 'public'
       AND table_name = 'followup_updates_individuals'
       AND column_name IN ('idtax_n', 'idtax_n_new', 'created_by')
     ORDER BY column_name"))

  cli::cli_alert_success("Migration complete")
  invisible(TRUE)
}
