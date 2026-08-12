# ARCHIVED MIGRATION - applied, kept for the record
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
# To run one (should not be necessary - these are one-shot):
#   source(system.file("migrations", "tag_to_numeric.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()


#' Migration: Widen tag from real to numeric
#'
#' `data_individuals.tag` is a PostgreSQL `real` — single precision. It
#' represents integers exactly only up to 2^24 (16,777,216), so a tag beyond
#' that is stored as a *different* number with nothing downstream to notice:
#' `20250001::real` comes back as `20250000`. Eight and nine digit barcode and
#' RFID tags are ordinary in current inventories, which makes this a latent
#' corruption bug rather than a theoretical one.
#'
#' @details
#' `numeric` rather than `double precision`, for two reasons. It is exact for
#' integers of any length this database will ever see, and it also cleans up
#' the float noise already present in the fractional multi-stem tags — the
#' `22.1, 22.2, …` convention used in 82 plots:
#'
#' \preformatted{
#'   (22.1::real)::numeric           -> 22.1
#'   (22.1::real)::double precision  -> 22.100000381469727
#' }
#'
#' The cast is lossless on the existing data: no stored tag changes its text
#' representation. Step 1 proves that on the live table before anything is
#' altered, and refuses to continue if it does not hold.
#'
#' `followup_updates_individuals.tag` is migrated in the same transaction.
#' Leaving it as `real` would keep rounding tags in the audit copy while the
#' main table recorded them correctly.
#'
#' The R side is unaffected: RPostgres returns `real`, `double precision` and
#' `numeric` alike as R doubles, so no downstream code changes class.
#'
#' @section Locking:
#' Changing a column type rewrites the table under an `ACCESS EXCLUSIVE` lock.
#' The two tables are about 64 MB and 13 MB, so the rewrite takes seconds, but
#' no one should be writing to them meanwhile. A `lock_timeout` is set so the
#' migration fails fast instead of queueing behind a long-running query.
#'
#' @param con Database connection with rights to ALTER these tables.
#' @param dry_run If TRUE (the default), run every check and report what would
#'   happen without altering anything.
#' @return Invisibly, a list with the pre- and post-migration fingerprints.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Checks only — alters nothing
#' migrate_tag_to_numeric(con)
#'
#' # Apply it
#' migrate_tag_to_numeric(con, dry_run = FALSE)
#' }
#'
#' @seealso [.validate_tag_values()], whose precision ceiling follows the
#'   column type via [.tag_precision_limit()] and so needs no edit after this
#'   runs.
#' @keywords internal
migrate_tag_to_numeric <- function(con, dry_run = TRUE) {

  cli::cli_h1("Migration: data_individuals.tag from real to numeric")

  if (!DBI::dbIsValid(con)) {
    cli::cli_abort("Invalid database connection")
  }

  tables <- c("data_individuals", "followup_updates_individuals")

  # -- Step 1: what the columns are now -------------------------------------
  cli::cli_h2("Step 1: Current column types")

  types <- DBI::dbGetQuery(con, "
    SELECT table_name, data_type
      FROM information_schema.columns
     WHERE table_schema = 'public'
       AND table_name IN ('data_individuals', 'followup_updates_individuals')
       AND column_name = 'tag'
     ORDER BY table_name")
  print(types)

  missing <- setdiff(tables, types$table_name)
  if (length(missing) > 0) {
    cli::cli_abort("No tag column found on: {missing}")
  }

  already <- types$data_type[types$table_name == "data_individuals"] == "numeric"
  if (already) {
    cli::cli_alert_success("data_individuals.tag is already numeric — nothing to do")
    return(invisible(NULL))
  }

  # -- Step 2: prove the cast is lossless before trusting it ----------------
  cli::cli_h2("Step 2: Verifying the cast changes no stored value")

  fingerprint <- function(tbl) {
    DBI::dbGetQuery(con, sprintf("
      SELECT count(*)                                   AS n_rows,
             count(tag)                                 AS n_tagged,
             md5(string_agg(id_n::text || '=' ||
                            coalesce(tag::text, 'NULL'), ',' ORDER BY id_n)) AS hash
        FROM %s", tbl))
  }
  differs <- function(tbl) {
    DBI::dbGetQuery(con, sprintf("
      SELECT count(*) AS n
        FROM %s
       WHERE tag IS NOT NULL AND tag::text <> tag::numeric::text", tbl))$n
  }

  before <- stats::setNames(lapply(tables, fingerprint), tables)
  n_diff <- stats::setNames(vapply(tables, differs, numeric(1)), tables)

  for (tbl in tables) {
    cli::cli_alert_info(
      "{tbl}: {before[[tbl]]$n_rows} row{?s}, {before[[tbl]]$n_tagged} tagged, {n_diff[[tbl]]} value{?s} would change"
    )
  }

  if (any(n_diff > 0)) {
    cli::cli_abort(c(
      "The cast is not lossless on this data — migration refused.",
      i = "Inspect the rows where tag::text <> tag::numeric::text before proceeding."
    ))
  }
  cli::cli_alert_success("Every stored tag survives the cast unchanged")

  # -- Step 3: tags already beyond single precision -------------------------
  cli::cli_h2("Step 3: Tags already past the single-precision ceiling")

  beyond <- DBI::dbGetQuery(con, sprintf("
    SELECT count(*) AS n FROM data_individuals WHERE tag >= %f", 2^24))$n
  if (beyond > 0) {
    cli::cli_alert_warning(paste(
      "{beyond} tag{?s} sit{?s/} at or above 2^24 and may already have been",
      "rounded on insert. The migration cannot recover the original value —",
      "check them against the field records."
    ))
  } else {
    cli::cli_alert_success("No tag has reached the ceiling yet — nothing was lost")
  }

  # -- Step 4: the change ---------------------------------------------------
  cli::cli_h2("Step 4: Altering the column type")

  statements <- sprintf(
    "ALTER TABLE %s ALTER COLUMN tag TYPE numeric USING tag::numeric", tables)

  if (dry_run) {
    for (s in statements) cli::cli_alert_info("Would execute: {.code {s}}")
    cli::cli_alert_info("Dry run — nothing was altered. Re-run with {.code dry_run = FALSE}.")
    return(invisible(list(before = before, after = NULL)))
  }

  # Fail fast rather than queue behind a long query holding the table
  DBI::dbExecute(con, "SET lock_timeout = '30s'")

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

  # -- Step 5: confirm nothing moved ----------------------------------------
  cli::cli_h2("Step 5: Verifying the result")

  after <- stats::setNames(lapply(tables, fingerprint), tables)
  for (tbl in tables) {
    same <- identical(before[[tbl]]$hash, after[[tbl]]$hash) &&
      identical(before[[tbl]]$n_rows, after[[tbl]]$n_rows)
    if (same) {
      cli::cli_alert_success("{tbl}: {after[[tbl]]$n_rows} row{?s}, fingerprint unchanged")
    } else {
      cli::cli_alert_danger(paste(
        "{tbl}: fingerprint CHANGED. The data was altered by the cast —",
        "restore from backup and investigate."
      ))
    }
  }

  print(DBI::dbGetQuery(con, "
    SELECT table_name, data_type
      FROM information_schema.columns
     WHERE table_schema = 'public'
       AND table_name IN ('data_individuals', 'followup_updates_individuals')
       AND column_name = 'tag'
     ORDER BY table_name"))

  cli::cli_alert_success("Migration complete")
  invisible(list(before = before, after = after))
}

