# ARCHIVED MIGRATION - applied 2026-10-13, kept for the record
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
# To run it (should not be necessary - this is one-shot):
#   source(system.file("migrations", "tag_label.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#   migrate_tag_label(con)                   # rehearsal: prints, changes nothing
#   migrate_tag_label(con, dry_run = FALSE)  # apply


#' Migration: Add a text tag_label beside the numeric tag
#'
#' Field teams tag a recruit by hanging a decimal off a stem already tagged:
#' `22` gets `22.1`, `22.2`, ... `22.9`, then `22.10`, `22.11`. On a label,
#' `22.10` and `22.1` are two trees. As numbers they are equal, and
#' `data_individuals.tag` is `numeric`, so the second one to arrive either
#' collides with the first or is silently relabelled. `numeric` keeps the
#' scale it is given, but compares `22.10 = 22.1` as true and RPostgres hands
#' both to R as the same double, so no numeric column can carry this identity.
#'
#' The damage is already in the data. In `alpicam001` the family of tag 22
#' runs `22.1 ... 22.9, 22.11 ... 22.19, 22.21 ... 22.23`: `22.10` and `22.20`
#' are absent, and the individual ids are contiguous, so the two rows were
#' dropped before insertion. In `dengdeng001` the suffix is a plot-wide
#' recruit counter (`89.11`, `87.12`, `96.13`, ... `488.27`), suffixes 10 and
#' 20 are absent and `.1` and `.2` each occur once too often - recruits #10
#' and #20 are stored under the wrong label.
#'
#' @details
#' `tag_label` holds the tag exactly as written in the field. `tag` stays: it
#' sorts, it is what roughly 780 references in `R/` read, and it is what every
#' legacy row is keyed on. Matching moves to `tag_label` in a separate change
#' to `R/`; until then nothing reads the new column.
#'
#' Backfill is `tag::text`. That is the truth for every row except the few
#' whose trailing zero was already lost, and those cannot be recovered from
#' the database - only from the field sheets. Fix them by hand afterwards.
#'
#' A `BEFORE INSERT OR UPDATE` trigger keeps the two columns coherent for
#' code that still writes `tag` alone: an insert with no label gets
#' `tag::text`, and an update that changes `tag` without touching
#' `tag_label` re-derives it. Code that writes both is left alone. The
#' trigger never makes the loss worse than today - a legacy insert of
#' `22.1` for a field `22.10` gets `"22.1"`, which is what it gets now.
#'
#' Two `CHECK` constraints keep the label honest: it is trimmed and
#' non-empty, and when it reads as a plain number it equals `tag`
#' numerically. So `"22.10"` with `tag = 22.1` is valid, `"22.10"` with
#' `tag = 22.2` is not. Alphanumeric labels (`21a`, `21bis`) are allowed and
#' unconstrained against `tag`.
#'
#' `followup_updates_individuals` gets the column too, backfilled the same
#' way, with no trigger and no constraint: it is an audit copy, and the
#' backup in `R/updates_tables_functions.R` writes only the columns the two
#' tables share, so leaving it out would quietly drop the label from the
#' audit trail.
#'
#' An index on `(id_table_liste_plots_n, tag_label)` serves the census
#' match. It is not unique: plot + tag is not unique today either (legacy
#' plots numbered tags per quadrat).
#'
#' @section Locking:
#' `ADD COLUMN` with no default is a catalogue change. The backfill rewrites
#' every tagged row of `data_individuals` (about 410,000) and its audit
#' table, under row locks, inside one transaction. A `lock_timeout` makes the
#' migration fail fast instead of queueing behind a long-running query.
#' Step 2 lists the triggers already on `data_individuals`, because the
#' backfill `UPDATE` fires every row trigger that is not limited to other
#' columns.
#'
#' @param con Database connection with rights to ALTER these tables.
#' @param dry_run If TRUE (the default), run every check and print what would
#'   be executed without altering anything.
#' @return Invisibly, a list with the pre- and post-migration fingerprints of
#'   `tag`, which must not move.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' migrate_tag_label(con)                   # checks only
#' migrate_tag_label(con, dry_run = FALSE)  # apply
#' check_tag_label_migration(con)
#' }
#' @keywords internal
migrate_tag_label <- function(con, dry_run = TRUE) {

  cli::cli_h1("Migration: data_individuals.tag_label")

  if (!DBI::dbIsValid(con)) {
    cli::cli_abort("Invalid database connection")
  }

  tables <- c("data_individuals", "followup_updates_individuals")

  # -- Step 1: current state ------------------------------------------------
  cli::cli_h2("Step 1: Current columns")

  cols <- DBI::dbGetQuery(con, "
    SELECT table_name, column_name, data_type
      FROM information_schema.columns
     WHERE table_schema = 'public'
       AND table_name IN ('data_individuals', 'followup_updates_individuals')
       AND column_name IN ('tag', 'tag_label')
     ORDER BY table_name, column_name")
  print(cols)

  has_tag <- tables %in% cols$table_name[cols$column_name == "tag"]
  if (!all(has_tag)) {
    cli::cli_abort("No tag column found on: {tables[!has_tag]}")
  }
  if (any(cols$data_type[cols$column_name == "tag"] != "numeric")) {
    cli::cli_abort(c(
      "tag is not numeric on every table.",
      i = "Run tag_to_numeric.R first: the backfill relies on tag::text being exact."
    ))
  }

  has_label <- tables %in% cols$table_name[cols$column_name == "tag_label"]
  if (all(has_label)) {
    cli::cli_alert_success("tag_label already exists on both tables - nothing to do")
    return(invisible(NULL))
  }
  if (any(has_label)) {
    cli::cli_abort(c(
      "tag_label exists on {tables[has_label]} but not {tables[!has_label]}.",
      i = "A partial earlier run? Inspect before continuing."
    ))
  }

  # -- Step 2: what the backfill touches ------------------------------------
  cli::cli_h2("Step 2: Rows to backfill, and triggers that will fire")

  # Order on each table's primary key. The audit table holds several rows per
  # id_n (84,612 rows for 71,773 individuals when this ran), so ordering on
  # id_n left ties in physical order, and the backfill UPDATE, which moves
  # every row, changed the hash without changing a value.
  pk <- c(data_individuals = "id_n",
          followup_updates_individuals = "id_fol_up_ind")
  fingerprint <- function(tbl) {
    DBI::dbGetQuery(con, sprintf("
      SELECT count(*)::numeric   AS n_rows,
             count(tag)::numeric AS n_tagged,
             md5(string_agg(%1$s::text || '=' ||
                            coalesce(tag::text, 'NULL'), ',' ORDER BY %1$s)) AS hash
        FROM %2$s", pk[[tbl]], tbl))
  }
  before <- stats::setNames(lapply(tables, fingerprint), tables)
  for (tbl in tables) {
    cli::cli_alert_info(
      "{tbl}: {before[[tbl]]$n_rows} row{?s}, {before[[tbl]]$n_tagged} tagged"
    )
  }

  # A label that would not read back as its own tag means tag::text is not
  # canonical (float noise, exponent notation) - refuse rather than store it.
  n_odd <- DBI::dbGetQuery(con, "
    SELECT count(*)::numeric AS n
      FROM data_individuals
     WHERE tag IS NOT NULL
       AND tag::text !~ '^-?[0-9]+([.][0-9]+)?$'")$n
  if (n_odd > 0) {
    cli::cli_abort("{n_odd} tag{?s} do not print as a plain decimal - inspect them first.")
  }
  cli::cli_alert_success("Every tag prints as a plain decimal")

  triggers <- DBI::dbGetQuery(con, "
    SELECT tgname AS trigger, pg_get_triggerdef(oid) AS definition
      FROM pg_trigger
     WHERE tgrelid = 'public.data_individuals'::regclass
       AND NOT tgisinternal
     ORDER BY tgname")
  if (nrow(triggers) > 0) {
    cli::cli_alert_info("Existing triggers on data_individuals (check none rewrites on any UPDATE):")
    for (i in seq_len(nrow(triggers))) {
      cli::cli_text("  {.code {triggers$definition[i]}}")
    }
  } else {
    cli::cli_alert_info("No user trigger on data_individuals")
  }

  # -- Step 3: the change ---------------------------------------------------
  cli::cli_h2("Step 3: Statements")

  statements <- c(
    "ALTER TABLE data_individuals ADD COLUMN tag_label text",
    "ALTER TABLE followup_updates_individuals ADD COLUMN tag_label text",

    "COMMENT ON COLUMN data_individuals.tag_label IS
       'Tag exactly as written in the field (22.10 is not 22.1). tag holds its numeric value for sorting.'",

    "UPDATE data_individuals
        SET tag_label = tag::text
      WHERE tag IS NOT NULL",
    "UPDATE followup_updates_individuals
        SET tag_label = tag::text
      WHERE tag IS NOT NULL",

    "ALTER TABLE data_individuals
       ADD CONSTRAINT tag_label_trimmed
       CHECK (tag_label IS NULL OR (tag_label = btrim(tag_label) AND tag_label <> ''))",
    "ALTER TABLE data_individuals
       ADD CONSTRAINT tag_label_matches_tag
       CHECK (CASE WHEN tag_label ~ '^[0-9]+([.][0-9]+)?$'
                   THEN tag_label::numeric = tag
                   ELSE true END)",

    "CREATE OR REPLACE FUNCTION data_individuals_tag_label()
       RETURNS trigger
       LANGUAGE plpgsql
       SET search_path = pg_catalog, public
     AS $fn$
     BEGIN
       IF TG_OP = 'INSERT' THEN
         IF NEW.tag_label IS NULL AND NEW.tag IS NOT NULL THEN
           NEW.tag_label := NEW.tag::text;
         END IF;
       ELSIF NEW.tag IS DISTINCT FROM OLD.tag
             AND NEW.tag_label IS NOT DISTINCT FROM OLD.tag_label THEN
         NEW.tag_label := NEW.tag::text;
       END IF;
       RETURN NEW;
     END
     $fn$",
    "CREATE TRIGGER trg_data_individuals_tag_label
       BEFORE INSERT OR UPDATE OF tag, tag_label ON data_individuals
       FOR EACH ROW EXECUTE FUNCTION data_individuals_tag_label()",

    "CREATE INDEX idx_data_individuals_plot_tag_label
       ON data_individuals (id_table_liste_plots_n, tag_label)"
  )

  if (dry_run) {
    for (s in statements) cli::cli_alert_info("Would execute: {.code {s}}")
    cli::cli_alert_info("Dry run - nothing was altered. Re-run with {.code dry_run = FALSE}.")
    return(invisible(list(before = before, after = NULL)))
  }

  DBI::dbExecute(con, "SET lock_timeout = '30s'")

  DBI::dbBegin(con)
  ok <- tryCatch({
    for (s in statements) {
      cli::cli_alert_info("Executing: {.code {strsplit(s, '\n')[[1]][1]}}")
      DBI::dbExecute(con, s)
    }
    DBI::dbCommit(con)
    TRUE
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Migration rolled back: {e$message}")
    FALSE
  })
  if (!ok) stop("Migration failed - no change was committed.", call. = FALSE)

  # -- Step 4: confirm ------------------------------------------------------
  cli::cli_h2("Step 4: Verifying the result")

  after <- stats::setNames(lapply(tables, fingerprint), tables)
  for (tbl in tables) {
    same <- identical(before[[tbl]]$hash, after[[tbl]]$hash) &&
      identical(before[[tbl]]$n_rows, after[[tbl]]$n_rows)
    if (same) {
      cli::cli_alert_success("{tbl}: tag fingerprint unchanged")
    } else {
      cli::cli_alert_danger("{tbl}: tag fingerprint CHANGED - investigate before anything else.")
    }
  }

  check_tag_label_migration(con)
  invisible(list(before = before, after = after))
}


#' Check that the tag_label migration is in place
#'
#' Read-only. Reports the column on both tables, the two constraints, the
#' trigger and the index, and counts tagged rows left without a label.
#'
#' @param con Database connection.
#' @return Invisibly, TRUE when every check passes.
#' @keywords internal
check_tag_label_migration <- function(con) {

  cli::cli_h2("tag_label migration status")

  cols <- DBI::dbGetQuery(con, "
    SELECT table_name
      FROM information_schema.columns
     WHERE table_schema = 'public'
       AND table_name IN ('data_individuals', 'followup_updates_individuals')
       AND column_name = 'tag_label'")$table_name

  checks <- c(
    column_main     = "data_individuals" %in% cols,
    column_followup = "followup_updates_individuals" %in% cols
  )

  if (checks[["column_main"]]) {
    constraints <- DBI::dbGetQuery(con, "
      SELECT conname, convalidated
        FROM pg_constraint
       WHERE conrelid = 'public.data_individuals'::regclass
         AND conname IN ('tag_label_trimmed', 'tag_label_matches_tag')")
    checks[["constraints"]] <- nrow(constraints) == 2 && all(constraints$convalidated)

    checks[["trigger"]] <- DBI::dbGetQuery(con, "
      SELECT count(*)::numeric AS n
        FROM pg_trigger
       WHERE tgrelid = 'public.data_individuals'::regclass
         AND tgname = 'trg_data_individuals_tag_label'")$n == 1

    checks[["index"]] <- DBI::dbGetQuery(con, "
      SELECT count(*)::numeric AS n
        FROM pg_indexes
       WHERE tablename = 'data_individuals'
         AND indexname = 'idx_data_individuals_plot_tag_label'")$n == 1

    n_missing <- DBI::dbGetQuery(con, "
      SELECT count(*)::numeric AS n
        FROM data_individuals
       WHERE tag IS NOT NULL AND tag_label IS NULL")$n
    checks[["backfilled"]] <- n_missing == 0
    if (n_missing > 0) {
      cli::cli_alert_warning("{n_missing} tagged row{?s} without a tag_label")
    }
  }

  for (nm in names(checks)) {
    if (checks[[nm]]) cli::cli_alert_success(nm) else cli::cli_alert_danger(nm)
  }
  invisible(all(checks))
}
