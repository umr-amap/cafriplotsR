# PENDING MIGRATION - written, not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what is done to the database stays readable.
# It implements step 1 of Phase 4 in inst/docs/migration_plan_multi_backbone.md.
# It needs multi_backbone.R to have been applied first.
#
# To run it (taxa database):
#   source(system.file("migrations", "apd_backbone.R", package = "CafriplotsR"))
#   con_taxa <- CafriplotsR::call.mydb.taxa()
#   migrate_apd_backbone(con_taxa)                   # rehearsal: prints, changes nothing
#   migrate_apd_backbone(con_taxa, dry_run = FALSE)  # apply
#   check_apd_backbone_migration(con_taxa)


#' Migration: room for the African Plant Database
#'
#' Creates the empty mirror of the African Plant Database (APD, Conservatoire
#' et Jardin botaniques de Geneve) and registers it as a backbone:
#'
#' - `apd_names`: APD's export columns, the three ID columns renamed, plus the
#'   canonical fields derived at import;
#' - `v_backbone_names_apd`: the same canonical columns as
#'   `v_backbone_names_wcvp`;
#' - a `backbone_list` row with `is_name_source = false`.
#'
#' @details
#' **Loads no data.** Filling `apd_names` is the job of the importer, which is
#' re-run at every APD export and therefore belongs in `R/`, not here.
#'
#' **Not offered to users yet.** `is_name_source` stays `false` until the links
#' have been matched and reviewed; then
#' `UPDATE backbone_list SET is_name_source = true WHERE code = 'apd'`.
#'
#' **The three ID columns are renamed** (`ID` to `apd_id`, `idtax_good_n` to
#' `apd_accepted_id`, `id_PARENT` to `apd_parent_id`) so that no column in the
#' taxa database called `idtax_good_n` holds an ID that is not ours.
#'
#' **Synonymy.** `apd_accepted_id` is the authority whatever `taxon_status`
#' says, so the view passes it through unchanged. It has no foreign key: 326
#' pointers of the 2026-07-29 export target IDs absent from the export.
#'
#' **Not imported:** `STATUT_SYN`, which adds nothing to `taxon_status` and
#' `idtax_good_n`.
#'
#' @param con_taxa Connection (or pool) to the taxa database, with rights to
#'   create tables and grant privileges.
#' @param dry_run If `TRUE` (the default), print the statements without
#'   changing anything.
#' @return Invisibly `TRUE`.
#'
#' @examples
#' \dontrun{
#' con_taxa <- call.mydb.taxa()
#' migrate_apd_backbone(con_taxa)                   # checks only
#' migrate_apd_backbone(con_taxa, dry_run = FALSE)  # apply
#' }
#' @keywords internal
migrate_apd_backbone <- function(con_taxa, dry_run = TRUE) {

  cli::cli_h1("Migration: African Plant Database mirror")

  if (inherits(con_taxa, "Pool")) {
    con <- pool::poolCheckout(con_taxa)
    on.exit(pool::poolReturn(con), add = TRUE)
  } else {
    con <- con_taxa
  }
  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  q <- function(sql) DBI::dbGetQuery(con, sql)
  relation_exists <- function(name) {
    !is.na(q(sprintf("SELECT to_regclass('public.%s')::text AS r", name))$r)
  }

  if (!relation_exists("backbone_list")) {
    cli::cli_abort(c(
      "{.field backbone_list} does not exist.",
      "i" = "Apply {.file inst/migrations/multi_backbone.R} first."
    ))
  }

  state <- c(
    apd_names            = relation_exists("apd_names"),
    v_backbone_names_apd = relation_exists("v_backbone_names_apd"),
    backbone_list_row    = nrow(q("SELECT 1 FROM backbone_list WHERE code = 'apd'")) > 0
  )
  if (all(state)) {
    cli::cli_alert_success("Already applied.")
    cli::cli_alert_info("Run {.fn check_apd_backbone_migration} to verify it.")
    return(invisible(TRUE))
  }
  if (any(state)) {
    cli::cli_abort(c(
      "Partially applied: present {.field {names(state)[state]}}, missing {.field {names(state)[!state]}}.",
      "i" = "This migration runs in one transaction, so it did not leave this state. Inspect before going further."
    ))
  }

  # intToUtf8(232) is the accented e, kept out of the source to leave it ASCII
  publisher <- paste0("Conservatoire et Jardin botaniques de Gen",
                      intToUtf8(232), "ve")

  statements <- c(
    "Create apd_names" = "
CREATE TABLE apd_names (
  apd_id            integer PRIMARY KEY,
  apd_accepted_id   integer,
  apd_parent_id     integer,
  taxon_name        text NOT NULL,
  nom_standard      text,
  tax_level         text,
  taxrank           text,
  tax_famclass      text,
  fk_famille        text,
  tax_gen           text,
  tax_esp           text,
  author1           text,
  author2           text,
  taxon_status      text,
  citation          text,
  year_description  integer,
  date_modification timestamp,
  family            text,
  species           text,
  infra_rank        text,
  infra_epithet     text,
  authors           text,
  apd_version       text NOT NULL
)",

    "Index APD IDs as text" = "
CREATE INDEX idx_apd_names_id_text ON apd_names ((apd_id::text))",

    "Index APD accepted IDs as text" = "
CREATE INDEX idx_apd_names_accepted_text ON apd_names ((apd_accepted_id::text))",

    "Index APD names" = "
CREATE INDEX idx_apd_names_taxon_name ON apd_names (taxon_name)",

    "Index APD genera" = "
CREATE INDEX idx_apd_names_genus ON apd_names (tax_gen)",

    "Create v_backbone_names_apd" = "
CREATE VIEW v_backbone_names_apd AS
SELECT apd_id::text          AS external_id,
       apd_accepted_id::text AS accepted_external_id,
       taxon_name,
       family,
       tax_gen               AS genus,
       species,
       infra_rank,
       infra_epithet,
       authors,
       tax_level             AS rank,
       CASE taxon_status
         WHEN 'Accepted' THEN 'accepted'
         WHEN 'Synonyme' THEN 'synonym'
         ELSE 'other'
       END                   AS status,
       taxon_status          AS status_raw
  FROM apd_names",

    "Register APD, not yet offered to users" = sprintf("
INSERT INTO backbone_list (code, name, publisher, names_view, is_name_source)
VALUES ('apd', 'African Plant Database', %s, 'v_backbone_names_apd', false)",
      DBI::dbQuoteString(con, publisher)),

    "Grant read access" = "
GRANT SELECT ON apd_names, v_backbone_names_apd TO PUBLIC"
  )

  cli::cli_h2("Statements")
  for (i in seq_along(statements)) {
    cli::cli_text("{.strong {names(statements)[i]}}")
    cat(trimws(statements[[i]]), "\n\n")
  }

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed. Re-run with {.code dry_run = FALSE}.")
    return(invisible(TRUE))
  }

  DBI::dbBegin(con)
  ok <- tryCatch({
    for (i in seq_along(statements)) {
      cli::cli_alert_info("Executing: {names(statements)[i]}")
      DBI::dbExecute(con, statements[[i]])
    }
    DBI::dbCommit(con)
    TRUE
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Migration rolled back: {conditionMessage(e)}")
    FALSE
  })
  if (!ok) stop("Migration failed - no change was committed.", call. = FALSE)

  cli::cli_alert_success("Migration committed")
  check_apd_backbone_migration(con)
  cli::cli_h2("Next")
  cli::cli_ol(c(
    "Import an export with the APD importer (in R/, see the plan, Phase 4).",
    "Match internal taxa to APD, review, save the links.",
    "Offer APD to users: UPDATE backbone_list SET is_name_source = true WHERE code = 'apd'.",
    "Record this migration in the status table of inst/migrations/README.md."
  ))
  invisible(TRUE)
}


#' Check the APD backbone migration
#'
#' Verifies what [migrate_apd_backbone()] should have left, and reports how
#' far APD has got after it (import, links, offered to users). Read-only.
#'
#' @param con_taxa Connection (or pool) to the taxa database.
#' @return Invisibly, a data frame with columns `check`, `ok` (`NA` for
#'   information only) and `detail`.
#' @keywords internal
check_apd_backbone_migration <- function(con_taxa) {

  cli::cli_h1("Check: African Plant Database mirror")

  if (inherits(con_taxa, "Pool")) {
    con <- pool::poolCheckout(con_taxa)
    on.exit(pool::poolReturn(con), add = TRUE)
  } else {
    con <- con_taxa
  }

  q <- function(sql) DBI::dbGetQuery(con, sql)
  relation_exists <- function(name) {
    !is.na(q(sprintf("SELECT to_regclass('public.%s')::text AS r", name))$r)
  }

  results <- data.frame(check = character(0), ok = logical(0),
                        detail = character(0), stringsAsFactors = FALSE)
  record <- function(check, ok, detail) {
    results <<- rbind(results, data.frame(check = check, ok = ok,
                                          detail = detail,
                                          stringsAsFactors = FALSE))
  }
  report <- function() {
    for (i in seq_len(nrow(results))) {
      line <- paste0(results$check[i], ": ", results$detail[i])
      if (is.na(results$ok[i])) cli::cli_alert_info("{line}")
      else if (results$ok[i]) cli::cli_alert_success("{line}")
      else cli::cli_alert_danger("{line}")
    }
    invisible(results)
  }

  objects <- c("backbone_list", "apd_names", "v_backbone_names_apd")
  present <- vapply(objects, relation_exists, logical(1))
  record("objects", all(present),
         if (all(present)) "all exist"
         else paste("missing:", paste(objects[!present], collapse = ", ")))
  if (!all(present)) return(report())

  apd <- q("SELECT id_backbone, names_view, is_name_source
              FROM backbone_list WHERE code = 'apd'")
  record("APD registered", nrow(apd) == 1 && apd$names_view == "v_backbone_names_apd",
         if (nrow(apd) == 1) sprintf("view %s", apd$names_view)
         else "no row with code 'apd' in backbone_list")
  if (nrow(apd) != 1) return(report())

  canonical <- c("external_id", "accepted_external_id", "taxon_name", "family",
                 "genus", "species", "infra_rank", "infra_epithet", "authors",
                 "rank", "status", "status_raw")
  view_cols <- q("
    SELECT column_name, data_type FROM information_schema.columns
     WHERE table_schema = 'public' AND table_name = 'v_backbone_names_apd'
     ORDER BY ordinal_position")
  record("view columns", identical(view_cols$column_name, canonical),
         paste(view_cols$column_name, collapse = ", "))
  record("view column types", all(view_cols$data_type == "text"),
         paste(unique(view_cols$data_type), collapse = ", "))

  n_names <- q("SELECT count(*)::int AS n FROM apd_names")$n
  record("names imported", NA, sprintf("%d row(s) in apd_names", n_names))

  current <- q(sprintf("
    SELECT version, record_count FROM backbone_import
     WHERE id_backbone = %d AND is_current", apd$id_backbone))
  record("current import", NA,
         if (nrow(current) == 1)
           sprintf("version %s, %d record(s)", current$version, current$record_count)
         else "none yet")

  links <- q(sprintf("
    SELECT count(*)::int AS n,
           (count(*) FILTER (WHERE is_preferred))::int AS n_preferred,
           (count(*) FILTER (WHERE verified))::int AS n_verified
      FROM taxa_backbone_link WHERE id_backbone = %d", apd$id_backbone))
  record("links", NA,
         sprintf("%d link(s), %d preferred, %d verified",
                 links$n, links$n_preferred, links$n_verified))

  record("offered to users", NA,
         if (isTRUE(apd$is_name_source)) "yes" else "not yet (is_name_source = false)")

  report()
}
