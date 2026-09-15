# PENDING MIGRATION - written, not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what is done to the database stays readable.
# It implements Phase 1 of inst/docs/migration_plan_multi_backbone.md.
#
# To run it (taxa database):
#   source(system.file("migrations", "multi_backbone.R", package = "CafriplotsR"))
#   con_taxa <- CafriplotsR::call.mydb.taxa()
#   migrate_multi_backbone(con_taxa)                   # rehearsal: reports, changes nothing
#   migrate_multi_backbone(con_taxa, dry_run = FALSE)  # apply
#   check_multi_backbone_migration(con_taxa)


#' Migration: one link table for every taxonomic backbone
#'
#' WCVP is stored as a mirror (`wcvp_names`) plus a link table
#' (`wcvp_idtax_link`) whose names are written into the package code, so a
#' third backbone would mean copying all of it. This creates the generic
#' structure the package code will read instead, and copies the WCVP state
#' into it:
#'
#' - `backbone_list`: one row per external backbone (`internal` is implicit);
#' - `backbone_import`: import history per backbone, backfilled from
#'   `wcvp_import_metadata`;
#' - `taxa_backbone_link`: `idtax_n` to external ID for every backbone,
#'   backfilled from `wcvp_idtax_link`, with a foreign key to `table_taxa`;
#' - `v_backbone_names_wcvp`: WCVP exposed under the canonical columns every
#'   backbone view shares.
#'
#' @details
#' **Additive.** `wcvp_names`, `wcvp_idtax_link` and `wcvp_import_metadata` are
#' not altered, so installed copies of the package keep working until they are
#' updated. Only two expression indexes are added to `wcvp_names`, so that the
#' view's text IDs can use an index.
#'
#' **Preferred links.** A taxon may be linked to several WCVP names (homonyms).
#' Only the preferred link supplies names. The backfill marks a link preferred
#' when it is the taxon's only one, or the only verified one among several;
#' other taxa with several links get none and keep their internal name until
#' reviewed.
#'
#' **Synonym pointers.** The view nulls the pointer of `Accepted` names and of
#' self-pointers, so that following every non-NULL pointer reproduces what the
#' package does with WCVP today.
#'
#' **Grants.** `SELECT` to `PUBLIC` on the new objects, as `setup_wcvp_schema()`
#' did. The write privileges held on `wcvp_idtax_link` by roles other than its
#' owner are repeated on `taxa_backbone_link`.
#'
#' **Refuses to apply** while any of these holds, and lists it:
#' - a linked `idtax_n` is missing from `table_taxa` (the foreign key would fail);
#' - `table_taxa.idtax_n` has no primary key or unique constraint;
#' - `wcvp_import_metadata` has more than one current row.
#'
#' Everything runs in one transaction.
#'
#' @param con_taxa Connection (or pool) to the taxa database, with rights to
#'   create tables and grant privileges.
#' @param dry_run If `TRUE` (the default), report the current state and print
#'   the statements without changing anything.
#' @param on_taxon_delete What deleting a taxon does to its links:
#'   `"cascade"` (the default) deletes them, `"restrict"` blocks the deletion.
#' @return Invisibly `TRUE`.
#'
#' @examples
#' \dontrun{
#' con_taxa <- call.mydb.taxa()
#' migrate_multi_backbone(con_taxa)                   # checks only
#' migrate_multi_backbone(con_taxa, dry_run = FALSE)  # apply
#' }
#' @keywords internal
migrate_multi_backbone <- function(con_taxa,
                                   dry_run = TRUE,
                                   on_taxon_delete = c("cascade", "restrict")) {

  on_taxon_delete <- match.arg(on_taxon_delete)
  cli::cli_h1("Migration: one link table for every taxonomic backbone")

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

  # ---- Where are we? --------------------------------------------------------

  legacy <- c("table_taxa", "wcvp_names", "wcvp_idtax_link",
              "wcvp_import_metadata")
  missing_legacy <- legacy[!vapply(legacy, relation_exists, logical(1))]
  if (length(missing_legacy) > 0) {
    cli::cli_abort(c(
      "Missing: {.field {missing_legacy}}.",
      "i" = "Run this on the taxa database ({.fn call.mydb.taxa}), where WCVP is set up."
    ))
  }

  new_objects <- c("backbone_list", "backbone_import", "taxa_backbone_link",
                   "v_backbone_names_wcvp")
  present <- vapply(new_objects, relation_exists, logical(1))
  if (all(present)) {
    cli::cli_alert_success("Already applied: all four objects exist.")
    cli::cli_alert_info("Run {.fn check_multi_backbone_migration} to verify it.")
    return(invisible(TRUE))
  }
  if (any(present)) {
    cli::cli_abort(c(
      "Partially applied: {.field {new_objects[present]}} exist{?s/}, {.field {new_objects[!present]}} do{?es/} not.",
      "i" = "This migration runs in one transaction, so it did not leave this state. Inspect before going further."
    ))
  }

  blockers <- character(0)

  # The foreign key needs idtax_n to be a single-column key of table_taxa
  taxa_key <- q("
    SELECT count(*)::int AS n
      FROM pg_constraint c
     WHERE c.conrelid = 'public.table_taxa'::regclass
       AND c.contype IN ('p', 'u')
       AND c.conkey = ARRAY[(SELECT attnum FROM pg_attribute
                              WHERE attrelid = 'public.table_taxa'::regclass
                                AND attname = 'idtax_n')]")$n
  if (taxa_key == 0) {
    blockers <- c(blockers, paste(
      "table_taxa.idtax_n has no primary key or unique constraint,",
      "so taxa_backbone_link cannot reference it."))
  }

  cli::cli_h2("Current WCVP state")

  n_names <- q("SELECT count(*)::int AS n FROM wcvp_names")$n
  cli::cli_alert_info("Names in wcvp_names: {n_names}")

  links <- q("
    SELECT match_type, verified, count(*)::int AS n
      FROM wcvp_idtax_link GROUP BY 1, 2 ORDER BY 1, 2")
  cli::cli_alert_info("Links in wcvp_idtax_link: {sum(links$n)}")
  if (nrow(links) > 0) print(links, row.names = FALSE)

  homonyms <- q("
    SELECT count(*)::int AS n_taxa,
           (count(*) FILTER (WHERE n_verified = 1))::int AS n_one_verified
      FROM (SELECT idtax_n, sum(COALESCE(verified, false)::int) AS n_verified
              FROM wcvp_idtax_link
             GROUP BY idtax_n HAVING count(*) > 1) h")
  if (homonyms$n_taxa > 0) {
    cli::cli_alert_warning("Taxa linked to several WCVP names: {homonyms$n_taxa}")
    cli::cli_bullets(c(
      " " = "exactly one verified link, made preferred: {homonyms$n_one_verified}",
      " " = "no preferred link, internal name kept until reviewed: {homonyms$n_taxa - homonyms$n_one_verified}"
    ))
  } else {
    cli::cli_alert_success("No taxon is linked to several WCVP names")
  }

  orphans <- q("
    SELECT l.idtax_n, count(*)::int AS n_links
      FROM wcvp_idtax_link l
     WHERE NOT EXISTS (SELECT 1 FROM table_taxa t WHERE t.idtax_n = l.idtax_n)
     GROUP BY l.idtax_n ORDER BY l.idtax_n")
  if (nrow(orphans) > 0) {
    cli::cli_alert_danger("Linked idtax_n missing from table_taxa: {nrow(orphans)}")
    print(orphans, row.names = FALSE)
    blockers <- c(blockers, sprintf(paste(
      "%d linked idtax_n do not exist in table_taxa, so the foreign key cannot",
      "be added. Delete or re-point those links first."), nrow(orphans)))
  } else {
    cli::cli_alert_success("Every linked idtax_n exists in table_taxa")
  }

  imports <- q("
    SELECT count(*)::int AS n,
           (count(*) FILTER (WHERE is_current))::int AS n_current
      FROM wcvp_import_metadata")
  cli::cli_alert_info("WCVP import records: {imports$n} ({imports$n_current} current)")
  if (imports$n_current > 1) {
    blockers <- c(blockers, paste(
      "wcvp_import_metadata has more than one current row; backbone_import",
      "allows one per backbone. Keep only the latest current."))
  }

  status_case <- "CASE taxon_status
              WHEN 'Accepted' THEN 'accepted'
              WHEN 'Synonym'  THEN 'synonym'
              ELSE 'other'
            END"

  cli::cli_h2("WCVP status values, as the view will normalise them")
  print(q(paste0("
    SELECT taxon_status, ", status_case, " AS status, count(*)::int AS n
      FROM wcvp_names GROUP BY 1, 2 ORDER BY 3 DESC")), row.names = FALSE)

  n_accepted_elsewhere <- q("
    SELECT count(*)::int AS n FROM wcvp_names
     WHERE taxon_status = 'Accepted'
       AND accepted_plant_name_id IS NOT NULL
       AND accepted_plant_name_id <> plant_name_id")$n
  cli::cli_alert_info(paste(
    "Accepted names pointing to another name: {n_accepted_elsewhere}",
    "(not followed today, not followed through the view)"))

  cli::cli_h2("Privileges on wcvp_idtax_link, repeated on taxa_backbone_link")
  grants <- q("
    SELECT CASE WHEN a.grantee = 0 THEN 'PUBLIC'
                ELSE pg_get_userbyid(a.grantee) END AS grantee,
           a.privilege_type
      FROM pg_class c, aclexplode(c.relacl) a
     WHERE c.oid = 'public.wcvp_idtax_link'::regclass
       AND a.grantee <> c.relowner
       AND a.privilege_type IN ('SELECT', 'INSERT', 'UPDATE', 'DELETE')
     ORDER BY 1, 2")
  if (nrow(grants) > 0) {
    print(grants, row.names = FALSE)
  } else {
    cli::cli_alert_info("Only the owner holds privileges; nothing to repeat")
  }

  role_sql <- function(role) {
    if (identical(role, "PUBLIC")) "PUBLIC"
    else as.character(DBI::dbQuoteIdentifier(con, role))
  }
  write_grants <- grants[grants$privilege_type != "SELECT", , drop = FALSE]
  by_role <- split(write_grants$privilege_type, write_grants$grantee)
  grant_statements <- vapply(names(by_role), function(role) {
    sprintf("GRANT %s ON taxa_backbone_link TO %s",
            paste(by_role[[role]], collapse = ", "), role_sql(role))
  }, character(1), USE.NAMES = FALSE)

  # ---- Statements -----------------------------------------------------------

  fk_action <- if (on_taxon_delete == "cascade") "CASCADE" else "RESTRICT"

  statements <- c(
    "Create backbone_list" = "
CREATE TABLE backbone_list (
  id_backbone    serial PRIMARY KEY,
  code           text NOT NULL UNIQUE
                 CHECK (code ~ '^[a-z][a-z0-9_]*$' AND code <> 'internal'),
  name           text NOT NULL,
  publisher      text,
  names_view     text NOT NULL,
  url_template   text,
  is_name_source boolean NOT NULL DEFAULT false
)",

    "Create backbone_import" = "
CREATE TABLE backbone_import (
  id_import      serial PRIMARY KEY,
  id_backbone    integer NOT NULL REFERENCES backbone_list(id_backbone),
  version        text NOT NULL,
  import_date    timestamptz DEFAULT CURRENT_TIMESTAMP,
  imported_by    text,
  record_count   integer,
  source_version text,
  is_current     boolean NOT NULL DEFAULT true
)",

    "One current import per backbone" = "
CREATE UNIQUE INDEX backbone_import_one_current
  ON backbone_import (id_backbone) WHERE is_current",

    "Create taxa_backbone_link" = sprintf("
CREATE TABLE taxa_backbone_link (
  idtax_n      integer NOT NULL
               REFERENCES table_taxa(idtax_n) ON DELETE %s,
  id_backbone  integer NOT NULL REFERENCES backbone_list(id_backbone),
  external_id  text    NOT NULL,
  is_preferred boolean NOT NULL DEFAULT false,
  match_type   varchar(20) NOT NULL,
  match_score  numeric(4,3),
  matched_on   timestamptz DEFAULT CURRENT_TIMESTAMP,
  matched_by   varchar(100),
  verified     boolean NOT NULL DEFAULT false,
  notes        text,
  PRIMARY KEY (idtax_n, id_backbone, external_id)
)", fk_action),

    "One preferred link per taxon and backbone" = "
CREATE UNIQUE INDEX taxa_backbone_link_one_preferred
  ON taxa_backbone_link (idtax_n, id_backbone) WHERE is_preferred",

    "Index links by external ID" = "
CREATE INDEX taxa_backbone_link_external
  ON taxa_backbone_link (id_backbone, external_id)",

    "Index WCVP IDs as text" = "
CREATE INDEX IF NOT EXISTS idx_wcvp_names_id_text
  ON wcvp_names ((plant_name_id::text))",

    "Index WCVP accepted IDs as text" = "
CREATE INDEX IF NOT EXISTS idx_wcvp_names_accepted_text
  ON wcvp_names ((accepted_plant_name_id::text))",

    "Create v_backbone_names_wcvp" = paste0("
CREATE VIEW v_backbone_names_wcvp AS
SELECT plant_name_id::text               AS external_id,
       CASE WHEN taxon_status = 'Accepted'
                 OR accepted_plant_name_id = plant_name_id THEN NULL
            ELSE accepted_plant_name_id::text
       END                               AS accepted_external_id,
       taxon_name::text                  AS taxon_name,
       family::text                      AS family,
       genus::text                       AS genus,
       species::text                     AS species,
       NULLIF(infraspecific_rank, '')::text AS infra_rank,
       NULLIF(infraspecies, '')::text    AS infra_epithet,
       taxon_authors::text               AS authors,
       taxon_rank::text                  AS rank,
       ", status_case, "                 AS status,
       taxon_status::text                AS status_raw
  FROM wcvp_names"),

    "Register WCVP" = "
INSERT INTO backbone_list (code, name, publisher, names_view, is_name_source)
VALUES ('wcvp', 'World Checklist of Vascular Plants',
        'Royal Botanic Gardens, Kew', 'v_backbone_names_wcvp', true)",

    "Copy WCVP import history" = "
INSERT INTO backbone_import
       (id_backbone, version, import_date, imported_by, record_count,
        source_version, is_current)
SELECT b.id_backbone, m.wcvp_version, m.import_date, m.imported_by,
       m.record_count, m.r_package_version, COALESCE(m.is_current, false)
  FROM wcvp_import_metadata m
  JOIN backbone_list b ON b.code = 'wcvp'",

    "Copy WCVP links" = "
INSERT INTO taxa_backbone_link
       (idtax_n, id_backbone, external_id, is_preferred, match_type,
        match_score, matched_on, matched_by, verified, notes)
SELECT l.idtax_n, b.id_backbone, l.plant_name_id::text,
       (count(*) OVER w = 1)
         OR (COALESCE(l.verified, false)
             AND sum(COALESCE(l.verified, false)::int) OVER w = 1),
       l.match_type, l.match_score, l.matched_on, l.matched_by,
       COALESCE(l.verified, false), l.notes
  FROM wcvp_idtax_link l
  JOIN backbone_list b ON b.code = 'wcvp'
WINDOW w AS (PARTITION BY l.idtax_n)",

    "Grant read access" = "
GRANT SELECT ON backbone_list, backbone_import, taxa_backbone_link,
                v_backbone_names_wcvp TO PUBLIC"
  )
  names(grant_statements) <- rep("Repeat write privileges", length(grant_statements))
  statements <- c(statements, grant_statements)

  cli::cli_h2("Statements")
  cli::cli_alert_info("Deleting a taxon will {.strong {tolower(fk_action)}} its links")
  for (i in seq_along(statements)) {
    cli::cli_text("{.strong {names(statements)[i]}}")
    cat(trimws(statements[[i]]), "\n\n")
  }

  if (length(blockers) > 0) {
    cli::cli_h2("Blocked")
    for (b in blockers) cli::cli_alert_danger("{b}")
    if (!dry_run) {
      cli::cli_abort("Nothing was changed. Resolve the points above first.")
    }
  }

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed. Re-run with {.code dry_run = FALSE}.")
    return(invisible(TRUE))
  }

  # ---- Apply ----------------------------------------------------------------

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
  check_multi_backbone_migration(con)
  cli::cli_alert_info("Record it in the status table of inst/migrations/README.md.")
  invisible(TRUE)
}


#' Check the multi-backbone migration
#'
#' Verifies what [migrate_multi_backbone()] should have left. Read-only.
#'
#' The comparison with `wcvp_idtax_link` holds right after the migration. Once
#' the package writes links to `taxa_backbone_link`, replaced links make it
#' drift, and that is expected.
#'
#' @param con_taxa Connection (or pool) to the taxa database.
#' @return Invisibly, a data frame with columns `check`, `ok` (`NA` for
#'   information only) and `detail`.
#' @keywords internal
check_multi_backbone_migration <- function(con_taxa) {

  cli::cli_h1("Check: multi-backbone link table")

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

  objects <- c("backbone_list", "backbone_import", "taxa_backbone_link",
               "v_backbone_names_wcvp")
  present <- vapply(objects, relation_exists, logical(1))
  record("objects", all(present),
         if (all(present)) "all four exist"
         else paste("missing:", paste(objects[!present], collapse = ", ")))
  if (!all(present)) return(report())

  wcvp <- q("SELECT names_view, is_name_source FROM backbone_list WHERE code = 'wcvp'")
  record("WCVP registered", nrow(wcvp) == 1,
         if (nrow(wcvp) == 1)
           sprintf("view %s, offered to users: %s", wcvp$names_view, wcvp$is_name_source)
         else "no row with code 'wcvp' in backbone_list")

  canonical <- c("external_id", "accepted_external_id", "taxon_name", "family",
                 "genus", "species", "infra_rank", "infra_epithet", "authors",
                 "rank", "status", "status_raw")
  view_cols <- q("
    SELECT column_name, data_type FROM information_schema.columns
     WHERE table_schema = 'public' AND table_name = 'v_backbone_names_wcvp'
     ORDER BY ordinal_position")
  record("view columns", identical(view_cols$column_name, canonical),
         paste(view_cols$column_name, collapse = ", "))
  record("view column types", all(view_cols$data_type == "text"),
         paste(unique(view_cols$data_type), collapse = ", "))

  fk <- q("
    SELECT confdeltype::text AS on_delete FROM pg_constraint
     WHERE conrelid = 'public.taxa_backbone_link'::regclass
       AND contype = 'f'
       AND confrelid = 'public.table_taxa'::regclass")
  record("foreign key to table_taxa", nrow(fk) == 1,
         if (nrow(fk) == 1)
           paste("on delete:", c(c = "cascade", r = "restrict", a = "no action",
                                 n = "set null", d = "set default")[fk$on_delete])
         else "absent")

  n_current <- q("
    SELECT count(*)::int AS n FROM backbone_import i
      JOIN backbone_list b USING (id_backbone)
     WHERE b.code = 'wcvp' AND i.is_current")$n
  record("current WCVP import", n_current == 1, sprintf("%d current row(s)", n_current))

  if (relation_exists("wcvp_idtax_link")) {
    not_copied <- q("
      SELECT count(*)::int AS n FROM wcvp_idtax_link l
       WHERE NOT EXISTS (
         SELECT 1 FROM taxa_backbone_link t
           JOIN backbone_list b USING (id_backbone)
          WHERE b.code = 'wcvp'
            AND t.idtax_n = l.idtax_n
            AND t.external_id = l.plant_name_id::text)")$n
    record("legacy links carried over", not_copied == 0,
           sprintf("%d link(s) of wcvp_idtax_link absent from taxa_backbone_link", not_copied))
  }

  dup_preferred <- q("
    SELECT count(*)::int AS n FROM (
      SELECT idtax_n, id_backbone FROM taxa_backbone_link
       WHERE is_preferred GROUP BY 1, 2 HAVING count(*) > 1) d")$n
  record("one preferred link at most", dup_preferred == 0,
         sprintf("%d taxon/backbone pair(s) with several", dup_preferred))

  dangling <- q("
    SELECT count(*)::int AS n FROM taxa_backbone_link t
      JOIN backbone_list b USING (id_backbone)
     WHERE b.code = 'wcvp'
       AND NOT EXISTS (SELECT 1 FROM v_backbone_names_wcvp v
                        WHERE v.external_id = t.external_id)")$n
  record("WCVP links resolve", dangling == 0,
         sprintf("%d link(s) to an ID absent from the view", dangling))

  no_preferred <- q("
    SELECT count(*)::int AS n FROM (
      SELECT idtax_n, id_backbone FROM taxa_backbone_link
       GROUP BY 1, 2 HAVING count(*) > 1 AND NOT bool_or(is_preferred)) h")$n
  record("homonyms to review", NA,
         sprintf("%d taxon/backbone pair(s) with several links and none preferred", no_preferred))

  per_backbone <- q("
    SELECT b.code, count(t.idtax_n)::int AS n
      FROM backbone_list b
      LEFT JOIN taxa_backbone_link t USING (id_backbone)
     GROUP BY b.code ORDER BY b.code")
  record("links per backbone", NA,
         paste(sprintf("%s %d", per_backbone$code, per_backbone$n), collapse = ", "))

  report()
}
