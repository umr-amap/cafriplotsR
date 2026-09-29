# =============================================================================
# DELETE only on plots you created
#
# WHAT WAS FOUND (2026-09-28)
#
# report_plot_access_seed() showed 13,913 of 14,018 plot grants carrying write,
# and because define_user_policy(operations = "ALL") wrote _select, _update and
# _delete together, nearly all carry DELETE: nine accounts on more than 1,100
# plots, three on more than 1,900 of 2,194. Thirty-two accounts also hold the
# DELETE table privilege directly, and the six child tables have no row-level
# security at all.
#
# THE TARGET STATE
#
#   an account may delete the plots it created, and nothing else
#
# That needs both layers moved, because either one alone leaves the right
# useless or the restriction cosmetic:
#
#   1. the POLICIES. Dropping the per-account policy_<user>_delete policies
#      leaves creator_access_delete -- a global policy reading
#      `created_by = current_user`, already installed by add_created_by.R -- as
#      the only DELETE policy on data_liste_plots. Deletion becomes
#      creator-scoped by the database's own rule rather than by a plot list.
#      Without this step alexmass could still delete 2,036 plots rather than the
#      68 it imported.
#
#   2. the TABLE PRIVILEGE, revoked from everyone *except* the accounts that
#      appear in created_by. Without table-level DELETE the creator policy grants
#      nothing, so revoking from everybody would take away the right this
#      migration exists to preserve.
#
# WHAT THIS DOES NOT CLOSE, AND CANNOT YET
#
# PostgreSQL has no per-row GRANT. The six child tables have no policy, so the
# accounts that keep the table privilege - the creators - can delete individual,
# measurement and specimen rows belonging to any plot they can read, not only
# their own. Step 5 closes that by giving those tables policies keyed on
# plot_access.can_delete; until then it is a residual on a handful of accounts
# instead of a general one on thirty-two.
#
# Revoking from the creators as well is not an option: they would be able to
# delete the plot row and not its children, and the foreign keys added by
# fk_subplot_plot_integrity.R are ON DELETE NO ACTION, so the deletion would
# fail outright. The alternative that would close it early is a SECURITY DEFINER
# delete_own_plot() function - a new and much larger surface, deliberately not
# built here.
#
# WHAT STOPS WORKING
#
# Exactly the functions meant to delete, for accounts that did not create the
# plot. Checked across R/: every DELETE FROM against these seven tables lives in
# safe_delete_plot(), safe_delete_individuals(), safe_delete_individual_features()
# or safe_delete_specimen_links(). The rest target lookup tables (specimens,
# table_countries, table_colnam, traitlist), the taxa database, or
# aggregate_individual_traits(). No import or update path deletes from them.
#
# THE DURABLE HALF IS IN R/
#
# define_user_policy() no longer writes a DELETE policy or grants DELETE for
# operations = "ALL" (R/connections_db.R). Without that change the next
# define_full_access_policy() call would reopen everything this migration closes.
#
# ROLLBACK
#   The dropped policies are written to a .sql file before anything is dropped,
#   and the rehearsal prints the GRANT for every REVOKE. Both together restore
#   the previous state exactly.
#
# Run as the owner of the tables.
# =============================================================================


#' Who can delete what, today
#'
#' Read-only. Covers every table in the schema, not just the seven, so that
#' DELETE held on an audit trail is visible before a narrower sweep hides it.
#'
#' @param con A connection to plots_transects.
#' @param tables Character vector of the tables the sweep would touch.
#' @return Invisibly a list of data frames.
report_delete_rights <- function(con,
                                 tables = CafriplotsR:::.plot_scope_tables()) {

  stopifnot("Invalid connection" = DBI::dbIsValid(con))

  cli::cli_h1("DELETE on plots_transects")

  owner <- DBI::dbGetQuery(con, "
    SELECT pg_get_userbyid(relowner) AS owner_name
      FROM pg_class WHERE oid = 'public.data_liste_plots'::regclass")$owner_name

  # --- 1. the table privilege ----------------------------------------------
  all_del <- DBI::dbGetQuery(con, "
    SELECT c.relname AS table_name,
           pg_get_userbyid(a.grantee) AS grantee
      FROM pg_class c
           CROSS JOIN LATERAL
             aclexplode(COALESCE(c.relacl, acldefault('r', c.relowner))) a
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND c.relkind = 'r'
       AND a.privilege_type = 'DELETE'
       AND a.grantee <> 0
       AND a.grantee <> c.relowner
     ORDER BY 1, 2")

  in_scope  <- all_del[all_del$table_name %in% tables, , drop = FALSE]
  out_scope <- all_del[!all_del$table_name %in% tables, , drop = FALSE]

  cli::cli_h2("Table privilege, on the {length(tables)} tables a deletion reaches")
  if (nrow(in_scope) == 0) {
    cli::cli_alert_success("Nobody but the owner holds DELETE on these")
  } else {
    per_user <- as.data.frame(table(grantee = in_scope$grantee))
    names(per_user)[2] <- "n_tables"
    per_user <- per_user[order(-per_user$n_tables, per_user$grantee), ]
    print(per_user, row.names = FALSE)
    cli::cli_alert_warning(
      "{length(unique(in_scope$grantee))} account{?s} hold{?s/} DELETE here
       ({nrow(in_scope)} grant{?s} in all)")
  }

  cli::cli_h2("Table privilege, on other tables - not touched by this sweep")
  if (nrow(out_scope) == 0) {
    cli::cli_alert_success("None")
  } else {
    per_table <- as.data.frame(table(table_name = out_scope$table_name))
    names(per_table)[2] <- "n_accounts"
    per_table <- per_table[order(-per_table$n_accounts), ]
    print(utils::head(per_table, 40), row.names = FALSE)
    followup <- per_table[grepl("^followup_", per_table$table_name), , drop = FALSE]
    if (nrow(followup) > 0) {
      cli::cli_alert_warning(
        "{nrow(followup)} of them {?is/are} an audit trail. DELETE on an audit
         trail is worth removing too, but it is a separate decision.")
    }
  }

  # --- 2. the policies -----------------------------------------------------
  pol <- .delete_policies(con)

  cli::cli_h2("DELETE policies on data_liste_plots")
  if (nrow(pol) == 0) {
    cli::cli_alert_warning("None - nobody can delete a plot row at all")
  } else {
    print(pol[, c("policyname", "role_list", "n_ids", "is_public")],
          row.names = FALSE)
  }

  creator_policy <- pol[pol$is_public, , drop = FALSE]
  if (nrow(creator_policy) == 0) {
    cli::cli_alert_danger(c(
      "No TO PUBLIC DELETE policy - creator_access_delete is missing.",
      i = "Dropping the per-account policies would leave nobody able to delete
           their own plots. This migration will refuse."))
  } else {
    cli::cli_alert_success(
      "{.val {creator_policy$policyname}} is TO PUBLIC and will be kept - it is
       what makes deletion creator-scoped")
  }

  to_drop <- pol[!pol$is_public, , drop = FALSE]
  if (nrow(to_drop) > 0) {
    cli::cli_alert_warning(
      "{nrow(to_drop)} per-account DELETE polic{?y/ies}, covering
       {sum(to_drop$n_ids)} plot grant{?s}, would be dropped")
  }

  # --- 3. who created something -------------------------------------------
  creators <- DBI::dbGetQuery(con, "
    SELECT created_by AS db_user, count(*)::int AS n_created
      FROM data_liste_plots
     WHERE created_by IS NOT NULL
       AND created_by <> (SELECT pg_get_userbyid(relowner) FROM pg_class
                           WHERE oid = 'public.data_liste_plots'::regclass)
     GROUP BY 1 ORDER BY 2 DESC")

  cli::cli_h2("Accounts that created plots - these keep the table privilege")
  if (nrow(creators) == 0) cli::cli_alert_info("None but the owner")
  else print(creators, row.names = FALSE)

  invisible(list(owner = owner, in_scope = in_scope, out_of_scope = out_scope,
                 policies = pol, to_drop = to_drop,
                 creator_policy = creator_policy, creators = creators))
}


#' The DELETE policies on data_liste_plots, with enough to rebuild them
#' @keywords internal
#' @noRd
.delete_policies <- function(con) {
  pol <- DBI::dbGetQuery(con, "
    SELECT p.policyname,
           p.permissive,
           array_to_string(
             ARRAY(SELECT quote_ident(r) FROM unnest(p.roles) r), ', ') AS role_list,
           ('public' = ANY(p.roles)) AS is_public,
           p.qual
      FROM pg_policies p
     WHERE p.schemaname = 'public' AND p.tablename = 'data_liste_plots'
       AND p.cmd = 'DELETE'
     ORDER BY p.policyname")

  if (nrow(pol) == 0) return(pol)

  pol$n_ids <- vapply(pol$qual, function(q) {
    length(CafriplotsR:::.parse_policy_plot_ids(q)$ids)
  }, integer(1), USE.NAMES = FALSE)

  pol$restore_sql <- sprintf(
    'CREATE POLICY %s ON public.data_liste_plots AS %s FOR DELETE TO %s USING (%s);',
    paste0('"', pol$policyname, '"'), pol$permissive, pol$role_list, pol$qual)

  pol
}


#' Make DELETE creator-only
#'
#' @param con A connection to plots_transects, as the owner of the tables.
#' @param tables Character vector of tables. Defaults to the seven a plot
#'   deletion reaches.
#' @param keep Character vector of roles that keep the DELETE table privilege.
#'   `NULL` (the default) derives it from `created_by`, which is the whole point:
#'   whoever created a plot keeps the right to delete it.
#' @param out_dir Directory for the rollback `.sql`. `NULL` uses a timestamped
#'   folder under `LOCALAPPDATA` (or `tempdir()`).
#' @param dry_run Logical. `TRUE` (the default) writes nothing and changes
#'   nothing, but prints every statement and its inverse.
#' @return Invisibly a list with what was dropped and revoked.
migrate_revoke_delete_rights <- function(con,
                                         tables = CafriplotsR:::.plot_scope_tables(),
                                         keep = NULL,
                                         out_dir = NULL,
                                         dry_run = TRUE) {

  state <- report_delete_rights(con, tables = tables)

  i_am_owner <- DBI::dbGetQuery(con,
    "SELECT pg_get_userbyid(relowner) = current_user AS ok
       FROM pg_class WHERE oid = 'public.data_liste_plots'::regclass")$ok
  if (!isTRUE(i_am_owner)) {
    cli::cli_abort(c(
      "Only the owner can drop a policy or revoke a privilege it granted.",
      i = "Connect as {.val {state$owner}}."))
  }

  # Refuse to drop the per-account policies unless the creator policy is there
  # to catch what they were doing. Otherwise this migration would take away the
  # right it exists to preserve.
  if (nrow(state$to_drop) > 0 && nrow(state$creator_policy) == 0) {
    cli::cli_abort(c(
      "There is no TO PUBLIC DELETE policy on data_liste_plots.",
      x = "Dropping the per-account policies would leave nobody able to delete
           the plots they created.",
      i = "Restore creator_access_delete from
           {.file inst/migrations/add_created_by.R} first."))
  }

  if (is.null(keep)) keep <- state$creators$db_user
  keep <- setdiff(unique(keep), c("", NA, state$owner))

  cli::cli_h2("Plan")
  cli::cli_alert_info(
    "Keep the table privilege for {length(keep)} creator account{?s}: {.val {keep}}")
  cli::cli_alert_info(
    "Drop {nrow(state$to_drop)} per-account DELETE polic{?y/ies}, keep
     {.val {state$creator_policy$policyname}}")

  to_revoke <- state$in_scope[!state$in_scope$grantee %in% keep, , drop = FALSE]

  q <- function(x) DBI::dbQuoteIdentifier(con, x)
  drops <- if (nrow(state$to_drop) > 0) {
    paste0("DROP POLICY ", q(state$to_drop$policyname),
           " ON public.data_liste_plots;")
  } else character(0)
  revokes <- if (nrow(to_revoke) > 0) {
    paste0("REVOKE DELETE ON public.", q(to_revoke$table_name),
           " FROM ", q(to_revoke$grantee), ";")
  } else character(0)
  grants <- if (nrow(to_revoke) > 0) {
    paste0("GRANT DELETE ON public.", q(to_revoke$table_name),
           " TO ", q(to_revoke$grantee), ";")
  } else character(0)

  if (length(drops) == 0 && length(revokes) == 0) {
    cli::cli_alert_success("Nothing to do - the database is already in the target state.")
    return(invisible(list(dropped = character(0), revoked = character(0))))
  }

  cli::cli_h2("{length(drops)} DROP POLICY + {length(revokes)} REVOKE")
  for (s in c(drops, revokes)) cli::cli_verbatim(paste0("  ", s))

  cli::cli_h2("The inverse, which restores the previous state")
  for (s in c(state$to_drop$restore_sql, grants)) {
    cli::cli_verbatim(paste0("  ", s))
  }

  cli::cli_alert_warning(c(
    "After this, an account that did not create a plot cannot delete it, and
     {.fn safe_delete_plot} will fail for it with a permission error. The
     {length(keep)} creator account{?s} keep{?s/} DELETE on all
     {length(tables)} tables, so until row-level security reaches the child
     tables {?it/they} can still delete child rows of plots {?it/they} did not
     create. No import or update path is affected."))

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was written or changed.")
    cli::cli_alert_info("Re-run with {.code dry_run = FALSE} to apply.")
    return(invisible(list(dropped = drops, revoked = revokes,
                          restore = c(state$to_drop$restore_sql, grants))))
  }

  # --- write the rollback BEFORE dropping anything -------------------------
  if (is.null(out_dir)) {
    base <- Sys.getenv("LOCALAPPDATA", unset = "")
    if (!nzchar(base)) base <- tempdir()
    out_dir <- file.path(base, "cafri_delete_policies",
                         format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sql_file <- file.path(out_dir, "restore_delete_rights.sql")

  writeLines(c(
    "-- Restores the DELETE policies and table privileges removed by",
    "-- inst/migrations/revoke_delete_rights.R",
    paste0("-- written ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
           " by ", state$owner),
    "",
    "-- 1. the per-account DELETE policies on data_liste_plots",
    state$to_drop$restore_sql,
    "",
    "-- 2. the table privileges",
    grants
  ), sql_file)

  # Verify by reading it back, not by trusting writeLines.
  back <- readLines(sql_file, warn = FALSE)
  n_expected <- length(state$to_drop$restore_sql) + length(grants)
  n_found <- sum(grepl("^(CREATE POLICY|GRANT DELETE)", back))
  if (n_found != n_expected) {
    cli::cli_abort(c(
      "The rollback file does not contain what it should - refusing to proceed.",
      i = "Expected {n_expected} statement{?s}, read back {n_found}.",
      i = "{.file {sql_file}}"))
  }
  cli::cli_alert_success("Rollback written and verified: {.file {sql_file}}")

  DBI::dbBegin(con)
  ok <- FALSE
  on.exit({
    if (!ok) {
      try(DBI::dbRollback(con), silent = TRUE)
      cli::cli_alert_danger("Rolled back - nothing was changed.")
    }
  }, add = TRUE)

  for (s in c(drops, revokes)) DBI::dbExecute(con, s)
  DBI::dbCommit(con)
  ok <- TRUE

  cli::cli_alert_success(
    "{length(drops)} polic{?y/ies} dropped, {length(revokes)} privilege{?s} revoked")

  check_delete_rights(con, tables = tables, keep = keep)
  invisible(list(dropped = drops, revoked = revokes, rollback_file = sql_file))
}


#' Verify that DELETE is creator-only
#'
#' @inheritParams migrate_revoke_delete_rights
#' @return Invisibly `TRUE` if every check passes.
check_delete_rights <- function(con,
                               tables = CafriplotsR:::.plot_scope_tables(),
                               keep = NULL) {

  cli::cli_h2("Verification")
  pass <- TRUE
  say <- function(ok, msg) {
    if (ok) cli::cli_alert_success(msg) else {
      cli::cli_alert_danger(msg); pass <<- FALSE
    }
  }

  if (is.null(keep)) {
    keep <- DBI::dbGetQuery(con, "
      SELECT DISTINCT created_by AS db_user FROM data_liste_plots
       WHERE created_by IS NOT NULL
         AND created_by <> (SELECT pg_get_userbyid(relowner) FROM pg_class
                             WHERE oid = 'public.data_liste_plots'::regclass)")$db_user
  }

  # --- the policies --------------------------------------------------------
  pol <- .delete_policies(con)
  print(pol[, c("policyname", "role_list", "n_ids", "is_public")],
        row.names = FALSE)

  say(any(pol$is_public),
      "A TO PUBLIC DELETE policy remains - creators can delete their own plots")
  say(!any(!pol$is_public),
      "No per-account DELETE policy remains - nobody deletes by plot list")

  # --- the table privilege -------------------------------------------------
  left <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT c.relname AS table_name, pg_get_userbyid(a.grantee) AS grantee
      FROM pg_class c
           CROSS JOIN LATERAL
             aclexplode(COALESCE(c.relacl, acldefault('r', c.relowner))) a
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public'
       AND c.relname IN ({tables*})
       AND a.privilege_type = 'DELETE'
       AND a.grantee <> 0
       AND a.grantee <> c.relowner
     ORDER BY 2, 1", .con = con))

  unexpected <- left[!left$grantee %in% keep, , drop = FALSE]
  say(nrow(unexpected) == 0,
      "Only the owner and the {length(keep)} creator account{?s} hold the DELETE
       table privilege")
  if (nrow(unexpected) > 0) print(unexpected, row.names = FALSE)

  # A creator needs it on every table, or deleting their own plot fails partway
  # and the foreign keys refuse the parent row.
  short <- setdiff(keep, unique(left$grantee[left$table_name == tables[1]]))
  per_creator <- table(left$grantee[left$grantee %in% keep])
  incomplete <- names(per_creator)[per_creator < length(tables)]
  say(length(short) == 0 && length(incomplete) == 0,
      "Every creator account holds DELETE on all {length(tables)} tables")
  if (length(incomplete) > 0) {
    cli::cli_alert_info("Incomplete: {.val {incomplete}} - deleting a plot would
                         fail partway and the foreign keys would refuse it")
  }

  cli::cli_alert_info(
    "Residual until step 5: those {length(keep)} account{?s} can delete child
     rows of plots {?it/they} did not create, because the child tables have no
     policy and PostgreSQL has no per-row GRANT.")

  if (pass) cli::cli_alert_success("DELETE is creator-only on the plot table")
  else      cli::cli_alert_danger("Verification failed - see above")

  invisible(pass)
}
