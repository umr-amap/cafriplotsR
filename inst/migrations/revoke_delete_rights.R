# =============================================================================
# Take DELETE away from every account, and make handing it back deliberate
#
# WHAT WAS FOUND (2026-09-28)
#
# report_plot_access_seed() showed 13,913 of 14,018 plot grants carrying write,
# and because define_user_policy(operations = "ALL") writes _select, _update and
# _delete together, nearly all of them carry DELETE. Nine accounts hold it on
# more than 1,100 plots; three on more than 1,900 of the 2,194. Combined with
# the 32 accounts holding the DELETE table privilege directly, roughly thirty
# people can delete plot records they did not create - and the six child tables
# have no row-level security at all, so the same is true one level down, with
# nothing plot-scoped about it.
#
# WHICH LEVER THIS PULLS
#
# The table privilege, not the policies. Two reasons:
#
#   - it is the stronger one. Without table-level DELETE no policy matters, and
#     it is the only thing that gates the six child tables today, since none of
#     them has a policy.
#   - it leaves the record intact. The policy_<user>_delete policies stay in
#     pg_policies, so who used to hold DELETE remains answerable, and step 5
#     replaces all of those policies anyway.
#
# WHAT STOPS WORKING
#
# Exactly the functions meant to delete, and nothing else. No import or update
# path in the package deletes from these seven tables - checked across R/: every
# DELETE FROM against them lives in safe_delete_plot(),
# safe_delete_individuals(), safe_delete_individual_features() or
# safe_delete_specimen_links(). The rest target lookup tables (specimens,
# table_countries, table_colnam, traitlist), the taxa database, or
# aggregate_individual_traits(), which the owner runs.
#
# So after this, an ordinary account calling safe_delete_plot() gets a
# permission error instead of deleting. That is the intent.
#
# HANDING IT BACK
#
# grant_delete_right(con, user, ids) in R/plot_access.R, which sets can_delete on
# those plots and grants the table privilege. It warns on every call that the
# table privilege cannot be plot-scoped until row-level security reaches the
# child tables, because it cannot.
#
# TO ROLL BACK WHOLESALE
#   The rehearsal prints one GRANT per (role, table) it is about to revoke.
#   Keep that output; it is the rollback script.
#
# Run as the owner of the tables.
# =============================================================================


#' Who holds DELETE, on what
#'
#' Read-only. Covers every table in the schema, not just the seven, so that
#' anything held on an audit trail or a lookup table is visible before a
#' narrower sweep hides it.
#'
#' @param con A connection to plots_transects.
#' @param tables Character vector of the tables the sweep would touch.
#' @return Invisibly a list of data frames.
report_delete_rights <- function(con,
                                 tables = CafriplotsR:::.plot_scope_tables()) {

  stopifnot("Invalid connection" = DBI::dbIsValid(con))

  cli::cli_h1("DELETE privileges on plots_transects")

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

  cli::cli_h2("In scope: the {length(tables)} tables a plot deletion reaches")
  if (nrow(in_scope) == 0) {
    cli::cli_alert_success("Nobody but the owner holds DELETE on these")
  } else {
    per_user <- as.data.frame(table(grantee = in_scope$grantee))
    names(per_user)[2] <- "n_tables"
    per_user <- per_user[order(-per_user$n_tables, per_user$grantee), ]
    print(per_user, row.names = FALSE)
    cli::cli_alert_warning(
      "{length(unique(in_scope$grantee))} account{?s} hold{?s/} DELETE on
       {length(unique(in_scope$table_name))} of these table{?s}
       ({nrow(in_scope)} grant{?s} in all)")
  }

  cli::cli_h2("Out of scope: DELETE held on other tables")
  if (nrow(out_scope) == 0) {
    cli::cli_alert_success("None")
  } else {
    per_table <- as.data.frame(table(table_name = out_scope$table_name))
    names(per_table)[2] <- "n_accounts"
    per_table <- per_table[order(-per_table$n_accounts), ]
    print(utils::head(per_table, 40), row.names = FALSE)
    cli::cli_alert_info(
      "This sweep will not touch {?this/these} - listed so the choice is visible.")

    followup <- per_table[grepl("^followup_", per_table$table_name), , drop = FALSE]
    if (nrow(followup) > 0) {
      cli::cli_alert_warning(c(
        "{nrow(followup)} of them {?is/are} an audit trail
         ({.val {as.character(followup$table_name)}}). DELETE on an audit trail is
         worth removing too, but it is a separate decision from plot deletion."))
    }
  }

  # Which RLS policies grant DELETE - not touched, but the record of who had it.
  pol <- DBI::dbGetQuery(con, "
    SELECT tablename, count(*)::int AS n_delete_policies
      FROM pg_policies
     WHERE schemaname = 'public' AND cmd = 'DELETE'
     GROUP BY 1 ORDER BY 2 DESC")
  cli::cli_h2("DELETE policies (left in place, as the record)")
  if (nrow(pol) == 0) cli::cli_alert_info("None")
  else print(pol, row.names = FALSE)

  invisible(list(in_scope = in_scope, out_of_scope = out_scope, policies = pol))
}


#' Revoke DELETE from every non-owner account on the plot-scope tables
#'
#' @param con A connection to plots_transects, as the owner of the tables.
#' @param tables Character vector of tables. Defaults to the seven a plot
#'   deletion reaches.
#' @param keep Character vector of roles to leave alone. Empty by default - the
#'   point is that nobody keeps it, and it is handed back per account with
#'   `grant_delete_right()`.
#' @param dry_run Logical. `TRUE` (the default) prints every REVOKE, and the
#'   GRANT that would undo it, and changes nothing.
#' @return Invisibly the number of privileges revoked.
migrate_revoke_delete_rights <- function(con,
                                         tables = CafriplotsR:::.plot_scope_tables(),
                                         keep = character(0),
                                         dry_run = TRUE) {

  state <- report_delete_rights(con, tables = tables)

  owner <- DBI::dbGetQuery(con, "
    SELECT pg_get_userbyid(relowner) AS owner_name,
           pg_get_userbyid(relowner) = current_user AS i_am_owner
      FROM pg_class WHERE oid = 'public.data_liste_plots'::regclass")

  if (!isTRUE(owner$i_am_owner)) {
    cli::cli_abort(c(
      "Only the owner can revoke a privilege it granted.",
      i = "Connect as {.val {owner$owner_name}}."))
  }

  todo <- state$in_scope[!state$in_scope$grantee %in% c(keep, owner$owner_name),
                         , drop = FALSE]

  if (nrow(todo) == 0) {
    cli::cli_alert_success("Nothing to do - no account holds DELETE on these tables.")
    return(invisible(0L))
  }

  q <- function(x) DBI::dbQuoteIdentifier(con, x)

  revokes <- paste0("REVOKE DELETE ON public.", q(todo$table_name),
                    " FROM ", q(todo$grantee), ";")
  grants  <- paste0("GRANT DELETE ON public.", q(todo$table_name),
                    " TO ", q(todo$grantee), ";")

  cli::cli_h2("{length(revokes)} REVOKE statement{?s}")
  for (s in revokes) cli::cli_verbatim(paste0("  ", s))

  cli::cli_h2("Keep this: the rollback")
  for (s in grants) cli::cli_verbatim(paste0("  ", s))

  if (length(keep) > 0) {
    cli::cli_alert_info("Left alone by request: {.val {keep}}")
  }

  cli::cli_alert_warning(c(
    "After this, {.fn safe_delete_plot}, {.fn safe_delete_individuals},
     {.fn safe_delete_individual_features} and {.fn safe_delete_specimen_links}
     will fail with a permission error for these accounts. No import or update
     path is affected."))

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed.")
    cli::cli_alert_info("Re-run with {.code dry_run = FALSE} to apply.")
    return(invisible(0L))
  }

  DBI::dbBegin(con)
  ok <- FALSE
  on.exit({
    if (!ok) {
      try(DBI::dbRollback(con), silent = TRUE)
      cli::cli_alert_danger("Rolled back - nothing was changed.")
    }
  }, add = TRUE)

  for (s in revokes) DBI::dbExecute(con, s)
  DBI::dbCommit(con)
  ok <- TRUE

  cli::cli_alert_success("{length(revokes)} DELETE privilege{?s} revoked")
  check_delete_rights(con, tables = tables, keep = keep)
  invisible(length(revokes))
}


#' Verify that nobody but the owner holds DELETE
#'
#' @inheritParams migrate_revoke_delete_rights
#' @return Invisibly `TRUE` if every check passes.
check_delete_rights <- function(con,
                                tables = CafriplotsR:::.plot_scope_tables(),
                                keep = character(0)) {

  cli::cli_h2("Verification")

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
     ORDER BY 1, 2", .con = con))

  unexpected <- left[!left$grantee %in% keep, , drop = FALSE]

  if (nrow(unexpected) == 0) {
    cli::cli_alert_success(
      "No account but the owner{if (length(keep)) ' and those kept' else ''}
       holds DELETE on the {length(tables)} plot-scope tables")
  } else {
    cli::cli_alert_danger("{nrow(unexpected)} DELETE privilege{?s} remain{?s/}:")
    print(unexpected, row.names = FALSE)
  }

  # The policies are meant to survive. Say so, so their presence is not read as
  # a failed revoke.
  pol <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n FROM pg_policies
     WHERE schemaname = 'public' AND tablename = 'data_liste_plots'
       AND cmd = 'DELETE'")$n
  cli::cli_alert_info(
    "{pol} DELETE polic{?y/ies} still on data_liste_plots - deliberately kept as
     the record of who held the right, and inert without the table privilege.")

  invisible(nrow(unexpected) == 0)
}
