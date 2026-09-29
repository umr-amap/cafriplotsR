# =============================================================================
# Take back the table grants nothing uses
#
# WHAT THE ACL ACTUALLY SAYS, read 2026-09-29
#
#   grantee                      privileges on the 7 plot-scope tables
#   PUBLIC                       SELECT on all 7          <- the leak, plainly
#   plots_transects-ro     (21)  SELECT on all 7
#   plots_transects-rw     (13)  INSERT, UPDATE, DELETE, REFERENCES on all 7
#   plots_transects-admin   (4)  TRIGGER, TRUNCATE on all 7
#   plots_transects-overquota    DELETE on all 7, and no members
#   thibauld                     INSERT, UPDATE, DELETE on all 7, directly
#   33 accounts                  SELECT/INSERT/UPDATE/DELETE on data_liste_plots
#
# THREE THINGS TO TAKE BACK, none of which anything uses
#
# 1. SELECT FROM PUBLIC on all seven. This is the read leak in its simplest
#    form: not "35 accounts can read everything" but "anything that can connect
#    can read everything". user_test3 and user_test4 are in
#    plots_transects-none, which grants nothing, and can still read all 2,053,481
#    measurements today - purely through PUBLIC.
#
#    Row-level security contains it, because the privilege check and the row
#    filter are separate: once plot_access_child_rls.R is applied, PUBLIC SELECT
#    on a table with policies yields only the caller's rows. But it should still
#    go. plots_transects-ro already gives the 21 read accounts SELECT on all
#    seven, so PUBLIC adds nothing except a table that goes wide open the moment
#    anyone disables row-level security on it - and DISABLE is one statement.
#
# 2. TRUNCATE from plots_transects-admin. TRUNCATE is the one privilege
#    row-level security does not filter: there are no rows to filter, the table
#    is emptied wholesale. Four accounts inherit it - dauby, klein, ploton,
#    texier - and nothing in CafriplotsR truncates any of the seven. The package
#    truncates table_idtax_temp and wcvp_names, both staging tables, and nothing
#    else. The owner keeps its own TRUNCATE, which is a separate ACL entry.
#
# 3. DELETE from plots_transects-overquota. It holds DELETE on all seven and has
#    no members, so this changes nothing today. That is exactly why it is worth
#    doing: it is a grant waiting for a membership. If OVH ever moves an account
#    into that group during a quota event, that account acquires DELETE on every
#    plot table, and nobody would be looking.
#
# WHAT IS DELIBERATELY LEFT
#
# - plots_transects-rw keeps INSERT/UPDATE/DELETE. The 13 accounts in it need to
#   write, and after plot_access_child_rls.R row-level security scopes those
#   writes to their own plots. Taking the privilege away instead of scoping it
#   would stop them importing.
# - plots_transects-ro keeps SELECT, for the same reason in reverse.
# - TRIGGER on plots_transects-admin is left alone: it is how OVH's own tooling
#   is likely to work, and a trigger cannot be created on a table you do not own
#   anyway.
# - thibauld's direct INSERT/UPDATE/DELETE on all seven is left alone here. It
#   is an outlier - every other account gets child-table access through a group -
#   but revoking it would take away write access that is legitimately used, and
#   row-level security scopes it like everyone else's. Noted, not changed.
# - postgres is never touched. Stripping the instance's administrative account is
#   how you lock yourself out of a managed database.
#
# THE REVOKE THAT DOES NOTHING
#
# A REVOKE only removes grants made by the current role. Every grant above shows
# dauby as grantor on tables dauby owns, so all three will bite - but P0.2
# revoked write FROM PUBLIC and left three accounts' separate direct grants
# untouched while appearing to succeed, so this re-reads the ACL after committing
# and reports what is actually left rather than reporting that it ran.
#
# HOW TO RUN
#
#   source(system.file("migrations", "tighten_table_grants.R",
#                      package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#
#   report_table_grants(con)                    # read-only
#   migrate_tighten_table_grants(con)           # rehearsal
#   migrate_tighten_table_grants(con, dry_run = FALSE)
#   check_table_grants(con)
#
# ORDER RELATIVE TO STEP 5
#
# Item 2 can go any time and should go soon. Items 1 and 3 are safe before or
# after plot_access_child_rls.R. Nothing here depends on step 5 and step 5 does
# not depend on this.
# =============================================================================


.grants_con <- function(con) {
  if (inherits(con, "Pool")) pool::poolCheckout(con) else con
}

.grants_release <- function(con, actual) {
  if (inherits(con, "Pool") && !is.null(actual)) pool::poolReturn(actual)
  invisible(NULL)
}


# What to take back, and from whom. Each entry is checked against the live ACL
# before anything is emitted, so a grant already gone produces no statement.
.grant_removals <- function() {
  list(
    list(grantee = "PUBLIC",                    privilege = "SELECT",
         why = "anything that can connect can read all seven"),
    list(grantee = "plots_transects-admin",     privilege = "TRUNCATE",
         why = "the one privilege row-level security does not filter"),
    list(grantee = "plots_transects-overquota", privilege = "DELETE",
         why = "a grant waiting for a membership")
  )
}


#' Read the ACL of the seven plot-scope tables
#'
#' Grantee OID 0 is PUBLIC, which `pg_get_userbyid()` does not render as
#' anything useful - hence the CASE.
#' @noRd
.table_grants <- function(con) {

  tables <- CafriplotsR:::.plot_scope_tables()

  DBI::dbGetQuery(con, glue::glue_sql("
    SELECT c.relname AS table_name,
           CASE WHEN a.grantee = 0 THEN 'PUBLIC'
                ELSE pg_get_userbyid(a.grantee) END AS grantee,
           a.privilege_type,
           pg_get_userbyid(a.grantor) AS grantor,
           EXISTS (SELECT 1 FROM pg_roles r
                    WHERE r.oid = a.grantee AND r.rolcanlogin) AS grantee_logs_in
      FROM pg_class c
      JOIN pg_namespace n ON n.oid = c.relnamespace
      CROSS JOIN LATERAL
        aclexplode(COALESCE(c.relacl, acldefault('r', c.relowner))) a
     WHERE n.nspname = 'public' AND c.relname IN ({tables*})
     ORDER BY 2, 3, 1", .con = con))
}


#' The statements, and why each one is or is not needed
#' @noRd
.grant_statements <- function(acl, owner) {

  tables <- CafriplotsR:::.plot_scope_tables()
  out <- list()

  for (rm in .grant_removals()) {

    held <- acl[acl$grantee == rm$grantee &
                acl$privilege_type == rm$privilege, , drop = FALSE]

    if (nrow(held) == 0) {
      out[[length(out) + 1L]] <- list(
        grantee = rm$grantee, privilege = rm$privilege, n_tables = 0L,
        why = rm$why, statement = NA_character_,
        note = "already gone")
      next
    }

    foreign <- setdiff(unique(held$grantor), owner)

    target <- if (rm$grantee == "PUBLIC") "PUBLIC" else .gq(rm$grantee)

    out[[length(out) + 1L]] <- list(
      grantee = rm$grantee, privilege = rm$privilege,
      n_tables = nrow(held), why = rm$why,
      statement = paste0(
        "REVOKE ", rm$privilege, " ON ",
        paste0("public.", sort(unique(held$table_name)), collapse = ", "),
        " FROM ", target, ";"),
      note = if (length(foreign))
        paste0("granted by ", paste(foreign, collapse = ", "),
               " - a REVOKE from this session will not remove it")
      else "")
  }

  out
}


# Role names here contain a hyphen (plots_transects-rw), which has to be quoted
# or the parser reads it as subtraction.
.gq <- function(x) paste0('"', gsub('"', '""', x), '"')


#' @title What the seven plot-scope tables grant, and to whom
#' @description
#' Read-only. Row-level security decides which *rows* an account sees; this is
#' the layer underneath, which decides whether it may touch the table at all.
#'
#' @param con A connection to the main database.
#' @return Invisibly, a list with the ACL and the planned statements.
#' @export
report_table_grants <- function(con) {

  actual <- .grants_con(con)
  on.exit(.grants_release(con, actual), add = TRUE)

  acl   <- .table_grants(actual)
  owner <- DBI::dbGetQuery(actual, "SELECT current_user AS u")$u

  cli::cli_h1("Table grants on the seven plot-scope tables")

  cli::cli_h2("Everything held on all seven")
  wide <- acl[acl$grantee != owner, , drop = FALSE]
  wide <- as.data.frame(table(wide$grantee, wide$privilege_type))
  names(wide) <- c("grantee", "privilege", "n_tables")
  wide <- wide[wide$n_tables == length(CafriplotsR:::.plot_scope_tables()), ]
  print(wide[order(wide$grantee), ], row.names = FALSE)

  cli::cli_alert_info(
    "A grantee listed on all seven has blanket access to the child tables. One
     listed on a single table is almost always {.val data_liste_plots}, from
     {.fn define_user_policy}.")

  cli::cli_h2("What this migration would take back")

  plan <- .grant_statements(acl, owner)

  for (p in plan) {
    if (is.na(p$statement)) {
      cli::cli_alert_success(
        "{p$privilege} from {.val {p$grantee}}: already gone")
      next
    }
    cli::cli_alert_warning(
      "{p$privilege} from {.val {p$grantee}} on {p$n_tables} table{?s} - {p$why}")
    cli::cli_verbatim(p$statement)
    if (nzchar(p$note)) cli::cli_alert_danger(p$note)
  }

  cli::cli_h2("Deliberately left alone")
  cli::cli_alert_info(
    "{.val plots_transects-rw} keeps INSERT/UPDATE/DELETE and
     {.val plots_transects-ro} keeps SELECT: those accounts need to read and
     write, and row-level security is what scopes them to their own plots.
     Removing the privilege instead would stop them working.")
  cli::cli_alert_info(
    "{.val thibauld} holds INSERT/UPDATE/DELETE on all seven directly, unlike
     every other account. Left as it is - the access is used, and row-level
     security scopes it the same as everyone else's - but it is the one account a
     group-level change would not reach.")

  invisible(list(acl = acl, plan = plan))
}


#' @title Take back the grants nothing uses
#' @param con A connection to the main database.
#' @param dry_run Logical. `TRUE` (the default) prints and changes nothing.
#' @param restore_file Path for the restore script, written and read back before
#'   anything changes.
#' @return Invisibly, the statements applied.
#' @export
migrate_tighten_table_grants <- function(con, dry_run = TRUE,
                                         restore_file = "restore_table_grants.sql") {

  actual <- .grants_con(con)
  on.exit(.grants_release(con, actual), add = TRUE)

  acl   <- .table_grants(actual)
  owner <- DBI::dbGetQuery(actual, "SELECT current_user AS u")$u
  plan  <- .grant_statements(acl, owner)

  todo <- Filter(function(p) !is.na(p$statement), plan)

  cli::cli_h1("Tightening table grants{if (dry_run) ' (rehearsal)' else ''}")

  if (length(todo) == 0) {
    cli::cli_alert_success("Nothing to do - the target state is already reached")
    return(invisible(character(0)))
  }

  statements <- vapply(todo, function(p) p$statement, character(1))

  # The inverse, built from the ACL as it is now rather than from what the
  # migration assumes was there.
  restore <- c(
    "-- Restore the grants tighten_table_grants.R removed.",
    paste0("-- written ", Sys.Date(), " by ", owner),
    "")

  for (p in todo) {
    held <- acl[acl$grantee == p$grantee &
                acl$privilege_type == p$privilege, , drop = FALSE]
    target <- if (p$grantee == "PUBLIC") "PUBLIC" else .gq(p$grantee)
    restore <- c(restore, paste0(
      "GRANT ", p$privilege, " ON ",
      paste0("public.", sort(unique(held$table_name)), collapse = ", "),
      " TO ", target, ";"))
  }

  cli::cli_h2("{length(statements)} statement{?s}")
  for (s in statements) cli::cli_verbatim(s)

  if (dry_run) {
    cli::cli_h2("Restore")
    for (s in restore) cli::cli_verbatim(s)
    cli::cli_alert_info("Rehearsal only. {.code dry_run = FALSE} to apply.")
    cli::cli_alert_warning(
      "Revoking SELECT from PUBLIC is the one with reach. Anything that reads
       these tables without being in plots_transects-ro or -rw loses access -
       check that no reporting job or external connection relies on it.")
    return(invisible(statements))
  }

  writeLines(restore, restore_file)
  if (!identical(readLines(restore_file), restore)) {
    cli::cli_abort("{.file {restore_file}} did not read back as written")
  }
  cli::cli_alert_success(
    "Restore script at {.file {normalizePath(restore_file)}}")

  DBI::dbBegin(actual)
  tryCatch({
    for (s in statements) DBI::dbExecute(actual, s)
    DBI::dbCommit(actual)
  }, error = function(e) {
    tryCatch(DBI::dbRollback(actual), error = function(e2) NULL)
    cli::cli_abort(c("Nothing applied, rolled back.", x = conditionMessage(e)))
  })

  cli::cli_alert_success("{length(statements)} statement{?s} applied")

  # The whole point: a REVOKE can succeed and remove nothing.
  cli::cli_h2("Re-reading the ACL after commit")
  check_table_grants(con)

  invisible(statements)
}


#' @title Verify the grants are gone
#' @param con A connection to the main database.
#' @return Invisibly, `TRUE` when every targeted grant is gone.
#' @export
check_table_grants <- function(con) {

  actual <- .grants_con(con)
  on.exit(.grants_release(con, actual), add = TRUE)

  acl   <- .table_grants(actual)
  owner <- DBI::dbGetQuery(actual, "SELECT current_user AS u")$u
  plan  <- .grant_statements(acl, owner)

  left <- Filter(function(p) !is.na(p$statement), plan)

  if (length(left) == 0) {
    cli::cli_alert_success(
      "None of the three grants remains on any of the seven tables")
    return(invisible(TRUE))
  }

  for (p in left) {
    cli::cli_alert_danger(
      "{p$privilege} still held by {.val {p$grantee}} on {p$n_tables} table{?s}")
    if (nzchar(p$note)) cli::cli_alert_info(p$note)
  }
  cli::cli_alert_info(
    "A REVOKE only removes grants made by the current role - check the
     {.code grantor} column in {.fn report_table_grants}.")

  invisible(FALSE)
}
