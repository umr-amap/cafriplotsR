# MIGRATION - not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
#   source(system.file("migrations", "revoke_stray_dml_grants.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#   migrate_revoke_stray_dml(con)                   # rehearsal: prints, changes nothing
#   migrate_revoke_stray_dml(con, dry_run = FALSE)  # apply


#' Migration: revoke the direct write grants held by the published and test accounts
#'
#' `CafriP_public` — the account whose credential is published on gh-pages and
#' readable by anyone on the internet — holds `INSERT, UPDATE, DELETE` on
#' `data_liste_plots`, and has live `policy_CafriP_public_update` and
#' `policy_CafriP_public_delete` covering 83 plot ids. Both conditions
#' PostgreSQL requires for a write to succeed are therefore present: a table
#' privilege, and a row policy that matches. `user_test3` and `user_test4` are
#' in the same state, with `INSERT` as well.
#'
#' @details
#' This survived P0.2. That step revoked `INSERT, UPDATE, DELETE` **`FROM
#' PUBLIC`**, which removes the grantee-0 entry from the ACL and nothing else.
#' The grants here are separate ACL entries made directly to each role by
#' [CafriplotsR::define_user_policy()], which issues
#'
#' \preformatted{
#'   GRANT SELECT, INSERT, UPDATE, DELETE ON <table> TO <user>
#' }
#'
#' unconditionally, whatever its `operations` argument says — so
#' `define_read_only_policy()` hands out full DML too. That is the defect
#' recorded as P4.5; this migration removes the consequence, and the defect
#' itself is fixed in the package so it cannot reappear.
#'
#' The post-P0.2 verification reported "CafriP_public writable = 0". It was not
#' wrong about what it measured — it walked the 32 tables that had been granted
#' to `PUBLIC`, and `data_liste_plots` was never one of them. The direct grant
#' sat outside the question being asked.
#'
#' @section What this changes, and what it deliberately does not:
#' For each named role, on every table in `public` where the role holds a
#' *direct* grant: `INSERT`, `UPDATE`, `DELETE` are revoked and `SELECT` is
#' kept, and any policy `FOR INSERT/UPDATE/DELETE` naming the role is dropped
#' while its `FOR SELECT` policy is kept. Read access is untouched — the public
#' apps keep working on the same 83 plots.
#'
#' Both halves matter. Revoking the grant alone would leave the policies in
#' place, so a single careless `define_user_policy()` call would make the
#' account writable again without anyone re-creating a policy.
#'
#' 28 other named accounts also hold direct `INSERT, UPDATE, DELETE` on
#' `data_liste_plots` from the same defect. They are **reported and not
#' touched**: unlike the published credential and the test logins, some of them
#' are real collaborators who may be writing legitimately, and deciding that
#' needs the per-user evidence in `inst/scripts/who_actually_writes.R`, not a
#' blanket revoke.
#'
#' @section Why this does not need an admin role:
#' `dauby` owns every table in the schema and issued these grants, so it can
#' revoke them and drop the policies. Nothing here needs `SUPERUSER`,
#' `CREATEROLE` or `BYPASSRLS`, none of which are obtainable on the OVH managed
#' instance. No role is created, altered or dropped.
#'
#' @param con Database connection to `plots_transects`, as the table owner.
#' @param roles Character vector of roles to strip write access from.
#' @param dry_run If TRUE (the default), report what would happen and change
#'   nothing.
#' @return Invisibly, a list with the grants and policies found, and the
#'   post-migration verification when it ran.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' migrate_revoke_stray_dml(con)                   # rehearse
#' migrate_revoke_stray_dml(con, dry_run = FALSE)  # apply
#' }
#'
#' @keywords internal
migrate_revoke_stray_dml <- function(con,
                                     roles = c("CafriP_public",
                                               "user_test3",
                                               "user_test4"),
                                     dry_run = TRUE) {

  cli::cli_h1("Migration: revoke stray DML grants from published and test accounts")

  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  writes <- c("INSERT", "UPDATE", "DELETE")

  # -- Step 1: are we where we think we are, and can we act? ----------------
  cli::cli_h2("Step 1: Preflight")

  whoami <- DBI::dbGetQuery(con, "
    SELECT current_database() AS db, current_user AS usr")
  cli::cli_alert_info("Connected to {.val {whoami$db}} as {.val {whoami$usr}}")

  if (whoami$db != "plots_transects") {
    cli::cli_abort("This migration belongs to plots_transects, not {.val {whoami$db}}")
  }

  unknown <- setdiff(roles, DBI::dbGetQuery(con, "SELECT rolname FROM pg_roles")$rolname)
  if (length(unknown) > 0) {
    cli::cli_abort("No such role{?s}: {.val {unknown}}")
  }

  # Ownership is what makes the revoke possible without an admin role.
  not_owned <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n
      FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND c.relkind = 'r'
       AND pg_get_userbyid(c.relowner) <> current_user")$n
  if (not_owned > 0) {
    cli::cli_alert_warning(
      "{not_owned} table{?s} in {.field public} {?is/are} owned by someone else - \\
       grants on {?it/them} cannot be revoked from this session.")
  } else {
    cli::cli_alert_success("Every table in {.field public} is owned by this role")
  }

  # -- Step 2: the direct grants ---------------------------------------------
  # aclexplode() reads pg_class.relacl, so it shows grants made TO the role
  # itself. Privileges reaching the role through PUBLIC or a group role are
  # not here on purpose - those are not ours to revoke.
  cli::cli_h2("Step 2: Direct write grants held by these roles")

  grants <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT c.relname                AS table_name,
           g.rolname                AS grantee,
           a.privilege_type         AS privilege
      FROM pg_class c
      JOIN pg_namespace n ON n.oid = c.relnamespace
      CROSS JOIN LATERAL aclexplode(c.relacl) a
      JOIN pg_roles g ON g.oid = a.grantee
     WHERE n.nspname = 'public'
       AND c.relkind = 'r'
       AND g.rolname IN ({roles*})
       AND a.privilege_type IN ({writes*})
     ORDER BY 1, 2, 3", roles = roles, writes = writes, .con = con))

  if (nrow(grants) == 0) {
    cli::cli_alert_success("No direct write grants found - that half is already clean")
  } else {
    print(grants, row.names = FALSE)
    cli::cli_alert_warning(
      "{nrow(grants)} direct write grant{?s} across \\
       {length(unique(grants$table_name))} table{?s}")
  }

  # Sequence USAGE: an INSERT into a serial-key table needs it, and leaving it
  # behind produces confusing half-failures rather than a clean refusal.
  seq_grants <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT c.relname AS sequence_name, g.rolname AS grantee,
           a.privilege_type AS privilege
      FROM pg_class c
      JOIN pg_namespace n ON n.oid = c.relnamespace
      CROSS JOIN LATERAL aclexplode(c.relacl) a
      JOIN pg_roles g ON g.oid = a.grantee
     WHERE n.nspname = 'public'
       AND c.relkind = 'S'
       AND g.rolname IN ({roles*})
       AND a.privilege_type IN ('USAGE', 'UPDATE')
     ORDER BY 1, 2, 3", roles = roles, .con = con))

  if (nrow(seq_grants) > 0) {
    cli::cli_alert_warning("{nrow(seq_grants)} sequence grant{?s} will go too")
    print(seq_grants, row.names = FALSE)
  }

  # -- Step 3: the policies that make those grants live ----------------------
  cli::cli_h2("Step 3: Write policies naming these roles")

  policies <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT schemaname, tablename, policyname, cmd, roles::text AS roles
      FROM pg_policies
     WHERE schemaname = 'public'
       AND cmd IN ({writes*})
       AND roles && ARRAY[{roles*}]::name[]
     ORDER BY tablename, policyname", roles = roles, writes = writes, .con = con))

  if (nrow(policies) == 0) {
    cli::cli_alert_success("No write policies name these roles")
  } else {
    print(policies[, c("tablename", "policyname", "cmd", "roles")], row.names = FALSE)
  }

  # A policy naming several roles must not be dropped on one role's account.
  shared <- policies[vapply(policies$roles,
                            function(r) length(strsplit(gsub("[{}]", "", r), ",")[[1]]) > 1,
                            logical(1)), ]
  if (nrow(shared) > 0) {
    cli::cli_abort(c(
      "{nrow(shared)} policy names more than one role and would affect others if dropped.",
      i = "Rewrite {.val {shared$policyname}} by hand before running this.",
      i = "Dropping a shared policy is not reversible from inside this migration."
    ))
  }

  # -- Step 4: what survives -------------------------------------------------
  cli::cli_h2("Step 4: What each role keeps")

  keeps <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT g.rolname AS grantee, c.relname AS table_name
      FROM pg_class c
      JOIN pg_namespace n ON n.oid = c.relnamespace
      CROSS JOIN LATERAL aclexplode(c.relacl) a
      JOIN pg_roles g ON g.oid = a.grantee
     WHERE n.nspname = 'public' AND c.relkind = 'r'
       AND g.rolname IN ({roles*})
       AND a.privilege_type = 'SELECT'
     ORDER BY 1, 2", roles = roles, .con = con))
  if (nrow(keeps) > 0) {
    cli::cli_alert_info("Direct SELECT grants kept: {nrow(keeps)}")
    print(keeps, row.names = FALSE)
  }

  read_pol <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT tablename, policyname, cmd
      FROM pg_policies
     WHERE schemaname = 'public' AND cmd = 'SELECT'
       AND roles && ARRAY[{roles*}]::name[]
     ORDER BY 1, 2", roles = roles, .con = con))
  if (nrow(read_pol) > 0) {
    cli::cli_alert_info("SELECT policies kept - read access is unchanged:")
    print(read_pol, row.names = FALSE)
  }

  # -- Step 5: the accounts this does NOT touch ------------------------------
  cli::cli_h2("Step 5: Other accounts holding the same grant (reported only)")

  others <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT g.rolname AS grantee,
           string_agg(DISTINCT a.privilege_type, ', ' ORDER BY a.privilege_type)
             AS privileges
      FROM pg_class c
      JOIN pg_namespace n ON n.oid = c.relnamespace
      CROSS JOIN LATERAL aclexplode(c.relacl) a
      JOIN pg_roles g ON g.oid = a.grantee
     WHERE n.nspname = 'public'
       AND c.relname = 'data_liste_plots'
       AND g.rolname NOT IN ({roles*})
       AND g.rolcanlogin
       AND g.rolname <> current_user
       AND a.privilege_type IN ({writes*})
     GROUP BY 1 ORDER BY 1", roles = roles, writes = writes, .con = con))

  if (nrow(others) > 0) {
    cli::cli_alert_warning(
      "{nrow(others)} other login role{?s} hold{?s/} direct write on \\
       {.field data_liste_plots} from the same defect. Left alone here - see \\
       {.path inst/scripts/who_actually_writes.R} before deciding on them.")
    print(others, row.names = FALSE)
  }

  # -- Step 6: apply ---------------------------------------------------------
  cli::cli_h2("Step 6: Applying")

  qi <- function(x) DBI::dbQuoteIdentifier(con, x)

  statements <- character()
  for (r in roles) {
    tabs <- unique(grants$table_name[grants$grantee == r])
    for (t in tabs) {
      statements <- c(statements, sprintf(
        "REVOKE INSERT, UPDATE, DELETE ON %s FROM %s", qi(t), qi(r)))
    }
    seqs <- unique(seq_grants$sequence_name[seq_grants$grantee == r])
    for (s in seqs) {
      statements <- c(statements, sprintf(
        "REVOKE USAGE, UPDATE ON SEQUENCE %s FROM %s", qi(s), qi(r)))
    }
  }
  for (i in seq_len(nrow(policies))) {
    statements <- c(statements, sprintf(
      "DROP POLICY IF EXISTS %s ON %s",
      qi(policies$policyname[i]), qi(policies$tablename[i])))
  }

  if (length(statements) == 0) {
    cli::cli_alert_success("Nothing to do - the database is already in the target state")
    return(invisible(list(grants = grants, policies = policies, after = NULL)))
  }

  if (dry_run) {
    for (s in statements) cli::cli_alert_info("Would execute: {.code {s}}")
    cli::cli_alert_info(
      "Dry run - nothing was changed. Re-run with {.code dry_run = FALSE}.")
    return(invisible(list(grants = grants, policies = policies, after = NULL)))
  }

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
    cli::cli_alert_danger("Rolled back: {e$message}")
    FALSE
  })
  if (!ok) stop("Migration failed - no change was committed.", call. = FALSE)

  # -- Step 7: verify --------------------------------------------------------
  # has_table_privilege() is the honest test: it follows PUBLIC and group-role
  # paths too, so it answers "can this account write" rather than "did we
  # remove our own ACL entry".
  cli::cli_h2("Step 7: Verifying")

  after <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT r.rolname AS grantee, c.relname AS table_name,
           has_table_privilege(r.rolname, c.oid, 'SELECT') AS can_select,
           has_table_privilege(r.rolname, c.oid, 'INSERT') AS can_insert,
           has_table_privilege(r.rolname, c.oid, 'UPDATE') AS can_update,
           has_table_privilege(r.rolname, c.oid, 'DELETE') AS can_delete
      FROM pg_roles r
      CROSS JOIN pg_class c
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND c.relkind = 'r'
       AND r.rolname IN ({roles*})
       AND (has_table_privilege(r.rolname, c.oid, 'INSERT')
         OR has_table_privilege(r.rolname, c.oid, 'UPDATE')
         OR has_table_privilege(r.rolname, c.oid, 'DELETE'))
     ORDER BY 1, 2", roles = roles, .con = con))

  if (nrow(after) == 0) {
    cli::cli_alert_success(
      "None of {.val {roles}} can write to any table in {.field public}")
  } else {
    cli::cli_alert_danger(
      "{nrow(after)} table/role pair{?s} {?is/are} still writable - \\
       the privilege arrives through PUBLIC or a group role, not a direct grant:")
    print(after, row.names = FALSE)
    cli::cli_alert_info(
      "A group-role path can only be cut in the OVH panel - see P0.PANEL.")
  }

  left <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT tablename, policyname, cmd
      FROM pg_policies
     WHERE schemaname = 'public' AND cmd IN ({writes*})
       AND roles && ARRAY[{roles*}]::name[]",
    roles = roles, writes = writes, .con = con))
  if (nrow(left) == 0) {
    cli::cli_alert_success("No write policy names these roles any more")
  } else {
    cli::cli_alert_danger("{nrow(left)} write polic{?y/ies} survived:")
    print(left, row.names = FALSE)
  }

  cli::cli_alert_success("Migration complete")
  invisible(list(grants = grants, policies = policies, after = after))
}
