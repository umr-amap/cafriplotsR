# =============================================================================
# plot_access: the plot grant table (step 4a/4b of the plot-scope RLS work)
#
# WHAT THIS IS FOR
#
# Today a grant is three CREATE POLICY statements carrying a literal list of
# plot IDs, and CREATE POLICY requires ownership of the table. That has two
# consequences: only the owner can grant anything, and there are now ~125
# policies on data_liste_plots whose ID lists are the only record of who may
# see what. Extending row-level security to the six child tables by the same
# means would multiply that by seven.
#
# This migration replaces the mechanism with data. One row per (account, plot).
# A child-table policy then reads:
#
#   USING (EXISTS (SELECT 1 FROM plot_access a
#                   WHERE a.db_user = current_user
#                     AND a.id_liste_plots = t.<key>))
#
# WHAT IT DELIBERATELY DOES NOT DO
#
#   - it drops no existing policy
#   - it enables row-level security on no table except plot_access itself
#   - it changes no existing grant
#
# So behaviour on the live database is identical the moment it finishes. The
# 125 policies remain the thing actually enforcing access until step 5. This is
# the reversible half of the work, and it is safe to apply before deciding
# whether reads get scoped as well as writes.
#
# DESIGN NOTES
#
# No `can_read` column. A row in plot_access *is* read access; can_write is the
# escalation. Write implies read, because a policy's USING expression is
# evaluated against rows the account must already be able to see, so a
# read-less write row could never be satisfied.
#
# plot_access is readable, not hidden. Each account can SELECT its own rows and
# only its own (policy plot_access_self). That leaks nothing it does not
# already know, and it buys something worth more: a child policy becomes a
# plain semi-join the planner has statistics for, instead of an opaque function
# call. No SECURITY DEFINER is needed on the read path at all.
#
# `can_grant` is created now and granted to creators, but nothing reads it yet:
# no account has INSERT on plot_access. It is here so that enabling delegation
# later -- letting whoever imported a plot share it -- is a matter of adding
# grants and policies, with no table rewrite and no data migration.
#
# The creator trigger is the one SECURITY DEFINER function, because the
# inserting account has no INSERT on plot_access by design. Its search_path is
# pinned at birth (P4.4), it takes its values from NEW, and it builds no
# dynamic SQL.
#
# PREREQUISITE: inst/migrations/created_by_server_asserted.R must be applied
# first. The trigger reads NEW.created_by, so while `insert_open` is
# `WITH CHECK (true)` a client could name any account there and grant it
# access. This migration refuses to run until that is closed.
#
# TO ROLL BACK
#   DROP TRIGGER trg_plot_access_creator ON data_liste_plots;
#   DROP FUNCTION plot_access_creator();
#   DROP FUNCTION accessible_plots(text);
#   DROP TABLE plot_access;
#
# Run as the owner of data_liste_plots.
# =============================================================================


#' Who can read data_liste_plots today?
#'
#' The grant list for plot_access is derived from this rather than hardcoded, so
#' every account that can read a plot can read its own grants, and nothing has
#' to be remembered when an account is added. Grants go to these names
#' directly, never to PUBLIC: a `REVOKE ... FROM PUBLIC` does not touch a direct
#' grant, and mixing the two is how the write hole in P0.2 stayed open.
#'
#' @param con A connection to plots_transects.
#' @return Character vector of role names, excluding PUBLIC and the owner.
.plot_access_grantees <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT DISTINCT pg_get_userbyid(a.grantee) AS grantee
      FROM pg_class c
           CROSS JOIN LATERAL
             aclexplode(COALESCE(c.relacl, acldefault('r', c.relowner))) a
     WHERE c.oid = 'public.data_liste_plots'::regclass
       AND a.privilege_type = 'SELECT'
       AND a.grantee <> 0
       AND a.grantee <> c.relowner
     ORDER BY 1")$grantee
}


#' Create the plot_access table, its policy, its lookup and its trigger
#'
#' @param con A connection to plots_transects, as the owner of
#'   `data_liste_plots`.
#' @param grant_to Character vector of roles to grant SELECT on `plot_access`
#'   and EXECUTE on `accessible_plots()`. `NULL` (the default) derives the list
#'   from whoever holds SELECT on `data_liste_plots`.
#' @param dry_run Logical. `TRUE` (the default) prints every statement and
#'   changes nothing.
#' @return Invisibly `TRUE` when the change was applied.
migrate_plot_access_table <- function(con, grant_to = NULL, dry_run = TRUE) {

  stopifnot("Invalid connection" = DBI::dbIsValid(con))

  cli::cli_h1("plot_access")

  # --- pre-flight ----------------------------------------------------------
  owner <- DBI::dbGetQuery(con, "
    SELECT pg_get_userbyid(relowner) AS owner_name,
           pg_get_userbyid(relowner) = current_user AS i_am_owner
      FROM pg_class WHERE oid = 'public.data_liste_plots'::regclass")

  if (!isTRUE(owner$i_am_owner)) {
    cli::cli_abort(c(
      "Only the owner of data_liste_plots can create policies on it.",
      i = "Connect as {.val {owner$owner_name}}."))
  }

  already <- DBI::dbGetQuery(con, "
    SELECT to_regclass('public.plot_access') IS NOT NULL AS table_exists")$table_exists
  if (isTRUE(already)) {
    cli::cli_alert_warning("plot_access already exists.")
    cli::cli_alert_info("Nothing to do. Run {.code check_plot_access_table(con)}.")
    return(invisible(FALSE))
  }

  # The trigger turns created_by into a grant, so created_by must not be
  # client-settable. Refuse rather than warn: this is the whole reason P4.3 is
  # sequenced first.
  insert_open <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n
      FROM pg_policies
     WHERE schemaname = 'public' AND tablename = 'data_liste_plots'
       AND cmd = 'INSERT' AND with_check = 'true'")$n

  if (insert_open > 0) {
    cli::cli_abort(c(
      "data_liste_plots still has an INSERT policy with {.code WITH CHECK (true)}.",
      x = "The creator trigger reads NEW.created_by. With that policy in place, a
           client can name any account there and grant it access to a plot.",
      i = "Apply {.file inst/migrations/created_by_server_asserted.R} first."))
  }
  cli::cli_alert_success("created_by is server-asserted - the trigger is safe")

  if (is.null(grant_to)) grant_to <- .plot_access_grantees(con)
  grant_to <- setdiff(unique(grant_to), c("", NA, owner$owner_name))

  if (length(grant_to) == 0) {
    cli::cli_alert_warning(
      "No role holds SELECT on data_liste_plots - plot_access will be owner-only")
  } else {
    cli::cli_alert_info(
      "Will grant SELECT on plot_access to {length(grant_to)} role{?s}: {.val {grant_to}}")
  }

  # --- statements ----------------------------------------------------------
  q <- function(x) DBI::dbQuoteIdentifier(con, x)

  statements <- c(
    "CREATE TABLE public.plot_access (
       id_plot_access serial  PRIMARY KEY,
       db_user        name    NOT NULL,
       id_liste_plots integer NOT NULL
                      REFERENCES public.data_liste_plots(id_liste_plots)
                      ON DELETE CASCADE,
       can_write      boolean NOT NULL DEFAULT FALSE,
       can_delete     boolean NOT NULL DEFAULT FALSE,
       can_grant      boolean NOT NULL DEFAULT FALSE,
       origin         text    NOT NULL DEFAULT 'admin'
                      CONSTRAINT plot_access_origin_check
                      CHECK (origin IN ('admin', 'creator', 'delegated')),
       granted_by     name    NOT NULL DEFAULT current_user,
       granted_on     date    NOT NULL DEFAULT current_date,
       note           text,
       CONSTRAINT plot_access_user_plot_key UNIQUE (db_user, id_liste_plots)
     );",

    "CREATE INDEX plot_access_plot_idx ON public.plot_access (id_liste_plots);",

    "COMMENT ON TABLE public.plot_access IS
       'One row per (account, plot) that the account may read. can_write adds
        UPDATE, can_delete adds DELETE, can_grant would add onward sharing.
        Presence of a row IS read access - there is no can_read column, because
        every capability implies read.';",
    "COMMENT ON COLUMN public.plot_access.can_delete IS
       'DELETE. Kept separate from can_write because it is the destructive one,
        it cascades through six child tables, and safe_delete_plot() is not
        atomic. TRUE on creator rows - deleting a plot you imported needs no
        permission - and FALSE on admin rows, where it is handed out per plot
        with grant_delete_right().';",
    "COMMENT ON COLUMN public.plot_access.db_user IS
       'A database role name. Not a foreign key: PostgreSQL cannot reference
        pg_roles. A dropped role leaves a harmless stale row.';",
    "COMMENT ON COLUMN public.plot_access.origin IS
       'admin: granted by the owner. creator: written by trg_plot_access_creator
        from data_liste_plots.created_by. delegated: granted by another account
        holding can_grant (not enabled yet).';",

    # RLS on plot_access itself. TO PUBLIC rather than a role list, so a role
    # granted SELECT later is still confined to its own rows: without a
    # matching policy it would see nothing, which fails closed either way, but
    # this way it also works.
    "ALTER TABLE public.plot_access ENABLE ROW LEVEL SECURITY;",
    "CREATE POLICY plot_access_self ON public.plot_access
       FOR SELECT TO PUBLIC
       USING (db_user = current_user);",
    "COMMENT ON POLICY plot_access_self ON public.plot_access IS
       'An account sees its own grants and no others.';",

    "REVOKE ALL ON public.plot_access FROM PUBLIC;",

    # The R-side convenience reader. Deliberately NOT used inside any policy:
    # a function with a SET clause cannot be inlined by the planner, and an
    # opaque call is exactly what the semi-join design avoids.
    "CREATE OR REPLACE FUNCTION public.accessible_plots(mode text DEFAULT 'read')
     RETURNS integer[]
     LANGUAGE plpgsql
     STABLE
     SECURITY INVOKER
     SET search_path = pg_catalog, public
     AS $fn$
     DECLARE
       result integer[];
     BEGIN
       IF mode IS NULL OR mode NOT IN ('read', 'write') THEN
         RAISE EXCEPTION
           'accessible_plots(): mode must be ''read'' or ''write'', got %',
           COALESCE(mode, 'NULL');
       END IF;
       SELECT COALESCE(array_agg(id_liste_plots ORDER BY id_liste_plots),
                       ARRAY[]::integer[])
         INTO result
         FROM public.plot_access
        WHERE db_user = current_user
          AND (mode = 'read' OR can_write);
       RETURN result;
     END
     $fn$;",
    "COMMENT ON FUNCTION public.accessible_plots(text) IS
       'The plots the calling account may read, or write with mode = write.
        SECURITY INVOKER, so it reads plot_access under the caller own policy.';",
    "REVOKE ALL ON FUNCTION public.accessible_plots(text) FROM PUBLIC;",

    # The creator trigger. SECURITY DEFINER because the inserting account has
    # no INSERT on plot_access, by design. search_path pinned (P4.4).
    "CREATE OR REPLACE FUNCTION public.plot_access_creator()
     RETURNS trigger
     LANGUAGE plpgsql
     SECURITY DEFINER
     SET search_path = pg_catalog, public
     AS $fn$
     DECLARE
       owner_name name;
     BEGIN
       SELECT pg_get_userbyid(relowner) INTO owner_name
         FROM pg_class WHERE oid = 'public.data_liste_plots'::regclass;

       IF NEW.created_by IS NOT NULL AND NEW.created_by <> owner_name THEN
         INSERT INTO public.plot_access
           (db_user, id_liste_plots, can_write, can_delete, can_grant,
            origin, granted_by, note)
         VALUES
           (NEW.created_by, NEW.id_liste_plots, TRUE, TRUE, TRUE, 'creator',
            NEW.created_by, 'imported the plot')
         ON CONFLICT (db_user, id_liste_plots) DO NOTHING;
       END IF;
       RETURN NULL;
     END
     $fn$;",
    "COMMENT ON FUNCTION public.plot_access_creator() IS
       'Gives whoever imports a plot read, write and grant rights over it. The
        owner is skipped: it bypasses row-level security by ownership, so a row
        would be 2000+ rows of noise in every who-can-see-this report.';",
    "REVOKE ALL ON FUNCTION public.plot_access_creator() FROM PUBLIC;",

    "DROP TRIGGER IF EXISTS trg_plot_access_creator ON public.data_liste_plots;",
    "CREATE TRIGGER trg_plot_access_creator
       AFTER INSERT ON public.data_liste_plots
       FOR EACH ROW EXECUTE FUNCTION public.plot_access_creator();"
  )

  for (role in grant_to) {
    statements <- c(statements,
      paste0("GRANT SELECT ON public.plot_access TO ", q(role), ";"),
      paste0("GRANT EXECUTE ON FUNCTION public.accessible_plots(text) TO ",
             q(role), ";"))
  }

  cli::cli_h2("{length(statements)} statement{?s}")
  for (s in statements) {
    cli::cli_verbatim(paste0("  ", gsub("[[:space:]]+", " ", trimws(s))))
  }

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed.")
    cli::cli_alert_info("Re-run with {.code dry_run = FALSE} to apply.")
    return(invisible(FALSE))
  }

  DBI::dbBegin(con)
  ok <- FALSE
  on.exit({
    if (!ok) {
      try(DBI::dbRollback(con), silent = TRUE)
      cli::cli_alert_danger("Rolled back - nothing was changed.")
    }
  }, add = TRUE)

  for (s in statements) DBI::dbExecute(con, s)
  DBI::dbCommit(con)
  ok <- TRUE

  cli::cli_alert_success("plot_access created - and empty")
  cli::cli_alert_info(
    "Seed it with {.file inst/migrations/plot_access_seed.R}. Until then it
     grants nobody anything, and the 125 policies still enforce access.")

  check_plot_access_table(con)
  invisible(TRUE)
}


#' Verify the applied state of plot_access
#'
#' @param con A connection to plots_transects.
#' @return Invisibly `TRUE` if every check passes.
check_plot_access_table <- function(con) {

  cli::cli_h1("Verifying plot_access")
  pass <- TRUE
  say <- function(ok, msg) {
    if (ok) cli::cli_alert_success(msg) else {
      cli::cli_alert_danger(msg); pass <<- FALSE
    }
  }

  exists_now <- DBI::dbGetQuery(con, "
    SELECT to_regclass('public.plot_access') IS NOT NULL AS ok")$ok
  say(isTRUE(exists_now), "Table plot_access exists")
  if (!isTRUE(exists_now)) return(invisible(FALSE))

  # --- columns -------------------------------------------------------------
  cols <- DBI::dbGetQuery(con, "
    SELECT a.attname AS column_name,
           format_type(a.atttypid, a.atttypmod) AS col_type,
           a.attnotnull AS not_null
      FROM pg_attribute a
     WHERE a.attrelid = 'public.plot_access'::regclass
       AND a.attnum > 0 AND NOT a.attisdropped
     ORDER BY a.attnum")
  cli::cli_h2("Columns")
  print(cols, row.names = FALSE)
  say(!("can_read" %in% cols$column_name),
      "No can_read column - a row is read access")
  say("can_delete" %in% cols$column_name,
      "can_delete is separate from can_write")

  defaults <- DBI::dbGetQuery(con, "
    SELECT a.attname, pg_get_expr(d.adbin, d.adrelid) AS col_default
      FROM pg_attribute a
      LEFT JOIN pg_attrdef d ON d.adrelid = a.attrelid AND d.adnum = a.attnum
     WHERE a.attrelid = 'public.plot_access'::regclass
       AND a.attname IN ('can_write', 'can_delete', 'can_grant')")
  say(all(grepl("false", defaults$col_default, ignore.case = TRUE)),
      "can_write, can_delete and can_grant all default to FALSE")

  # --- constraints ---------------------------------------------------------
  cons <- DBI::dbGetQuery(con, "
    SELECT conname, contype, convalidated, pg_get_constraintdef(oid) AS definition
      FROM pg_constraint
     WHERE conrelid = 'public.plot_access'::regclass
     ORDER BY contype, conname")
  cli::cli_h2("Constraints")
  print(cons, row.names = FALSE)
  say(any(cons$contype == "f" & grepl("data_liste_plots", cons$definition)),
      "Foreign key to data_liste_plots")
  say(any(grepl("ON DELETE CASCADE", cons$definition)),
      "Deleting a plot removes its grants")
  say(any(cons$conname == "plot_access_user_plot_key"),
      "One row per (account, plot)")

  # --- row-level security --------------------------------------------------
  rls <- DBI::dbGetQuery(con, "
    SELECT relrowsecurity AS rls_enabled, relforcerowsecurity AS rls_forced
      FROM pg_class WHERE oid = 'public.plot_access'::regclass")
  say(isTRUE(rls$rls_enabled), "Row-level security enabled on plot_access")
  say(!isTRUE(rls$rls_forced),
      "FORCE not set - the owner still bypasses (no admin role on this server)")

  pol <- DBI::dbGetQuery(con, "
    SELECT policyname, cmd, roles::text AS roles, qual
      FROM pg_policies
     WHERE schemaname = 'public' AND tablename = 'plot_access'
     ORDER BY policyname")
  cli::cli_h2("Policies on plot_access")
  print(pol, row.names = FALSE)
  self <- pol[pol$policyname == "plot_access_self", , drop = FALSE]
  say(nrow(self) == 1 && grepl("current_user", self$qual[1], ignore.case = TRUE),
      "plot_access_self confines an account to its own rows")
  say(nrow(pol) == 1,
      "Exactly one policy - nobody but the owner can write a grant")

  # --- privileges ----------------------------------------------------------
  # aclexplode reports PUBLIC as grantee 0, which pg_get_userbyid() does not
  # render as a role name - so branch on the OID rather than on the text.
  privs <- DBI::dbGetQuery(con, "
    SELECT CASE WHEN a.grantee = 0 THEN 'PUBLIC'
                ELSE pg_get_userbyid(a.grantee) END AS grantee,
           (a.grantee = 0) AS is_public,
           string_agg(a.privilege_type, ', ' ORDER BY a.privilege_type) AS privs
      FROM pg_class c
           CROSS JOIN LATERAL
             aclexplode(COALESCE(c.relacl, acldefault('r', c.relowner))) a
     WHERE c.oid = 'public.plot_access'::regclass
     GROUP BY 1, 2 ORDER BY 1")
  cli::cli_h2("Privileges on plot_access")
  print(privs, row.names = FALSE)

  owner_name <- DBI::dbGetQuery(con, "
    SELECT pg_get_userbyid(relowner) AS n
      FROM pg_class WHERE oid = 'public.plot_access'::regclass")$n
  others <- privs[privs$grantee != owner_name & !privs$is_public, , drop = FALSE]
  say(!any(grepl("INSERT|UPDATE|DELETE", others$privs)),
      "No account but the owner can write a grant")
  say(!any(privs$is_public), "Nothing granted to PUBLIC")

  # --- functions -----------------------------------------------------------
  fns <- DBI::dbGetQuery(con, "
    SELECT p.proname,
           p.prosecdef AS security_definer,
           p.provolatile AS volatility,
           COALESCE(array_to_string(p.proconfig, ', '), '(none)') AS settings
      FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
     WHERE n.nspname = 'public'
       AND p.proname IN ('accessible_plots', 'plot_access_creator')
     ORDER BY p.proname")
  cli::cli_h2("Functions")
  print(fns, row.names = FALSE)
  say(nrow(fns) == 2, "Both functions exist")
  say(all(grepl("search_path", fns$settings)),
      "search_path pinned on both (P4.4)")
  ap <- fns[fns$proname == "accessible_plots", , drop = FALSE]
  say(nrow(ap) == 1 && !isTRUE(ap$security_definer),
      "accessible_plots() is SECURITY INVOKER - it reads under the caller policy")
  pc <- fns[fns$proname == "plot_access_creator", , drop = FALSE]
  say(nrow(pc) == 1 && isTRUE(pc$security_definer),
      "plot_access_creator() is SECURITY DEFINER - it must be, to insert a grant")

  # --- trigger -------------------------------------------------------------
  trg <- DBI::dbGetQuery(con, "
    SELECT tgname, tgenabled, pg_get_triggerdef(oid) AS definition
      FROM pg_trigger
     WHERE tgrelid = 'public.data_liste_plots'::regclass AND NOT tgisinternal
     ORDER BY tgname")
  cli::cli_h2("Triggers on data_liste_plots")
  print(trg[, c("tgname", "tgenabled")], row.names = FALSE)
  creator <- trg[trg$tgname == "trg_plot_access_creator", , drop = FALSE]
  say(nrow(creator) == 1, "trg_plot_access_creator is present")
  say(nrow(creator) == 1 && creator$tgenabled[1] == "O",
      "and enabled")

  # --- nothing enforced anywhere else --------------------------------------
  child_rls <- DBI::dbGetQuery(con, "
    SELECT c.relname, c.relrowsecurity AS rls_enabled
      FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public'
       AND c.relname IN ('data_individuals', 'data_traits_measures',
                         'data_ind_measures_feat', 'data_liste_sub_plots',
                         'data_subplot_feat', 'data_link_specimens')
     ORDER BY 1")
  cli::cli_h2("Child tables - should all still be FALSE at this step")
  print(child_rls, row.names = FALSE)
  say(!any(child_rls$rls_enabled),
      "No child table has row-level security yet - nothing was enforced")

  n <- DBI::dbGetQuery(con, "SELECT count(*)::int AS n FROM public.plot_access")$n
  cli::cli_alert_info("plot_access holds {n} row{?s}")

  if (pass) cli::cli_alert_success("plot_access is correctly installed")
  else      cli::cli_alert_danger("Verification failed - see above")

  invisible(pass)
}
