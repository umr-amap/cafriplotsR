# =============================================================================
# Make data_liste_plots.created_by server-asserted (P4.3)
#
# WHY THIS IS A PREREQUISITE, not a hygiene item.
#
# `created_by` already defaults to `current_user` -- add_created_by.R:67 added
# the column as `TEXT DEFAULT current_user`, and the import wizard never names
# it (.prepare_plot_data(), R/import_with_transactions.R:967), so a plot
# imported through the app records the database role that imported it. That
# part is correct today.
#
# What is not correct is that `insert_open` is `WITH CHECK (true)`
# (add_created_by.R:132-134). A client that *does* name the column may put any
# string in it. While `created_by` was only an audit column, that was a lie in
# a log. Once plot_access exists and a trigger reads NEW.created_by to write a
# grant row, the same lie **grants another account access to a plot**. So this
# has to land before inst/migrations/plot_access_table.R, not after.
#
# WHAT CHANGES
#   - `insert_open  WITH CHECK (true)` is replaced by
#     `insert_own   WITH CHECK (created_by = current_user)`
#   - the column default is re-asserted (idempotent; it is already there)
#   - optionally `created_by` is set NOT NULL, which add_created_by.R's
#     backfill already made true for every row
#
# WHAT DOES NOT CHANGE
#   Every write path in the package. `created_by` is written in exactly one
#   place in R/ -- specimen_linking_functions.R:548 -- and that targets
#   data_link_specimens, a different table. The plot import path omits the
#   column and takes the default, which satisfies the new check by
#   construction. The owner bypasses row-level security entirely.
#
# TO ROLL BACK
#   DROP POLICY insert_own ON data_liste_plots;
#   CREATE POLICY insert_open ON data_liste_plots FOR INSERT TO PUBLIC
#     WITH CHECK (true);
#   -- and, if set_not_null was used:
#   ALTER TABLE data_liste_plots ALTER COLUMN created_by DROP NOT NULL;
#
# Run as the owner of data_liste_plots. Creating a policy requires it.
# =============================================================================


#' Report the current state of created_by, read-only
#'
#' Answers the three questions the migration depends on, and the one the
#' plot_access seed depends on: does every value in `created_by` name a real
#' database role? A value that does not is a grant the seed cannot honour.
#'
#' @param con A connection to plots_transects, as the table owner.
#' @return Invisibly, a list with the pieces gathered.
report_created_by_state <- function(con) {

  stopifnot("Invalid connection" = DBI::dbIsValid(con))

  cli::cli_h1("created_by on data_liste_plots")

  owner <- DBI::dbGetQuery(con, "
    SELECT pg_get_userbyid(relowner) AS owner_name,
           pg_get_userbyid(relowner) = current_user AS i_am_owner
      FROM pg_class WHERE oid = 'public.data_liste_plots'::regclass")

  cli::cli_alert_info("Table owner: {.val {owner$owner_name}}")
  if (!isTRUE(owner$i_am_owner)) {
    cli::cli_alert_warning(
      "You are not the owner - the migration will not be able to create policies")
  }

  # --- the column itself ---------------------------------------------------
  col <- DBI::dbGetQuery(con, "
    SELECT a.attname,
           format_type(a.atttypid, a.atttypmod) AS col_type,
           a.attnotnull                        AS not_null,
           pg_get_expr(d.adbin, d.adrelid)     AS col_default
      FROM pg_attribute a
      LEFT JOIN pg_attrdef d ON d.adrelid = a.attrelid AND d.adnum = a.attnum
     WHERE a.attrelid = 'public.data_liste_plots'::regclass
       AND a.attname = 'created_by' AND NOT a.attisdropped")

  if (nrow(col) == 0) {
    cli::cli_abort(c(
      "data_liste_plots has no created_by column.",
      i = "Run inst/migrations/add_created_by.R first."))
  }
  cli::cli_h2("Column")
  print(col, row.names = FALSE)

  # --- INSERT policies -----------------------------------------------------
  pol <- DBI::dbGetQuery(con, "
    SELECT policyname, cmd, permissive, roles::text AS roles, with_check
      FROM pg_policies
     WHERE schemaname = 'public' AND tablename = 'data_liste_plots'
       AND cmd IN ('INSERT', 'ALL')
     ORDER BY policyname")
  cli::cli_h2("Policies that can admit an INSERT")
  if (nrow(pol) == 0) cli::cli_alert_warning("None - nobody but the owner can insert a plot")
  else print(pol, row.names = FALSE)

  # --- do the values name real roles? --------------------------------------
  vals <- DBI::dbGetQuery(con, "
    SELECT COALESCE(p.created_by, '(NULL)') AS created_by,
           count(*)::int                    AS n_plots,
           EXISTS (SELECT 1 FROM pg_roles r WHERE r.rolname = p.created_by)
                                            AS is_a_role,
           p.created_by = (SELECT pg_get_userbyid(relowner) FROM pg_class
                            WHERE oid = 'public.data_liste_plots'::regclass)
                                            AS is_the_owner
      FROM data_liste_plots p
     GROUP BY p.created_by
     ORDER BY is_a_role NULLS FIRST, n_plots DESC")

  cli::cli_h2("Distinct created_by values")
  print(vals, row.names = FALSE)

  n_null <- sum(vals$n_plots[vals$created_by == "(NULL)"])
  # A NULL created_by is its own problem, reported just below. Keep it out of
  # "names no role", or the two get conflated in the abort message.
  bad <- vals[vals$created_by != "(NULL)" &
              !is.na(vals$is_a_role) & !vals$is_a_role, , drop = FALSE]

  if (n_null > 0) {
    cli::cli_alert_warning(
      "{n_null} plot{?s} {?has/have} created_by NULL - NOT NULL cannot be set")
  } else {
    cli::cli_alert_success("No NULL created_by")
  }

  if (nrow(bad) > 0) {
    cli::cli_alert_danger(
      "{nrow(bad)} created_by value{?s} name{?s/} no database role:")
    print(bad, row.names = FALSE)
    cli::cli_alert_info(
      "The plot_access seed cannot write a grant for a role that does not exist.")
  } else {
    cli::cli_alert_success("Every created_by value names a real database role")
  }

  invisible(list(owner = owner, column = col, policies = pol, values = vals,
                 n_null = n_null, not_a_role = bad))
}


#' Replace insert_open with insert_own
#'
#' @param con A connection to plots_transects, as the table owner.
#' @param set_not_null Logical. Also set `created_by NOT NULL`. Refuses, rather
#'   than fails, if any row is NULL. Default `TRUE`.
#' @param dry_run Logical. `TRUE` (the default) prints the statements and
#'   changes nothing.
#' @return Invisibly `TRUE` when the change was applied.
migrate_created_by_server_asserted <- function(con, set_not_null = TRUE,
                                               dry_run = TRUE) {

  state <- report_created_by_state(con)

  if (!isTRUE(state$owner$i_am_owner)) {
    cli::cli_abort(c(
      "Only the owner of data_liste_plots can replace its policies.",
      i = "Connect as {.val {state$owner$owner_name}}."))
  }

  if (set_not_null && state$n_null > 0) {
    cli::cli_abort(c(
      "{state$n_null} plot{?s} {?has/have} created_by NULL.",
      i = "Either fix those rows or call with {.code set_not_null = FALSE}.",
      i = "Leaving them NULL means those plots grant nobody creator access."))
  }

  if (nrow(state$not_a_role) > 0) {
    cli::cli_alert_warning(c(
      "Proceeding, but note: created_by holds {nrow(state$not_a_role)} value{?s} ",
      "that name no role. This migration does not change existing rows - it ",
      "only stops new ones being written that way."))
  }

  statements <- c(
    "ALTER TABLE public.data_liste_plots
       ALTER COLUMN created_by SET DEFAULT current_user;",
    "DROP POLICY IF EXISTS insert_open ON public.data_liste_plots;",
    "DROP POLICY IF EXISTS insert_own  ON public.data_liste_plots;",
    "CREATE POLICY insert_own ON public.data_liste_plots
       FOR INSERT TO PUBLIC
       WITH CHECK (created_by = current_user);",
    "COMMENT ON POLICY insert_own ON public.data_liste_plots IS
       'An account may only insert a plot attributed to itself. Load-bearing: the
        plot_access creator trigger turns created_by into a grant.';"
  )

  if (set_not_null) {
    statements <- c(statements,
      "ALTER TABLE public.data_liste_plots ALTER COLUMN created_by SET NOT NULL;")
  }

  cli::cli_h2("Statements")
  for (s in statements) cli::cli_code(gsub("[[:space:]]+", " ", trimws(s)))

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

  cli::cli_alert_success("created_by is now server-asserted on insert")
  check_created_by_server_asserted(con)
  invisible(TRUE)
}


#' Verify the applied state
#'
#' @param con A connection to plots_transects.
#' @return Invisibly `TRUE` if every check passes.
check_created_by_server_asserted <- function(con) {

  cli::cli_h2("Verification")
  pass <- TRUE

  pol <- DBI::dbGetQuery(con, "
    SELECT policyname, with_check
      FROM pg_policies
     WHERE schemaname = 'public' AND tablename = 'data_liste_plots'
       AND cmd = 'INSERT' ORDER BY policyname")
  print(pol, row.names = FALSE)

  if ("insert_open" %in% pol$policyname) {
    cli::cli_alert_danger("insert_open is still present")
    pass <- FALSE
  } else {
    cli::cli_alert_success("insert_open is gone")
  }

  own <- pol[pol$policyname == "insert_own", , drop = FALSE]
  if (nrow(own) == 1 && grepl("created_by", own$with_check[1]) &&
      grepl("CURRENT_USER", own$with_check[1], ignore.case = TRUE)) {
    cli::cli_alert_success("insert_own checks created_by against current_user")
  } else {
    cli::cli_alert_danger("insert_own is missing or does not check created_by")
    pass <- FALSE
  }

  col <- DBI::dbGetQuery(con, "
    SELECT a.attnotnull AS not_null, pg_get_expr(d.adbin, d.adrelid) AS col_default
      FROM pg_attribute a
      LEFT JOIN pg_attrdef d ON d.adrelid = a.attrelid AND d.adnum = a.attnum
     WHERE a.attrelid = 'public.data_liste_plots'::regclass
       AND a.attname = 'created_by'")

  if (!is.na(col$col_default[1]) &&
      grepl("CURRENT_USER", col$col_default[1], ignore.case = TRUE)) {
    cli::cli_alert_success("Column default is current_user")
  } else {
    cli::cli_alert_danger("Column default is {.val {col$col_default[1]}}")
    pass <- FALSE
  }

  cli::cli_alert_info("created_by NOT NULL: {col$not_null[1]}")

  if (pass) cli::cli_alert_success("created_by is server-asserted")
  else      cli::cli_alert_danger("Verification failed - see above")

  invisible(pass)
}
