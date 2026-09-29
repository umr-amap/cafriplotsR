# =============================================================================
# Step 5: put the six child tables under plot scope
#
# WHAT IT CHANGES
#
# Row-level security guards data_liste_plots (2,194 rows) and nothing below it.
# Every account that can log in can read and write all ~3.2M rows in the six
# tables underneath, whatever plots it was granted - which makes the leak a
# publishable geolocated attributed occurrence dataset, not a list of plot
# names. This enables row-level security on those six and gives each four
# policies driven by plot_access.
#
# data_liste_plots is deliberately untouched. It keeps its ~117 per-account
# policies, insert_own, and the three creator_access_* policies. Two reasons:
# plot_access_seed.R proved the two agree plot-for-plot for all 35 accounts, so
# nothing is gained by swapping it now; and creator_access_select is what lets
# `INSERT INTO data_liste_plots ... RETURNING` work at all, because at the
# moment the returned row is checked the creator trigger has not fired yet and
# plot_access cannot authorise it. Leaving that table alone means step 5 cannot
# change how a plot is created.
#
# THE PREDICATES LIVE IN R/plot_scope_policies.R
#
# Not here. They have to be right, and being right is testable without a
# database: tests/testthat/test-plot-scope-policies.R checks all 132 properties
# of the generated SQL offline. This file is the part that runs once.
#
# WHAT WILL NOT BREAK, AND WHY THAT WAS CHECKED RATHER THAN ASSUMED
#
# The import wizard keeps working with no intervention. Traced end to end:
#
#   plot         import_with_transactions.R:235     INSERT ... VALUES RETURNING
#   subplot      census_import_transaction.R:139    INSERT ... VALUES RETURNING
#   individuals  census_import_transaction.R:173    INSERT ... VALUES
#   measurements add_functions.R:1724               temp table, INSERT ... SELECT
#   ind features mod_feat_step6_import.R:939        .db_append_table()
#   sub features mod_census_information.R:540       .db_append_table()
#
# The chain is self-starting: trg_plot_access_creator is AFTER INSERT on
# data_liste_plots and row-level AFTER triggers fire at end of *statement*, so
# by the time the subplot INSERT runs, plot_access already holds the row with
# can_write. Every later statement in the transaction passes on it.
#
# data_ind_measures_feat is the subtle one. The wizard never sends
# id_table_liste_plots; the BEFORE INSERT trigger derives it. PostgreSQL runs
# BEFORE triggers before evaluating WITH CHECK, so the policy sees the derived
# plot - which is why the policy keys on the column rather than requiring the
# client to supply it.
#
# No COPY. RPostgres implements dbWriteTable()/dbAppendTable() with COPY FROM,
# and PostgreSQL refuses COPY FROM on a table where row-level security applies
# to the caller. R/db_append.R exists for exactly this, and every write to the
# seven tables now goes through INSERT ... VALUES, INSERT ... SELECT, or
# .db_append_table(). Verified against all remaining dbWriteTable call sites:
# they target specimens, traitlist, methodslist, table_colnam, subplotype_list,
# followup_updates_*, the taxa database, or temp tables.
#
# DELETE ON THE CHILDREN FOLLOWS can_write, NOT can_delete
#
# can_delete exists to stop an account destroying a plot and cascading through
# six tables. Deleting a measurement row is not that - it is ordinary curation,
# and safe_delete_individual_features() and safe_delete_individuals() do it as
# part of re-importing. Keying child DELETE on can_delete would break both for
# the 13,913 grants that carry write without delete. So can_delete governs
# data_liste_plots and the children follow can_write, which still narrows every
# account from all 2,194 plots to its own.
#
# THE TRAP
#
# Every one of these failure modes hits non-owners only. dauby owns all 38
# tables and bypasses row-level security, so every test run as dauby passes.
# COPY FROM is likewise refused only when row-level security applies to the
# caller. rehearse_child_rls_as_role() is here because a catalog check cannot
# tell you the wizard still works; a SET ROLE session running real inserts can.
#
# ROLLBACK
#
# One statement per table: ALTER TABLE ... DISABLE ROW LEVEL SECURITY takes
# effect immediately and leaves the policies in place to re-enable. The
# migration writes restore_child_rls.sql and reads it back before it changes
# anything.
#
# HOW TO RUN
#
#   source(system.file("migrations", "plot_access_child_rls.R",
#                      package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#
#   report_child_rls_state(con)                        # read-only
#   rehearse_child_rls_as_role(con, "some_account")    # read-only: rolls back
#   migrate_plot_access_child_rls(con)                 # rehearsal, prints SQL
#   migrate_plot_access_child_rls(con, dry_run = FALSE)
#   check_plot_access_child_rls(con)
# =============================================================================


# --- connection plumbing -----------------------------------------------------

.child_con <- function(con) {
  if (inherits(con, "Pool")) pool::poolCheckout(con) else con
}

.child_release <- function(con, actual) {
  if (inherits(con, "Pool") && !is.null(actual)) pool::poolReturn(actual)
  invisible(NULL)
}


# A failed statement aborts the whole transaction, and tryCatch() does not undo
# that - every later statement then fails with "current transaction is aborted".
# One bad column in inst/scripts/check_plot_access_cost.R cost two whole
# sections of a run before this was added there.
.child_savepoint <- function(con, name, expr) {

  DBI::dbExecute(con, paste("SAVEPOINT", name))

  out <- tryCatch(expr, error = function(e) {
    DBI::dbExecute(con, paste("ROLLBACK TO SAVEPOINT", name))
    structure(list(message = conditionMessage(e)), class = "child_rls_failure")
  })

  DBI::dbExecute(con, paste("RELEASE SAVEPOINT", name))
  out
}


# --- column validation -------------------------------------------------------

#' Check every column the predicates name against the catalog
#'
#' The first run of inst/scripts/check_plot_access_cost.R guessed
#' `id_ind_measures_feat`, which does not exist, and the failure took the whole
#' transaction with it. Nothing is trusted here.
#'
#' @return Data frame of the referenced columns with a `present` flag.
#' @noRd
.validate_scope_columns <- function(con) {

  want <- CafriplotsR:::.plot_scope_referenced_columns()

  have <- DBI::dbGetQuery(con, "
    SELECT c.relname AS table_name, a.attname AS column_name
      FROM pg_attribute a
      JOIN pg_class     c ON c.oid = a.attrelid
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND a.attnum > 0 AND NOT a.attisdropped")

  want$present <- paste(want$table_name, want$column_name) %in%
                  paste(have$table_name, have$column_name)
  want
}


# --- reachability ------------------------------------------------------------

# A row that reaches no plot matches no policy and goes invisible to everyone
# but the owner. delete_orphan_plot_rows.R cleaned the subplots, the subplot
# features and three measurements; it never checked data_individuals itself.
.unreachable_sql <- function(table_name) {

  route <- CafriplotsR:::.plot_scope_route(table_name)

  reach_own <- function(col) paste0(
    "EXISTS (SELECT 1 FROM public.data_liste_plots p",
    " WHERE p.id_liste_plots = t.", col, ")")

  reach_via <- function(v) paste0(
    "EXISTS (SELECT 1 FROM public.", v$table, " v",
    " JOIN public.data_liste_plots p ON p.id_liste_plots = v.", v$remote_key,
    " WHERE v.", v$on_remote, " = t.", v$on_local, ")")

  branches <- c(
    if (!is.null(route$column))    reach_own(route$column),
    if (!is.null(route$via))       reach_via(route$via),
    if (!is.null(route$or_column)) reach_own(route$or_column))

  paste0("SELECT count(*) AS n FROM public.", table_name, " t",
         " WHERE NOT (", paste(branches, collapse = " OR "), ")")
}


# --- the read-only report ----------------------------------------------------

#' @title What step 5 would change, before it changes anything
#' @description
#' Four questions, none of which needs a write:
#'
#' 1. Does every column the predicates name exist?
#' 2. Which tables already have row-level security, and what policies?
#' 3. How many rows reach no plot, and would go invisible?
#' 4. How many rows does each account see now, and how many after?
#'
#' Question 4 is the one that matters operationally. Nothing errors when scope
#' narrows - queries just return fewer rows - so a collaborator's script gets
#' quietly shorter output. This is what to tell them before, not after.
#'
#' @param con A connection to the main database.
#' @return Invisibly, a list of the four data frames.
#' @export
report_child_rls_state <- function(con) {

  actual <- .child_con(con)
  on.exit(.child_release(con, actual), add = TRUE)

  tables <- CafriplotsR:::.plot_scope_child_tables()

  cli::cli_h1("Step 5: plot scope on the child tables")

  # --- 1. columns ------------------------------------------------------------
  cli::cli_h2("Columns the policies name")

  cols <- .validate_scope_columns(actual)
  if (all(cols$present)) {
    cli::cli_alert_success(
      "All {nrow(cols)} referenced column{?s} exist")
  } else {
    cli::cli_alert_danger("Missing columns - the migration would fail:")
    print(cols[!cols$present, , drop = FALSE], row.names = FALSE)
  }

  # --- 2. current state -----------------------------------------------------
  cli::cli_h2("Row-level security as it stands")

  state <- DBI::dbGetQuery(actual, glue::glue_sql("
    SELECT c.relname       AS table_name,
           c.relrowsecurity AS rls_enabled,
           c.relforcerowsecurity AS rls_forced,
           (SELECT count(*) FROM pg_policy p WHERE p.polrelid = c.oid)
             AS n_policies,
           c.reltuples::bigint AS est_rows
      FROM pg_class     c
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public'
       AND c.relname IN ({tables*})
     ORDER BY c.relname", .con = actual))

  print(state, row.names = FALSE)

  if (any(state$rls_forced)) {
    cli::cli_alert_danger(
      "FORCE ROW LEVEL SECURITY is set on {.val {state$table_name[state$rls_forced]}}.
       Table ownership is the only way round row-level security on this server -
       there is no admin role - so forcing it would lock the owner out too.")
  }

  already <- state$table_name[state$rls_enabled]
  if (length(already)) {
    cli::cli_alert_warning(
      "Already enabled on {.val {already}} - the migration will refuse rather
       than add a second layer to something it did not install.")
  }

  # --- 3. reachability ------------------------------------------------------
  cli::cli_h2("Rows that reach no plot")
  cli::cli_alert_info(
    "These would match no policy and become invisible to every account but the
     owner. data_individuals has never been counted: delete_orphan_plot_rows.R
     cleaned the subplots and three measurements, not this.")

  reach <- do.call(rbind, lapply(tables, function(tb) {
    n <- DBI::dbGetQuery(actual, .unreachable_sql(tb))$n
    data.frame(table_name = tb, unreachable = as.numeric(n),
               stringsAsFactors = FALSE)
  }))
  print(reach, row.names = FALSE)

  if (sum(reach$unreachable) == 0) {
    cli::cli_alert_success("Every row in all six tables reaches a plot")
  } else {
    cli::cli_alert_danger(
      "{sum(reach$unreachable)} row{?s} would go invisible. Characterise them
       before enabling - the same shape of problem that
       delete_orphan_plot_rows.R exported to CSV before deleting.")
  }

  # --- 4. visibility delta --------------------------------------------------
  cli::cli_h2("Rows visible per account, now and after")
  cli::cli_alert_info(
    "One grouped pass per table. Two of them are over two million rows, so this
     section takes a little while.")

  # Rows per plot, one pass per table, rather than one count per account per
  # table - 6 queries instead of 210.
  per_plot <- lapply(tables, function(tb) {

    route <- CafriplotsR:::.plot_scope_route(tb)

    sql <- if (!is.null(route$column)) {

      paste0("SELECT t.", route$column, " AS id_liste_plots, count(*) AS n",
             " FROM public.", tb, " t GROUP BY 1")

    } else {

      v <- route$via

      # LEFT JOIN and COALESCE, not an inner join: data_link_specimens has 277
      # rows that reach a plot only through its own id_liste_plots, and an inner
      # join would drop them from the delta.
      key <- if (is.null(route$or_column)) {
        paste0("v.", v$remote_key)
      } else {
        paste0("COALESCE(v.", v$remote_key, ", t.", route$or_column, ")")
      }

      paste0("SELECT ", key, " AS id_liste_plots, count(*) AS n",
             " FROM public.", tb, " t",
             " LEFT JOIN public.", v$table, " v ON v.", v$on_remote,
             " = t.", v$on_local, " GROUP BY 1")
    }

    out <- DBI::dbGetQuery(actual, sql)
    out$n <- as.numeric(out$n)
    out
  })
  names(per_plot) <- tables

  grants <- DBI::dbGetQuery(actual,
    "SELECT db_user, id_liste_plots FROM public.plot_access")

  accounts <- sort(unique(grants$db_user))

  delta <- do.call(rbind, lapply(accounts, function(u) {

    mine <- grants$id_liste_plots[grants$db_user == u]

    row <- data.frame(db_user = u, n_plots = length(mine),
                      stringsAsFactors = FALSE)

    for (tb in tables) {
      pp <- per_plot[[tb]]
      row[[paste0(tb, "_after")]] <- sum(pp$n[pp$id_liste_plots %in% mine])
      row[[paste0(tb, "_now")]]   <- sum(pp$n)
    }
    row
  }))

  summary_delta <- data.frame(
    db_user = delta$db_user,
    n_plots = delta$n_plots,
    rows_now   = rowSums(delta[, paste0(tables, "_now"),   drop = FALSE]),
    rows_after = rowSums(delta[, paste0(tables, "_after"), drop = FALSE]),
    stringsAsFactors = FALSE)

  summary_delta$keeps_pct <- round(
    100 * summary_delta$rows_after / pmax(summary_delta$rows_now, 1), 1)

  print(summary_delta[order(-summary_delta$rows_after), ], row.names = FALSE)

  cli::cli_alert_info(
    "{nrow(summary_delta)} account{?s}. {.code rows_now} is what each can read
     today across the six tables; {.code rows_after} is what it would read
     under plot scope. The owner is absent from plot_access and unaffected.")

  # --- 5. the prerequisite grant -------------------------------------------
  cli::cli_h2("SELECT on plot_access")
  cli::cli_alert_info(
    "Every policy reads plot_access, and a policy expression runs as the calling
     account - so an account without SELECT on it gets
     {.emph permission denied for table plot_access} instead of its rows.")

  priv <- DBI::dbGetQuery(actual, "
    SELECT r.rolname AS db_user,
           has_table_privilege(r.rolname, 'public.plot_access', 'SELECT')
             AS can_read_plot_access
      FROM pg_roles r
     WHERE r.rolcanlogin
       AND r.rolname IN (SELECT DISTINCT db_user FROM public.plot_access)
     ORDER BY 1")

  missing <- priv$db_user[!priv$can_read_plot_access]

  if (length(missing) == 0) {
    cli::cli_alert_success(
      "All {nrow(priv)} account{?s} can read plot_access")
  } else {
    cli::cli_alert_danger(
      "{length(missing)} account{?s} cannot: {.val {missing}}")
    cli::cli_alert_info(
      "The fix that stops this mattering:
       {.code GRANT SELECT ON public.plot_access TO PUBLIC;} - it leaks nothing,
       because plot_access_self restricts every reader to its own rows.")
  }

  invisible(list(columns = cols, state = state, reachable = reach,
                 delta = delta, privileges = priv))
}


# --- the SET ROLE rehearsal --------------------------------------------------

#' @title Prove the import chain still works, as somebody else
#' @description
#' Runs the five inserts the import wizard makes - plot, subplot, individual,
#' measurement, individual feature - inside a transaction that always rolls
#' back, with `SET LOCAL ROLE` so the policies actually apply.
#'
#' This is the only verification that answers the question. The owner bypasses
#' row-level security, so a catalog check and a run as `dauby` both pass whether
#' the policies are right or not.
#'
#' Each insert clones an existing row of one of the account's own plots and
#' changes only what it must, so every foreign key is satisfied by construction
#' - the same trick `plot_access_seed.R` uses to make its inserts FK-safe. A
#' step with nothing to clone is skipped and said to be skipped, not failed.
#'
#' Safe to run before the migration: it then shows the chain passing with no
#' policies in the way, which is the baseline the run after should match.
#'
#' @param con A connection to the main database.
#' @param role Character. The account to impersonate. Must appear in
#'   `plot_access` and the current role must be able to `SET ROLE` to it.
#' @return Invisibly, a data frame with one row per step.
#' @export
rehearse_child_rls_as_role <- function(con, role) {

  stopifnot(is.character(role), length(role) == 1L)

  actual <- .child_con(con)
  on.exit(.child_release(con, actual), add = TRUE)

  cli::cli_h1("Import rehearsal as {.val {role}}")

  # A plot the role can write, with enough underneath it to clone from.
  plot_id <- DBI::dbGetQuery(actual, glue::glue_sql("
    SELECT a.id_liste_plots
      FROM public.plot_access a
     WHERE a.db_user = {role} AND a.can_write
     ORDER BY (SELECT count(*) FROM public.data_individuals i
                WHERE i.id_table_liste_plots_n = a.id_liste_plots) DESC
     LIMIT 1", .con = actual))

  if (nrow(plot_id) == 0) {
    cli::cli_alert_danger(
      "{.val {role}} has no writable plot in plot_access - nothing to rehearse")
    return(invisible(NULL))
  }
  plot_id <- plot_id$id_liste_plots[1]
  cli::cli_alert_info("Cloning from plot {.val {plot_id}}")

  steps <- character(0)
  result <- list()

  DBI::dbBegin(actual)

  # SET LOCAL reverts when the transaction ends, however it ends.
  ok <- tryCatch({
    DBI::dbExecute(actual, glue::glue_sql(
      "SET LOCAL ROLE {`role`}", .con = actual))
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Cannot SET ROLE to {.val {role}}: {e$message}")
    cli::cli_alert_info(
      "Grant it to yourself first: {.code GRANT {role} TO current_user;}")
    FALSE
  })

  if (!ok) {
    DBI::dbRollback(actual)
    return(invisible(NULL))
  }

  # An INSERT ... SELECT whose source row the role cannot see inserts nothing
  # and raises nothing. That is not a pass: it means the policy under test was
  # never exercised, so it is reported as a skip.
  step <- function(label, sql, sp) {

    out <- .child_savepoint(actual, sp, DBI::dbGetQuery(actual, sql))

    if (inherits(out, "child_rls_failure")) {
      cli::cli_alert_danger("{label}: {out$message}")
      result[[label]] <<- list(ok = FALSE, message = out$message, id = NA)
      return(NA_integer_)
    }

    if (nrow(out) == 0) {
      cli::cli_alert_warning(
        "{label}: nothing inserted - no source row visible to clone, so this
         policy was not exercised")
      result[[label]] <<- list(ok = NA, id = NA,
                               message = "no source row visible")
      return(NA_integer_)
    }

    id <- as.integer(out[[1]][1])
    cli::cli_alert_success("{label}: inserted, id {id}")
    result[[label]] <<- list(ok = TRUE, message = "", id = id)
    id
  }

  # 1. a plot, cloned from one of theirs. created_by is left to its default so
  #    insert_own sees current_user; the RETURNING is what creator_access_select
  #    has to carry, since the creator trigger has not fired yet.
  new_plot <- step("plot", glue::glue_sql("
    INSERT INTO public.data_liste_plots (plot_name, method, country)
    SELECT 'RLS_REHEARSAL_' || {as.character(Sys.getpid())}, method, country
      FROM public.data_liste_plots WHERE id_liste_plots = {plot_id}
    RETURNING id_liste_plots", .con = actual), "sp_plot")

  # 2. a subplot under the new plot.
  new_sub <- if (!is.na(new_plot)) step("subplot", glue::glue_sql("
    INSERT INTO public.data_liste_sub_plots
           (id_table_liste_plots, id_type_sub_plot, typevalue)
    SELECT {new_plot}, id_type_sub_plot, typevalue
      FROM public.data_liste_sub_plots
     WHERE id_table_liste_plots = {plot_id} LIMIT 1
    RETURNING id_sub_plots", .con = actual), "sp_sub") else NA_integer_

  # 3. an individual on the new plot.
  new_ind <- if (!is.na(new_plot)) step("individual", glue::glue_sql("
    INSERT INTO public.data_individuals (id_table_liste_plots_n, tag)
    SELECT {new_plot}, tag
      FROM public.data_individuals
     WHERE id_table_liste_plots_n = {plot_id} LIMIT 1
    RETURNING id_n", .con = actual), "sp_ind") else NA_integer_

  # 4. a measurement on the new individual. Routes through the individual, so
  #    this is the two-hop policy under test.
  new_meas <- if (!is.na(new_ind)) step("measurement", glue::glue_sql("
    INSERT INTO public.data_traits_measures
           (id_data_individuals, id_trait, traitvalue)
    SELECT {new_ind}, m.id_trait, m.traitvalue
      FROM public.data_traits_measures m
      JOIN public.data_individuals i ON i.id_n = m.id_data_individuals
     WHERE i.id_table_liste_plots_n = {plot_id} LIMIT 1
    RETURNING id_trait_measures", .con = actual), "sp_meas") else NA_integer_

  # 5. an individual feature. id_table_liste_plots is not supplied - the BEFORE
  #    trigger derives it, and WITH CHECK is evaluated after that trigger runs.
  #    If the ordering were the other way this would fail on NOT NULL.
  if (!is.na(new_meas)) step("individual feature", glue::glue_sql("
    INSERT INTO public.data_ind_measures_feat
           (id_trait_measures, id_trait, typevalue)
    SELECT {new_meas}, f.id_trait, f.typevalue
      FROM public.data_ind_measures_feat f LIMIT 1
    RETURNING id_trait_measures", .con = actual), "sp_feat")

  # 6. and what it can read, which is the other half of the question.
  vis <- .child_savepoint(actual, "sp_read", {
    do.call(rbind, lapply(CafriplotsR:::.plot_scope_child_tables(),
      function(tb) data.frame(
        table_name = tb,
        visible = as.numeric(DBI::dbGetQuery(actual,
          paste0("SELECT count(*) AS n FROM public.", tb))$n),
        stringsAsFactors = FALSE)))
  })

  DBI::dbRollback(actual)
  cli::cli_alert_info("Transaction rolled back - nothing was written")

  if (!inherits(vis, "child_rls_failure")) {
    cli::cli_h2("Rows {.val {role}} could read during the rehearsal")
    print(vis, row.names = FALSE)
  }

  out <- do.call(rbind, lapply(names(result), function(k) data.frame(
    step = k, ok = result[[k]]$ok, id = result[[k]]$id,
    message = result[[k]]$message, stringsAsFactors = FALSE)))

  if (!is.null(out)) {
    n_fail <- sum(!is.na(out$ok) & !out$ok)
    n_skip <- sum(is.na(out$ok))

    if (n_fail == 0 && n_skip == 0) {
      cli::cli_alert_success(
        "All {nrow(out)} insert{?s} passed as {.val {role}} - the wizard chain
         works unattended")
    } else if (n_fail == 0) {
      cli::cli_alert_warning(
        "{sum(out$ok, na.rm = TRUE)} passed, {n_skip} skipped for want of a
         source row. The skipped steps prove nothing either way - rehearse with
         an account that has data in every table.")
    } else {
      cli::cli_alert_danger(
        "{n_fail} step{?s} failed as {.val {role}} - the wizard would not
         complete for this account")
    }
  }

  invisible(out)
}


# --- the migration -----------------------------------------------------------

#' @title Enable plot scope on the six child tables
#' @description
#' One transaction. Half-enabled is the bad state - some tables filtered and
#' some not, with no record of which - so either all six go under scope or none
#' does. `ENABLE ROW LEVEL SECURITY` and `CREATE POLICY` take ACCESS EXCLUSIVE
#' but are metadata-only, so the locks are brief; they will queue behind a
#' long-running reader.
#'
#' @param con A connection to the main database.
#' @param dry_run Logical. `TRUE` (the default) prints every statement and
#'   changes nothing.
#' @param restore_file Path for the rollback script. Written and read back
#'   before anything changes.
#' @return Invisibly, the statements.
#' @export
migrate_plot_access_child_rls <- function(con, dry_run = TRUE,
                                          restore_file = "restore_child_rls.sql") {

  actual <- .child_con(con)
  on.exit(.child_release(con, actual), add = TRUE)

  tables <- CafriplotsR:::.plot_scope_child_tables()

  cli::cli_h1("Plot scope on the child tables{if (dry_run) ' (rehearsal)' else ''}")

  # --- guards ---------------------------------------------------------------

  cols <- .validate_scope_columns(actual)
  if (!all(cols$present)) {
    cli::cli_abort(c(
      "Columns the policies name do not exist:",
      x = paste(cols$table_name[!cols$present], cols$column_name[!cols$present])))
  }

  if (!CafriplotsR:::.plot_access_present(actual)) {
    cli::cli_abort(c(
      "plot_access is missing - every policy here reads it.",
      i = "Apply plot_access_table.R and plot_access_seed.R first."))
  }

  n_grants <- DBI::dbGetQuery(actual,
    "SELECT count(*) AS n FROM public.plot_access")$n
  if (n_grants == 0) {
    cli::cli_abort(c(
      "plot_access is empty - enabling these policies would hide every row from
       every account.",
      i = "Run plot_access_seed.R first."))
  }
  cli::cli_alert_info("{n_grants} grant{?s} in plot_access")

  state <- DBI::dbGetQuery(actual, glue::glue_sql("
    SELECT c.relname AS table_name, c.relrowsecurity AS rls_enabled
      FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND c.relname IN ({tables*})", .con = actual))

  if (nrow(state) != length(tables)) {
    cli::cli_abort("Expected {length(tables)} tables, found {nrow(state)}")
  }

  if (any(state$rls_enabled)) {
    cli::cli_abort(c(
      "Row-level security is already enabled on
       {.val {state$table_name[state$rls_enabled]}}.",
      i = "Refusing rather than layering policies onto something this migration
           did not install. Inspect with report_child_rls_state()."))
  }

  # --- the restore script, written and read back first ---------------------

  restore <- c(
    "-- Undo step 5: plot scope on the child tables.",
    "-- DISABLE takes effect immediately; the DROPs are only so a re-run of",
    "-- the migration starts clean.",
    paste0("-- written ", Sys.Date(), " by ",
           DBI::dbGetQuery(actual, "SELECT current_user AS u")$u),
    "",
    unlist(lapply(tables, CafriplotsR:::.plot_scope_rollback_statements)))

  if (!dry_run) {
    writeLines(restore, restore_file)
    back <- readLines(restore_file)
    if (!identical(back, restore)) {
      cli::cli_abort("{.file {restore_file}} did not read back as written")
    }
    cli::cli_alert_success(
      "Rollback script at {.file {normalizePath(restore_file)}}
       ({length(restore)} lines, read back and verified)")
  }

  # --- the statements ------------------------------------------------------

  statements <- unlist(lapply(tables,
                              CafriplotsR:::.plot_scope_policy_statements))

  cli::cli_h2("{length(statements)} statement{?s}")

  if (dry_run) {
    for (s in statements) cli::cli_verbatim(s)
    cli::cli_h2("Rollback")
    for (s in restore) cli::cli_verbatim(s)
    cli::cli_alert_info(
      "Rehearsal only. Run with {.code dry_run = FALSE} to apply.")
    cli::cli_alert_warning(
      "Before applying: rehearse_child_rls_as_role(con, \"<an account>\") -
       nothing you can run as the owner will show you whether this works, since
       the owner bypasses row-level security.")
    return(invisible(statements))
  }

  DBI::dbBegin(actual)

  applied <- tryCatch({
    for (s in statements) DBI::dbExecute(actual, s)
    DBI::dbCommit(actual)
    TRUE
  }, error = function(e) {
    tryCatch(DBI::dbRollback(actual), error = function(e2) NULL)
    cli::cli_abort(c("Nothing applied, transaction rolled back.",
                     x = conditionMessage(e)))
  })

  cli::cli_alert_success("{length(statements)} statement{?s} applied")
  cli::cli_alert_warning(
    "Now run rehearse_child_rls_as_role(con, \"<an account>\"). If it fails,
     restore with {.file {restore_file}}.")

  invisible(statements)
}


# --- verification ------------------------------------------------------------

#' @title Verify step 5 landed as intended
#' @description
#' Checks the catalog, and checks the policy expressions against the ones
#' `R/plot_scope_policies.R` generates rather than against a description of
#' them - so drift in either is caught.
#'
#' Says plainly what it cannot check: whether a non-owner can still import.
#' Only `rehearse_child_rls_as_role()` answers that.
#'
#' @param con A connection to the main database.
#' @return Invisibly, `TRUE` when every check passes.
#' @export
check_plot_access_child_rls <- function(con) {

  actual <- .child_con(con)
  on.exit(.child_release(con, actual), add = TRUE)

  tables <- CafriplotsR:::.plot_scope_child_tables()
  ok <- TRUE
  say <- function(pass, msg) {
    if (pass) cli::cli_alert_success(msg) else {
      cli::cli_alert_danger(msg); ok <<- FALSE }
  }

  cli::cli_h1("Verifying plot scope on the child tables")

  state <- DBI::dbGetQuery(actual, glue::glue_sql("
    SELECT c.relname AS table_name, c.relrowsecurity AS enabled,
           c.relforcerowsecurity AS forced
      FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND c.relname IN ({tables*})
     ORDER BY 1", .con = actual))

  say(all(state$enabled),
      "Row-level security enabled on all {nrow(state)} table{?s}")

  # Ownership is the only route past row-level security on this server. Forcing
  # it would lock the owner out with no way back.
  say(!any(state$forced), "FORCE ROW LEVEL SECURITY set on none of them")

  pol <- DBI::dbGetQuery(actual, glue::glue_sql("
    SELECT tablename AS table_name, policyname, cmd, permissive, qual, with_check
      FROM pg_policies
     WHERE schemaname = 'public' AND tablename IN ({tables*})
       AND policyname LIKE 'plot_scope_%'
     ORDER BY tablename, policyname", .con = actual))

  say(nrow(pol) == 4L * length(tables),
      "{nrow(pol)} plot_scope policies, expected {4L * length(tables)}")

  say(all(pol$permissive == "PERMISSIVE"),
      "All permissive - a restrictive policy here would AND with the per-account
       policies on data_liste_plots instead of standing on its own")

  for (tb in tables) {
    cmds <- sort(pol$cmd[pol$table_name == tb])
    say(identical(cmds, c("DELETE", "INSERT", "SELECT", "UPDATE")),
        "{tb}: all four commands covered")
  }

  # Every policy reads plot_access, and a policy runs as the caller.
  priv <- DBI::dbGetQuery(actual, "
    SELECT count(*) AS n
      FROM pg_roles r
     WHERE r.rolcanlogin
       AND r.rolname IN (SELECT DISTINCT db_user FROM public.plot_access)
       AND NOT has_table_privilege(r.rolname, 'public.plot_access', 'SELECT')")

  say(priv$n == 0,
      "Every account in plot_access can read it ({priv$n} cannot)")

  # can_delete must not have leaked into a child policy: it would break
  # safe_delete_individual_features() for the write-without-delete grants.
  leaked <- pol$policyname[grepl("can_delete", paste(pol$qual, pol$with_check))]
  say(length(leaked) == 0,
      "No child policy keys on can_delete ({length(leaked)} do{?es/})")

  say(all(!is.na(pol$with_check[pol$cmd == "UPDATE"])),
      "Every UPDATE policy carries WITH CHECK as well as USING")

  cli::cli_h2("What this cannot tell you")
  cli::cli_alert_info(
    "Whether a non-owner can still import. You own these tables and bypass
     row-level security, so this whole check passes either way. Run
     {.code rehearse_child_rls_as_role(con, \"<an account>\")}.")

  if (ok) cli::cli_alert_success("All catalog checks passed")
  invisible(ok)
}
