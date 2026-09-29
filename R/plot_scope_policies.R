# =============================================================================
# The plot-scope row-level security predicates
#
# Step 5 of inst/docs/PLAN_SECURITY_REMEDIATION.md: row-level security guards
# data_liste_plots (2,194 rows) and nothing below it, so ~3.2M rows in six
# child tables are readable and writable by every account regardless of which
# plots it was granted. This file builds the policy expressions that close it.
#
# It is here rather than in inst/migrations/ because the predicates have to be
# *right*, and being right is testable without a database. The migration that
# installs them is inst/migrations/plot_access_child_rls.R and it calls into
# these functions; the SQL text has one definition, checked by
# tests/testthat/test-plot-scope-policies.R.
#
# THREE DECISIONS WORTH THE COMMENT
#
# 1. The grant list is a subquery, not a literal array of plot ids.
#    inst/scripts/check_plot_access_cost.R measured both. A literal array is
#    25-55% faster but needs one policy per account per table - 245 of them,
#    rewritten on every grant change. The subquery needs seven and is read from
#    plot_access at query time. Before denormalise_ind_measures_plot.R the gap
#    was 3x and the choice was arguable; at 0 hops it is not.
#
# 2. DELETE on the child tables keys on can_write, not can_delete.
#    can_delete exists to stop an account destroying a plot and cascading
#    through six tables. Deleting a measurement row is not that: it is ordinary
#    curation, and safe_delete_individual_features() and
#    safe_delete_individuals() do it as a normal part of re-importing. Keying
#    child DELETE on can_delete would break both for the 13,913 grants that
#    carry write without delete. So can_delete governs data_liste_plots, and
#    the children follow can_write - which still narrows every account from all
#    2,194 plots to its own.
#
# 3. data_traits_measures routes through data_individuals, never through its
#    own id_table_liste_plots. That denormalised column is NULL on 256,179 rows
#    and names the wrong plot on 8,575 more; keying on it would orphan the
#    first set and mis-scope the second. See inst/scripts/check_traits_plot_key.R.
# =============================================================================


#' @title The six child tables step 5 puts under row-level security
#' @description
#' `data_liste_plots` is deliberately absent. It already has row-level security
#' and ~117 per-account policies that `plot_access_seed.R` proved agree with
#' `plot_access` plot-for-plot, plus `insert_own` and the three
#' `creator_access_*` policies the import wizard depends on. Swapping it over to
#' `plot_access` is a separate migration with a different risk profile, and
#' leaving it alone here means step 5 cannot change how a plot is created.
#'
#' @return Character vector of table names.
#' @seealso [.plot_scope_tables()], which is all seven.
#' @keywords internal
#' @export
.plot_scope_child_tables <- function() {
  c("data_liste_sub_plots", "data_subplot_feat", "data_individuals",
    "data_traits_measures", "data_ind_measures_feat", "data_link_specimens")
}


#' @title How a child table reaches the plot that owns its rows
#' @description
#' Each entry says which column a grant is compared against and, when the table
#' has no plot column of its own, which table to reach through.
#'
#' `hops` is recorded because it is what the cost tracks:
#' `inst/scripts/check_plot_access_cost.R` found predicate time follows the
#' number of joins between a row and its plot, not the shape of the predicate.
#'
#' @param table_name Character of length 1.
#' @return A list with `hops`, and either `column` (the table's own plot key) or
#'   `via` (a list of `table`, `on_local`, `on_remote`, `remote_key`). May also
#'   carry `or_column`, a second branch for rows the join cannot reach.
#' @keywords internal
#' @export
.plot_scope_route <- function(table_name) {

  routes <- list(

    # Its own NOT NULL plot key, with a validated foreign key since
    # fk_subplot_plot_integrity.R.
    data_liste_sub_plots = list(
      hops = 0L, column = "id_table_liste_plots"),

    data_individuals = list(
      hops = 0L, column = "id_table_liste_plots_n"),

    # Its own plot key since denormalise_ind_measures_plot.R, maintained by
    # three triggers. Was three hops and 1,931 ms; is 0 hops and 71 ms.
    data_ind_measures_feat = list(
      hops = 0L, column = "id_table_liste_plots"),

    data_subplot_feat = list(
      hops = 1L,
      via = list(table = "data_liste_sub_plots", alias = "s",
                 on_local = "id_sub_plots", on_remote = "id_sub_plots",
                 remote_key = "id_table_liste_plots")),

    # Through the individual, not through its own id_table_liste_plots. See the
    # header of this file.
    data_traits_measures = list(
      hops = 1L,
      via = list(table = "data_individuals", alias = "i",
                 on_local = "id_data_individuals", on_remote = "id_n",
                 remote_key = "id_table_liste_plots_n")),

    # 160,903 of its 161,180 rows reach a plot through the individual; 277 reach
    # one only through its own id_liste_plots. Without the second branch those
    # 277 go invisible to everyone but the owner.
    data_link_specimens = list(
      hops = 1L,
      via = list(table = "data_individuals", alias = "i",
                 on_local = "id_n", on_remote = "id_n",
                 remote_key = "id_table_liste_plots_n"),
      or_column = "id_liste_plots")
  )

  if (!table_name %in% names(routes)) {
    cli::cli_abort("No plot-scope route defined for {.val {table_name}}")
  }

  routes[[table_name]]
}


#' @title The grant list a policy compares against
#' @description
#' `read` is every row in `plot_access` for the calling account; `write` is the
#' subset carrying `can_write`. There is no `delete` mode: see decision 2 in the
#' header of this file.
#'
#' Kept as a bare subquery rather than a call to `accessible_plots()` on
#' purpose. That function carries a `SET search_path`, which stops the planner
#' inlining it, and an opaque call is exactly what the measured shape avoids.
#'
#' @param mode `"read"` or `"write"`.
#' @return Character of length 1: an SQL `ARRAY(SELECT ...)` expression.
#' @keywords internal
#' @export
.plot_grant_list_sql <- function(mode = c("read", "write")) {

  mode <- match.arg(mode)

  paste0(
    "ARRAY(SELECT a.id_liste_plots FROM public.plot_access a",
    " WHERE a.db_user = current_user",
    if (mode == "write") " AND a.can_write" else "",
    ")")
}


#' @title The boolean expression a plot-scope policy tests
#' @description
#' Built from [.plot_scope_route()] and [.plot_grant_list_sql()]. Columns of the
#' policy's own table are qualified with the table name, which is what
#' PostgreSQL accepts inside a policy and what keeps the `EXISTS` branches
#' unambiguous against their alias.
#'
#' @param table_name Character of length 1. One of [.plot_scope_child_tables()].
#' @param mode `"read"` or `"write"`.
#' @return Character of length 1.
#' @keywords internal
#' @export
.plot_scope_predicate <- function(table_name, mode = c("read", "write")) {

  mode  <- match.arg(mode)
  route <- .plot_scope_route(table_name)
  ids   <- .plot_grant_list_sql(mode)

  main <- if (!is.null(route$column)) {

    paste0(table_name, ".", route$column, " = ANY (", ids, ")")

  } else {

    v <- route$via
    paste0(
      "EXISTS (SELECT 1 FROM public.", v$table, " ", v$alias,
      " WHERE ", v$alias, ".", v$on_remote,
      " = ", table_name, ".", v$on_local,
      " AND ", v$alias, ".", v$remote_key, " = ANY (", ids, "))")
  }

  if (is.null(route$or_column)) return(main)

  paste0("(", main, " OR ", table_name, ".", route$or_column,
         " = ANY (", ids, "))")
}


#' @title Policy names step 5 creates on a child table
#' @description
#' One per command. Named so they are greppable and so nothing collides with
#' the `policy_<account>_<command>` names `define_user_policy()` writes or the
#' `creator_access_*` names from `inst/migrations/add_created_by.R`.
#'
#' @param table_name Character of length 1.
#' @return Named character vector, one entry per SQL command.
#' @keywords internal
#' @export
.plot_scope_policy_names <- function(table_name) {
  c(select = "plot_scope_select",
    insert = "plot_scope_insert",
    update = "plot_scope_update",
    delete = "plot_scope_delete")
}


#' @title The statements that put one child table under plot scope
#' @description
#' Enables row-level security and creates four policies. All four are needed:
#' row-level security denies any command it has no policy for, so a missing
#' `INSERT` policy does not leave inserts open, it stops them.
#'
#' `UPDATE` carries both `USING` and `WITH CHECK`. `USING` alone would let an
#' account update a row it can see and re-point it at a plot it cannot -
#' which is exactly what moving an individual between plots does
#' (`R/mod_update_record.R:376`).
#'
#' `INSERT` gets `WITH CHECK` on the write list, which is what lets the import
#' wizard keep working unattended: the creator trigger on `data_liste_plots`
#' writes a `plot_access` row with `can_write` at the end of the plot's own
#' `INSERT` statement, so every later statement in the same transaction passes.
#'
#' @param table_name Character of length 1. One of [.plot_scope_child_tables()].
#' @return Character vector of SQL statements, in the order they must run.
#' @keywords internal
#' @export
.plot_scope_policy_statements <- function(table_name) {

  read  <- .plot_scope_predicate(table_name, "read")
  write <- .plot_scope_predicate(table_name, "write")
  nm    <- .plot_scope_policy_names(table_name)

  c(
    paste0("ALTER TABLE public.", table_name,
           " ENABLE ROW LEVEL SECURITY;"),

    paste0("CREATE POLICY ", nm[["select"]], " ON public.", table_name,
           " FOR SELECT USING (", read, ");"),

    paste0("CREATE POLICY ", nm[["insert"]], " ON public.", table_name,
           " FOR INSERT WITH CHECK (", write, ");"),

    paste0("CREATE POLICY ", nm[["update"]], " ON public.", table_name,
           " FOR UPDATE USING (", write, ") WITH CHECK (", write, ");"),

    # can_write, not can_delete - decision 2 in the header of this file.
    paste0("CREATE POLICY ", nm[["delete"]], " ON public.", table_name,
           " FOR DELETE USING (", write, ");")
  )
}


#' @title The statements that undo step 5 for one child table
#' @description
#' `DISABLE ROW LEVEL SECURITY` is immediate and total, so the first statement
#' alone restores the previous behaviour; the drops are there so a re-run of the
#' migration starts clean.
#'
#' @param table_name Character of length 1.
#' @return Character vector of SQL statements.
#' @keywords internal
#' @export
.plot_scope_rollback_statements <- function(table_name) {

  nm <- .plot_scope_policy_names(table_name)

  c(paste0("ALTER TABLE public.", table_name,
           " DISABLE ROW LEVEL SECURITY;"),
    paste0("DROP POLICY IF EXISTS ", nm, " ON public.", table_name, ";"))
}


#' @title Every column the plot-scope predicates name
#' @description
#' The first run of `inst/scripts/check_plot_access_cost.R` guessed a column
#' name that did not exist, and the failure took a whole transaction with it.
#' Nothing here is trusted against the catalog without being checked first, so
#' this lists what to check.
#'
#' @return Data frame with `table_name` and `column_name`.
#' @keywords internal
#' @export
.plot_scope_referenced_columns <- function() {

  rows <- lapply(.plot_scope_child_tables(), function(tb) {

    route <- .plot_scope_route(tb)

    own <- c(route$column, route$or_column,
             if (is.null(route$column)) route$via$on_local else NULL)

    remote <- if (is.null(route$via)) NULL else
      data.frame(table_name  = route$via$table,
                 column_name = c(route$via$on_remote, route$via$remote_key),
                 stringsAsFactors = FALSE)

    rbind(
      data.frame(table_name = tb, column_name = own, stringsAsFactors = FALSE),
      remote)
  })

  out <- unique(do.call(rbind, rows))
  rownames(out) <- NULL
  out
}
