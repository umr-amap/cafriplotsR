# =============================================================================
# What would a plot_access policy cost? (step 4e)
#
# This is the measurement step 4 exists to produce. It decides two things that
# should not be decided by argument:
#
#   1. WHICH PREDICATE SHAPE. A literal array, as the current 125 policies use,
#      or a semi-join against plot_access. The semi-join is the better design -
#      the planner gets statistics instead of an opaque list - but "better
#      design" is not a number.
#
#   2. WHAT THE TWO-KEY TABLES DO. data_traits_measures and data_link_specimens
#      each have two routes to a plot, and rows where the preferred one is NULL.
#      A single-key policy makes those rows invisible to everyone but the owner.
#      Section 3 counts them, per branch, so the CASE in step 5's policy is
#      written against evidence.
#
# NOTHING IS ENFORCED AND NOTHING IS WRITTEN. The whole run happens inside a
# READ ONLY transaction, and every predicate is measured as an ordinary WHERE
# clause. No table gets ENABLE ROW LEVEL SECURITY.
#
# TWO LIMITS, STATED UP FRONT
#
#   - A WHERE clause is a LOWER BOUND on the RLS cost, not an equivalent. With
#     several permissive policies PostgreSQL ORs them, and an RLS qual
#     constrains pushdown across joins in ways a hand-written WHERE does not.
#     Expect the real number to be at or above what this prints.
#
#   - Proving it exactly means ALTER TABLE ... ENABLE ROW LEVEL SECURITY, which
#     takes an ACCESS EXCLUSIVE lock and blocks every reader of data_individuals
#     while held - including the live apps, even inside a transaction that is
#     rolled back. That belongs in step 5, one table at a time, at a quiet
#     moment. It is not something to slip into a measurement.
#
# RUNTIME: a few minutes. The 2,000-plot fetch on data_traits_measures alone
# took 1.2 s when the indexes were measured. Pass fewer `sizes` to cut it down.
#
# Run from the package root as the owner, after devtools::load_all(".").
# =============================================================================

library(DBI)

# The baselines recorded for inst/migrations/fk_indexes_plot_scope.R, so the new
# numbers read as a delta rather than in isolation.
BASELINE <- data.frame(
  table_name = c("data_individuals", "data_individuals",
                 "data_traits_measures", "data_traits_measures"),
  n_plots    = c(100L, 2000L, 100L, 2000L),
  predicate_ms = c(2.119, 45.667, 21.389, 305.985),
  fetch_ms     = c(15.788, 81.301, 158.884, 1233.528),
  stringsAsFactors = FALSE
)

# How each table reaches a plot. `join` is the FROM/WHERE fragment that gets it
# there; `key` is the expression a grant is compared against.
#
# data_traits_measures goes through id_data_individuals, NOT its own
# id_table_liste_plots: keying on the denormalised column orphans 256,179 rows
# and scopes 8,575 to the wrong plot's grant list. See P4.4.
#
# `prefer` names the columns worth fetching, but is not trusted: whatever is not
# in the catalog is dropped and the first three columns are used instead. The
# first run of this script guessed `id_ind_measures_feat`, which does not exist,
# and the failure took the whole transaction with it.
PLOT_ROUTES <- list(
  data_liste_plots = list(
    hops = 0L,
    from = "data_liste_plots t",
    key  = "t.id_liste_plots",
    prefer = c("id_liste_plots", "plot_name", "ddlat", "ddlon")),

  data_individuals = list(
    hops = 1L,
    from = "data_individuals t",
    key  = "t.id_table_liste_plots_n",
    prefer = c("id_n", "id_table_liste_plots_n", "tag")),

  data_liste_sub_plots = list(
    hops = 1L,
    from = "data_liste_sub_plots t",
    key  = "t.id_table_liste_plots",
    prefer = c("id_sub_plots", "id_table_liste_plots", "typevalue")),

  data_subplot_feat = list(
    hops = 2L,
    from = "data_subplot_feat t JOIN data_liste_sub_plots s
              ON s.id_sub_plots = t.id_sub_plots",
    key  = "s.id_table_liste_plots",
    prefer = c("id_subplot_feat", "id_sub_plots", "typevalue")),

  # decimallatitude is in the list on purpose: it is the column that makes the
  # leak publishable (P4.4), so the cost of fetching it is the cost that matters.
  data_traits_measures = list(
    hops = 2L,
    from = "data_traits_measures t JOIN data_individuals i
              ON i.id_n = t.id_data_individuals",
    key  = "i.id_table_liste_plots_n",
    prefer = c("id_trait_measures", "traitvalue", "decimallatitude")),

  data_ind_measures_feat = list(
    hops = 3L,
    from = "data_ind_measures_feat t
              JOIN data_traits_measures m ON m.id_trait_measures = t.id_trait_measures
              JOIN data_individuals i     ON i.id_n = m.id_data_individuals",
    key  = "i.id_table_liste_plots_n",
    prefer = c("id_trait_measures", "id_trait", "typevalue")),

  data_link_specimens = list(
    hops = 1L,
    from = "data_link_specimens t JOIN data_individuals i ON i.id_n = t.id_n",
    key  = "i.id_table_liste_plots_n",
    prefer = c("id_link_specimens", "id_n"))
)


#' Columns that actually exist, preferring the interesting ones
#'
#' Row width drives the fetch cost, so which columns are read matters - but not
#' enough to justify guessing at names. Whatever `prefer` names and the catalog
#' does not have is dropped; if nothing survives, the first three columns are
#' used and the substitution is reported.
#' @noRd
.fetch_columns <- function(con, table_name, prefer) {

  have <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT a.attname, a.attnum
       FROM pg_attribute a
      WHERE a.attrelid = {paste0('public.', table_name)}::regclass
        AND a.attnum > 0 AND NOT a.attisdropped
      ORDER BY a.attnum", .con = con))

  keep <- intersect(prefer, have$attname)
  missing <- setdiff(prefer, have$attname)

  if (length(keep) == 0) {
    keep <- utils::head(have$attname, 3)
    cli::cli_alert_warning(
      "{table_name}: none of {.val {prefer}} exist - fetching {.val {keep}} instead")
  } else if (length(missing) > 0) {
    cli::cli_alert_info(
      "{table_name}: {.val {missing}} {?does/do} not exist, fetching {.val {keep}}")
  }

  paste0("t.", keep, collapse = ", ")
}


#' Plan every route before measuring any of it
#'
#' EXPLAIN without ANALYZE parses and plans but does not execute, so this
#' validates the FROM, the key and the column list for a few milliseconds and
#' fails before spending minutes on a run that will break in the middle.
#' @noRd
.validate_routes <- function(con, routes) {

  broken <- list()
  for (tb in names(routes)) {
    r <- routes[[tb]]
    sql <- paste0("SELECT ", r$cols, " FROM ", r$from, " WHERE ", r$key, " = 1")
    DBI::dbExecute(con, "SAVEPOINT sp_validate")
    err <- tryCatch({
      DBI::dbGetQuery(con, paste0("EXPLAIN ", sql)); NULL
    }, error = function(e) conditionMessage(e))
    DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT sp_validate")
    DBI::dbExecute(con, "RELEASE SAVEPOINT sp_validate")
    if (!is.null(err)) broken[[tb]] <- err
  }

  if (length(broken) > 0) {
    for (tb in names(broken)) {
      cli::cli_alert_danger("{tb}: {broken[[tb]]}")
      cols <- DBI::dbGetQuery(con, glue::glue_sql(
        "SELECT string_agg(a.attname, ', ' ORDER BY a.attnum) AS cols
           FROM pg_attribute a
          WHERE a.attrelid = {paste0('public.', tb)}::regclass
            AND a.attnum > 0 AND NOT a.attisdropped", .con = con))$cols
      cli::cli_alert_info("  {tb} has: {cols}")
    }
    cli::cli_abort("{length(broken)} route{?s} {?does/do} not plan - fix PLOT_ROUTES.")
  }
  cli::cli_alert_success("All {length(routes)} routes plan cleanly")
  invisible(TRUE)
}


#' Pull "Execution Time" out of an EXPLAIN ANALYZE, plus what it warns about
#' @noRd
.explain_ms <- function(con, sql) {
  plan <- DBI::dbGetQuery(con, paste0("EXPLAIN (ANALYZE, BUFFERS) ", sql))
  txt  <- plan[[1]]

  grab <- function(pattern) {
    hit <- grep(pattern, txt, value = TRUE)
    if (length(hit) == 0) return(NA_real_)
    sum(as.numeric(sub(paste0(".*", pattern, "[^0-9]*([0-9]+).*"), "\\1", hit)))
  }

  list(
    ms          = as.numeric(sub(".*Execution Time: ([0-9.]+) ms.*", "\\1",
                                 grep("Execution Time", txt, value = TRUE)[1])),
    seq_scans   = length(grep("Seq Scan", txt)),
    index_only  = length(grep("Index Only Scan", txt)),
    # NA here means no index-only scan in the plan, not a clean visibility map.
    heap_fetches = grab("Heap Fetches:"),
    plan        = txt
  )
}


#' Run one measurement without letting a failure poison the transaction
#'
#' A failed statement aborts the whole transaction, so every later query returns
#' "current transaction is aborted" - which on the first run of this script turned
#' one wrong column name into 8 lost measurements and both remaining sections.
#' tryCatch alone does not help: the damage is at the server, not in R.
#' @noRd
.with_savepoint <- function(con, expr) {
  DBI::dbExecute(con, "SAVEPOINT sp_measure")
  out <- tryCatch(list(value = force(expr), error = NULL),
                  error = function(e) list(value = NULL,
                                           error = conditionMessage(e)))
  if (!is.null(out$error)) DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT sp_measure")
  DBI::dbExecute(con, "RELEASE SAVEPOINT sp_measure")
  out
}


#' Measure one table, one shape, one grant size
#' @noRd
.measure_one <- function(con, table_name, route, ids, shape, role) {

  pred <- if (shape == "array") {
    paste0(route$key, " = ANY (ARRAY[", paste(ids, collapse = ", "),
           "]::integer[])")
  } else {
    paste0("EXISTS (SELECT 1 FROM plot_access a",
           "  WHERE a.db_user = ", DBI::dbQuoteString(con, role),
           "    AND a.id_liste_plots = ", route$key, ")")
  }

  # Two numbers, because one is not enough. count(*) on an indexed column can be
  # served by an index-only scan without touching the table, which understates
  # the cost by an order of magnitude - a mistake made once already while
  # measuring the indexes.
  row <- function(predicate, fetch, note) data.frame(
    table_name   = table_name,
    hops         = route$hops,
    shape        = shape,
    n_plots      = length(ids),
    predicate_ms = if (is.null(predicate)) NA_real_ else round(predicate$ms, 3),
    fetch_ms     = if (is.null(fetch)) NA_real_ else round(fetch$ms, 3),
    seq_scans    = if (is.null(fetch)) NA_integer_ else fetch$seq_scans,
    index_only   = if (is.null(fetch)) NA_integer_ else fetch$index_only,
    heap_fetches = if (is.null(fetch)) NA_real_ else fetch$heap_fetches,
    note         = note,
    stringsAsFactors = FALSE
  )

  p <- .with_savepoint(con, .explain_ms(con, paste0(
    "SELECT count(*) FROM ", route$from, " WHERE ", pred)))
  if (!is.null(p$error)) return(row(NULL, NULL, paste("predicate:", p$error)))

  f <- .with_savepoint(con, .explain_ms(con, paste0(
    "SELECT ", route$cols, " FROM ", route$from, " WHERE ", pred)))
  if (!is.null(f$error)) return(row(p$value, NULL, paste("fetch:", f$error)))

  row(p$value, f$value, "")
}


#' What grant sizes actually exist?
#'
#' Measured sizes are taken from reality rather than invented. Three real
#' accounts hold more than 1,900 of the 2,194 plots, so the top of the range is
#' not a synthetic worst case.
#' @noRd
.real_grant_sizes <- function(con, n_sizes = 4L) {

  have_pa <- isTRUE(DBI::dbGetQuery(con,
    "SELECT to_regclass('public.plot_access') IS NOT NULL AS ok")$ok)

  if (have_pa) {
    g <- DBI::dbGetQuery(con, "
      SELECT db_user, count(*)::int AS n_plots
        FROM plot_access GROUP BY 1 ORDER BY 2 DESC")
  } else {
    # Fall back to the policies, so this runs before plot_access is applied.
    pol <- DBI::dbGetQuery(con, "
      SELECT u.role_name AS db_user, p.qual
        FROM pg_policies p, LATERAL unnest(p.roles) AS u(role_name)
       WHERE p.schemaname = 'public' AND p.tablename = 'data_liste_plots'
         AND p.cmd = 'SELECT' AND lower(u.role_name) <> 'public'")
    ids <- lapply(pol$qual, function(q) CafriplotsR:::.parse_policy_plot_ids(q)$ids)
    g <- data.frame(db_user = pol$db_user,
                    n_plots = vapply(ids, length, integer(1)),
                    stringsAsFactors = FALSE)
    g <- g[g$n_plots > 0, , drop = FALSE]
    g <- g[order(-g$n_plots), , drop = FALSE]
  }

  if (nrow(g) == 0) return(g[0, ])

  # Largest, smallest, and evenly spaced through the middle - the accounts that
  # will actually pay these numbers, named.
  idx <- unique(round(seq(1, nrow(g), length.out = min(n_sizes, nrow(g)))))
  out <- g[idx, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "source") <- if (have_pa) "plot_access" else "pg_policies"
  out
}


#' @title What a plot_access policy would cost
#' @param con A connection to plots_transects, as the owner.
#' @param sizes Optional integer vector of grant sizes to measure. `NULL` takes
#'   them from the real accounts.
#' @param tables Which tables to measure. Defaults to all seven.
#' @param shapes Which predicate shapes. `"array"` always works; `"semijoin"`
#'   needs plot_access to exist and be seeded.
#' @return Invisibly a list of data frames.
report_plot_access_cost <- function(con, sizes = NULL,
                                    tables = names(PLOT_ROUTES),
                                    shapes = c("array", "semijoin")) {

  stopifnot("Invalid connection" = DBI::dbIsValid(con))

  cli::cli_h1("What a plot_access policy would cost")
  cli::cli_alert_info(
    "Read-only. Every predicate is measured as a plain WHERE clause; no table
     gets row-level security, and nothing is written.")

  have_pa <- isTRUE(DBI::dbGetQuery(con,
    "SELECT to_regclass('public.plot_access') IS NOT NULL AS ok")$ok)
  if (!have_pa && "semijoin" %in% shapes) {
    cli::cli_alert_warning(c(
      "plot_access does not exist, so only the {.val array} shape can be
       measured. Apply {.file inst/migrations/plot_access_table.R} and seed it,
       then re-run to compare the two."))
    shapes <- setdiff(shapes, "semijoin")
  }

  # Everything below happens read-only at the database's insistence, not mine.
  DBI::dbExecute(con, "BEGIN")
  DBI::dbExecute(con, "SET TRANSACTION READ ONLY")
  on.exit({
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    cli::cli_alert_info("Read-only transaction closed")
  }, add = TRUE)

  # --- 1. whose grants are we measuring? -----------------------------------
  grants <- .real_grant_sizes(con)
  src <- attr(grants, "source")
  if (is.null(src)) src <- "pg_policies"
  cli::cli_h2("Grant sizes taken from {src}")
  if (nrow(grants) == 0) {
    cli::cli_abort("No grants found to measure.")
  }

  # How many accounts each measured size speaks for. The distribution is heavily
  # skewed - three accounts above 1,900 plots and a long tail under 10 - so a
  # timing without this column invites reading the worst case as an outlier.
  all_grants <- if (identical(src, "plot_access")) {
    DBI::dbGetQuery(con, "SELECT count(*)::int AS n FROM plot_access
                           GROUP BY db_user")$n
  } else {
    grants$n_plots
  }
  grants$accounts_at_least <- vapply(grants$n_plots,
    function(n) sum(all_grants >= n), integer(1))
  print(grants, row.names = FALSE)

  if (is.null(sizes)) sizes <- grants$n_plots
  sizes <- sort(unique(as.integer(sizes)))

  # A plot list of each size, drawn from real plot IDs so the selectivity is
  # real. Ordered by id for a stable, reproducible sample.
  all_ids <- DBI::dbGetQuery(con,
    "SELECT id_liste_plots FROM data_liste_plots ORDER BY 1")$id_liste_plots
  n_total <- length(all_ids)
  stopifnot(
    "plot ids came back empty or non-integer" =
      is.numeric(all_ids) && n_total > 0L
  )
  cli::cli_alert_info("{n_total} plots in data_liste_plots")

  sizes <- sizes[sizes <= n_total & sizes > 0]
  if (length(sizes) == 0) cli::cli_abort("No usable grant sizes.")

  # --- 2. the measurements -------------------------------------------------
  role_for <- function(n) {
    hit <- grants$db_user[grants$n_plots == n]
    if (length(hit) > 0) hit[1] else grants$db_user[1]
  }

  # Resolve the fetch columns against the catalog, then plan every route before
  # measuring any of it.
  routes <- PLOT_ROUTES[intersect(tables, names(PLOT_ROUTES))]
  for (tb in names(routes)) {
    routes[[tb]]$cols <- .fetch_columns(con, tb, routes[[tb]]$prefer)
  }
  .validate_routes(con, routes)

  rows <- list()
  total <- length(routes) * length(shapes) * length(sizes)
  done <- 0L

  for (tb in names(routes)) {
    route <- routes[[tb]]
    for (shape in shapes) {
      for (n in sizes) {
        role <- role_for(n)
        # For the semi-join, the sample must be that account's actual grant, or
        # the predicate matches nothing and the timing is meaningless.
        ids <- if (shape == "semijoin") {
          DBI::dbGetQuery(con, glue::glue_sql(
            "SELECT id_liste_plots FROM plot_access WHERE db_user = {role}
              ORDER BY 1", .con = con))$id_liste_plots
        } else {
          all_ids[seq_len(n)]
        }
        if (length(ids) == 0) next

        done <- done + 1L
        cli::cli_alert_info(
          "[{done}/{total}] {tb} - {shape} - {length(ids)} plots ({role})")
        one <- .measure_one(con, tb, route, ids, shape, role)
        if (nzchar(one$note)) cli::cli_alert_danger("  {one$note}")
        rows[[length(rows) + 1L]] <- one
      }
    }
  }

  res <- do.call(rbind, rows)

  cli::cli_h2("Cost per table, per shape, per grant size")
  print(res[, setdiff(names(res), "note")], row.names = FALSE)

  failed <- res[nzchar(res$note), , drop = FALSE]
  if (nrow(failed) > 0) {
    cli::cli_alert_danger("{nrow(failed)} measurement{?s} failed:")
    print(unique(failed[, c("table_name", "shape", "note")]), row.names = FALSE)
  }

  if (any(res$seq_scans > 0, na.rm = TRUE)) {
    cli::cli_alert_warning(
      "{sum(res$seq_scans > 0, na.rm = TRUE)} measurement{?s} plan a sequential
       scan. On data_liste_plots that is correct - 2,194 rows are cheaper to scan
       than to index. Elsewhere it means the predicate is not reaching an index.")
  }
  if (all(is.na(res$heap_fetches))) {
    cli::cli_alert_info(
      "No index-only scan in any plan, so Heap Fetches is not applicable - these
       queries read real columns, unlike the count(*) measurements taken when the
       indexes went in.")
  } else if (any(res$heap_fetches > 0, na.rm = TRUE)) {
    cli::cli_alert_warning(
      "Non-zero Heap Fetches: the visibility map is stale. Run
       {.code VACUUM (ANALYZE)} on those tables and measure again - it was worth
       an order of magnitude last time.")
  }

  # --- 3. the two-key tables ----------------------------------------------
  cli::cli_h2("Rows a single-key policy would hide")
  cli::cli_alert_info(
    "These are the counts step 5's CASE has to be written against. A row whose
     key is NULL matches no policy and goes invisible to everyone but the owner.")

  branch_sql <- "
    SELECT 'data_traits_measures'                              AS table_name,
           count(*)::int                                       AS n_rows,
           (count(*) FILTER (WHERE id_data_individuals IS NOT NULL))::int
                                                               AS via_individual,
           (count(*) FILTER (WHERE id_data_individuals IS NULL
                               AND id_table_liste_plots IS NOT NULL))::int
                                                               AS via_plot_only,
           (count(*) FILTER (WHERE id_data_individuals IS NULL
                               AND id_table_liste_plots IS NULL))::int
                                                               AS via_neither
      FROM data_traits_measures
    UNION ALL
    SELECT 'data_link_specimens',
           count(*)::int,
           (count(*) FILTER (WHERE id_n IS NOT NULL))::int,
           (count(*) FILTER (WHERE id_n IS NULL
                               AND id_liste_plots IS NOT NULL))::int,
           (count(*) FILTER (WHERE id_n IS NULL AND id_liste_plots IS NULL))::int
      FROM data_link_specimens
    UNION ALL
    SELECT 'data_ind_measures_feat',
           count(*)::int,
           (count(*) FILTER (WHERE id_trait_measures IS NOT NULL))::int,
           0,
           (count(*) FILTER (WHERE id_trait_measures IS NULL))::int
      FROM data_ind_measures_feat"

  b <- .with_savepoint(con, DBI::dbGetQuery(con, branch_sql))
  if (!is.null(b$error)) {
    cli::cli_alert_danger("Branch counts failed: {b$error}")
    branches <- data.frame()
  } else {
    branches <- b$value
    print(branches, row.names = FALSE)
  }

  for (i in seq_len(nrow(branches))) {
    b <- branches[i, ]
    if (b$via_plot_only > 0) {
      cli::cli_alert_warning(
        "{b$table_name}: {b$via_plot_only} row{?s} reachable only by the second
         key - the policy needs that branch or they disappear")
    }
    if (b$via_neither > 0) {
      cli::cli_alert_warning(
        "{b$table_name}: {b$via_neither} row{?s} reach no plot at all. Either
         they are not plot data and the policy should admit them, or they are
         orphans. That is a decision, not a default.")
    }
  }

  # --- 4. against the baseline --------------------------------------------
  cli::cli_h2("Against the pre-policy baseline")
  cli::cli_alert_info(
    "Measured for inst/migrations/fk_indexes_plot_scope.R, same queries, no
     predicate on plot_access:")
  print(BASELINE, row.names = FALSE)
  cli::cli_alert_info(
    "Subtract to get what the policy adds. A WHERE clause is a lower bound on
     the RLS cost - see the header.")

  invisible(list(grants = grants, measurements = res, branches = branches,
                 baseline = BASELINE))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
if (!interactive()) {
  cat("Source this and call report_plot_access_cost(con).\n")
} else {
  cat("Loaded. Run:\n",
      "  con <- CafriplotsR::call.mydb()\n",
      "  report_plot_access_cost(con)\n", sep = "")
}
