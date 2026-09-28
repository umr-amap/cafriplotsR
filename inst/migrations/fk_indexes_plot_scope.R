# MIGRATION - not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
#   source(system.file("migrations", "fk_indexes_plot_scope.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#   migrate_fk_indexes_plot_scope(con)                   # rehearsal: prints, changes nothing
#   migrate_fk_indexes_plot_scope(con, dry_run = FALSE)  # apply


#' Migration: index the columns that link plot data back to a plot
#'
#' Not one of the six columns that connect a plot's data to the plot is
#' indexed, and four of them are declared foreign keys. PostgreSQL indexes the
#' *referenced* side of a foreign key automatically, never the referencing
#' side, so every lookup "give me the rows belonging to these plots" is a
#' sequential scan today.
#'
#' Measured on the live database (2026-09-26): filtering
#' `data_traits_measures` on a 100-plot list costs **75 ms** and 24,219 buffer
#' reads — most of the 249 MB table — and the two-hop version through
#' `data_ind_measures_feat` costs **116 ms**.
#'
#' @details
#' This is worth doing on its own merits: these are the joins the extraction
#' layer and the Shiny apps perform constantly. It is also a prerequisite for
#' row-level security on the child tables (plan P4.2/P4.4), because an RLS
#' policy evaluates its predicate on *every* query by *every* scoped account.
#' Putting policies on unindexed columns would turn one seq scan per query into
#' the normal case for everyone but the table owner.
#'
#' No policy, privilege or row is touched here. Nothing about who can see what
#' changes. This migration is safe to apply and live with indefinitely whatever
#' is later decided about RLS.
#'
#' @section Locking:
#' `CREATE INDEX CONCURRENTLY` does not block reads or writes, which is why it
#' is used here — `data_traits_measures` is 2 M rows and in daily use. The
#' price is that it cannot run inside a transaction, so this migration commits
#' each index on its own and is written to be re-runnable: an index that
#' already exists is skipped.
#'
#' A `CONCURRENTLY` build that fails leaves an **invalid** index behind, which
#' takes space and is never used by the planner. Step 1 looks for those and
#' names them; they must be dropped before retrying.
#'
#' @section Why these six:
#' One per table, keyed on the column that reaches a plot:
#'
#' \itemize{
#'   \item `data_individuals.id_table_liste_plots_n` → the plot (FK)
#'   \item `data_liste_sub_plots.id_table_liste_plots` → the plot (no FK)
#'   \item `data_link_specimens.id_n` → the individual (FK).
#'         `id_liste_plots` is already indexed
#'   \item `data_subplot_feat.id_sub_plots` → the subplot (FK)
#'   \item `data_traits_measures.id_data_individuals` → the individual (FK)
#'   \item `data_ind_measures_feat.id_trait_measures` → the measurement (FK)
#' }
#'
#' @param con Database connection to `plots_transects`, as the table owner.
#' @param dry_run If TRUE (the default), report what would be built and change
#'   nothing.
#' @param analyze If TRUE (the default), run `ANALYZE` on each table after its
#'   index is built so the planner starts using it immediately.
#' @return Invisibly, a data frame of the indexes with their before/after state.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' migrate_fk_indexes_plot_scope(con)                   # rehearse
#' migrate_fk_indexes_plot_scope(con, dry_run = FALSE)  # apply
#' }
#'
#' @keywords internal
migrate_fk_indexes_plot_scope <- function(con, dry_run = TRUE, analyze = TRUE) {

  cli::cli_h1("Migration: index the plot-scoping foreign keys")

  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  targets <- data.frame(
    table_name = c("data_individuals",
                   "data_liste_sub_plots",
                   "data_link_specimens",
                   "data_subplot_feat",
                   "data_traits_measures",
                   "data_ind_measures_feat"),
    column_name = c("id_table_liste_plots_n",
                    "id_table_liste_plots",
                    "id_n",
                    "id_sub_plots",
                    "id_data_individuals",
                    "id_trait_measures"),
    stringsAsFactors = FALSE
  )
  targets$index_name <- paste0("idx_", targets$table_name, "_", targets$column_name)

  # -- Step 1: preflight ------------------------------------------------------
  cli::cli_h2("Step 1: Preflight")

  whoami <- DBI::dbGetQuery(con, "SELECT current_database() AS db, current_user AS usr")
  cli::cli_alert_info("Connected to {.val {whoami$db}} as {.val {whoami$usr}}")
  if (whoami$db != "plots_transects") {
    cli::cli_abort("This migration belongs to plots_transects, not {.val {whoami$db}}")
  }

  # Every column must exist, or a typo here becomes a confusing failure later.
  present <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT table_name, column_name
      FROM information_schema.columns
     WHERE table_schema = 'public'
       AND table_name IN ({unique(targets$table_name)*})",
    .con = con))
  missing <- !mapply(function(t, c) any(present$table_name == t & present$column_name == c),
                     targets$table_name, targets$column_name)
  if (any(missing)) {
    gone <- paste0(targets$table_name[missing], ".", targets$column_name[missing])
    cli::cli_abort(c("Column not found: {.field {gone}}",
                     i = "The schema has moved since this migration was written."))
  }
  cli::cli_alert_success("All {nrow(targets)} target columns exist")

  # An invalid index is the debris of a failed CONCURRENTLY build. It occupies
  # space, is never used, and blocks a rebuild under the same name.
  invalid <- DBI::dbGetQuery(con, "
    SELECT c.relname AS index_name, t.relname AS table_name,
           pg_size_pretty(pg_relation_size(c.oid)) AS size
      FROM pg_index i
      JOIN pg_class c ON c.oid = i.indexrelid
      JOIN pg_class t ON t.oid = i.indrelid
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND NOT i.indisvalid
     ORDER BY 1")
  if (nrow(invalid) > 0) {
    print(invalid, row.names = FALSE)
    cli::cli_abort(c(
      "{nrow(invalid)} invalid index/indexes from an earlier failed build.",
      i = "Drop {?it/them} first: {.code DROP INDEX CONCURRENTLY <name>}",
      i = "A rebuild under the same name cannot proceed while one exists."
    ))
  }

  # -- Step 2: what exists already -------------------------------------------
  cli::cli_h2("Step 2: Current state")

  existing <- DBI::dbGetQuery(con, "
    SELECT t.relname AS table_name, c.relname AS index_name,
           pg_get_indexdef(i.indexrelid) AS definition
      FROM pg_index i
      JOIN pg_class c ON c.oid = i.indexrelid
      JOIN pg_class t ON t.oid = i.indrelid
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public'")

  # Match on the definition, not the name: an index on the right column under a
  # different name is still an index on the right column, and building a second
  # one would be waste.
  targets$already <- mapply(function(t, col) {
    on_table <- existing[existing$table_name == t, ]
    if (nrow(on_table) == 0) return(NA_character_)
    hit <- grepl(paste0("\\(", col, "\\)$"), on_table$definition)
    if (any(hit)) on_table$index_name[which(hit)[1]] else NA_character_
  }, targets$table_name, targets$column_name)

  sizes <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT c.relname AS table_name, c.reltuples::bigint AS est_rows,
           pg_size_pretty(pg_relation_size(c.oid)) AS heap_size
      FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND c.relname IN ({unique(targets$table_name)*})",
    .con = con))
  report <- merge(targets, sizes, by = "table_name", all.x = TRUE)
  print(report[, c("table_name", "column_name", "est_rows", "heap_size", "already")],
        row.names = FALSE)

  todo <- report[is.na(report$already), ]
  done <- report[!is.na(report$already), ]

  if (nrow(done) > 0) {
    cli::cli_alert_success("{nrow(done)} column{?s} already indexed - skipping")
  }
  if (nrow(todo) == 0) {
    cli::cli_alert_success("Nothing to build - every scoping column is indexed")
    return(invisible(report))
  }
  cli::cli_alert_warning("{nrow(todo)} index/indexes to build")

  # -- Step 3: build ---------------------------------------------------------
  cli::cli_h2("Step 3: Building")

  statements <- sprintf(
    "CREATE INDEX CONCURRENTLY %s ON public.%s (%s)",
    DBI::dbQuoteIdentifier(con, todo$index_name),
    DBI::dbQuoteIdentifier(con, todo$table_name),
    DBI::dbQuoteIdentifier(con, todo$column_name))

  if (dry_run) {
    for (s in statements) cli::cli_alert_info("Would execute: {.code {s}}")
    if (analyze) {
      for (t in unique(todo$table_name)) {
        cli::cli_alert_info("Would execute: {.code ANALYZE public.{t}}")
      }
    }
    cli::cli_alert_info(
      "Dry run - nothing was built. Re-run with {.code dry_run = FALSE}.")
    return(invisible(report))
  }

  # No dbBegin(): CONCURRENTLY is refused inside a transaction block. Each
  # index therefore commits independently, and a failure part-way leaves the
  # earlier ones in place - which is fine, they are additive and the migration
  # is re-runnable.
  built <- character()
  for (i in seq_len(nrow(todo))) {
    s <- statements[i]
    cli::cli_alert_info("Executing: {.code {s}}")
    t0 <- Sys.time()
    ok <- tryCatch({
      DBI::dbExecute(con, s)
      TRUE
    }, error = function(e) {
      cli::cli_alert_danger("Failed on {todo$index_name[i]}: {e$message}")
      cli::cli_alert_info(
        "Check for an invalid index of that name and drop it before retrying.")
      FALSE
    })
    if (!ok) break

    cli::cli_alert_success(
      "Built {todo$index_name[i]} in {round(as.numeric(difftime(Sys.time(), t0, units = 'secs')), 1)}s")
    built <- c(built, todo$index_name[i])

    if (analyze) {
      DBI::dbExecute(con, paste("ANALYZE public.",
                                DBI::dbQuoteIdentifier(con, todo$table_name[i]),
                                sep = ""))
    }
  }

  # -- Step 4: verify --------------------------------------------------------
  cli::cli_h2("Step 4: Verifying")

  after <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT c.relname AS index_name, t.relname AS table_name,
           i.indisvalid AS valid,
           pg_size_pretty(pg_relation_size(c.oid)) AS size
      FROM pg_index i
      JOIN pg_class c ON c.oid = i.indexrelid
      JOIN pg_class t ON t.oid = i.indrelid
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND c.relname IN ({targets$index_name*})
     ORDER BY 2", .con = con))
  if (nrow(after) > 0) print(after, row.names = FALSE)

  if (any(!after$valid)) {
    cli::cli_alert_danger(
      "Invalid: {.val {after$index_name[!after$valid]}} - drop and rebuild")
  }
  if (length(built) == nrow(todo) && all(after$valid)) {
    cli::cli_alert_success("All {length(built)} index/indexes built and valid")
  } else {
    cli::cli_alert_warning(
      "{length(built)} of {nrow(todo)} built - re-run to finish the rest")
  }

  invisible(report)
}


#' Report whether the plot-scoping indexes are in place
#'
#' Read-only companion to [migrate_fk_indexes_plot_scope()]. Also re-measures
#' the predicate the RLS policies would use, so the cost can be compared
#' against the pre-index baseline recorded in that function's documentation
#' (75 ms direct, 116 ms two-hop).
#'
#' @param con Database connection to `plots_transects`.
#' @return Invisibly, a list with the index state and the two plans.
#' @keywords internal
check_fk_indexes_plot_scope <- function(con) {

  cli::cli_h2("Plot-scoping indexes")

  state <- DBI::dbGetQuery(con, "
    SELECT t.relname AS table_name, c.relname AS index_name, i.indisvalid AS valid,
           pg_get_indexdef(i.indexrelid) AS definition
      FROM pg_index i
      JOIN pg_class c ON c.oid = i.indexrelid
      JOIN pg_class t ON t.oid = i.indrelid
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public'
       AND t.relname IN ('data_individuals','data_liste_sub_plots',
                         'data_link_specimens','data_subplot_feat',
                         'data_traits_measures','data_ind_measures_feat')
     ORDER BY 1, 2")
  print(state[, c("table_name", "index_name", "valid")], row.names = FALSE)

  cli::cli_h2("Cost of the candidate RLS predicate")

  ids <- DBI::dbGetQuery(con,
    "SELECT id_liste_plots FROM data_liste_plots ORDER BY random() LIMIT 100")$id_liste_plots
  arr <- paste(ids, collapse = ",")

  plans <- list()
  plans$direct <- DBI::dbGetQuery(con, sprintf(
    "EXPLAIN (ANALYZE, BUFFERS) SELECT count(*) FROM data_individuals
      WHERE id_table_liste_plots_n = ANY (ARRAY[%s])", arr))
  cat("\n-- data_individuals, direct plot key --\n")
  cat(paste(plans$direct[[1]], collapse = "\n"), "\n")

  plans$two_hop <- DBI::dbGetQuery(con, sprintf(
    "EXPLAIN (ANALYZE, BUFFERS) SELECT count(*) FROM data_traits_measures m
      WHERE EXISTS (SELECT 1 FROM data_individuals i
                     WHERE i.id_n = m.id_data_individuals
                       AND i.id_table_liste_plots_n = ANY (ARRAY[%s]))", arr))
  cat("\n-- data_traits_measures via the individual (the real policy shape) --\n")
  cat(paste(plans$two_hop[[1]], collapse = "\n"), "\n")

  invisible(list(indexes = state, plans = plans))
}
