# ---------------------------------------------------------------------------
# Permission guard (internal)
# ---------------------------------------------------------------------------

# Checks that the connected PostgreSQL user has sufficient privileges to read
# monitoring system views.  Accepts any of:
#   - PostgreSQL superuser
#   - member of the predefined 'pg_monitor' role (PostgreSQL >= 10)
#   - member of the custom 'db_manager' role (legacy, project-specific)
# Called at the entry of every monitoring function that queries sensitive
# system views.
.check_monitoring_permission <- function(con) {
  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  # Build the privilege query defensively: pg_has_role() raises an error if the
  # named role does not exist, so probe each role independently and fall back
  # to FALSE when missing.  pg_monitor exists on PostgreSQL >= 10; db_manager
  # is an optional, project-specific role.
  probe_role <- function(role_name) {
    res <- tryCatch(
      DBI::dbGetQuery(
        actual_con,
        sprintf(
          "SELECT pg_has_role(current_user, '%s', 'MEMBER') AS member",
          role_name
        )
      ),
      error = function(e) NULL
    )
    isTRUE(res$member[1L])
  }

  result <- tryCatch(
    DBI::dbGetQuery(actual_con, "
      SELECT current_user AS username,
             rolsuper     AS is_superuser
      FROM pg_roles
      WHERE rolname = current_user
    "),
    error = function(e) {
      data.frame(username = "unknown", is_superuser = FALSE,
                 stringsAsFactors = FALSE)
    }
  )

  is_superuser  <- nrow(result) > 0L && isTRUE(result$is_superuser[1L])
  is_pg_monitor <- probe_role("pg_monitor")
  is_db_manager <- probe_role("db_manager")

  allowed <- is_superuser || is_pg_monitor || is_db_manager

  if (!allowed) {
    who <- if (nrow(result) > 0L) result$username[1L] else "unknown"
    stop(
      "Database monitoring functions are restricted to database administrators.\n",
      "Connected as: '", who, "'\n",
      "Required: PostgreSQL superuser OR member of 'pg_monitor' ",
      "OR member of 'db_manager'.\n",
      "Ask your database administrator to run one of:\n",
      "  GRANT pg_monitor TO ", who, ";   -- preferred on managed PostgreSQL\n",
      "  GRANT db_manager TO ", who, ";   -- if a custom monitoring role exists"
    )
  }

  invisible(TRUE)
}


# ---------------------------------------------------------------------------
# Snapshot collection
# ---------------------------------------------------------------------------

#' Collect database activity metrics
#'
#' Queries PostgreSQL system views to capture a snapshot of database activity:
#' active connections, cache hit rates, table access patterns, and sizes.
#'
#' Restricted to database administrators: PostgreSQL superusers, members of
#' the predefined \code{pg_monitor} role (PostgreSQL >= 10), or members of
#' the custom \code{db_manager} role.  Regular users will receive an
#' informative error.
#'
#' @param con A database connection from \code{\link{call.mydb}}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{timestamp}{POSIXct snapshot time}
#'   \item{active_connections}{data.frame from pg_stat_activity}
#'   \item{db_stats}{data.frame from pg_stat_database (cache, transactions)}
#'   \item{table_stats}{data.frame from pg_stat_user_tables (top 20 by access)}
#'   \item{db_sizes}{data.frame of database sizes}
#'   \item{table_sizes}{data.frame of largest tables in current database}
#'   \item{locks}{data.frame of ungranted locks (empty if none)}
#' }
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' activity <- get_db_activity(con)
#' print_db_activity(activity)
#' }
#' @export
get_db_activity <- function(con) {
  .check_monitoring_permission(con)

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  timestamp <- Sys.time()

  active_connections <- tryCatch(
    DBI::dbGetQuery(actual_con, "
      SELECT
        pid,
        usename,
        application_name,
        client_addr::text        AS client_addr,
        state,
        wait_event_type,
        wait_event,
        ROUND(EXTRACT(EPOCH FROM (now() - query_start))::numeric, 1) AS duration_sec,
        LEFT(query, 120)         AS query_preview
      FROM pg_stat_activity
      WHERE datname = current_database()
        AND pid <> pg_backend_pid()
      ORDER BY query_start DESC NULLS LAST
    "),
    error = function(e) {
      message("pg_stat_activity unavailable: ", e$message)
      data.frame()
    }
  )

  db_stats <- tryCatch(
    DBI::dbGetQuery(actual_con, "
      SELECT
        datname,
        numbackends,
        xact_commit,
        xact_rollback,
        blks_read,
        blks_hit,
        ROUND(
          blks_hit::numeric / NULLIF(blks_hit + blks_read, 0) * 100, 1
        )                        AS cache_hit_pct,
        tup_inserted,
        tup_updated,
        tup_deleted,
        conflicts,
        deadlocks,
        stats_reset::text        AS stats_reset
      FROM pg_stat_database
      WHERE datname NOT IN ('template0', 'template1', 'postgres')
      ORDER BY datname
    "),
    error = function(e) {
      message("pg_stat_database unavailable: ", e$message)
      data.frame()
    }
  )

  table_stats <- tryCatch(
    DBI::dbGetQuery(actual_con, "
      SELECT
        schemaname,
        relname                  AS table_name,
        seq_scan,
        seq_tup_read,
        idx_scan,
        idx_tup_fetch,
        n_tup_ins,
        n_tup_upd,
        n_tup_del,
        n_live_tup,
        n_dead_tup,
        last_autovacuum::text    AS last_autovacuum,
        last_autoanalyze::text   AS last_autoanalyze
      FROM pg_stat_user_tables
      ORDER BY (COALESCE(seq_scan, 0) + COALESCE(idx_scan, 0)) DESC
      LIMIT 20
    "),
    error = function(e) {
      message("pg_stat_user_tables unavailable: ", e$message)
      data.frame()
    }
  )

  db_sizes <- tryCatch(
    DBI::dbGetQuery(actual_con, "
      SELECT
        datname,
        pg_size_pretty(pg_database_size(datname)) AS size_pretty,
        pg_database_size(datname)                  AS size_bytes
      FROM pg_database
      WHERE datname NOT IN ('template0', 'template1')
      ORDER BY size_bytes DESC
    "),
    error = function(e) {
      message("pg_database_size unavailable: ", e$message)
      data.frame()
    }
  )

  table_sizes <- tryCatch(
    DBI::dbGetQuery(actual_con, "
      SELECT
        schemaname,
        tablename,
        pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS total_size,
        pg_size_pretty(pg_relation_size(schemaname||'.'||tablename))       AS table_size,
        pg_size_pretty(pg_indexes_size(schemaname||'.'||tablename))        AS index_size,
        pg_total_relation_size(schemaname||'.'||tablename)                 AS size_bytes
      FROM pg_tables
      WHERE schemaname NOT IN ('pg_catalog', 'information_schema')
      ORDER BY size_bytes DESC
      LIMIT 15
    "),
    error = function(e) {
      message("table sizes unavailable: ", e$message)
      data.frame()
    }
  )

  locks <- tryCatch(
    DBI::dbGetQuery(actual_con, "
      SELECT
        l.pid,
        l.locktype,
        l.mode,
        l.granted,
        a.usename,
        LEFT(a.query, 80) AS query_preview
      FROM pg_locks l
      JOIN pg_stat_activity a ON l.pid = a.pid
      WHERE NOT l.granted
    "),
    error = function(e) data.frame()
  )

  structure(
    list(
      timestamp          = timestamp,
      active_connections = active_connections,
      db_stats           = db_stats,
      table_stats        = table_stats,
      db_sizes           = db_sizes,
      table_sizes        = table_sizes,
      locks              = locks
    ),
    class = "db_activity"
  )
}


#' Print a database activity summary to the console
#'
#' Formats and prints the output of \code{\link{get_db_activity}} using
#' \pkg{cli} for readable, colour-highlighted output.
#'
#' @param x A \code{db_activity} object from \code{\link{get_db_activity}}.
#' @param ... Ignored.
#'
#' @return \code{x} invisibly.
#' @export
print.db_activity <- function(x, ...) {
  cli::cli_rule(
    left = cli::style_bold("DB Activity Snapshot"),
    right = format(x$timestamp, "%Y-%m-%d %H:%M:%S")
  )

  # --- Connections --------------------------------------------------------
  cli::cli_h2("Active connections")
  n <- nrow(x$active_connections)
  if (n == 0L) {
    cli::cli_alert_success("No other connections active.")
  } else {
    cli::cli_alert_info("{n} connection{?s} found.")
    cols <- intersect(
      c("usename", "state", "duration_sec", "wait_event", "query_preview"),
      names(x$active_connections)
    )
    print(x$active_connections[, cols, drop = FALSE])
  }

  # --- DB stats -----------------------------------------------------------
  cli::cli_h2("Database statistics")
  if (nrow(x$db_stats) == 0L) {
    cli::cli_alert_warning("Statistics not available.")
  } else {
    for (i in seq_len(nrow(x$db_stats))) {
      r <- x$db_stats[i, ]
      hit <- if (!is.na(r$cache_hit_pct)) paste0(r$cache_hit_pct, "%") else "N/A"
      cli::cli_bullets(c(
        " " = "{.strong {r$datname}}: {r$numbackends} backend{?s},
                cache hit {hit},
                commits {r$xact_commit},
                deadlocks {r$deadlocks}"
      ))
    }
  }

  # --- Sizes --------------------------------------------------------------
  cli::cli_h2("Database sizes")
  if (nrow(x$db_sizes) > 0L) {
    for (i in seq_len(nrow(x$db_sizes))) {
      r <- x$db_sizes[i, ]
      cli::cli_bullets(c(" " = "{.strong {r$datname}}: {r$size_pretty}"))
    }
  }

  # --- Top tables ---------------------------------------------------------
  cli::cli_h2("Most-accessed tables (top 10)")
  if (nrow(x$table_stats) > 0L) {
    top <- utils::head(x$table_stats, 10)
    cols <- intersect(
      c("table_name", "seq_scan", "idx_scan", "n_live_tup", "n_dead_tup"),
      names(top)
    )
    print(top[, cols, drop = FALSE])
  }

  # --- Locks --------------------------------------------------------------
  if (nrow(x$locks) > 0L) {
    cli::cli_alert_danger(
      "{nrow(x$locks)} ungranted lock{?s} detected — possible contention!"
    )
    print(x$locks)
  }

  cli::cli_rule()
  invisible(x)
}


#' Save a database activity report as an HTML file
#'
#' Generates a self-contained HTML snapshot of a \code{\link{get_db_activity}}
#' result and optionally opens it in the default browser.
#'
#' @param activity A \code{db_activity} object from \code{\link{get_db_activity}}.
#' @param output_dir Directory where the HTML file is saved. Defaults to the
#'   current working directory.
#' @param open Logical. Open the file in the default browser after saving?
#'   Defaults to \code{TRUE}.
#'
#' @return Path to the saved HTML file (invisibly).
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' activity <- get_db_activity(con)
#' save_db_activity_report(activity, output_dir = "~/db_reports")
#' }
#' @export
save_db_activity_report <- function(activity, output_dir = ".", open = TRUE) {

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for HTML reports. Install it with install.packages('knitr').")
  }

  ts_label <- format(activity$timestamp, "%Y%m%d_%H%M%S")
  out_file  <- file.path(output_dir, paste0("db_activity_", ts_label, ".html"))

  .html_table <- function(df, caption = NULL) {
    if (is.null(df) || nrow(df) == 0L) {
      return("<p class='none'>No data.</p>")
    }
    tbl <- knitr::kable(df, format = "html", caption = caption,
                        table.attr = 'class="data-table"')
    as.character(tbl)
  }

  .badge <- function(val, good_above = NULL, bad_below = NULL) {
    cls <- "badge-neutral"
    if (!is.null(good_above) && !is.na(val) && val >= good_above) cls <- "badge-good"
    if (!is.null(bad_below)  && !is.na(val) && val <  bad_below)  cls <- "badge-warn"
    sprintf('<span class="%s">%s</span>', cls, val)
  }

  # -- Summary bar ---------------------------------------------------------
  n_conn  <- nrow(activity$active_connections)
  n_locks <- nrow(activity$locks)

  hit_pct <- if (nrow(activity$db_stats) > 0 && "cache_hit_pct" %in% names(activity$db_stats)) {
    stats::median(activity$db_stats$cache_hit_pct, na.rm = TRUE)
  } else NA_real_

  summary_html <- sprintf(
    '<div class="summary-bar">
       <div class="stat-card">
         <div class="stat-label">Active connections</div>
         <div class="stat-value">%s</div>
       </div>
       <div class="stat-card">
         <div class="stat-label">Cache hit rate</div>
         <div class="stat-value">%s</div>
       </div>
       <div class="stat-card">
         <div class="stat-label">Ungranted locks</div>
         <div class="stat-value">%s</div>
       </div>
     </div>',
    .badge(n_conn),
    .badge(if (!is.na(hit_pct)) paste0(hit_pct, "%") else "N/A",
           good_above = NULL, bad_below = NULL),
    .badge(n_locks, bad_below = 1)
  )

  # -- Assemble sections ---------------------------------------------------
  sections <- paste(
    .html_section("Active Connections",
                  .html_table(activity$active_connections)),
    .html_section("Database Statistics (cache, transactions)",
                  .html_table(activity$db_stats)),
    .html_section("Database Sizes",
                  .html_table(activity$db_sizes[, c("datname", "size_pretty"), drop = FALSE])),
    .html_section("Largest Tables",
                  .html_table(activity$table_sizes[
                    , setdiff(names(activity$table_sizes), "size_bytes"), drop = FALSE
                  ])),
    .html_section("Most-Accessed Tables (top 20)",
                  .html_table(activity$table_stats)),
    if (n_locks > 0)
      .html_section("Ungranted Locks", .html_table(activity$locks), warn = TRUE)
    else ""
  )

  html <- .build_html_page(
    title   = paste("DB Activity —", format(activity$timestamp, "%Y-%m-%d %H:%M:%S")),
    summary = summary_html,
    body    = sections
  )

  writeLines(html, out_file, useBytes = FALSE)
  cli::cli_alert_success("Report saved: {.file {out_file}}")

  if (isTRUE(open)) utils::browseURL(out_file)

  invisible(out_file)
}


#' Collect, print, and save a database activity report in one call
#'
#' Convenience wrapper that calls \code{\link{get_db_activity}},
#' \code{\link{print.db_activity}}, and \code{\link{save_db_activity_report}}.
#'
#' @param con A database connection from \code{\link{call.mydb}}.
#' @param output_dir Directory for the HTML report (passed to
#'   \code{\link{save_db_activity_report}}). Set to \code{NULL} to skip
#'   saving.
#' @param open Logical. Open the HTML report in the browser? Default
#'   \code{TRUE}.
#'
#' @return A \code{db_activity} object (invisibly).
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' monitor_db(con)
#'
#' # Save to a specific folder without opening browser
#' monitor_db(con, output_dir = "~/db_reports", open = FALSE)
#' }
#' @export
monitor_db <- function(con, output_dir = ".", open = TRUE) {
  activity <- get_db_activity(con)
  print(activity)
  if (!is.null(output_dir)) {
    save_db_activity_report(activity, output_dir = output_dir, open = open)
  }
  invisible(activity)
}


# ---------------------------------------------------------------------------
# Activity logging (scheduled snapshots)
# ---------------------------------------------------------------------------

#' Initialise the activity log directory and CSV files
#'
#' Creates \code{log_dir} if needed and writes header rows to
#' \file{connections_log.csv} and \file{stats_log.csv} when the files do not
#' yet exist.
#'
#' @param log_dir Path to the directory that will hold the log files.
#'
#' @return \code{log_dir} invisibly.
#' @export
init_activity_log <- function(log_dir) {
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

  conn_file  <- file.path(log_dir, "connections_log.csv")
  stats_file <- file.path(log_dir, "stats_log.csv")

  if (!file.exists(conn_file)) {
    write.csv(
      data.frame(
        snapshot_time    = character(),
        username         = character(),
        application_name = character(),
        client_addr      = character(),
        state            = character(),
        duration_sec     = numeric()
      ),
      conn_file, row.names = FALSE
    )
    cli::cli_alert_info("Created {.file {conn_file}}")
  }

  if (!file.exists(stats_file)) {
    write.csv(
      data.frame(
        snapshot_time  = character(),
        datname        = character(),
        numbackends    = integer(),
        xact_commit    = numeric(),
        xact_rollback  = numeric(),
        tup_inserted   = numeric(),
        tup_updated    = numeric(),
        tup_deleted    = numeric(),
        blks_read      = numeric(),
        blks_hit       = numeric(),
        deadlocks      = integer()
      ),
      stats_file, row.names = FALSE
    )
    cli::cli_alert_info("Created {.file {stats_file}}")
  }

  invisible(log_dir)
}


#' Append a 30-minute activity snapshot to the log files
#'
#' Calls \code{\link{get_db_activity}} and appends one row per active
#' connection to \file{connections_log.csv} and one row per database to
#' \file{stats_log.csv}.  Designed to be called on a regular schedule
#' (e.g. every 30 minutes via Windows Task Scheduler).
#'
#' @param con A database connection from \code{\link{call.mydb}}.
#' @param log_dir Directory containing the log files.  Will be initialised
#'   with \code{\link{init_activity_log}} if it does not exist yet.
#' @param verbose Logical.  Print a one-line confirmation?  Default
#'   \code{TRUE}.
#'
#' @return The \code{db_activity} snapshot (invisibly).
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' log_db_snapshot(con, log_dir = "~/db_logs")
#' }
#' @export
log_db_snapshot <- function(con, log_dir, verbose = TRUE) {
  init_activity_log(log_dir)

  activity <- get_db_activity(con)
  ts       <- format(activity$timestamp, "%Y-%m-%d %H:%M:%S")

  conn_file  <- file.path(log_dir, "connections_log.csv")
  stats_file <- file.path(log_dir, "stats_log.csv")

  # -- connections ----------------------------------------------------------
  if (nrow(activity$active_connections) > 0L) {
    conn_rows <- data.frame(
      snapshot_time    = ts,
      username         = activity$active_connections$usename,
      application_name = activity$active_connections$application_name,
      client_addr      = activity$active_connections$client_addr,
      state            = activity$active_connections$state,
      duration_sec     = activity$active_connections$duration_sec,
      stringsAsFactors = FALSE
    )
  } else {
    conn_rows <- data.frame(
      snapshot_time    = ts,
      username         = NA_character_,
      application_name = NA_character_,
      client_addr      = NA_character_,
      state            = NA_character_,
      duration_sec     = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  write.table(conn_rows, conn_file, sep = ",", col.names = FALSE,
              row.names = FALSE, append = TRUE, quote = TRUE)

  # -- db stats -------------------------------------------------------------
  if (nrow(activity$db_stats) > 0L) {
    stats_rows <- data.frame(
      snapshot_time = ts,
      datname       = activity$db_stats$datname,
      numbackends   = activity$db_stats$numbackends,
      xact_commit   = activity$db_stats$xact_commit,
      xact_rollback = activity$db_stats$xact_rollback,
      tup_inserted  = activity$db_stats$tup_inserted,
      tup_updated   = activity$db_stats$tup_updated,
      tup_deleted   = activity$db_stats$tup_deleted,
      blks_read     = activity$db_stats$blks_read,
      blks_hit      = activity$db_stats$blks_hit,
      deadlocks     = activity$db_stats$deadlocks,
      stringsAsFactors = FALSE
    )
    write.table(stats_rows, stats_file, sep = ",", col.names = FALSE,
                row.names = FALSE, append = TRUE, quote = TRUE)
  }

  if (verbose) {
    n <- sum(!is.na(conn_rows$username))
    cli::cli_alert_success(
      "Snapshot logged at {ts} — {n} active connection{?s}."
    )
  }

  invisible(activity)
}


#' Read the activity log files into R
#'
#' @param log_dir Directory containing \file{connections_log.csv} and
#'   \file{stats_log.csv}.
#' @param from Optional \code{Date} or \code{POSIXct}. Filter rows on or
#'   after this time.
#' @param to Optional \code{Date} or \code{POSIXct}. Filter rows up to and
#'   including this time.
#'
#' @return A named list with elements \code{connections} and \code{stats},
#'   both data.frames with a \code{snapshot_time} POSIXct column.
#'
#' @examples
#' \dontrun{
#' log <- read_activity_log("~/db_logs")
#' log <- read_activity_log("~/db_logs", from = Sys.Date() - 30)
#' }
#' @export
read_activity_log <- function(log_dir, from = NULL, to = NULL) {
  conn_file  <- file.path(log_dir, "connections_log.csv")
  stats_file <- file.path(log_dir, "stats_log.csv")

  if (!file.exists(conn_file) || !file.exists(stats_file)) {
    stop("Log files not found in '", log_dir, "'. Run init_activity_log() first.")
  }

  connections <- utils::read.csv(conn_file, stringsAsFactors = FALSE)
  stats       <- utils::read.csv(stats_file, stringsAsFactors = FALSE)

  connections$snapshot_time <- as.POSIXct(connections$snapshot_time,
                                          format = "%Y-%m-%d %H:%M:%S")
  stats$snapshot_time       <- as.POSIXct(stats$snapshot_time,
                                          format = "%Y-%m-%d %H:%M:%S")

  if (!is.null(from)) {
    from <- as.POSIXct(from)
    connections <- connections[connections$snapshot_time >= from, , drop = FALSE]
    stats       <- stats[stats$snapshot_time >= from, , drop = FALSE]
  }
  if (!is.null(to)) {
    to <- as.POSIXct(to)
    connections <- connections[connections$snapshot_time <= to, , drop = FALSE]
    stats       <- stats[stats$snapshot_time <= to, , drop = FALSE]
  }

  list(connections = connections, stats = stats)
}


#' Summarise logged connection activity
#'
#' Counts the number of 30-minute slots each user/application was seen active,
#' and computes delta activity metrics (transactions, inserts, etc.) between
#' consecutive stats snapshots.
#'
#' @param log A list returned by \code{\link{read_activity_log}}.
#'
#' @return A list with:
#' \describe{
#'   \item{by_user}{data.frame — slots active, estimated minutes, and peak
#'     hour per user}
#'   \item{by_user_app}{data.frame — same broken down by application}
#'   \item{hourly}{data.frame — connection count per hour}
#'   \item{stats_delta}{data.frame — per-snapshot delta activity metrics}
#' }
#'
#' @examples
#' \dontrun{
#' log     <- read_activity_log("~/db_logs", from = Sys.Date() - 30)
#' summary <- summarize_activity_log(log)
#' summary$by_user
#' }
#' @export
summarize_activity_log <- function(log) {
  conn  <- log$connections[!is.na(log$connections$username), , drop = FALSE]
  stats <- log$stats

  # -- per-user summary -----------------------------------------------------
  if (nrow(conn) > 0L) {
    conn$date <- as.Date(conn$snapshot_time)
    conn$hour <- as.integer(format(conn$snapshot_time, "%H"))

    by_user <- do.call(rbind, lapply(
      split(conn, conn$username),
      function(d) {
        slots <- length(unique(d$snapshot_time))
        data.frame(
          username         = d$username[1L],
          slots_active     = slots,
          estimated_min    = slots * 30L,
          first_seen       = min(d$snapshot_time),
          last_seen        = max(d$snapshot_time),
          peak_hour        = as.integer(names(which.max(table(d$hour)))),
          stringsAsFactors = FALSE
        )
      }
    ))
    rownames(by_user) <- NULL
    by_user <- by_user[order(-by_user$slots_active), ]

    by_user_app <- do.call(rbind, lapply(
      split(conn, paste(conn$username, conn$application_name, sep = " | ")),
      function(d) {
        data.frame(
          username         = d$username[1L],
          application_name = d$application_name[1L],
          slots_active     = length(unique(d$snapshot_time)),
          stringsAsFactors = FALSE
        )
      }
    ))
    rownames(by_user_app) <- NULL
    by_user_app <- by_user_app[order(-by_user_app$slots_active), ]

    conn$slot_hour <- as.POSIXct(
      format(conn$snapshot_time, "%Y-%m-%d %H:00:00"),
      format = "%Y-%m-%d %H:%M:%S"
    )
    hourly <- do.call(rbind, lapply(
      split(conn, conn$slot_hour),
      function(d) {
        data.frame(
          hour             = d$slot_hour[1L],
          n_connections    = length(unique(d$username)),
          stringsAsFactors = FALSE
        )
      }
    ))
    rownames(hourly) <- NULL
    hourly <- hourly[order(hourly$hour), ]
  } else {
    by_user     <- data.frame()
    by_user_app <- data.frame()
    hourly      <- data.frame()
  }

  # -- stats deltas ---------------------------------------------------------
  if (nrow(stats) > 1L) {
    stats_delta <- do.call(rbind, lapply(
      split(stats, stats$datname),
      function(d) {
        d <- d[order(d$snapshot_time), ]
        if (nrow(d) < 2L) return(NULL)
        delta <- d[-1L, , drop = FALSE]
        numeric_cols <- c("xact_commit", "xact_rollback",
                          "tup_inserted", "tup_updated", "tup_deleted",
                          "blks_read", "blks_hit")
        for (col in numeric_cols) {
          if (col %in% names(d)) {
            delta[[col]] <- pmax(0, d[[col]][-1L] - d[[col]][-nrow(d)])
          }
        }
        delta$cache_hit_pct <- round(
          delta$blks_hit / pmax(1, delta$blks_hit + delta$blks_read) * 100, 1
        )
        delta
      }
    ))
    rownames(stats_delta) <- NULL
  } else {
    stats_delta <- data.frame()
  }

  list(
    by_user     = by_user,
    by_user_app = by_user_app,
    hourly      = hourly,
    stats_delta = stats_delta
  )
}


#' Plot logged database activity
#'
#' Produces interactive \pkg{plotly} charts from a
#' \code{\link{summarize_activity_log}} result:
#' \enumerate{
#'   \item Active connections over time (line chart)
#'   \item Slots active per user (bar chart)
#'   \item Transactions per 30-min slot by database (line chart)
#' }
#'
#' @param summary A list returned by \code{\link{summarize_activity_log}}.
#' @param title Optional character string added to each chart title.
#'
#' @return A named list of three \code{plotly} objects:
#'   \code{connections_over_time}, \code{slots_per_user},
#'   \code{transactions_over_time}.
#'
#' @examples
#' \dontrun{
#' log     <- read_activity_log("~/db_logs", from = Sys.Date() - 30)
#' summary <- summarize_activity_log(log)
#' charts  <- plot_activity_log(summary)
#' charts$connections_over_time
#' charts$slots_per_user
#' }
#' @export
plot_activity_log <- function(summary, title = NULL) {
  pfx <- if (!is.null(title)) paste0(title, " — ") else ""

  # 1. Connections over time ------------------------------------------------
  if (nrow(summary$hourly) > 0L) {
    p1 <- plotly::plot_ly(
      summary$hourly,
      x = ~hour, y = ~n_connections, type = "scatter", mode = "lines+markers",
      line    = list(color = "#1a3a5c", width = 2),
      marker  = list(color = "#1a3a5c", size = 5),
      hovertemplate = "%{x}<br>%{y} user(s)<extra></extra>"
    ) |>
      plotly::layout(
        title  = list(text = paste0(pfx, "Active users per 30-min slot")),
        xaxis  = list(title = "Time"),
        yaxis  = list(title = "Distinct users", rangemode = "tozero"),
        hovermode = "x unified"
      )
  } else {
    p1 <- plotly::plot_ly() |>
      plotly::layout(title = list(text = "No connection data"))
  }

  # 2. Slots per user -------------------------------------------------------
  if (nrow(summary$by_user) > 0L) {
    d <- summary$by_user
    d$username <- factor(d$username,
                         levels = d$username[order(d$slots_active)])
    p2 <- plotly::plot_ly(
      d,
      x = ~slots_active, y = ~username, type = "bar",
      orientation = "h",
      marker = list(color = "#2e7d5e"),
      text   = ~paste0(estimated_min, " min est."),
      hovertemplate = "%{y}: %{x} slots (%{text})<extra></extra>"
    ) |>
      plotly::layout(
        title  = list(text = paste0(pfx, "Activity slots per user")),
        xaxis  = list(title = "30-min slots active"),
        yaxis  = list(title = ""),
        margin = list(l = 120)
      )
  } else {
    p2 <- plotly::plot_ly() |>
      plotly::layout(title = list(text = "No user data"))
  }

  # 3. Transactions over time -----------------------------------------------
  if (nrow(summary$stats_delta) > 0L &&
      "xact_commit" %in% names(summary$stats_delta)) {
    colours <- c("#1a3a5c", "#2e7d5e", "#c0392b", "#8e44ad")
    dbs <- unique(summary$stats_delta$datname)
    traces <- lapply(seq_along(dbs), function(i) {
      d <- summary$stats_delta[summary$stats_delta$datname == dbs[i], ]
      list(
        x    = d$snapshot_time,
        y    = d$xact_commit,
        name = dbs[i],
        type = "scatter", mode = "lines",
        line = list(color = colours[(i - 1L) %% length(colours) + 1L], width = 2)
      )
    })
    p3 <- plotly::plot_ly()
    for (tr in traces) p3 <- plotly::add_trace(p3, x = tr$x, y = tr$y,
                                                name = tr$name, type = tr$type,
                                                mode = tr$mode, line = tr$line)
    p3 <- plotly::layout(p3,
      title     = list(text = paste0(pfx, "Commits per 30-min slot")),
      xaxis     = list(title = "Time"),
      yaxis     = list(title = "Commits", rangemode = "tozero"),
      hovermode = "x unified"
    )
  } else {
    p3 <- plotly::plot_ly() |>
      plotly::layout(title = list(text = "No stats data"))
  }

  list(
    connections_over_time  = p1,
    slots_per_user         = p2,
    transactions_over_time = p3
  )
}


# ---------------------------------------------------------------------------
# Windows Task Scheduler helper
# ---------------------------------------------------------------------------

#' Register the 30-minute recording script in Windows Task Scheduler
#'
#' Creates a Windows Task Scheduler task that runs
#' \file{record_db_activity.R} every 30 minutes, passing database credentials
#' as environment variables.
#'
#' @param log_dir Directory where the CSV logs will be written.
#' @param db_user Database username (stored as a Task Scheduler env var).
#' @param db_password Database password (stored as a Task Scheduler env var).
#' @param task_name Name for the scheduled task.  Defaults to
#'   \code{"CafriplotsR_db_monitor"}.
#' @param rscript_exe Path to \file{Rscript.exe}.  Auto-detected by default.
#'
#' @return Invisible \code{TRUE} on success.
#'
#' @details
#' The task is created via \code{schtasks.exe} (Windows only).  Run once
#' from an R session with administrator privileges, or add the task manually
#' using the Windows Task Scheduler GUI with the printed command.
#'
#' @examples
#' \dontrun{
#' setup_db_activity_scheduler(
#'   log_dir     = "C:/db_logs/cafri",
#'   db_user     = "your_username",
#'   db_password = "your_password"
#' )
#' }
#' @export
setup_db_activity_scheduler <- function(
    log_dir,
    db_user,
    db_password,
    task_name   = "CafriplotsR_db_monitor",
    rscript_exe = NULL
) {
  if (.Platform$OS.type != "windows") {
    stop("setup_db_activity_scheduler() only supports Windows. ",
         "On Linux/Mac, use cron: */30 * * * * Rscript <script_path>")
  }

  if (is.null(rscript_exe)) {
    rscript_exe <- file.path(R.home("bin"), "Rscript.exe")
  }

  script_path <- system.file("scripts", "record_db_activity.R",
                              package = "CafriplotsR")
  if (!nzchar(script_path)) {
    stop("Script not found — is CafriplotsR installed?")
  }

  log_dir <- normalizePath(log_dir, mustWork = FALSE)

  # schtasks /Create command
  cmd <- sprintf(
    paste(
      'schtasks /Create /F /TN "%s"',
      '/TR "\\"%s\\" \\"%s\\""',
      '/SC MINUTE /MO 30',
      '/RU "%s"',
      '/RP "%s"',
      '/IT'
    ),
    task_name,
    rscript_exe, script_path,
    Sys.getenv("USERNAME"),
    db_password
  )

  env_cmd <- sprintf(
    'schtasks /Change /TN "%s" /RU "%s" /RP "%s"',
    task_name, Sys.getenv("USERNAME"), db_password
  )

  cli::cli_alert_info("Creating scheduled task: {.strong {task_name}}")
  cli::cli_alert_info("Script : {.file {script_path}}")
  cli::cli_alert_info("Log dir: {.file {log_dir}}")

  # Set env vars via registry path after task creation
  result <- tryCatch({
    system2("schtasks", args = c(
      "/Create", "/F",
      "/TN", task_name,
      "/TR", paste0('"', rscript_exe, '" "', script_path, '"'),
      "/SC", "MINUTE", "/MO", "30",
      "/IT"
    ), stdout = TRUE, stderr = TRUE)
  }, error = function(e) e)

  if (inherits(result, "error")) {
    cli::cli_alert_warning("Could not auto-register task (try running R as administrator).")
    cli::cli_alert_info("Run this manually in an admin PowerShell:")
    cat(sprintf(
      '\n$env:CAFRI_DB_USER="%s"\n$env:CAFRI_DB_PASSWORD="<your_password>"\n$env:CAFRI_LOG_DIR="%s"\nRscript "%s"\n\n',
      db_user, log_dir, script_path
    ))
  } else {
    cli::cli_alert_success("Task '{task_name}' registered — runs every 30 minutes.")
    cli::cli_alert_info(
      "Set credentials as env vars in the Task Scheduler task properties:"
    )
    cat(sprintf("  CAFRI_DB_USER     = %s\n", db_user))
    cat(sprintf("  CAFRI_DB_PASSWORD = (your password)\n"))
    cat(sprintf("  CAFRI_LOG_DIR     = %s\n", log_dir))
  }

  invisible(TRUE)
}


# ---------------------------------------------------------------------------
# Internal HTML helpers
# ---------------------------------------------------------------------------

.html_section <- function(title, content, warn = FALSE) {
  cls <- if (warn) "section section-warn" else "section"
  sprintf(
    '<div class="%s"><h2>%s</h2>%s</div>',
    cls, title, content
  )
}

.build_html_page <- function(title, summary, body) {
  css <- "
    body { font-family: 'Segoe UI', Arial, sans-serif; margin: 0; background: #f5f7fa; color: #222; }
    header { background: #1a3a5c; color: #fff; padding: 18px 32px; }
    header h1 { margin: 0; font-size: 1.4em; }
    header p  { margin: 4px 0 0; opacity: .75; font-size: .9em; }
    .summary-bar { display: flex; gap: 16px; padding: 20px 32px; background: #fff;
                   border-bottom: 1px solid #dce1e9; }
    .stat-card { background: #f0f4fa; border-radius: 8px; padding: 12px 20px;
                 min-width: 140px; text-align: center; }
    .stat-label { font-size: .78em; color: #666; text-transform: uppercase;
                  letter-spacing: .05em; }
    .stat-value { font-size: 1.7em; font-weight: bold; margin-top: 4px; }
    main { padding: 24px 32px; }
    .section { background: #fff; border-radius: 8px; padding: 20px 24px;
               margin-bottom: 20px; box-shadow: 0 1px 4px rgba(0,0,0,.07); }
    .section-warn { border-left: 4px solid #e05c40; }
    .section h2 { margin-top: 0; font-size: 1.05em; color: #1a3a5c;
                  border-bottom: 1px solid #eee; padding-bottom: 8px; }
    .data-table { border-collapse: collapse; width: 100%; font-size: .85em; }
    .data-table th { background: #1a3a5c; color: #fff; padding: 7px 12px;
                     text-align: left; white-space: nowrap; }
    .data-table td { padding: 6px 12px; border-bottom: 1px solid #eef0f4; }
    .data-table tr:hover td { background: #f0f4fa; }
    .none { color: #999; font-style: italic; }
    .badge-neutral { background: #e8edf5; color: #333; padding: 2px 10px;
                     border-radius: 12px; font-weight: bold; }
    .badge-good    { background: #d4edda; color: #155724; padding: 2px 10px;
                     border-radius: 12px; font-weight: bold; }
    .badge-warn    { background: #f8d7da; color: #721c24; padding: 2px 10px;
                     border-radius: 12px; font-weight: bold; }
  "

  sprintf(
    '<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>%s</title>
  <style>%s</style>
</head>
<body>
  <header>
    <h1>CafriplotsR — Database Activity Report</h1>
    <p>%s</p>
  </header>
  %s
  <main>%s</main>
</body>
</html>',
    title, css, title, summary, body
  )
}
