#' Set Up postgres_fdw for table_idtax
#'
#' Upgrades `table_idtax` so its materialized view queries `rainbio` directly
#' via a Foreign Data Wrapper (FDW), rather than via a local staging table.
#'
#' After this setup, the `refresh_table_idtax()` SECURITY DEFINER function
#' actually fetches fresh data from `rainbio` — meaning any authorised user
#' can refresh without needing an R-level connection to the taxa database.
#'
#' ## What the function does (in order)
#'
#' 1. **Diagnoses** what is already in place (extension, server, foreign table,
#'    current `table_idtax` definition) — safe to run at any time.
#' 2. **Enables** the `postgres_fdw` extension (may fail without superuser; often
#'    pre-installed on managed hosts like OVH).
#' 3. **Creates** the foreign server pointing to `rainbio`.
#' 4. **Creates** a user mapping so the SECURITY DEFINER function (running as
#'    `fdw_user`) can authenticate to `rainbio`.
#' 5. **Creates** the foreign table `table_taxa_foreign`.
#' 6. **Recreates** `table_idtax` as a materialized view over `table_taxa_foreign`.
#' 7. **Updates** `refresh_table_idtax()` to remove the now-unnecessary
#'    `table_idtax_temp` references (the function body stays the same; only its
#'    source data changes because the matview definition changed).
#'
#' Use `dry_run = TRUE` to see what would happen without making any changes.
#' Use `diagnose_only = TRUE` to inspect the current state without touching anything.
#'
#' @param con Main database connection (`plots_transects`). If NULL calls
#'   `call.mydb()`.
#' @param rainbio_host Host of the rainbio PostgreSQL server. Defaults to
#'   `"localhost"` (typical for a single OVH managed instance).
#' @param rainbio_port Port of the rainbio server. Default `5432`.
#' @param rainbio_dbname Name of the taxa database. Default `"rainbio"`.
#' @param fdw_user PostgreSQL username used for the FDW user mapping. This
#'   user must exist in `rainbio` and have SELECT on `table_taxa`. Defaults to
#'   the current session user.
#' @param fdw_password Password for `fdw_user` in `rainbio`. Required unless
#'   trust authentication is configured between the two databases.
#' @param server_name Name to give the foreign server object. Default
#'   `"rainbio_server"`.
#' @param dry_run Logical. If `TRUE`, prints SQL that would be executed without
#'   running it. Default `FALSE`.
#' @param diagnose_only Logical. If `TRUE`, only checks current state and
#'   returns a diagnostic report — no changes made. Default `FALSE`.
#'
#' @return Invisibly, a named list with one element per step:
#'   `"ok"` (succeeded), `"skipped"` (already in place), or `"failed:<msg>"`
#'   (hit a permission or other error).
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # 1. Diagnose current state (safe, no changes)
#' setup_fdw_table_idtax(con, diagnose_only = TRUE)
#'
#' # 2. Dry run — see the SQL without executing
#' setup_fdw_table_idtax(
#'   con,
#'   rainbio_host     = "localhost",
#'   rainbio_port     = 5432,
#'   rainbio_dbname   = "rainbio",
#'   fdw_user         = "dauby",
#'   fdw_password     = "your_password",
#'   dry_run          = TRUE
#' )
#'
#' # 3. Full setup
#' setup_fdw_table_idtax(
#'   con,
#'   rainbio_host     = "localhost",
#'   rainbio_port     = 5432,
#'   rainbio_dbname   = "rainbio",
#'   fdw_user         = "dauby",
#'   fdw_password     = "your_password"
#' )
#'
#' # 4. Verify the result
#' setup_fdw_table_idtax(con, diagnose_only = TRUE)
#' }
#'
#' @seealso [update_taxa_link_table()], [migrate_table_idtax_to_materialized_view()]
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
setup_fdw_table_idtax <- function(
    con             = NULL,
    rainbio_host    = "localhost",
    rainbio_port    = 5432L,
    rainbio_dbname  = "rainbio",
    fdw_user        = NULL,
    fdw_password    = NULL,
    server_name     = "rainbio_server",
    dry_run         = FALSE,
    diagnose_only   = FALSE
) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  # Resolve current user if fdw_user not supplied
  if (is.null(fdw_user)) {
    fdw_user <- tryCatch(
      DBI::dbGetQuery(actual_con, "SELECT current_user AS u;")$u,
      error = function(e) Sys.info()["user"]
    )
  }

  results <- list()

  # ---------------------------------------------------------------------------
  # Helper: execute SQL or just print it
  # ---------------------------------------------------------------------------
  exec <- function(label, sql, critical = TRUE) {
    if (dry_run) {
      cli::cli_h3("[DRY RUN] {label}")
      cli::cli_code(sql)
      results[[label]] <<- "dry_run"
      return(invisible(TRUE))
    }
    tryCatch({
      DBI::dbExecute(actual_con, sql)
      cli::cli_alert_success("{label}")
      results[[label]] <<- "ok"
      invisible(TRUE)
    }, error = function(e) {
      msg <- conditionMessage(e)
      if (critical) {
        cli::cli_alert_danger("{label}: {msg}")
      } else {
        cli::cli_alert_warning("{label} (non-critical): {msg}")
      }
      results[[label]] <<- paste0("failed: ", msg)
      invisible(FALSE)
    })
  }

  query <- function(sql) {
    tryCatch(DBI::dbGetQuery(actual_con, sql), error = function(e) NULL)
  }

  # ---------------------------------------------------------------------------
  # STEP 0 — Diagnose current state
  # ---------------------------------------------------------------------------
  cli::cli_h1("Diagnosing current state")

  diag <- .fdw_diagnose(actual_con, server_name)
  .fdw_print_diag(diag)

  if (diagnose_only) {
    cli::cli_alert_info("diagnose_only = TRUE — stopping here, no changes made.")
    return(invisible(diag))
  }

  if (dry_run) {
    cli::cli_alert_info("dry_run = TRUE — showing SQL only, nothing will be executed.")
    cli::cli_rule()
  }

  # ---------------------------------------------------------------------------
  # STEP 1 — Enable postgres_fdw extension
  # ---------------------------------------------------------------------------
  cli::cli_h1("Step 1: Enable postgres_fdw extension")

  if (diag$extension_installed) {
    cli::cli_alert_success("postgres_fdw already installed — skipping")
    results[["Enable postgres_fdw"]] <- "skipped"
  } else if (!diag$extension_available) {
    cli::cli_alert_danger("postgres_fdw is not available on this server.")
    cli::cli_alert_info("Ask your hosting provider (OVH) to enable the extension.")
    results[["Enable postgres_fdw"]] <- "failed: extension not available"
    return(invisible(results))
  } else {
    exec(
      "Enable postgres_fdw",
      "CREATE EXTENSION postgres_fdw;",
      critical = TRUE
    )
    if (isTRUE(grepl("^failed", results[["Enable postgres_fdw"]])) && !dry_run) {
      cli::cli_alert_info(
        "Cannot CREATE EXTENSION — this requires superuser on standard PostgreSQL."
      )
      cli::cli_alert_info(
        "On OVH managed PostgreSQL, check if the extension is already enabled by
         running: SELECT * FROM pg_extension WHERE extname = 'postgres_fdw';"
      )
      cli::cli_alert_info(
        "If it is listed there, re-run this function (it will detect it and proceed)."
      )
      return(invisible(results))
    }
  }

  # ---------------------------------------------------------------------------
  # STEP 2 — Create foreign server
  # ---------------------------------------------------------------------------
  cli::cli_h1("Step 2: Create foreign server '{server_name}'")

  if (diag$server_exists) {
    cli::cli_alert_success("Foreign server '{server_name}' already exists — skipping")
    results[["Create foreign server"]] <- "skipped"
  } else {
    sql_server <- sprintf(
      "CREATE SERVER %s
         FOREIGN DATA WRAPPER postgres_fdw
         OPTIONS (host '%s', port '%s', dbname '%s');",
      server_name, rainbio_host, as.character(rainbio_port), rainbio_dbname
    )
    exec("Create foreign server", sql_server)
    if (isTRUE(grepl("^failed", results[["Create foreign server"]])) && !dry_run) {
      return(invisible(results))
    }
  }

  # ---------------------------------------------------------------------------
  # STEP 3 — Create user mapping
  # ---------------------------------------------------------------------------
  cli::cli_h1("Step 3: Create user mapping for '{fdw_user}'")

  mapping_exists <- isTRUE(diag$mapping_exists)
  if (mapping_exists) {
    cli::cli_alert_success("User mapping for '{fdw_user}' already exists — skipping")
    results[["Create user mapping"]] <- "skipped"
  } else {
    if (is.null(fdw_password)) {
      cli::cli_alert_warning(
        "fdw_password is NULL — creating mapping without password.
         This only works if trust authentication is configured between the databases."
      )
      sql_mapping <- sprintf(
        "CREATE USER MAPPING FOR %s
           SERVER %s
           OPTIONS (user '%s');",
        fdw_user, server_name, fdw_user
      )
    } else {
      sql_mapping <- sprintf(
        "CREATE USER MAPPING FOR %s
           SERVER %s
           OPTIONS (user '%s', password '%s');",
        fdw_user, server_name, fdw_user, fdw_password
      )
    }
    exec("Create user mapping", sql_mapping)
    if (isTRUE(grepl("^failed", results[["Create user mapping"]])) && !dry_run) {
      return(invisible(results))
    }
  }

  # ---------------------------------------------------------------------------
  # STEP 4 — Create foreign table
  # ---------------------------------------------------------------------------
  cli::cli_h1("Step 4: Create foreign table 'table_taxa_foreign'")

  if (diag$foreign_table_exists) {
    cli::cli_alert_success("Foreign table 'table_taxa_foreign' already exists — skipping")
    results[["Create foreign table"]] <- "skipped"
  } else {
    sql_ftable <- sprintf(
      "CREATE FOREIGN TABLE table_taxa_foreign (
         idtax_n      integer,
         idtax_good_n integer
       )
       SERVER %s
       OPTIONS (schema_name 'public', table_name 'table_taxa');",
      server_name
    )
    exec("Create foreign table", sql_ftable)
    if (isTRUE(grepl("^failed", results[["Create foreign table"]])) && !dry_run) {
      return(invisible(results))
    }
  }

  # Test that the foreign table is actually reachable
  if (!dry_run) {
    cli::cli_alert_info("Testing foreign table connectivity to rainbio...")
    test <- query("SELECT COUNT(*) AS n FROM table_taxa_foreign LIMIT 1;")
    if (is.null(test)) {
      cli::cli_alert_danger(
        "Cannot query table_taxa_foreign.
         Check host/port/dbname/credentials and that the fdw_user has SELECT on table_taxa in rainbio."
      )
      results[["Test foreign table"]] <- "failed: cannot query"
      return(invisible(results))
    }
    cli::cli_alert_success(
      "Foreign table reachable — rainbio.table_taxa has {test$n} rows visible from here."
    )
    results[["Test foreign table"]] <- "ok"
  }

  # ---------------------------------------------------------------------------
  # STEP 5 — Backup & recreate table_idtax over the foreign table
  # ---------------------------------------------------------------------------
  cli::cli_h1("Step 5: Recreate table_idtax over table_taxa_foreign")

  if (!dry_run && diag$matview_uses_fdw) {
    cli::cli_alert_success(
      "table_idtax is already a materialized view over table_taxa_foreign — skipping recreation."
    )
    results[["Recreate materialized view"]] <- "skipped"
  } else {

    # 5a — backup
    exec(
      "Create backup table_idtax_backup",
      "CREATE TABLE IF NOT EXISTS table_idtax_backup AS SELECT * FROM table_idtax;",
      critical = FALSE
    )

    # 5b — drop current object (table or matview)
    exec(
      "Drop existing table_idtax",
      "DROP MATERIALIZED VIEW IF EXISTS table_idtax;
       DROP TABLE IF EXISTS table_idtax;"
    )

    # 5c — create new materialized view
    exec(
      "Create materialized view table_idtax over FDW",
      "CREATE MATERIALIZED VIEW table_idtax AS
         SELECT idtax_n::integer, idtax_good_n::integer
         FROM table_taxa_foreign
       WITH DATA;"
    )

    if (isTRUE(grepl("^failed", results[["Create materialized view table_idtax over FDW"]])) && !dry_run) {
      cli::cli_alert_warning("Attempting to restore from backup...")
      exec(
        "Restore table_idtax from backup",
        "CREATE TABLE table_idtax AS SELECT * FROM table_idtax_backup;",
        critical = FALSE
      )
      return(invisible(results))
    }

    # 5d — indexes
    exec(
      "Create unique index on idtax_n",
      "CREATE UNIQUE INDEX idx_table_idtax_idtax_n ON table_idtax(idtax_n);"
    )
    exec(
      "Create index on idtax_good_n",
      "CREATE INDEX idx_table_idtax_good_n ON table_idtax(idtax_good_n);",
      critical = FALSE
    )

    # 5e — update metadata
    exec(
      "Update table_idtax_metadata",
      sprintf(
        "INSERT INTO table_idtax_metadata
           (table_name, last_updated, updated_by, record_count, source_info)
         VALUES
           ('table_idtax', CURRENT_TIMESTAMP, '%s',
            (SELECT COUNT(*)::integer FROM table_idtax),
            'Recreated via setup_fdw_table_idtax() — source: rainbio via postgres_fdw')
         ON CONFLICT (table_name) DO UPDATE SET
           last_updated  = CURRENT_TIMESTAMP,
           updated_by    = '%s',
           record_count  = (SELECT COUNT(*)::integer FROM table_idtax),
           source_info   = 'Recreated via setup_fdw_table_idtax() — source: rainbio via postgres_fdw';",
        fdw_user, fdw_user
      ),
      critical = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # STEP 6 — Drop the now-redundant staging table
  # ---------------------------------------------------------------------------
  cli::cli_h1("Step 6: Remove staging table table_idtax_temp")

  if (diag$staging_table_exists) {
    exec(
      "Drop table_idtax_temp (staging table, no longer needed)",
      "DROP TABLE IF EXISTS table_idtax_temp;",
      critical = FALSE
    )
  } else {
    cli::cli_alert_success("table_idtax_temp does not exist — nothing to remove")
    results[["Drop staging table"]] <- "skipped"
  }

  # ---------------------------------------------------------------------------
  # STEP 7 — Grant USAGE on foreign server to data managers
  # ---------------------------------------------------------------------------
  cli::cli_h1("Step 7: Grant USAGE on foreign server")

  exec(
    sprintf("Grant USAGE on %s to public", server_name),
    sprintf("GRANT USAGE ON FOREIGN SERVER %s TO PUBLIC;", server_name),
    critical = FALSE
  )

  # ---------------------------------------------------------------------------
  # Summary
  # ---------------------------------------------------------------------------
  cli::cli_rule()
  cli::cli_h1("Summary")

  n_ok      <- sum(unlist(results) == "ok")
  n_skipped <- sum(unlist(results) == "skipped")
  n_dry     <- sum(unlist(results) == "dry_run")
  n_failed  <- sum(grepl("^failed", unlist(results)))

  if (dry_run) {
    cli::cli_alert_info("{n_dry} SQL block(s) shown. Re-run with dry_run = FALSE to apply.")
  } else {
    cli::cli_alert_success("{n_ok} step(s) completed, {n_skipped} step(s) already in place.")
    if (n_failed > 0) {
      cli::cli_alert_danger("{n_failed} step(s) failed — see messages above.")
    } else {
      cli::cli_alert_success(
        "table_idtax is now a materialized view querying rainbio via postgres_fdw."
      )
      cli::cli_alert_info(
        "refresh_table_idtax() now fetches real-time data from rainbio.
         Any user with EXECUTE on that function can do a full refresh without needing con_taxa."
      )
      cli::cli_alert_info(
        "Verify with: setup_fdw_table_idtax(con, diagnose_only = TRUE)"
      )
    }
  }

  invisible(results)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' Diagnose current FDW / table_idtax state
#' @keywords internal
.fdw_diagnose <- function(con, server_name = "rainbio_server") {

  q <- function(sql) tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)

  # postgres_fdw available in pg_available_extensions?
  avail <- q("SELECT COUNT(*) AS n FROM pg_available_extensions WHERE name = 'postgres_fdw';")
  extension_available <- isTRUE(!is.null(avail) && avail$n[1] > 0)

  # postgres_fdw installed?
  inst <- q("SELECT COUNT(*) AS n FROM pg_extension WHERE extname = 'postgres_fdw';")
  extension_installed <- isTRUE(!is.null(inst) && inst$n[1] > 0)

  # Foreign server exists?
  srv <- q(sprintf(
    "SELECT COUNT(*) AS n FROM pg_foreign_server WHERE srvname = '%s';", server_name
  ))
  server_exists <- isTRUE(!is.null(srv) && srv$n[1] > 0)

  # User mapping for current user?
  mapping <- q(sprintf(
    "SELECT COUNT(*) AS n FROM pg_user_mappings WHERE srvname = '%s';", server_name
  ))
  mapping_exists <- isTRUE(!is.null(mapping) && mapping$n[1] > 0)

  # Foreign table exists?
  ft <- q(
    "SELECT COUNT(*) AS n FROM information_schema.foreign_tables
     WHERE foreign_table_name = 'table_taxa_foreign';"
  )
  foreign_table_exists <- isTRUE(!is.null(ft) && ft$n[1] > 0)

  # table_idtax: regular table, matview (staging), or matview (FDW)?
  is_matview <- q(
    "SELECT COUNT(*) AS n FROM pg_matviews WHERE matviewname = 'table_idtax';"
  )
  is_matview <- isTRUE(!is.null(is_matview) && is_matview$n[1] > 0)

  matview_def <- if (is_matview) {
    d <- q("SELECT definition FROM pg_matviews WHERE matviewname = 'table_idtax';")
    if (!is.null(d) && nrow(d) > 0) d$definition[1] else NA_character_
  } else {
    NA_character_
  }

  matview_uses_fdw <- isTRUE(
    !is.na(matview_def) &&
    grepl("table_taxa_foreign", matview_def, fixed = TRUE)
  )

  # Staging table still present?
  staging <- q(
    "SELECT COUNT(*) AS n FROM information_schema.tables
     WHERE table_name = 'table_idtax_temp' AND table_type = 'BASE TABLE';"
  )
  staging_table_exists <- isTRUE(!is.null(staging) && staging$n[1] > 0)

  # refresh_table_idtax() function present?
  fn <- q(
    "SELECT COUNT(*) AS n FROM pg_proc WHERE proname = 'refresh_table_idtax';"
  )
  refresh_fn_exists <- isTRUE(!is.null(fn) && fn$n[1] > 0)

  # table_idtax record count (quick check)
  cnt <- tryCatch(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM table_idtax;")$n[1],
    error = function(e) NA_integer_
  )

  list(
    extension_available  = extension_available,
    extension_installed  = extension_installed,
    server_exists        = server_exists,
    mapping_exists       = mapping_exists,
    foreign_table_exists = foreign_table_exists,
    is_matview           = is_matview,
    matview_def          = matview_def,
    matview_uses_fdw     = matview_uses_fdw,
    staging_table_exists = staging_table_exists,
    refresh_fn_exists    = refresh_fn_exists,
    table_idtax_n        = cnt
  )
}


#' Pretty-print FDW diagnostic results
#' @keywords internal
.fdw_print_diag <- function(diag) {

  tick <- function(x) if (isTRUE(x)) cli::col_green("\u2714") else cli::col_red("\u2718")

  cli::cli_h2("postgres_fdw extension")
  cli::cli_bullets(c(
    " " = "{tick(diag$extension_available)} Available in pg_available_extensions",
    " " = "{tick(diag$extension_installed)} Currently installed"
  ))

  cli::cli_h2("Foreign server & mapping")
  cli::cli_bullets(c(
    " " = "{tick(diag$server_exists)}  Foreign server exists",
    " " = "{tick(diag$mapping_exists)} User mapping exists"
  ))

  cli::cli_h2("Foreign table")
  cli::cli_bullets(c(
    " " = "{tick(diag$foreign_table_exists)} table_taxa_foreign present"
  ))

  cli::cli_h2("table_idtax status")
  cli::cli_bullets(c(
    " " = "{tick(diag$is_matview)}       Is a materialized view",
    " " = "{tick(diag$matview_uses_fdw)} Matview queries table_taxa_foreign (FDW)",
    " " = "{tick(!diag$staging_table_exists)} Staging table table_idtax_temp removed",
    " " = "{tick(diag$refresh_fn_exists)} refresh_table_idtax() function present"
  ))

  if (!is.na(diag$table_idtax_n)) {
    cli::cli_alert_info("table_idtax currently holds {diag$table_idtax_n} rows.")
  }

  if (isTRUE(diag$is_matview) && !isTRUE(diag$matview_uses_fdw)) {
    cli::cli_alert_warning(
      "table_idtax is a matview but still reads from table_idtax_temp (staging table).
       Run setup_fdw_table_idtax() to upgrade it to read from rainbio via FDW."
    )
    if (!is.na(diag$matview_def)) {
      cli::cli_alert_info("Current matview definition: {diag$matview_def}")
    }
  }

  if (isTRUE(diag$matview_uses_fdw)) {
    cli::cli_alert_success("FDW setup is complete and correct.")
  }

  invisible(diag)
}
