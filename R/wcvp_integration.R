# =============================================================================
# WCVP (World Checklist of Vascular Plants) Integration
#
# Functions for importing WCVP data into the taxa database and linking
# internal taxonomy (idtax_n) to WCVP plant_name_id.
#
# Main functions:
# - setup_wcvp_schema(): Create WCVP tables in taxa DB
# - import_wcvp_names(): Import rWCVPdata into the database
# - match_taxa_to_wcvp(): Match internal taxa to WCVP names
# - save_wcvp_links(): Write reviewed matches to link table
# - get_wcvp_names(): Lookup WCVP names for given idtax_n
# - get_wcvp_status(): Check WCVP import status
# - check_wcvp_update(): Check if newer WCVP version is available
#
# Dependencies: DBI, dplyr, cli, glue
# Optional: rWCVP, rWCVPdata (in Suggests)
# =============================================================================


# ---- Schema Setup -----------------------------------------------------------

#' Setup WCVP Database Schema
#'
#' Creates the WCVP-related tables in the taxa database:
#' \itemize{
#'   \item \code{wcvp_names}: Full WCVP dataset
#'   \item \code{wcvp_idtax_link}: Bridge between internal \code{idtax_n} and WCVP \code{plant_name_id}
#'   \item \code{wcvp_import_metadata}: Version tracking
#' }
#'
#' @param con_taxa Connection to the taxa database. If NULL, calls \code{call.mydb.taxa()}.
#' @param dry_run Logical. If TRUE, prints SQL without executing. Default FALSE.
#'
#' @return Invisible list with success status and steps completed.
#'
#' @examples
#' \dontrun{
#' con_taxa <- call.mydb.taxa()
#' # Preview SQL
#' setup_wcvp_schema(con_taxa, dry_run = TRUE)
#' # Execute
#' setup_wcvp_schema(con_taxa)
#' }
#'
#' @export
setup_wcvp_schema <- function(con_taxa = NULL, dry_run = FALSE) {

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }

  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  results <- list(
    success = FALSE,
    steps_completed = character(),
    errors = character()
  )

  cli::cli_h1("Setting Up WCVP Schema")

  if (dry_run) {
    cli::cli_alert_warning("DRY RUN MODE - Commands will be printed but not executed")
  }

  exec_sql <- function(sql, description, critical = TRUE) {
    cli::cli_alert_info(description)
    if (dry_run) {
      cli::cli_alert_info("SQL: {sql}")
      return(TRUE)
    }
    tryCatch({
      DBI::dbExecute(actual_con, sql)
      results$steps_completed <<- c(results$steps_completed, description)
      cli::cli_alert_success("{description}")
      return(TRUE)
    }, error = function(e) {
      msg <- paste0(description, ": ", e$message)
      if (critical) {
        results$errors <<- c(results$errors, msg)
        cli::cli_alert_danger("{msg}")
        stop(msg)
      } else {
        cli::cli_alert_warning("{msg}")
        return(FALSE)
      }
    })
  }

  # -- Table: wcvp_names
  cli::cli_h2("Creating wcvp_names table")
  exec_sql(
    "CREATE TABLE IF NOT EXISTS wcvp_names (
       plant_name_id          INTEGER PRIMARY KEY,
       ipni_id                VARCHAR(50),
       accepted_plant_name_id INTEGER,
       parent_plant_name_id   INTEGER,
       family                 VARCHAR(100),
       genus                  VARCHAR(100),
       species                VARCHAR(150),
       infraspecific_rank     VARCHAR(20),
       infraspecies           VARCHAR(150),
       taxon_name             VARCHAR(300),
       taxon_status           VARCHAR(50),
       taxon_authors          TEXT,
       taxon_rank             VARCHAR(30),
       geographic_area        TEXT,
       lifeform_description   TEXT,
       first_published        VARCHAR(50),
       wcvp_version           VARCHAR(50) NOT NULL
     );",
    "Create wcvp_names table"
  )

  exec_sql(
    "CREATE INDEX IF NOT EXISTS idx_wcvp_accepted ON wcvp_names(accepted_plant_name_id);",
    "Create index on accepted_plant_name_id", critical = FALSE
  )
  exec_sql(
    "CREATE INDEX IF NOT EXISTS idx_wcvp_family ON wcvp_names(family);",
    "Create index on family", critical = FALSE
  )
  exec_sql(
    "CREATE INDEX IF NOT EXISTS idx_wcvp_genus ON wcvp_names(genus);",
    "Create index on genus", critical = FALSE
  )
  exec_sql(
    "CREATE INDEX IF NOT EXISTS idx_wcvp_taxon_name ON wcvp_names(taxon_name);",
    "Create index on taxon_name", critical = FALSE
  )
  exec_sql(
    "CREATE INDEX IF NOT EXISTS idx_wcvp_status ON wcvp_names(taxon_status);",
    "Create index on taxon_status", critical = FALSE
  )

  # -- Table: wcvp_idtax_link
  cli::cli_h2("Creating wcvp_idtax_link table")
  exec_sql(
    "CREATE TABLE IF NOT EXISTS wcvp_idtax_link (
       idtax_n       INTEGER NOT NULL,
       plant_name_id INTEGER NOT NULL REFERENCES wcvp_names(plant_name_id),
       match_type    VARCHAR(20) NOT NULL,
       match_score   NUMERIC(4,3),
       matched_on    TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
       matched_by    VARCHAR(100),
       verified      BOOLEAN DEFAULT FALSE,
       notes         TEXT,
       PRIMARY KEY (idtax_n, plant_name_id)
     );",
    "Create wcvp_idtax_link table"
  )

  # -- Table: wcvp_import_metadata
  cli::cli_h2("Creating wcvp_import_metadata table")
  exec_sql(
    "CREATE TABLE IF NOT EXISTS wcvp_import_metadata (
       id              SERIAL PRIMARY KEY,
       wcvp_version    VARCHAR(50) NOT NULL,
       import_date     TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
       imported_by     VARCHAR(100),
       record_count    INTEGER,
       link_count      INTEGER,
       r_package_version VARCHAR(20),
       is_current      BOOLEAN DEFAULT TRUE
     );",
    "Create wcvp_import_metadata table"
  )

  # Grant SELECT to public
  exec_sql("GRANT SELECT ON wcvp_names TO public;", "Grant SELECT on wcvp_names", critical = FALSE)
  exec_sql("GRANT SELECT ON wcvp_idtax_link TO public;", "Grant SELECT on wcvp_idtax_link", critical = FALSE)
  exec_sql("GRANT SELECT ON wcvp_import_metadata TO public;", "Grant SELECT on wcvp_import_metadata", critical = FALSE)

  results$success <- TRUE
  cli::cli_alert_success("WCVP schema setup complete")
  return(invisible(results))
}


# ---- Import WCVP Data -------------------------------------------------------

#' Import WCVP Names into Database
#'
#' Imports the WCVP dataset from the \code{rWCVPdata} package into the
#' \code{wcvp_names} table. Requires \code{rWCVPdata} and \code{rWCVP} packages.
#'
#' @param con_taxa Connection to the taxa database. If NULL, calls \code{call.mydb.taxa()}.
#' @param batch_size Number of rows to insert per batch. Default 50000.
#' @param force Logical. If TRUE, reimports even if the same version is already present.
#' @param verbose Logical. Show progress messages. Default TRUE.
#'
#' @return Invisible list with import results (version, record_count).
#'
#' @examples
#' \dontrun{
#' con_taxa <- call.mydb.taxa()
#' import_wcvp_names(con_taxa)
#' }
#'
#' @export
import_wcvp_names <- function(con_taxa = NULL,
                              batch_size = 50000,
                              force = FALSE,
                              verbose = TRUE) {

  if (!requireNamespace("rWCVPdata", quietly = TRUE)) {
    stop(
      "Package 'rWCVPdata' is required for WCVP import.\n",
      "Install with: install.packages('rWCVPdata', repos = 'https://matildabrown.github.io/drat')",
      call. = FALSE
    )
  }
  if (!requireNamespace("rWCVP", quietly = TRUE)) {
    stop(
      "Package 'rWCVP' is required for WCVP import.\n",
      "Install with: install.packages('rWCVP')",
      call. = FALSE
    )
  }

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }

  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Get WCVP version
  wcvp_ver <- rWCVPdata::wcvp_version()
  if (verbose) cli::cli_alert_info("rWCVPdata version: {wcvp_ver}")

  # Check if already imported
  if (!force) {
    existing <- tryCatch(
      DBI::dbGetQuery(
        actual_con,
        "SELECT wcvp_version, record_count FROM wcvp_import_metadata WHERE is_current = TRUE;"
      ),
      error = function(e) data.frame()
    )
    if (nrow(existing) > 0 && existing$wcvp_version[1] == wcvp_ver) {
      cli::cli_alert_info(
        "WCVP version {wcvp_ver} already imported ({existing$record_count[1]} records). Use force = TRUE to reimport."
      )
      return(invisible(list(version = wcvp_ver, record_count = existing$record_count[1], skipped = TRUE)))
    }
  }

  # Load WCVP data from R package
  if (verbose) cli::cli_alert_info("Loading WCVP names from rWCVPdata...")
  wcvp_all <- rWCVPdata::wcvp_names

  # Select only the columns we need
  keep_cols <- c(
    "plant_name_id", "ipni_id", "accepted_plant_name_id", "parent_plant_name_id",
    "family", "genus", "species", "infraspecific_rank", "infraspecies",
    "taxon_name", "taxon_status", "taxon_authors", "taxon_rank",
    "geographic_area", "lifeform_description", "first_published"
  )
  available_cols <- intersect(keep_cols, names(wcvp_all))
  wcvp_data <- wcvp_all[, available_cols, drop = FALSE]
  wcvp_data$wcvp_version <- wcvp_ver

  n_total <- nrow(wcvp_data)
  if (verbose) cli::cli_alert_info("Preparing to import {n_total} WCVP records")

  # Transaction: mark old version as not current, truncate, insert
  DBI::dbBegin(actual_con)
  tryCatch({
    # Mark old imports as not current
    DBI::dbExecute(actual_con, "UPDATE wcvp_import_metadata SET is_current = FALSE WHERE is_current = TRUE;")

    # Truncate existing data
    DBI::dbExecute(actual_con, "TRUNCATE TABLE wcvp_idtax_link;")
    DBI::dbExecute(actual_con, "TRUNCATE TABLE wcvp_names CASCADE;")

    # Batch insert
    n_batches <- ceiling(n_total / batch_size)
    for (i in seq_len(n_batches)) {
      start_row <- (i - 1) * batch_size + 1
      end_row <- min(i * batch_size, n_total)
      batch <- wcvp_data[start_row:end_row, , drop = FALSE]

      DBI::dbWriteTable(actual_con, "wcvp_names", batch, append = TRUE, row.names = FALSE)

      if (verbose) {
        cli::cli_alert_info("Batch {i}/{n_batches}: rows {start_row}-{end_row}")
      }
    }

    # Insert metadata
    meta_sql <- glue::glue_sql(
      "INSERT INTO wcvp_import_metadata (wcvp_version, imported_by, record_count, r_package_version, is_current)
       VALUES ({wcvp_ver}, {Sys.info()['user']}, {n_total}, {as.character(utils::packageVersion('rWCVPdata'))}, TRUE);",
      .con = actual_con
    )
    DBI::dbExecute(actual_con, meta_sql)

    DBI::dbCommit(actual_con)
    if (verbose) cli::cli_alert_success("Successfully imported {n_total} WCVP records (version {wcvp_ver})")

  }, error = function(e) {
    DBI::dbRollback(actual_con)
    stop("WCVP import failed: ", e$message, call. = FALSE)
  })

  return(invisible(list(version = wcvp_ver, record_count = n_total, skipped = FALSE)))
}


# ---- Matching Internal Taxa to WCVP -----------------------------------------

#' Exact name match with fuzzy author disambiguation
#'
#' Internal helper. Runs \code{._wcvp_match_exact_db} without author filtering,
#' then adds fuzzy author similarity (Jaro-Winkler via \pkg{stringdist}) and:
#' \itemize{
#'   \item When a name has multiple WCVP hits, selects the hit with the highest
#'     author similarity.
#'   \item Nullifies the match when \code{author_threshold} is set AND author
#'     info is present on both sides AND the best similarity is below the threshold.
#'   \item Leaves matches intact when either side has no author string (NA).
#' }
#'
#' @param names_df data.frame with columns \code{name_col}, \code{author_col},
#'   and \code{id_col}.
#' @param wcvp_names data.frame of WCVP names (from database or \code{rWCVPdata}).
#' @param name_col Character. Column in \code{names_df} holding taxon names.
#' @param author_col Character. Column in \code{names_df} holding author strings.
#' @param id_col Character. Unique row identifier column.
#' @param author_threshold Numeric (0–1). Minimum author similarity to keep a
#'   match when author info is present on both sides. Default 0.6.
#'
#' @return Same column structure as \code{._wcvp_match_exact_db}, plus an
#'   \code{author_similarity} column.
#'
#' @keywords internal
.wcvp_match_fuzzy_author <- function(names_df, wcvp_names, name_col,
                                     author_col, id_col,
                                     author_threshold = 0.6) {

  # Step 1: exact name match, no author filter — keeps all homonym hits
  result <- ._wcvp_match_exact_db(
    names_df   = names_df,
    wcvp_names = wcvp_names,
    name_col   = name_col,
    author_col = NULL,
    id_col     = id_col
  )

  # Step 2: compute fuzzy author similarity (Jaro-Winkler)
  result <- result %>%
    dplyr::mutate(
      author_similarity = dplyr::if_else(
        !is.na(wcvp_authors) & !is.na(.data[[author_col]]) &
          .data[[author_col]] != "",
        stringdist::stringsim(.data[[author_col]], wcvp_authors, method = "jw"),
        NA_real_
      )
    )

  # Step 3: for each input row (id_col), keep the hit with the best author similarity.
  # Rows with NA author_similarity rank below any numeric value so they are kept
  # only when no better alternative exists.
  result <- result %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::arrange(dplyr::desc(author_similarity), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(multiple_matches = FALSE)   # resolved to single best hit

  # Step 4: nullify matches where author similarity is below threshold
  # (only when we have author info on BOTH sides — if either is NA, keep the match)
  result <- result %>%
    dplyr::mutate(
      dplyr::across(
        c(wcvp_id, wcvp_name, wcvp_authors, wcvp_rank, wcvp_status,
          wcvp_homotypic, wcvp_ipni_id, wcvp_accepted_id,
          match_type, match_similarity, match_edit_distance),
        ~ dplyr::if_else(
          !is.na(author_similarity) & author_similarity < author_threshold,
          NA,
          .x
        )
      )
    )

  result
}


#' Fast Fuzzy Matching Against WCVP Using Genus Blocking
#'
#' Drop-in replacement for \code{rWCVP::wcvp_match_fuzzy} that reduces the
#' comparison space from O(n x 1.4M) to O(n x genus_size) by pre-filtering
#' WCVP candidates to the same genus as each input name.  For a typical input
#' of 80 000 names this is 1 000-10 000x faster (minutes instead of days).
#'
#' Algorithm:
#' \enumerate{
#'   \item Extract genus (first word) from each input name.
#'   \item Index WCVP by genus via \code{data.table} keyed lookup.
#'   \item For each unique genus, retrieve its WCVP candidates (usually < 500
#'     records).  If the genus is absent from WCVP, fall back to the closest
#'     WCVP genus by Jaro-Winkler similarity (\code{genus_threshold}).
#'   \item Apply a name-length pre-filter: only candidates whose name length
#'     is within \code{floor((1 - fuzzy_threshold) * max_len) + 1} characters
#'     of the input name length are retained (valid because Levenshtein distance
#'     is bounded by the length difference).
#'   \item Compute \code{stringdist::stringdistmatrix()} within the filtered
#'     candidate set and select the closest hit per input name.
#' }
#'
#' @param names_df data.frame with at least a column named \code{name_col}.
#' @param wcvp_names data.frame of WCVP names (from database or \code{rWCVPdata}).
#' @param name_col Character. Column in \code{names_df} holding taxon names.
#' @param fuzzy_threshold Numeric (0-1). Minimum normalised similarity to report
#'   a match.  Matches below this value are returned as NA rows.  Default 0.9.
#' @param genus_threshold Numeric (0-1). Jaro-Winkler threshold used when a
#'   genus is not found verbatim in WCVP (genus typo fallback).  Default 0.9.
#' @param n_cores Integer. Number of parallel workers.  On Windows a PSOCK
#'   cluster is used; on Unix forking via \code{parallel::mclapply}.  Default 1
#'   (sequential).
#' @param verbose Logical. Show a CLI progress bar over genus blocks. Default TRUE.
#'
#' @return A data.frame with one row per input name and columns matching the
#'   output of \code{rWCVP::wcvp_match_fuzzy}:
#'   \code{name}, \code{wcvp_name}, \code{match_type}, \code{multiple_matches},
#'   \code{match_similarity}, \code{match_edit_distance}, \code{wcvp_id},
#'   \code{wcvp_authors}, \code{wcvp_rank}, \code{wcvp_status},
#'   \code{wcvp_homotypic}, \code{wcvp_ipni_id}, \code{wcvp_accepted_id}.
#'   Unmatched rows have NA in all WCVP columns.
#'
#' @keywords internal
.wcvp_match_fuzzy_fast <- function(names_df, wcvp_names, name_col,
                                    fuzzy_threshold = 0.9,
                                    genus_threshold  = 0.9,
                                    n_cores          = 1L,
                                    verbose          = TRUE) {

  input_names <- names_df[[name_col]]

  # Extract genus (first whitespace-delimited word)
  input_genera <- sub("^(\\S+).*", "\\1", trimws(input_names))

  # Index WCVP by genus for fast keyed lookup
  wcvp_dt <- data.table::as.data.table(wcvp_names)
  data.table::setkey(wcvp_dt, genus)
  wcvp_genera_set <- unique(wcvp_dt[["genus"]])

  unique_genera <- unique(input_genera)
  n_genera      <- length(unique_genera)

  # ---- Per-genus matching closure -----------------------------------------
  match_genus_block <- function(g) {

    # Handle NA genus (from taxon names that are NA or have no extractable genus)
    if (is.na(g)) {
      idx <- which(is.na(input_genera))
    } else {
      idx <- which(input_genera == g)
    }
    these_names <- input_names[idx]

    # Guard: empty block or all-NA names — return NULL (bind_rows ignores NULLs)
    if (length(these_names) == 0L || all(is.na(these_names))) return(NULL)

    # WCVP candidates for this genus (keyed lookup)
    cands <- as.data.frame(wcvp_dt[.(g)])

    if (nrow(cands) == 0L) {
      # Genus not found verbatim: fuzzy genus fallback via Jaro-Winkler
      genus_sims   <- stringdist::stringsim(g, wcvp_genera_set, method = "jw")
      close_genera <- wcvp_genera_set[!is.na(genus_sims) & genus_sims >= genus_threshold]
      if (length(close_genera) > 0L) {
        cands <- as.data.frame(wcvp_dt[.(close_genera)])
      }
    }

    # Build NA-filled output skeleton (one row per input name)
    out <- data.frame(
      name                = these_names,
      wcvp_name           = NA_character_,
      match_type          = NA_character_,
      multiple_matches    = NA,
      match_similarity    = NA_real_,
      match_edit_distance = NA_real_,
      wcvp_id             = NA_integer_,
      wcvp_authors        = NA_character_,
      wcvp_rank           = NA_character_,
      wcvp_status         = NA_character_,
      wcvp_homotypic      = NA,
      wcvp_ipni_id        = NA_character_,
      wcvp_accepted_id    = NA_integer_,
      stringsAsFactors    = FALSE
    )

    if (nrow(cands) == 0L) return(out)

    # Name-length pre-filter:
    # Levenshtein distance >= |len_a - len_b|, so candidates whose length
    # differs from every input name by more than max_allowed_dist can never
    # reach the similarity threshold and are safely discarded.
    name_lengths     <- nchar(these_names)
    max_allowed_dist <- floor((1 - fuzzy_threshold) * max(name_lengths)) + 1L
    cand_nchar       <- nchar(cands$taxon_name)
    keep_len         <- cand_nchar >= (min(name_lengths) - max_allowed_dist) &
                        cand_nchar <= (max(name_lengths) + max_allowed_dist)
    cands_f <- cands[keep_len, ]
    if (nrow(cands_f) == 0L) cands_f <- cands   # safety fallback

    cand_names  <- cands_f$taxon_name
    cand_nchar2 <- nchar(cand_names)

    # Batch edit-distance matrix: rows = input names, cols = candidates
    dist_mat <- stringdist::stringdistmatrix(
      these_names, cand_names,
      method = "lv", useNames = FALSE
    )
    # Force correct dims (stringdistmatrix drops dims for length-1 inputs)
    dim(dist_mat) <- c(length(these_names), length(cand_names))

    # Best (minimum-distance) candidate per input name
    best_j    <- apply(dist_mat, 1L, which.min)
    best_dist <- dist_mat[cbind(seq_along(these_names), best_j)]
    multi     <- apply(dist_mat, 1L, function(r) sum(r == min(r)) > 1L)

    # Normalised similarity: 1 - dist / max(len_a, len_b)
    max_len    <- pmax(name_lengths, cand_nchar2[best_j])
    max_len[max_len == 0L] <- 1L
    similarity <- 1 - best_dist / max_len

    keep <- !is.na(similarity) & similarity >= fuzzy_threshold
    if (any(keep)) {
      best_cands <- cands_f[best_j[keep], ]
      out$wcvp_name[keep]           <- best_cands$taxon_name
      out$match_type[keep]          <- "fuzzy"
      out$multiple_matches[keep]    <- multi[keep]
      out$match_similarity[keep]    <- round(similarity[keep], 4L)
      out$match_edit_distance[keep] <- best_dist[keep]
      out$wcvp_id[keep]             <- as.integer(best_cands$plant_name_id)
      out$wcvp_authors[keep]        <- best_cands$taxon_authors
      out$wcvp_rank[keep]           <- best_cands$taxon_rank
      out$wcvp_status[keep]         <- best_cands$taxon_status
      out$wcvp_ipni_id[keep]        <- best_cands$ipni_id
      out$wcvp_accepted_id[keep]    <- as.integer(best_cands$accepted_plant_name_id)
    }

    out
  }
  # -------------------------------------------------------------------------

  if (n_cores > 1L) {
    if (.Platform$OS.type == "unix") {
      if (verbose) cli::cli_alert_info("Fuzzy matching: {n_genera} genus blocks, {n_cores} cores (fork)...")
      results <- parallel::mclapply(unique_genera, match_genus_block, mc.cores = n_cores)
    } else {
      if (verbose) cli::cli_alert_info("Fuzzy matching: {n_genera} genus blocks, {n_cores} cores (PSOCK)...")
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      # Export the WCVP data table and matching variables to workers
      parallel::clusterEvalQ(cl, {
        library(data.table)    # nolint
        library(stringdist)    # nolint
      })

      parallel::clusterExport(
        cl,
        varlist = c("input_names", "input_genera", "fuzzy_threshold",
                     "genus_threshold", "wcvp_dt", "wcvp_genera_set"),
        envir   = environment()
      )

      # Replace match_genus_block's closure environment with a lightweight env that
      # does NOT contain wcvp_dt.  Without this, R serialises the entire local
      # environment (including wcvp_dt) when sending the function to workers.
      # Workers find wcvp_dt and wcvp_genera_set in their .GlobalEnv (set above)
      # via the parent = globalenv() chain.
      fn_env                  <- new.env(parent = globalenv())
      fn_env$input_names      <- input_names
      fn_env$input_genera     <- input_genera
      fn_env$fuzzy_threshold  <- fuzzy_threshold
      fn_env$genus_threshold  <- genus_threshold
      environment(match_genus_block) <- fn_env

      results <- parallel::parLapply(cl, unique_genera, match_genus_block)
    }
  } else {
    if (verbose) {
      cli::cli_progress_bar(
        "Fuzzy matching genus blocks",
        total  = n_genera,
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} genera | ETA {cli::pb_eta}"
      )
    }
    results <- vector("list", n_genera)
    for (i in seq_len(n_genera)) {
      results[[i]] <- match_genus_block(unique_genera[[i]])
      if (verbose) cli::cli_progress_update()
    }
    if (verbose) cli::cli_progress_done()
  }

  dplyr::bind_rows(results)
}


#' Exact Name Match (Database-Compatible Version)
#'
#' Database-compatible replacement for \code{rWCVP::wcvp_match_exact}.
#' Matches input names (from \code{names_df}) to WCVP taxon names using
#' exact string comparison, with optional author matching.
#'
#' @param names_df data.frame with columns \code{name_col}, optionally \code{author_col},
#'   and \code{id_col}.
#' @param wcvp_names data.frame of WCVP names with columns: \code{taxon_name},
#'   \code{plant_name_id}, \code{taxon_authors}, \code{taxon_rank}, \code{taxon_status},
#'   \code{ipni_id}, \code{accepted_plant_name_id}.
#' @param name_col Character. Column in \code{names_df} holding taxon names.
#' @param author_col Character or NULL. Column in \code{names_df} holding authors.
#'   If NULL, author matching is skipped. Default NULL.
#' @param id_col Character. Unique identifier column in \code{names_df}
#'   (used to disambiguate rows with identical names).
#'
#' @return data.frame with one row per input name and columns:
#'   \code{name}, \code{wcvp_id}, \code{wcvp_name}, \code{wcvp_authors},
#'   \code{wcvp_rank}, \code{wcvp_status}, \code{wcvp_homotypic},
#'   \code{wcvp_ipni_id}, \code{wcvp_accepted_id}, \code{match_type},
#'   \code{match_similarity}, \code{match_edit_distance}, \code{id_col}.
#'   Unmatched rows have NA in WCVP columns.
#'
#' @keywords internal
._wcvp_match_exact_db <- function(names_df, wcvp_names, name_col,
                                   author_col = NULL, id_col) {

  input_names <- names_df[[name_col]]
  input_ids <- names_df[[id_col]]

  # Ensure wcvp_names is a data.frame with required columns
  if (!is.data.frame(wcvp_names)) {
    wcvp_names <- as.data.frame(wcvp_names)
  }

  required_cols <- c("taxon_name", "plant_name_id", "taxon_authors",
                     "taxon_rank", "taxon_status", "ipni_id", "accepted_plant_name_id")
  missing_cols <- setdiff(required_cols, names(wcvp_names))
  if (length(missing_cols) > 0) {
    stop("wcvp_names missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Build input data frame for joining
  input_df <- data.frame(
    name     = input_names,
    .row_id  = input_ids,
    stringsAsFactors = FALSE
  )
  if (!is.null(author_col)) {
    input_df$.author <- names_df[[author_col]]
  }

  # Join on exact taxon_name match — returns multiple rows per input when
  # there are homonyms in WCVP (needed by ._wcvp_match_fuzzy_author)
  matched <- merge(
    input_df,
    wcvp_names[, required_cols, drop = FALSE],
    by.x = "name", by.y = "taxon_name",
    all.x = TRUE
  )

  # Apply exact author filtering when requested
  if (!is.null(author_col)) {
    has_author <- !is.na(matched$.author) & matched$.author != ""
    has_wcvp_author <- !is.na(matched$taxon_authors)
    both_have <- has_author & has_wcvp_author

    # Nullify WCVP columns where author doesn't match exactly
    mismatch <- both_have & matched$.author != matched$taxon_authors
    wcvp_cols <- c("plant_name_id", "taxon_authors", "taxon_rank",
                   "taxon_status", "ipni_id", "accepted_plant_name_id")
    matched[mismatch, wcvp_cols] <- NA
    matched$.author <- NULL
  }

  # Build output in the expected column format
  has_match <- !is.na(matched$plant_name_id)
  n_matches_per_name <- stats::ave(
    as.integer(has_match), matched$.row_id,
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  out <- data.frame(
    name                = matched$name,
    wcvp_id             = ifelse(has_match, as.integer(matched$plant_name_id), NA_integer_),
    wcvp_name           = ifelse(has_match, matched$name, NA_character_),
    wcvp_authors        = ifelse(has_match, as.character(matched$taxon_authors), NA_character_),
    wcvp_rank           = ifelse(has_match, as.character(matched$taxon_rank), NA_character_),
    wcvp_status         = ifelse(has_match, as.character(matched$taxon_status), NA_character_),
    wcvp_homotypic      = NA,
    wcvp_ipni_id        = ifelse(has_match, as.character(matched$ipni_id), NA_character_),
    wcvp_accepted_id    = ifelse(has_match, as.integer(matched$accepted_plant_name_id), NA_integer_),
    match_type          = ifelse(has_match, "exact", NA_character_),
    match_similarity    = ifelse(has_match, 1.0, NA_real_),
    match_edit_distance = ifelse(has_match, 0L, NA_integer_),
    multiple_matches    = n_matches_per_name > 1,
    stringsAsFactors    = FALSE
  )
  out[[id_col]] <- matched$.row_id

  # For unmatched names, collapse to single row (no duplicates from merge)
  # Keep all rows for matched names (homonyms needed by fuzzy_author)
  unmatched <- out[!has_match, ]
  unmatched <- unmatched[!duplicated(unmatched[[id_col]]), ]
  out <- rbind(out[has_match, ], unmatched)

  out
}


#' Match Internal Taxa to WCVP Names
#'
#' Matches taxa from the internal \code{table_taxa} to WCVP names already
#' uploaded in the database, using exact and optionally fuzzy matching.
#'
#' Returns a tibble for review. Does NOT write to the database automatically.
#' Use \code{save_wcvp_links()} to persist reviewed matches.
#'
#' @param con_taxa Connection to the taxa database. If NULL, calls \code{call.mydb.taxa()}.
#' @param tax_ids Optional integer vector of \code{idtax_n} to match. If NULL, matches all accepted taxa.
#' @param methods Character vector of matching methods to use. Default \code{c("exact", "fuzzy")}.
#' @param fuzzy_threshold Numeric (0-1). Minimum similarity for fuzzy matches. Default 0.9.
#' @param author_match Character. How to use author strings during exact name matching.
#'   \itemize{
#'     \item \code{"none"} (default): ignore authors entirely.
#'     \item \code{"exact"}: authors must match character-for-character.
#'       Reduces false positives
#'       but misses any formatting difference.
#'     \item \code{"fuzzy"}: exact name match first, then Jaro-Winkler author
#'       similarity to select among homonyms and filter below \code{author_threshold}.
#'       More tolerant of abbreviation/spacing differences.
#'   }
#'   Authors are built from \code{author1} (basionym) and \code{author2} (combination)
#'   columns in \code{table_taxa}: \code{"(author1) author2"}.
#'   Not applied to fuzzy name matching (author disambiguation is not meaningful
#'   when the name itself is inexact).
#' @param author_threshold Numeric (0-1). Minimum Jaro-Winkler similarity required
#'   to keep a match when \code{author_match = "fuzzy"} and author info is present
#'   on both sides. Default 0.6.
#' @param n_cores Integer. Number of parallel workers for fuzzy matching.
#'   Uses forking on Unix and a PSOCK cluster on Windows.  Default 1 (sequential).
#'   Set to \code{parallel::detectCores() - 1} to use all available cores.
#' @param verbose Logical. Show progress. Default TRUE.
#'
#' @return A tibble with columns: \code{idtax_n}, \code{taxon_name_internal},
#'   \code{plant_name_id}, \code{wcvp_taxon_name}, \code{match_type}, \code{match_score}.
#'
#' @examples
#' \dontrun{
#' con_taxa <- call.mydb.taxa()
#' matches <- match_taxa_to_wcvp(con_taxa)
#' # Review matches, then save
#' save_wcvp_links(matches, con_taxa)
#' }
#'
#' @export
match_taxa_to_wcvp <- function(con_taxa = NULL,
                               tax_ids = NULL,
                               methods = c("exact", "fuzzy"),
                               fuzzy_threshold = 0.9,
                               author_match = c("none", "exact", "fuzzy"),
                               author_threshold = 0.6,
                               n_cores = 1L,
                               verbose = TRUE) {

  author_match <- match.arg(author_match)

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }

  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Fetch internal taxa (accepted names only: idtax_good_n IS NULL)
  if (verbose) cli::cli_alert_info("Fetching internal taxa...")

  taxa_query <- dplyr::tbl(actual_con, "table_taxa") %>% 
    filter(morpho_species == "false",
           !grepl("Musci-", tax_fam),
           !grepl("Lichenes", tax_fam),
           tax_fam != "Fungi")

  if (!is.null(tax_ids)) {
    taxa_query <- taxa_query %>%
      dplyr::filter(idtax_n %in% !!tax_ids)
  }

  use_authors <- author_match != "none"
  sel_cols <- c("idtax_n", "tax_fam", "tax_gen", "tax_esp",
                "tax_rank01", "tax_nam01", "tax_rank02", "tax_nam02")
  if (use_authors) sel_cols <- c(sel_cols, "author1", "author2", "author3")

  internal_taxa <- taxa_query %>%
    dplyr::select(dplyr::all_of(sel_cols)) %>%
    dplyr::collect()

  if (nrow(internal_taxa) == 0) {
    cli::cli_alert_warning("No taxa to match")
    return(dplyr::tibble(
      idtax_n = integer(),
      taxon_name_internal = character(),
      plant_name_id = integer(),
      wcvp_taxon_name = character(),
      match_type = character(),
      match_score = numeric()
    ))
  }

  # Build taxon name strings from raw columns.
  # Column semantics:
  #   tax_gen, tax_esp              = genus, species epithet
  #   tax_rank01, tax_nam01         = first infraspecific rank/epithet
  #   tax_rank02, tax_nam02         = second infraspecific rank/epithet
  #   author1                       = author of species epithet
  #   author2                       = author of first infraspecific (tax_nam01)
  #   author3                       = author of second infraspecific (tax_nam02)
  # All components are conditional on being non-NA and non-empty.
  internal_taxa <- internal_taxa %>%
    dplyr::mutate(
      taxon_name_internal = dplyr::case_when(
        !is.na(tax_nam02) & nzchar(tax_nam02) & !is.na(tax_rank02) & nzchar(tax_rank02) ~
          paste(tax_gen, tax_esp, tax_rank01, tax_nam01, tax_rank02, tax_nam02),
        !is.na(tax_nam01) & nzchar(tax_nam01) & !is.na(tax_rank01) & nzchar(tax_rank01) ~
          paste(tax_gen, tax_esp, tax_rank01, tax_nam01),
        !is.na(tax_esp) & nzchar(tax_esp) ~ paste(tax_gen, tax_esp),
        TRUE ~ tax_gen
      )
    )

  # Build author string: use the deepest available author for the deepest infraspecific level.
  #   - Two infra levels present: use author3 (or author2 as fallback)
  #   - One infra level present:  use author2 (author of tax_nam01)
  #   - Species only:             use author1
  if (use_authors) {
    internal_taxa <- internal_taxa %>%
      dplyr::mutate(
        taxon_authors_internal = dplyr::case_when(
          !is.na(tax_nam02) & nzchar(tax_nam02) & !is.na(author3) & nzchar(author3) ~ author3,
          !is.na(tax_nam02) & nzchar(tax_nam02) & !is.na(author2) & nzchar(author2) ~ author2,
          !is.na(tax_nam01) & nzchar(tax_nam01) & !is.na(author2) & nzchar(author2) ~ author2,
          !is.na(tax_esp)   & nzchar(tax_esp)   & !is.na(author1) & nzchar(author1) ~ author1,
          TRUE ~ NA_character_
        )
      )
  }

  if (verbose) cli::cli_alert_info("Matching {nrow(internal_taxa)} taxa against WCVP...")

  # Fetch WCVP data from database instead of rWCVPdata package
  if (verbose) cli::cli_alert_info("Fetching WCVP data from database...")
  wcvp_db <- tryCatch({
    dplyr::tbl(actual_con, "wcvp_names") %>%
      dplyr::collect() %>%
      as.data.frame()
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch WCVP from database: {e$message}")
    if (!requireNamespace("rWCVPdata", quietly = TRUE)) {
      stop("WCVP data not found in database and rWCVPdata package not available", call. = FALSE)
    }
    cli::cli_alert_info("Falling back to rWCVPdata package...")
    rWCVPdata::wcvp_names
  })

  if (nrow(wcvp_db) == 0) {
    stop("No WCVP data available. Please run setup_wcvp_schema() and import_wcvp_names() first.", call. = FALSE)
  }

  all_matches <- dplyr::tibble(
    idtax_n = integer(), taxon_name_internal = character(),
    plant_name_id = integer(), wcvp_taxon_name = character(),
    match_type = character(), match_score = numeric()
  )

  # Deduplicate names before matching (performance), then re-expand to all idtax_n.
  # When using authors, deduplicate on name + author to preserve author variants.
  if (use_authors) {
    unique_names <- 
      internal_taxa %>%
      dplyr::distinct(taxon_name_internal, taxon_authors_internal) %>%
      dplyr::mutate(.match_id = dplyr::row_number())
  } else {
    unique_names <- internal_taxa %>%
      dplyr::distinct(taxon_name_internal) %>%
      dplyr::mutate(.match_id = dplyr::row_number())
  }

  # Exact matching
  if ("exact" %in% methods) {
    if (verbose) {
      author_note <- switch(author_match,
        exact = " (exact author filter)",
        fuzzy = glue::glue(" (fuzzy author, threshold {author_threshold})"),
        ""
      )
      cli::cli_alert_info("Running exact name matching on {nrow(unique_names)} unique names{author_note}...")
    }

    # Use column name "name" (not "taxon_name") to avoid collision with WCVP's own column.
    names_df <- data.frame(
      .match_id = unique_names$.match_id,
      name      = unique_names$taxon_name_internal,
      stringsAsFactors = FALSE
    )
    if (use_authors) {
      names_df$author <- unique_names$taxon_authors_internal
    }

    exact_result <- tryCatch({
      if (author_match == "fuzzy") {
        .wcvp_match_fuzzy_author(
          names_df         = names_df,
          wcvp_names       = wcvp_db,
          name_col         = "name",
          author_col       = "author",
          id_col           = ".match_id",
          author_threshold = author_threshold
        )
      } else {
        ._wcvp_match_exact_db(
          names_df   = names_df,
          wcvp_names = wcvp_db,
          name_col   = "name",
          author_col = if (author_match == "exact") "author" else NULL,
          id_col     = ".match_id"
        )
      }
    }, error = function(e) {
      cli::cli_alert_warning("Exact matching failed: {e$message}")
      NULL
    })

    if (!is.null(exact_result) && nrow(exact_result) > 0) {
      matched_unique <- exact_result %>%
        dplyr::filter(!is.na(wcvp_id)) %>%
        dplyr::transmute(
          taxon_name_internal = name,
          plant_name_id       = as.integer(wcvp_id),
          wcvp_taxon_name     = wcvp_name,
          match_type          = "exact",
          match_score         = as.numeric(match_similarity)
        ) %>%
        # Remove duplicate rows caused by author deduplication producing multiple
        # .match_id entries for the same name that all resolved to the same WCVP record.
        dplyr::distinct(taxon_name_internal, plant_name_id, .keep_all = TRUE)

      # Re-expand: join back to all idtax_n sharing the same name.
      # many-to-many is expected: multiple idtax_n can share a name (synonyms stored
      # separately), and genuine homonyms produce multiple plant_name_id per name.
      matched <- internal_taxa %>%
        dplyr::select(idtax_n, taxon_name_internal) %>%
        dplyr::inner_join(matched_unique, by = "taxon_name_internal",
                          relationship = "many-to-many")

      all_matches <- dplyr::bind_rows(all_matches, matched)

      if (verbose) cli::cli_alert_success("Exact: {nrow(matched_unique)} unique names matched ({nrow(matched)} taxa total)")
    }
  }

  # Fuzzy matching (for unmatched taxa)
  if ("fuzzy" %in% methods) {
    matched_ids <- unique(all_matches$idtax_n)
    unmatched_taxa <- internal_taxa %>%
      dplyr::filter(!idtax_n %in% matched_ids)

    unmatched_unique <- unmatched_taxa %>%
      dplyr::distinct(taxon_name_internal) %>%
      dplyr::mutate(.match_id = dplyr::row_number())

    if (nrow(unmatched_unique) > 0) {
      if (verbose) cli::cli_alert_info("Running fuzzy matching on {nrow(unmatched_unique)} unique unmatched names...")

      unmatched_df <- data.frame(
        name = unmatched_unique$taxon_name_internal,
        stringsAsFactors = FALSE
      )

      fuzzy_result <- tryCatch(
        .wcvp_match_fuzzy_fast(
          names_df        = unmatched_df,
          wcvp_names      = wcvp_db,
          name_col        = "name",
          fuzzy_threshold = fuzzy_threshold,
          n_cores         = n_cores,
          verbose         = verbose
        ),
        error = function(e) {
          cli::cli_alert_warning("Fuzzy matching failed: {e$message}")
          NULL
        }
      )

      if (!is.null(fuzzy_result) && nrow(fuzzy_result) > 0) {
        fuzzy_unique <- fuzzy_result %>%
          dplyr::filter(!is.na(wcvp_id)) %>%
          dplyr::transmute(
            taxon_name_internal = name,
            plant_name_id       = as.integer(wcvp_id),
            wcvp_taxon_name     = wcvp_name,
            match_type          = "fuzzy",
            match_score         = as.numeric(match_similarity)
          ) %>%
          dplyr::filter(match_score >= fuzzy_threshold) %>%
          dplyr::distinct(taxon_name_internal, plant_name_id, .keep_all = TRUE)

        # Re-expand to all idtax_n sharing the same name
        fuzzy_matched <- unmatched_taxa %>%
          dplyr::select(idtax_n, taxon_name_internal) %>%
          dplyr::inner_join(fuzzy_unique, by = "taxon_name_internal",
                            relationship = "many-to-many")

        all_matches <- dplyr::bind_rows(all_matches, fuzzy_matched)

        if (verbose) cli::cli_alert_success("Fuzzy: {nrow(fuzzy_unique)} unique names matched ({nrow(fuzzy_matched)} taxa total, threshold >= {fuzzy_threshold})")
      }
    }
  }

  # Summary
  n_matched <- length(unique(all_matches$idtax_n))
  n_unmatched <- nrow(internal_taxa) - n_matched
  if (verbose) {
    cli::cli_alert_info("Summary: {n_matched} matched, {n_unmatched} unmatched out of {nrow(internal_taxa)} taxa")
  }

  all_matches %>%
    dplyr::select(idtax_n, taxon_name_internal, plant_name_id, wcvp_taxon_name, match_type, match_score)
}


#' Save WCVP Links to Database
#'
#' Writes reviewed matches from \code{match_taxa_to_wcvp()} to the
#' \code{wcvp_idtax_link} table.
#'
#' @param matches Tibble of matches from \code{match_taxa_to_wcvp()}.
#' @param con_taxa Connection to the taxa database.
#' @param replace Logical. If TRUE, deletes existing links for affected \code{idtax_n}
#'   before inserting. Default TRUE.
#' @param verbose Logical. Show progress. Default TRUE.
#'
#' @return Invisible integer: number of links saved.
#'
#' @examples
#' \dontrun{
#' matches <- match_taxa_to_wcvp(con_taxa)
#' save_wcvp_links(matches, con_taxa)
#' }
#'
#' @export
save_wcvp_links <- function(matches,
                            con_taxa,
                            replace = TRUE,
                            verbose = TRUE) {

  if (nrow(matches) == 0) {
    if (verbose) cli::cli_alert_info("No matches to save")
    return(invisible(0L))
  }

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }

  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  link_data <- matches %>%
    dplyr::transmute(
      idtax_n = as.integer(idtax_n),
      plant_name_id = as.integer(plant_name_id),
      match_type = match_type,
      match_score = match_score,
      matched_by = Sys.info()["user"],
      verified = FALSE
    )

  DBI::dbBegin(actual_con)
  tryCatch({
    if (replace) {
      ids_to_replace <- unique(link_data$idtax_n)
      placeholders <- paste(ids_to_replace, collapse = ", ")
      DBI::dbExecute(
        actual_con,
        paste0("DELETE FROM wcvp_idtax_link WHERE idtax_n IN (", placeholders, ");")
      )
    }

    DBI::dbWriteTable(actual_con, "wcvp_idtax_link", link_data, append = TRUE, row.names = FALSE)

    # Update metadata link count
    link_count <- DBI::dbGetQuery(actual_con, "SELECT COUNT(*) as n FROM wcvp_idtax_link;")
    DBI::dbExecute(
      actual_con,
      paste0("UPDATE wcvp_import_metadata SET link_count = ", link_count$n[1],
             " WHERE is_current = TRUE;")
    )

    DBI::dbCommit(actual_con)
    if (verbose) cli::cli_alert_success("Saved {nrow(link_data)} links to wcvp_idtax_link")

  }, error = function(e) {
    DBI::dbRollback(actual_con)
    stop("Failed to save WCVP links: ", e$message, call. = FALSE)
  })

  return(invisible(nrow(link_data)))
}


# ---- Query WCVP Names -------------------------------------------------------

#' Get WCVP Names for Internal Taxa
#'
#' Looks up WCVP names for given \code{idtax_n} values via the link table.
#' Optionally resolves WCVP synonyms to their accepted names.
#'
#' Taxa not found in the link table get \code{name_source = "internal"}.
#'
#' @param idtax_n Integer vector of internal taxon IDs.
#' @param con_taxa Connection to the taxa database. If NULL, calls \code{call.mydb.taxa()}.
#' @param resolve_synonyms Logical. If TRUE and a linked WCVP name is a synonym,
#'   follow \code{accepted_plant_name_id} to the accepted name. Default TRUE.
#'
#' @return A tibble with columns: \code{idtax_n}, \code{plant_name_id},
#'   \code{wcvp_taxon_name}, \code{wcvp_family}, \code{wcvp_taxon_status},
#'   \code{wcvp_taxon_authors}, \code{name_source}.
#'
#' @examples
#' \dontrun{
#' con_taxa <- call.mydb.taxa()
#' wcvp_info <- get_wcvp_names(c(123, 456, 789), con_taxa)
#' }
#'
#' @export
get_wcvp_names <- function(idtax_n,
                           con_taxa = NULL,
                           resolve_synonyms = TRUE) {

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }

  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Check if WCVP tables exist
  wcvp_exists <- tryCatch({
    DBI::dbGetQuery(actual_con, "SELECT 1 FROM wcvp_idtax_link LIMIT 0;")
    TRUE
  }, error = function(e) FALSE)

  if (!wcvp_exists) {
    # No WCVP data available - return all as internal
    return(dplyr::tibble(
      idtax_n                     = idtax_n,
      wcvp_plant_name_id          = NA_integer_,
      wcvp_accepted_plant_name_id = NA_integer_,
      wcvp_taxon_name             = NA_character_,
      wcvp_family                 = NA_character_,
      wcvp_genus                  = NA_character_,
      wcvp_species                = NA_character_,
      wcvp_taxon_status           = NA_character_,
      wcvp_taxon_authors          = NA_character_,
      name_source                 = "internal"
    ))
  }

  unique_ids <- unique(idtax_n)
  placeholders <- paste(unique_ids, collapse = ", ")

  # Join link table to wcvp_names.
  # wcvp_plant_name_id = original matched WCVP ID (analogue of idtax_n)
  # wcvp_accepted_plant_name_id will be set below (analogue of idtax_good_n):
  #   NA when the matched taxon is already Accepted, otherwise the accepted WCVP ID.
  sql <- paste0(
    "SELECT l.idtax_n,
            w.plant_name_id AS wcvp_plant_name_id,
            w.taxon_name AS wcvp_taxon_name,
            w.family AS wcvp_family, w.genus AS wcvp_genus, w.species AS wcvp_species,
            w.taxon_status AS wcvp_taxon_status,
            w.taxon_authors AS wcvp_taxon_authors,
            w.accepted_plant_name_id AS wcvp_accepted_plant_name_id
     FROM wcvp_idtax_link l
     JOIN wcvp_names w ON l.plant_name_id = w.plant_name_id
     WHERE l.idtax_n IN (", placeholders, ");"
  )

  linked <- DBI::dbGetQuery(actual_con, sql)

  if (nrow(linked) > 0 && resolve_synonyms) {
    # For synonyms, follow wcvp_accepted_plant_name_id to get accepted name info
    synonyms <- linked %>%
      dplyr::filter(wcvp_taxon_status != "Accepted" & !is.na(wcvp_accepted_plant_name_id))

    if (nrow(synonyms) > 0) {
      accepted_ids <- paste(unique(synonyms$wcvp_accepted_plant_name_id), collapse = ", ")
      accepted_sql <- paste0(
        "SELECT plant_name_id, taxon_name AS wcvp_taxon_name,
                family AS wcvp_family, genus AS wcvp_genus, species AS wcvp_species,
                taxon_status AS wcvp_taxon_status, taxon_authors AS wcvp_taxon_authors
         FROM wcvp_names
         WHERE plant_name_id IN (", accepted_ids, ");"
      )
      accepted_names <- DBI::dbGetQuery(actual_con, accepted_sql)

      # Replace name/family/genus/species/authors with those of the accepted taxon.
      # wcvp_plant_name_id keeps the ORIGINAL matched ID (before following the synonym).
      # wcvp_accepted_plant_name_id is NA when the taxon is already Accepted.
      linked <- linked %>%
        dplyr::left_join(
          accepted_names %>%
            dplyr::rename(
              accepted_taxon_name   = wcvp_taxon_name,
              accepted_family       = wcvp_family,
              accepted_genus        = wcvp_genus,
              accepted_species      = wcvp_species,
              accepted_status       = wcvp_taxon_status,
              accepted_authors      = wcvp_taxon_authors
            ),
          by = c("wcvp_accepted_plant_name_id" = "plant_name_id")
        ) %>%
        dplyr::mutate(
          wcvp_taxon_name    = dplyr::coalesce(accepted_taxon_name, wcvp_taxon_name),
          wcvp_family        = dplyr::coalesce(accepted_family,     wcvp_family),
          wcvp_genus         = dplyr::coalesce(accepted_genus,      wcvp_genus),
          wcvp_species       = dplyr::coalesce(accepted_species,    wcvp_species),
          wcvp_taxon_status  = dplyr::coalesce(accepted_status,     wcvp_taxon_status),
          wcvp_taxon_authors = dplyr::coalesce(accepted_authors,    wcvp_taxon_authors),
          # wcvp_accepted_plant_name_id: NA for already-accepted taxa (mirrors idtax_good_n)
          wcvp_accepted_plant_name_id = dplyr::if_else(
            wcvp_taxon_status == "Accepted", NA_integer_, wcvp_accepted_plant_name_id
          )
        ) %>%
        dplyr::select(-accepted_taxon_name, -accepted_family, -accepted_genus,
                       -accepted_species, -accepted_status, -accepted_authors)
    } else {
      # All matched taxa are already Accepted — clear wcvp_accepted_plant_name_id
      linked <- linked %>%
        dplyr::mutate(wcvp_accepted_plant_name_id = NA_integer_)
    }
  } else if (nrow(linked) > 0) {
    # resolve_synonyms = FALSE — keep wcvp_accepted_plant_name_id as-is (raw from DB)
  }

  # Build result for all requested IDs (including unmatched → name_source = "internal")
  result <- dplyr::tibble(idtax_n = unique_ids) %>%
    dplyr::left_join(linked, by = "idtax_n") %>%
    dplyr::mutate(
      name_source = dplyr::if_else(is.na(wcvp_plant_name_id), "internal", "wcvp")
    )

  result
}


#' Apply WCVP backbone to a taxonomic result data frame
#'
#' Replaces the standard internal taxonomy columns (\code{tax_fam}, \code{tax_gen},
#' \code{tax_esp}, \code{tax_sp_level}, \code{tax_infra_level},
#' \code{tax_infra_level_auth}) with WCVP values where a WCVP match exists.
#' The original internal name is preserved in \code{alt_taxon_name} and a
#' \code{name_source} column (\code{"wcvp"} / \code{"internal"}) is added.
#' Taxa without a WCVP match keep their internal values with
#' \code{name_source = "internal"}.
#'
#' @param data Data frame with internal taxonomy columns.
#' @param wcvp_info Tibble returned by \code{get_wcvp_names()}.
#' @param id_col Character. Name of the column in \code{data} that matches
#'   \code{wcvp_info$idtax_n}. Default \code{"idtax_n"}.
#'
#' @return \code{data} with standard columns overwritten by WCVP values where
#'   available, plus \code{name_source} and \code{alt_taxon_name}.
#'
#' @keywords internal
.apply_wcvp_backbone <- function(data, wcvp_info, id_col = "idtax_n") {

  # Save internal name (most detailed available) as the "other backbone" name
  internal_name_col <- dplyr::case_when(
    "tax_infra_level" %in% names(data) ~ "tax_infra_level",
    "tax_sp_level"    %in% names(data) ~ "tax_sp_level",
    TRUE ~ NA_character_
  )
  if (!is.na(internal_name_col)) {
    data <- data %>%
      dplyr::mutate(alt_taxon_name = .data[[internal_name_col]])
  }

  # Join WCVP info (only the columns we need)
  wcvp_cols <- dplyr::intersect(
    names(wcvp_info),
    c("idtax_n", "wcvp_plant_name_id", "wcvp_accepted_plant_name_id",
      "wcvp_taxon_name", "wcvp_family", "wcvp_genus", "wcvp_species",
      "wcvp_taxon_authors", "wcvp_taxon_status", "name_source")
  )
  data <- data %>%
    dplyr::left_join(
      wcvp_info %>% dplyr::select(dplyr::all_of(wcvp_cols)),
      by = stats::setNames("idtax_n", id_col)
    ) %>%
    # Any row not matched by join gets name_source from wcvp_info (NA here) → set to "internal"
    dplyr::mutate(
      name_source = dplyr::coalesce(.data$name_source, "internal")
    )

  has_wcvp <- data$name_source == "wcvp"

  # Replace each column only if it exists and a WCVP value is available
  if ("tax_fam" %in% names(data))
    data$tax_fam <- dplyr::if_else(
      has_wcvp & !is.na(data$wcvp_family), data$wcvp_family, data$tax_fam)

  if ("tax_gen" %in% names(data))
    data$tax_gen <- dplyr::if_else(
      has_wcvp & !is.na(data$wcvp_genus), data$wcvp_genus, data$tax_gen)

  if ("tax_esp" %in% names(data))
    data$tax_esp <- dplyr::if_else(
      has_wcvp & !is.na(data$wcvp_species), data$wcvp_species, data$tax_esp)

  if ("tax_sp_level" %in% names(data)) {
    wcvp_sp <- dplyr::if_else(
      !is.na(data$wcvp_genus) & !is.na(data$wcvp_species),
      paste(data$wcvp_genus, data$wcvp_species),
      NA_character_
    )
    data$tax_sp_level <- dplyr::if_else(
      has_wcvp & !is.na(wcvp_sp), wcvp_sp, data$tax_sp_level)
  }

  if ("tax_infra_level" %in% names(data))
    data$tax_infra_level <- dplyr::if_else(
      has_wcvp & !is.na(data$wcvp_taxon_name),
      data$wcvp_taxon_name, data$tax_infra_level)

  if ("tax_infra_level_auth" %in% names(data)) {
    wcvp_full_auth <- dplyr::if_else(
      !is.na(data$wcvp_taxon_authors),
      paste(data$wcvp_taxon_name, data$wcvp_taxon_authors),
      data$wcvp_taxon_name
    )
    data$tax_infra_level_auth <- dplyr::if_else(
      has_wcvp & !is.na(data$wcvp_taxon_name),
      wcvp_full_auth, data$tax_infra_level_auth)
  }

  # Drop the wcvp helper columns — values are now in the standard columns
  # wcvp_plant_name_id and wcvp_accepted_plant_name_id are kept as additional ID columns
  data %>% dplyr::select(-dplyr::any_of(c(
    "wcvp_taxon_name", "wcvp_family", "wcvp_genus", "wcvp_species",
    "wcvp_taxon_authors", "wcvp_taxon_status"
  )))
}


# ---- Status & Update Check ---------------------------------------------------

#' Get WCVP Import Status
#'
#' Returns information about the current WCVP import in the database.
#'
#' @param con_taxa Connection to the taxa database. If NULL, calls \code{call.mydb.taxa()}.
#'
#' @return A list with: \code{version}, \code{import_date}, \code{record_count},
#'   \code{link_count}, \code{imported_by}. Returns NULL if no import found.
#'
#' @examples
#' \dontrun{
#' get_wcvp_status()
#' }
#'
#' @export
get_wcvp_status <- function(con_taxa = NULL) {

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }

  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  meta <- tryCatch(
    DBI::dbGetQuery(actual_con, "SELECT * FROM wcvp_import_metadata WHERE is_current = TRUE;"),
    error = function(e) data.frame()
  )

  if (nrow(meta) == 0) {
    cli::cli_alert_info("No WCVP data imported yet")
    return(NULL)
  }

  status <- list(
    version = meta$wcvp_version[1],
    import_date = meta$import_date[1],
    record_count = meta$record_count[1],
    link_count = meta$link_count[1],
    imported_by = meta$imported_by[1],
    r_package_version = meta$r_package_version[1]
  )

  cli::cli_h2("WCVP Import Status")
  cli::cli_alert_info("Version: {status$version}")
  cli::cli_alert_info("Imported: {status$import_date}")
  cli::cli_alert_info("Records: {status$record_count}")
  cli::cli_alert_info("Links: {status$link_count %||% 'none'}")
  cli::cli_alert_info("Imported by: {status$imported_by}")

  return(invisible(status))
}


#' Check if WCVP Update is Available
#'
#' Compares the database WCVP version with the version available in the
#' \code{rWCVP} package.
#'
#' @param con_taxa Connection to the taxa database. If NULL, calls \code{call.mydb.taxa()}.
#'
#' @return Logical. TRUE if a newer version is available.
#'
#' @examples
#' \dontrun{
#' if (check_wcvp_update()) {
#'   import_wcvp_names(con_taxa, force = TRUE)
#' }
#' }
#'
#' @export
check_wcvp_update <- function(con_taxa = NULL) {

  if (!requireNamespace("rWCVP", quietly = TRUE)) {
    stop("Package 'rWCVP' is required. Install with: install.packages('rWCVP')", call. = FALSE)
  }

  status <- get_wcvp_status(con_taxa)

  if (is.null(status)) {
    cli::cli_alert_warning("No WCVP data in database. Run import_wcvp_names() first.")
    return(TRUE)
  }

  available_ver <- rWCVP::wcvp_version()

  if (available_ver != status$version) {
    cli::cli_alert_warning("Update available: DB has {status$version}, rWCVPdata has {available_ver}")
    return(TRUE)
  }

  cli::cli_alert_success("WCVP is up to date (version {status$version})")
  return(FALSE)
}
