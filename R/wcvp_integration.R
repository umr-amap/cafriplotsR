# =============================================================================
# WCVP (World Checklist of Vascular Plants) Integration
#
# WCVP-specific functions: creating the mirror table and importing it from
# rWCVPdata. Matching, links, names and status go through the generic backbone
# layer in R/backbone_core.R; the *_wcvp functions below that do the same are
# kept as thin wrappers with their original column names.
#
# Main functions:
# - setup_wcvp_schema(): Create WCVP tables in taxa DB
# - import_wcvp_names(): Import rWCVPdata into the database
# - check_wcvp_update(): Check if newer WCVP version is available
# - match_taxa_to_wcvp(), save_wcvp_links(), get_wcvp_names(),
#   get_wcvp_status(): wrappers over match_taxa_to_backbone(),
#   save_backbone_links(), get_backbone_names(), get_backbone_status()
#
# The name-matching helpers (.wcvp_match_*) are used for every backbone: the
# backbone view is fetched under WCVP's column names.
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
#' On a new database, apply \file{inst/migrations/multi_backbone.R} afterwards:
#' the package reads WCVP links from \code{taxa_backbone_link} and names through
#' \code{v_backbone_names_wcvp}, not from \code{wcvp_idtax_link}.
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
#' Links in \code{taxa_backbone_link} are kept. Links whose WCVP ID no longer
#' exists in the new version are reported by \code{check_backbone_links("wcvp")},
#' run at the end. The legacy \code{wcvp_idtax_link} is emptied, as before.
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
        "SELECT i.version AS wcvp_version, i.record_count
           FROM backbone_import i
           JOIN backbone_list b USING (id_backbone)
          WHERE b.code = 'wcvp' AND i.is_current;"
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
    DBI::dbExecute(
      actual_con,
      "UPDATE backbone_import SET is_current = FALSE
        WHERE is_current
          AND id_backbone = (SELECT id_backbone FROM backbone_list WHERE code = 'wcvp');"
    )

    # Replace the names. taxa_backbone_link has no foreign key to wcvp_names,
    # so links survive; CASCADE only empties the legacy wcvp_idtax_link, which
    # the package no longer reads.
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
    DBI::dbExecute(actual_con, glue::glue_sql(
      "INSERT INTO backbone_import (id_backbone, version, imported_by, record_count, source_version, is_current)
       SELECT id_backbone, {wcvp_ver}, {Sys.info()['user']}, {n_total}, {as.character(utils::packageVersion('rWCVPdata'))}, TRUE
         FROM backbone_list WHERE code = 'wcvp';",
      .con = actual_con
    ))

    DBI::dbCommit(actual_con)
    if (verbose) cli::cli_alert_success("Successfully imported {n_total} WCVP records (version {wcvp_ver})")

  }, error = function(e) {
    DBI::dbRollback(actual_con)
    stop("WCVP import failed: ", e$message, call. = FALSE)
  })

  if (verbose) check_backbone_links("wcvp", con_taxa = actual_con)

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

  # The exact match does not return the input columns: bring the authors back
  # by row id. Without this, Step 2 failed on a missing column and the caller's
  # tryCatch() turned every author_match = "fuzzy" run into zero exact matches.
  result[[author_col]] <- names_df[[author_col]][match(result[[id_col]], names_df[[id_col]])]

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
      wcvp_id             = NA_character_,
      wcvp_authors        = NA_character_,
      wcvp_rank           = NA_character_,
      wcvp_status         = NA_character_,
      wcvp_homotypic      = NA,
      wcvp_ipni_id        = NA_character_,
      wcvp_accepted_id    = NA_character_,
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
      out$wcvp_id[keep]             <- as.character(best_cands$plant_name_id)
      out$wcvp_authors[keep]        <- best_cands$taxon_authors
      out$wcvp_rank[keep]           <- best_cands$taxon_rank
      out$wcvp_status[keep]         <- best_cands$taxon_status
      out$wcvp_ipni_id[keep]        <- best_cands$ipni_id
      out$wcvp_accepted_id[keep]    <- as.character(best_cands$accepted_plant_name_id)
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
    wcvp_id             = ifelse(has_match, as.character(matched$plant_name_id), NA_character_),
    wcvp_name           = ifelse(has_match, matched$name, NA_character_),
    wcvp_authors        = ifelse(has_match, as.character(matched$taxon_authors), NA_character_),
    wcvp_rank           = ifelse(has_match, as.character(matched$taxon_rank), NA_character_),
    wcvp_status         = ifelse(has_match, as.character(matched$taxon_status), NA_character_),
    wcvp_homotypic      = NA,
    wcvp_ipni_id        = ifelse(has_match, as.character(matched$ipni_id), NA_character_),
    wcvp_accepted_id    = ifelse(has_match, as.character(matched$accepted_plant_name_id), NA_character_),
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
#' Superseded by \code{match_taxa_to_backbone("wcvp", ...)}, which it calls.
#' Kept with its original column names for existing scripts.
#'
#' Returns a tibble for review. Does NOT write to the database automatically.
#' Use \code{save_wcvp_links()} to persist reviewed matches.
#'
#' @inheritParams match_taxa_to_backbone
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

  res <- match_taxa_to_backbone(
    "wcvp",
    con_taxa         = con_taxa,
    tax_ids          = tax_ids,
    methods          = methods,
    fuzzy_threshold  = fuzzy_threshold,
    author_match     = author_match,
    author_threshold = author_threshold,
    n_cores          = n_cores,
    verbose          = verbose
  )

  dplyr::tibble(
    idtax_n             = res$idtax_n,
    taxon_name_internal = res$taxon_name_internal,
    plant_name_id       = suppressWarnings(as.integer(res$external_id)),
    wcvp_taxon_name     = res$backbone_taxon_name,
    match_type          = res$match_type,
    match_score         = res$match_score
  )
}


#' Save WCVP Links to Database
#'
#' Superseded by \code{save_backbone_links(matches, "wcvp", ...)}, which it
#' calls. Links are written to \code{taxa_backbone_link}; a taxon left with a
#' single WCVP link gets it marked preferred.
#'
#' @param matches Tibble of matches from \code{match_taxa_to_wcvp()}, with
#'   \code{idtax_n}, \code{plant_name_id}, \code{match_type} and optionally
#'   \code{match_score}.
#' @param con_taxa Connection to the taxa database.
#' @param replace Logical. If TRUE, deletes the existing WCVP links of the
#'   affected \code{idtax_n} before inserting. Default TRUE.
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

  if (!"external_id" %in% names(matches) && "plant_name_id" %in% names(matches)) {
    matches$external_id <- as.character(matches$plant_name_id)
  }

  save_backbone_links(matches, "wcvp", con_taxa = con_taxa,
                      replace = replace, verbose = verbose)
}


# ---- Query WCVP Names -------------------------------------------------------

#' Get WCVP Names for Internal Taxa
#'
#' Superseded by \code{get_backbone_names(idtax_n, "wcvp")}, which it calls.
#' Kept with its original \code{wcvp_*} column names for existing scripts and
#' the Shiny modules.
#'
#' Only preferred links are used. Taxa without one get
#' \code{name_source = "internal"}, as do all taxa when the WCVP backbone is
#' not available.
#'
#' @param idtax_n Integer vector of internal taxon IDs.
#' @param con_taxa Connection to the taxa database. If NULL, calls \code{call.mydb.taxa()}.
#' @param resolve_synonyms Logical. If TRUE and a linked WCVP name is a synonym,
#'   follow its pointer to the accepted name. Default TRUE.
#'
#' @return A tibble with columns: \code{idtax_n}, \code{wcvp_plant_name_id},
#'   \code{wcvp_accepted_plant_name_id}, \code{wcvp_taxon_name},
#'   \code{wcvp_family}, \code{wcvp_genus}, \code{wcvp_species},
#'   \code{wcvp_taxon_status}, \code{wcvp_taxon_authors}, \code{name_source}.
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

  info <- tryCatch(
    get_backbone_names(idtax_n, "wcvp", con_taxa = con_taxa,
                       resolve_synonyms = resolve_synonyms),
    error = function(e) {
      message("Note: WCVP names not available (", conditionMessage(e),
              "). Returning internal names.")
      NULL
    }
  )
  if (is.null(info)) {
    info <- .shape_backbone_names(NULL, unique(stats::na.omit(as.integer(idtax_n))), "wcvp")
  }

  dplyr::tibble(
    idtax_n                     = info$idtax_n,
    wcvp_plant_name_id          = suppressWarnings(as.integer(info$backbone_name_id)),
    wcvp_accepted_plant_name_id = suppressWarnings(as.integer(info$backbone_accepted_id)),
    wcvp_taxon_name             = info$backbone_taxon_name,
    wcvp_family                 = info$backbone_family,
    wcvp_genus                  = info$backbone_genus,
    wcvp_species                = info$backbone_species,
    wcvp_taxon_status           = info$backbone_status_raw,
    wcvp_taxon_authors          = info$backbone_authors,
    name_source                 = info$name_source
  )
}


# ---- Status & Update Check ---------------------------------------------------

#' Get WCVP Import Status
#'
#' Returns information about the current WCVP import in the database.
#' Superseded by \code{get_backbone_status("wcvp")}, which it calls.
#'
#' @param con_taxa Connection to the taxa database. If NULL, calls \code{call.mydb.taxa()}.
#'
#' @return A list with: \code{version}, \code{import_date}, \code{record_count},
#'   \code{link_count}, \code{imported_by}, \code{r_package_version}. Returns NULL if no import found.
#'
#' @examples
#' \dontrun{
#' get_wcvp_status()
#' }
#'
#' @export
get_wcvp_status <- function(con_taxa = NULL) {

  status <- get_backbone_status("wcvp", con_taxa = con_taxa)
  if (is.null(status)) return(NULL)

  status$r_package_version <- status$source_version
  invisible(status)
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
