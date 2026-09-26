# Cache Management for Taxonomic Backbone
#
# Functions for caching the taxonomic backbone to improve performance
# for users with slow internet connections.

# Bump this whenever a change makes existing caches wrong rather than merely
# old, so `load_backbone_cache()` discards them and the app downloads again.
# A stale cache is otherwise invisible: it loads, it looks complete, and the
# only symptom is names that quietly fail to match.
#
# 1.0 -> 1.1: caches written before the "ZZ auct." filter stopped discarding
#   every taxon with no recorded author are missing those taxa entirely.
.backbone_cache_version <- "1.1"

# One expanded backbone per R process.
#
# The cache file is 365k records that come back as about 80 MB of tibble, and
# `load_backbone_cache()` has five callers -- the login module, the name review
# module, the matching module, the app itself and `match_taxa()` -- several of
# which run in a single session. Each call used to allocate its own copy, and
# on Windows, where freed pages are not readily returned to the OS, repeated
# attempts walked the process past the system commit limit: first a worker
# unable to map a DLL ("LoadLibrary failure: The paging file is too small"),
# then allocations failing in the parent at a few Mb.
#
# The backbone is immutable reference data, so one copy serves everyone. The
# key is the cache file's identity on disk (see below), which means a file
# refreshed by any route -- this process, another session, a hand-edit --
# invalidates the memo without anyone having to remember to say so.
.backbone_memo <- new.env(parent = emptyenv())

# Identity of the cache file as it is on disk right now, or NULL if absent.
# The path is part of it: `get_backbone_cache_path()` is not a constant --- the
# tests point it elsewhere, and so could a changed R_user_dir --- and two
# different files can share a size and an mtime.
.backbone_memo_key <- function(cache_file) {
  info <- file.info(cache_file)
  if (is.na(info$size)) return(NULL)
  paste0(cache_file, "|", info$size, "|", as.numeric(info$mtime))
}

#' Forget the in-memory backbone
#'
#' Drops the process-wide copy held by [load_backbone_cache()]. The memo
#' invalidates itself when the cache file changes, so this is only needed to
#' hand the memory back --- after a long matching session, say.
#'
#' @return Invisibly TRUE if a copy was held, FALSE otherwise.
#' @keywords internal
#' @export
release_backbone_memory <- function() {
  held <- exists("data", envir = .backbone_memo, inherits = FALSE)
  rm(list = ls(.backbone_memo, all.names = TRUE), envir = .backbone_memo)
  invisible(held)
}

#' Get backbone cache directory path
#'
#' @description
#' Returns the path to the cache directory for taxonomic backbone storage.
#' Creates the directory if it doesn't exist. Uses platform-appropriate cache
#' location via `tools::R_user_dir()`.
#'
#' @return Character string, full path to cache directory
#'
#' @keywords internal
#' @export
get_backbone_cache_path <- function() {
  cache_dir <- tools::R_user_dir("CafriplotsR", which = "cache")

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  return(cache_dir)
}


#' Check if valid backbone cache exists
#'
#' @description
#' Checks whether both the cache data file and metadata file exist.
#'
#' @return Logical, TRUE if cache exists
#'
#' @keywords internal
#' @export
cache_exists <- function() {
  cache_dir <- get_backbone_cache_path()
  cache_file <- file.path(cache_dir, "backbone_cache.rds")
  metadata_file <- file.path(cache_dir, "backbone_metadata.rds")

  return(file.exists(cache_file) && file.exists(metadata_file))
}


#' Get cache metadata with formatted displays
#'
#' @description
#' Retrieves cache metadata including download date, file size, and record count.
#' Formats age display (e.g., "Today", "3 days ago", "2 weeks ago") and
#' size display (e.g., "650 KB") for user-friendly presentation.
#'
#' @return List with metadata fields, or NULL if cache unavailable or invalid:
#'   - download_date: Date cache was created
#'   - n_records: Number of taxa records
#'   - file_size_bytes: File size in bytes
#'   - age_days: Numeric age in days
#'   - age_display: Human-readable age string
#'   - size_display: Human-readable size string
#'   - columns: Character vector of column names
#'   - cache_version: Cache format version
#'
#' @keywords internal
#' @export
get_cache_metadata <- function() {
  if (!cache_exists()) {
    return(NULL)
  }

  cache_dir <- get_backbone_cache_path()
  metadata_file <- file.path(cache_dir, "backbone_metadata.rds")

  tryCatch({
    metadata <- readRDS(metadata_file)

    # Calculate age
    cache_date <- as.Date(metadata$download_date)
    today <- Sys.Date()
    age_days <- as.numeric(difftime(today, cache_date, units = "days"))

    # Format age display
    metadata$age_days <- age_days
    metadata$age_display <- if (age_days == 0) {
      "Today"
    } else if (age_days == 1) {
      "Yesterday"
    } else if (age_days < 7) {
      paste(age_days, "days ago")
    } else if (age_days < 30) {
      paste(floor(age_days / 7), "weeks ago")
    } else if (age_days < 365) {
      paste(floor(age_days / 30), "months ago")
    } else {
      paste(floor(age_days / 365), "years ago")
    }

    # Format size
    metadata$size_display <- format(
      structure(metadata$file_size_bytes, class = "object_size"),
      units = "auto"
    )

    return(metadata)
  }, error = function(e) {
    return(NULL)
  })
}


#' Save backbone data to cache
#'
#' @description
#' Saves the processed taxonomic backbone data to cache along with metadata.
#' Compresses data for efficient storage.
#'
#' @param backbone_data Data frame or tibble with processed backbone data.
#'   Must include required columns: idtax_n, idtax_good_n, tax_fam, tax_gen,
#'   tax_esp, tax_sp_level, tax_gen_level, tax_fam_level, tax_class_level
#'
#' @return Logical, TRUE on successful save, FALSE on error
#'
#' @keywords internal
#' @export
save_backbone_cache <- function(backbone_data) {
  cache_dir <- get_backbone_cache_path()
  cache_file <- file.path(cache_dir, "backbone_cache.rds")
  metadata_file <- file.path(cache_dir, "backbone_metadata.rds")

  tryCatch({
    # Save backbone
    saveRDS(backbone_data, cache_file, compress = TRUE)

    # Create metadata
    metadata <- list(
      download_date = Sys.Date(),
      n_records = nrow(backbone_data),
      file_size_bytes = file.info(cache_file)$size,
      columns = names(backbone_data),
      cache_version = .backbone_cache_version
    )

    saveRDS(metadata, metadata_file, compress = FALSE)

    # The object is already expanded in front of us, so seed the memo rather
    # than let the next `load_backbone_cache()` read back what we just wrote.
    .backbone_memo$key  <- .backbone_memo_key(cache_file)
    .backbone_memo$data <- dplyr::as_tibble(backbone_data)

    return(TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to save cache: {e$message}")
    return(FALSE)
  })
}


#' Load backbone from cache with validation
#'
#' @description
#' Loads the taxonomic backbone from cache and validates its structure.
#' Returns NULL if cache is corrupted or missing required columns.
#'
#' @return Tibble with backbone data, or NULL if invalid/unavailable
#'
#' @keywords internal
#' @export
load_backbone_cache <- function() {
  if (!cache_exists()) {
    return(NULL)
  }

  cache_dir <- get_backbone_cache_path()
  cache_file <- file.path(cache_dir, "backbone_cache.rds")
  metadata_file <- file.path(cache_dir, "backbone_metadata.rds")

  tryCatch({
    # Refuse a cache written by a version whose contents we now know to be
    # wrong. Returning NULL is what callers already treat as "no usable
    # cache", so this simply routes them to a fresh download.
    version <- tryCatch(readRDS(metadata_file)$cache_version,
                        error = function(e) NULL)

    if (!identical(version, .backbone_cache_version)) {
      cli::cli_alert_warning(
        "Cached backbone was written by an older version ({version %||% 'unknown'}) and will be re-downloaded"
      )
      return(NULL)
    }

    # Hand back the copy this process already expanded, unless the file has
    # been rewritten since. Re-reading is what used to exhaust memory.
    key <- .backbone_memo_key(cache_file)
    if (!is.null(key) &&
        identical(.backbone_memo$key, key) &&
        !is.null(.backbone_memo$data)) {
      return(.backbone_memo$data)
    }

    backbone <- readRDS(cache_file)

    # Validate structure
    required_cols <- c("idtax_n", "idtax_good_n", "tax_fam", "tax_gen",
                       "tax_esp", "tax_sp_level", "tax_gen_level",
                       "tax_fam_level", "tax_class_level")

    if (!all(required_cols %in% names(backbone))) {
      cli::cli_alert_warning("Cache has invalid structure, will re-download")
      return(NULL)
    }

    backbone <- dplyr::as_tibble(backbone)

    .backbone_memo$key  <- key
    .backbone_memo$data <- backbone

    return(backbone)
  }, error = function(e) {
    cli::cli_alert_warning("Failed to load cache: {e$message}")
    return(NULL)
  })
}


#' Clear backbone cache
#'
#' @description
#' Deletes the cached taxonomic backbone data and metadata files.
#' This function is exported for user convenience when they want to
#' force a fresh download or clear disk space.
#'
#' @return Logical (invisibly), TRUE if files were deleted, FALSE if no cache existed
#'
#' @examples
#' \dontrun{
#' # Clear the taxonomic backbone cache
#' delete_backbone_cache()
#' }
#'
#' @export
delete_backbone_cache <- function() {
  cache_dir <- get_backbone_cache_path()
  cache_file <- file.path(cache_dir, "backbone_cache.rds")
  metadata_file <- file.path(cache_dir, "backbone_metadata.rds")

  deleted <- FALSE

  if (file.exists(cache_file)) {
    unlink(cache_file)
    deleted <- TRUE
  }

  if (file.exists(metadata_file)) {
    unlink(metadata_file)
    deleted <- TRUE
  }

  # The file is gone, so the copy in memory has nothing left to agree with.
  release_backbone_memory()

  if (deleted) {
    cli::cli_alert_success("Cache cleared successfully")
  } else {
    cli::cli_alert_info("No cache to clear")
  }

  return(invisible(deleted))
}
