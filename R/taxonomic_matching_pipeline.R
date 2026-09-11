# Taxonomic matching pipeline — the computation, separated from the app
#
# Everything here is pure: it takes the user's table, a backbone data.frame and
# a few parameters, and returns results. It touches no database, no reactive
# value and no Shiny session, which is what makes it safe to run in a separate
# R process (see .start_matching_job() below).
#
# That separation is the point. Shiny Server Open Source runs ONE R process per
# app, shared by every visitor and by Kubernetes' health probes. Running a
# multi-minute match inside it froze every other session and got the pod
# restarted mid-run. The app now hands this work to a background process and
# polls it, so the Shiny worker stays responsive throughout.

# ---------------------------------------------------------------------------
# Cross-process signalling
# ---------------------------------------------------------------------------
# The parent and the worker are separate R processes, so they communicate
# through three files, all created by the parent in a job directory it owns:
#
#   input.rds     written once by the parent, read once by the worker
#   progress.json overwritten by the worker, polled by the parent
#   result.rds    written once by the worker, read once by the parent
#   cancel        created by the parent, tested by the worker
#
# Files, not a socket or a queue, because the whole exchange is four messages
# and a file survives the worker outliving its parent's interest in it.

#' Has the caller asked for this run to stop?
#'
#' @param cancel_file Character path, or NULL when the run cannot be cancelled.
#' @return Logical scalar.
#' @keywords internal
.matching_cancelled <- function(cancel_file) {
  !is.null(cancel_file) && file.exists(cancel_file)
}

#' Publish progress for a polling parent process
#'
#' Written to a temporary file and renamed into place, so a parent polling
#' mid-write reads the previous state rather than half a line of JSON.
#'
#' @param progress_file Character path, or NULL to discard progress.
#' @param stage Character, one of "resume", "fuzzy_start", "fuzzy".
#' @param i,n Integer, position and total for the current stage.
#' @param name Character, the name being matched, if any.
#' @return Invisibly NULL.
#' @keywords internal
.write_matching_progress <- function(progress_file, stage, i = NA_integer_,
                                     n = NA_integer_, name = NA_character_) {
  if (is.null(progress_file)) return(invisible(NULL))

  tmp <- paste0(progress_file, ".tmp")
  ok <- tryCatch({
    writeLines(
      jsonlite::toJSON(
        list(stage = stage, i = i, n = n, name = name,
             time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        auto_unbox = TRUE, null = "null", na = "null"
      ),
      tmp
    )
    TRUE
  }, error = function(e) FALSE)

  # A failed progress write must never take the run down with it: progress is
  # cosmetic, the match is not.
  if (ok) tryCatch(file.rename(tmp, progress_file), error = function(e) FALSE)

  invisible(NULL)
}

#' Read the progress published by a matching worker
#'
#' @param progress_file Character path.
#' @return A list with `stage`, `i`, `n`, `name`, or NULL if nothing readable.
#' @keywords internal
.read_matching_progress <- function(progress_file) {
  if (is.null(progress_file) || !file.exists(progress_file)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(progress_file, simplifyVector = TRUE),
    error = function(e) NULL
  )
}

#' Run the taxonomic matching pipeline
#'
#' The batch-exact cascade followed by per-name fuzzy matching, with
#' checkpointing so an interrupted run can resume. Pure computation: give it
#' the same inputs and it returns the same results, in this process or any
#' other.
#'
#' @param user_df data.frame. The user's table.
#' @param col_name Character. Column of `user_df` holding the names to match.
#' @param backbone data.frame. Shaped backbone, from `.shape_backbone()` or
#'   `load_backbone_cache()`.
#' @param min_similarity Numeric in 0-1. Fuzzy matching threshold.
#' @param include_authors Logical. Match on authorship as well as name.
#' @param input_hash Character. Identifies the run, from `.compute_input_hash()`.
#' @param rm_mode Character, "fresh" or "resume".
#' @param checkpoint_file Character path for the checkpoint. Passed explicitly
#'   because a worker process has its own `tempdir()` and would otherwise write
#'   its checkpoint somewhere the parent can never find.
#' @param cancel_file Character path polled for cancellation, or NULL.
#' @param progress Function called as `progress(stage, i, n, name)`, or NULL.
#'
#' @return A list with `status`:
#'   * `"ok"` — plus `updated_data`, `best_matches` and `stats`
#'   * `"empty"` — the chosen column held no names
#'   * `"cancelled"` — `cancel_file` appeared mid-run; the checkpoint is current
#' @keywords internal
.run_matching_pipeline <- function(user_df, col_name, backbone,
                                   min_similarity, include_authors,
                                   input_hash, rm_mode = "fresh",
                                   checkpoint_file = NULL,
                                   cancel_file = NULL,
                                   progress = NULL) {

  stopifnot(is.data.frame(user_df), is.character(col_name), length(col_name) == 1)
  if (!col_name %in% names(user_df)) {
    stop("Column '", col_name, "' is not in the supplied data.")
  }

  # Local aliases so the pipeline body below reads as it did when it lived
  # inside the module, back when these were reactive reads.
  min_sim      <- min_similarity
  incl_authors <- isTRUE(include_authors)
  cancelled    <- FALSE

  if (is.null(progress)) progress <- function(...) invisible(NULL)


  # --- Decide: restore checkpoint or run exact matching from scratch ---

  best_matches    <- NULL
  fuzzy_results   <- list()
  still_unmatched <- character(0)
  start_idx       <- 1L
  total_names     <- 0L

  if (rm_mode == "resume") {
    chk <- .load_matching_checkpoint(input_hash, checkpoint_file)
    if (!is.null(chk)) {
      best_matches    <- chk$best_matches
      fuzzy_results   <- chk$fuzzy_results
      still_unmatched <- chk$still_unmatched
      total_names     <- chk$total_names
      start_idx       <- chk$current_index + 1L

      progress("resume", i = start_idx, n = length(still_unmatched))
    } else {
      # Checkpoint disappeared — fall back to fresh
      rm_mode <- "fresh"
    }
  }

  if (rm_mode == "fresh") {
    unique_names <- user_df %>%
      dplyr::pull(!!rlang::sym(col_name)) %>%
      unique() %>%
      {ifelse(is.na(.), "NA", .)}

    unique_names_to_match <- unique_names
    total_names           <- length(unique_names)

    if (total_names == 0) {
      return(list(status = "empty"))
    }

    cleaned_names <- clean_taxonomic_name(unique_names_to_match)

    # Authorship is stripped once, here, so the batch stages below can
    # match "Genus species Author" on the name alone. Authors vary far
    # more than names do, and without this every such name skipped the
    # batch stages entirely and paid for the slow per-name path.
    stripped_names <- vapply(
      cleaned_names,
      function(n) parse_taxonomic_name(n)$full_name_no_auth %||% NA_character_,
      character(1),
      USE.NAMES = FALSE
    )

    input_df <- data.frame(
      input_name   = unique_names_to_match,
      cleaned_name = cleaned_names,
      stringsAsFactors = FALSE
    )

    # Kept apart from input_df so the extra key never leaks into the
    # results that are joined back onto the user's data.
    strip_map <- data.frame(
      input_name     = unique_names_to_match,
      stripped_name  = stripped_names,
      stringsAsFactors = FALSE
    )

    # STEP 3: Batch exact — species level
    unique_species <- backbone %>%
      dplyr::filter(!is.na(tax_sp_level)) %>%
      dplyr::group_by(tax_sp_level) %>%
      dplyr::filter(dplyr::n() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        tax_sp_level, idtax_n, idtax_good_n,
        tax_fam, tax_gen, tax_esp, tax_rank01, tax_nam01
      ) %>%
      dplyr::mutate(
        matched_name = tax_sp_level,
        match_method = "exact",
        match_score  = 1.0
      )

    matches_species <- input_df %>%
      dplyr::left_join(unique_species, by = c("cleaned_name" = "tax_sp_level"))

    # STEP 3b: Batch exact — species level, author names included
    # `include_authors` used to reach the per-name fallback only, so a
    # file written "Genus species Author" matched nothing in this batch
    # stage and every single name paid for the slow path. The key built
    # here is the one .build_backbone_name_field() searches on.
    unmatched_after_species <- matches_species %>%
      dplyr::filter(is.na(idtax_n)) %>%
      dplyr::select(input_name, cleaned_name)

    matches_species_auth <- NULL

    if (isTRUE(incl_authors) && "author1" %in% names(backbone)) {
      # The key is built by the same helper .match_exact_r() searches on,
      # so this batch stage and the per-name fallback cannot drift apart.
      sp_auth_key <- .build_backbone_name_field(backbone,
                                                include_authors = TRUE)

      unique_species_auth <- backbone %>%
        dplyr::mutate(tax_sp_auth_level = sp_auth_key) %>%
        dplyr::filter(!is.na(tax_esp), tax_esp != "",
                      !is.na(author1), author1 != "",
                      !is.na(tax_sp_auth_level)) %>%
        dplyr::group_by(tax_sp_auth_level) %>%
        dplyr::filter(dplyr::n() == 1) %>%
        dplyr::ungroup() %>%
        dplyr::select(
          tax_sp_auth_level, idtax_n, idtax_good_n,
          tax_fam, tax_gen, tax_esp, tax_rank01, tax_nam01
        ) %>%
        dplyr::mutate(
          matched_name = tax_sp_auth_level,
          match_method = "exact",
          match_score  = 1.0
        )

      matches_species_auth <- unmatched_after_species %>%
        dplyr::left_join(unique_species_auth,
                         by = c("cleaned_name" = "tax_sp_auth_level"))

      unmatched_after_species <- matches_species_auth %>%
        dplyr::filter(is.na(idtax_n)) %>%
        dplyr::select(input_name, cleaned_name)
    }

    # STEP 3c: Batch exact — species level, on the author-stripped name
    # This is the common case: genus and epithet agree exactly and only
    # the authorship is written differently.
    matches_species_strip <- unmatched_after_species %>%
      dplyr::left_join(strip_map, by = "input_name") %>%
      dplyr::left_join(unique_species,
                       by = c("stripped_name" = "tax_sp_level")) %>%
      dplyr::select(-stripped_name)

    unmatched_after_species <- matches_species_strip %>%
      dplyr::filter(is.na(idtax_n)) %>%
      dplyr::select(input_name, cleaned_name)

    # STEP 4: Batch exact — genus level

    unique_genera <- backbone %>%
      dplyr::filter(tax_level == "genus", !is.na(tax_gen_level)) %>%
      dplyr::group_by(tax_gen_level) %>%
      dplyr::filter(dplyr::n() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(tax_gen_level, idtax_n, idtax_good_n, tax_fam, tax_gen) %>%
      dplyr::mutate(
        matched_name = tax_gen_level,
        match_method = "exact",
        match_score  = 1.0
      )

    matches_genus <- unmatched_after_species %>%
      dplyr::left_join(unique_genera, by = c("cleaned_name" = "tax_gen_level"))

    # STEP 4b: Batch exact — genus level, on the author-stripped name
    matches_genus_strip <- matches_genus %>%
      dplyr::filter(is.na(idtax_n)) %>%
      dplyr::select(input_name, cleaned_name) %>%
      dplyr::left_join(strip_map, by = "input_name") %>%
      dplyr::left_join(unique_genera,
                       by = c("stripped_name" = "tax_gen_level")) %>%
      dplyr::select(-stripped_name)

    # STEP 5: Batch exact — family level
    unmatched_after_genus <- matches_genus_strip %>%
      dplyr::filter(is.na(idtax_n)) %>%
      dplyr::select(input_name, cleaned_name)

    unique_families <- backbone %>%
      dplyr::filter(tax_level == "family", !is.na(tax_fam_level)) %>%
      dplyr::group_by(tax_fam_level) %>%
      dplyr::filter(dplyr::n() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(tax_fam_level, idtax_n, idtax_good_n, tax_fam) %>%
      dplyr::mutate(
        matched_name = tax_fam_level,
        match_method = "exact",
        match_score  = 1.0
      )

    matches_family <- unmatched_after_genus %>%
      dplyr::left_join(unique_families, by = c("cleaned_name" = "tax_fam_level"))

    # STEP 5.5: Batch exact — class level
    unmatched_after_family <- matches_family %>%
      dplyr::filter(is.na(idtax_n)) %>%
      dplyr::select(input_name, cleaned_name)

    unique_classes <- backbone %>%
      dplyr::filter(tax_level == "higher", !is.na(tax_class_level)) %>%
      dplyr::group_by(tax_class_level) %>%
      dplyr::filter(dplyr::n() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(tax_class_level, idtax_n, idtax_good_n) %>%
      dplyr::mutate(
        matched_name = tax_class_level,
        match_method = "exact",
        match_score  = 1.0,
        tax_fam      = NA_character_,
        tax_gen      = NA_character_,
        tax_esp      = NA_character_,
        tax_rank01   = NA_character_,
        tax_nam01    = NA_character_
      )

    matches_class <- unmatched_after_family %>%
      dplyr::left_join(unique_classes, by = c("cleaned_name" = "tax_class_level"))

    # STEP 6: Combine all exact matches
    if (!is.null(matches_species_auth)) {
      matches_species <- matches_species %>%
        dplyr::rows_update(
          matches_species_auth %>% dplyr::filter(!is.na(idtax_n)),
          by = "input_name", unmatched = "ignore"
        )
    }

    matches_species <- matches_species %>%
      dplyr::rows_update(
        matches_species_strip %>% dplyr::filter(!is.na(idtax_n)),
        by = "input_name", unmatched = "ignore"
      ) %>%
      dplyr::rows_update(
        matches_genus %>% dplyr::filter(!is.na(idtax_n)),
        by = "input_name", unmatched = "ignore"
      ) %>%
      dplyr::rows_update(
        matches_genus_strip %>% dplyr::filter(!is.na(idtax_n)),
        by = "input_name", unmatched = "ignore"
      ) %>%
      dplyr::rows_update(
        matches_family %>% dplyr::filter(!is.na(idtax_n)),
        by = "input_name", unmatched = "ignore"
      ) %>%
      dplyr::rows_update(
        matches_class %>% dplyr::filter(!is.na(idtax_n)),
        by = "input_name", unmatched = "ignore"
      )

    best_matches <- matches_species

    still_unmatched <- best_matches %>%
      dplyr::filter(is.na(idtax_n)) %>%
      dplyr::pull(input_name)

    fuzzy_results <- list()
    start_idx     <- 1L
  }

  # --- STEP 7: Fuzzy matching (shared path for fresh and resume) ---

  if (start_idx <= length(still_unmatched)) {
    progress("fuzzy_start", i = 0L, n = length(still_unmatched))

    # Checkpointing is throttled rather than done on every name. Each
    # save re-serialises best_matches AND the whole growing
    # fuzzy_results list, so saving per name costs O(n^2) writes and
    # came to dominate long runs. Every 25 names or 30 seconds bounds
    # the rework on resume to a handful of names while making the I/O
    # negligible.
    chk_every_n    <- 25L
    chk_every_secs <- 30
    last_chk_index <- start_idx - 1L
    last_chk_time  <- Sys.time()

    save_checkpoint <- function(index) {
      .save_matching_checkpoint(
        input_hash, best_matches, fuzzy_results,
        still_unmatched, index, total_names,
        checkpoint_file
      )
      last_chk_index <<- index
      last_chk_time  <<- Sys.time()
    }

    for (i in start_idx:length(still_unmatched)) {
      # Cancellation arrives as a file, not a variable: the caller is
      # normally a different R process (see .start_matching_job()), and a
      # file is the one channel both ends can see. Checked before the
      # work rather than after, so a cancel costs at most one name.
      if (.matching_cancelled(cancel_file)) {
        # Abandoning the run: flush what the throttle is still holding,
        # so the user resumes from the last name actually matched.
        if (i > start_idx && last_chk_index < i - 1L) save_checkpoint(i - 1L)
        cancelled <- TRUE
        break
      }

      name <- still_unmatched[i]

      progress("fuzzy", i = i, n = length(still_unmatched), name = name)

      match_result <- match_taxonomic_names(
        names          = name,
        method         = "hierarchical",
        max_matches    = 1,
        min_similarity = min_sim,
        include_synonyms = TRUE,
        return_scores  = TRUE,
        include_authors = incl_authors,
        con            = NULL,
        backbone       = backbone,
        verbose        = FALSE
      )

      fuzzy_results[[i]] <- match_result

      # Persist progress — survives laptop sleep / crash. Note this is
      # tempdir(), i.e. the container's filesystem on a served
      # deployment: it survives a browser reload, not a pod restart.
      if (i - last_chk_index >= chk_every_n ||
          as.numeric(difftime(Sys.time(), last_chk_time, units = "secs")) >= chk_every_secs) {
        save_checkpoint(i)
      }
    }

    if (cancelled) return(list(status = "cancelled"))

    # Merge fuzzy results into best_matches
    fuzzy_matches <- dplyr::bind_rows(fuzzy_results) %>%
      dplyr::filter(match_rank == 1) %>%
      dplyr::distinct(input_name, .keep_all = TRUE)

    if (nrow(fuzzy_matches) > 0) {
      fuzzy_for_update <- fuzzy_matches %>%
        dplyr::select(
          input_name, idtax_n, idtax_good_n,
          matched_name, match_method, match_score,
          tax_fam, tax_gen, tax_esp
        )

      best_matches <- best_matches %>%
        dplyr::rows_update(
          fuzzy_for_update,
          by = "input_name", unmatched = "ignore"
        )
    }
  }

  # Matching complete — remove checkpoint file
  .delete_matching_checkpoint(input_hash, checkpoint_file)

  # --- Synonym information ---
  best_matches <- best_matches %>%
    dplyr::mutate(
      is_synonym = idtax_n != idtax_good_n & !is.na(idtax_n) & !is.na(idtax_good_n)
    )

  if (any(best_matches$is_synonym, na.rm = TRUE)) {
    synonym_ids <- best_matches %>%
      dplyr::filter(is_synonym) %>%
      dplyr::pull(idtax_good_n) %>%
      unique()

    accepted_names <- backbone %>%
      dplyr::filter(idtax_n %in% synonym_ids) %>%
      dplyr::mutate(
        accepted_name = dplyr::case_when(
          !is.na(tax_nam01) & tax_nam01 != "" ~ paste(tax_gen, tax_esp, tax_rank01, tax_nam01),
          !is.na(tax_esp)   & tax_esp != ""   ~ paste(tax_gen, tax_esp),
          !is.na(tax_gen)                     ~ tax_gen,
          TRUE                                ~ tax_fam
        )
      ) %>%
      dplyr::select(idtax_n, accepted_name) %>%
      dplyr::distinct(idtax_n, .keep_all = TRUE)

    best_matches <- best_matches %>%
      dplyr::left_join(accepted_names, by = c("idtax_good_n" = "idtax_n"))
  } else {
    best_matches$accepted_name <- NA_character_
  }

  # --- Statistics ---
  n_exact    <- sum(best_matches$match_method == "exact",              na.rm = TRUE)
  n_genus    <- sum(best_matches$match_method == "genus_constrained",  na.rm = TRUE)
  n_fuzzy    <- sum(best_matches$match_method == "fuzzy",              na.rm = TRUE)
  n_unmatched <- sum(is.na(best_matches$idtax_n))

  stats <- list(
    total_names = total_names,
    n_exact     = n_exact,
    n_genus     = n_genus,
    n_fuzzy     = n_fuzzy,
    n_unmatched = n_unmatched
  )

  # --- Join with user data ---
  best_matches_for_join <- best_matches %>%
    dplyr::select(
      input_name, idtax_n, idtax_good_n,
      matched_name, match_method, match_score,
      is_synonym, accepted_name
    ) %>%
    dplyr::distinct(input_name, .keep_all = TRUE) %>%
    dplyr::rename(!!col_name := input_name)

  updated_data <- user_df %>%
    dplyr::left_join(best_matches_for_join, by = col_name)

  updated_data <- updated_data %>%
    dplyr::mutate(
      corrected_name = dplyr::case_when(
        is_synonym & !is.na(accepted_name) ~ accepted_name,
        !is.na(matched_name)               ~ matched_name,
        TRUE                               ~ NA_character_
      )
    )


  list(
    status       = "ok",
    updated_data = updated_data,
    best_matches = best_matches,
    stats        = stats
  )
}

# ---------------------------------------------------------------------------
# Running the pipeline in a background R process
# ---------------------------------------------------------------------------

#' Can matching run in a background process here?
#'
#' Two things have to hold. `callr` must be installed, and CafriplotsR must be
#' *installed* rather than merely loaded from source: the worker is a fresh R
#' session that resolves `CafriplotsR::.matching_worker`, which a
#' `devtools::load_all()` source tree cannot satisfy. The presence of
#' `Meta/package.rds` is what separates the two.
#'
#' When this returns FALSE the caller runs the pipeline in-process, which is
#' correct but blocks — fine for a developer at a console, not for a server.
#'
#' @return Logical scalar.
#' @keywords internal
.async_matching_available <- function() {
  if (!isTRUE(getOption("cafri.async_matching", TRUE))) return(FALSE)
  if (!requireNamespace("callr", quietly = TRUE)) return(FALSE)

  pkg <- tryCatch(find.package("CafriplotsR"), error = function(e) character(0))
  length(pkg) == 1 && file.exists(file.path(pkg, "Meta", "package.rds"))
}

#' Entry point executed inside the worker process
#'
#' Reads one input file, runs the pipeline, writes one result file. Exported
#' only so a fresh R session can reach it as `CafriplotsR::.matching_worker()`;
#' it is not meant to be called by hand.
#'
#' @param input_file Character path to the RDS written by `.start_matching_job()`.
#' @param result_file Character path the result RDS is written to.
#' @return Invisibly NULL. The result travels through `result_file`.
#' @export
#' @keywords internal
.matching_worker <- function(input_file, result_file) {
  args <- readRDS(input_file)

  result <- tryCatch(
    .run_matching_pipeline(
      user_df         = args$user_df,
      col_name        = args$col_name,
      backbone        = args$backbone,
      min_similarity  = args$min_similarity,
      include_authors = args$include_authors,
      input_hash      = args$input_hash,
      rm_mode         = args$rm_mode,
      checkpoint_file = args$checkpoint_file,
      cancel_file     = args$cancel_file,
      progress        = function(stage, i = NA_integer_, n = NA_integer_,
                                 name = NA_character_) {
        .write_matching_progress(args$progress_file, stage, i, n, name)
      }
    ),
    error = function(e) list(status = "error", message = conditionMessage(e))
  )

  saveRDS(result, result_file)
  invisible(NULL)
}

#' Start a matching run in a background R process
#'
#' Serialises the inputs to a job directory and launches a worker against them.
#' Returns immediately; the caller polls with `.poll_matching_job()`.
#'
#' Note the backbone crosses the process boundary as a file, so it is briefly
#' held twice in memory and once on disk. That is the price of not blocking the
#' Shiny worker, and the backbone is the largest thing being passed.
#'
#' @param user_df,col_name,backbone,min_similarity,include_authors,input_hash,rm_mode
#'   As for `.run_matching_pipeline()`.
#' @param checkpoint_file Character path for the checkpoint, computed by the
#'   caller so that both processes agree on it.
#'
#' @return A job list: `proc` (the `callr` process), `dir`, `progress_file`,
#'   `result_file`, `cancel_file`, `started_at`.
#' @keywords internal
.start_matching_job <- function(user_df, col_name, backbone,
                                min_similarity, include_authors,
                                input_hash, rm_mode, checkpoint_file) {

  dir <- file.path(tempdir(), paste0("cafri_match_", input_hash, "_",
                                     as.integer(Sys.time())))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  input_file    <- file.path(dir, "input.rds")
  result_file   <- file.path(dir, "result.rds")
  progress_file <- file.path(dir, "progress.json")
  cancel_file   <- file.path(dir, "cancel")

  saveRDS(
    list(
      user_df         = user_df,
      col_name        = col_name,
      backbone        = backbone,
      min_similarity  = min_similarity,
      include_authors = include_authors,
      input_hash      = input_hash,
      rm_mode         = rm_mode,
      checkpoint_file = checkpoint_file,
      cancel_file     = cancel_file,
      progress_file   = progress_file
    ),
    input_file
  )

  proc <- callr::r_bg(
    func = function(input_file, result_file) {
      CafriplotsR::.matching_worker(input_file, result_file)
    },
    args = list(input_file = input_file, result_file = result_file),
    # supervise: if the Shiny process dies, the worker must not outlive it and
    # sit on a CPU in a shared cluster.
    supervise = TRUE,
    stdout = file.path(dir, "stdout.log"),
    stderr = file.path(dir, "stderr.log")
  )

  list(
    proc          = proc,
    dir           = dir,
    input_file    = input_file,
    result_file   = result_file,
    progress_file = progress_file,
    cancel_file   = cancel_file,
    started_at    = Sys.time()
  )
}

#' Check on a running matching job
#'
#' @param job A job list from `.start_matching_job()`.
#' @return A list with `state`, one of:
#'   * `"running"` — plus `progress`, whatever the worker last published
#'   * `"done"` — plus `result`, the pipeline's return value
#'   * `"failed"` — plus `message`, including the worker's stderr if any
#' @keywords internal
.poll_matching_job <- function(job) {
  if (job$proc$is_alive()) {
    return(list(state = "running",
                progress = .read_matching_progress(job$progress_file)))
  }

  # The process is gone. Either it wrote a result, or it died trying.
  if (file.exists(job$result_file)) {
    result <- tryCatch(readRDS(job$result_file), error = function(e) NULL)
    if (!is.null(result)) {
      if (identical(result$status, "error")) {
        return(list(state = "failed", message = result$message))
      }
      return(list(state = "done", result = result))
    }
  }

  list(state = "failed", message = .matching_job_error(job))
}

#' Explain why a worker died without producing a result
#'
#' stderr first — a crashed R session says why there. An exit status alone is
#' what a killed process leaves behind, so it is the fallback, not the headline.
#'
#' @param job A job list from `.start_matching_job()`.
#' @return A character scalar.
#' @keywords internal
.matching_job_error <- function(job) {
  err <- tryCatch({
    f <- file.path(job$dir, "stderr.log")
    if (file.exists(f)) {
      paste(utils::tail(readLines(f, warn = FALSE), 20), collapse = "\n")
    } else {
      ""
    }
  }, error = function(e) "")

  if (nzchar(err)) return(err)

  status <- tryCatch(job$proc$get_exit_status(), error = function(e) NA_integer_)
  if (!is.na(status) && status != 0) {
    # 137 = SIGKILL, which inside a container is nearly always the OOM killer.
    if (identical(as.integer(status), 137L)) {
      return(paste(
        "The matching process was killed (exit 137), which usually means it ran",
        "out of memory. Try a smaller file, or raise the container memory limit."
      ))
    }
    return(paste0("The matching process exited with status ", status, "."))
  }

  "The matching process ended without producing a result."
}

#' Ask a running matching job to stop, then clean up after it
#'
#' Cancellation is cooperative first: the worker notices `cancel_file` between
#' two names and saves its checkpoint, so the run can be resumed later. `kill()`
#' is the backstop for a worker stuck inside a single long name.
#'
#' @param job A job list from `.start_matching_job()`, or NULL.
#' @param wait Numeric seconds to allow for a clean stop before killing.
#' @return Invisibly NULL.
#' @keywords internal
.cancel_matching_job <- function(job, wait = 2) {
  if (is.null(job)) return(invisible(NULL))

  tryCatch({
    file.create(job$cancel_file)
    job$proc$wait(timeout = wait * 1000)
    if (job$proc$is_alive()) job$proc$kill()
  }, error = function(e) NULL)

  invisible(NULL)
}

#' Remove a finished job's working directory
#'
#' @param job A job list from `.start_matching_job()`, or NULL.
#' @return Invisibly NULL.
#' @keywords internal
.cleanup_matching_job <- function(job) {
  if (is.null(job) || is.null(job$dir)) return(invisible(NULL))
  tryCatch(unlink(job$dir, recursive = TRUE), error = function(e) NULL)
  invisible(NULL)
}
