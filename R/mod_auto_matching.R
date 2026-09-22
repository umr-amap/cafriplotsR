# Auto Matching Module
#
# Automatically matches taxonomic names against the backbone database

# ---------------------------------------------------------------------------
# Checkpoint helpers
# ---------------------------------------------------------------------------

.checkpoint_dir <- function() {
  d <- file.path(tempdir(), "cafriplotsr_checkpoints")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

.checkpoint_path <- function(input_hash) {
  file.path(.checkpoint_dir(), paste0("taxo_chk_", input_hash, ".rds"))
}

# Simple hash: collision-unlikely for practical species lists
.compute_input_hash <- function(names) {
  sorted <- sort(unique(as.character(names[!is.na(names)])))
  sprintf("n%d_c%d_s%d",
          length(sorted),
          sum(nchar(sorted)),
          sum(utf8ToInt(substr(paste(sorted, collapse = ""), 1L, 500L))))
}

# `path` is explicit throughout because the matching pipeline may run in a
# background R process, which has its own tempdir() and would otherwise write
# its checkpoint somewhere this process can never find. The parent computes the
# path once and both ends use it. Passing NULL keeps the old local behaviour.
.save_matching_checkpoint <- function(input_hash, best_matches, fuzzy_results,
                                      still_unmatched, current_index, total_names,
                                      path = NULL) {
  if (is.null(path)) path <- .checkpoint_path(input_hash)
  saveRDS(
    list(
      input_hash     = input_hash,
      best_matches   = best_matches,
      fuzzy_results  = fuzzy_results,
      still_unmatched = still_unmatched,
      current_index  = current_index,
      total_names    = total_names,
      timestamp      = Sys.time()
    ),
    path
  )
}

.load_matching_checkpoint <- function(input_hash, path = NULL) {
  if (is.null(path)) path <- .checkpoint_path(input_hash)
  if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
}

.delete_matching_checkpoint <- function(input_hash, path = NULL) {
  if (is.null(path)) path <- .checkpoint_path(input_hash)
  if (file.exists(path)) file.remove(path)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Backbone shaping
# ---------------------------------------------------------------------------

#' Shape a freshly downloaded taxon table into the matching backbone
#'
#' @description
#' Drops the `"ZZ auct."` placeholder rows and derives the level keys the
#' matching stages join on. Split out from the download so the rule that
#' decides which taxa exist for the app is one testable thing rather than a
#' line buried in a Shiny observer.
#'
#' @param taxa Data frame, the collected `table_taxa` columns.
#'
#' @return The same data frame, filtered, with `tax_sp_level`,
#'   `tax_gen_level`, `tax_fam_level` and `tax_class_level` added.
#'
#' @keywords internal
.shape_backbone <- function(taxa) {
  taxa %>%
    # `author1 != "ZZ auct."` alone also discarded every taxon with no
    # recorded author, because NA propagates through `!=` and `filter()`
    # keeps only TRUE. Those taxa are perfectly valid - they simply have no
    # authorship on file - and dropping them made the app unable to find
    # names that `match_taxonomic_names()` matches exactly against the live
    # database, which applies no such filter.
    dplyr::filter(is.na(author1) | author1 != "ZZ auct.") %>%
    dplyr::mutate(
      tax_sp_level = dplyr::case_when(
        !is.na(tax_nam01) & tax_nam01 != "" ~ paste(tax_gen, tax_esp, tax_rank01, tax_nam01),
        !is.na(tax_esp) & tax_esp != "" ~ paste(tax_gen, tax_esp),
        TRUE ~ NA_character_
      ),
      tax_gen_level   = tax_gen,
      tax_fam_level   = tax_fam,
      tax_class_level = tax_famclass
    )
}

# ---------------------------------------------------------------------------
# Output column protection
# ---------------------------------------------------------------------------

# Columns the matching pipeline writes into the user's table. If an uploaded
# file already carries one of them (e.g. an `idtax_n` left over from a previous
# run), the join with the match results silently yields `idtax_n.x` /
# `idtax_n.y`, and every downstream step — review, export — then fails looking
# for the unsuffixed name.
.taxo_match_output_columns <- function() {
  c(
    "idtax_n", "idtax_good_n", "matched_name", "match_method", "match_score",
    "is_synonym", "accepted_name", "corrected_name",
    "backbone_taxon_name", "backbone_family", "backbone_authors",
    "backbone_status_raw",
    # Written only for WCVP, but reserved whatever the chosen backbone: a
    # table uploaded with a `wcvp_taxon_name` column must not decide, by
    # itself, which backbone the run may use.
    "wcvp_taxon_name", "wcvp_family", "wcvp_taxon_authors",
    "wcvp_taxon_status", "name_source"
  )
}

# ---------------------------------------------------------------------------
# Names from another backbone
# ---------------------------------------------------------------------------

# Which backbone the output names should come from. Anything that is not a
# usable code - no selector, a cleared input, a reactive that errors - means
# the internal backbone, which is what the app does without the option.
.chosen_name_backbone <- function(x) {
  if (is.null(x)) return("internal")
  code <- tryCatch(if (is.function(x)) x() else x, error = function(e) NULL)
  if (is.null(code) || length(code) != 1L || is.na(code) ||
      !nzchar(as.character(code))) {
    return("internal")
  }
  as.character(code)
}

# Join a backbone's names onto the matching output and let them stand in for
# `corrected_name` wherever the backbone has one. Kept apart from the module
# so the rules can be tested without a database or a session.
#
# `data` is the matching output, `info` the tibble from get_backbone_names().
.apply_backbone_names <- function(data, info, backbone) {
  value_cols <- c("backbone_taxon_name", "backbone_family",
                  "backbone_authors", "backbone_status_raw")
  alias_cols <- c("wcvp_taxon_name", "wcvp_family", "wcvp_taxon_authors",
                  "wcvp_taxon_status")

  if (is.null(info) || !is.data.frame(info) || nrow(info) == 0L ||
      !"idtax_n" %in% names(info) || !"idtax_n" %in% names(data)) {
    return(data)
  }

  info <- info[!duplicated(info$idtax_n),
               intersect(names(info), c("idtax_n", value_cols, "name_source")),
               drop = FALSE]

  # A second run must overwrite the first one's columns, not sit beside them
  # as `.x` and `.y`
  data <- dplyr::select(
    data, -dplyr::any_of(c(value_cols, alias_cols, "name_source"))
  )
  data <- dplyr::left_join(data, info, by = "idtax_n")

  for (col in value_cols) {
    if (!col %in% names(data)) data[[col]] <- NA_character_
  }
  if (!"name_source" %in% names(data)) data$name_source <- NA_character_
  data$name_source[is.na(data$name_source)] <- "internal"

  if ("corrected_name" %in% names(data)) {
    data$corrected_name <- ifelse(
      !is.na(data$backbone_taxon_name),
      data$backbone_taxon_name,
      data$corrected_name
    )
  }

  # WCVP keeps the column names it had before any other backbone existed, so
  # that scripts written against the old output still find them.
  if (identical(backbone, "wcvp")) {
    data$wcvp_taxon_name    <- data$backbone_taxon_name
    data$wcvp_family        <- data$backbone_family
    data$wcvp_taxon_authors <- data$backbone_authors
    data$wcvp_taxon_status  <- data$backbone_status_raw
  }

  data
}

# Park user columns that clash with the pipeline output under an `_input`
# suffix, so their content survives while the output names stay free.
# Returns the data plus a named vector old name -> new name.
.rename_conflicting_columns <- function(df) {
  clashing <- intersect(names(df), .taxo_match_output_columns())

  if (length(clashing) == 0) {
    return(list(data = df, renamed = character(0)))
  }

  taken <- names(df)
  new_names <- character(length(clashing))

  for (i in seq_along(clashing)) {
    candidate <- paste0(clashing[i], "_input")
    suffix <- 2L
    while (candidate %in% taken) {
      candidate <- paste0(clashing[i], "_input", suffix)
      suffix <- suffix + 1L
    }
    new_names[i] <- candidate
    taken <- c(taken, candidate)
  }

  names(df)[match(clashing, names(df))] <- new_names
  list(data = df, renamed = stats::setNames(new_names, clashing))
}

# ---------------------------------------------------------------------------

#' Auto Matching Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_auto_matching_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(shiny::textOutput(ns("title"))),

    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::numericInput(
          inputId = ns("min_similarity"),
          label = shiny::textOutput(ns("min_sim_label")),
          value = 60,
          min = 0,
          max = 100,
          step = 5
        ),
        shiny::helpText(shiny::textOutput(ns("min_sim_help")))
      ),
      shiny::column(
        width = 6,
        shiny::br(),
        shiny::uiOutput(ns("start_button"))
      )
    ),

    shiny::hr(),

    shiny::uiOutput(ns("matching_status")),
    shiny::uiOutput(ns("matching_summary"))
  )
}


#' Auto Matching Module - Server
#'
#' @param id Character, module ID
#' @param data Reactive data.frame from data input module
#' @param column_name Reactive character, name of column to match
#' @param include_authors Reactive logical, whether to include author names
#' @param min_similarity Numeric (0-1), minimum similarity threshold for fallback.
#'   Note: UI displays as percentage (0-100) but parameter uses decimal (default: 0.3 = 30\%)
#' @param i18n Reactive returning shiny.i18n translator
#' @param name_backbone Reactive returning the code of the backbone whose
#'   names should appear in the output, or \code{"internal"} (the default) to
#'   keep the internal ones. See \code{list_backbones()}.
#'
#' @return Reactive list containing:
#'   \itemize{
#'     \item \code{data}: Updated data frame with match results
#'     \item \code{unmatched}: Data frame of unmatched names
#'     \item \code{stats}: List of matching statistics
#'     \item \code{params}: Settings used by the last run (column, similarity
#'       threshold, author matching, name backbone, offline flag)
#'   }
#'
#' @keywords internal
mod_auto_matching_server <- function(id, data, column_name, include_authors,
                                     min_similarity = 0.3, i18n,
                                     name_backbone = NULL,
                                     is_offline = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    matched_data        <- shiny::reactiveVal(NULL)
    match_stats         <- shiny::reactiveVal(NULL)
    matching_in_progress <- shiny::reactiveVal(FALSE)

    # Settings actually used by the last run — captured here rather than read
    # back from the inputs, which the user may have changed since. Consumed by
    # the R-code preview so the generated script matches what was run.
    run_params          <- shiny::reactiveVal(NULL)

    # Checkpoint / resume state
    resume_mode         <- shiny::reactiveVal(NULL)   # "resume" | "fresh"
    pending_input_hash  <- shiny::reactiveVal(NULL)
    trigger_cache_modal <- shiny::reactiveVal(NULL)

    # Handle on the background matching process, when there is one. NULL both
    # before a run and after it finishes, which is what the polling observer
    # below keys off.
    match_job <- shiny::reactiveVal(NULL)

    # Last progress published by the pipeline, shown in the status panel.
    match_progress <- shiny::reactiveVal(NULL)

    # One display for both run modes. The notification is what an in-process
    # run relies on: it is sent to the browser immediately, whereas the status
    # panel cannot re-render until the blocking run has returned.
    show_matching_progress <- function(p) {
      if (is.null(p) || is.null(p$stage)) return(invisible(NULL))

      msg <- switch(
        p$stage,
        fuzzy = paste0(
          i18n()$t("Fuzzy matching:"), " ", p$i, " / ", p$n,
          if (!is.null(p$name) && !is.na(p$name)) paste0(" (", p$name, ")")
        ),
        fuzzy_start = paste0(
          i18n()$t("Starting fuzzy matching for"), " ", p$n, " ",
          i18n()$t("unmatched name(s)... This may take some time.")
        ),
        resume = paste0(i18n()$t("Resuming from name"), " ", p$i, " / ", p$n),
        NULL
      )
      if (is.null(msg)) return(invisible(NULL))

      shiny::showNotification(
        msg,
        duration = if (identical(p$stage, "resume")) 4 else NULL,
        closeButton = FALSE,
        id = "fuzzy_progress",
        type = "message"
      )
      match_progress(msg)
      invisible(NULL)
    }

    # Put the module back in a state where the Start button works again.
    reset_matching_state <- function() {
      shinybusy::hide_spinner()
      match_progress(NULL)
      matching_in_progress(FALSE)
      resume_mode(NULL)
      pending_input_hash(NULL)
      trigger_cache_modal(NULL)
      cache_choice(NULL)
    }

    # Everything that happens once the pipeline returns — whether it ran here
    # or in a worker process. The backbone lookup lives here rather than in the
    # pipeline because it needs a database connection, which does not survive
    # the trip into another process.
    apply_matching_result <- function(result) {
      if (is.null(result)) {
        result <- list(status = "error",
                       message = "The matching produced no result.")
      }

      if (identical(result$status, "empty")) {
        shiny::showNotification(i18n()$t("No data loaded"), type = "warning")
        reset_matching_state()
        return(invisible(NULL))
      }

      if (identical(result$status, "cancelled")) {
        # The checkpoint is deliberately left in place: cancelling is how a
        # user parks a long run, and the next Start offers to resume it.
        shiny::showNotification(
          i18n()$t("Matching cancelled. Your progress was saved - start again to resume it."),
          type = "warning",
          duration = 6
        )
        reset_matching_state()
        return(invisible(NULL))
      }

      if (identical(result$status, "error")) {
        shiny::showNotification(
          paste(i18n()$t("Error:"), result$message),
          type = "error",
          duration = 10
        )
        reset_matching_state()
        return(invisible(NULL))
      }

      updated_data <- result$updated_data
      match_stats(result$stats)

      # --- Optional names from another backbone ---
      chosen <- .chosen_name_backbone(name_backbone)

      if (!identical(chosen, "internal")) {
        matched_ids <- unique(stats::na.omit(updated_data$idtax_n))
        label <- .backbone_display_name(chosen)

        if (length(matched_ids) > 0) {
          shiny::showNotification(
            paste0(i18n()$t("Fetching names from"), " ", label, "..."),
            id       = "backbone_fetch",
            duration = NULL,
            type     = "message"
          )

          backbone_info <- tryCatch(
            get_backbone_names(matched_ids, chosen),
            error = function(e) {
              message("Could not fetch names from ", chosen, ": ", e$message)
              NULL
            }
          )

          shiny::removeNotification("backbone_fetch")

          if (!is.null(backbone_info)) {
            updated_data <- .apply_backbone_names(updated_data, backbone_info,
                                                  chosen)

            n_replaced <- sum(!is.na(updated_data$backbone_taxon_name),
                              na.rm = TRUE)
            shiny::showNotification(
              paste0(
                format(n_replaced, big.mark = ","), " ",
                i18n()$t("names replaced with names from"), " ", label
              ),
              duration = 4,
              type     = "message"
            )
          } else {
            shiny::showNotification(
              paste0(label, " ",
                     i18n()$t("names not available. Internal names used.")),
              duration = 5,
              type     = "warning"
            )
          }
        }
      }

      matched_data(updated_data)

      shinybusy::hide_spinner()
      matching_in_progress(FALSE)

      # Reset resume state
      resume_mode(NULL)
      pending_input_hash(NULL)
      trigger_cache_modal(NULL)
      cache_choice(NULL)

      shiny::showNotification(
        i18n()$t("Matching complete!"),
        type = "message",
        duration = 3
      )
    }

    # -----------------------------------------------------------------------
    # Watching the background matching process
    # -----------------------------------------------------------------------
    # A process has no reactive identity, so it has to be polled.
    # invalidateLater at 600 ms is the compromise: often enough that the name
    # counter looks live, rare enough to be nothing against a run measured in
    # minutes. When match_job() is NULL the req() below stops the observer and
    # no timer is scheduled at all.
    shiny::observe({
      job <- match_job()
      shiny::req(job)

      shiny::invalidateLater(600, session)

      status <- .poll_matching_job(job)

      if (identical(status$state, "running")) {
        # isolate: match_progress() is written here, and must not re-trigger
        # this observer on top of its own timer.
        shiny::isolate(show_matching_progress(status$progress))
        return(invisible(NULL))
      }

      # Finished, one way or another. Clear the handle first so that nothing
      # below can schedule another poll.
      match_job(NULL)
      match_progress(NULL)
      shiny::removeNotification("fuzzy_progress")

      if (identical(status$state, "done")) {
        apply_matching_result(status$result)
      } else {
        apply_matching_result(list(status = "error", message = status$message))
      }

      .cleanup_matching_job(job)
    })

    # Cancel button (background runs only — see output$start_button)
    shiny::observeEvent(input$cancel_matching, {
      job <- match_job()
      shiny::req(job)

      .cancel_matching_job(job)
      match_job(NULL)
      shiny::removeNotification("fuzzy_progress")
      .cleanup_matching_job(job)

      shiny::showNotification(
        i18n()$t("Matching cancelled. Your progress was saved - start again to resume it."),
        type = "warning",
        duration = 6
      )
      reset_matching_state()
    })

    # A worker started for a session nobody is watching any more is pure waste
    # on a shared cluster. supervise = TRUE in .start_matching_job() covers the
    # whole R process dying; this covers one visitor closing one tab.
    session$onSessionEnded(function() {
      tryCatch(
        .cancel_matching_job(shiny::isolate(match_job())),
        error = function(e) NULL
      )
    })

    # Cache selection module — triggered after resume choice is made
    cache_choice <- mod_backbone_cache_selection_server(
      id = "backbone_cache",
      i18n = i18n,
      trigger = shiny::reactive(trigger_cache_modal())
    )

    # Reset results when data changes
    shiny::observe({
      data()  # Trigger on data change

      matched_data(NULL)
      match_stats(NULL)
      matching_in_progress(FALSE)
      resume_mode(NULL)
      pending_input_hash(NULL)
      run_params(NULL)
    })

    # Module title
    output$title <- shiny::renderText({
      i18n()$t("Automatic Matching")
    })

    # Labels
    output$min_sim_label <- shiny::renderText({
      i18n()$t("Minimum similarity (%):")
    })

    output$min_sim_help <- shiny::renderText({
      i18n()$t("Minimum similarity percentage for fuzzy matching. Names with similarity below this threshold will not be matched. Higher values = more strict matching (fewer but more accurate matches).")
    })

    # Start button
    output$start_button <- shiny::renderUI({
      ns <- session$ns

      req(data())
      req(column_name())

      if (matching_in_progress()) {
        shiny::div(
          shinybusy::use_busy_spinner(spin = "fading-circle"),
          shiny::p(i18n()$t("Matching in progress..."), style = "color: blue;"),
          # Offered only for a background run. An in-process run could not
          # service the click until it had already finished, so a button there
          # would do nothing but mislead.
          if (!is.null(match_job())) {
            shiny::actionButton(
              inputId = ns("cancel_matching"),
              label = i18n()$t("Cancel"),
              class = "btn-warning btn-sm",
              icon = shiny::icon("stop")
            )
          }
        )
      } else {
        shiny::actionButton(
          inputId = ns("start_matching"),
          label = i18n()$t("Start Matching"),
          class = "btn-primary",
          icon = shiny::icon("play")
        )
      }
    })

    # -----------------------------------------------------------------------
    # Step 1 — button click: check for checkpoint, show resume modal if found
    # -----------------------------------------------------------------------
    shiny::observeEvent(input$start_matching, {
      req(data(), column_name())

      matching_in_progress(TRUE)
      shinybusy::show_spinner()

      # Compute hash for the current input
      raw_names <- data() %>%
        dplyr::pull(!!rlang::sym(column_name())) %>%
        unique()
      h <- .compute_input_hash(raw_names)
      pending_input_hash(h)

      # Check for an existing checkpoint
      chk <- .load_matching_checkpoint(h)

      if (!is.null(chk)) {
        pct_done <- round(chk$current_index / max(length(chk$still_unmatched), 1L) * 100)
        shiny::showModal(shiny::modalDialog(
          title = i18n()$t("Interrupted matching found"),
          shiny::p(
            i18n()$t("An interrupted matching session was found for this dataset:"),
            shiny::br(),
            shiny::strong(sprintf(
              "%d / %d %s (%d%%)",
              chk$current_index,
              length(chk$still_unmatched),
              i18n()$t("names processed by fuzzy matching"),
              pct_done
            )),
            shiny::br(),
            shiny::em(format(chk$timestamp, "%Y-%m-%d %H:%M"))
          ),
          shiny::p(i18n()$t("Do you want to resume where you left off, or start fresh?")),
          footer = shiny::tagList(
            shiny::actionButton(
              session$ns("btn_resume_matching"),
              label = shiny::tagList(shiny::icon("play-circle"), i18n()$t("Resume")),
              class = "btn-primary"
            ),
            shiny::actionButton(
              session$ns("btn_fresh_matching"),
              label = shiny::tagList(shiny::icon("redo"), i18n()$t("Start Fresh")),
              class = "btn-warning"
            )
          ),
          easyClose = FALSE
        ))
      } else {
        # No checkpoint — go straight to cache selection
        resume_mode("fresh")
      }
    })

    # Resume modal: user chose "Resume"
    shiny::observeEvent(input$btn_resume_matching, {
      shiny::removeModal()
      resume_mode("resume")
    })

    # Resume modal: user chose "Start Fresh"
    shiny::observeEvent(input$btn_fresh_matching, {
      shiny::removeModal()
      h <- pending_input_hash()
      if (!is.null(h)) .delete_matching_checkpoint(h)
      resume_mode("fresh")
    })

    # Once resume choice is made, open the backbone cache modal — except in
    # offline mode, where "download fresh" is not an option, so auto-pick the
    # cache without prompting.
    shiny::observeEvent(resume_mode(), {
      req(resume_mode())
      if (isTRUE(is_offline())) {
        cache_choice("cache")
      } else {
        trigger_cache_modal((trigger_cache_modal() %||% 0L) + 1L)
      }
    })

    # -----------------------------------------------------------------------
    # Step 2 — backbone loaded: run matching (fresh or resumed)
    # -----------------------------------------------------------------------
    shiny::observeEvent(cache_choice(), {
      req(cache_choice(), resume_mode(), pending_input_hash())

      choice     <- cache_choice()
      input_hash <- pending_input_hash()
      rm_mode    <- resume_mode()

      # --- Load backbone (always needed for synonym resolution) ---
      backbone <- NULL

      if (choice == "cache") {
        shiny::showNotification(
          i18n()$t("Loading taxonomic backbone from cache..."),
          duration = 3,
          id = "loading_cache",
          type = "message"
        )

        backbone <- load_backbone_cache()

        if (is.null(backbone)) {
          shiny::removeNotification("loading_cache")
          if (isTRUE(is_offline())) {
            # Offline mode: cannot fall back to download — abort with a clear msg
            shiny::showNotification(
              i18n()$t("Cached backbone is invalid. Please connect online to refresh it."),
              duration = 8,
              type = "error"
            )
            shinybusy::hide_spinner()
            matching_in_progress(FALSE)
            resume_mode(NULL)
            pending_input_hash(NULL)
            trigger_cache_modal(NULL)
            cache_choice(NULL)
            return(NULL)
          }
          shiny::showNotification(
            i18n()$t("Cache load failed, downloading fresh backbone..."),
            duration = 5,
            type = "warning"
          )
          choice <- "download"
        } else {
          shiny::removeNotification("loading_cache")
          shiny::showNotification(
            i18n()$t("Loaded backbone from cache successfully!"),
            duration = 3,
            type = "message"
          )
        }
      }

      if (choice == "download") {
        shiny::showNotification(
          i18n()$t("Downloading taxonomic backbone from database... This may take a moment."),
          duration = NULL,
          closeButton = FALSE,
          id = "download_backbone",
          type = "message"
        )

        mydb_taxa <- call.mydb.taxa()

        backbone <- try_open_postgres_table(table = "table_taxa", con = mydb_taxa) %>%
          dplyr::select(
            idtax_n,
            idtax_good_n,
            tax_fam,
            tax_famclass,
            tax_gen,
            tax_esp,
            tax_rank01,
            tax_nam01,
            tax_rank02,
            tax_nam02,
            tax_level,
            author1
          ) %>%
          dplyr::collect()

        shiny::removeNotification("download_backbone")

        backbone <- .shape_backbone(backbone)

        shiny::showNotification(
          i18n()$t("Caching backbone for future use..."),
          duration = 2,
          type = "message"
        )
        save_backbone_cache(backbone)
        shiny::showNotification(
          i18n()$t("Backbone cached successfully!"),
          duration = 2,
          type = "message"
        )
      }

      user_df      <- data()
      col_name     <- column_name()
      incl_authors <- include_authors() %||% FALSE
      min_sim <- if (!is.null(input$min_similarity)) {
        input$min_similarity / 100
      } else {
        min_similarity
      }

      run_params(list(
        column          = col_name,
        include_authors = incl_authors,
        min_similarity  = min_sim,
        name_backbone   = .chosen_name_backbone(name_backbone),
        is_offline      = isTRUE(is_offline())
      ))

      # Computed here, in the parent, so that a worker process writes its
      # checkpoint where this process will look for it on the next run.
      checkpoint_file <- .checkpoint_path(input_hash)

      # --- Hand the computation to a background process ------------------
      # The matching itself is pure (see .run_matching_pipeline()), so it can
      # run anywhere. Running it *here* would block the single R worker that
      # Shiny Server shares between every visitor and the health probes, which
      # is what used to freeze other sessions and get the pod restarted
      # mid-run. When a worker cannot be started we still run in-process,
      # because a slow answer beats no answer.
      if (.async_matching_available()) {
        job <- tryCatch(
          .start_matching_job(
            user_df         = user_df,
            col_name        = col_name,
            backbone        = backbone,
            min_similarity  = min_sim,
            include_authors = incl_authors,
            input_hash      = input_hash,
            rm_mode         = rm_mode,
            checkpoint_file = checkpoint_file
          ),
          error = function(e) {
            message("Could not start a matching worker (", conditionMessage(e),
                    "). Falling back to in-process matching.")
            NULL
          }
        )

        if (!is.null(job)) {
          match_job(job)
          # The polling observer below owns the rest of this run.
          return(invisible(NULL))
        }
      }

      # In-process fallback: this blocks until done (e.g. under
      # devtools::load_all(), where no worker can be started). It used to pass
      # progress = NULL, so a long run showed nothing but "Processing...".
      # Notifications still reach the browser mid-run; throttled so a fast
      # stretch of names does not flood the websocket.
      last_sent <- Sys.time() - 10
      inprocess_progress <- function(stage, i = NA_integer_, n = NA_integer_,
                                     name = NA_character_) {
        now <- Sys.time()
        if (identical(stage, "fuzzy") && i < n &&
            as.numeric(difftime(now, last_sent, units = "secs")) < 0.5) {
          return(invisible(NULL))
        }
        last_sent <<- now
        show_matching_progress(list(stage = stage, i = i, n = n, name = name))
      }

      on.exit({
        shiny::removeNotification("fuzzy_progress")
        match_progress(NULL)
      }, add = TRUE)

      apply_matching_result(
        tryCatch(
          .run_matching_pipeline(
            user_df         = user_df,
            col_name        = col_name,
            backbone        = backbone,
            min_similarity  = min_sim,
            include_authors = incl_authors,
            input_hash      = input_hash,
            rm_mode         = rm_mode,
            checkpoint_file = checkpoint_file,
            cancel_file     = NULL,
            progress        = inprocess_progress
          ),
          error = function(e) list(status = "error", message = conditionMessage(e))
        )
      )
    })

    # Matching status
    output$matching_status <- shiny::renderUI({
      if (matching_in_progress()) {
        shiny::div(
          style = "padding: 10px; background-color: #d1ecf1; border-radius: 5px;",
          shiny::p(
            shiny::icon("spinner", class = "fa-spin"),
            i18n()$t("Processing..."),
            style = "color: #0c5460;"
          ),
          if (!is.null(match_progress())) {
            shiny::p(match_progress(), style = "color: #0c5460; margin: 0;")
          }
        )
      }
    })

    # Matching summary
    output$matching_summary <- shiny::renderUI({
      req(match_stats())

      stats <- match_stats()

      shiny::div(
        style = "padding: 15px; background-color: #d4edda; border-radius: 5px; margin-top: 10px;",
        shiny::h4(i18n()$t("Matching Summary")),

        shiny::tags$ul(
          shiny::tags$li(
            shiny::strong(i18n()$t("Total unique names:")),
            stats$total_names
          ),
          shiny::tags$li(
            shiny::strong(i18n()$t("Exact matches:")),
            paste0(stats$n_exact, " (",
                  round(stats$n_exact / stats$total_names * 100, 1), "%)")
          ),
          shiny::tags$li(
            shiny::strong(i18n()$t("Genus-level matches:")),
            paste0(stats$n_genus, " (",
                  round(stats$n_genus / stats$total_names * 100, 1), "%)")
          ),
          shiny::tags$li(
            shiny::strong(i18n()$t("Fuzzy matches:")),
            paste0(stats$n_fuzzy, " (",
                  round(stats$n_fuzzy / stats$total_names * 100, 1), "%)")
          ),
          shiny::tags$li(
            style = if (stats$n_unmatched > 0) "color: orange; font-weight: bold;" else "",
            shiny::strong(i18n()$t("Requiring review:")),
            paste0(stats$n_unmatched, " (",
                  round(stats$n_unmatched / stats$total_names * 100, 1), "%)")
          )
        ),

        if (stats$n_unmatched > 0) {
          shiny::p(
            shiny::icon("info-circle"),
            i18n()$t("Go to the Review tab to manually review unmatched names."),
            style = "margin-top: 10px; color: #856404;"
          )
        }
      )
    })

    # Return reactive results
    return(
      shiny::reactive({
        req(matched_data())
        req(match_stats())

        col_name <- column_name()
        unmatched <- matched_data() %>%
          dplyr::filter(
            is.na(idtax_n),
            !.is_missing_name(!!rlang::sym(col_name))
          ) %>%
          dplyr::distinct(!!rlang::sym(col_name)) %>%
          dplyr::pull(!!rlang::sym(col_name))

        list(
          data      = matched_data(),
          unmatched = unmatched,
          stats     = match_stats(),
          params    = run_params()
        )
      })
    )
  })
}
