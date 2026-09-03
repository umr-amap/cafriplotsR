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

.save_matching_checkpoint <- function(input_hash, best_matches, fuzzy_results,
                                      still_unmatched, current_index, total_names) {
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
    .checkpoint_path(input_hash)
  )
}

.load_matching_checkpoint <- function(input_hash) {
  path <- .checkpoint_path(input_hash)
  if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
}

.delete_matching_checkpoint <- function(input_hash) {
  path <- .checkpoint_path(input_hash)
  if (file.exists(path)) file.remove(path)
  invisible(NULL)
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
    "wcvp_taxon_name", "wcvp_family", "wcvp_taxon_authors",
    "wcvp_taxon_status", "name_source"
  )
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
#'
#' @return Reactive list containing:
#'   \itemize{
#'     \item \code{data}: Updated data frame with match results
#'     \item \code{unmatched}: Data frame of unmatched names
#'     \item \code{stats}: List of matching statistics
#'   }
#'
#' @keywords internal
mod_auto_matching_server <- function(id, data, column_name, include_authors,
                                     min_similarity = 0.3, i18n,
                                     use_wcvp_names = NULL,
                                     is_offline = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    matched_data        <- shiny::reactiveVal(NULL)
    match_stats         <- shiny::reactiveVal(NULL)
    matching_in_progress <- shiny::reactiveVal(FALSE)

    # Checkpoint / resume state
    resume_mode         <- shiny::reactiveVal(NULL)   # "resume" | "fresh"
    pending_input_hash  <- shiny::reactiveVal(NULL)
    trigger_cache_modal <- shiny::reactiveVal(NULL)

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
          shiny::p(i18n()$t("Matching in progress..."), style = "color: blue;")
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
          dplyr::collect() %>%
          dplyr::filter(author1 != "ZZ auct.")

        shiny::removeNotification("download_backbone")

        backbone <- backbone %>%
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

      tryCatch({
        user_df      <- data()
        col_name     <- column_name()
        incl_authors <- include_authors() %||% FALSE
        min_sim <- if (!is.null(input$min_similarity)) {
          input$min_similarity / 100
        } else {
          min_similarity
        }

        # --- Decide: restore checkpoint or run exact matching from scratch ---

        best_matches    <- NULL
        fuzzy_results   <- list()
        still_unmatched <- character(0)
        start_idx       <- 1L
        total_names     <- 0L

        if (rm_mode == "resume") {
          chk <- .load_matching_checkpoint(input_hash)
          if (!is.null(chk)) {
            best_matches    <- chk$best_matches
            fuzzy_results   <- chk$fuzzy_results
            still_unmatched <- chk$still_unmatched
            total_names     <- chk$total_names
            start_idx       <- chk$current_index + 1L

            shiny::showNotification(
              paste0(
                i18n()$t("Resuming from name"),
                " ", start_idx, " / ", length(still_unmatched)
              ),
              duration = 4,
              type = "message"
            )
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
            shiny::showNotification(
              i18n()$t("No data loaded"),
              type = "warning"
            )
            matching_in_progress(FALSE)
            shinybusy::hide_spinner()
            resume_mode(NULL)
            pending_input_hash(NULL)
            trigger_cache_modal(NULL)
            cache_choice(NULL)
            return(NULL)
          }

          cleaned_names <- sapply(unique_names_to_match, clean_taxonomic_name)

          input_df <- data.frame(
            input_name   = unique_names_to_match,
            cleaned_name = cleaned_names,
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

          # STEP 4: Batch exact — genus level
          unmatched_after_species <- matches_species %>%
            dplyr::filter(is.na(idtax_n)) %>%
            dplyr::select(input_name, cleaned_name)

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

          # STEP 5: Batch exact — family level
          unmatched_after_genus <- matches_genus %>%
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
          matches_species <- matches_species %>%
            dplyr::rows_update(
              matches_genus %>% dplyr::filter(!is.na(idtax_n)),
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
          shiny::showNotification(
            paste0(
              i18n()$t("Starting fuzzy matching for"),
              " ", length(still_unmatched), " ",
              i18n()$t("unmatched name(s)... This may take some time.")
            ),
            duration = NULL,
            closeButton = FALSE,
            id = "fuzzy_matching",
            type = "message"
          )

          for (i in start_idx:length(still_unmatched)) {
            # Flush httpuv's pending event queue so the browser-disconnect event
            # can be processed mid-loop (otherwise session$isEnded() stays FALSE
            # because Shiny's event loop is blocked by this synchronous for loop).
            tryCatch(later::run_now(timeoutSecs = 0), error = function(e) NULL)
            if (session$isEnded()) break

            name <- still_unmatched[i]

            shiny::showNotification(
              paste0(
                i18n()$t("Fuzzy matching:"), " ", i,
                " / ", length(still_unmatched),
                " (", name, ")"
              ),
              duration = 2,
              id = "fuzzy_progress",
              type = "message"
            )

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

            # Persist progress — survives laptop sleep / crash
            .save_matching_checkpoint(
              input_hash, best_matches, fuzzy_results,
              still_unmatched, i, total_names
            )
          }

          shiny::removeNotification("fuzzy_matching")
          shiny::removeNotification("fuzzy_progress")

          if (session$isEnded()) return(NULL)

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
        .delete_matching_checkpoint(input_hash)

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

        match_stats(stats)

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

        # --- Optional WCVP enrichment ---
        if (isTRUE(!is.null(use_wcvp_names) && use_wcvp_names())) {
          matched_ids <- unique(stats::na.omit(updated_data$idtax_n))

          if (length(matched_ids) > 0) {
            shiny::showNotification(
              i18n()$t("Fetching WCVP names..."),
              id       = "wcvp_fetch",
              duration = NULL,
              type     = "message"
            )

            wcvp_info <- tryCatch(
              get_wcvp_names(matched_ids),
              error = function(e) {
                message("Could not fetch WCVP names: ", e$message)
                NULL
              }
            )

            shiny::removeNotification("wcvp_fetch")

            if (!is.null(wcvp_info)) {
              updated_data <- updated_data %>%
                dplyr::left_join(
                  wcvp_info %>%
                    dplyr::select(
                      idtax_n, wcvp_taxon_name, wcvp_family,
                      wcvp_taxon_authors, wcvp_taxon_status, name_source
                    ),
                  by = "idtax_n"
                ) %>%
                dplyr::mutate(
                  corrected_name = dplyr::if_else(
                    !is.na(wcvp_taxon_name), wcvp_taxon_name, corrected_name
                  ),
                  name_source = dplyr::coalesce(name_source, "internal")
                )

              n_wcvp <- sum(!is.na(updated_data$wcvp_taxon_name), na.rm = TRUE)
              shiny::showNotification(
                paste0(
                  format(n_wcvp, big.mark = ","), " ",
                  i18n()$t("names replaced with WCVP names")
                ),
                duration = 4,
                type     = "message"
              )
            } else {
              shiny::showNotification(
                i18n()$t("WCVP names not available. Internal names used."),
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

      }, error = function(e) {
        shinybusy::hide_spinner()
        matching_in_progress(FALSE)
        resume_mode(NULL)
        pending_input_hash(NULL)

        shiny::showNotification(
          paste(i18n()$t("Error:"), e$message),
          type = "error",
          duration = 10
        )
      })
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
          )
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
            !!rlang::sym(col_name) != ""
          ) %>%
          dplyr::distinct(!!rlang::sym(col_name)) %>%
          dplyr::pull(!!rlang::sym(col_name))

        list(
          data      = matched_data(),
          unmatched = unmatched,
          stats     = match_stats()
        )
      })
    )
  })
}
