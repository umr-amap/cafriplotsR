# Auto Matching Module
#
# Automatically matches taxonomic names against the backbone database

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
          value = 0.3,
          min = 0,
          max = 1,
          step = 0.05
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
#' @param min_similarity Numeric, minimum similarity threshold (default: 0.3)
#' @param language Reactive returning current language ("en" or "fr")
#'
#' @return Reactive list containing:
#'   - data: Updated data frame with match results
#'   - unmatched: Data frame of unmatched names
#'   - stats: List of matching statistics
#'
#' @keywords internal
mod_auto_matching_server <- function(id, data, column_name, include_authors,
                                     min_similarity = 0.3, language = shiny::reactive("en")) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    matched_data <- shiny::reactiveVal(NULL)
    match_stats <- shiny::reactiveVal(NULL)
    matching_in_progress <- shiny::reactiveVal(FALSE)

    # Reset results when data changes
    shiny::observe({
      data()  # Trigger on data change

      # Reset all reactive values
      matched_data(NULL)
      match_stats(NULL)
      matching_in_progress(FALSE)
    })

    # Get translations
    t <- shiny::reactive({
      get_translations(language())
    })

    # Module title
    output$title <- shiny::renderText({
      t()$auto_match_title
    })

    # Labels
    output$min_sim_label <- shiny::renderText({
      t()$auto_match_min_sim
    })

    output$min_sim_help <- shiny::renderText({
      t()$auto_match_min_sim_help
    })

    # Start button
    output$start_button <- shiny::renderUI({
      ns <- session$ns

      req(data())
      req(column_name())

      if (matching_in_progress()) {
        shiny::div(
          shinybusy::use_busy_spinner(spin = "fading-circle"),
          shiny::p(t()$auto_match_running, style = "color: blue;")
        )
      } else {
        shiny::actionButton(
          inputId = ns("start_matching"),
          label = t()$auto_match_start,
          class = "btn-primary",
          icon = shiny::icon("play")
        )
      }
    })

    # Matching logic
    shiny::observeEvent(input$start_matching, {

      req(data())
      req(column_name())

      matching_in_progress(TRUE)
      shinybusy::show_spinner()

      tryCatch({
        # Get user data
        user_df <- data()
        col_name <- column_name()
        incl_authors <- include_authors() %||% FALSE
        min_sim <- input$min_similarity %||% min_similarity

        # Extract unique names from selected column
        unique_names <- user_df %>%
          dplyr::pull(!!rlang::sym(col_name)) %>%
          unique() %>%
          .[!is.na(.)]

        total_names <- length(unique_names)

        if (total_names == 0) {
          shiny::showNotification(
            t()$msg_no_data,
            type = "warning"
          )
          matching_in_progress(FALSE)
          shinybusy::hide_spinner()
          return(NULL)
        }

        # STEP 1: Download entire taxonomic backbone once
        mydb_taxa <- call.mydb.taxa()

        # Show message about downloading backbone (can be slow)
        shiny::showNotification(
          "Downloading taxonomic backbone from database... This may take a moment, especially with slow internet connection.",
          duration = NULL,  # Stays until dismissed
          closeButton = FALSE,
          id = "download_backbone",
          type = "message"
        )

        backbone <- dplyr::tbl(mydb_taxa, "table_taxa") %>%
          dplyr::select(
            idtax_n,
            idtax_good_n,
            tax_fam,
            tax_gen,
            tax_esp,
            tax_rank01,
            tax_nam01,
            tax_rank02,
            tax_nam02,
            tax_level
          ) %>%
          dplyr::collect()

        # Remove the download notification
        shiny::removeNotification("download_backbone")

        # Create formatted name columns for matching
        backbone <- backbone %>%
          dplyr::mutate(
            # Full species-level name (genus + species + infraspecific)
            tax_sp_level = dplyr::case_when(
              !is.na(tax_nam01) & tax_nam01 != "" ~ paste(tax_gen, tax_esp, tax_rank01, tax_nam01),
              !is.na(tax_esp) & tax_esp != "" ~ paste(tax_gen, tax_esp),
              TRUE ~ NA_character_
            ),
            # Genus-level name (just genus)
            tax_gen_level = tax_gen,
            # Family-level name (just family)
            tax_fam_level = tax_fam
          )

        # STEP 2: Prepare input names for batch matching
        input_df <- data.frame(
          input_name = unique_names,
          stringsAsFactors = FALSE
        )

        # STEP 3: Batch exact matching on species level (where unique)
        # First identify unique species names in backbone
        unique_species <- backbone %>%
          dplyr::filter(!is.na(tax_sp_level)) %>%
          dplyr::group_by(tax_sp_level) %>%
          dplyr::filter(dplyr::n() == 1) %>%  # Only unique matches
          dplyr::ungroup() %>%
          dplyr::select(
            tax_sp_level,
            idtax_n,
            idtax_good_n,
            tax_fam,
            tax_gen,
            tax_esp,
            tax_rank01,
            tax_nam01
          ) %>%
          dplyr::mutate(
            matched_name = tax_sp_level,
            match_method = "exact",
            match_score = 1.0
          )

        # Match input names to unique species
        matches_species <- input_df %>%
          dplyr::left_join(
            unique_species,
            by = c("input_name" = "tax_sp_level")
          )

        # STEP 4: Batch exact matching on genus level (where unique, for unmatched)
        unmatched_after_species <- matches_species %>%
          dplyr::filter(is.na(idtax_n)) %>%
          dplyr::select(input_name)

        unique_genera <- backbone %>%
          dplyr::filter(tax_level == "genus", !is.na(tax_gen_level)) %>%  # Genus-level taxa
          dplyr::group_by(tax_gen_level) %>%
          dplyr::filter(dplyr::n() == 1) %>%  # Only unique matches
          dplyr::ungroup() %>%
          dplyr::select(
            tax_gen_level,
            idtax_n,
            idtax_good_n,
            tax_fam,
            tax_gen
          ) %>%
          dplyr::mutate(
            matched_name = tax_gen_level,
            match_method = "exact",
            match_score = 1.0
          )

        matches_genus <- unmatched_after_species %>%
          dplyr::left_join(
            unique_genera,
            by = c("input_name" = "tax_gen_level")
          )

        # STEP 5: Batch exact matching on family level (where unique, for unmatched)
        unmatched_after_genus <- matches_genus %>%
          dplyr::filter(is.na(idtax_n)) %>%
          dplyr::select(input_name)

        unique_families <- backbone %>%
          dplyr::filter(tax_level == "family", !is.na(tax_fam_level)) %>%  # Family-level taxa
          dplyr::group_by(tax_fam_level) %>%
          dplyr::filter(dplyr::n() == 1) %>%  # Only unique matches
          dplyr::ungroup() %>%
          dplyr::select(
            tax_fam_level,
            idtax_n,
            idtax_good_n,
            tax_fam
          ) %>%
          dplyr::mutate(
            matched_name = tax_fam_level,
            match_method = "exact",
            match_score = 1.0
          )

        matches_family <- unmatched_after_genus %>%
          dplyr::left_join(
            unique_families,
            by = c("input_name" = "tax_fam_level")
          )

        # STEP 6: Combine all batch exact matches
        # Update matches_species with genus matches
        matches_species <- matches_species %>%
          dplyr::rows_update(
            matches_genus %>% dplyr::filter(!is.na(idtax_n)),
            by = "input_name",
            unmatched = "ignore"
          )

        # Update with family matches
        matches_species <- matches_species %>%
          dplyr::rows_update(
            matches_family %>% dplyr::filter(!is.na(idtax_n)),
            by = "input_name",
            unmatched = "ignore"
          )

        best_matches <- matches_species

        # STEP 7: Fuzzy matching for remaining unmatched names
        still_unmatched <- best_matches %>%
          dplyr::filter(is.na(idtax_n)) %>%
          dplyr::pull(input_name)

        if (length(still_unmatched) > 0) {
          # Show notification about fuzzy matching
          shiny::showNotification(
            paste0("Starting fuzzy matching for ", length(still_unmatched), " unmatched name(s)... This may take some time."),
            duration = NULL,
            closeButton = FALSE,
            id = "fuzzy_matching",
            type = "message"
          )

          # Process names one by one to show progress
          fuzzy_results <- list()

          for (i in seq_along(still_unmatched)) {
            name <- still_unmatched[i]

            # Update progress notification
            shiny::showNotification(
              paste0("Fuzzy matching: ", i, " of ", length(still_unmatched), " (", name, ")"),
              duration = 2,
              id = "fuzzy_progress",
              type = "message"
            )

            # Match this name
            match_result <- match_taxonomic_names(
              names = name,
              method = "hierarchical",
              max_matches = 1,
              min_similarity = min_sim,
              include_synonyms = TRUE,
              return_scores = TRUE,
              include_authors = incl_authors,
              con = NULL,
              verbose = FALSE
            )

            fuzzy_results[[i]] <- match_result
          }

          # Combine all fuzzy results
          fuzzy_matches <- dplyr::bind_rows(fuzzy_results) %>%
            dplyr::filter(match_rank == 1)

          # Remove fuzzy matching notification
          shiny::removeNotification("fuzzy_matching")
          shiny::removeNotification("fuzzy_progress")

          # Update best_matches with fuzzy results
          if (nrow(fuzzy_matches) > 0) {
            fuzzy_for_update <- fuzzy_matches %>%
              dplyr::select(
                input_name,
                idtax_n,
                idtax_good_n,
                matched_name,
                match_method,
                match_score,
                tax_fam,
                tax_gen,
                tax_esp
              )

            best_matches <- best_matches %>%
              dplyr::rows_update(
                fuzzy_for_update,
                by = "input_name",
                unmatched = "ignore"
              )
          }
        }

        # Add synonym information
        best_matches <- best_matches %>%
          dplyr::mutate(
            is_synonym = idtax_n != idtax_good_n & !is.na(idtax_n) & !is.na(idtax_good_n)
          )

        # Get accepted names for synonyms
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
                !is.na(tax_esp) & tax_esp != "" ~ paste(tax_gen, tax_esp),
                !is.na(tax_gen) ~ tax_gen,
                TRUE ~ tax_fam
              )
            ) %>%
            dplyr::select(idtax_n, accepted_name)

          best_matches <- best_matches %>%
            dplyr::left_join(
              accepted_names,
              by = c("idtax_good_n" = "idtax_n")
            )
        } else {
          best_matches$accepted_name <- NA_character_
        }

        # Calculate statistics
        n_exact <- sum(best_matches$match_method == "exact", na.rm = TRUE)
        n_genus <- sum(best_matches$match_method == "genus_constrained", na.rm = TRUE)
        n_fuzzy <- sum(best_matches$match_method == "fuzzy", na.rm = TRUE)
        n_unmatched <- sum(is.na(best_matches$idtax_n))

        stats <- list(
          total_names = total_names,
          n_exact = n_exact,
          n_genus = n_genus,
          n_fuzzy = n_fuzzy,
          n_unmatched = n_unmatched
        )

        match_stats(stats)

        # Add match results to user data
        # Rename matched columns to avoid conflicts
        best_matches_for_join <- best_matches %>%
          dplyr::select(
            input_name,
            idtax_n,
            idtax_good_n,
            matched_name,
            match_method,
            match_score,
            is_synonym,
            accepted_name
          ) %>%
          dplyr::rename(
            !!col_name := input_name
          )

        # Join with user data
        updated_data <- user_df %>%
          dplyr::left_join(
            best_matches_for_join,
            by = col_name
          )

        # Add corrected_name column (use accepted_name if synonym, otherwise matched_name)
        updated_data <- updated_data %>%
          dplyr::mutate(
            corrected_name = dplyr::case_when(
              is_synonym & !is.na(accepted_name) ~ accepted_name,
              !is.na(matched_name) ~ matched_name,
              TRUE ~ NA_character_
            )
          )

        matched_data(updated_data)

        shinybusy::hide_spinner()
        matching_in_progress(FALSE)

        shiny::showNotification(
          t()$auto_match_complete,
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        shinybusy::hide_spinner()
        matching_in_progress(FALSE)

        shiny::showNotification(
          paste(t()$msg_error, e$message),
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
            t()$msg_processing,
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
        shiny::h4(t()$auto_match_summary),

        shiny::tags$ul(
          shiny::tags$li(
            shiny::strong(t()$auto_match_total),
            stats$total_names
          ),
          shiny::tags$li(
            shiny::strong(t()$auto_match_exact),
            paste0(stats$n_exact, " (",
                  round(stats$n_exact / stats$total_names * 100, 1), "%)")
          ),
          shiny::tags$li(
            shiny::strong(t()$auto_match_genus),
            paste0(stats$n_genus, " (",
                  round(stats$n_genus / stats$total_names * 100, 1), "%)")
          ),
          shiny::tags$li(
            shiny::strong(t()$auto_match_fuzzy),
            paste0(stats$n_fuzzy, " (",
                  round(stats$n_fuzzy / stats$total_names * 100, 1), "%)")
          ),
          shiny::tags$li(
            style = if (stats$n_unmatched > 0) "color: orange; font-weight: bold;" else "",
            shiny::strong(t()$auto_match_unmatched),
            paste0(stats$n_unmatched, " (",
                  round(stats$n_unmatched / stats$total_names * 100, 1), "%)")
          )
        ),

        if (stats$n_unmatched > 0) {
          shiny::p(
            shiny::icon("info-circle"),
            "Go to the Review tab to manually review unmatched names.",
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

        # Get unmatched names
        col_name <- column_name()
        unmatched <- matched_data() %>%
          dplyr::filter(is.na(idtax_n)) %>%
          dplyr::distinct(!!rlang::sym(col_name)) %>%
          dplyr::pull(!!rlang::sym(col_name))

        list(
          data = matched_data(),
          unmatched = unmatched,
          stats = match_stats()
        )
      })
    )
  })
}
