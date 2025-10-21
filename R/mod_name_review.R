# Name Review Module
#
# Interactive review of unmatched taxonomic names with fuzzy suggestions

#' Name Review Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_name_review_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(shiny::textOutput(ns("title"))),

    shiny::uiOutput(ns("review_status")),

    shiny::hr(),

    # Current name being reviewed
    shiny::uiOutput(ns("current_name_display")),

    # Fuzzy suggestions
    mod_fuzzy_suggestions_ui(ns("suggestions")),

    shiny::hr(),

    # Manual input option
    shiny::uiOutput(ns("manual_input")),

    shiny::hr(),

    # Navigation buttons
    shiny::uiOutput(ns("navigation_buttons"))
  )
}


#' Name Review Module - Server
#'
#' @param id Character, module ID
#' @param match_results Reactive list from auto matching module
#' @param mode Character, review mode ("interactive" or "batch")
#' @param max_suggestions Integer, maximum suggestions per name
#' @param min_similarity Numeric, minimum similarity threshold
#' @param language Reactive returning current language ("en" or "fr")
#'
#' @return Reactive list with updated match results
#'
#' @keywords internal
mod_name_review_server <- function(id, match_results, mode = "interactive",
                                   max_suggestions = 10, min_similarity = 0.3,
                                   language = shiny::reactive("en")) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    current_index <- shiny::reactiveVal(1)
    unmatched_names <- shiny::reactiveVal(NULL)
    review_decisions <- shiny::reactiveVal(list())
    updated_data <- shiny::reactiveVal(NULL)

    # Get translations
    t <- shiny::reactive({
      get_translations(language())
    })

    # Initialize unmatched names from match results
    shiny::observe({
      req(match_results())

      results <- match_results()
      unmatched <- results$unmatched

      if (length(unmatched) > 0) {
        unmatched_names(unmatched)
        current_index(1)
      } else {
        unmatched_names(character(0))
      }

      # Initialize with matched data
      updated_data(results$data)
    })

    # Module title
    output$title <- shiny::renderText({
      t()$review_title
    })

    # Review status
    output$review_status <- shiny::renderUI({
      req(unmatched_names())

      unmatched <- unmatched_names()

      if (length(unmatched) == 0) {
        return(
          shiny::div(
            style = "padding: 20px; background-color: #d4edda; border-radius: 5px;",
            shiny::h4(
              shiny::icon("check-circle"),
              t()$msg_no_unmatched,
              style = "color: #155724; margin: 0;"
            )
          )
        )
      }

      total <- length(unmatched)
      reviewed <- length(review_decisions())
      remaining <- total - reviewed

      shiny::div(
        style = "padding: 15px; background-color: #fff3cd; border-radius: 5px;",
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::p(
              shiny::strong(t()$progress_total),
              total,
              style = "margin: 0;"
            )
          ),
          shiny::column(
            width = 4,
            shiny::p(
              shiny::strong("Reviewed:"),
              reviewed,
              style = "margin: 0;"
            )
          ),
          shiny::column(
            width = 4,
            shiny::p(
              shiny::strong(t()$progress_remaining),
              remaining,
              style = "margin: 0; color: #856404;"
            )
          )
        )
      )
    })

    # Current name display
    output$current_name_display <- shiny::renderUI({
      req(unmatched_names())
      req(current_index() <= length(unmatched_names()))

      unmatched <- unmatched_names()
      curr_idx <- current_index()
      current_name <- unmatched[curr_idx]

      shiny::div(
        style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; border: 2px solid #007bff;",
        shiny::h4(
          t()$review_input_name,
          style = "margin-top: 0; color: #495057;"
        ),
        shiny::h3(
          current_name,
          shiny::tags$small(
            class = "text-muted ml-3",
            paste0("(", curr_idx, " ", t()$unit_of, " ", length(unmatched), ")")
          ),
          style = "margin-bottom: 0; color: #007bff;"
        )
      )
    })

    # Get current name
    current_name <- shiny::reactive({
      req(unmatched_names())
      req(current_index() <= length(unmatched_names()))

      unmatched_names()[current_index()]
    })

    # Fuzzy suggestions module
    selected_suggestion <- mod_fuzzy_suggestions_server(
      "suggestions",
      input_name = current_name,
      max_suggestions = shiny::reactive(max_suggestions),
      min_similarity = shiny::reactive(min_similarity),
      include_authors = shiny::reactive(FALSE),
      language = language
    )

    # Manual input option
    output$manual_input <- shiny::renderUI({
      ns <- session$ns

      shiny::div(
        shiny::h5(t()$review_custom_name),
        shiny::fluidRow(
          shiny::column(
            width = 5,
            shiny::textInput(
              inputId = ns("custom_name"),
              label = NULL,
              placeholder = "Enter taxonomic name..."
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = ns("custom_level"),
              label = NULL,
              choices = c(
                "All levels" = "all",
                "Species" = "species",
                "Genus" = "genus",
                "Family" = "family",
                "Order" = "order",
                "Infraspecific" = "infraspecific"
              ),
              selected = "all"
            )
          ),
          shiny::column(
            width = 4,
            shiny::br(),
            shiny::actionButton(
              inputId = ns("accept_custom"),
              label = "Accept Custom",
              class = "btn-warning btn-block"
            )
          )
        ),
        shiny::hr(),
        shiny::actionButton(
          inputId = ns("mark_unresolved"),
          label = t()$review_mark_unresolved,
          class = "btn-secondary"
        )
      )
    })

    # Navigation buttons
    output$navigation_buttons <- shiny::renderUI({
      req(unmatched_names())

      ns <- session$ns
      unmatched <- unmatched_names()
      curr_idx <- current_index()

      shiny::div(
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = ns("btn_previous"),
              label = paste(shiny::icon("arrow-left"), t()$review_prev),
              class = "btn-secondary btn-block",
              disabled = if (curr_idx == 1) "disabled" else NULL
            )
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = ns("btn_skip"),
              label = t()$review_skip,
              class = "btn-outline-secondary btn-block"
            )
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = ns("btn_next"),
              label = paste(t()$review_next, shiny::icon("arrow-right")),
              class = "btn-primary btn-block",
              disabled = if (curr_idx >= length(unmatched)) "disabled" else NULL
            )
          )
        )
      )
    })

    # Handle suggestion selection
    shiny::observeEvent(selected_suggestion(), {
      req(selected_suggestion())
      req(current_name())

      idtax <- selected_suggestion()
      name <- current_name()

      # Look up the matched name details
      matches <- match_taxonomic_names(
        names = name,
        method = "hierarchical",
        max_matches = 1,
        min_similarity = min_similarity,
        include_synonyms = TRUE,
        return_scores = TRUE,
        include_authors = FALSE,
        con = NULL,
        verbose = FALSE
      )

      matched_row <- matches %>%
        dplyr::filter(idtax_n == idtax) %>%
        dplyr::slice(1)

      if (nrow(matched_row) > 0) {
        # Record decision
        decisions <- review_decisions()
        decisions[[name]] <- list(
          type = "suggestion",
          idtax_n = matched_row$idtax_n,
          idtax_good_n = matched_row$idtax_good_n,
          matched_name = matched_row$matched_name,
          corrected_name = if (matched_row$is_synonym && !is.na(matched_row$accepted_name)) {
            matched_row$accepted_name
          } else {
            matched_row$matched_name
          },
          match_method = matched_row$match_method,
          match_score = matched_row$match_score
        )
        review_decisions(decisions)

        # Update data
        .update_data_with_decision(name, decisions[[name]])

        # Move to next
        .move_next()
      }
    })

    # Handle custom name
    shiny::observeEvent(input$accept_custom, {
      req(input$custom_name)
      req(current_name())

      custom <- input$custom_name
      name <- current_name()
      level_filter <- input$custom_level %||% "all"

      # Try to match custom name (using fuzzy for better results)
      matches <- match_taxonomic_names(
        names = custom,
        method = "hierarchical",
        max_matches = 10,  # Get multiple to filter by level
        min_similarity = 0.7,
        include_synonyms = TRUE,
        return_scores = TRUE,
        con = NULL,
        verbose = FALSE
      )

      # Apply taxonomic level filter if specified
      if (nrow(matches) > 0 && level_filter != "all") {
        matches <- matches %>% dplyr::filter(tax_level == level_filter)
      }

      # Take best match after filtering
      if (nrow(matches) > 0 && !is.na(matches$idtax_n[1])) {
        matched_row <- matches[1, ]

        decisions <- review_decisions()
        decisions[[name]] <- list(
          type = "custom",
          idtax_n = matched_row$idtax_n,
          idtax_good_n = matched_row$idtax_good_n,
          matched_name = matched_row$matched_name,
          corrected_name = if (matched_row$is_synonym && !is.na(matched_row$accepted_name)) {
            matched_row$accepted_name
          } else {
            matched_row$matched_name
          },
          match_method = "manual",
          match_score = matched_row$match_score
        )
        review_decisions(decisions)

        .update_data_with_decision(name, decisions[[name]])
        .move_next()

        # Clear input
        shiny::updateTextInput(session, "custom_name", value = "")

        # Show success notification with matched name
        shiny::showNotification(
          paste0("Matched to: ", matched_row$matched_name,
                if (level_filter != "all") paste0(" (", level_filter, " level)") else ""),
          type = "message",
          duration = 3
        )
      } else {
        shiny::showNotification(
          if (level_filter != "all") {
            paste0("Custom name not found at '", level_filter, "' level. Try 'All levels' or a different level.")
          } else {
            "Custom name not found in database. Please try again."
          },
          type = "warning",
          duration = 5
        )
      }
    })

    # Handle mark unresolved
    shiny::observeEvent(input$mark_unresolved, {
      req(current_name())

      name <- current_name()

      decisions <- review_decisions()
      decisions[[name]] <- list(
        type = "unresolved",
        idtax_n = NA,
        idtax_good_n = NA,
        matched_name = NA,
        corrected_name = NA,
        match_method = "unresolved",
        match_score = NA
      )
      review_decisions(decisions)

      .move_next()
    })

    # Navigation handlers
    shiny::observeEvent(input$btn_previous, {
      curr <- current_index()
      if (curr > 1) {
        current_index(curr - 1)
      }
    })

    shiny::observeEvent(input$btn_skip, {
      .move_next()
    })

    shiny::observeEvent(input$btn_next, {
      .move_next()
    })

    # Helper function to move to next name
    .move_next <- function() {
      unmatched <- unmatched_names()
      curr <- current_index()

      if (curr < length(unmatched)) {
        current_index(curr + 1)
      } else {
        shiny::showNotification(
          "Review complete! Go to Export tab to download results.",
          type = "message",
          duration = 5
        )
      }
    }

    # Helper function to update data with decision
    .update_data_with_decision <- function(name, decision) {
      data <- updated_data()

      # Find column name (from match_results)
      results <- match_results()
      col_name <- names(results$data)[which(sapply(results$data, function(col) {
        any(col == name, na.rm = TRUE)
      }))[1]]

      if (!is.null(col_name)) {
        # Update rows with this name
        data <- data %>%
          dplyr::mutate(
            idtax_n = ifelse(!!rlang::sym(col_name) == name & is.na(idtax_n),
                            decision$idtax_n, idtax_n),
            idtax_good_n = ifelse(!!rlang::sym(col_name) == name & is.na(idtax_good_n),
                                 decision$idtax_good_n, idtax_good_n),
            matched_name = ifelse(!!rlang::sym(col_name) == name & is.na(matched_name),
                                 decision$matched_name, matched_name),
            corrected_name = ifelse(!!rlang::sym(col_name) == name & is.na(corrected_name),
                                   decision$corrected_name, corrected_name),
            match_method = ifelse(!!rlang::sym(col_name) == name & is.na(match_method),
                                 decision$match_method, match_method),
            match_score = ifelse(!!rlang::sym(col_name) == name & is.na(match_score),
                                decision$match_score, match_score)
          )

        updated_data(data)
      }
    }

    # Return updated results
    return(
      shiny::reactive({
        req(match_results())

        # Calculate updated stats
        data <- updated_data() %||% match_results()$data
        total_reviewed <- length(review_decisions())

        unmatched <- if (!is.null(unmatched_names())) {
          unmatched_names()[!unmatched_names() %in% names(review_decisions())]
        } else {
          character(0)
        }

        list(
          data = data,
          unmatched = unmatched,
          stats = list(
            total_names = match_results()$stats$total_names,
            n_exact = match_results()$stats$n_exact,
            n_genus = match_results()$stats$n_genus,
            n_fuzzy = match_results()$stats$n_fuzzy,
            n_reviewed = total_reviewed,
            n_unmatched = length(unmatched)
          ),
          decisions = review_decisions()
        )
      })
    )
  })
}
