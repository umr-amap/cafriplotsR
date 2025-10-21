# Fuzzy Suggestions Module
#
# Displays fuzzy match suggestions for a given taxonomic name

#' Fuzzy Suggestions Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_fuzzy_suggestions_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("suggestions_header")),
    shiny::uiOutput(ns("suggestions_controls")),
    shiny::hr(),
    shiny::uiOutput(ns("suggestions_list"))
  )
}


#' Fuzzy Suggestions Module - Server
#'
#' @param id Character, module ID
#' @param input_name Reactive character, the name to find suggestions for
#' @param max_suggestions Reactive or numeric, maximum suggestions to show
#' @param min_similarity Reactive or numeric, minimum similarity threshold
#' @param include_authors Reactive or logical, whether to include author names
#' @param language Reactive returning current language ("en" or "fr")
#'
#' @return Reactive integer, idtax_n of selected suggestion (or NULL)
#'
#' @keywords internal
mod_fuzzy_suggestions_server <- function(id, input_name, max_suggestions = shiny::reactive(10),
                                         min_similarity = shiny::reactive(0.3),
                                         include_authors = shiny::reactive(FALSE),
                                         language = shiny::reactive("en")) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    suggestions <- shiny::reactiveVal(NULL)
    selected_id <- shiny::reactiveVal(NULL)

    # Get translations
    t <- shiny::reactive({
      get_translations(language())
    })

    # Fetch suggestions when input name changes
    shiny::observe({
      req(input_name())

      name <- input_name()
      max_sug <- if (shiny::is.reactive(max_suggestions)) max_suggestions() else max_suggestions
      min_sim <- if (shiny::is.reactive(min_similarity)) min_similarity() else min_similarity
      incl_auth <- if (shiny::is.reactive(include_authors)) include_authors() else include_authors

      # Get suggestions using match_taxonomic_names
      matches <- match_taxonomic_names(
        names = name,
        method = "hierarchical",
        max_matches = max_sug,
        min_similarity = min_sim,
        include_synonyms = TRUE,
        return_scores = TRUE,
        include_authors = incl_auth,
        con = NULL,
        verbose = FALSE
      )

      suggestions(matches)
    })

    # Suggestions header
    output$suggestions_header <- shiny::renderUI({
      req(input_name())

      shiny::div(
        style = "padding: 10px; background-color: #e7f3ff; border-radius: 5px; margin-bottom: 10px;",
        shiny::h4(
          paste(t()$suggestions_title, '"', input_name(), '"'),
          style = "margin: 0; color: #0056b3;"
        )
      )
    })

    # Suggestions controls
    output$suggestions_controls <- shiny::renderUI({
      ns <- session$ns

      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::numericInput(
            inputId = ns("num_suggestions"),
            label = t()$review_num_suggestions,
            value = if (shiny::is.reactive(max_suggestions)) max_suggestions() else max_suggestions,
            min = 5,
            max = 30,
            step = 5
          )
        ),
        shiny::column(
          width = 6,
          shiny::radioButtons(
            inputId = ns("sort_by"),
            label = t()$review_sort,
            choices = c(
              "Similarity" = "similarity",
              "Alphabetical" = "alphabetical"
            ),
            selected = "similarity",
            inline = TRUE
          )
        )
      )
    })

    # Suggestions list
    output$suggestions_list <- shiny::renderUI({
      req(suggestions())

      sug <- suggestions()

      if (nrow(sug) == 0 || all(is.na(sug$idtax_n))) {
        return(
          shiny::div(
            style = "padding: 20px; background-color: #fff3cd; border-radius: 5px;",
            shiny::p(
              shiny::icon("exclamation-triangle"),
              t()$suggestions_no_match,
              style = "color: #856404; margin: 0;"
            )
          )
        )
      }

      # Filter out NA matches
      sug <- sug %>% dplyr::filter(!is.na(idtax_n))

      # Sort suggestions
      if (!is.null(input$sort_by) && input$sort_by == "alphabetical") {
        sug <- sug %>% dplyr::arrange(matched_name)
      } else {
        sug <- sug %>% dplyr::arrange(desc(match_score))
      }

      # Limit number shown
      num_show <- input$num_suggestions %||% (if (shiny::is.reactive(max_suggestions)) max_suggestions() else max_suggestions)
      sug <- head(sug, num_show)

      ns <- session$ns

      # Create suggestion cards
      suggestion_cards <- lapply(1:nrow(sug), function(i) {
        row <- sug[i, ]

        score_pct <- round(row$match_score * 100)
        color_class <- if (score_pct >= 90) {
          "success"
        } else if (score_pct >= 70) {
          "info"
        } else if (score_pct >= 50) {
          "warning"
        } else {
          "secondary"
        }

        shiny::div(
          class = "card mb-2",
          style = "border-left: 4px solid #007bff;",

          shiny::div(
            class = "card-body p-3",

            shiny::fluidRow(
              shiny::column(
                width = 8,
                shiny::h5(
                  row$matched_name,
                  shiny::tags$small(
                    class = paste0("badge badge-", color_class, " ml-2"),
                    paste0(score_pct, "%")
                  ),
                  style = "margin: 0;"
                ),
                shiny::p(
                  class = "text-muted mb-1",
                  style = "font-size: 0.9em;",
                  paste0(
                    if (!is.na(row$tax_fam)) paste(t()$review_family, row$tax_fam, " | ") else "",
                    if (!is.na(row$tax_gen)) paste(t()$review_genus, row$tax_gen) else ""
                  )
                ),
                shiny::p(
                  class = "text-muted mb-0",
                  style = "font-size: 0.85em;",
                  paste(t()$review_method, row$match_method)
                ),
                if (row$is_synonym && !is.na(row$accepted_name)) {
                  shiny::p(
                    class = "text-warning mb-0",
                    style = "font-size: 0.85em;",
                    shiny::icon("info-circle"),
                    paste("Synonym →", row$accepted_name)
                  )
                }
              ),
              shiny::column(
                width = 4,
                class = "text-right",
                shiny::actionButton(
                  inputId = ns(paste0("select_", i)),
                  label = t()$review_select_match,
                  class = "btn-sm btn-primary",
                  onclick = paste0("Shiny.setInputValue('", ns("selected_row"), "', ", i, ");")
                )
              )
            )
          )
        )
      })

      shiny::div(suggestion_cards)
    })

    # Handle selection
    shiny::observeEvent(input$selected_row, {
      req(suggestions())
      req(input$selected_row)

      sug <- suggestions()

      # Sort same way as display
      if (!is.null(input$sort_by) && input$sort_by == "alphabetical") {
        sug <- sug %>% dplyr::arrange(matched_name)
      } else {
        sug <- sug %>% dplyr::arrange(desc(match_score))
      }

      sug <- sug %>% dplyr::filter(!is.na(idtax_n))

      selected_row_idx <- input$selected_row

      if (selected_row_idx > 0 && selected_row_idx <= nrow(sug)) {
        selected_id(sug$idtax_n[selected_row_idx])
      }
    })

    # Return selected ID
    return(selected_id)
  })
}
