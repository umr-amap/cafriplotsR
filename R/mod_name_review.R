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
#' @param i18n Reactive returning shiny.i18n translator
#' @param backbone Reactive returning the cached backbone tibble (or NULL).
#'   When non-NULL, custom searches and selection lookups go through the
#'   cached backbone — required for offline mode.
#'
#' @return Reactive list with updated match results
#'
#' @keywords internal
mod_name_review_server <- function(id, match_results, mode = "interactive",
                                   max_suggestions = 10, min_similarity = 0.3,
                                   i18n,
                                   backbone = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    current_index <- shiny::reactiveVal(1)
    unmatched_names <- shiny::reactiveVal(NULL)
    review_decisions <- shiny::reactiveVal(list())
    updated_data <- shiny::reactiveVal(NULL)
    custom_search_matches <- shiny::reactiveVal(NULL)

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
      i18n()$t("Review Unmatched Names")
    })

    # Review status
    output$review_status <- shiny::renderUI({
      req(!is.null(unmatched_names()))

      unmatched <- unmatched_names()

      if (length(unmatched) == 0) {
        return(
          shiny::div(
            style = "padding: 20px; background-color: #d4edda; border-radius: 5px;",
            shiny::h4(
              shiny::icon("check-circle"),
              i18n()$t("All names successfully matched!"),
              style = "color: #155724; margin: 0;"
            ),
            shiny::p(
              shiny::icon("arrow-right"),
              i18n()$t("No names require manual review. You can skip this step and go directly to the next tab (Export)."),
              style = "color: #155724; margin: 12px 0 0 0;"
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
              shiny::strong(i18n()$t("Total unique names:")),
              total,
              style = "margin: 0;"
            )
          ),
          shiny::column(
            width = 4,
            shiny::p(
              shiny::strong(i18n()$t("Reviewed:")),
              reviewed,
              style = "margin: 0;"
            )
          ),
          shiny::column(
            width = 4,
            shiny::p(
              shiny::strong(i18n()$t("Remaining:")),
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

      # Check if current name is NA
      is_na_value <- is.na(current_name)
      display_name <- if (is_na_value) {
        shiny::tagList(
          shiny::tags$span(i18n()$t("Missing Taxonomic Name"), style = "color: #856404;"),
          shiny::tags$small(
            class = "text-muted ml-3",
            i18n()$t("(Original value: NA)")
          )
        )
      } else {
        current_name
      }

      shiny::div(
        style = paste0("padding: 20px; background-color: #f8f9fa; border-radius: 5px; border: 2px solid ",
                      if (is_na_value) "#ffc107" else "#007bff", ";"),
        shiny::h4(
          i18n()$t("Input name:"),
          style = "margin-top: 0; color: #495057;"
        ),
        shiny::h3(
          display_name,
          shiny::tags$small(
            class = "text-muted ml-3",
            paste0("(", curr_idx, " ", i18n()$t("of"), " ", length(unmatched), ")")
          ),
          style = paste0("margin-bottom: 0; color: ", if (is_na_value) "#856404" else "#007bff", ";")
        )
      )
    })

    # Get current name
    current_name <- shiny::reactive({
      req(unmatched_names())
      req(current_index() <= length(unmatched_names()))

      unmatched_names()[current_index()]
    })

    # Get current name for fuzzy matching (NULL if NA, to skip fuzzy suggestions)
    current_name_for_fuzzy <- shiny::reactive({
      name <- current_name()
      if (is.na(name)) return(NULL)
      return(name)
    })

    # Fuzzy suggestions module (will be empty/hidden for NA values)
    selected_suggestion <- mod_fuzzy_suggestions_server(
      "suggestions",
      input_name = current_name_for_fuzzy,
      max_suggestions = shiny::reactive(max_suggestions),
      min_similarity = shiny::reactive(min_similarity),
      include_authors = shiny::reactive(FALSE),
      i18n = i18n,
      backbone = backbone
    )

    # Manual input option
    output$manual_input <- shiny::renderUI({
      ns <- session$ns

      # Check if current name is NA
      is_na_value <- is.na(current_name())

      shiny::div(
        # Show special message for NA values
        if (is_na_value) {
          shiny::div(
            style = "padding: 10px; background-color: #fff3cd; border-radius: 5px; margin-bottom: 15px;",
            shiny::p(
              shiny::icon("info-circle"),
              shiny::strong("Missing Taxonomic Name:"),
              "This row has no taxonomic name (NA value). Use the search below to find and assign a taxonomic ID.",
              style = "margin: 0; color: #856404;"
            )
          )
        },
        shiny::h5(
          shiny::icon("search"),
          "Search Taxonomic Backbone",
          style = "color: #495057;"
        ),
        shiny::p(
          if (is_na_value) {
            "Search for the correct taxonomic name and select it to assign to this row."
          } else {
            "Enter a taxonomic name to search the backbone database. Select a taxonomic level to narrow results."
          },
          class = "text-muted",
          style = "font-size: 0.9em;"
        ),
        shiny::fluidRow(
          shiny::column(
            width = 5,
            shiny::textInput(
              inputId = ns("custom_name"),
              label = "Name to search:",
              placeholder = "e.g., Fabaceae, Brachystegia..."
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = ns("custom_level"),
              label = "Taxonomic level:",
              choices = c(
                "All levels" = "all",
                "Species" = "species",
                "Genus" = "genus",
                "Family" = "family",
                "Order" = "order",
                "Infraspecific" = "infraspecific",
                "Higher" = "higher"
              ),
              selected = "all"
            )
          ),
          shiny::column(
            width = 4,
            shiny::br(),
            shiny::actionButton(
              inputId = ns("search_custom"),
              label = shiny::tagList(shiny::icon("search"), "Search"),
              class = "btn-info btn-block"
            )
          )
        ),
        shiny::uiOutput(ns("custom_search_results")),
        shiny::hr(),
        shiny::actionButton(
          inputId = ns("mark_unresolved"),
          label = i18n()$t("Mark as unresolved"),
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
              label = shiny::tagList(shiny::icon("arrow-left"), i18n()$t("Previous")),
              class = "btn-secondary btn-block",
              disabled = if (curr_idx == 1) "disabled" else NULL
            )
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = ns("btn_skip"),
              label = i18n()$t("Skip"),
              class = "btn-outline-secondary btn-block"
            )
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = ns("btn_next"),
              label = shiny::tagList(i18n()$t("Next"), shiny::icon("arrow-right")),
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
      bb <- backbone()

      # Look up the taxon details by idtax_n — prefer cache when offline
      matched_row <- if (!is.null(bb)) {
        .lookup_taxon_in_backbone(bb, idtax)
      } else {
        mydb_taxa <- call.mydb.taxa()
        sql <- glue::glue_sql("
          SELECT
            t.idtax_n,
            t.idtax_good_n,
            t.tax_gen,
            t.tax_esp,
            t.tax_fam,
            t.tax_famclass,
            t.tax_level,
            CASE
              WHEN t.tax_level = 'species' THEN CONCAT_WS(' ', t.tax_gen, t.tax_esp)
              WHEN t.tax_level = 'genus' THEN t.tax_gen
              WHEN t.tax_level = 'family' THEN t.tax_fam
              WHEN t.tax_level = 'higher' THEN t.tax_famclass
              WHEN t.tax_level = 'infraspecific' THEN CONCAT_WS(' ', t.tax_gen, t.tax_esp, t.tax_rank01, t.tax_nam01)
              ELSE COALESCE(t.tax_gen, t.tax_fam, t.tax_famclass)
            END AS matched_name,
            a.tax_gen AS accepted_gen,
            a.tax_esp AS accepted_esp,
            a.tax_fam AS accepted_fam,
            a.tax_famclass AS accepted_famclass,
            a.tax_level AS accepted_level,
            CASE
              WHEN a.tax_level = 'species' THEN CONCAT_WS(' ', a.tax_gen, a.tax_esp)
              WHEN a.tax_level = 'genus' THEN a.tax_gen
              WHEN a.tax_level = 'family' THEN a.tax_fam
              WHEN a.tax_level = 'higher' THEN a.tax_famclass
              WHEN a.tax_level = 'infraspecific' THEN CONCAT_WS(' ', a.tax_gen, a.tax_esp, a.tax_rank01, a.tax_nam01)
              ELSE COALESCE(a.tax_gen, a.tax_fam, a.tax_famclass)
            END AS accepted_name
          FROM table_taxa t
          LEFT JOIN table_taxa a ON t.idtax_good_n = a.idtax_n
          WHERE t.idtax_n = {idtax}
        ", idtax = idtax, .con = mydb_taxa)

        tryCatch({
          func_try_fetch(con = mydb_taxa, sql = sql)
        }, error = function(e) {
          tibble()
        })
      }

      if (nrow(matched_row) > 0) {
        is_synonym <- matched_row$idtax_n != matched_row$idtax_good_n

        # Record decision
        decisions <- review_decisions()
        decisions[[name]] <- list(
          type = "suggestion",
          idtax_n = matched_row$idtax_n,
          idtax_good_n = matched_row$idtax_good_n,
          matched_name = matched_row$matched_name,
          corrected_name = if (is_synonym && !is.na(matched_row$accepted_name)) {
            matched_row$accepted_name
          } else {
            matched_row$matched_name
          },
          match_method = "fuzzy",
          match_score = 1.0  # User selected this, so it's a confirmed match
        )
        review_decisions(decisions)

        # Update data
        .update_data_with_decision(name, decisions[[name]])

        # Show success notification
        shiny::showNotification(
          paste0("Matched to: ", matched_row$matched_name),
          type = "message",
          duration = 3
        )

        # Move to next
        .move_next()
      }
    })

    # Handle custom name search
    shiny::observeEvent(input$search_custom, {
      req(input$custom_name)

      custom <- input$custom_name
      level_filter <- input$custom_level %||% "all"
      bb <- backbone()

      # If user specified "higher" level, search for class names —
      # prefer cached backbone when offline
      if (level_filter == "higher") {
        if (!is.null(bb)) {
          all_matches <- .level_fuzzy_search_r(bb, custom, "higher",
                                               min_sim = 0.3, max_results = 50L)
        } else {
          mydb_taxa <- call.mydb.taxa()

          sql <- glue::glue_sql("
            SELECT DISTINCT ON (tax_famclass)
              idtax_n,
              idtax_good_n,
              tax_gen,
              tax_esp,
              tax_fam,
              tax_level,
              tax_famclass AS matched_name,
              SIMILARITY(lower(tax_famclass), lower({search_name})) AS match_score
            FROM table_taxa
            WHERE tax_famclass IS NOT NULL
              AND tax_level = 'higher'
              AND SIMILARITY(lower(tax_famclass), lower({search_name})) >= 0.3
            ORDER BY tax_famclass, match_score DESC
            LIMIT 50
          ", search_name = custom, .con = mydb_taxa)

          all_matches <- tryCatch({
            result <- func_try_fetch(con = mydb_taxa, sql = sql)
            if (nrow(result) > 0) {
              result %>%
                dplyr::mutate(
                  input_name = custom,
                  match_method = "fuzzy",
                  match_rank = 1,
                  is_synonym = FALSE,
                  accepted_name = NA_character_
                )
            } else {
              tibble()
            }
          }, error = function(e) {
            tibble()
          })
        }
      } else {
        # Use standard hierarchical matching (R-side when bb is non-NULL)
        all_matches <- match_taxonomic_names(
          names = custom,
          method = "hierarchical",  # Tries exact first, then fuzzy
          max_matches = 50,  # Get many matches for filtering
          min_similarity = 0.3,  # Low threshold to allow fuzzy matching
          include_synonyms = TRUE,
          return_scores = TRUE,
          con = NULL,
          backbone = bb,
          verbose = FALSE
        )

        # Apply taxonomic level filter if specified
        if (nrow(all_matches) > 0 && level_filter != "all") {
          all_matches <- all_matches %>% dplyr::filter(tax_level == level_filter)
        }
      }

      # Separate exact matches from fuzzy matches
      exact_matches <- all_matches %>%
        dplyr::filter(!is.na(idtax_n), match_score >= 0.99) %>%
        dplyr::arrange(dplyr::desc(match_score))

      fuzzy_matches <- all_matches %>%
        dplyr::filter(!is.na(idtax_n), match_score < 0.99) %>%
        dplyr::arrange(dplyr::desc(match_score))

      # Prefer exact matches, but show fuzzy if no exact found
      if (nrow(exact_matches) > 0) {
        matches <- exact_matches %>% dplyr::slice(1:min(4, dplyr::n()))
        match_type <- "exact"
      } else if (nrow(fuzzy_matches) > 0) {
        matches <- fuzzy_matches %>% dplyr::slice(1:min(4, dplyr::n()))
        match_type <- "fuzzy"
      } else {
        matches <- all_matches[0, ]  # Empty with structure
        match_type <- NULL
      }

      # Store results
      custom_search_matches(matches)

      if (nrow(matches) == 0 || all(is.na(matches$idtax_n))) {
        shiny::showNotification(
          if (level_filter != "all") {
            paste0("No matches found for '", custom, "' at '", level_filter, "' level. Try 'All levels' or a different level.")
          } else {
            paste0("No matches found for '", custom, "' in the backbone database.")
          },
          type = "warning",
          duration = 5
        )
      } else {
        # Inform user whether exact or fuzzy matches were found
        shiny::showNotification(
          paste0("Found ", nrow(matches), " ", match_type, " match", if(nrow(matches) > 1) "es" else "", "."),
          type = "message",
          duration = 3
        )
      }
    })

    # Display custom search results
    output$custom_search_results <- shiny::renderUI({
      matches <- custom_search_matches()

      if (is.null(matches) || nrow(matches) == 0 || all(is.na(matches$idtax_n))) {
        return(NULL)
      }

      # Filter out NA matches
      matches <- matches %>% dplyr::filter(!is.na(idtax_n))

      if (nrow(matches) == 0) {
        return(NULL)
      }

      ns <- session$ns

      shiny::div(
        style = "margin-top: 15px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
        shiny::h6(
          paste0("Search Results (", nrow(matches), " found)"),
          style = "margin-top: 0; color: #495057;"
        ),
        shiny::div(
          style = "max-height: 300px; overflow-y: auto;",
          lapply(1:nrow(matches), function(i) {
            row <- matches[i, ]

            score_pct <- round(row$match_score * 100)
            color_class <- if (score_pct >= 90) {
              "success"
            } else if (score_pct >= 70) {
              "info"
            } else {
              "secondary"
            }

            shiny::div(
              class = "card mb-2",
              style = "border-left: 3px solid #17a2b8;",

              shiny::div(
                class = "card-body p-2",

                shiny::fluidRow(
                  shiny::column(
                    width = 9,
                    shiny::strong(row$matched_name),
                    shiny::tags$small(
                      class = paste0("badge badge-", color_class, " ml-2"),
                      paste0(score_pct, "%")
                    ),
                    shiny::br(),
                    shiny::tags$small(
                      class = "text-muted",
                      paste0(
                        if (!is.na(row$tax_level)) paste("Level:", row$tax_level, " | ") else "",
                        if (!is.na(row$tax_fam)) paste("Family:", row$tax_fam, " | ") else "",
                        if (!is.na(row$tax_gen)) paste("Genus:", row$tax_gen) else ""
                      )
                    ),
                    if (row$is_synonym && !is.na(row$accepted_name)) {
                      shiny::tagList(
                        shiny::br(),
                        shiny::tags$small(
                          class = "text-warning",
                          shiny::icon("info-circle"),
                          paste("Synonym →", row$accepted_name)
                        )
                      )
                    }
                  ),
                  shiny::column(
                    width = 3,
                    class = "text-right",
                    shiny::actionButton(
                      inputId = ns(paste0("select_custom_", i)),
                      label = "Select",
                      class = "btn-sm btn-info",
                      onclick = paste0("Shiny.setInputValue('", ns("custom_selected_row"), "', ", i, ", {priority: 'event'});")
                    )
                  )
                )
              )
            )
          })
        )
      )
    })

    # Handle custom search selection
    # Use ignoreInit = FALSE and ignoreNULL = FALSE to catch all clicks
    shiny::observeEvent(input$custom_selected_row, ignoreInit = FALSE, ignoreNULL = FALSE, {
      req(custom_search_matches())
      req(input$custom_selected_row)

      # Don't require current_name() as it might be NA
      matches <- custom_search_matches()
      selected_idx <- input$custom_selected_row
      name <- current_name()

      if (selected_idx > 0 && selected_idx <= nrow(matches)) {
        matched_row <- matches[selected_idx, ]

        decisions <- review_decisions()
        decisions[[name]] <- list(
          type = "custom_search",
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

        # Clear search
        custom_search_matches(NULL)
        shiny::updateTextInput(session, "custom_name", value = "")

        # Show success and move to next
        shiny::showNotification(
          paste0("Matched to: ", matched_row$matched_name),
          type = "message",
          duration = 3
        )

        .move_next()
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

      # Find the column containing the name (handle NA specially)
      # Only check character or factor columns to avoid date/time comparison errors
      if (is.na(name)) {
        col_name <- names(results$data)[which(sapply(results$data, function(col) {
          # Only check character or factor columns
          if (is.character(col) || is.factor(col)) {
            any(is.na(col))
          } else {
            FALSE
          }
        }))[1]]
      } else {
        col_name <- names(results$data)[which(sapply(results$data, function(col) {
          # Only check character or factor columns to avoid POSIXt comparison errors
          if (is.character(col) || is.factor(col)) {
            any(col == name, na.rm = TRUE)
          } else {
            FALSE
          }
        }))[1]]
      }

      if (!is.null(col_name)) {
        # Create a row mask for matching (handle NA specially)
        if (is.na(name)) {
          # For NA values, match rows where column is NA and idtax_n is also NA
          data <- data %>%
            dplyr::mutate(
              idtax_n = ifelse(is.na(!!rlang::sym(col_name)) & is.na(idtax_n),
                              decision$idtax_n, idtax_n),
              idtax_good_n = ifelse(is.na(!!rlang::sym(col_name)) & is.na(idtax_good_n),
                                   decision$idtax_good_n, idtax_good_n),
              matched_name = ifelse(is.na(!!rlang::sym(col_name)) & is.na(matched_name),
                                   decision$matched_name, matched_name),
              corrected_name = ifelse(is.na(!!rlang::sym(col_name)) & is.na(corrected_name),
                                     decision$corrected_name, corrected_name),
              match_method = ifelse(is.na(!!rlang::sym(col_name)) & is.na(match_method),
                                   decision$match_method, match_method),
              match_score = ifelse(is.na(!!rlang::sym(col_name)) & is.na(match_score),
                                  decision$match_score, match_score)
            )
        } else {
          # For non-NA values, use normal equality check
          data <- data %>%
            dplyr::mutate(
              idtax_n = ifelse(!is.na(!!rlang::sym(col_name)) &
                              !!rlang::sym(col_name) == name &
                              is.na(idtax_n),
                              decision$idtax_n, idtax_n),
              idtax_good_n = ifelse(!is.na(!!rlang::sym(col_name)) &
                                   !!rlang::sym(col_name) == name &
                                   is.na(idtax_good_n),
                                   decision$idtax_good_n, idtax_good_n),
              matched_name = ifelse(!is.na(!!rlang::sym(col_name)) &
                                   !!rlang::sym(col_name) == name &
                                   is.na(matched_name),
                                   decision$matched_name, matched_name),
              corrected_name = ifelse(!is.na(!!rlang::sym(col_name)) &
                                     !!rlang::sym(col_name) == name &
                                     is.na(corrected_name),
                                     decision$corrected_name, corrected_name),
              match_method = ifelse(!is.na(!!rlang::sym(col_name)) &
                                   !!rlang::sym(col_name) == name &
                                   is.na(match_method),
                                   decision$match_method, match_method),
              match_score = ifelse(!is.na(!!rlang::sym(col_name)) &
                                  !!rlang::sym(col_name) == name &
                                  is.na(match_score),
                                  decision$match_score, match_score)
            )
        }

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


#' Look up a single idtax_n in the cached backbone, returning a tibble with
#' the same shape as the SQL self-join used in the online path
#'
#' Mirrors the SQL `SELECT t.*, a.* FROM table_taxa t LEFT JOIN table_taxa a
#' ON t.idtax_good_n = a.idtax_n` lookup, returning matched_name and
#' accepted_name fields built from the backbone columns.
#'
#' @keywords internal
.lookup_taxon_in_backbone <- function(backbone, idtax) {
  if (is.null(backbone) || nrow(backbone) == 0L) return(dplyr::tibble())

  hit <- backbone %>% dplyr::filter(.data$idtax_n == idtax) %>% dplyr::slice(1L)
  if (nrow(hit) == 0L) return(dplyr::tibble())

  acc <- backbone %>% dplyr::filter(.data$idtax_n == hit$idtax_good_n) %>% dplyr::slice(1L)

  build_label <- function(row) {
    if (nrow(row) == 0L) return(NA_character_)
    lvl <- row$tax_level
    if (!is.na(lvl) && lvl == "species" && !is.na(row$tax_esp)) {
      paste(row$tax_gen, row$tax_esp)
    } else if (!is.na(lvl) && lvl == "genus") {
      row$tax_gen
    } else if (!is.na(lvl) && lvl == "family") {
      row$tax_fam
    } else if (!is.na(lvl) && lvl == "higher") {
      row$tax_famclass
    } else if (!is.na(lvl) && lvl == "infraspecific" && !is.na(row$tax_esp)) {
      paste(c(row$tax_gen, row$tax_esp,
              if (!is.na(row$tax_rank01)) row$tax_rank01,
              if (!is.na(row$tax_nam01)) row$tax_nam01),
            collapse = " ")
    } else {
      dplyr::coalesce(row$tax_gen, row$tax_fam, row$tax_famclass)
    }
  }

  dplyr::tibble(
    idtax_n           = hit$idtax_n,
    idtax_good_n      = hit$idtax_good_n,
    tax_gen           = hit$tax_gen,
    tax_esp           = hit$tax_esp,
    tax_fam           = hit$tax_fam,
    tax_famclass      = hit$tax_famclass,
    tax_level         = hit$tax_level,
    matched_name      = build_label(hit),
    accepted_gen      = if (nrow(acc) > 0L) acc$tax_gen      else NA_character_,
    accepted_esp      = if (nrow(acc) > 0L) acc$tax_esp      else NA_character_,
    accepted_fam      = if (nrow(acc) > 0L) acc$tax_fam      else NA_character_,
    accepted_famclass = if (nrow(acc) > 0L) acc$tax_famclass else NA_character_,
    accepted_level    = if (nrow(acc) > 0L) acc$tax_level    else NA_character_,
    accepted_name     = build_label(acc)
  )
}
