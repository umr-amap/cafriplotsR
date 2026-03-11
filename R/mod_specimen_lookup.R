# Specimen Import Wizard - Step 3: Lookup Matching
#
# Module for matching collector names to database

#' Specimen Lookup Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_specimen_lookup_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("link"),
      i18n$t("Step 3: Match Collector Names"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Match collector names to the database. Taxonomic IDs should already be standardized from the taxonomic matching app."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Analyze button
    shiny::actionButton(
      ns("analyze_lookups"),
      shiny::tagList(shiny::icon("search"), paste0(" ", i18n$t("Analyze Lookup Values"))),
      class = "btn-primary btn-lg",
      style = "margin-bottom: 30px;"
    ),

    # Analysis results
    shiny::uiOutput(ns("analysis_summary")),

    # Tabs for different lookup types
    shiny::uiOutput(ns("lookup_tabs"))
  )
}


#' Specimen Lookup Module - Server
#'
#' @param id Module namespace ID
#' @param data Reactive containing uploaded user data
#' @param mappings Reactive containing column mappings
#' @param con_main Reactive main database connection pool
#' @param con_taxa Reactive taxa database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive list containing matched data and status
#' @keywords internal
#' @export
mod_specimen_lookup_server <- function(id, data, mappings, con_main, con_taxa, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for analysis results
    collector_analysis <- shiny::reactiveVal(NULL)
    taxon_analysis <- shiny::reactiveVal(NULL)
    det_by_analysis <- shiny::reactiveVal(NULL)
    matched_data <- shiny::reactiveVal(NULL)
    matching_complete <- shiny::reactiveVal(FALSE)

    # Analyze lookup values when button clicked
    shiny::observeEvent(input$analyze_lookups, {
      shiny::req(data(), mappings(), con_main())

      shiny::withProgress(message = i18n()$t("Analyzing lookup values..."), value = 0, {

        user_data <- data()
        maps <- mappings()

        # ===== ANALYZE COLLECTORS =====
        shiny::incProgress(0.2, detail = i18n()$t("Analyzing collectors..."))

        collector_col <- maps$collector
        unique_collectors <- unique(user_data[[collector_col]])
        unique_collectors <- unique_collectors[!is.na(unique_collectors) & unique_collectors != ""]

        # Get collector names from database
        db_con <- con_main()
        actual_con <- if (inherits(db_con, "Pool")) pool::poolCheckout(db_con) else db_con
        on.exit({
          if (inherits(db_con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
        }, add = TRUE)

        db_collectors <- DBI::dbGetQuery(
          actual_con,
          "SELECT id_table_colnam, colnam FROM table_colnam ORDER BY colnam"
        )

        # Match collectors
        collector_matches <- .match_lookup_values(
          user_values = unique_collectors,
          db_values = db_collectors$colnam,
          db_ids = db_collectors$id_table_colnam
        )
        collector_analysis(collector_matches)

        # ===== DET_BY is stored as free text (not matched to table_colnam) =====
        # Note: detby field stores the determiner's name as text, not as an ID reference
        # It is not linked to table_colnam
        det_by_analysis(NULL)

        # ===== VALIDATE TAXONOMIC IDS =====
        shiny::incProgress(0.3, detail = i18n()$t("Validating taxonomic IDs..."))

        # Validate that idtax_n column exists and contains valid numeric IDs
        idtax_col <- maps$idtax_n
        idtax_values <- user_data[[idtax_col]]

        # Check for missing or invalid IDs
        invalid_idtax <- is.na(idtax_values) | !is.numeric(as.numeric(idtax_values))
        n_invalid <- sum(invalid_idtax)

        if (n_invalid > 0) {
          shiny::showNotification(
            sprintf(i18n()$t("%d rows have missing or invalid taxonomic IDs. Please ensure all specimens have valid idtax_n values from taxonomic matching."), n_invalid),
            type = "warning",
            duration = 10
          )
        }

        # Set taxon_analysis to NULL since we don't do matching here
        taxon_analysis(NULL)

        shiny::incProgress(1, detail = i18n()$t("Complete!"))
      })

      shiny::showNotification(
        i18n()$t("Analysis complete. Review the results below."),
        type = "message",
        duration = 4
      )
    })

    # Analysis summary
    output$analysis_summary <- shiny::renderUI({
      coll <- collector_analysis()

      if (is.null(coll)) {
        return(NULL)
      }

      # Calculate stats
      coll_exact <- length(coll$exact)
      coll_fuzzy <- length(coll$fuzzy)
      coll_unmatched <- length(coll$unmatched)

      total_issues <- coll_fuzzy + coll_unmatched

      shiny::div(
        style = "margin-bottom: 30px;",

        shiny::h4(i18n()$t("Collector Analysis"), style = "margin-bottom: 15px;"),

        shiny::fluidRow(
          shiny::column(
            4,
            shiny::div(
              class = "card",
              style = "padding: 15px; background: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
              shiny::h3(coll_exact, style = "margin: 0; color: #28a745;"),
              shiny::p(i18n()$t("Exact Matches"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            4,
            shiny::div(
              class = "card",
              style = "padding: 15px; background: #f8f9fa; border-left: 4px solid #ffc107; text-align: center;",
              shiny::h3(coll_fuzzy, style = "margin: 0; color: #ffc107;"),
              shiny::p(i18n()$t("Fuzzy Matches"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            4,
            shiny::div(
              class = "card",
              style = sprintf("padding: 15px; background: #f8f9fa; border-left: 4px solid %s; text-align: center;",
                              if (coll_unmatched == 0) "#28a745" else "#dc3545"),
              shiny::h3(coll_unmatched,
                        style = sprintf("margin: 0; color: %s;",
                                        if (coll_unmatched == 0) "#28a745" else "#dc3545")),
              shiny::p(i18n()$t("Unmatched"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          )
        ),

        if (total_issues == 0) {
          shiny::div(
            class = "alert alert-success",
            style = "margin-top: 20px;",
            shiny::icon("check-circle"),
            " ",
            i18n()$t("All collectors matched successfully! You can proceed to the next step.")
          )
        } else {
          shiny::div(
            class = "alert alert-warning",
            style = "margin-top: 20px;",
            shiny::icon("exclamation-triangle"),
            " ",
            i18n()$t("Some collectors need review. Please check below and resolve any issues.")
          )
        }
      )
    })

    # Collector matching display (no tabs needed, only collectors)
    output$lookup_tabs <- shiny::renderUI({
      coll <- collector_analysis()

      if (is.null(coll)) {
        return(NULL)
      }

      ns <- session$ns

      shiny::div(
        style = "padding: 20px; background: #f8f9fa; border-radius: 8px;",
        shiny::h4(
          shiny::icon("user"),
          " ",
          i18n()$t("Collector Matching"),
          if (length(coll$unmatched) > 0) {
            shiny::span(
              class = "badge badge-danger",
              style = "margin-left: 10px;",
              length(coll$unmatched)
            )
          }
        ),
        shiny::hr(),
        shiny::uiOutput(ns("collector_matching"))
      )
    })

    # Collector matching interface
    output$collector_matching <- shiny::renderUI({
      coll <- collector_analysis()
      shiny::req(coll)

      ns <- session$ns

      shiny::tagList(
        # Exact matches
        if (length(coll$exact) > 0) {
          shiny::div(
            class = "alert alert-success",
            shiny::h5(shiny::icon("check-circle"), " ", i18n()$t("Exact Matches"), paste0(" (", length(coll$exact), ")")),
            shiny::tags$ul(
              lapply(names(coll$exact), function(name) {
                shiny::tags$li(shiny::tags$code(name))
              })
            )
          )
        },

        # Fuzzy matches (need confirmation)
        if (length(coll$fuzzy) > 0) {
          shiny::div(
            class = "alert alert-warning",
            shiny::h5(shiny::icon("question-circle"), " ", i18n()$t("Fuzzy Matches - Please Confirm")),
            lapply(seq_along(coll$fuzzy), function(i) {
              user_val <- names(coll$fuzzy)[i]
              suggestions <- coll$fuzzy[[i]]

              shiny::div(
                style = "margin: 10px 0; padding: 10px; background: white; border-radius: 4px;",
                shiny::strong(user_val), " ", shiny::icon("arrow-right"), " ",
                shiny::selectInput(
                  ns(paste0("coll_match_", i)),
                  label = NULL,
                  choices = c(setNames("", i18n()$t("-- Select match --")),
                              setNames(suggestions$id, suggestions$name)),
                  width = "300px"
                )
              )
            }),
            shiny::actionButton(
              ns("apply_fuzzy_matches"),
              shiny::tagList(shiny::icon("check"), paste0(" ", i18n()$t("Apply Fuzzy Matches"))),
              class = "btn-warning",
              style = "margin-top: 10px;"
            )
          )
        },

        # Unmatched values
        if (length(coll$unmatched) > 0) {
          shiny::div(
            class = "alert alert-danger",
            shiny::h5(shiny::icon("times-circle"), " ", i18n()$t("Unmatched Values")),
            shiny::p(i18n()$t("These collector names were not found in the database:")),
            shiny::tags$ul(
              lapply(coll$unmatched, function(name) {
                shiny::tags$li(shiny::tags$code(name))
              })
            ),
            shiny::p(
              class = "text-muted",
              i18n()$t("You may need to add these collectors to the database first, or check for typos.")
            )
          )
        }
      )
    })

    # Apply confirmed fuzzy matches when button clicked
    shiny::observeEvent(input$apply_fuzzy_matches, {
      coll <- collector_analysis()
      shiny::req(coll, length(coll$fuzzy) > 0)

      fuzzy_names <- names(coll$fuzzy)
      newly_matched <- list()
      still_fuzzy <- list()

      for (i in seq_along(fuzzy_names)) {
        selected_id <- input[[paste0("coll_match_", i)]]
        user_val <- fuzzy_names[i]

        if (!is.null(selected_id) && selected_id != "") {
          # Move to exact with the user-confirmed ID
          coll$exact[[user_val]] <- as.integer(selected_id)
          newly_matched[[user_val]] <- selected_id
        } else {
          still_fuzzy[[user_val]] <- coll$fuzzy[[user_val]]
        }
      }

      coll$fuzzy <- still_fuzzy
      collector_analysis(coll)

      n_applied <- length(newly_matched)
      n_remaining <- length(still_fuzzy)

      if (n_applied > 0) {
        shiny::showNotification(
          sprintf(i18n()$t("%d fuzzy matches applied."), n_applied),
          type = "message", duration = 3
        )
      }
      if (n_remaining > 0) {
        shiny::showNotification(
          sprintf(i18n()$t("%d fuzzy matches still need a selection."), n_remaining),
          type = "warning", duration = 4
        )
      }
    })

    # Check if matching is complete (no unmatched collectors)
    shiny::observe({
      coll <- collector_analysis()

      if (is.null(coll)) {
        matching_complete(FALSE)
        return()
      }

      coll_ok <- length(coll$unmatched) == 0
      matching_complete(coll_ok)

      # Build matched data if complete
      if (coll_ok && !is.null(data()) && !is.null(mappings())) {
        .build_matched_data()
      }
    })

    # Build matched data with IDs
    .build_matched_data <- function() {
      user_data <- data()
      maps <- mappings()
      coll <- collector_analysis()

      if (is.null(user_data) || is.null(maps)) return()

      # Start with user data (already contains idtax_n column from upload)
      result <- user_data

      # Add collector IDs (exact matches only — fuzzy must be confirmed via Apply button first)
      if (!is.null(coll)) {
        collector_col <- maps$collector
        result$id_colnam <- sapply(result[[collector_col]], function(val) {
          if (is.na(val) || val == "") return(NA_integer_)
          if (val %in% names(coll$exact)) return(as.integer(coll$exact[[val]]))
          return(NA_integer_)
        })
      }

      # Note: idtax_n is already in user data from upload step (pre-standardized)
      # We just need to ensure it's properly formatted
      idtax_col <- maps$idtax_n
      if (!is.null(idtax_col) && idtax_col %in% names(result)) {
        # Ensure idtax_n is numeric
        result$idtax_n <- as.integer(result[[idtax_col]])
      }

      # Note: det_by is stored as free text, no matching needed
      # The original text value will be used directly during import

      matched_data(result)
    }

    # Return matched data and status
    return(list(
      matched_data = matched_data,
      is_complete = matching_complete,
      collector_analysis = collector_analysis
    ))
  })
}


#' Match Lookup Values (Internal Helper)
#'
#' @param user_values Vector of user-provided values
#' @param db_values Vector of database values
#' @param db_ids Vector of database IDs
#' @return List with exact matches, fuzzy matches, and unmatched values
#' @keywords internal
.match_lookup_values <- function(user_values, db_values, db_ids) {
  exact <- list()
  fuzzy <- list()
  unmatched <- c()

  db_values_lower <- tolower(db_values)

  for (val in user_values) {
    val_lower <- tolower(val)

    # Check exact match (case-insensitive)
    idx <- which(db_values_lower == val_lower)
    if (length(idx) > 0) {
      exact[[val]] <- db_ids[idx[1]]
      next
    }

    # Check fuzzy match using string distance
    distances <- utils::adist(val_lower, db_values_lower)[1, ]
    min_dist <- min(distances)
    max_len <- max(nchar(val_lower), nchar(db_values_lower))

    # If similarity > 70%, consider it a fuzzy match
    similarity <- 1 - (min_dist / max_len)
    if (any(similarity > 0.7)) {
      top_matches <- order(distances)[1:min(3, length(distances))]
      fuzzy[[val]] <- list(
        name = db_values[top_matches],
        id = db_ids[top_matches]
      )
    } else {
      unmatched <- c(unmatched, val)
    }
  }

  return(list(
    exact = exact,
    fuzzy = fuzzy,
    unmatched = unmatched
  ))
}
