# Import Wizard - Step 3: Column Mapping
#
# Module for mapping user columns to database schema with visual interface

#' Step 3 Module: Column Mapping - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
mod_step3_mapping_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("exchange-alt"),
      i18n$t("Step 3: Map Your Columns to Database Schema"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Review and adjust the automatic column mapping. The wizard has attempted to match your columns to the database schema."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Mapping summary
    shiny::uiOutput(ns("mapping_summary")),

    shiny::hr(),

    # Column mapping interface
    shiny::h4(i18n$t("Column Mappings"), style = "margin-bottom: 20px;"),
    shiny::uiOutput(ns("mapping_interface")),

    # Validation messages
    shiny::uiOutput(ns("mapping_validation"))
  )
}


#' Step 3 Module: Column Mapping - Server
#'
#' @param id Module namespace ID
#' @param data Reactive containing uploaded user data
#' @param config Reactive containing import configuration
#' @return Reactive list containing mappings and validation status
#' @keywords internal
mod_step3_mapping_server <- function(id, data, config) {
  shiny::moduleServer(id, function(input, output, session) {

    # Auto-generate initial mappings when data/config available
    auto_mappings <- shiny::reactive({
      shiny::req(data(), config())

      cli::cli_alert_info("Running automatic column mapping...")

      result <- map_user_columns(
        user_data = data(),
        config = config(),
        similarity_threshold = 0.6,
        interactive = FALSE  # No console prompts in Shiny
      )

      cli::cli_alert_success("Auto-mapping complete")
      result
    })

    # Get all valid schema columns for dropdowns
    schema_columns <- shiny::reactive({
      shiny::req(config())

      all_cols <- c(
        config()$direct_columns,
        if (!is.null(config()$subplot_features)) config()$subplot_features else character(0)
      )

      sort(unique(all_cols))
    })

    # Mapping summary statistics
    output$mapping_summary <- shiny::renderUI({
      shiny::req(auto_mappings())

      am <- auto_mappings()

      n_exact <- sum(am$methods == "exact", na.rm = TRUE)
      n_synonym <- sum(am$methods == "synonym", na.rm = TRUE)
      n_fuzzy <- sum(am$methods == "fuzzy", na.rm = TRUE)
      n_unmapped <- length(am$unmapped)
      n_total <- length(am$mappings)

      shiny::div(
        class = "alert alert-info",
        style = "background-color: #e7f3ff; border-left: 4px solid #007bff;",

        shiny::fluidRow(
          shiny::column(
            3,
            shiny::div(
              style = "text-align: center;",
              shiny::h3(n_exact + n_synonym, style = "color: #28a745; margin: 0;"),
              shiny::p(shiny::icon("check-circle"), " Auto-Mapped", style = "margin: 5px 0 0 0; color: #28a745;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              style = "text-align: center;",
              shiny::h3(n_fuzzy, style = "color: #ffc107; margin: 0;"),
              shiny::p(shiny::icon("question-circle"), " Review Suggested", style = "margin: 5px 0 0 0; color: #ffc107;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              style = "text-align: center;",
              shiny::h3(n_unmapped, style = "color: #dc3545; margin: 0;"),
              shiny::p(shiny::icon("times-circle"), " Needs Mapping", style = "margin: 5px 0 0 0; color: #dc3545;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              style = "text-align: center;",
              shiny::h3(n_total, style = "color: #6c757d; margin: 0;"),
              shiny::p(shiny::icon("list"), " Total Columns", style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          )
        )
      )
    })

    # Column mapping interface
    output$mapping_interface <- shiny::renderUI({
      shiny::req(data(), auto_mappings(), schema_columns(), config())

      am <- auto_mappings()
      user_cols <- names(am$mappings)
      schema_choices <- c("(Skip this column)" = "", schema_columns())

      # Get column descriptions from config
      column_descriptions <- config()$import_config$column_descriptions
      if (is.null(column_descriptions)) {
        column_descriptions <- list()
      }

      # Create a row for each user column
      mapping_rows <- lapply(user_cols, function(user_col) {

        # Get auto-mapping info
        mapped_to <- am$mappings[[user_col]]
        method <- am$methods[[user_col]]
        confidence <- am$confidence[[user_col]]

        # Determine status icon and color
        status_info <- if (method == "exact") {
          list(icon = "check-circle", color = "#28a745", label = "Exact Match")
        } else if (method == "synonym") {
          list(icon = "check-circle", color = "#28a745", label = "Synonym")
        } else if (method == "fuzzy") {
          list(icon = "question-circle", color = "#ffc107", label = sprintf("Fuzzy (%.0f%%)", confidence * 100))
        } else {
          list(icon = "times-circle", color = "#dc3545", label = "Not Mapped")
        }

        # Get sample values from data
        sample_vals <- head(data()[[user_col]], 3)
        sample_text <- paste(sample_vals, collapse = ", ")
        if (nchar(sample_text) > 50) {
          sample_text <- paste0(substr(sample_text, 1, 47), "...")
        }

        # Build the row
        shiny::div(
          class = "mapping-row",
          style = sprintf("border-left-color: %s;", status_info$color),

          shiny::fluidRow(
            # User column name + samples
            shiny::column(
              4,
              shiny::strong(user_col, style = "font-size: 14px;"),
              shiny::br(),
              shiny::tags$small(
                shiny::icon("eye"),
                " Samples: ",
                shiny::tags$code(sample_text, style = "font-size: 11px;"),
                style = "color: #6c757d;"
              )
            ),

            # Arrow
            shiny::column(
              1,
              shiny::div(
                shiny::icon("arrow-right", style = "font-size: 24px; color: #007bff;"),
                style = "text-align: center; padding-top: 10px;"
              )
            ),

            # Database column dropdown + description (reactive)
            shiny::column(
              5,
              shiny::selectInput(
                session$ns(paste0("map_", user_col)),
                label = NULL,
                choices = schema_choices,
                selected = if (!is.na(mapped_to)) mapped_to else "",
                width = "100%"
              ),
              shiny::uiOutput(session$ns(paste0("desc_", user_col)))
            ),

            # Status indicator
            shiny::column(
              2,
              shiny::div(
                shiny::icon(status_info$icon, style = sprintf("color: %s;", status_info$color)),
                shiny::br(),
                shiny::tags$small(status_info$label, style = sprintf("color: %s;", status_info$color)),
                style = "text-align: center; padding-top: 10px;"
              )
            )
          )
        )
      })

      shiny::tagList(mapping_rows)
    })

    # Reactive descriptions for each column
    shiny::observe({
      shiny::req(data(), auto_mappings(), config())

      user_cols <- names(auto_mappings()$mappings)
      column_descriptions <- config()$import_config$column_descriptions
      if (is.null(column_descriptions)) {
        column_descriptions <- list()
      }

      # Create a reactive description output for each user column
      lapply(user_cols, function(user_col) {
        output_id <- paste0("desc_", user_col)

        output[[output_id]] <- shiny::renderUI({
          # Get currently selected mapping
          mapped_to <- input[[paste0("map_", user_col)]]

          # Return NULL if no mapping or skipped
          if (is.null(mapped_to) || mapped_to == "") {
            return(NULL)
          }

          # Get column info
          col_info <- column_descriptions[[mapped_to]]
          if (is.null(col_info)) {
            return(NULL)
          }

          # Build description UI
          desc_parts <- list(
            shiny::tags$small(
              shiny::icon("info-circle", style = "color: #007bff;"),
              " ",
              col_info$description,
              style = "color: #6c757d;"
            )
          )

          # Add factor levels if available (for traits)
          if (!is.null(col_info$factorlevels) && col_info$factorlevels != "") {
            desc_parts <- c(desc_parts, list(
              shiny::br(),
              shiny::tags$small(
                shiny::icon("list"),
                " Expected values: ",
                shiny::tags$code(col_info$factorlevels, style = "font-size: 10px;"),
                style = "color: #856404;"
              )
            ))
          }

          # Add expected unit if available (for traits)
          if (!is.null(col_info$expectedunit) && col_info$expectedunit != "") {
            desc_parts <- c(desc_parts, list(
              shiny::br(),
              shiny::tags$small(
                shiny::icon("ruler"),
                " Unit: ",
                shiny::tags$strong(col_info$expectedunit),
                style = "color: #28a745;"
              )
            ))
          }

          shiny::div(
            style = "margin-top: 8px; padding: 8px; background-color: #f0f8ff; border-radius: 4px; border-left: 3px solid #007bff;",
            desc_parts
          )
        })
      })
    })

    # Collect current mappings from UI
    current_mappings <- shiny::reactive({
      shiny::req(data(), auto_mappings())

      user_cols <- names(auto_mappings()$mappings)
      mappings <- list()

      for (user_col in user_cols) {
        map_to <- input[[paste0("map_", user_col)]]

        if (!is.null(map_to) && map_to != "") {
          mappings[[user_col]] <- map_to
        }
      }

      mappings
    })

    # Validation - check if required columns are mapped
    mapping_validation <- shiny::reactive({
      shiny::req(config(), current_mappings())

      required_cols <- config()$required_columns
      mapped_db_cols <- unlist(current_mappings())

      missing_required <- setdiff(required_cols, mapped_db_cols)

      list(
        valid = length(missing_required) == 0,
        missing_required = missing_required,
        n_mapped = length(current_mappings())
      )
    })

    # Validation UI
    output$mapping_validation <- shiny::renderUI({
      shiny::req(mapping_validation())

      val <- mapping_validation()

      if (val$valid) {
        shiny::div(
          class = "alert alert-success",
          style = "margin-top: 30px;",
          shiny::icon("check-circle"),
          shiny::strong(" Mapping Complete: "),
          sprintf(
            "All %d required columns are mapped. You have mapped %d columns total.",
            length(config()$required_columns),
            val$n_mapped
          )
        )
      } else {
        shiny::div(
          class = "alert alert-danger",
          style = "margin-top: 30px;",
          shiny::icon("exclamation-circle"),
          shiny::strong(" Missing Required Columns: "),
          sprintf(
            "You must map the following required columns: %s",
            paste(shiny::tags$code(val$missing_required), collapse = ", ")
          )
        )
      }
    })

    # Return mappings and validation status
    return(
      shiny::reactive({
        list(
          mappings = current_mappings(),
          validation = mapping_validation()
        )
      })
    )
  })
}
