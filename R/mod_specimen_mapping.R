# Specimen Import Wizard - Step 2: Column Mapping
#
# Module for mapping user columns to database specimen columns

#' Specimen Mapping Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_specimen_mapping_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("columns"),
      i18n$t("Step 2: Map Your Columns"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Match your data columns to the database fields. Required fields are marked with *"),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Auto-detect button
    shiny::actionButton(
      ns("auto_detect"),
      shiny::tagList(shiny::icon("magic"), paste0(" ", i18n$t("Auto-detect Columns"))),
      class = "btn-info",
      style = "margin-bottom: 20px;"
    ),

    # Mapping interface
    shiny::div(
      class = "mapping-container",
      style = "background: #f8f9fa; padding: 20px; border-radius: 8px;",

      # Required fields section
      shiny::h4(
        shiny::icon("asterisk", style = "color: #dc3545;"),
        paste0(" ", i18n$t("Required Fields")),
        style = "margin-bottom: 15px;"
      ),

      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("col_collector"),
            shiny::tagList(i18n$t("Collector"), " *"),
            choices = NULL,
            width = "100%"
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(
            ns("col_colnbr"),
            shiny::tagList(i18n$t("Specimen Number"), " *"),
            choices = NULL,
            width = "100%"
          )
        )
      ),

      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("col_idtax_n"),
            shiny::tagList(i18n$t("Taxonomic ID (idtax_n)"), " *"),
            choices = NULL,
            width = "100%"
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(
            ns("col_suffix"),
            i18n$t("Suffix (optional)"),
            choices = NULL,
            width = "100%"
          )
        )
      ),

      shiny::hr(),

      # Optional fields section
      shiny::h4(
        shiny::icon("plus-circle", style = "color: #6c757d;"),
        paste0(" ", i18n$t("Optional Fields")),
        style = "margin-bottom: 15px;"
      ),

      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("col_det_by"),
            i18n$t("Determined by"),
            choices = NULL,
            width = "100%"
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(
            ns("col_det_year"),
            i18n$t("Determination Year"),
            choices = NULL,
            width = "100%"
          )
        )
      ),

      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("col_det_month"),
            i18n$t("Determination Month"),
            choices = NULL,
            width = "100%"
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(
            ns("col_det_day"),
            i18n$t("Determination Day"),
            choices = NULL,
            width = "100%"
          )
        )
      )
    ),

    # Mapping validation
    shiny::uiOutput(ns("mapping_validation"))
  )
}


#' Specimen Mapping Module - Server
#'
#' @param id Module namespace ID
#' @param data Reactive containing uploaded user data
#' @param con Reactive database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive list containing column mappings
#' @keywords internal
#' @export
mod_specimen_mapping_server <- function(id, data, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Store mappings
    mappings <- shiny::reactiveVal(NULL)
    mapping_valid <- shiny::reactiveVal(FALSE)

    # Update column choices when data is loaded
    shiny::observe({
      shiny::req(data())

      user_cols <- names(data())
      # Remove internal columns
      user_cols <- user_cols[!user_cols %in% c("_row_id")]

      # Add empty option
      choices <- c(setNames("", paste0("-- ", i18n()$t("Not mapped"), " --")),
                   setNames(user_cols, user_cols))

      # Update all select inputs
      shiny::updateSelectInput(session, "col_collector", choices = choices)
      shiny::updateSelectInput(session, "col_colnbr", choices = choices)
      shiny::updateSelectInput(session, "col_idtax_n", choices = choices)
      shiny::updateSelectInput(session, "col_suffix", choices = choices)
      shiny::updateSelectInput(session, "col_det_by", choices = choices)
      shiny::updateSelectInput(session, "col_det_year", choices = choices)
      shiny::updateSelectInput(session, "col_det_month", choices = choices)
      shiny::updateSelectInput(session, "col_det_day", choices = choices)
    })

    # Auto-detect columns based on common naming patterns
    shiny::observeEvent(input$auto_detect, {
      shiny::req(data())

      user_cols <- names(data())
      user_cols_lower <- tolower(user_cols)

      # Collector patterns
      collector_patterns <- c("collector", "collecteur", "col_name", "colnam", "coll")
      collector_match <- .find_column_match(user_cols, user_cols_lower, collector_patterns)
      if (!is.null(collector_match)) {
        shiny::updateSelectInput(session, "col_collector", selected = collector_match)
      }

      # Number patterns
      number_patterns <- c("colnbr", "number", "numero", "num", "no", "nbr", "col_nbr")
      number_match <- .find_column_match(user_cols, user_cols_lower, number_patterns)
      if (!is.null(number_match)) {
        shiny::updateSelectInput(session, "col_colnbr", selected = number_match)
      }

      # Taxon ID patterns
      idtax_patterns <- c("idtax_n", "idtax", "taxon_id", "tax_id", "id_tax")
      idtax_match <- .find_column_match(user_cols, user_cols_lower, idtax_patterns)
      if (!is.null(idtax_match)) {
        shiny::updateSelectInput(session, "col_idtax_n", selected = idtax_match)
      }

      # Suffix patterns
      suffix_patterns <- c("suffix", "suffixe", "suf")
      suffix_match <- .find_column_match(user_cols, user_cols_lower, suffix_patterns)
      if (!is.null(suffix_match)) {
        shiny::updateSelectInput(session, "col_suffix", selected = suffix_match)
      }

      # Det by patterns
      det_by_patterns <- c("det_by", "detby", "determined_by", "determinateur", "det")
      det_by_match <- .find_column_match(user_cols, user_cols_lower, det_by_patterns)
      if (!is.null(det_by_match)) {
        shiny::updateSelectInput(session, "col_det_by", selected = det_by_match)
      }

      # Year patterns
      year_patterns <- c("det_year", "dety", "year", "annee", "det_y")
      year_match <- .find_column_match(user_cols, user_cols_lower, year_patterns)
      if (!is.null(year_match)) {
        shiny::updateSelectInput(session, "col_det_year", selected = year_match)
      }

      # Month patterns
      month_patterns <- c("det_month", "detm", "month", "mois", "det_m")
      month_match <- .find_column_match(user_cols, user_cols_lower, month_patterns)
      if (!is.null(month_match)) {
        shiny::updateSelectInput(session, "col_det_month", selected = month_match)
      }

      # Day patterns
      day_patterns <- c("det_day", "detd", "day", "jour", "det_d")
      day_match <- .find_column_match(user_cols, user_cols_lower, day_patterns)
      if (!is.null(day_match)) {
        shiny::updateSelectInput(session, "col_det_day", selected = day_match)
      }

      shiny::showNotification(
        i18n()$t("Auto-detection complete. Please verify the mappings."),
        type = "message",
        duration = 3
      )
    })

    # Validate mappings and update reactive
    shiny::observe({
      # Check required fields
      collector_ok <- !is.null(input$col_collector) && input$col_collector != ""
      colnbr_ok <- !is.null(input$col_colnbr) && input$col_colnbr != ""
      idtax_ok <- !is.null(input$col_idtax_n) && input$col_idtax_n != ""

      is_valid <- collector_ok && colnbr_ok && idtax_ok
      mapping_valid(is_valid)

      if (is_valid) {
        # Build mappings object
        current_mappings <- list(
          collector = input$col_collector,
          colnbr = input$col_colnbr,
          idtax_n = input$col_idtax_n,
          suffix = if (!is.null(input$col_suffix) && input$col_suffix != "") input$col_suffix else NULL,
          det_by = if (!is.null(input$col_det_by) && input$col_det_by != "") input$col_det_by else NULL,
          det_year = if (!is.null(input$col_det_year) && input$col_det_year != "") input$col_det_year else NULL,
          det_month = if (!is.null(input$col_det_month) && input$col_det_month != "") input$col_det_month else NULL,
          det_day = if (!is.null(input$col_det_day) && input$col_det_day != "") input$col_det_day else NULL
        )
        mappings(current_mappings)
      } else {
        mappings(NULL)
      }
    })

    # Mapping validation display
    output$mapping_validation <- shiny::renderUI({
      collector_ok <- !is.null(input$col_collector) && input$col_collector != ""
      colnbr_ok <- !is.null(input$col_colnbr) && input$col_colnbr != ""
      idtax_ok <- !is.null(input$col_idtax_n) && input$col_idtax_n != ""

      all_ok <- collector_ok && colnbr_ok && idtax_ok

      shiny::div(
        style = "margin-top: 20px;",

        if (all_ok) {
          shiny::div(
            class = "alert alert-success",
            shiny::icon("check-circle"),
            " ",
            i18n()$t("All required fields are mapped. You can proceed to the next step.")
          )
        } else {
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            " ",
            i18n()$t("Please map all required fields:"),
            shiny::tags$ul(
              if (!collector_ok) shiny::tags$li(i18n()$t("Collector column is required")),
              if (!colnbr_ok) shiny::tags$li(i18n()$t("Specimen number column is required")),
              if (!idtax_ok) shiny::tags$li(i18n()$t("Taxonomic ID (idtax_n) column is required"))
            )
          )
        }
      )
    })

    # Return mappings and validity
    return(list(
      mappings = mappings,
      is_valid = mapping_valid
    ))
  })
}


#' Find Column Match (Internal Helper)
#'
#' @param user_cols Original column names
#' @param user_cols_lower Lowercase column names
#' @param patterns Patterns to match
#' @return Matched column name or NULL
#' @keywords internal
.find_column_match <- function(user_cols, user_cols_lower, patterns) {
  for (pattern in patterns) {
    # Exact match
    idx <- which(user_cols_lower == pattern)
    if (length(idx) > 0) {
      return(user_cols[idx[1]])
    }

    # Partial match
    idx <- grep(pattern, user_cols_lower, fixed = TRUE)
    if (length(idx) > 0) {
      return(user_cols[idx[1]])
    }
  }
  return(NULL)
}
