# Column Selection Module
#
# Allows user to select which column contains taxonomic names
# Supports both single column and multi-column (genus/species/family) modes

#' Column Select Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_column_select_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4(shiny::textOutput(ns("title"))),
    shiny::uiOutput(ns("column_controls"))
  )
}


#' Column Select Module - Server
#'
#' @param id Character, module ID
#' @param data Reactive data.frame from data input module
#' @param initial_column Character, optional pre-selected column name
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return Reactive list with $column (selected column name), $include_authors (logical), and $data (potentially modified data)
#'
#' @keywords internal
mod_column_select_server <- function(id, data, initial_column = NULL, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Module title
    output$title <- shiny::renderText({
      i18n()$t("Column Selection")
    })

    # Column selection controls
    output$column_controls <- shiny::renderUI({
      req(data())

      ns <- session$ns
      df <- data()

      # Get character columns
      char_cols <- names(df)[sapply(df, is.character)]

      if (length(char_cols) == 0) {
        return(
          shiny::div(
            style = "color: red;",
            shiny::p(i18n()$t("Error:"), i18n()$t("No character columns found in data"))
          )
        )
      }

      # Determine selected column
      selected_col <- if (!is.null(initial_column) && initial_column %in% char_cols) {
        initial_column
      } else {
        char_cols[1]
      }

      # Build choices vector for column mode
      mode_choices <- c("single", "multiple")
      names(mode_choices) <- c(
        i18n()$t("Single column (all taxonomic info in one column)"),
        i18n()$t("Multiple columns (genus, species, family separated)")
      )

      # Build choices with translated "(none)" option
      none_choice <- c("")
      names(none_choice) <- i18n()$t("(none)")

      shiny::tagList(
        shiny::radioButtons(
          inputId = ns("column_mode"),
          label = shiny::strong(i18n()$t("Column structure:")),
          choices = mode_choices,
          selected = "single"
        ),

        # Single column mode
        shiny::conditionalPanel(
          condition = "input.column_mode == 'single'",
          ns = ns,
          shiny::selectInput(
            inputId = ns("column_name"),
            label = i18n()$t("Select name column:"),
            choices = char_cols,
            selected = selected_col
          )
        ),

        # Multiple columns mode
        shiny::conditionalPanel(
          condition = "input.column_mode == 'multiple'",
          ns = ns,
          shiny::div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
            shiny::p(
              shiny::icon("info-circle"),
              shiny::strong(i18n()$t("Select columns for each taxonomic component:")),
              style = "margin-top: 0;"
            ),
            shiny::helpText(i18n()$t("The app will create a combined column using available information (genus + species epithet, or genus only, or family only).")),

            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = ns("genus_column"),
                  label = i18n()$t("Genus column:"),
                  choices = c(none_choice, char_cols),
                  selected = ""
                )
              ),
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = ns("species_column"),
                  label = i18n()$t("Species epithet column:"),
                  choices = c(none_choice, char_cols),
                  selected = ""
                )
              ),
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = ns("family_column"),
                  label = i18n()$t("Family column:"),
                  choices = c(none_choice, char_cols),
                  selected = ""
                )
              )
            )
          )
        ),

        shiny::checkboxInput(
          inputId = ns("include_authors"),
          label = i18n()$t("Match with author names"),
          value = FALSE
        ),
        shiny::helpText(i18n()$t("Include author names in matching (slower but more precise)"))
      )
    })

    # Reactive to create combined column if in multiple mode
    processed_data <- shiny::reactive({
      req(data())
      req(input$column_mode)

      df <- data()

      if (input$column_mode == "multiple") {
        req(input$genus_column, input$species_column, input$family_column)

        # Check that at least one column is selected
        if (input$genus_column == "" && input$species_column == "" && input$family_column == "") {
          return(NULL)
        }

        # Create combined taxonomic column
        df$taxonomic_name_combined <- apply(df, 1, function(row) {
          genus <- if (input$genus_column != "") as.character(row[input$genus_column]) else ""
          species <- if (input$species_column != "") as.character(row[input$species_column]) else ""
          family <- if (input$family_column != "") as.character(row[input$family_column]) else ""

          # Replace NA with empty string
          genus <- ifelse(is.na(genus), "", genus)
          species <- ifelse(is.na(species), "", species)
          family <- ifelse(is.na(family), "", family)

          # Trim whitespace
          genus <- trimws(genus)
          species <- trimws(species)
          family <- trimws(family)

          # Build taxonomic name according to hierarchy
          if (genus != "" && species != "") {
            paste(genus, species)
          } else if (genus != "") {
            genus
          } else if (family != "") {
            family
          } else {
            NA_character_
          }
        })

        return(df)
      } else {
        return(df)
      }
    })

    # Warn once per upload when the file already carries columns the matching
    # pipeline produces (an `idtax_n` from an earlier run is the common case).
    shiny::observeEvent(data(), {
      renamed <- .rename_conflicting_columns(data())$renamed

      if (length(renamed) > 0) {
        shiny::showNotification(
          paste0(
            i18n()$t("Existing columns renamed to avoid conflicts with the matching results:"),
            " ",
            paste(names(renamed), "\u2192", renamed, collapse = ", ")
          ),
          duration = 10,
          type = "warning"
        )
      }
    })

    # Return reactive list
    return(
      shiny::reactive({
        req(input$column_mode)

        if (input$column_mode == "single") {
          req(data())
          sanitised <- .rename_conflicting_columns(data())
          selected  <- input$column_name
        } else {
          # Multiple column mode
          req(processed_data())
          sanitised <- .rename_conflicting_columns(processed_data())
          selected  <- "taxonomic_name_combined"
        }

        # The selected column itself may have been renamed away from a
        # reserved name — follow it so the two stay in sync.
        if (selected %in% names(sanitised$renamed)) {
          selected <- unname(sanitised$renamed[[selected]])
        }

        list(
          column = selected,
          include_authors = input$include_authors %||% FALSE,
          data = sanitised$data
        )
      })
    )
  })
}
