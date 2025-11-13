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
#' @param language Reactive returning current language ("en" or "fr")
#'
#' @return Reactive list with $column (selected column name), $include_authors (logical), and $data (potentially modified data)
#'
#' @keywords internal
mod_column_select_server <- function(id, data, initial_column = NULL, language = shiny::reactive("en")) {
  shiny::moduleServer(id, function(input, output, session) {

    # Get translations
    t <- shiny::reactive({
      get_translations(language())
    })

    # Module title
    output$title <- shiny::renderText({
      t()$column_select_title
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
            shiny::p(t()$msg_error, "No character columns found in data")
          )
        )
      }

      # Determine selected column
      selected_col <- if (!is.null(initial_column) && initial_column %in% char_cols) {
        initial_column
      } else {
        char_cols[1]
      }

      shiny::tagList(
        shiny::radioButtons(
          inputId = ns("column_mode"),
          label = shiny::strong("Column structure:"),
          choices = c(
            "Single column (all taxonomic info in one column)" = "single",
            "Multiple columns (genus, species, family separated)" = "multiple"
          ),
          selected = "single"
        ),

        # Single column mode
        shiny::conditionalPanel(
          condition = "input.column_mode == 'single'",
          ns = ns,
          shiny::selectInput(
            inputId = ns("column_name"),
            label = t()$column_select_name,
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
              shiny::strong("Select columns for each taxonomic component:"),
              style = "margin-top: 0;"
            ),
            shiny::helpText("The app will create a combined column using available information (genus + species epithet, or genus only, or family only)."),

            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = ns("genus_column"),
                  label = "Genus column:",
                  choices = c("(none)" = "", char_cols),
                  selected = ""
                )
              ),
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = ns("species_column"),
                  label = "Species epithet column:",
                  choices = c("(none)" = "", char_cols),
                  selected = ""
                )
              ),
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = ns("family_column"),
                  label = "Family column:",
                  choices = c("(none)" = "", char_cols),
                  selected = ""
                )
              )
            )
          )
        ),

        shiny::checkboxInput(
          inputId = ns("include_authors"),
          label = t()$column_match_authors,
          value = FALSE
        ),
        shiny::helpText(t()$column_match_authors_help)
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

    # Return reactive list
    return(
      shiny::reactive({
        req(input$column_mode)

        if (input$column_mode == "single") {
          list(
            column = input$column_name,
            include_authors = input$include_authors %||% FALSE,
            data = data()  # Return original data
          )
        } else {
          # Multiple column mode
          req(processed_data())

          list(
            column = "taxonomic_name_combined",
            include_authors = input$include_authors %||% FALSE,
            data = processed_data()  # Return data with combined column
          )
        }
      })
    )
  })
}
