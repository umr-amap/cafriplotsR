# Herbarium Parser Module
#
# Module for parsing herbarium_nbe_char and herbarium_nbe_type columns
# to extract collector names and specimen numbers

#' Herbarium Parser Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_herbarium_parser_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      class = "section-card",
      shiny::h3(
        shiny::icon("search"),
        " ",
        i18n$t("Step 2: Parse Herbarium Information"),
        style = "color: #495057; margin-bottom: 20px;"
      ),

      shiny::p(
        i18n$t("Extract collector names and specimen numbers from herbarium columns."),
        style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
      ),

      # Parse button
      shiny::actionButton(
        ns("parse_herbarium"),
        shiny::tagList(shiny::icon("cogs"), paste0(" ", i18n$t("Parse Herbarium Data"))),
        class = "btn-primary btn-lg",
        style = "margin-bottom: 30px;"
      ),

      # Parsing results
      shiny::uiOutput(ns("parsing_summary")),

      # Preview parsed data
      shiny::uiOutput(ns("parsed_data_preview"))
    )
  )
}


#' Herbarium Parser Module - Server
#'
#' @param id Module namespace ID
#' @param individuals_data Reactive containing individuals data from Step 1
#' @param i18n Reactive returning translator object
#' @return Reactive list containing parsed data
#' @keywords internal
#' @export
mod_herbarium_parser_server <- function(id, individuals_data, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for parsed data
    parsed_data <- shiny::reactiveVal(NULL)
    parsing_complete <- shiny::reactiveVal(FALSE)

    # Parse herbarium info when button clicked
    shiny::observeEvent(input$parse_herbarium, {
      shiny::req(individuals_data())

      individuals <- individuals_data()

      shiny::withProgress(message = i18n()$t("Parsing herbarium information..."), value = 0, {

        shiny::incProgress(0.3, detail = i18n()$t("Extracting collector names and numbers..."))

        tryCatch({
          # Parse herbarium columns
          parsed <- .parse_herbarium_columns(individuals)

          shiny::incProgress(0.8, detail = i18n()$t("Finalizing..."))

          if (nrow(parsed) == 0) {
            shiny::showNotification(
              i18n()$t("No valid herbarium information could be parsed."),
              type = "warning",
              duration = 5
            )
            parsing_complete(FALSE)
          } else {
            parsed_data(parsed)
            parsing_complete(TRUE)

            shiny::incProgress(1, detail = i18n()$t("Complete!"))

            shiny::showNotification(
              sprintf(
                i18n()$t("Successfully parsed %d potential links from %d individuals."),
                nrow(parsed),
                length(unique(parsed$id_n))
              ),
              type = "message",
              duration = 5
            )
          }
        }, error = function(e) {
          shiny::showNotification(
            paste(i18n()$t("Error parsing herbarium data:"), e$message),
            type = "error",
            duration = NULL
          )
          parsing_complete(FALSE)
        })
      })
    })

    # Render parsing summary
    output$parsing_summary <- shiny::renderUI({
      shiny::req(parsed_data())

      parsed <- parsed_data()
      n_individuals <- length(unique(parsed$id_n))
      n_type <- sum(parsed$link_type == "type_individual", na.rm = TRUE)
      n_ref <- sum(parsed$link_type == "referenced_individual", na.rm = TRUE)
      n_collectors <- length(unique(parsed$extracted_collector))

      shiny::tagList(
        shiny::h4(i18n()$t("Parsing Results"), style = "margin-top: 30px; margin-bottom: 15px;"),

        shiny::fluidRow(
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
              shiny::h3(n_individuals, style = "margin: 0; color: #007bff;"),
              shiny::p(i18n()$t("Individuals"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
              shiny::h3(n_type, style = "margin: 0; color: #28a745;"),
              shiny::p(i18n()$t("Type Specimens"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #ffc107; text-align: center;",
              shiny::h3(n_ref, style = "margin: 0; color: #ffc107;"),
              shiny::p(i18n()$t("Reference Specimens"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #6c757d; text-align: center;",
              shiny::h3(n_collectors, style = "margin: 0; color: #6c757d;"),
              shiny::p(i18n()$t("Unique Collectors"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          )
        ),

        shiny::hr(),

        shiny::div(
          class = "alert alert-success",
          shiny::icon("check-circle"),
          " ",
          i18n()$t("Parsing complete! You can proceed to match collectors.")
        )
      )
    })

    # Preview parsed data
    output$parsed_data_preview <- shiny::renderUI({
      shiny::req(parsed_data())

      parsed <- parsed_data()

      shiny::tagList(
        shiny::h4(i18n()$t("Parsed Data Preview"), style = "margin-top: 30px; margin-bottom: 15px;"),
        DT::dataTableOutput(session$ns("parsed_table"))
      )
    })

    output$parsed_table <- DT::renderDataTable({
      shiny::req(parsed_data())

      parsed <- parsed_data()

      # Select columns to display
      display_data <- parsed %>%
        dplyr::select(
          id_n, tag, plot_name,
          extracted_collector, extracted_number,
          link_type, source_column
        )

      DT::datatable(
        display_data,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle(
          "link_type",
          backgroundColor = DT::styleEqual(
            c("type_individual", "referenced_individual"),
            c("#d4edda", "#fff3cd")
          )
        )
    })

    # Return parsed data and status
    return(list(
      parsed_data = parsed_data,
      is_complete = parsing_complete
    ))
  })
}


#' Parse Herbarium Columns (Internal Helper)
#'
#' Extracts collector names and specimen numbers from herbarium_nbe_char
#' and herbarium_nbe_type columns. Determines link type based on column content.
#'
#' @param individuals Data frame with columns: id_n, herbarium_nbe_char, herbarium_nbe_type
#'
#' @return Data frame with parsed information
#' @keywords internal
.parse_herbarium_columns <- function(individuals) {

  # Initialize result list
  parsed_list <- list()

  for (i in seq_len(nrow(individuals))) {
    row <- individuals[i, ]

    herb_nbe_char <- row$herbarium_nbe_char
    herb_type <- row$herbarium_nbe_type

    # Skip if both are NA
    if ((is.na(herb_nbe_char) || herb_nbe_char == "") && (is.na(herb_type) || herb_type == "")) {
      next
    }

    # Parse herbarium_nbe_char - ALWAYS referenced_individual
    # This column indicates trees field-identified as same species as the specimen tree
    if (!is.na(herb_nbe_char) && herb_nbe_char != "") {
      parsed_nbe <- .extract_collector_and_number(herb_nbe_char)

      if (!is.null(parsed_nbe)) {
        parsed_list[[length(parsed_list) + 1]] <- data.frame(
          id_n = row$id_n,
          tag = row$tag,
          code_individu = row$code_individu,
          plot_name = row$plot_name,
          idtax_n = row$idtax_n,
          extracted_collector = parsed_nbe$collector,
          extracted_number = parsed_nbe$number,
          link_type = "referenced_individual",
          source_column = "herbarium_nbe_char",
          original_value = herb_nbe_char,
          stringsAsFactors = FALSE
        )
      }
    }

    # Parse herbarium_nbe_type - ALWAYS type_individual
    # This column indicates the ACTUAL tree where the specimen was collected
    if (!is.na(herb_type) && herb_type != "") {
      parsed_type <- .extract_collector_and_number(herb_type)

      if (!is.null(parsed_type)) {
        parsed_list[[length(parsed_list) + 1]] <- data.frame(
          id_n = row$id_n,
          tag = row$tag,
          code_individu = row$code_individu,
          plot_name = row$plot_name,
          idtax_n = row$idtax_n,
          extracted_collector = parsed_type$collector,
          extracted_number = parsed_type$number,
          link_type = "type_individual",
          source_column = "herbarium_nbe_type",
          original_value = herb_type,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Combine into data frame
  if (length(parsed_list) == 0) {
    return(data.frame())
  }

  parsed_df <- dplyr::bind_rows(parsed_list)

  # Deduplicate: for individuals with both link types, keep only type_individual
  # (type_individual has higher confidence than referenced_individual)
  parsed_df <- parsed_df %>%
    dplyr::group_by(id_n) %>%
    dplyr::filter(
      dplyr::n() == 1 | link_type == "type_individual"
    ) %>%
    dplyr::slice(1) %>%  # In case type_individual itself appears more than once
    dplyr::ungroup()

  return(parsed_df)
}


#' Extract Collector and Number from Herbarium String (Internal Helper)
#'
#' Parses a string like "Dauby 1234" into collector name and number.
#'
#' @param herbarium_string Character string with collector and number
#'
#' @return List with collector and number, or NULL if parsing fails
#' @keywords internal
.extract_collector_and_number <- function(herbarium_string) {

  if (is.na(herbarium_string) || herbarium_string == "") {
    return(NULL)
  }

  # Clean string
  cleaned <- herbarium_string
  cleaned <- stringr::str_replace(cleaned, "-", " ")
  cleaned <- stringr::str_replace_all(cleaned, "[.]", " ")

  # Extract number using readr::parse_number
  number <- tryCatch({
    num <- readr::parse_number(cleaned)
    # Handle negative numbers
    if (!is.na(num) && num < 1) {
      num <- num * -1
    }
    as.integer(num)
  }, error = function(e) {
    NA_integer_
  }, warning = function(w) {
    NA_integer_
  })

  # If no number found, return NULL
  if (is.na(number)) {
    return(NULL)
  }

  # Extract collector name by removing the number
  collector <- stringr::str_replace(cleaned, as.character(number), "")
  collector <- stringr::str_trim(collector)

  # If collector is empty, return NULL
  if (collector == "") {
    return(NULL)
  }

  return(list(
    collector = collector,
    number = number
  ))
}
