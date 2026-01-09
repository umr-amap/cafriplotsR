# Specimen Search Module
#
# Search and select specimens from the database by collector, number, or taxonomy
#
# Part of the specimen linking Shiny app system

#' Specimen Search Module - UI
#'
#' @param id Character, module namespace ID
#' @param i18n shiny.i18n translator object
#'
#' @return Shiny UI element
#' @export
mod_specimen_search_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::selectizeInput(
          inputId = ns("collector"),
          label = i18n$t("Collector"),
          choices = NULL,
          options = list(
            placeholder = i18n$t("Type to search..."),
            maxOptions = 50
          )
        )
      ),
      shiny::column(
        width = 3,
        shiny::numericInput(
          inputId = ns("colnbr"),
          label = i18n$t("Specimen Number"),
          value = NULL,
          min = 1
        )
      ),
      shiny::column(
        width = 3,
        shiny::textInput(
          inputId = ns("suffix"),
          label = i18n$t("Suffix"),
          value = "",
          placeholder = "A, B, bis..."
        )
      ),
      shiny::column(
        width = 2,
        shiny::div(
          style = "margin-top: 25px;",
          shiny::actionButton(
            inputId = ns("btn_search"),
            label = i18n$t("Search"),
            icon = shiny::icon("search"),
            class = "btn-primary"
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::textInput(
          inputId = ns("taxon_filter"),
          label = i18n$t("Filter by taxon name"),
          value = "",
          placeholder = i18n$t("Enter genus or species name...")
        )
      )
    ),
    shiny::hr(),
    shiny::div(
      style = "margin-bottom: 10px;",
      shiny::uiOutput(ns("search_summary"))
    ),
    DT::dataTableOutput(ns("specimens_table")),
    shiny::div(
      style = "margin-top: 10px;",
      shiny::actionButton(
        inputId = ns("btn_select"),
        label = i18n$t("Add selected to link list"),
        icon = shiny::icon("plus"),
        class = "btn-success",
        disabled = TRUE
      )
    )
  )
}


#' Specimen Search Module - Server
#'
#' @param id Character, module namespace ID
#' @param pool_main Reactive returning database connection pool
#' @param pool_taxa Reactive returning taxa database connection pool
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return Reactive list with selected specimens
#' @export
mod_specimen_search_server <- function(id, pool_main, pool_taxa, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    search_results <- shiny::reactiveVal(NULL)
    selected_specimens <- shiny::reactiveVal(NULL)
    collectors_list <- shiny::reactiveVal(NULL)

    # Initialize collectors list
    shiny::observe({
      shiny::req(pool_main())

      tryCatch({
        con <- pool_main()
        actual_con <- if (inherits(con, "Pool")) {
          pool::poolCheckout(con)
        } else {
          con
        }

        on.exit({
          if (inherits(con, "Pool") && !is.null(actual_con)) {
            pool::poolReturn(actual_con)
          }
        }, add = TRUE)

        # Get collectors from table_colnam
        collectors <- DBI::dbGetQuery(
          actual_con,
          "SELECT id_table_colnam, colnam FROM table_colnam ORDER BY colnam"
        )

        choices <- stats::setNames(collectors$id_table_colnam, collectors$colnam)
        collectors_list(choices)

        shiny::updateSelectizeInput(
          session,
          "collector",
          choices = c(setNames("", ""), choices),
          server = TRUE
        )

      }, error = function(e) {
        cli::cli_alert_warning("Could not load collectors: {e$message}")
      })
    })

    # Search specimens
    shiny::observeEvent(input$btn_search, {
      shiny::req(pool_main())

      con <- pool_main()
      actual_con <- if (inherits(con, "Pool")) {
        pool::poolCheckout(con)
      } else {
        con
      }

      on.exit({
        if (inherits(con, "Pool") && !is.null(actual_con)) {
          pool::poolReturn(actual_con)
        }
      }, add = TRUE)

      # Build query
      query <- dplyr::tbl(actual_con, "specimens") %>%
        dplyr::select(id_specimen, id_colnam, colnbr, suffix, idtax_n,
                      detby, detd, detm, dety)

      # Filter by collector
      if (!is.null(input$collector) && input$collector != "") {
        collector_id <- as.integer(input$collector)
        query <- query %>%
          dplyr::filter(id_colnam == collector_id)
      }

      # Filter by number
      if (!is.null(input$colnbr) && !is.na(input$colnbr)) {
        query <- query %>%
          dplyr::filter(colnbr == !!input$colnbr)
      }

      # Filter by suffix
      if (!is.null(input$suffix) && input$suffix != "") {
        query <- query %>%
          dplyr::filter(suffix == !!input$suffix)
      }

      # Collect results (limit to 500 for performance)
      results <- query %>%
        dplyr::collect() %>%
        utils::head(500)

      if (nrow(results) == 0) {
        shiny::showNotification(
          i18n()$t("No specimens found matching criteria"),
          type = "warning"
        )
        search_results(NULL)
        return()
      }

      # Get collector names
      collector_names <- DBI::dbGetQuery(
        actual_con,
        "SELECT id_table_colnam, colnam FROM table_colnam"
      )

      results <- results %>%
        dplyr::left_join(
          collector_names,
          by = c("id_colnam" = "id_table_colnam")
        ) %>%
        dplyr::rename(collector_name = colnam)

      # Get taxon names from taxa database
      if (!is.null(pool_taxa()) && any(!is.na(results$idtax_n))) {
        con_taxa <- pool_taxa()
        actual_con_taxa <- if (inherits(con_taxa, "Pool")) {
          pool::poolCheckout(con_taxa)
        } else {
          con_taxa
        }

        on.exit({
          if (inherits(con_taxa, "Pool") && !is.null(actual_con_taxa)) {
            pool::poolReturn(actual_con_taxa)
          }
        }, add = TRUE)

        taxa_names <- tryCatch({
          DBI::dbGetQuery(
            actual_con_taxa,
            paste0(
              "SELECT idtax_n, tax_gen, tax_esp, tax_fam, full_name_no_auth ",
              "FROM table_taxa WHERE idtax_n IN (",
              paste(unique(stats::na.omit(results$idtax_n)), collapse = ","),
              ")"
            )
          )
        }, error = function(e) {
          cli::cli_alert_warning("Could not fetch taxon names: {e$message}")
          NULL
        })

        if (!is.null(taxa_names) && nrow(taxa_names) > 0) {
          results <- results %>%
            dplyr::left_join(taxa_names, by = "idtax_n")
        }
      }

      # Filter by taxon name if provided
      if (!is.null(input$taxon_filter) && input$taxon_filter != "") {
        filter_pattern <- tolower(input$taxon_filter)
        results <- results %>%
          dplyr::filter(
            grepl(filter_pattern, tolower(full_name_no_auth), fixed = FALSE) |
              grepl(filter_pattern, tolower(tax_gen), fixed = FALSE)
          )
      }

      # Format specimen label
      results <- results %>%
        dplyr::mutate(
          specimen_label = paste0(
            collector_name, " ",
            colnbr,
            ifelse(!is.na(suffix) & suffix != "", paste0(" ", suffix), "")
          ),
          det_date = paste0(
            ifelse(!is.na(dety), dety, ""),
            ifelse(!is.na(detm), paste0("-", sprintf("%02d", detm)), ""),
            ifelse(!is.na(detd), paste0("-", sprintf("%02d", detd)), "")
          )
        )

      search_results(results)
    })

    # Search summary
    output$search_summary <- shiny::renderUI({
      results <- search_results()

      if (is.null(results)) {
        return(shiny::p(i18n()$t("Enter search criteria and click Search")))
      }

      shiny::div(
        class = "alert alert-info",
        shiny::strong(nrow(results)),
        " ",
        i18n()$t("specimens found")
      )
    })

    # Results table
    output$specimens_table <- DT::renderDataTable({
      results <- search_results()
      shiny::req(results)

      display_data <- results %>%
        dplyr::select(
          id_specimen,
          specimen_label,
          full_name_no_auth,
          tax_fam,
          det_date
        ) %>%
        dplyr::rename(
          ID = id_specimen,
          Specimen = specimen_label,
          Taxon = full_name_no_auth,
          Family = tax_fam,
          `Det. Date` = det_date
        )

      DT::datatable(
        display_data,
        selection = "multiple",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(
            search = i18n()$t("Search:"),
            lengthMenu = paste(i18n()$t("Show"), "_MENU_", i18n()$t("entries")),
            info = paste(i18n()$t("Showing"), "_START_", i18n()$t("to"), "_END_",
                         i18n()$t("of"), "_TOTAL_", i18n()$t("entries"))
          )
        ),
        rownames = FALSE
      )
    })

    # Enable/disable select button based on selection
    shiny::observe({
      selected_rows <- input$specimens_table_rows_selected

      if (length(selected_rows) > 0) {
        shinyjs::enable("btn_select")
      } else {
        shinyjs::disable("btn_select")
      }
    })

    # Handle selection
    shiny::observeEvent(input$btn_select, {
      selected_rows <- input$specimens_table_rows_selected
      shiny::req(length(selected_rows) > 0)

      results <- search_results()
      shiny::req(results)

      selected <- results[selected_rows, ]
      selected_specimens(selected)

      shiny::showNotification(
        paste(nrow(selected), i18n()$t("specimens added to link list")),
        type = "message"
      )
    })

    # Return selected specimens
    return(selected_specimens)
  })
}
