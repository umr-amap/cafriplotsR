# Individual Search Module
#
# Search and select individuals from the database by plot, tag, or taxonomy
#
# Part of the specimen linking Shiny app system

#' Individual Search Module - UI
#'
#' @param id Character, module namespace ID
#' @param i18n shiny.i18n translator object
#'
#' @return Shiny UI element
#' @export
mod_individual_search_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::selectizeInput(
          inputId = ns("plot"),
          label = i18n$t("Plot"),
          choices = NULL,
          options = list(
            placeholder = i18n$t("Select a plot..."),
            maxOptions = 100
          )
        )
      ),
      shiny::column(
        width = 3,
        shiny::textInput(
          inputId = ns("tag"),
          label = i18n$t("Tag"),
          value = "",
          placeholder = "e.g., 123"
        )
      ),
      shiny::column(
        width = 3,
        shiny::textInput(
          inputId = ns("code_individu"),
          label = i18n$t("Individual Code"),
          value = "",
          placeholder = "e.g., A1-T1"
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
        width = 6,
        shiny::textInput(
          inputId = ns("taxon_filter"),
          label = i18n$t("Filter by taxon name"),
          value = "",
          placeholder = i18n$t("Enter genus or species name...")
        )
      ),
      shiny::column(
        width = 6,
        shiny::checkboxInput(
          inputId = ns("only_unlinked"),
          label = i18n$t("Only show individuals without specimen links"),
          value = FALSE
        )
      )
    ),
    shiny::hr(),
    shiny::div(
      style = "margin-bottom: 10px;",
      shiny::uiOutput(ns("search_summary"))
    ),
    DT::dataTableOutput(ns("individuals_table")),
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


#' Individual Search Module - Server
#'
#' @param id Character, module namespace ID
#' @param pool_main Reactive returning database connection pool
#' @param pool_taxa Reactive returning taxa database connection pool
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return Reactive list with selected individuals
#' @export
mod_individual_search_server <- function(id, pool_main, pool_taxa, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    search_results <- shiny::reactiveVal(NULL)
    selected_individuals <- shiny::reactiveVal(NULL)
    plots_list <- shiny::reactiveVal(NULL)

    # Initialize plots list
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

        # Get plots from data_liste_plots
        plots <- DBI::dbGetQuery(
          actual_con,
          "SELECT id_liste_plots, plot_name FROM data_liste_plots ORDER BY plot_name"
        )

        choices <- stats::setNames(plots$id_liste_plots, plots$plot_name)
        plots_list(choices)

        shiny::updateSelectizeInput(
          session,
          "plot",
          choices = c(setNames("", ""), choices),
          server = TRUE
        )

      }, error = function(e) {
        cli::cli_alert_warning("Could not load plots: {e$message}")
      })
    })

    # Search individuals
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

      # Build query - include all herbarium-related columns for preview
      query <- dplyr::tbl(actual_con, "data_individuals") %>%
        dplyr::select(id_n, id_table_liste_plots_n, tag, code_individu,
                      idtax_n, dbh,
                      herbarium_nbe_char, herbarium_code_char, herbarium_nbe_type)

      # Filter by plot
      if (!is.null(input$plot) && input$plot != "") {
        plot_id <- as.integer(input$plot)
        query <- query %>%
          dplyr::filter(id_table_liste_plots_n == plot_id)
      }

      # Filter by tag
      if (!is.null(input$tag) && input$tag != "") {
        query <- query %>%
          dplyr::filter(tag == !!input$tag)
      }

      # Filter by code_individu
      if (!is.null(input$code_individu) && input$code_individu != "") {
        query <- query %>%
          dplyr::filter(code_individu == !!input$code_individu)
      }

      # Collect results (limit to 500 for performance)
      results <- query %>%
        dplyr::collect() %>%
        utils::head(500)

      if (nrow(results) == 0) {
        shiny::showNotification(
          i18n()$t("No individuals found matching criteria"),
          type = "warning"
        )
        search_results(NULL)
        return()
      }

      # Filter to only unlinked if requested
      if (input$only_unlinked) {
        linked_ids <- tryCatch({
          DBI::dbGetQuery(
            actual_con,
            "SELECT DISTINCT id_n FROM data_link_specimens"
          )$id_n
        }, error = function(e) integer(0))

        results <- results %>%
          dplyr::filter(!id_n %in% linked_ids)

        if (nrow(results) == 0) {
          shiny::showNotification(
            i18n()$t("All matching individuals already have specimen links"),
            type = "info"
          )
          search_results(NULL)
          return()
        }
      }

      # Get plot names
      plot_names <- DBI::dbGetQuery(
        actual_con,
        "SELECT id_liste_plots, plot_name FROM data_liste_plots"
      )

      results <- results %>%
        dplyr::left_join(
          plot_names,
          by = c("id_table_liste_plots_n" = "id_liste_plots")
        )

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

      # Format individual label
      results <- results %>%
        dplyr::mutate(
          individual_label = paste0(
            plot_name, " - ",
            ifelse(!is.na(tag), paste0("Tag ", tag), ""),
            ifelse(!is.na(code_individu), paste0(" (", code_individu, ")"), "")
          ),
          dbh_display = ifelse(!is.na(dbh), paste0(round(dbh, 1), " cm"), "-")
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
        i18n()$t("individuals found")
      )
    })

    # Results table
    output$individuals_table <- DT::renderDataTable({
      results <- search_results()
      shiny::req(results)

      display_data <- results %>%
        dplyr::select(
          dplyr::any_of(c(
            "id_n",
            "individual_label",
            "full_name_no_auth",
            "tax_fam",
            "dbh_display",
            "herbarium_code_char",
            "herbarium_nbe_type",
            "herbarium_nbe_char"
          ))
        ) %>%
        dplyr::rename(
          ID = id_n,
          Individual = individual_label,
          Taxon = dplyr::any_of("full_name_no_auth"),
          Family = dplyr::any_of("tax_fam"),
          DBH = dbh_display,
          `Herb. Code` = dplyr::any_of("herbarium_code_char"),
          `Herb. Type` = dplyr::any_of("herbarium_nbe_type"),
          `Herb. Ref.` = dplyr::any_of("herbarium_nbe_char")
        )

      DT::datatable(
        display_data,
        selection = "multiple",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          columnDefs = list(
            list(width = '80px', targets = 0),  # ID column
            list(width = '200px', targets = 1),  # Individual column
            list(width = '150px', targets = 2),  # Taxon column
            list(width = '120px', targets = 5:7)  # Herbarium columns
          ),
          language = list(
            search = i18n()$t("Search:"),
            lengthMenu = paste(i18n()$t("Show"), "_MENU_", i18n()$t("entries")),
            info = paste(i18n()$t("Showing"), "_START_", i18n()$t("to"), "_END_",
                         i18n()$t("of"), "_TOTAL_", i18n()$t("entries"))
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          columns = c("Herb. Code", "Herb. Type", "Herb. Ref."),
          backgroundColor = DT::styleEqual(c(NA, ""), c("#f8f9fa", "#f8f9fa"), default = "#d4edda"),
          fontWeight = DT::styleEqual(c(NA, ""), c("normal", "normal"), default = "bold")
        )
    })

    # Enable/disable select button based on selection
    shiny::observe({
      selected_rows <- input$individuals_table_rows_selected

      if (length(selected_rows) > 0) {
        shinyjs::enable("btn_select")
      } else {
        shinyjs::disable("btn_select")
      }
    })

    # Handle selection
    shiny::observeEvent(input$btn_select, {
      selected_rows <- input$individuals_table_rows_selected
      shiny::req(length(selected_rows) > 0)

      results <- search_results()
      shiny::req(results)

      selected <- results[selected_rows, ]
      selected_individuals(selected)

      shiny::showNotification(
        paste(nrow(selected), i18n()$t("individuals added to link list")),
        type = "message"
      )
    })

    # Return selected individuals
    return(selected_individuals)
  })
}
