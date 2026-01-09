# Specimen Add Module
#
# Add new specimens to the database
#
# Part of the specimen linking Shiny app system

#' Specimen Add Module - UI
#'
#' @param id Character, module namespace ID
#' @param i18n shiny.i18n translator object
#'
#' @return Shiny UI element
#' @export
mod_specimen_add_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      shiny::h4(i18n$t("Add New Specimen")),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::selectizeInput(
            inputId = ns("collector"),
            label = i18n$t("Collector"),
            choices = NULL,
            options = list(
              placeholder = i18n$t("Select collector...")
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
          width = 2,
          shiny::textInput(
            inputId = ns("suffix"),
            label = i18n$t("Suffix"),
            value = "",
            placeholder = "A, B..."
          )
        ),
        shiny::column(
          width = 3,
          shiny::textInput(
            inputId = ns("taxon_search"),
            label = i18n$t("Taxon name"),
            value = "",
            placeholder = i18n$t("Search taxon...")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::uiOutput(ns("taxon_results"))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::selectizeInput(
            inputId = ns("detby"),
            label = i18n$t("Determined by"),
            choices = NULL,
            options = list(
              placeholder = i18n$t("Select determiner...")
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::numericInput(
            inputId = ns("dety"),
            label = i18n$t("Det. Year"),
            value = as.integer(format(Sys.Date(), "%Y")),
            min = 1900,
            max = as.integer(format(Sys.Date(), "%Y"))
          )
        ),
        shiny::column(
          width = 2,
          shiny::numericInput(
            inputId = ns("detm"),
            label = i18n$t("Month"),
            value = as.integer(format(Sys.Date(), "%m")),
            min = 1,
            max = 12
          )
        ),
        shiny::column(
          width = 2,
          shiny::numericInput(
            inputId = ns("detd"),
            label = i18n$t("Day"),
            value = as.integer(format(Sys.Date(), "%d")),
            min = 1,
            max = 31
          )
        ),
        shiny::column(
          width = 2,
          shiny::div(
            style = "margin-top: 25px;",
            shiny::actionButton(
              inputId = ns("btn_add"),
              label = i18n$t("Add specimen"),
              icon = shiny::icon("plus"),
              class = "btn-success"
            )
          )
        )
      ),
      shiny::hr(),
      shiny::uiOutput(ns("add_result"))
    )
  )
}


#' Specimen Add Module - Server
#'
#' @param id Character, module namespace ID
#' @param pool_main Reactive returning database connection pool
#' @param pool_taxa Reactive returning taxa database connection pool
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return Reactive with newly added specimen info
#' @export
mod_specimen_add_server <- function(id, pool_main, pool_taxa, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    selected_taxon <- shiny::reactiveVal(NULL)
    taxon_search_results <- shiny::reactiveVal(NULL)
    newly_added_specimen <- shiny::reactiveVal(NULL)

    # Initialize collector lists
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

        # Get collectors
        collectors <- DBI::dbGetQuery(
          actual_con,
          "SELECT id_table_colnam, colnam FROM table_colnam ORDER BY colnam"
        )

        choices <- stats::setNames(collectors$id_table_colnam, collectors$colnam)

        shiny::updateSelectizeInput(
          session,
          "collector",
          choices = c(setNames("", ""), choices),
          server = TRUE
        )

        shiny::updateSelectizeInput(
          session,
          "detby",
          choices = c(setNames("", ""), choices),
          server = TRUE
        )

      }, error = function(e) {
        cli::cli_alert_warning("Could not load collectors: {e$message}")
      })
    })

    # Search taxa on input change (debounced)
    shiny::observeEvent(input$taxon_search, {
      search_term <- input$taxon_search

      if (is.null(search_term) || nchar(search_term) < 3) {
        taxon_search_results(NULL)
        return()
      }

      shiny::req(pool_taxa())

      con_taxa <- pool_taxa()
      actual_con <- if (inherits(con_taxa, "Pool")) {
        pool::poolCheckout(con_taxa)
      } else {
        con_taxa
      }

      on.exit({
        if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
          pool::poolReturn(actual_con)
        }
      }, add = TRUE)

      # Search in table_taxa
      results <- tryCatch({
        DBI::dbGetQuery(
          actual_con,
          paste0(
            "SELECT idtax_n, tax_gen, tax_esp, tax_fam, full_name_no_auth ",
            "FROM table_taxa ",
            "WHERE LOWER(full_name_no_auth) LIKE '%", tolower(search_term), "%' ",
            "OR LOWER(tax_gen) LIKE '%", tolower(search_term), "%' ",
            "ORDER BY full_name_no_auth ",
            "LIMIT 20"
          )
        )
      }, error = function(e) {
        cli::cli_alert_warning("Taxon search failed: {e$message}")
        NULL
      })

      taxon_search_results(results)
    }, ignoreInit = TRUE)

    # Display taxon search results
    output$taxon_results <- shiny::renderUI({
      results <- taxon_search_results()

      if (is.null(results) || nrow(results) == 0) {
        if (!is.null(input$taxon_search) && nchar(input$taxon_search) >= 3) {
          return(shiny::p(
            class = "text-muted",
            i18n()$t("No taxa found matching search")
          ))
        }
        return(NULL)
      }

      ns <- session$ns

      # Create clickable list of taxa
      shiny::div(
        style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; padding: 5px;",
        lapply(seq_len(nrow(results)), function(i) {
          row <- results[i, ]
          shiny::actionLink(
            inputId = ns(paste0("select_taxon_", i)),
            label = shiny::div(
              shiny::strong(row$full_name_no_auth),
              shiny::br(),
              shiny::tags$small(
                class = "text-muted",
                row$tax_fam
              )
            ),
            style = "display: block; padding: 5px; border-bottom: 1px solid #eee;"
          )
        })
      )
    })

    # Handle taxon selection (create observers for each result)
    shiny::observe({
      results <- taxon_search_results()
      shiny::req(results)

      lapply(seq_len(nrow(results)), function(i) {
        shiny::observeEvent(input[[paste0("select_taxon_", i)]], {
          row <- results[i, ]
          selected_taxon(row)

          # Update the search input to show selected taxon
          shiny::updateTextInput(
            session,
            "taxon_search",
            value = row$full_name_no_auth
          )

          # Clear search results
          taxon_search_results(NULL)

          shiny::showNotification(
            paste(i18n()$t("Selected:"), row$full_name_no_auth),
            type = "message",
            duration = 2
          )
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Add specimen
    shiny::observeEvent(input$btn_add, {
      # Validate required fields
      if (is.null(input$collector) || input$collector == "") {
        shiny::showNotification(
          i18n()$t("Collector is required"),
          type = "error"
        )
        return()
      }

      if (is.null(input$colnbr) || is.na(input$colnbr)) {
        shiny::showNotification(
          i18n()$t("Specimen number is required"),
          type = "error"
        )
        return()
      }

      taxon <- selected_taxon()
      if (is.null(taxon)) {
        shiny::showNotification(
          i18n()$t("Please select a taxon"),
          type = "error"
        )
        return()
      }

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

      # Check if specimen already exists
      existing <- DBI::dbGetQuery(
        actual_con,
        paste0(
          "SELECT id_specimen FROM specimens ",
          "WHERE id_colnam = ", input$collector,
          " AND colnbr = ", input$colnbr,
          ifelse(input$suffix != "", paste0(" AND suffix = '", input$suffix, "'"), "")
        )
      )

      if (nrow(existing) > 0) {
        shiny::showNotification(
          i18n()$t("This specimen already exists in the database"),
          type = "error",
          duration = 5
        )
        return()
      }

      # Prepare new specimen data
      new_specimen <- data.frame(
        id_colnam = as.integer(input$collector),
        colnbr = as.integer(input$colnbr),
        suffix = ifelse(input$suffix != "", input$suffix, NA_character_),
        idtax_n = taxon$idtax_n,
        detby = ifelse(!is.null(input$detby) && input$detby != "",
                       as.integer(input$detby), NA_integer_),
        dety = input$dety,
        detm = input$detm,
        detd = input$detd,
        stringsAsFactors = FALSE
      )

      # Insert into database
      tryCatch({
        DBI::dbWriteTable(
          actual_con,
          "specimens",
          new_specimen,
          append = TRUE,
          row.names = FALSE
        )

        # Get the ID of the newly inserted specimen
        new_id <- DBI::dbGetQuery(
          actual_con,
          paste0(
            "SELECT id_specimen FROM specimens ",
            "WHERE id_colnam = ", input$collector,
            " AND colnbr = ", input$colnbr,
            ifelse(input$suffix != "", paste0(" AND suffix = '", input$suffix, "'"), ""),
            " ORDER BY id_specimen DESC LIMIT 1"
          )
        )$id_specimen[1]

        # Get collector name for display
        collector_name <- DBI::dbGetQuery(
          actual_con,
          paste0("SELECT colnam FROM table_colnam WHERE id_table_colnam = ", input$collector)
        )$colnam[1]

        result <- list(
          id_specimen = new_id,
          collector_name = collector_name,
          colnbr = input$colnbr,
          suffix = input$suffix,
          taxon = taxon$full_name_no_auth,
          idtax_n = taxon$idtax_n,
          tax_gen = taxon$tax_gen,
          tax_fam = taxon$tax_fam
        )

        newly_added_specimen(result)

        shiny::showNotification(
          paste(
            i18n()$t("Specimen added successfully:"),
            collector_name, input$colnbr
          ),
          type = "message",
          duration = 5
        )

        # Reset form
        shiny::updateNumericInput(session, "colnbr", value = NA)
        shiny::updateTextInput(session, "suffix", value = "")
        shiny::updateTextInput(session, "taxon_search", value = "")
        selected_taxon(NULL)

      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("Error adding specimen:"), e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Display add result
    output$add_result <- shiny::renderUI({
      result <- newly_added_specimen()

      if (is.null(result)) {
        return(NULL)
      }

      shiny::div(
        class = "alert alert-success",
        shiny::icon("check-circle"),
        " ",
        shiny::strong(i18n()$t("Last added:")),
        " ",
        paste0(result$collector_name, " ", result$colnbr),
        ifelse(!is.na(result$suffix) && result$suffix != "",
               paste0(" ", result$suffix), ""),
        " - ",
        result$taxon,
        shiny::br(),
        shiny::tags$small(
          "ID: ", result$id_specimen
        )
      )
    })

    # Return newly added specimen
    return(newly_added_specimen)
  })
}
