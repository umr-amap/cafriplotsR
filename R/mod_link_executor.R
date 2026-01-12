# Link Executor Module
#
# Module for executing the creation of specimen-individual links
# in the database

#' Link Executor Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_link_executor_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      class = "section-card",
      shiny::h3(
        shiny::icon("link"),
        " ",
        i18n$t("Step 6: Create Links"),
        style = "color: #495057; margin-bottom: 20px;"
      ),

      shiny::p(
        i18n$t("Review and create the validated links between individuals and specimens in the database."),
        style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
      ),

      # Summary of links to create
      shiny::uiOutput(ns("links_summary")),

      # Dry run option
      shiny::div(
        style = "margin-top: 20px; margin-bottom: 20px;",
        shiny::checkboxInput(
          ns("dry_run"),
          shiny::tagList(
            shiny::icon("flask"),
            " ",
            i18n$t("Dry run (preview only, no database changes)")
          ),
          value = TRUE
        )
      ),

      # Create links button
      shiny::actionButton(
        ns("create_links"),
        shiny::tagList(shiny::icon("save"), paste0(" ", i18n$t("Create Links"))),
        class = "btn-success btn-lg",
        style = "margin-bottom: 30px;"
      ),

      # Execution results
      shiny::uiOutput(ns("execution_results"))
    )
  )
}


#' Link Executor Module - Server
#'
#' @param id Module namespace ID
#' @param validated_links Reactive containing validated links from Step 5
#' @param con Reactive database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive list containing execution results
#' @keywords internal
#' @export
mod_link_executor_server <- function(id, validated_links, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for execution results
    execution_results <- shiny::reactiveVal(NULL)
    execution_complete <- shiny::reactiveVal(FALSE)

    # Render links summary
    output$links_summary <- shiny::renderUI({
      shiny::req(validated_links())

      links <- validated_links()
      n_total <- nrow(links)
      n_type <- sum(links$link_type == "type_individual", na.rm = TRUE)
      n_ref <- sum(links$link_type == "referenced_individual", na.rm = TRUE)
      n_individuals <- length(unique(links$id_n))
      n_specimens <- length(unique(links$id_specimen))

      shiny::tagList(
        shiny::h4(i18n()$t("Links to Create"), style = "margin-bottom: 15px;"),

        shiny::fluidRow(
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
              shiny::h3(n_total, style = "margin: 0; color: #007bff;"),
              shiny::p(i18n()$t("Total Links"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
              shiny::h3(n_type, style = "margin: 0; color: #28a745;"),
              shiny::p(i18n()$t("Type Individual"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #ffc107; text-align: center;",
              shiny::h3(n_ref, style = "margin: 0; color: #ffc107;"),
              shiny::p(i18n()$t("Referenced Individual"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #17a2b8; text-align: center;",
              shiny::h3(sprintf("%d/%d", n_individuals, n_specimens), style = "margin: 0; color: #17a2b8; font-size: 1.5em;"),
              shiny::p(i18n()$t("Individuals/Specimens"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          )
        ),

        shiny::hr()
      )
    })

    # Create links when button clicked
    shiny::observeEvent(input$create_links, {
      shiny::req(validated_links(), con())

      links <- validated_links()
      dry_run <- input$dry_run

      shiny::withProgress(message = i18n()$t("Creating links..."), value = 0, {

        shiny::incProgress(0.2, detail = i18n()$t("Preparing data..."))

        tryCatch({
          # Map link_type to id_linktype
          linktypes <- get_linktypes(con())

          links_to_create <- links %>%
            dplyr::left_join(
              linktypes %>% dplyr::select(linktype, id_linktype),
              by = c("link_type" = "linktype")
            ) %>%
            dplyr::select(id_specimen, id_n, id_linktype)

          shiny::incProgress(0.4, detail = i18n()$t("Validating links..."))

          if (dry_run) {
            # Dry run - just show what would be created
            shiny::incProgress(1, detail = i18n()$t("Dry run complete"))

            execution_results(list(
              success = TRUE,
              dry_run = TRUE,
              n_links = nrow(links_to_create),
              preview = links_to_create
            ))

            shiny::showNotification(
              sprintf(i18n()$t("Dry run: %d links would be created"), nrow(links_to_create)),
              type = "message",
              duration = 5
            )

          } else {
            # Actual execution
            shiny::incProgress(0.6, detail = i18n()$t("Creating links in database..."))

            result <- .add_link_specimens(
              new_data = links_to_create,
              col_names_select = c("id_specimen", "id_n", "id_linktype"),
              launch_adding_data = TRUE,
              validate = TRUE,
              con = con()
            )

            shiny::incProgress(1, detail = i18n()$t("Complete!"))

            execution_results(list(
              success = TRUE,
              dry_run = FALSE,
              n_links = nrow(links_to_create),
              result = result
            ))

            execution_complete(TRUE)

            shiny::showNotification(
              sprintf(i18n()$t("Successfully created %d links!"), nrow(links_to_create)),
              type = "message",
              duration = 5
            )
          }

        }, error = function(e) {
          shiny::showNotification(
            paste(i18n()$t("Error creating links:"), e$message),
            type = "error",
            duration = NULL
          )

          execution_results(list(
            success = FALSE,
            dry_run = dry_run,
            error = e$message
          ))
        })
      })
    })

    # Render execution results
    output$execution_results <- shiny::renderUI({
      shiny::req(execution_results())

      results <- execution_results()

      if (results$dry_run) {
        shiny::tagList(
          shiny::h4(i18n()$t("Dry Run Results"), style = "margin-top: 30px; margin-bottom: 15px;"),

          shiny::div(
            class = "alert alert-info",
            shiny::icon("flask"),
            " ",
            sprintf(i18n()$t("%d links would be created in the database."), results$n_links),
            shiny::br(),
            shiny::br(),
            i18n()$t("Uncheck 'Dry run' and click 'Create Links' again to perform the actual operation.")
          ),

          shiny::h5(i18n()$t("Preview of links to create:")),
          DT::dataTableOutput(session$ns("dry_run_preview"))
        )

      } else if (results$success) {
        shiny::div(
          class = "alert alert-success",
          style = "margin-top: 30px;",
          shiny::h4(
            shiny::icon("check-circle"),
            " ",
            i18n()$t("Links Created Successfully!")
          ),
          shiny::p(
            sprintf(
              i18n()$t("%d links have been created between individuals and specimens."),
              results$n_links
            )
          ),
          shiny::p(
            shiny::tags$strong(i18n()$t("What's next?")),
            shiny::br(),
            i18n()$t("The links are now stored in the data_link_specimens table and can be queried using query_all_specimen_links().")
          )
        )

      } else {
        shiny::div(
          class = "alert alert-danger",
          style = "margin-top: 30px;",
          shiny::h4(
            shiny::icon("times-circle"),
            " ",
            i18n()$t("Link Creation Failed")
          ),
          shiny::p(results$error)
        )
      }
    })

    # Dry run preview table
    output$dry_run_preview <- DT::renderDataTable({
      shiny::req(execution_results())
      results <- execution_results()
      shiny::req(results$dry_run, !is.null(results$preview))

      DT::datatable(
        results$preview,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    # Return results
    return(list(
      results = execution_results,
      is_complete = execution_complete
    ))
  })
}
