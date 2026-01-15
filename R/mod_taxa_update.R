#' Taxa Update Module - UI
#'
#' UI component for updating existing taxonomic records
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_taxa_update_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("update_ui"))
  )
}

#' Taxa Update Module - Server
#'
#' Server logic for updating taxonomic records
#'
#' @param id Module namespace ID
#' @param pool Reactive returning taxa database connection pool
#' @param selected_taxon Reactive returning selected taxon data from search module
#' @param has_write_permission Reactive returning TRUE if user can write
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL
#'
#' @keywords internal
#' @export
mod_taxa_update_server <- function(id, pool, selected_taxon, has_write_permission, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      modified_fields = list(),
      show_form = FALSE
    )

    # Main UI
    output$update_ui <- shiny::renderUI({
      if (!has_write_permission()) {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("lock"),
            " ",
            i18n()$t("Write Access:"),
            " ",
            i18n()$t("Only users with INSERT privileges can modify data")
          )
        )
      }

      if (is.null(selected_taxon()) || nrow(selected_taxon()) == 0) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("Please select a taxon from the Browse & Search tab first")
          )
        )
      }

      shiny::tagList(
        shiny::h4(i18n()$t("Update Existing Taxon")),

        # Show selected taxon
        shiny::wellPanel(
          style = "background-color: #e7f3ff;",
          shiny::h5(i18n()$t("Selected Taxon")),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::strong("ID:"), " ", selected_taxon()$idtax_n, shiny::br(),
              shiny::strong(i18n()$t("Family:")), " ", if (is.null(selected_taxon()$tax_fam)) "N/A" else selected_taxon()$tax_fam, shiny::br(),
              shiny::strong(i18n()$t("Genus:")), " ", if (is.null(selected_taxon()$tax_gen)) "N/A" else selected_taxon()$tax_gen, shiny::br(),
              shiny::strong(i18n()$t("Species:")), " ", if (is.null(selected_taxon()$tax_esp)) "N/A" else selected_taxon()$tax_esp
            ),
            shiny::column(
              6,
              shiny::strong(i18n()$t("Order:")), " ", if (is.null(selected_taxon()$tax_order)) "N/A" else selected_taxon()$tax_order, shiny::br(),
              shiny::strong(i18n()$t("Class:")), " ", if (is.null(selected_taxon()$tax_famclass)) "N/A" else selected_taxon()$tax_famclass, shiny::br(),
              if (!is.null(selected_taxon()$idtax_good_n) && !is.na(selected_taxon()$idtax_good_n)) {
                shiny::tagList(
                  shiny::strong(i18n()$t("Status:")),
                  shiny::span(
                    class = "synonym-indicator",
                    " ",
                    i18n()$t("Synonym"),
                    " (→ ID: ", selected_taxon()$idtax_good_n, ")"
                  )
                )
              } else {
                shiny::tagList(
                  shiny::strong(i18n()$t("Status:")),
                  " ",
                  i18n()$t("Accepted")
                )
              }
            )
          ),
          shiny::hr(),
          shiny::actionButton(
            ns("btn_start_edit"),
            i18n()$t("Edit This Taxon"),
            icon = shiny::icon("edit"),
            class = "btn-primary"
          )
        ),

        # Edit form (conditionally shown)
        shiny::conditionalPanel(
          condition = "output.show_form == true",
          ns = ns,
          shiny::wellPanel(
            shiny::h5(i18n()$t("Edit Taxonomic Fields")),
            shiny::p(
              class = "text-muted",
              i18n()$t("Modify the fields you want to update. Leave others unchanged.")
            ),

            shiny::fluidRow(
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_gen"),
                  i18n()$t("Genus"),
                  value = ""
                )
              ),
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_esp"),
                  i18n()$t("Species epithet"),
                  value = ""
                )
              ),
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_fam"),
                  i18n()$t("Family"),
                  value = ""
                )
              )
            ),

            shiny::fluidRow(
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_order"),
                  i18n()$t("Order"),
                  value = ""
                )
              ),
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_famclass"),
                  i18n()$t("Class"),
                  value = ""
                )
              )
            ),

            shiny::h6(i18n()$t("Infraspecific (if applicable)")),
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::selectInput(
                  ns("new_tax_rank1"),
                  i18n()$t("Infraspecific rank"),
                  choices = c("None" = "", "var." = "var.", "subsp." = "subsp.", "f." = "f."),
                  selected = ""
                )
              ),
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_name1"),
                  i18n()$t("Infraspecific name"),
                  value = ""
                )
              )
            ),

            shiny::div(
              id = ns("modified_fields_display"),
              shiny::uiOutput(ns("modified_fields_ui"))
            ),

            shiny::hr(),

            shiny::fluidRow(
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_cancel"),
                  i18n()$t("Cancel"),
                  icon = shiny::icon("times"),
                  class = "btn-secondary btn-block"
                )
              ),
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_submit_update"),
                  i18n()$t("Submit Changes"),
                  icon = shiny::icon("save"),
                  class = "btn-success btn-block"
                )
              )
            )
          )
        )
      )
    })

    # Output for conditional panel
    output$show_form <- shiny::reactive({
      rv$show_form
    })
    shiny::outputOptions(output, "show_form", suspendWhenHidden = FALSE)

    # Start edit
    shiny::observeEvent(input$btn_start_edit, {
      taxon <- selected_taxon()

      # Prefill form with current values
      shiny::updateTextInput(session, "new_tax_gen", value = if (is.null(taxon$tax_gen)) "" else taxon$tax_gen)
      shiny::updateTextInput(session, "new_tax_esp", value = if (is.null(taxon$tax_esp)) "" else taxon$tax_esp)
      shiny::updateTextInput(session, "new_tax_fam", value = if (is.null(taxon$tax_fam)) "" else taxon$tax_fam)
      shiny::updateTextInput(session, "new_tax_order", value = if (is.null(taxon$tax_order)) "" else taxon$tax_order)
      shiny::updateTextInput(session, "new_tax_famclass", value = if (is.null(taxon$tax_famclass)) "" else taxon$tax_famclass)
      shiny::updateSelectInput(session, "new_tax_rank1", selected = if (is.null(taxon$tax_rank1)) "" else taxon$tax_rank1)
      shiny::updateTextInput(session, "new_tax_name1", value = if (is.null(taxon$tax_name1)) "" else taxon$tax_name1)

      rv$show_form <- TRUE
    })

    # Cancel edit
    shiny::observeEvent(input$btn_cancel, {
      rv$show_form <- FALSE
      rv$modified_fields <- list()
    })

    # Track field changes
    shiny::observe({
      shiny::req(rv$show_form)
      taxon <- selected_taxon()

      modified <- list()

      # Check each field for changes
      if (!is.null(input$new_tax_gen) && trimws(input$new_tax_gen) != (if (is.null(taxon$tax_gen)) "" else taxon$tax_gen)) {
        modified$tax_gen <- list(old = taxon$tax_gen, new = trimws(input$new_tax_gen))
      }
      if (!is.null(input$new_tax_esp) && trimws(input$new_tax_esp) != (if (is.null(taxon$tax_esp)) "" else taxon$tax_esp)) {
        modified$tax_esp <- list(old = taxon$tax_esp, new = trimws(input$new_tax_esp))
      }
      if (!is.null(input$new_tax_fam) && trimws(input$new_tax_fam) != (if (is.null(taxon$tax_fam)) "" else taxon$tax_fam)) {
        modified$tax_fam <- list(old = taxon$tax_fam, new = trimws(input$new_tax_fam))
      }
      if (!is.null(input$new_tax_order) && trimws(input$new_tax_order) != (if (is.null(taxon$tax_order)) "" else taxon$tax_order)) {
        modified$tax_order <- list(old = taxon$tax_order, new = trimws(input$new_tax_order))
      }
      if (!is.null(input$new_tax_famclass) && trimws(input$new_tax_famclass) != (if (is.null(taxon$tax_famclass)) "" else taxon$tax_famclass)) {
        modified$tax_famclass <- list(old = taxon$tax_famclass, new = trimws(input$new_tax_famclass))
      }
      if (!is.null(input$new_tax_rank1) && input$new_tax_rank1 != (if (is.null(taxon$tax_rank1)) "" else taxon$tax_rank1)) {
        modified$tax_rank1 <- list(old = taxon$tax_rank1, new = input$new_tax_rank1)
      }
      if (!is.null(input$new_tax_name1) && trimws(input$new_tax_name1) != (if (is.null(taxon$tax_name1)) "" else taxon$tax_name1)) {
        modified$tax_name1 <- list(old = taxon$tax_name1, new = trimws(input$new_tax_name1))
      }

      rv$modified_fields <- modified
    })

    # Display modified fields
    output$modified_fields_ui <- shiny::renderUI({
      if (length(rv$modified_fields) == 0) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("No changes yet - modify fields above to see changes")
          )
        )
      }

      shiny::div(
        class = "alert alert-warning",
        shiny::h6(i18n()$t("Modified Fields:")),
        shiny::tags$ul(
          lapply(names(rv$modified_fields), function(field) {
            change <- rv$modified_fields[[field]]
            shiny::tags$li(
              shiny::strong(field, ":"),
              " ",
              shiny::code(if (is.null(change$old)) "NULL" else change$old),
              " → ",
              shiny::code(if (change$new == "") "NULL" else change$new)
            )
          })
        )
      )
    })

    # Submit update
    shiny::observeEvent(input$btn_submit_update, {
      if (length(rv$modified_fields) == 0) {
        shiny::showNotification(
          i18n()$t("No changes to submit"),
          type = "warning"
        )
        return()
      }

      taxon <- selected_taxon()

      shiny::withProgress({
        tryCatch({
          cli::cli_alert_info("Updating taxon ID {taxon$idtax_n}...")

          # Prepare parameters for update_dico_name
          update_params <- list(
            id_searched = taxon$idtax_n,
            ask_before_update = FALSE,
            add_backup = TRUE,
            show_results = FALSE
          )

          # Add only modified fields
          if (!is.null(rv$modified_fields$tax_gen)) {
            update_params$new_tax_gen <- rv$modified_fields$tax_gen$new
          }
          if (!is.null(rv$modified_fields$tax_esp)) {
            update_params$new_tax_esp <- if (rv$modified_fields$tax_esp$new == "") NA else rv$modified_fields$tax_esp$new
          }
          if (!is.null(rv$modified_fields$tax_fam)) {
            update_params$new_tax_fam <- rv$modified_fields$tax_fam$new
          }
          if (!is.null(rv$modified_fields$tax_order)) {
            update_params$new_tax_order <- if (rv$modified_fields$tax_order$new == "") NA else rv$modified_fields$tax_order$new
          }
          if (!is.null(rv$modified_fields$tax_famclass)) {
            update_params$new_tax_famclass <- if (rv$modified_fields$tax_famclass$new == "") NA else rv$modified_fields$tax_famclass$new
          }
          if (!is.null(rv$modified_fields$tax_rank1)) {
            update_params$new_tax_rank1 <- if (rv$modified_fields$tax_rank1$new == "") NA else rv$modified_fields$tax_rank1$new
          }
          if (!is.null(rv$modified_fields$tax_name1)) {
            update_params$new_tax_name1 <- if (rv$modified_fields$tax_name1$new == "") NA else rv$modified_fields$tax_name1$new
          }

          # Call update_dico_name
          do.call(update_dico_name, update_params)

          shiny::showNotification(
            i18n()$t("Taxon updated successfully!"),
            type = "message",
            duration = 5
          )

          # Reset form
          rv$show_form <- FALSE
          rv$modified_fields <- list()

        }, error = function(e) {
          cli::cli_alert_danger("Failed to update taxon: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error updating taxon:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Updating taxon..."))
    })

    return(NULL)
  })
}
