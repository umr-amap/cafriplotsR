#' Taxa Synonymy Module - UI
#'
#' UI component for managing taxonomic synonymy
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_taxa_synonymy_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("synonymy_ui"))
  )
}

#' Taxa Synonymy Module - Server
#'
#' Server logic for managing synonymy relationships
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
mod_taxa_synonymy_server <- function(id, pool, selected_taxon, has_write_permission, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      show_set_synonym_form = FALSE,
      show_cancel_form = FALSE
    )

    # Main UI
    output$synonymy_ui <- shiny::renderUI({
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

      taxon <- selected_taxon()
      is_synonym <- !is.null(taxon$idtax_good_n) && !is.na(taxon$idtax_good_n)

      shiny::tagList(
        shiny::h4(i18n()$t("Synonymy Management")),

        # Show selected taxon
        shiny::wellPanel(
          style = if (is_synonym) "background-color: #fff3cd;" else "background-color: #d4edda;",
          shiny::h5(i18n()$t("Selected Taxon")),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::strong("ID:"), " ", taxon$idtax_n, shiny::br(),
              shiny::strong(i18n()$t("Family:")), " ", if (is.null(taxon$tax_fam)) "N/A" else taxon$tax_fam, shiny::br(),
              shiny::strong(i18n()$t("Genus:")), " ", if (is.null(taxon$tax_gen)) "N/A" else taxon$tax_gen, shiny::br(),
              shiny::strong(i18n()$t("Species:")), " ", if (is.null(taxon$tax_esp)) "N/A" else taxon$tax_esp
            ),
            shiny::column(
              6,
              shiny::strong(i18n()$t("Status:")),
              " ",
              if (is_synonym) {
                shiny::tagList(
                  shiny::span(
                    class = "synonym-indicator",
                    shiny::icon("link"),
                    " ",
                    i18n()$t("Synonym")
                  ),
                  shiny::br(),
                  shiny::strong(i18n()$t("Accepted taxon ID:")),
                  " ",
                  taxon$idtax_good_n
                )
              } else {
                shiny::span(
                  style = "color: #28a745; font-weight: bold;",
                  shiny::icon("check-circle"),
                  " ",
                  i18n()$t("Accepted name")
                )
              }
            )
          ),
          shiny::hr(),

          # Action buttons
          shiny::fluidRow(
            shiny::column(
              6,
              if (!is_synonym) {
                shiny::actionButton(
                  ns("btn_set_synonym"),
                  i18n()$t("Set as Synonym"),
                  icon = shiny::icon("link"),
                  class = "btn-warning btn-block"
                )
              } else {
                shiny::tags$button(
                  id = ns("btn_set_synonym_disabled"),
                  class = "btn btn-secondary btn-block",
                  disabled = "disabled",
                  style = "opacity: 0.6; cursor: not-allowed;",
                  shiny::icon("link"),
                  " ",
                  i18n()$t("Already a synonym")
                )
              }
            ),
            shiny::column(
              6,
              if (is_synonym) {
                shiny::actionButton(
                  ns("btn_cancel_synonym"),
                  i18n()$t("Cancel Synonymy"),
                  icon = shiny::icon("unlink"),
                  class = "btn-success btn-block"
                )
              } else {
                shiny::tags$button(
                  id = ns("btn_cancel_disabled"),
                  class = "btn btn-secondary btn-block",
                  disabled = "disabled",
                  style = "opacity: 0.6; cursor: not-allowed;",
                  shiny::icon("unlink"),
                  " ",
                  i18n()$t("Not a synonym")
                )
              }
            )
          )
        ),

        # Set synonym form (conditionally shown)
        shiny::conditionalPanel(
          condition = "output.show_set_synonym_form == true",
          ns = ns,
          shiny::wellPanel(
            shiny::h5(i18n()$t("Set Taxon as Synonym")),
            shiny::p(
              class = "text-muted",
              i18n()$t("Provide information to identify the accepted taxon name")
            ),

            shiny::fluidRow(
              shiny::column(
                4,
                shiny::textInput(
                  ns("accepted_genus"),
                  i18n()$t("Accepted genus")
                )
              ),
              shiny::column(
                4,
                shiny::textInput(
                  ns("accepted_species"),
                  i18n()$t("Accepted species")
                )
              ),
              shiny::column(
                4,
                shiny::numericInput(
                  ns("accepted_id"),
                  i18n()$t("Or accepted taxon ID"),
                  value = NA
                )
              )
            ),

            shiny::div(
              class = "alert alert-warning",
              shiny::icon("exclamation-triangle"),
              " ",
              i18n()$t("This will mark the selected taxon as a synonym. All linked data will reference the accepted name.")
            ),

            shiny::hr(),

            shiny::fluidRow(
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_cancel_set"),
                  i18n()$t("Cancel"),
                  icon = shiny::icon("times"),
                  class = "btn-secondary btn-block"
                )
              ),
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_confirm_set_synonym"),
                  i18n()$t("Confirm - Set as Synonym"),
                  icon = shiny::icon("link"),
                  class = "btn-warning btn-block"
                )
              )
            )
          )
        ),

        # Cancel synonymy form (conditionally shown)
        shiny::conditionalPanel(
          condition = "output.show_cancel_form == true",
          ns = ns,
          shiny::wellPanel(
            shiny::h5(i18n()$t("Cancel Synonymy")),

            shiny::div(
              class = "alert alert-warning",
              shiny::icon("exclamation-triangle"),
              " ",
              i18n()$t("This will remove the synonym relationship and restore the taxon as an accepted name.")
            ),

            shiny::hr(),

            shiny::fluidRow(
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_cancel_cancel"),
                  i18n()$t("Cancel"),
                  icon = shiny::icon("times"),
                  class = "btn-secondary btn-block"
                )
              ),
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_confirm_cancel_synonym"),
                  i18n()$t("Confirm - Cancel Synonymy"),
                  icon = shiny::icon("unlink"),
                  class = "btn-success btn-block"
                )
              )
            )
          )
        )
      )
    })

    # Outputs for conditional panels
    output$show_set_synonym_form <- shiny::reactive({
      rv$show_set_synonym_form
    })
    shiny::outputOptions(output, "show_set_synonym_form", suspendWhenHidden = FALSE)

    output$show_cancel_form <- shiny::reactive({
      rv$show_cancel_form
    })
    shiny::outputOptions(output, "show_cancel_form", suspendWhenHidden = FALSE)

    # Show set synonym form
    shiny::observeEvent(input$btn_set_synonym, {
      rv$show_set_synonym_form <- TRUE
      rv$show_cancel_form <- FALSE
    })

    # Cancel set synonym
    shiny::observeEvent(input$btn_cancel_set, {
      rv$show_set_synonym_form <- FALSE
      shiny::updateTextInput(session, "accepted_genus", value = "")
      shiny::updateTextInput(session, "accepted_species", value = "")
      shiny::updateNumericInput(session, "accepted_id", value = NA)
    })

    # Show cancel synonym form
    shiny::observeEvent(input$btn_cancel_synonym, {
      rv$show_cancel_form <- TRUE
      rv$show_set_synonym_form <- FALSE
    })

    # Cancel cancel synonym
    shiny::observeEvent(input$btn_cancel_cancel, {
      rv$show_cancel_form <- FALSE
    })

    # Confirm set synonym
    shiny::observeEvent(input$btn_confirm_set_synonym, {
      taxon <- selected_taxon()

      # Validate inputs
      has_genus <- !is.null(input$accepted_genus) && nchar(trimws(input$accepted_genus)) > 0
      has_species <- !is.null(input$accepted_species) && nchar(trimws(input$accepted_species)) > 0
      has_id <- !is.null(input$accepted_id) && !is.na(input$accepted_id)

      if (!has_genus && !has_species && !has_id) {
        shiny::showNotification(
          i18n()$t("Please provide at least genus, species, or taxon ID of the accepted name"),
          type = "error"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          cli::cli_alert_info("Setting taxon ID {taxon$idtax_n} as synonym...")

          # Build synonym_of list
          synonym_of <- list()
          if (has_genus) synonym_of$genus <- trimws(input$accepted_genus)
          if (has_species) synonym_of$species <- trimws(input$accepted_species)
          if (has_id) synonym_of$id <- input$accepted_id

          # Call update_dico_name with synonym_of
          update_dico_name(
            id_searched = taxon$idtax_n,
            synonym_of = synonym_of,
            ask_before_update = FALSE,
            add_backup = TRUE,
            show_results = FALSE
          )

          shiny::showNotification(
            i18n()$t("Synonym relationship set successfully!"),
            type = "message",
            duration = 5
          )

          # Reset form
          rv$show_set_synonym_form <- FALSE
          shiny::updateTextInput(session, "accepted_genus", value = "")
          shiny::updateTextInput(session, "accepted_species", value = "")
          shiny::updateNumericInput(session, "accepted_id", value = NA)

        }, error = function(e) {
          cli::cli_alert_danger("Failed to set synonym: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error setting synonym:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Setting synonym relationship..."))
    })

    # Confirm cancel synonym
    shiny::observeEvent(input$btn_confirm_cancel_synonym, {
      taxon <- selected_taxon()

      shiny::withProgress({
        tryCatch({
          cli::cli_alert_info("Canceling synonymy for taxon ID {taxon$idtax_n}...")

          # Call update_dico_name with cancel_synonymy
          update_dico_name(
            id_searched = taxon$idtax_n,
            cancel_synonymy = TRUE,
            ask_before_update = FALSE,
            add_backup = TRUE,
            show_results = FALSE
          )

          shiny::showNotification(
            i18n()$t("Synonymy cancelled successfully!"),
            type = "message",
            duration = 5
          )

          # Reset form
          rv$show_cancel_form <- FALSE

        }, error = function(e) {
          cli::cli_alert_danger("Failed to cancel synonymy: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error cancelling synonymy:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Cancelling synonymy..."))
    })

    return(NULL)
  })
}
