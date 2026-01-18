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
      show_cancel_form = FALSE,
      show_reverse_form = FALSE,
      searched_accepted_taxa = NULL,
      selected_accepted_id = NULL,
      existing_synonyms = NULL,
      reverse_synonyms_info = NULL
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
              4,
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
              4,
              if (is_synonym) {
                shiny::actionButton(
                  ns("btn_reverse_synonym"),
                  i18n()$t("Reverse Synonym"),
                  icon = shiny::icon("exchange-alt"),
                  class = "btn-info btn-block"
                )
              } else {
                shiny::tags$button(
                  id = ns("btn_reverse_disabled"),
                  class = "btn btn-secondary btn-block",
                  disabled = "disabled",
                  style = "opacity: 0.6; cursor: not-allowed;",
                  shiny::icon("exchange-alt"),
                  " ",
                  i18n()$t("Not a synonym")
                )
              }
            ),
            shiny::column(
              4,
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
                5,
                shiny::textInput(
                  ns("accepted_binomial"),
                  i18n()$t("Accepted name (binomial)"),
                  placeholder = "Genus species"
                ),
                shiny::helpText(i18n()$t("Enter genus and species separated by space (e.g., 'Pinus alba')"))
              ),
              shiny::column(
                4,
                shiny::numericInput(
                  ns("accepted_id"),
                  i18n()$t("Or accepted taxon ID"),
                  value = NA
                ),
                shiny::helpText(i18n()$t("Directly enter the taxon ID if known"))
              ),
              shiny::column(
                3,
                shiny::br(),
                shiny::actionButton(
                  ns("btn_search_accepted"),
                  i18n()$t("Search"),
                  icon = shiny::icon("search"),
                  class = "btn-primary btn-block",
                  style = "margin-top: 5px;"
                )
              )
            ),

            # Search results
            shiny::uiOutput(ns("search_results_ui")),

            # Existing synonyms warning
            shiny::uiOutput(ns("existing_synonyms_ui")),

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
        ),

        # Reverse synonym form (conditionally shown)
        shiny::conditionalPanel(
          condition = "output.show_reverse_form == true",
          ns = ns,
          shiny::wellPanel(
            shiny::h5(i18n()$t("Reverse Synonym Relationship")),

            shiny::p(
              class = "text-muted",
              i18n()$t("This will make the selected taxon (currently a synonym) the accepted name, and the current accepted name will become its synonym.")
            ),

            # Show info about current accepted taxon
            shiny::uiOutput(ns("reverse_current_accepted_ui")),

            # Show warning about other synonyms that will be redirected
            shiny::uiOutput(ns("reverse_synonyms_warning_ui")),

            shiny::div(
              class = "alert alert-info",
              shiny::icon("info-circle"),
              " ",
              i18n()$t("All taxa currently pointing to the old accepted name will be updated to point to the new accepted name.")
            ),

            shiny::hr(),

            shiny::fluidRow(
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_cancel_reverse"),
                  i18n()$t("Cancel"),
                  icon = shiny::icon("times"),
                  class = "btn-secondary btn-block"
                )
              ),
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_confirm_reverse_synonym"),
                  i18n()$t("Confirm - Reverse Relationship"),
                  icon = shiny::icon("exchange-alt"),
                  class = "btn-info btn-block"
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

    output$show_reverse_form <- shiny::reactive({
      rv$show_reverse_form
    })
    shiny::outputOptions(output, "show_reverse_form", suspendWhenHidden = FALSE)

    # Show set synonym form
    shiny::observeEvent(input$btn_set_synonym, {
      rv$show_set_synonym_form <- TRUE
      rv$show_cancel_form <- FALSE
      rv$show_reverse_form <- FALSE
    })

    # Search for accepted taxon
    shiny::observeEvent(input$btn_search_accepted, {
      # Validate inputs
      has_binomial <- !is.null(input$accepted_binomial) && nchar(trimws(input$accepted_binomial)) > 0
      has_id <- !is.null(input$accepted_id) && !is.na(input$accepted_id)

      if (!has_binomial && !has_id) {
        shiny::showNotification(
          i18n()$t("Please provide binomial name or taxon ID"),
          type = "warning"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          # Get pool connection
          pool_conn <- pool()
          actual_con <- pool::poolCheckout(pool_conn)
          on.exit(pool::poolReturn(actual_con), add = TRUE)

          # Parse binomial if provided
          genus <- NULL
          species <- NULL
          if (has_binomial) {
            binomial_parts <- trimws(strsplit(trimws(input$accepted_binomial), "\\s+")[[1]])
            if (length(binomial_parts) >= 1) genus <- binomial_parts[1]
            if (length(binomial_parts) >= 2) species <- binomial_parts[2]
          }

          # Search taxa
          results <- NULL
          if (has_id) {
            results <- dplyr::tbl(actual_con, "table_taxa") %>%
              dplyr::filter(idtax_n == !!input$accepted_id) %>%
              dplyr::collect()
          } else if (!is.null(genus) && !is.null(species)) {
            results <- dplyr::tbl(actual_con, "table_taxa") %>%
              dplyr::filter(tax_gen == !!genus, tax_esp == !!species) %>%
              dplyr::collect()
          } else if (!is.null(genus)) {
            results <- dplyr::tbl(actual_con, "table_taxa") %>%
              dplyr::filter(tax_gen == !!genus, is.na(tax_esp)) %>%
              dplyr::collect()
          }

          if (is.null(results) || nrow(results) == 0) {
            shiny::showNotification(
              i18n()$t("No taxa found matching your search"),
              type = "warning"
            )
            rv$searched_accepted_taxa <- NULL
            rv$selected_accepted_id <- NULL
            return()
          }

          rv$searched_accepted_taxa <- results

          # Auto-select if only one result
          if (nrow(results) == 1) {
            rv$selected_accepted_id <- results$idtax_n[1]

            # Check for existing synonyms pointing to current taxon
            current_taxon <- selected_taxon()
            existing_syns <- dplyr::tbl(actual_con, "table_taxa") %>%
              dplyr::filter(idtax_good_n == !!current_taxon$idtax_n) %>%
              dplyr::select(idtax_n, tax_gen, tax_esp, tax_nam01, tax_rank01) %>%
              dplyr::collect()

            rv$existing_synonyms <- if (nrow(existing_syns) > 0) existing_syns else NULL

            shiny::showNotification(
              i18n()$t("Found 1 taxon - automatically selected"),
              type = "message"
            )
          } else {
            rv$selected_accepted_id <- NULL
            shiny::showNotification(
              paste(i18n()$t("Found"), nrow(results), i18n()$t("taxa - please select one")),
              type = "message"
            )
          }

        }, error = function(e) {
          cli::cli_alert_danger("Search failed: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Search error:"), e$message),
            type = "error"
          )
        })
      }, message = i18n()$t("Searching..."))
    })

    # Display search results
    output$search_results_ui <- shiny::renderUI({
      if (is.null(rv$searched_accepted_taxa)) return(NULL)

      results <- rv$searched_accepted_taxa

      if (nrow(results) == 1) {
        # Single result - show as info box
        taxon <- results[1, ]
        shiny::div(
          class = "alert alert-success",
          style = "margin-top: 15px;",
          shiny::h5(
            shiny::icon("check-circle"),
            " ",
            i18n()$t("Accepted taxon found")
          ),
          shiny::hr(),
          shiny::strong("ID:"), " ", taxon$idtax_n, shiny::br(),
          shiny::strong(i18n()$t("Family:")), " ", if (is.na(taxon$tax_fam)) "N/A" else taxon$tax_fam, shiny::br(),
          shiny::strong(i18n()$t("Genus:")), " ", if (is.na(taxon$tax_gen)) "N/A" else taxon$tax_gen, shiny::br(),
          shiny::strong(i18n()$t("Species:")), " ", if (is.na(taxon$tax_esp)) "N/A" else taxon$tax_esp, shiny::br(),
          if (!is.na(taxon$tax_rank01) && !is.na(taxon$tax_nam01)) {
            shiny::tagList(
              shiny::strong(i18n()$t("Infraspecific:")), " ",
              taxon$tax_rank01, " ", taxon$tax_nam01
            )
          }
        )
      } else {
        # Multiple results - show table with radio buttons
        shiny::div(
          style = "margin-top: 15px;",
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("Multiple taxa found - select the correct one:")
          ),
          shiny::wellPanel(
            shiny::radioButtons(
              ns("selected_accepted_radio"),
              i18n()$t("Select accepted taxon:"),
              choices = setNames(
                results$idtax_n,
                paste0(
                  "ID: ", results$idtax_n, " | ",
                  ifelse(is.na(results$tax_gen), "", results$tax_gen), " ",
                  ifelse(is.na(results$tax_esp), "", results$tax_esp), " ",
                  ifelse(is.na(results$tax_rank01), "", paste(results$tax_rank01, results$tax_nam01))
                )
              )
            )
          )
        )
      }
    })

    # Display existing synonyms warning
    output$existing_synonyms_ui <- shiny::renderUI({
      if (is.null(rv$existing_synonyms)) return(NULL)

      syns <- rv$existing_synonyms
      current_taxon <- selected_taxon()

      shiny::div(
        class = "alert alert-warning",
        style = "margin-top: 15px;",
        shiny::h5(
          shiny::icon("exclamation-triangle"),
          " ",
          i18n()$t("Warning: Existing synonyms will be updated")
        ),
        shiny::hr(),
        shiny::p(
          i18n()$t("The taxon you are setting as synonym currently has"),
          " ", shiny::strong(nrow(syns)), " ",
          i18n()$t("synonym(s) pointing to it.")
        ),
        shiny::p(
          i18n()$t("These synonyms will be automatically redirected to the new accepted name to avoid synonym chains.")
        ),
        shiny::h6(i18n()$t("Synonyms that will be updated:")),
        shiny::tags$ul(
          lapply(1:min(nrow(syns), 10), function(i) {
            syn <- syns[i, ]
            shiny::tags$li(
              sprintf("ID %d: %s %s %s %s",
                      syn$idtax_n,
                      if (is.na(syn$tax_gen)) "" else syn$tax_gen,
                      if (is.na(syn$tax_esp)) "" else syn$tax_esp,
                      if (is.na(syn$tax_rank01)) "" else syn$tax_rank01,
                      if (is.na(syn$tax_nam01)) "" else syn$tax_nam01
              )
            )
          })
        ),
        if (nrow(syns) > 10) {
          shiny::p(shiny::em(sprintf(i18n()$t("... and %d more"), nrow(syns) - 10)))
        }
      )
    })

    # Reverse synonym UI: Show current accepted taxon
    output$reverse_current_accepted_ui <- shiny::renderUI({
      if (is.null(rv$reverse_synonyms_info)) return(NULL)

      info <- rv$reverse_synonyms_info
      accepted <- info$accepted_taxon

      if (nrow(accepted) == 0) return(NULL)

      acc <- accepted[1, ]

      shiny::div(
        class = "alert alert-primary",
        style = "margin-top: 15px;",
        shiny::h6(
          shiny::icon("arrow-right"),
          " ",
          i18n()$t("Current accepted taxon (will become synonym):")
        ),
        shiny::hr(),
        shiny::strong("ID:"), " ", acc$idtax_n, shiny::br(),
        shiny::strong(i18n()$t("Family:")), " ", if (is.na(acc$tax_fam)) "N/A" else acc$tax_fam, shiny::br(),
        shiny::strong(i18n()$t("Genus:")), " ", if (is.na(acc$tax_gen)) "N/A" else acc$tax_gen, shiny::br(),
        shiny::strong(i18n()$t("Species:")), " ", if (is.na(acc$tax_esp)) "N/A" else acc$tax_esp
      )
    })

    # Reverse synonym UI: Show other synonyms warning
    output$reverse_synonyms_warning_ui <- shiny::renderUI({
      if (is.null(rv$reverse_synonyms_info)) return(NULL)

      info <- rv$reverse_synonyms_info
      other_syns <- info$other_synonyms

      if (is.null(other_syns) || nrow(other_syns) == 0) return(NULL)

      shiny::div(
        class = "alert alert-warning",
        style = "margin-top: 15px;",
        shiny::h6(
          shiny::icon("exclamation-triangle"),
          " ",
          i18n()$t("Warning: Other synonyms will be redirected")
        ),
        shiny::hr(),
        shiny::p(
          i18n()$t("There are"),
          " ", shiny::strong(nrow(other_syns)), " ",
          i18n()$t("other synonym(s) currently pointing to the same accepted name.")
        ),
        shiny::p(
          i18n()$t("All of these will be updated to point to the newly accepted name.")
        ),
        shiny::h6(i18n()$t("Synonyms that will be redirected:")),
        shiny::tags$ul(
          lapply(1:min(nrow(other_syns), 10), function(i) {
            syn <- other_syns[i, ]
            shiny::tags$li(
              sprintf("ID %d: %s %s",
                      syn$idtax_n,
                      if (is.na(syn$tax_gen)) "" else syn$tax_gen,
                      if (is.na(syn$tax_esp)) "" else syn$tax_esp
              )
            )
          })
        ),
        if (nrow(other_syns) > 10) {
          shiny::p(shiny::em(sprintf(i18n()$t("... and %d more"), nrow(other_syns) - 10)))
        }
      )
    })

    # Update selected ID when radio button changes
    shiny::observeEvent(input$selected_accepted_radio, {
      if (!is.null(input$selected_accepted_radio)) {
        rv$selected_accepted_id <- as.integer(input$selected_accepted_radio)

        # Check for existing synonyms
        pool_conn <- pool()
        actual_con <- pool::poolCheckout(pool_conn)
        on.exit(pool::poolReturn(actual_con), add = TRUE)

        current_taxon <- selected_taxon()
        existing_syns <- dplyr::tbl(actual_con, "table_taxa") %>%
          dplyr::filter(idtax_good_n == !!current_taxon$idtax_n) %>%
          dplyr::select(idtax_n, tax_gen, tax_esp, tax_nam01, tax_rank01) %>%
          dplyr::collect()

        rv$existing_synonyms <- if (nrow(existing_syns) > 0) existing_syns else NULL
      }
    })

    # Cancel set synonym
    shiny::observeEvent(input$btn_cancel_set, {
      rv$show_set_synonym_form <- FALSE
      rv$searched_accepted_taxa <- NULL
      rv$selected_accepted_id <- NULL
      rv$existing_synonyms <- NULL
      shiny::updateTextInput(session, "accepted_binomial", value = "")
      shiny::updateNumericInput(session, "accepted_id", value = NA)
    })

    # Show cancel synonym form
    shiny::observeEvent(input$btn_cancel_synonym, {
      rv$show_cancel_form <- TRUE
      rv$show_set_synonym_form <- FALSE
      rv$show_reverse_form <- FALSE
    })

    # Show reverse synonym form
    shiny::observeEvent(input$btn_reverse_synonym, {
      taxon <- selected_taxon()

      if (is.null(taxon) || is.na(taxon$idtax_good_n)) {
        shiny::showNotification(
          i18n()$t("Error: Selected taxon is not a synonym"),
          type = "error"
        )
        return()
      }

      # Get the current accepted taxon and all other synonyms
      pool_conn <- pool()
      actual_con <- pool::poolCheckout(pool_conn)
      on.exit(pool::poolReturn(actual_con), add = TRUE)

      current_accepted_id <- taxon$idtax_good_n

      # Fetch the accepted taxon info
      accepted_taxon <- dplyr::tbl(actual_con, "table_taxa") %>%
        dplyr::filter(idtax_n == !!current_accepted_id) %>%
        dplyr::collect()

      # Find all synonyms pointing to this accepted taxon (excluding the selected one)
      other_synonyms <- dplyr::tbl(actual_con, "table_taxa") %>%
        dplyr::filter(idtax_good_n == !!current_accepted_id, idtax_n != !!taxon$idtax_n) %>%
        dplyr::select(idtax_n, tax_gen, tax_esp, tax_fam) %>%
        dplyr::collect()

      rv$reverse_synonyms_info <- list(
        accepted_taxon = accepted_taxon,
        other_synonyms = other_synonyms,
        selected_taxon_id = taxon$idtax_n,
        current_accepted_id = current_accepted_id
      )

      rv$show_reverse_form <- TRUE
      rv$show_set_synonym_form <- FALSE
      rv$show_cancel_form <- FALSE
    })

    # Cancel cancel synonym
    shiny::observeEvent(input$btn_cancel_cancel, {
      rv$show_cancel_form <- FALSE
    })

    # Cancel reverse synonym
    shiny::observeEvent(input$btn_cancel_reverse, {
      rv$show_reverse_form <- FALSE
      rv$reverse_synonyms_info <- NULL
    })

    # Confirm set synonym
    shiny::observeEvent(input$btn_confirm_set_synonym, {
      taxon <- selected_taxon()

      # Validate that user has searched and selected an accepted taxon
      if (is.null(rv$selected_accepted_id)) {
        shiny::showNotification(
          i18n()$t("Please search and select an accepted taxon first"),
          type = "error"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          cli::cli_alert_info("Setting taxon ID {taxon$idtax_n} as synonym of taxon ID {rv$selected_accepted_id}...")

          # Get pool connection
          pool_conn <- pool()

          # Check if there are existing synonyms that need to be redirected
          # If yes, use direct SQL for EVERYTHING to avoid interactive prompts from update_dico_name()
          # If no, use update_dico_name() which handles backups properly
          if (!is.null(rv$existing_synonyms) && nrow(rv$existing_synonyms) > 0) {
            # Handle ALL updates with direct SQL (main + cascades) to avoid interactive prompts
            cli::cli_alert_info("Using direct SQL for main synonym and {nrow(rv$existing_synonyms)} cascade synonym(s)...")

            # Get actual connection from pool
            actual_con <- pool::poolCheckout(pool_conn)
            on.exit({
              pool::poolReturn(actual_con)
            }, add = TRUE)

            # Build list of all IDs to update (main taxon + existing synonyms)
            all_ids_to_update <- c(taxon$idtax_n, rv$existing_synonyms$idtax_n)

            # Update all at once with single SQL statement
            sql <- sprintf(
              "UPDATE table_taxa SET idtax_good_n = %d WHERE idtax_n IN (%s)",
              rv$selected_accepted_id,
              paste(all_ids_to_update, collapse = ", ")
            )

            n_updated <- DBI::dbExecute(actual_con, sql)

            cli::cli_alert_success("Updated {n_updated} taxon/taxa (1 main + {nrow(rv$existing_synonyms)} cascade)")

            shiny::showNotification(
              i18n()$t(sprintf("Synonym relationship set successfully! %d existing synonym(s) redirected to prevent chains.", nrow(rv$existing_synonyms))),
              type = "message",
              duration = 5
            )
          } else {
            # No existing synonyms - use update_dico_name() which handles backups
            cli::cli_alert_info("No existing synonyms - using update_dico_name() with backups...")

            update_dico_name(
              id_searched = taxon$idtax_n,
              synonym_of = list(id = rv$selected_accepted_id),
              ask_before_update = FALSE,
              add_backup = TRUE,
              show_results = FALSE,
              con = pool_conn
            )

            cli::cli_alert_success("Main synonym relationship set")

            shiny::showNotification(
              i18n()$t("Synonym relationship set successfully!"),
              type = "message",
              duration = 5
            )
          }

          # Reset form and clear reactive values
          rv$show_set_synonym_form <- FALSE
          rv$selected_accepted_id <- NULL
          rv$searched_accepted_taxa <- NULL
          rv$existing_synonyms <- NULL
          shiny::updateTextInput(session, "accepted_binomial", value = "")
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

          # Get pool connection
          pool_conn <- pool()

          # Call update_dico_name with cancel_synonymy, passing connection
          update_dico_name(
            id_searched = taxon$idtax_n,
            cancel_synonymy = TRUE,
            ask_before_update = FALSE,
            add_backup = TRUE,
            show_results = FALSE,
            con = pool_conn
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

    # Confirm reverse synonym
    shiny::observeEvent(input$btn_confirm_reverse_synonym, {
      if (is.null(rv$reverse_synonyms_info)) {
        shiny::showNotification(
          i18n()$t("Error: Reverse synonym information not available"),
          type = "error"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          info <- rv$reverse_synonyms_info
          selected_id <- info$selected_taxon_id
          old_accepted_id <- info$current_accepted_id
          other_synonyms <- info$other_synonyms

          cli::cli_alert_info("Reversing synonym relationship...")
          cli::cli_alert_info("  Selected taxon ID (will become accepted): {selected_id}")
          cli::cli_alert_info("  Old accepted ID (will become synonym): {old_accepted_id}")
          cli::cli_alert_info("  Other synonyms to redirect: {nrow(other_synonyms)}")

          # Get pool connection
          pool_conn <- pool()
          actual_con <- pool::poolCheckout(pool_conn)

          on.exit({
            pool::poolReturn(actual_con)
          }, add = TRUE)

          # Build list of all IDs to update
          # 1. The old accepted taxon needs to point to the selected taxon
          # 2. All other synonyms need to point to the selected taxon
          ids_to_update <- c(old_accepted_id)
          if (!is.null(other_synonyms) && nrow(other_synonyms) > 0) {
            ids_to_update <- c(ids_to_update, other_synonyms$idtax_n)
          }

          cli::cli_alert_info("Updating {length(ids_to_update)} taxa to point to new accepted taxon")

          # Update all taxa to point to the new accepted name (selected taxon)
          sql_update <- sprintf(
            "UPDATE table_taxa SET idtax_good_n = %d WHERE idtax_n IN (%s)",
            selected_id,
            paste(ids_to_update, collapse = ", ")
          )

          n_updated <- DBI::dbExecute(actual_con, sql_update)
          cli::cli_alert_success("Updated {n_updated} taxon/taxa to point to new accepted name")

          # Make the selected taxon accepted (clear its idtax_good_n)
          sql_clear <- sprintf(
            "UPDATE table_taxa SET idtax_good_n = NULL WHERE idtax_n = %d",
            selected_id
          )

          DBI::dbExecute(actual_con, sql_clear)
          cli::cli_alert_success("Cleared synonym link for taxon {selected_id} - now accepted")

          shiny::showNotification(
            sprintf(
              i18n()$t("Synonym relationship reversed! %d taxon/taxa updated."),
              n_updated + 1
            ),
            type = "message",
            duration = 5
          )

          # Reset form
          rv$show_reverse_form <- FALSE
          rv$reverse_synonyms_info <- NULL

        }, error = function(e) {
          cli::cli_alert_danger("Failed to reverse synonym: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error reversing synonym:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Reversing synonym relationship..."))
    })

    return(NULL)
  })
}
