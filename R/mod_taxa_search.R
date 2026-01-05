#' Taxa Search & Browser Module - UI
#'
#' UI component for searching and browsing taxonomic records
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_taxa_search_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # All UI elements rendered dynamically for i18n support
    shiny::uiOutput(ns("search_ui"))
  )
}

#' Taxa Search & Browser Module - Server
#'
#' Server logic for taxonomic search and browsing
#'
#' @param id Module namespace ID
#' @param pool Reactive returning taxa database connection pool
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return Reactive containing selected taxon data (full row from table_taxa)
#'
#' @keywords internal
#' @export
mod_taxa_search_server <- function(id, pool, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      search_results = NULL,
      selected_row = NULL
    )

    # Main UI with translations
    output$search_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::h4(i18n()$t("Search Taxonomy")),

        # Search mode selector
        shiny::wellPanel(
          style = "background-color: #f8f9fa; padding: 10px;",
          shiny::radioButtons(
            ns("search_mode"),
            i18n()$t("Search mode:"),
            choices = setNames(
              c("binomial", "structured"),
              c(
                i18n()$t("Binomial search (e.g., Genus species)"),
                i18n()$t("Structured search (separate fields)")
              )
            ),
            selected = "binomial",
            inline = TRUE
          )
        ),

        # Search filters panel
        shiny::wellPanel(
          # Binomial search input
          shiny::conditionalPanel(
            condition = "input.search_mode == 'binomial'",
            ns = ns,
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::textInput(
                  ns("binomial_input"),
                  i18n()$t("Binomial name (genus + species)"),
                  placeholder = i18n()$t("e.g., Gilbertiodendron dewevrei or gilbertiodendron dewevrei")
                ),
                shiny::div(
                  class = "text-muted small",
                  shiny::icon("info-circle"),
                  " ",
                  i18n()$t("Case-insensitive. Uncheck 'Exact match' below for fuzzy matching.")
                )
              )
            )
          ),

          # Structured search inputs
          shiny::conditionalPanel(
            condition = "input.search_mode == 'structured'",
            ns = ns,
            shiny::fluidRow(
              shiny::column(
                3,
                shiny::textInput(
                  ns("genus_filter"),
                  i18n()$t("Genus"),
                  placeholder = i18n()$t("e.g., Gilbertiodendron")
                )
              ),
              shiny::column(
                3,
                shiny::textInput(
                  ns("species_filter"),
                  i18n()$t("Species"),
                  placeholder = i18n()$t("e.g., dewevrei")
                )
              ),
              shiny::column(
                3,
                shiny::textInput(
                  ns("family_filter"),
                  i18n()$t("Family"),
                  placeholder = i18n()$t("e.g., Fabaceae")
                )
              ),
              shiny::column(
                3,
                shiny::textInput(
                  ns("order_filter"),
                  i18n()$t("Order"),
                  placeholder = i18n()$t("e.g., Fabales")
                )
              )
            )
          ),

          shiny::hr(),

          # Common search options
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::checkboxInput(
                ns("exact_match"),
                i18n()$t("Exact match"),
                value = TRUE
              )
            ),
            shiny::column(
              3,
              shiny::selectInput(
                ns("synonymy_filter"),
                i18n()$t("Synonymy filter"),
                choices = c(
                  "All taxa" = "all",
                  "Accepted names only" = "accepted",
                  "Synonyms only" = "synonyms"
                ),
                selected = "all"
              )
            ),
            shiny::column(
              3,
              shiny::actionButton(
                ns("search_btn"),
                i18n()$t("Search"),
                icon = shiny::icon("search"),
                class = "btn-primary btn-block",
                style = "margin-top: 25px;"
              )
            )
          ),

          shiny::hr(),

          # Advanced options (collapsible)
          shiny::tags$details(
            shiny::tags$summary(
              style = "cursor: pointer; color: #007bff; font-weight: 500;",
              shiny::icon("cog"),
              " ",
              i18n()$t("Advanced options")
            ),
            shiny::div(
              style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
              shiny::numericInput(
                ns("id_filter"),
                i18n()$t("Search by Taxon ID"),
                value = NULL,
                min = 1,
                width = "200px"
              )
            )
          )
        ),

        shiny::br(),

        # Results table
        shiny::h5(i18n()$t("Search Results")),
        shiny::uiOutput(ns("results_info")),
        DT::DTOutput(ns("results_table")),

        shiny::br(),

        # Selected taxon info
        shiny::uiOutput(ns("selected_info"))
      )
    })

    # Update synonymy filter choices with translations
    shiny::observe({
      shiny::req(i18n())

      shiny::updateSelectInput(
        session,
        "synonymy_filter",
        choices = setNames(
          c("all", "accepted", "synonyms"),
          c(
            i18n()$t("All taxa"),
            i18n()$t("Accepted names only"),
            i18n()$t("Synonyms only")
          )
        )
      )
    })

    # Statistics panel - REMOVED for performance
    # Multiple aggregation queries are too slow for low bandwidth connections
    # If needed in future, consider caching or making it opt-in only

    # Execute search
    shiny::observeEvent(input$search_btn, {
      shiny::req(pool())

      tryCatch({
        cli::cli_alert_info("Executing taxa search...")

        # Determine search mode and build query parameters
        if (input$search_mode == "binomial") {
          # Binomial search mode
          binomial <- if (!is.null(input$binomial_input) && nzchar(input$binomial_input)) {
            # Normalize: trim whitespace, squish multiple spaces, handle case
            stringr::str_squish(tolower(input$binomial_input))
          } else {
            NULL
          }

          genus <- NULL
          species <- binomial  # Pass entire binomial to species parameter
          family <- NULL
          order <- NULL
          ids <- if (!is.null(input$id_filter) && !is.na(input$id_filter)) input$id_filter else NULL

        } else {
          # Structured search mode
          genus <- if (nzchar(input$genus_filter)) input$genus_filter else NULL
          species <- if (nzchar(input$species_filter)) input$species_filter else NULL
          family <- if (nzchar(input$family_filter)) input$family_filter else NULL
          order <- if (nzchar(input$order_filter)) input$order_filter else NULL
          ids <- if (!is.null(input$id_filter) && !is.na(input$id_filter)) input$id_filter else NULL
        }

        # Use query_taxa if we have search criteria
        if (!is.null(genus) || !is.null(species) || !is.null(family) || !is.null(order) || !is.null(ids)) {

          results <- query_taxa(
            genus = genus,
            species = species,
            family = family,
            order = order,
            ids = ids,
            exact_match = input$exact_match,
            check_synonymy = FALSE,
            extract_traits = FALSE,
            verbose = FALSE
          )

        } else {
          # Browse mode - get all taxa (limit to 1000 for performance)
          actual_con <- if (inherits(pool(), "Pool")) {
            pool::poolCheckout(pool())
          } else {
            pool()
          }

          on.exit({
            if (inherits(pool(), "Pool") && !is.null(actual_con)) {
              pool::poolReturn(actual_con)
            }
          }, add = TRUE)

          results <- dplyr::tbl(actual_con, "table_taxa") %>%
            dplyr::arrange(tax_fam, tax_gen, tax_esp) %>%
            dplyr::collect() %>%
            dplyr::slice(1:min(1000, dplyr::n()))
        }

        # Apply synonymy filter
        if (!is.null(results) && nrow(results) > 0) {
          if (input$synonymy_filter == "accepted") {
            results <- results %>%
              dplyr::filter(is.na(idtax_good_n))
          } else if (input$synonymy_filter == "synonyms") {
            results <- results %>%
              dplyr::filter(!is.na(idtax_good_n))
          }
        }

        rv$search_results <- results

        if (is.null(results) || nrow(results) == 0) {
          shiny::showNotification(
            i18n()$t("No taxa found matching criteria"),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            sprintf(i18n()$t("Found %d taxa"), nrow(results)),
            type = "message",
            duration = 3
          )
        }

      }, error = function(e) {
        cli::cli_alert_danger("Search failed: {e$message}")
        shiny::showNotification(
          paste(i18n()$t("Search error:"), e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Results info
    output$results_info <- shiny::renderUI({
      if (is.null(rv$search_results)) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("Use the search filters above and click 'Search' to browse taxa")
          )
        )
      }

      n_results <- nrow(rv$search_results)

      shiny::div(
        class = "alert alert-success",
        shiny::icon("check-circle"),
        sprintf(" %s: %d", i18n()$t("Results"), n_results)
      )
    })

    # Render results table
    output$results_table <- DT::renderDT({
      shiny::req(rv$search_results)

      # Select key columns to display
      display_cols <- c(
        "idtax_n", "tax_famclass", "tax_order", "tax_fam", "tax_gen", "tax_esp",
        "tax_rank01", "tax_nam01", "author1", "tax_rankinf", "idtax_good_n"
      )

      # Keep only existing columns
      display_cols <- intersect(display_cols, names(rv$search_results))

      results_display <- rv$search_results[, display_cols, drop = FALSE]

      # Add synonym indicator column
      results_display <- results_display %>%
        dplyr::mutate(
          synonym_status = ifelse(
            !is.na(idtax_good_n),
            "Synonym",
            "Accepted"
          )
        ) %>%
        dplyr::select(idtax_n, synonym_status, dplyr::everything())

      DT::datatable(
        results_display,
        selection = list(mode = "single"),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        rownames = FALSE,
        class = "display nowrap compact",
        filter = "top"
      )
    })

    # Handle table row selection
    shiny::observeEvent(input$results_table_rows_selected, {
      shiny::req(rv$search_results)

      selected_row <- input$results_table_rows_selected

      if (length(selected_row) > 0) {
        rv$selected_row <- rv$search_results[selected_row, ]
        cli::cli_alert_success("Taxon selected: ID {rv$selected_row$idtax_n}")
      }
    })

    # Selected taxon info
    output$selected_info <- shiny::renderUI({
      shiny::req(rv$selected_row)

      taxon <- rv$selected_row

      # Get accepted name if synonym
      accepted_name_info <- NULL
      if (!is.na(taxon$idtax_good_n)) {
        tryCatch({
          accepted_taxon <- query_taxa(
            ids = taxon$idtax_good_n,
            check_synonymy = FALSE,
            extract_traits = FALSE,
            verbose = FALSE
          )

          if (!is.null(accepted_taxon) && nrow(accepted_taxon) > 0) {
            accepted_name_info <- sprintf(
              "%s %s",
              accepted_taxon$tax_gen[1],
              if (!is.na(accepted_taxon$tax_esp[1])) accepted_taxon$tax_esp[1] else ""
            )
          }
        }, error = function(e) {
          cli::cli_alert_warning("Could not fetch accepted name")
        })
      }

      shiny::wellPanel(
        style = "background-color: #d4edda; border-color: #c3e6cb;",
        shiny::h5(
          shiny::icon("check-square"),
          " ",
          i18n()$t("Selected Taxon")
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::strong(i18n()$t("Taxon ID:")), " ", taxon$idtax_n, shiny::br(),
            shiny::strong(i18n()$t("Family:")), " ", taxon$tax_fam %||% "N/A", shiny::br(),
            shiny::strong(i18n()$t("Genus:")), " ", taxon$tax_gen %||% "N/A", shiny::br(),
            shiny::strong(i18n()$t("Species:")), " ", taxon$tax_esp %||% "N/A", shiny::br(),
            if (!is.na(taxon$tax_rank01) && !is.na(taxon$tax_nam01)) {
              shiny::tagList(
                shiny::strong(i18n()$t("Infraspecific:")),
                " ",
                taxon$tax_rank01,
                " ",
                taxon$tax_nam01,
                shiny::br()
              )
            }
          ),
          shiny::column(
            6,
            shiny::strong(i18n()$t("Author:")), " ", taxon$author1 %||% "N/A", shiny::br(),
            shiny::strong(i18n()$t("Rank:")), " ", taxon$tax_rankinf %||% "N/A", shiny::br(),
            shiny::strong(i18n()$t("Status:")),
            " ",
            if (!is.na(taxon$idtax_good_n)) {
              shiny::span(
                class = "synonym-indicator",
                i18n()$t("Synonym")
              )
            } else {
              shiny::span(
                style = "color: #28a745; font-weight: bold;",
                i18n()$t("Accepted name")
              )
            },
            shiny::br(),
            if (!is.null(accepted_name_info)) {
              shiny::tagList(
                shiny::strong(i18n()$t("Accepted name:")),
                " ",
                accepted_name_info,
                shiny::br()
              )
            }
          )
        ),
        shiny::hr(),
        shiny::p(
          class = "text-muted",
          style = "font-size: 0.9em; margin-bottom: 0;",
          shiny::icon("info-circle"),
          " ",
          i18n()$t("Use 'Update Taxon' or 'Synonymy Management' tabs to modify this record")
        )
      )
    })

    # Return selected taxon data
    return(shiny::reactive(rv$selected_row))
  })
}
