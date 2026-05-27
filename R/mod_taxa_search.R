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
#' @param is_public Reactive returning TRUE if user connected with public
#'   credentials (used for R code generation). Default: always FALSE.
#'
#' @return Reactive containing selected taxon data (full row from table_taxa)
#'
#' @keywords internal
#' @export
mod_taxa_search_server <- function(id, pool, i18n,
                                    is_public = shiny::reactive(FALSE),
                                    reset = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      search_results     = NULL,
      selected_row       = NULL,
      last_search_params = NULL
    )

    # Clear selection whenever the reset trigger changes value
    shiny::observeEvent(reset(), {
      rv$selected_row <- NULL
      DT::dataTableProxy("results_table", session = session) %>%
        DT::selectRows(NULL)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

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
                i18n()$t("Name search (any taxonomic level)"),
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
                  i18n()$t("Taxon name (family, genus, or species)"),
                  placeholder = i18n()$t("e.g., Fabaceae, Gilbertiodendron, or Gilbertiodendron dewevrei")
                ),
                shiny::div(
                  class = "text-muted small",
                  shiny::icon("info-circle"),
                  " ",
                  i18n()$t("Case-insensitive. Uses fuzzy matching by default. Check 'Exact match' below for strict matching.")
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
                value = FALSE
              )
            ),
            shiny::column(
              3,
              shiny::checkboxInput(
                ns("include_synonyms"),
                i18n()$t("Include synonyms of queried taxa"),
                value = FALSE
              )
            ),
            shiny::column(
              3,
              shiny::checkboxInput(
                ns("include_children"),
                i18n()$t("Include child taxa"),
                value = FALSE
              ),
              shiny::helpText(i18n()$t("Show all descendant taxa (e.g., species within genus, infraspecific taxa)"))
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
        shiny::uiOutput(ns("selected_info")),

        shiny::br(),

        # Traits explanation
        shiny::uiOutput(ns("traits_explanation")),

        # Selected taxon traits (text summary)
        shiny::uiOutput(ns("selected_traits")),

        # Trait table extraction (wide/long + citations)
        mod_taxa_traits_table_ui(ns("traits_table")),

        # Equivalent R code section
        mod_taxa_r_code_ui(ns("r_code"))
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

      # Capture search parameters for R code generation
      rv$last_search_params <- list(
        search_mode      = input$search_mode %||% "binomial",
        binomial         = input$binomial_input %||% "",
        genus            = input$genus_filter %||% "",
        species          = input$species_filter %||% "",
        family           = input$family_filter %||% "",
        order            = input$order_filter %||% "",
        id_filter        = if (!is.null(input$id_filter) && !is.na(input$id_filter)) input$id_filter else NULL,
        exact_match      = isTRUE(input$exact_match),
        include_synonyms = isTRUE(input$include_synonyms),
        include_children = isTRUE(input$include_children),
        synonymy_filter  = input$synonymy_filter %||% "all"
      )

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
            check_synonymy = TRUE,
            extract_traits = TRUE,
            include_children = isTRUE(input$include_children),
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

          # Add traits to browse results if any taxa were found
          if (!is.null(results) && nrow(results) > 0) {
            tryCatch({
              traits_result <- query_taxa_traits(
                idtax = results$idtax_n,
                format = "wide",
                add_taxa_info = FALSE,
                include_synonyms = TRUE,
                categorical_mode = "mode",
                con_taxa = actual_con
              )

              # Join numeric traits if available
              has_numeric <- !is.null(traits_result$traits_numeric) &&
                             !inherits(traits_result$traits_numeric, "logical")
              if (has_numeric && nrow(traits_result$traits_numeric) > 0) {
                results <- results %>%
                  dplyr::left_join(
                    traits_result$traits_numeric,
                    by = c("idtax_n" = "idtax")
                  )
              }

              # Join categorical traits if available
              has_categorical <- !is.null(traits_result$traits_categorical) &&
                                 !inherits(traits_result$traits_categorical, "logical")
              if (has_categorical && nrow(traits_result$traits_categorical) > 0) {
                results <- results %>%
                  dplyr::left_join(
                    traits_result$traits_categorical,
                    by = c("idtax_n" = "idtax")
                  )
              }
            }, error = function(e) {
              cli::cli_alert_warning("Could not fetch traits for browse mode: {e$message}")
            })
          }
        }

        # Include synonyms if requested
        if (!is.null(results) && nrow(results) > 0 && input$include_synonyms) {
          tryCatch({
            cli::cli_alert_info("Including synonyms of queried taxa...")

            # Get connection
            actual_con <- if (inherits(pool(), "Pool")) {
              pool::poolCheckout(pool())
            } else {
              pool()
            }

            on.exit({
              if (inherits(pool(), "Pool") && !is.null(actual_con)) {
                tryCatch({
                  pool::poolReturn(actual_con)
                }, error = function(e) {
                  # Connection might already be returned, ignore
                })
              }
            }, add = TRUE)

            # Get IDs of accepted taxa from results (those without idtax_good_n)
            accepted_ids <- results %>%
              dplyr::filter(is.na(idtax_good_n)) %>%
              dplyr::pull(idtax_n)

            if (length(accepted_ids) > 0) {
              # Find all synonyms pointing to these accepted taxa
              synonyms <- dplyr::tbl(actual_con, "table_taxa") %>%
                dplyr::filter(idtax_good_n %in% !!accepted_ids) %>%
                dplyr::collect()

              if (nrow(synonyms) > 0) {
                cli::cli_alert_success("Found {nrow(synonyms)} synonym{?s}")

                # Add traits to synonyms if results have trait columns
                trait_cols <- names(results)[grepl("^taxa_", names(results))]
                if (length(trait_cols) > 0) {
                  tryCatch({
                    traits_result <- query_taxa_traits(
                      idtax = synonyms$idtax_n,
                      format = "wide",
                      add_taxa_info = FALSE,
                      include_synonyms = TRUE,
                      categorical_mode = "mode",
                      con_taxa = NULL
                    )

                    # Join numeric traits
                    has_numeric <- !is.null(traits_result$traits_numeric) &&
                                   !inherits(traits_result$traits_numeric, "logical")
                    if (has_numeric && nrow(traits_result$traits_numeric) > 0) {
                      synonyms <- synonyms %>%
                        dplyr::left_join(
                          traits_result$traits_numeric,
                          by = c("idtax_n" = "idtax")
                        )
                    }

                    # Join categorical traits
                    has_categorical <- !is.null(traits_result$traits_categorical) &&
                                       !inherits(traits_result$traits_categorical, "logical")
                    if (has_categorical && nrow(traits_result$traits_categorical) > 0) {
                      synonyms <- synonyms %>%
                        dplyr::left_join(
                          traits_result$traits_categorical,
                          by = c("idtax_n" = "idtax")
                        )
                    }
                  }, error = function(e) {
                    cli::cli_alert_warning("Could not fetch traits for synonyms: {e$message}")
                  })
                }

                # Combine results with synonyms
                results <- dplyr::bind_rows(results, synonyms)
              }
            }
          }, error = function(e) {
            cli::cli_alert_warning("Could not include synonyms: {e$message}")
          })
        }

        # Note: include_children is now handled by query_taxa() directly.
        # The Shiny checkbox is passed via include_children parameter above.

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
        sprintf(" %s: %d  —  ", i18n()$t("Results"), n_results),
        i18n()$t("Click a row to select it, or hold Ctrl/Cmd to select multiple taxa. Selected taxa can be explored and updated in the other tabs, or compared using 'Extract as Table' below.")
      )
    })

    # Render results table
    output$results_table <- DT::renderDT({
      shiny::req(rv$search_results)

      # Define core taxonomic columns (in display order)
      core_cols <- c(
        "idtax_n", "tax_famclass", "tax_order", "tax_fam", "tax_gen", "tax_esp",
        "tax_rank01", "tax_nam01", "author1", "tax_rankinf", "idtax_good_n"
      )

      # Keep only existing core columns
      core_cols_present <- intersect(core_cols, names(rv$search_results))

      # Identify trait columns (all columns not in core set)
      all_cols <- names(rv$search_results)
      trait_cols <- setdiff(all_cols, core_cols)

      # Combine: core columns first, then trait columns
      display_cols <- c(core_cols_present, trait_cols)

      results_display <- rv$search_results[, display_cols, drop = FALSE]

      # Add synonym indicator column at the beginning
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
        selection = list(mode = "multiple"),
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
        cli::cli_alert_success(
          "{nrow(rv$selected_row)} taxon/taxa selected: IDs {paste(rv$selected_row$idtax_n, collapse = ', ')}"
        )
      }
    })

    # WCVP info for the selected taxon (fetched reactively)
    wcvp_info_reactive <- shiny::reactive({
      shiny::req(rv$selected_row)
      taxon <- rv$selected_row[1, ]
      tryCatch({
        get_wcvp_names(taxon$idtax_n, con_taxa = pool())
      }, error = function(e) {
        cli::cli_alert_warning("Could not fetch WCVP info: {e$message}")
        NULL
      })
    })

    # Selected taxon info
    output$selected_info <- shiny::renderUI({
      shiny::req(rv$selected_row)

      n_selected <- nrow(rv$selected_row)

      # ---- Multiple taxa selected: compact list ----
      if (n_selected > 1) {
        taxa_list_items <- lapply(seq_len(n_selected), function(i) {
          row <- rv$selected_row[i, ]
          label <- paste0(
            row$tax_gen %||% "", " ", row$tax_esp %||% "",
            " (ID: ", row$idtax_n, ")",
            if (!is.na(row$idtax_good_n)) paste0(" [", i18n()$t("Synonym"), "]") else ""
          )
          shiny::tags$li(label)
        })

        return(shiny::wellPanel(
          style = "background-color: #d4edda; border-color: #c3e6cb;",
          shiny::h5(
            shiny::icon("check-square"),
            " ",
            paste0(n_selected, " ", i18n()$t("taxa selected"))
          ),
          shiny::tags$ul(taxa_list_items),
          shiny::hr(),
          shiny::p(
            class = "text-muted",
            style = "font-size: 0.9em; margin-bottom: 0;",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("Update Taxon and Synonymy Management tabs act on the first selected taxon.")
          )
        ))
      }

      # ---- Single taxon selected: detailed panel ----
      taxon <- rv$selected_row[1, ]

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
              shiny::span(class = "synonym-indicator", i18n()$t("Synonym"))
            } else {
              shiny::span(style = "color: #28a745; font-weight: bold;", i18n()$t("Accepted name"))
            },
            shiny::br(),
            if (!is.null(accepted_name_info)) {
              shiny::tagList(
                shiny::strong(i18n()$t("Accepted name:")),
                " ",
                accepted_name_info,
                shiny::br()
              )
            },
            shiny::strong(i18n()$t("Morphotaxon:")), " ",
            if (isTRUE(taxon$morpho_species)) i18n()$t("Yes") else i18n()$t("No"),
            shiny::br()
          )
        ),
        shiny::hr(),
        # WCVP info section
        local({
          wcvp <- wcvp_info_reactive()
          if (!is.null(wcvp) && nrow(wcvp) > 0 && !is.na(wcvp$wcvp_plant_name_id[1])) {
            w <- wcvp[1, ]
            shiny::tagList(
              shiny::strong(i18n()$t("WCVP Link")), shiny::br(),
              shiny::strong(i18n()$t("WCVP ID:")), " ", w$wcvp_plant_name_id, shiny::br(),
              shiny::strong(i18n()$t("WCVP Status:")), " ", w$wcvp_taxon_status %||% "N/A", shiny::br(),
              shiny::strong(i18n()$t("WCVP Name:")), " ", w$wcvp_taxon_name %||% "N/A", shiny::br(),
              shiny::hr()
            )
          } else {
            shiny::tagList(
              shiny::span(
                class = "text-muted",
                style = "font-size: 0.9em;",
                shiny::icon("unlink"),
                " ",
                i18n()$t("Not linked to WCVP")
              ),
              shiny::hr()
            )
          }
        }),
        shiny::p(
          class = "text-muted",
          style = "font-size: 0.9em; margin-bottom: 0;",
          shiny::icon("info-circle"),
          " ",
          i18n()$t("Use 'Update Taxon' or 'Synonymy Management' tabs to modify this record")
        )
      )
    })

    # Traits explanation panel
    output$traits_explanation <- shiny::renderUI({
      # Only show explanation if a taxon is selected
      shiny::req(rv$selected_row)

      # Check if there are any taxa_ columns (use first row as representative)
      all_cols <- names(rv$selected_row[1, ])
      trait_cols <- all_cols[grepl("^taxa_", all_cols) & !grepl("id", all_cols, ignore.case = TRUE)]

      # Only show explanation if traits exist
      if (length(trait_cols) == 0) {
        return(NULL)
      }

      shiny::div(
        class = "alert alert-info",
        style = "background-color: #d1ecf1; border-color: #bee5eb; color: #0c5460;",
        shiny::h5(
          shiny::icon("info-circle"),
          " ",
          i18n()$t("About Taxa-Level Traits"),
          style = "color: #0c5460; margin-top: 0;"
        ),
        shiny::tags$ul(
          style = "color: #0c5460; margin-bottom: 10px;",
          shiny::tags$li(
            shiny::tags$strong(i18n()$t("Numeric traits:")),
            " ",
            i18n()$t("Each numeric trait appears as three columns: _mean (average value), _sd (standard deviation), and _n (number of measurements). For example, 'wood_density' becomes 'wood_density_mean', 'wood_density_sd', and 'wood_density_n'.")
          ),
          shiny::tags$li(
            shiny::tags$strong(i18n()$t("Categorical traits:")),
            " ",
            i18n()$t("For categorical traits (e.g., growth form, dispersal mode), you can choose to display either the most frequent value (mode) or all unique values concatenated.")
          )
        ),
        shiny::p(
          shiny::icon("link"),
          " ",
          i18n()$t("Synonym resolution: Trait data from taxonomic synonyms are automatically consolidated under the accepted taxon name."),
          style = "color: #0c5460; font-style: italic; margin-bottom: 0;"
        )
      )
    })

    # Selected taxon traits
    output$selected_traits <- shiny::renderUI({
      shiny::req(rv$selected_row)

      # Multiple taxa: redirect to Extract as Table
      if (nrow(rv$selected_row) > 1) {
        return(shiny::wellPanel(
          style = "background-color: #d1ecf1; border-color: #bee5eb;",
          shiny::h5(shiny::icon("leaf"), " ", i18n()$t("Taxa-Level Traits")),
          shiny::p(
            shiny::icon("table"),
            " ",
            sprintf(
              i18n()$t("%d taxa selected — use 'Extract as Table' below to compare their traits side by side."),
              nrow(rv$selected_row)
            )
          )
        ))
      }

      tryCatch({
        taxon <- rv$selected_row[1, ]

        # Get all column names
        all_cols <- names(taxon)

        # Identify trait columns: only columns starting with "taxa_"
        # Exclude any columns containing "id" in their name
        trait_cols <- all_cols[grepl("^taxa_", all_cols) & !grepl("id", all_cols, ignore.case = TRUE)]

        # If no trait columns, don't display anything
        if (length(trait_cols) == 0) {
          return(NULL)
        }

        # Filter out NA-only traits
        trait_values <- taxon[, trait_cols, drop = FALSE]
        has_value <- sapply(trait_values, function(x) !is.na(x))
        trait_cols_with_values <- trait_cols[has_value]

      # If no traits have values, don't display
      if (length(trait_cols_with_values) == 0) {
        return(
          shiny::wellPanel(
            style = "background-color: #d1ecf1; border-color: #bee5eb;",
            shiny::h5(
              shiny::icon("leaf"),
              " ",
              i18n()$t("Taxa-Level Traits")
            ),
            shiny::p(
              class = "text-muted",
              style = "margin-bottom: 0;",
              shiny::icon("info-circle"),
              " ",
              i18n()$t("No trait data available for this taxon")
            )
          )
        )
      }

      # Build trait display
      # Organize traits into rows of 3 columns each
      trait_items <- lapply(trait_cols_with_values, function(trait_name) {
        trait_value <- trait_values[[trait_name]]

        # Format value based on type
        formatted_value <- if (is.numeric(trait_value)) {
          if (is.na(trait_value)) {
            "N/A"
          } else {
            format(round(trait_value, 3), nsmall = 1)
          }
        } else {
          as.character(trait_value)
        }

        shiny::column(
          4,
          shiny::div(
            style = "margin-bottom: 8px;",
            shiny::strong(trait_name, ":"), " ", formatted_value
          )
        )
      })

      # Split into rows of 3
      n_traits <- length(trait_items)
      n_rows <- ceiling(n_traits / 3)

      trait_rows <- lapply(1:n_rows, function(i) {
        start_idx <- (i - 1) * 3 + 1
        end_idx <- min(i * 3, n_traits)
        shiny::fluidRow(
          trait_items[start_idx:end_idx]
        )
      })

      return(shiny::wellPanel(
        style = "background-color: #d1ecf1; border-color: #bee5eb;",
        shiny::h5(
          shiny::icon("leaf"),
          " ",
          i18n()$t("Taxa-Level Traits")
        ),
        trait_rows,
        shiny::hr(),
        shiny::p(
          class = "text-muted",
          style = "font-size: 0.9em; margin-bottom: 0;",
          shiny::icon("info-circle"),
          " ",
          sprintf(i18n()$t("Showing %d trait(s) for this taxon"), length(trait_cols_with_values))
        )
      ))
      }, error = function(e) {
        # If there's an error rendering traits, just don't show the panel
        cli::cli_alert_warning("Could not render traits panel: {e$message}")
        return(NULL)
      })
    })

    # Trait table module (wide/long/citations for selected taxon)
    traits_table_out <- mod_taxa_traits_table_server(
      "traits_table",
      selected_taxon = shiny::reactive(rv$selected_row),
      i18n = i18n
    )

    # R code preview module
    mod_taxa_r_code_server(
      "r_code",
      search_params  = shiny::reactive(rv$last_search_params),
      selected_taxon = shiny::reactive(rv$selected_row),
      traits_fetched = traits_table_out$traits_fetched,
      is_public      = is_public,
      i18n           = i18n
    )

    # Return selected taxon data
    return(shiny::reactive(rv$selected_row))
  })
}
