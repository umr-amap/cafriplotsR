# Traits Enrichment Module
#
# Enriches matched taxonomic names with trait data from the taxa database

#' Traits Enrichment Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_traits_enrichment_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(shiny::textOutput(ns("title"))),

    shiny::uiOutput(ns("enrichment_status")),

    shiny::hr(),

    # Explanatory information about trait aggregation
    shiny::uiOutput(ns("traits_info_box")),

    shiny::hr(),

    # Enrichment options
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::uiOutput(ns("format_selection"))
      ),
      shiny::column(
        width = 4,
        shiny::uiOutput(ns("categorical_mode"))
      ),
      shiny::column(
        width = 4,
        shiny::uiOutput(ns("include_original"))
      )
    ),

    shiny::hr(),

    shiny::uiOutput(ns("enrich_button")),

    shiny::hr(),

    # Tabset for different format views
    shiny::uiOutput(ns("results_tabs"))
  )
}


#' Traits Enrichment Module - Server
#'
#' @param id Character, module ID
#' @param results Reactive list from review module (contains final matched data)
#' @param column_name Reactive returning the selected column name containing taxa
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL
#'
#' @keywords internal
mod_traits_enrichment_server <- function(id, results, column_name, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    enriched_data <- shiny::reactiveVal(NULL)
    enriched_data_long <- shiny::reactiveVal(NULL)
    citation_summary_data <- shiny::reactiveVal(NULL)
    enrichment_in_progress <- shiny::reactiveVal(FALSE)

    # Module title
    output$title <- shiny::renderText({
      i18n()$t("Enrich with Traits")
    })

    # Explanatory info box about trait aggregation
    output$traits_info_box <- shiny::renderUI({
      shiny::div(
        class = "panel panel-info",
        style = "background-color: #e8f4f8; border: 1px solid #bee5eb; border-radius: 5px; padding: 15px; margin-bottom: 15px;",
        shiny::h5(
          shiny::icon("info-circle"),
          i18n()$t("How traits are attributed to taxa"),
          style = "color: #0c5460; margin-top: 0;"
        ),
        shiny::p(
          i18n()$t("Trait values are aggregated across all available measurements for each taxon in the database:"),
          style = "color: #0c5460; margin-bottom: 10px;"
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

    # Enrichment status
    output$enrichment_status <- shiny::renderUI({
      req(results())

      data <- results()$data

      # Count matched taxa (exclude NAs)
      n_matched <- data %>%
        dplyr::filter(!is.na(idtax_n)) %>%
        dplyr::distinct(idtax_n) %>%
        nrow()

      n_total <- data %>%
        dplyr::distinct(dplyr::across(dplyr::any_of(names(data)[1]))) %>%
        nrow()

      if (n_matched == 0) {
        shiny::div(
          style = "padding: 15px; background-color: #fff3cd; border-radius: 5px;",
          shiny::p(
            shiny::icon("exclamation-triangle"),
            i18n()$t("No matched taxa found. Complete the matching process first."),
            style = "color: #856404; margin: 0;"
          )
        )
      } else {
        shiny::div(
          style = "padding: 15px; background-color: #d4edda; border-radius: 5px;",
          shiny::p(
            shiny::icon("check-circle"),
            shiny::strong(paste0(n_matched, " ", i18n()$t("unique taxa matched"))),
            " - ", i18n()$t("Ready to enrich with trait data"),
            style = "color: #155724; margin: 0;"
          )
        )
      }
    })

    # Format selection (removed - always use wide format)
    # output$format_selection <- shiny::renderUI({
    #   # Always use wide format
    # })

    # Categorical mode selection
    output$categorical_mode <- shiny::renderUI({
      ns <- session$ns

      # Build choices with translations
      cat_choices <- c("mode", "concat")
      names(cat_choices) <- c(
        i18n()$t("Most frequent value (mode)"),
        i18n()$t("All values (concatenated)")
      )

      shiny::tagList(
        shiny::h5(i18n()$t("Categorical Traits")),
        shiny::radioButtons(
          inputId = ns("categorical_format"),
          label = NULL,
          choices = cat_choices,
          selected = "mode"
        ),
        shiny::tags$small(
          class = "text-muted",
          i18n()$t("How to aggregate categorical traits when multiple values exist")
        )
      )
    })

    # Include original name option
    output$include_original <- shiny::renderUI({
      ns <- session$ns

      # Build choices with translations
      col_choices <- c("original", "corrected", "ids", "metadata")
      names(col_choices) <- c(
        i18n()$t("Original input names"),
        i18n()$t("Corrected names"),
        i18n()$t("Taxonomic IDs"),
        i18n()$t("Match metadata")
      )

      shiny::tagList(
        shiny::h5(i18n()$t("Include Columns")),
        shiny::checkboxGroupInput(
          inputId = ns("include_cols"),
          label = NULL,
          choices = col_choices,
          selected = c("original", "corrected", "ids")
        )
      )
    })

    # Enrich button
    output$enrich_button <- shiny::renderUI({
      req(results())

      data <- results()$data
      n_matched <- data %>%
        dplyr::filter(!is.na(idtax_n)) %>%
        dplyr::distinct(idtax_n) %>%
        nrow()

      ns <- session$ns

      if (n_matched > 0) {
        shiny::actionButton(
          inputId = ns("btn_enrich"),
          label = shiny::tagList(shiny::icon("database"), i18n()$t("Fetch Traits from Database")),
          class = "btn-primary btn-lg"
        )
      }
    })

    # Handle enrichment
    shiny::observeEvent(input$btn_enrich, {
      req(results())

      enrichment_in_progress(TRUE)
      shinybusy::show_spinner()

      tryCatch({
        data <- results()$data

        # Get the selected column name to filter out NA input names
        selected_col_name <- column_name()

        # Get unique matched taxa (exclude NA taxa AND NA input names)
        matched_taxa <- data %>%
          dplyr::filter(
            !is.na(idtax_n),  # Exclude unmatched taxa
            !is.na(.data[[selected_col_name]]),  # Exclude NA input names
            .data[[selected_col_name]] != ""  # Exclude empty strings
          ) %>%
          dplyr::distinct(idtax_n, idtax_good_n, matched_name, corrected_name)

        if (nrow(matched_taxa) == 0) {
          shiny::showNotification(
            i18n()$t("No matched taxa to enrich"),
            type = "warning"
          )
          enrichment_in_progress(FALSE)
          shinybusy::hide_spinner()
          return(NULL)
        }

        # Fetch traits in WIDE format for aggregated view
        shiny::showNotification(
          paste0(i18n()$t("Fetching traits for"), " ", nrow(matched_taxa), " ", i18n()$t("taxa...")),
          duration = NULL,
          id = "fetch_traits",
          type = "message"
        )

        traits_result_wide <- query_taxa_traits(
          idtax = matched_taxa$idtax_n,
          format = "wide",
          add_taxa_info = FALSE,  # We already have taxa info
          include_synonyms = TRUE,
          categorical_mode = input$categorical_format %||% "mode",
          include_remarks = FALSE,
          include_measurement_features = FALSE,
          include_citation = TRUE,
          con_taxa = NULL
        )

        # Fetch traits in LONG format for detailed measurements
        traits_result_long <- query_taxa_traits(
          idtax = matched_taxa$idtax_n,
          format = "long",
          add_taxa_info = FALSE,
          include_synonyms = TRUE,
          include_remarks = TRUE,
          include_measurement_features = TRUE,
          include_citation = TRUE,
          con_taxa = NULL
        )

        shiny::removeNotification("fetch_traits")

        # Check if we got results (wide format)
        has_numeric <- !is.null(traits_result_wide$traits_numeric) &&
                       !inherits(traits_result_wide$traits_numeric, "logical")
        has_categorical <- !is.null(traits_result_wide$traits_categorical) &&
                          !inherits(traits_result_wide$traits_categorical, "logical")

        if (is.null(traits_result_wide) || (!has_numeric && !has_categorical)) {
          shiny::showNotification(
            i18n()$t("No trait data found for these taxa"),
            type = "warning",
            duration = 5
          )
          enrichment_in_progress(FALSE)
          shinybusy::hide_spinner()
          return(NULL)
        }

        # Build enriched dataset
        # One row per unique taxon, with concatenated input names
        enriched <- data

        # Determine which columns to include from original name
        include_opts <- input$include_cols %||% c("original", "corrected", "ids")

        # Get the selected column name (the one containing taxonomic names)
        selected_col_name <- column_name()

        # Filter out unmatched names (NA taxa) AND rows with NA input names
        enriched_filtered <- enriched %>%
          dplyr::filter(
            !is.na(idtax_n),  # Exclude unmatched taxa
            !is.na(.data[[selected_col_name]]),  # Exclude NA input names
            .data[[selected_col_name]] != ""  # Exclude empty strings
          )

        if (nrow(enriched_filtered) == 0) {
          shiny::showNotification(
            i18n()$t("No matched taxa to enrich. All input names are unmatched or invalid."),
            type = "warning",
            duration = 5
          )
          enrichment_in_progress(FALSE)
          shinybusy::hide_spinner()
          return(NULL)
        }

        # Create base table: one row per unique taxon with concatenated input names
        enriched_result <- enriched_filtered %>%
          dplyr::group_by(
            idtax_n,
            idtax_good_n,
            matched_name,
            corrected_name,
            is_synonym,
            accepted_name
          ) %>%
          dplyr::summarise(
            input_names = paste(unique(.data[[selected_col_name]]), collapse = " | "),
            match_methods = paste(unique(match_method), collapse = " | "),
            match_scores = paste(unique(round(match_score, 3)), collapse = " | "),
            .groups = "drop"
          ) %>%
          dplyr::ungroup()

        # Join numeric traits if available (WIDE format)
        # Note: query_taxa_traits returns column named "idtax", not "idtax_n"
        if (has_numeric && nrow(traits_result_wide$traits_numeric) > 0) {
          # Remove id_trait_measures columns before joining
          numeric_traits <- traits_result_wide$traits_numeric %>%
            dplyr::select(-dplyr::starts_with("id_trait_measures"))

          enriched_result <- enriched_result %>%
            dplyr::left_join(
              numeric_traits,
              by = c("idtax_n" = "idtax")
            )
        }

        # Join categorical traits if available (WIDE format)
        if (has_categorical && nrow(traits_result_wide$traits_categorical) > 0) {
          # Remove id_trait_measures columns before joining
          categorical_traits <- traits_result_wide$traits_categorical %>%
            dplyr::select(-dplyr::starts_with("id_trait_measures"))

          enriched_result <- enriched_result %>%
            dplyr::left_join(
              categorical_traits,
              by = c("idtax_n" = "idtax")
            )
        }

        # Filter columns based on user selection
        cols_to_keep <- c()

        if ("original" %in% include_opts) {
          cols_to_keep <- c(cols_to_keep, "input_names")
        }

        if ("corrected" %in% include_opts) {
          cols_to_keep <- c(cols_to_keep, "corrected_name", "matched_name")
        }

        if ("ids" %in% include_opts) {
          cols_to_keep <- c(cols_to_keep, "idtax_n", "idtax_good_n")
        }

        if ("metadata" %in% include_opts) {
          cols_to_keep <- c(cols_to_keep, "match_methods", "match_scores",
                           "is_synonym", "accepted_name")
        }

        # Keep selected columns plus all trait columns
        # Trait columns are everything except the taxonomic metadata columns
        metadata_cols <- c("input_names", "idtax_n", "idtax_good_n",
                          "matched_name", "corrected_name", "match_methods",
                          "match_scores", "is_synonym", "accepted_name")
        trait_cols <- setdiff(names(enriched_result), metadata_cols)
        cols_to_keep <- c(cols_to_keep, trait_cols)

        enriched_result <- enriched_result %>%
          dplyr::select(dplyr::any_of(cols_to_keep))

        # Store enriched data (wide format)
        enriched_data(enriched_result)

        # Process LONG format data
        # Combine traits_raw from long format with taxonomic information
        if (!is.null(traits_result_long$traits_raw) && nrow(traits_result_long$traits_raw) > 0) {
          enriched_long <- traits_result_long$traits_raw %>%
            dplyr::left_join(
              matched_taxa %>%
                dplyr::select(idtax_n, matched_name, corrected_name),
              by = c("idtax" = "idtax_n")
            ) %>%
            # Add input names by joining with the filtered data
            dplyr::left_join(
              enriched_filtered %>%
                dplyr::group_by(idtax_n) %>%
                dplyr::summarise(
                  input_names = paste(unique(.data[[selected_col_name]]), collapse = " | "),
                  .groups = "drop"
                ),
              by = c("idtax" = "idtax_n")
            ) %>%
            # Reorder columns: input names, matched/corrected names, then trait data
            dplyr::select(
              input_names,
              matched_name,
              corrected_name,
              idtax,
              trait,
              traitvalue,
              traitvalue_char,
              valuetype,
              dplyr::everything()
            )

          enriched_data_long(enriched_long)

          # Build citation summary from long-format raw data
          if ("citation_key" %in% names(enriched_long)) {
            cit_summ <- enriched_long %>%
              dplyr::group_by(id_citation, citation_key, citation_authors,
                              citation_year, citation_title, citation_journal,
                              citation_doi, citation_dataset_name) %>%
              dplyr::summarise(
                n_measurements = dplyr::n(),
                n_taxa = dplyr::n_distinct(idtax),
                n_traits = dplyr::n_distinct(trait),
                .groups = "drop"
              ) %>%
              dplyr::arrange(dplyr::desc(n_measurements))
            citation_summary_data(cit_summ)
          } else {
            citation_summary_data(NULL)
          }

        } else {
          enriched_data_long(NULL)
          citation_summary_data(NULL)
        }

        shiny::showNotification(
          paste0(i18n()$t("Successfully enriched"), " ", nrow(enriched_result), " ", i18n()$t("unique taxa with trait data")),
          type = "message",
          duration = 5
        )

        enrichment_in_progress(FALSE)
        shinybusy::hide_spinner()

      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("Error enriching data:"), e$message),
          type = "error",
          duration = 10
        )
        enrichment_in_progress(FALSE)
        shinybusy::hide_spinner()
      })
    })

    # Results tabs (wide and long format)
    output$results_tabs <- shiny::renderUI({
      req(enriched_data())

      ns <- session$ns

      shiny::tabsetPanel(
        type = "tabs",

        # Wide format tab
        shiny::tabPanel(
          title = i18n()$t("Wide Format (Aggregated)"),
          icon = shiny::icon("table"),
          shiny::br(),

          shiny::div(
            style = "margin-bottom: 15px;",
            shiny::downloadButton(
              outputId = ns("download_wide"),
              label = i18n()$t("Download Wide Format"),
              class = "btn-success"
            )
          ),

          shiny::uiOutput(ns("preview_wide"))
        ),

        # Long format tab
        shiny::tabPanel(
          title = i18n()$t("Long Format (Detailed)"),
          icon = shiny::icon("list"),
          shiny::br(),

          shiny::div(
            style = "margin-bottom: 15px;",
            shiny::conditionalPanel(
              condition = paste0("output['", ns("has_long_data"), "']"),
              shiny::downloadButton(
                outputId = ns("download_long"),
                label = i18n()$t("Download Long Format"),
                class = "btn-success"
              )
            )
          ),

          shiny::uiOutput(ns("preview_long"))
        ),

        # Data Sources tab
        shiny::tabPanel(
          title = i18n()$t("Data Sources"),
          icon = shiny::icon("book"),
          shiny::br(),
          shiny::uiOutput(ns("citation_panel"))
        )
      )
    })

    # Check if long data exists
    output$has_long_data <- shiny::reactive({
      !is.null(enriched_data_long())
    })
    shiny::outputOptions(output, "has_long_data", suspendWhenHidden = FALSE)

    # Download handler for wide format
    output$download_wide <- shiny::downloadHandler(
      filename = function() {
        paste0("taxa_traits_wide_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },

      content = function(file) {
        req(enriched_data())
        sheets <- list(traits = enriched_data())
        cit <- citation_summary_data()
        if (!is.null(cit) && nrow(cit) > 0) {
          sheets$citations <- cit
        }
        writexl::write_xlsx(sheets, path = file)
      }
    )

    # Download handler for long format
    output$download_long <- shiny::downloadHandler(
      filename = function() {
        paste0("taxa_traits_long_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },

      content = function(file) {
        req(enriched_data_long())
        sheets <- list(traits = enriched_data_long())
        cit <- citation_summary_data()
        if (!is.null(cit) && nrow(cit) > 0) {
          sheets$citations <- cit
        }
        writexl::write_xlsx(sheets, path = file)
      }
    )

    # Preview wide format
    output$preview_wide <- shiny::renderUI({
      req(enriched_data())

      data <- enriched_data()
      n_rows <- nrow(data)
      n_cols <- ncol(data)

      shiny::div(
        shiny::h4(paste0(
          i18n()$t("Wide Format Preview"),
          " (",
          n_rows, " ", i18n()$t("unique taxa"),
          ", ",
          n_cols, " ", i18n()$t("columns"),
          ")"
        )),
        DT::renderDataTable({
          DT::datatable(
            data,
            options = list(
              scrollX = TRUE,
              scrollY = "400px",
              pageLength = 25,
              lengthMenu = c(10, 25, 50, 100, -1),
              lengthChange = TRUE
            )
          )
        })
      )
    })

    # Preview long format
    output$preview_long <- shiny::renderUI({
      if (is.null(enriched_data_long())) {
        shiny::div(
          style = "padding: 15px; background-color: #fff3cd; border-radius: 5px;",
          shiny::p(
            shiny::icon("info-circle"),
            i18n()$t("No detailed measurements available for these taxa."),
            style = "color: #856404; margin: 0;"
          )
        )
      } else {
        data <- enriched_data_long()
        n_rows <- nrow(data)
        n_cols <- ncol(data)

        shiny::div(
          shiny::h4(paste0(
            i18n()$t("Long Format Preview"),
            " (",
            n_rows, " ", i18n()$t("measurements"),
            ", ",
            n_cols, " ", i18n()$t("columns"),
            ")"
          )),
          shiny::p(
            shiny::icon("info-circle"),
            i18n()$t("This view shows individual trait measurements with remarks and measurement features included."),
            style = "color: #6c757d; font-style: italic;"
          ),
          DT::renderDataTable({
            DT::datatable(
              data,
              options = list(
                scrollX = TRUE,
                scrollY = "400px",
                pageLength = 25,
                lengthMenu = c(10, 25, 50, 100, -1),
                lengthChange = TRUE
              )
            )
          })
        )
      }
    })

    # Citation panel
    output$citation_panel <- shiny::renderUI({
      cit <- citation_summary_data()

      if (is.null(cit) || nrow(cit) == 0) {
        return(shiny::div(
          style = "padding: 20px; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
          shiny::icon("exclamation-triangle", style = "color: #856404; font-size: 20px;"),
          shiny::p(
            i18n()$t("No citation information available for these trait measurements. Citations may not have been assigned yet."),
            style = "color: #856404; margin: 8px 0 0 0;"
          )
        ))
      }

      # Acknowledgement banner
      ack_banner <- shiny::div(
        style = "padding: 20px; background: #d4edda; border-left: 5px solid #28a745; border-radius: 4px; margin-bottom: 20px;",
        shiny::h4(
          shiny::icon("exclamation-circle", style = "color: #155724;"),
          paste0(" ", i18n()$t("Please cite or acknowledge data sources")),
          style = "color: #155724; margin-top: 0;"
        ),
        shiny::p(
          i18n()$t("The trait data used to enrich your dataset comes from the databases and publications listed below. If you use these data in a publication, you must cite or acknowledge each source accordingly. Proper attribution ensures the sustainability of open data initiatives and recognizes the work of data collectors and curators."),
          style = "color: #155724; margin-bottom: 0;"
        )
      )

      # Summary statistics
      total_measurements <- sum(cit$n_measurements)
      n_sources <- nrow(cit)
      n_uncited <- sum(is.na(cit$citation_key))

      stats_row <- shiny::fluidRow(
        shiny::column(4, shiny::div(
          class = "card text-center p-3",
          style = "border-color: #007bff;",
          shiny::h3(n_sources, style = "color: #007bff; margin: 0;"),
          shiny::tags$small(i18n()$t("Data sources"))
        )),
        shiny::column(4, shiny::div(
          class = "card text-center p-3",
          style = "border-color: #28a745;",
          shiny::h3(total_measurements, style = "color: #28a745; margin: 0;"),
          shiny::tags$small(i18n()$t("Total measurements"))
        )),
        shiny::column(4, shiny::div(
          class = "card text-center p-3",
          style = if (n_uncited > 0) "border-color: #ffc107;" else "border-color: #6c757d;",
          shiny::h3(n_uncited, style = paste0("color: ", if (n_uncited > 0) "#ffc107" else "#6c757d", "; margin: 0;")),
          shiny::tags$small(i18n()$t("Unassigned sources"))
        ))
      )

      # Build citation cards
      citation_cards <- lapply(seq_len(nrow(cit)), function(i) {
        r <- cit[i, ]
        is_unknown <- is.na(r$citation_key)

        if (is_unknown) {
          # Unknown/unassigned citation
          shiny::div(
            style = "padding: 15px; margin: 10px 0; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
            shiny::fluidRow(
              shiny::column(9,
                shiny::tags$strong(
                  shiny::icon("question-circle", style = "color: #856404;"),
                  paste0(" ", i18n()$t("Unassigned source")),
                  style = "color: #856404;"
                ),
                shiny::p(
                  i18n()$t("These measurements do not have a citation assigned yet."),
                  style = "color: #856404; margin: 4px 0 0 0; font-size: 13px;"
                )
              ),
              shiny::column(3,
                shiny::div(
                  style = "text-align: right; padding-top: 5px;",
                  shiny::tags$span(
                    paste0(r$n_measurements, " ", i18n()$t("measurements")),
                    style = "font-weight: bold; color: #856404;"
                  ),
                  shiny::br(),
                  shiny::tags$small(
                    paste0(r$n_taxa, " ", i18n()$t("taxa"), " | ",
                           r$n_traits, " ", i18n()$t("traits")),
                    style = "color: #856404;"
                  )
                )
              )
            )
          )
        } else {
          # Known citation
          authors_str <- if (!is.na(r$citation_authors) && nchar(r$citation_authors) > 0) {
            r$citation_authors
          } else ""
          year_str <- if (!is.na(r$citation_year)) paste0(" (", r$citation_year, ")") else ""
          title_str <- if (!is.na(r$citation_title) && nchar(r$citation_title) > 0) {
            r$citation_title
          } else ""
          journal_str <- if (!is.na(r$citation_journal) && nchar(r$citation_journal) > 0) {
            paste0(". ", shiny::tags$em(r$citation_journal))
          } else ""
          doi_str <- if (!is.na(r$citation_doi) && nchar(r$citation_doi) > 0) {
            paste0(" DOI: ", r$citation_doi)
          } else ""

          shiny::div(
            style = "padding: 15px; margin: 10px 0; background: #f8f9fa; border-left: 4px solid #007bff; border-radius: 4px;",
            shiny::fluidRow(
              shiny::column(9,
                shiny::tags$strong(r$citation_key, style = "color: #007bff; font-size: 14px;"),
                if (!is.na(r$citation_dataset_name) && nchar(r$citation_dataset_name) > 0) {
                  shiny::tags$span(
                    paste0(" [", r$citation_dataset_name, "]"),
                    style = "color: #6c757d; font-size: 12px;"
                  )
                },
                shiny::div(
                  style = "margin-top: 6px; color: #495057; font-size: 13px;",
                  shiny::HTML(paste0(
                    authors_str, year_str, ". ",
                    title_str, journal_str, doi_str
                  ))
                )
              ),
              shiny::column(3,
                shiny::div(
                  style = "text-align: right; padding-top: 5px;",
                  shiny::tags$span(
                    paste0(r$n_measurements, " ", i18n()$t("measurements")),
                    style = "font-weight: bold; color: #007bff;"
                  ),
                  shiny::br(),
                  shiny::tags$small(
                    paste0(r$n_taxa, " ", i18n()$t("taxa"), " | ",
                           r$n_traits, " ", i18n()$t("traits")),
                    style = "color: #6c757d;"
                  )
                )
              )
            )
          )
        }
      })

      shiny::tagList(
        ack_banner,
        stats_row,
        shiny::br(),
        shiny::h4(
          shiny::icon("list-alt"),
          paste0(" ", i18n()$t("Sources contributing to your enriched dataset"))
        ),
        do.call(shiny::tagList, citation_cards)
      )
    })

    return(invisible(NULL))
  })
}
