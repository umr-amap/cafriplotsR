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
    shiny::uiOutput(ns("download_button")),

    shiny::hr(),

    shiny::uiOutput(ns("preview"))
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

        # Fetch traits using query_taxa_traits
        shiny::showNotification(
          paste0(i18n()$t("Fetching traits for"), " ", nrow(matched_taxa), " ", i18n()$t("taxa...")),
          duration = NULL,
          id = "fetch_traits",
          type = "message"
        )

        traits_result <- query_taxa_traits(
          idtax = matched_taxa$idtax_n,
          format = input$output_format %||% "wide",
          add_taxa_info = FALSE,  # We already have taxa info
          include_synonyms = TRUE,
          categorical_mode = input$categorical_format %||% "mode",
          con_taxa = NULL
        )

        shiny::removeNotification("fetch_traits")

        # Check if we got results
        has_numeric <- !is.null(traits_result$traits_numeric) &&
                       !inherits(traits_result$traits_numeric, "logical")
        has_categorical <- !is.null(traits_result$traits_categorical) &&
                          !inherits(traits_result$traits_categorical, "logical")

        if (is.null(traits_result) || (!has_numeric && !has_categorical)) {
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

        # Join numeric traits if available
        # Note: query_taxa_traits returns column named "idtax", not "idtax_n"
        if (has_numeric && nrow(traits_result$traits_numeric) > 0) {
          # Remove id_trait_measures columns before joining
          numeric_traits <- traits_result$traits_numeric %>%
            dplyr::select(-dplyr::starts_with("id_trait_measures"))

          enriched_result <- enriched_result %>%
            dplyr::left_join(
              numeric_traits,
              by = c("idtax_n" = "idtax")
            )
        }

        # Join categorical traits if available
        if (has_categorical && nrow(traits_result$traits_categorical) > 0) {
          # Remove id_trait_measures columns before joining
          categorical_traits <- traits_result$traits_categorical %>%
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

        # Store enriched data
        enriched_data(enriched_result)

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

    # Download button
    output$download_button <- shiny::renderUI({
      req(enriched_data())

      ns <- session$ns

      shiny::div(
        style = "margin-top: 20px;",
        shiny::downloadButton(
          outputId = ns("download_enriched"),
          label = i18n()$t("Download Enriched Data"),
          class = "btn-success"
        )
      )
    })

    # Download handler
    output$download_enriched <- shiny::downloadHandler(
      filename = function() {
        paste0("taxa_traits_enriched_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },

      content = function(file) {
        req(enriched_data())
        writexl::write_xlsx(enriched_data(), path = file)
      }
    )

    # Preview
    output$preview <- shiny::renderUI({
      req(enriched_data())

      data <- enriched_data()
      n_rows <- nrow(data)
      n_cols <- ncol(data)

      shiny::div(
        shiny::h4(paste0("Enriched Data Preview (", n_rows, " unique taxa, ", n_cols, " columns)")),
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

    return(invisible(NULL))
  })
}
