#' Extraction Configuration Module - UI
#'
#' UI component for configuring individual extraction options
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_extraction_config_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("

        /* ---- CSS-only question-mark tooltips (no JS required) ---- */
        .tip {
          position: relative;
          display: inline-block;
          vertical-align: middle;
          cursor: help;
        }
        .tip .fa {
          color: #bbb;
          font-size: 0.82em;
          margin-left: 3px;
          transition: color 0.15s;
        }
        .tip:hover .fa { color: #0d6efd; }
        .tip::after {
          content: attr(data-tip);
          position: absolute;
          bottom: calc(100% + 6px);
          left: 50%;
          transform: translateX(-50%);
          background: #2c2c2c;
          color: #fff;
          padding: 7px 11px;
          border-radius: 5px;
          font-size: 0.78em;
          font-weight: normal;
          min-width: 200px;
          max-width: 280px;
          line-height: 1.5;
          z-index: 9999;
          pointer-events: none;
          white-space: normal;
          text-align: left;
          opacity: 0;
          visibility: hidden;
          transition: opacity 0.18s;
          box-shadow: 0 2px 8px rgba(0,0,0,0.25);
        }
        .tip:hover::after { opacity: 1; visibility: visible; }

        /* ---- Section cards ---- */
        .cfg-card {
          border: 1px solid #dee2e6;
          border-radius: 8px;
          margin-bottom: 18px;
          overflow: visible;
        }
        .cfg-card-header {
          display: flex;
          align-items: center;
          gap: 8px;
          padding: 10px 16px;
          border-bottom: 1px solid #dee2e6;
          border-radius: 8px 8px 0 0;
          font-weight: 600;
          font-size: 0.95em;
          color: #343a40;
        }
        .cfg-card-header.blue  { background: linear-gradient(90deg, #e3f0ff, #f0f7ff); }
        .cfg-card-header.green { background: linear-gradient(90deg, #e6f9f0, #f0fbf5); }
        .cfg-card-header.amber { background: linear-gradient(90deg, #fff8e1, #fffdf0); }
        .cfg-card-header.grey  { background: linear-gradient(90deg, #f0f4f8, #f8f9fa); }
        .cfg-card-body { padding: 14px 16px; }
        .cfg-divider { border: none; border-top: 1px dashed #dee2e6; margin: 12px 0; }

        /* ---- Advanced collapsible section (native <details>) ---- */
        details.adv-section { margin-top: 6px; }
        details.adv-section > summary {
          list-style: none;
          cursor: pointer;
          display: flex;
          align-items: center;
          gap: 8px;
          padding: 12px 16px;
          background: linear-gradient(90deg, #cfe2ff, #dbeafe);
          border: 1px solid #9ec5fe;
          border-radius: 8px;
          font-weight: 600;
          color: #084298;
          user-select: none;
          transition: background 0.2s;
        }
        details.adv-section > summary:hover {
          background: linear-gradient(90deg, #b6d4fe, #c5d9fe);
        }
        details.adv-section > summary::-webkit-details-marker { display: none; }
        details.adv-section > summary .adv-hint {
          font-size: 0.78em;
          font-weight: normal;
          font-style: italic;
          color: #4a6da8;
          margin-left: auto;
        }
        details.adv-section .adv-body {
          border: 1px solid #9ec5fe;
          border-top: none;
          border-radius: 0 0 8px 8px;
          padding: 16px;
          background: #f8faff;
        }

        /* ---- Misc ---- */
        .radio label { font-weight: normal !important; }
        .option-label {
          font-weight: 600;
          margin-bottom: 4px;
          font-size: 0.92em;
        }

      "))
    ),
    shiny::uiOutput(ns("config_ui"))
  )
}

#' Extraction Configuration Module - Server
#'
#' Server logic for extraction configuration
#'
#' @param id Module namespace ID
#' @param selected_plots Reactive containing selected plot IDs
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return A reactive list with:
#'   \item{options}{Named list of extraction options}
#'   \item{execute_trigger}{Reactive counter incremented on extract}
#'   \item{individual_features_options}{Named list of advanced query options}
#'   \item{individual_features_trigger}{Reactive counter incremented on query}
#'
#' @keywords internal
#' @export
mod_extraction_config_server <- function(id, selected_plots, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    execute_counter            <- shiny::reactiveVal(0)
    individual_features_counter <- shiny::reactiveVal(0)

    # ---- Main UI (all rendered dynamically for i18n) -------------------------
    output$config_ui <- shiny::renderUI({

      # CSS tooltip helper — hover the ? icon to read the explanation
      tip <- function(text) {
        shiny::tags$span(class = "tip", `data-tip` = text,
          shiny::tags$i(class = "fa fa-question-circle"))
      }

      # Labelled section card
      cfg_card <- function(color, icon_name, title, ...) {
        shiny::div(
          class = "cfg-card",
          shiny::div(
            class = paste("cfg-card-header", color),
            shiny::icon(icon_name), " ", title
          ),
          shiny::div(class = "cfg-card-body", ...)
        )
      }

      shiny::tagList(

        shiny::h4(shiny::icon("sliders-h"), " ",
                  i18n()$t("Individual Extraction Configuration")),
        shiny::p(class = "text-muted",
          i18n()$t("Configure how individual tree data should be extracted and formatted."),
          " ",
          i18n()$t("Each section is independent — hover the"),
          shiny::tags$i(class = "fa fa-question-circle", style = "color:#bbb;"),
          i18n()$t("icons for explanations.")
        ),

        # ===== 1. Output Style ===============================================
        cfg_card("blue", "table", i18n()$t("Output Style"),
          shiny::p(
            class = "text-muted",
            style = "font-size: 0.88em; margin-bottom: 10px;",
            i18n()$t("Choose which column preset to use in the extracted data."),
            " ",
            i18n()$t("Select the preset that best matches your analysis workflow.")
          ),
          shiny::radioButtons(
            ns("output_style"),
            NULL,
            choiceNames = list(
              shiny::span(
                i18n()$t("Auto-detect from plot method"),
                tip(i18n()$t("Automatically selects the best format based on the plot method: transect data use the transect preset, permanent plots use permanent_plot, etc."))
              ),
              shiny::span(
                i18n()$t("Minimal"),
                tip(i18n()$t("Only essential columns: plot name, species tag, and DBH. Best for quick summaries."))
              ),
              shiny::span(
                i18n()$t("Standard"),
                tip(i18n()$t("Common columns for general ecological analysis: dates, coordinates, and main measurements."))
              ),
              shiny::span(
                i18n()$t("Permanent Plot — single census"),
                tip(i18n()$t("Structured for permanent plot monitoring with one census per individual row."))
              ),
              shiny::span(
                i18n()$t("Permanent Plot — multi-census"),
                tip(i18n()$t("Preserves all census columns side by side. Best for time-series and growth-rate analysis."))
              ),
              shiny::span(
                i18n()$t("Transect"),
                tip(i18n()$t("Simplified format optimised for walk-survey transect data (no census dimension)."))
              ),
              shiny::span(
                i18n()$t("Full (all columns)"),
                tip(i18n()$t("Every available column included. Largest output; use for complete data export."))
              )
            ),
            choiceValues = c("auto", "minimal", "standard", "permanent_plot",
                             "permanent_plot_multi_census", "transect", "full"),
            selected = isolate(input$output_style) %||% "auto"
          ),
          shiny::uiOutput(ns("style_description"))
        ),

        # ===== 2. Census Handling =============================================
        cfg_card("green", "calendar", i18n()$t("Census Handling"),
          shiny::p(
            class = "text-muted",
            style = "font-size: 0.88em; margin-bottom: 10px;",
            i18n()$t("Control how data from multiple census visits is aggregated or laid out in the output.")
          ),

          # 2a — Census strategy
          shiny::div(class = "option-label",
            i18n()$t("When an individual has been measured several times, keep:"),
            tip(i18n()$t("Applies to plots with multiple census visits. Controls which measurement(s) appear in the output."))
          ),
          shiny::radioButtons(
            ns("census_strategy"),
            NULL,
            choiceNames = list(
              shiny::span(
                i18n()$t("Last (most recent) census"),
                tip(i18n()$t("Keep only the most recent measurement per individual."))
              ),
              shiny::span(
                i18n()$t("First (earliest) census"),
                tip(i18n()$t("Keep only the earliest measurement per individual."))
              ),
              shiny::span(
                i18n()$t("Mean across all censuses"),
                tip(i18n()$t("Average numeric values across all census dates per individual."))
              )
            ),
            choiceValues = c("last", "first", "mean"),
            selected = isolate(input$census_strategy) %||% "last",
            inline = TRUE
          ),

          shiny::div(class = "cfg-divider"),

          # 2b — Multiple census columns
          shiny::div(class = "option-label",
            i18n()$t("Multiple census columns"),
            tip(i18n()$t("Creates separate columns per census visit (e.g. dbh_2010, dbh_2015). Only meaningful for permanent multi-census plots."))
          ),
          shiny::checkboxInput(
            ns("show_multiple_census"),
            i18n()$t("Show each census as separate columns"),
            value = isolate(input$show_multiple_census) %||% FALSE
          ),

          shiny::div(class = "cfg-divider"),

          # 2c — Individual features format (main extraction)
          shiny::div(class = "option-label",
            i18n()$t("Individual features format — main extraction"),
            tip(i18n()$t("Controls the layout of individual-level features included alongside the main tree data. This is completely independent from the advanced query section below."))
          ),
          shiny::radioButtons(
            ns("individual_features_format"),
            NULL,
            choiceNames = list(
              shiny::span(
                i18n()$t("Wide — one row per individual"),
                tip(i18n()$t("Features become columns. Compact; multiple observations per individual are aggregated."))
              ),
              shiny::span(
                i18n()$t("Long — one row per measurement"),
                tip(i18n()$t("Each observation gets its own row. No aggregation; full detail but more rows."))
              ),
              shiny::span(
                i18n()$t("Census pairs — one row per census interval"),
                tip(i18n()$t("Pairs consecutive censuses per individual. Columns include dbh0, dbh1, date0, date1, elapsed days."))
              )
            ),
            choiceValues = c("wide", "long", "census_pairs"),
            selected = isolate(input$individual_features_format) %||% "wide",
            inline = TRUE
          )
        ),

        # ===== 3. Data Organisation ===========================================
        cfg_card("amber", "cog", i18n()$t("Data Organisation"),
          shiny::p(
            class = "text-muted",
            style = "font-size: 0.88em; margin-bottom: 10px;",
            i18n()$t("Fine-tune the structure and cleanliness of the extracted output.")
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(class = "option-label",
                i18n()$t("Multiple stems"),
                tip(i18n()$t("For multi-stemmed trees: merge all stem measurements into one row, values separated by semicolons. When unchecked, each stem gets its own row."))
              ),
              shiny::checkboxInput(
                ns("concatenate_stem"),
                i18n()$t("Concatenate stems per individual"),
                value = isolate(input$concatenate_stem) %||% FALSE
              )
            ),
            shiny::column(6,
              shiny::div(class = "option-label",
                i18n()$t("Database ID columns"),
                tip(i18n()$t("Internal IDs (id_n, id_plot, etc.) are useful for cross-referencing with the database but add clutter to analysis datasets."))
              ),
              shiny::checkboxInput(
                ns("remove_ids"),
                i18n()$t("Remove internal ID columns"),
                value = isolate(input$remove_ids) %||% TRUE
              )
            )
          ),
          shiny::div(class = "cfg-divider"),
          shiny::div(class = "option-label",
            i18n()$t("Flagged record handling"),
            tip(i18n()$t("Some records are flagged as potentially problematic (e.g. biologically implausible DBH growth between censuses). Choose how they appear in the output."))
          ),
          shiny::selectInput(
            ns("issues"),
            NULL,
            choices = stats::setNames(
              c("remove", "include", "ignore"),
              c(i18n()$t("Remove flagged records (recommended for analysis)"),
                i18n()$t("Keep flagged records and add flag columns"),
                i18n()$t("Ignore flags — include all records without flag columns"))
            ),
            selected = isolate(input$issues) %||% "remove"
          )
        ),

        # ===== 4. Additional Data =============================================
        cfg_card("grey", "plus-circle", i18n()$t("Additional Data"),
          shiny::p(
            class = "text-muted",
            style = "font-size: 0.88em; margin-bottom: 10px;",
            i18n()$t("Optionally enrich the extraction with linked datasets joined to individual tree records.")
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(class = "option-label",
                i18n()$t("Taxonomic traits"),
                tip(i18n()$t("Joins species-level traits (e.g. wood density, leaf area index) from the trait database to each individual row."))
              ),
              shiny::checkboxInput(
                ns("extract_traits"),
                i18n()$t("Include taxonomic traits"),
                value = isolate(input$extract_traits) %||% TRUE
              ),
              shiny::br(),
              shiny::div(class = "option-label",
                i18n()$t("Individual-level features"),
                tip(i18n()$t("Appends individual-level observations linked to each tree (e.g. phenology codes, damage records, buttress presence)."))
              ),
              shiny::checkboxInput(
                ns("extract_individual_features"),
                i18n()$t("Include individual features"),
                value = isolate(input$extract_individual_features) %||% TRUE
              )
            ),
            shiny::column(6,
              shiny::div(class = "option-label",
                i18n()$t("Subplot features"),
                tip(i18n()$t("Appends subplot-level information linked to the plot (e.g. soil samples, observer notes, census date metadata)."))
              ),
              shiny::checkboxInput(
                ns("extract_subplot_features"),
                i18n()$t("Include subplot features"),
                value = isolate(input$extract_subplot_features) %||% TRUE
              ),
              shiny::br(),
              shiny::div(class = "option-label",
                i18n()$t("Genus fallback for traits"),
                tip(i18n()$t("When a species has no trait data in the database, substitute with the genus-level average value."))
              ),
              shiny::checkboxInput(
                ns("traits_to_genera"),
                i18n()$t("Fall back to genus-level traits"),
                value = isolate(input$traits_to_genera) %||% FALSE
              )
            )
          )
        ),

        # ===== Extract Button =================================================
        shiny::actionButton(
          ns("extract_individuals"),
          shiny::tagList(
            shiny::icon("download"), " ",
            i18n()$t("Extract Individuals from Selected Plots")
          ),
          class = "btn-success btn-lg",
          style = "width: 100%; margin-bottom: 20px;"
        ),

        # ===== Advanced: Individual Features Query (collapsible) ==============
        shiny::tags$details(
          class = "adv-section",
          shiny::tags$summary(
            shiny::icon("microscope"), " ",
            i18n()$t("Advanced — Query Individual Features Separately"),
            shiny::span(class = "adv-hint", i18n()$t("click to expand"))
          ),
          shiny::div(
            class = "adv-body",

            shiny::p(
              class = "text-muted",
              style = "font-size: 0.88em; margin-bottom: 8px;",
              i18n()$t("This optional step queries all attributes linked to each individual tree — trait measurements, phenological observations, etc. — and returns them as a dedicated table."),
              " ",
              shiny::strong(i18n()$t("The result is independent from the main extraction above and can use a different format."))
            ),
            shiny::div(
              class = "alert alert-warning",
              style = "font-size: 0.85em; padding: 8px 12px; margin-bottom: 14px;",
              shiny::icon("exclamation-triangle"), " ",
              shiny::strong(i18n()$t("Prerequisite:")), " ",
              i18n()$t("Run the main extraction first (button above). This step uses the individual IDs from that result.")
            ),

            # Trait selection
            shiny::div(class = "option-label",
              i18n()$t("Trait selection"),
              tip(i18n()$t("Choose whether to retrieve all trait types or restrict to specific ones identified by their database IDs."))
            ),
            shiny::radioButtons(
              ns("trait_selection_mode"),
              NULL,
              choiceNames = list(
                shiny::span(i18n()$t("All available traits")),
                shiny::span(
                  i18n()$t("Specific traits only (enter IDs below)"),
                  tip(i18n()$t("Enter comma-separated trait IDs from the database to restrict the query."))
                )
              ),
              choiceValues = c("all", "specific"),
              selected = "all",
              inline = TRUE
            ),
            shinyjs::hidden(
              shiny::div(
                id = ns("trait_ids_input_panel"),
                shiny::textInput(
                  ns("trait_ids_input"),
                  i18n()$t("Trait IDs (comma-separated)"),
                  value = "",
                  placeholder = i18n()$t("e.g. 1, 2, 5, 10")
                )
              )
            ),

            shiny::div(class = "cfg-divider"),

            # Output format — independent from the main individual_features_format
            shiny::div(class = "option-label",
              i18n()$t("Output format for this table"),
              tip(i18n()$t("Controls the shape of this separate individual features table. Completely independent from the format chosen in the Census Handling section above."))
            ),
            shiny::radioButtons(
              ns("indiv_feat_query_format"),
              NULL,
              choiceNames = list(
                shiny::span(
                  i18n()$t("Wide — measurements as columns"),
                  tip(i18n()$t("One row per individual; each trait is a column. Values are aggregated if multiple observations exist."))
                ),
                shiny::span(
                  i18n()$t("Long — measurements as rows"),
                  tip(i18n()$t("One row per measurement. No aggregation; the most complete representation."))
                ),
                shiny::span(
                  i18n()$t("Census pairs — one row per census interval"),
                  tip(i18n()$t("Pairs consecutive censuses per individual. Columns: dbh0, dbh1, date0, date1, elapsed days, stem status."))
                )
              ),
              choiceValues = c("wide", "long", "census_pairs"),
              selected = "wide",
              inline = TRUE
            ),
            shiny::div(
              class = "alert alert-info",
              style = "font-size: 0.84em; margin-top: -4px; padding: 8px 12px;",
              shiny::uiOutput(ns("format_explanation"))
            ),

            shiny::div(class = "cfg-divider"),

            shiny::fluidRow(
              shiny::column(6,
                shiny::div(class = "option-label",
                  i18n()$t("Multi-census data"),
                  tip(i18n()$t("Include features recorded at multiple census dates rather than only the selected census."))
                ),
                shiny::checkboxInput(
                  ns("include_multi_census_features"),
                  i18n()$t("Include multi-census data"),
                  value = FALSE
                )
              ),
              shiny::column(6,
                shiny::div(class = "option-label",
                  i18n()$t("Measurement metadata"),
                  tip(i18n()$t("Add columns with observer name, recording date, and method for each measurement."))
                ),
                shiny::checkboxInput(
                  ns("include_metadata_features"),
                  i18n()$t("Include measurement metadata"),
                  value = FALSE
                )
              )
            ),

            shiny::div(
              style = "margin-top: 12px;",
              shiny::actionButton(
                ns("query_individual_features"),
                shiny::tagList(
                  shiny::icon("search"), " ",
                  i18n()$t("Query Individual Features")
                ),
                class = "btn-info",
                style = "width: 100%;"
              )
            )
          )
        )
      )
    })

    # ---- Toggle trait IDs input when "specific" mode is selected ------------
    shiny::observeEvent(input$trait_selection_mode, {
      if (identical(input$trait_selection_mode, "specific")) {
        shinyjs::show("trait_ids_input_panel")
      } else {
        shinyjs::hide("trait_ids_input_panel")
      }
    })

    # ---- Dynamic format explanation for the advanced query section ----------
    output$format_explanation <- shiny::renderUI({
      shiny::req(input$indiv_feat_query_format)

      if (input$indiv_feat_query_format == "wide") {
        shiny::tagList(
          shiny::strong(i18n()$t("Wide format:")), " ",
          i18n()$t("One row per individual, measurements as columns. Values are aggregated if multiple observations exist per individual.")
        )
      } else if (input$indiv_feat_query_format == "census_pairs") {
        shiny::tagList(
          shiny::strong(i18n()$t("Census pairs format:")), " ",
          i18n()$t("One row per consecutive census pair per individual. Columns: dbh0, dbh1, date_census0, date_census1, time (days between censuses), stem_status at second census.")
        )
      } else {
        shiny::tagList(
          shiny::strong(i18n()$t("Long format:")), " ",
          i18n()$t("One row per measurement. Most complete representation — no aggregation, no information loss.")
        )
      }
    })

    # ---- Dynamic description for the selected output style ------------------
    output$style_description <- shiny::renderUI({
      shiny::req(input$output_style)

      style_descriptions <- list(
        auto                      = i18n()$t("Automatically selects the best format based on plot method"),
        minimal                   = i18n()$t("Returns only essential columns (plot, tag, species, dbh)"),
        standard                  = i18n()$t("Common columns for general ecological analysis"),
        permanent_plot            = i18n()$t("Structured format for permanent plot monitoring (single census)"),
        permanent_plot_multi_census = i18n()$t("Preserves all census columns for time-series analysis"),
        transect                  = i18n()$t("Simplified format optimised for transect walk surveys"),
        full                      = i18n()$t("Complete dataset with all available columns")
      )

      shiny::div(
        class = "alert alert-info",
        style = "margin-top: 10px; font-size: 0.88em;",
        shiny::icon("info-circle"), " ",
        style_descriptions[[input$output_style]]
      )
    })

    # ---- Execute button handler ---------------------------------------------
    shiny::observeEvent(input$extract_individuals, {
      cli::cli_alert_info("Extract button clicked!")
      plots <- selected_plots()
      cli::cli_alert_info("Selected plots: {if(is.null(plots)) 'NULL' else paste(length(plots), 'plots')}")

      if (is.null(plots) || length(plots) == 0) {
        cli::cli_alert_warning("No plots selected!")
        shiny::showNotification(
          i18n()$t("Please select at least one plot before extracting individuals"),
          type = "warning", duration = 5
        )
        return()
      }

      execute_counter(execute_counter() + 1)
      cli::cli_alert_success("Execute counter now at: {execute_counter()}")
    })

    # ---- Individual features query button handler --------------------------
    shiny::observeEvent(input$query_individual_features, {
      cli::cli_alert_info("Query individual features button clicked!")
      plots <- selected_plots()

      if (is.null(plots) || length(plots) == 0) {
        cli::cli_alert_warning("No plots selected!")
        shiny::showNotification(
          i18n()$t("Please select at least one plot before querying individual features"),
          type = "warning", duration = 5
        )
        return()
      }

      individual_features_counter(individual_features_counter() + 1)
      cli::cli_alert_success("Individual features counter now at: {individual_features_counter()}")
    })

    # ---- Options reactives --------------------------------------------------
    options <- shiny::reactive({
      list(
        output_style               = input$output_style                %||% "auto",
        census_strategy            = input$census_strategy             %||% "last",
        show_multiple_census       = input$show_multiple_census        %||% FALSE,
        individual_features_format = input$individual_features_format  %||% "wide",
        concatenate_stem           = input$concatenate_stem            %||% FALSE,
        remove_ids                 = input$remove_ids                  %||% TRUE,
        issues                     = input$issues                      %||% "remove",
        extract_traits             = input$extract_traits              %||% TRUE,
        extract_individual_features = input$extract_individual_features %||% TRUE,
        extract_subplot_features   = input$extract_subplot_features    %||% TRUE,
        traits_to_genera           = input$traits_to_genera            %||% FALSE
      )
    })

    individual_features_options <- shiny::reactive({
      trait_ids <- NULL
      if (identical(input$trait_selection_mode, "specific")) {
        trait_ids_text <- input$trait_ids_input %||% ""
        if (nzchar(trait_ids_text)) {
          trait_ids <- as.integer(strsplit(trait_ids_text, ",\\s*")[[1]])
          trait_ids <- trait_ids[!is.na(trait_ids)]
        }
      }

      list(
        enabled             = TRUE,  # section opened by user clicking <details>
        trait_ids           = trait_ids,
        format              = input$indiv_feat_query_format   %||% "wide",
        include_multi_census = input$include_multi_census_features %||% FALSE,
        census_strategy     = input$census_strategy           %||% "last",
        include_metadata    = input$include_metadata_features %||% FALSE,
        issues              = input$issues                    %||% "remove"
      )
    })

    return(list(
      options                    = options,
      execute_trigger            = shiny::reactive(execute_counter()),
      individual_features_options = individual_features_options,
      individual_features_trigger = shiny::reactive(individual_features_counter())
    ))
  })
}
