# Feature Wizard - Step 2: Choose Operation Mode
#
# Module for selecting between operation modes:
#   - New Census (plot-level census metadata)
#   - Add Plot Features (any plot-level features)
#   - Add Individual Measurements (traits for existing individuals)
#   - Add Recruits (new individuals, uses import wizard flow)

#' Feature Wizard Step 2: Choose Mode - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step2_choose_mode_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("cogs"),
      i18n$t("Step 2: Choose Operation"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Select the type of data you want to add to your selected plots."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # ---- Row 1: Plot-level operations ----
    shiny::h5(i18n$t("Plot-Level Operations"),
              style = "color: #495057; margin-bottom: 10px;"),

    shiny::fluidRow(
      # New Census card
      shiny::column(
        6,
        shiny::div(
          id = ns("card_census"),
          class = "mode-card",
          onclick = sprintf(
            "Shiny.setInputValue('%s', 'new_census', {priority: 'event'})",
            ns("mode_selected")
          ),
          shiny::h4(
            shiny::icon("calendar-plus", style = "color: #28a745; margin-right: 10px;"),
            i18n$t("New Census")
          ),
          shiny::p(
            i18n$t("Add census metadata (census number, dates, team members) to selected plots. Ideal for recording a new field campaign."),
            style = "color: #6c757d;"
          ),
          shiny::tags$ul(
            style = "color: #6c757d; font-size: 14px;",
            shiny::tags$li(i18n$t("Auto-detects next census number")),
            shiny::tags$li(i18n$t("Date fields (year, month, day)")),
            shiny::tags$li(i18n$t("Team members (leader, PI, data manager)")),
            shiny::tags$li(i18n$t("Form entry or xlsx upload"))
          )
        )
      ),

      # Add Plot Features card
      shiny::column(
        6,
        shiny::div(
          id = ns("card_features"),
          class = "mode-card",
          onclick = sprintf(
            "Shiny.setInputValue('%s', 'add_features', {priority: 'event'})",
            ns("mode_selected")
          ),
          shiny::h4(
            shiny::icon("list-alt", style = "color: #007bff; margin-right: 10px;"),
            i18n$t("Add Plot Features")
          ),
          shiny::p(
            i18n$t("Add any plot-level features from the feature catalog. Choose from available feature types and enter values."),
            style = "color: #6c757d;"
          ),
          shiny::tags$ul(
            style = "color: #6c757d; font-size: 14px;",
            shiny::tags$li(i18n$t("Dynamic feature type selector")),
            shiny::tags$li(i18n$t("Numeric, character, or lookup values")),
            shiny::tags$li(i18n$t("Form entry or xlsx upload")),
            shiny::tags$li(i18n$t("Supports any subplot feature type"))
          )
        )
      )
    ),

    # ---- Row 2: Individual-level operations ----
    shiny::h5(i18n$t("Individual-Level Operations"),
              style = "color: #495057; margin-top: 25px; margin-bottom: 10px;"),

    shiny::fluidRow(
      # Add Recruits card (first — recruits should be added before measurements)
      shiny::column(
        6,
        shiny::div(
          id = ns("card_recruits"),
          class = "mode-card",
          onclick = sprintf(
            "Shiny.setInputValue('%s', 'add_recruits', {priority: 'event'})",
            ns("mode_selected")
          ),
          shiny::h4(
            shiny::icon("user-plus", style = "color: #fd7e14; margin-right: 10px;"),
            i18n$t("Add Recruits")
          ),
          shiny::p(
            i18n$t("Add new individuals (recruits) to selected plots. This opens the Import Wizard which handles column mapping, taxonomy matching, and validation for flat individual columns."),
            style = "color: #6c757d;"
          ),
          shiny::tags$ul(
            style = "color: #6c757d; font-size: 14px;",
            shiny::tags$li(i18n$t("Uses the Import Wizard (launch_import_wizard)")),
            shiny::tags$li(i18n$t("Flat columns only (no individual features)")),
            shiny::tags$li(i18n$t("Requires idtax_n from taxonomic matching")),
            shiny::tags$li(i18n$t("Add recruits before adding measurements"))
          )
        )
      ),

      # Add Individual Measurements card
      shiny::column(
        6,
        shiny::div(
          id = ns("card_measurements"),
          class = "mode-card",
          onclick = sprintf(
            "Shiny.setInputValue('%s', 'add_measurements', {priority: 'event'})",
            ns("mode_selected")
          ),
          shiny::h4(
            shiny::icon("ruler", style = "color: #17a2b8; margin-right: 10px;"),
            i18n$t("Add Individual Measurements")
          ),
          shiny::p(
            i18n$t("Add trait observations (DBH, height, etc.) for existing tagged individuals. Upload a file with plot name, tag and trait values."),
            style = "color: #6c757d;"
          ),
          shiny::tags$ul(
            style = "color: #6c757d; font-size: 14px;",
            shiny::tags$li(i18n$t("Wide format (one column per trait) or long format")),
            shiny::tags$li(i18n$t("Column/trait name mapping with synonyms")),
            shiny::tags$li(i18n$t("Matches individuals by plot name + tag")),
            shiny::tags$li(i18n$t("Links measurements to a census"))
          )
        )
      )
    ),

    # ---- Row 3: Structural operations ----
    shiny::h5(i18n$t("Structural Operations"),
              style = "color: #495057; margin-top: 25px; margin-bottom: 10px;"),

    shiny::fluidRow(
      # Define Multi-Stems card
      shiny::column(
        6,
        shiny::div(
          id = ns("card_multi_stems"),
          class = "mode-card",
          onclick = sprintf(
            "Shiny.setInputValue('%s', 'define_multi_stems', {priority: 'event'})",
            ns("mode_selected")
          ),
          shiny::h4(
            shiny::icon("project-diagram", style = "color: #6f42c1; margin-right: 10px;"),
            i18n$t("Define Multi-Stems")
          ),
          shiny::p(
            i18n$t("Group individual tags that belong to the same multi-stem tree. Upload a grouping table or define groups interactively."),
            style = "color: #6c757d;"
          ),
          shiny::tags$ul(
            style = "color: #6c757d; font-size: 14px;",
            shiny::tags$li(i18n$t("Upload pre-filled grouping table (xlsx)")),
            shiny::tags$li(i18n$t("Or group stems interactively per plot")),
            shiny::tags$li(i18n$t("Validates tag existence and taxonomy")),
            shiny::tags$li(i18n$t("Updates stem_grouping in database"))
          )
        )
      )
    ),

    # Selection indicator
    shiny::uiOutput(ns("mode_indicator"))
  )
}


#' Feature Wizard Step 2: Choose Mode - Server
#'
#' @param id Module namespace ID
#' @param i18n Reactive returning translator object
#' @return Reactive containing selected mode string
#' @keywords internal
#' @export
mod_feat_step2_choose_mode_server <- function(id, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_mode <- shiny::reactiveVal(NULL)
    all_card_ids <- c("card_census", "card_features", "card_measurements", "card_recruits", "card_multi_stems")

    shiny::observeEvent(input$mode_selected, {
      mode <- input$mode_selected
      selected_mode(mode)

      # Map mode to card id
      card_id <- switch(mode,
        new_census         = "card_census",
        add_features       = "card_features",
        add_measurements   = "card_measurements",
        add_recruits       = "card_recruits",
        define_multi_stems = "card_multi_stems"
      )

      # Update card styling via JS: remove 'selected' from all, add to chosen
      remove_js <- paste(
        sprintf("document.getElementById('%s').classList.remove('selected');", ns(all_card_ids)),
        collapse = "\n"
      )
      add_js <- sprintf("document.getElementById('%s').classList.add('selected');", ns(card_id))
      shinyjs::runjs(paste(remove_js, add_js, sep = "\n"))
    })

    output$mode_indicator <- shiny::renderUI({
      mode <- selected_mode()
      if (is.null(mode)) return(NULL)

      label <- switch(mode,
        new_census         = i18n()$t("New Census"),
        add_features       = i18n()$t("Add Plot Features"),
        add_measurements   = i18n()$t("Add Individual Measurements"),
        add_recruits       = i18n()$t("Add Recruits"),
        define_multi_stems = i18n()$t("Define Multi-Stems")
      )

      shiny::div(
        class = "alert alert-success",
        style = "margin-top: 20px;",
        shiny::icon("check-circle"), " ",
        shiny::strong(sprintf(i18n()$t("Selected: %s"), label)),
        " - ",
        i18n()$t("Click Next to continue.")
      )
    })

    return(shiny::reactive(selected_mode()))
  })
}
