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
    shiny::h5(i18n$t("Whole Campaign"),
              style = "color: #495057; margin-top: 25px; margin-bottom: 10px;"),

    shiny::fluidRow(
      # Full census card — the one-file path, listed first because it replaces
      # the recruits + measurements + multi-stems sequence below
      shiny::column(
        12,
        shiny::div(
          id = ns("card_import_census"),
          class = "mode-card",
          style = "border-color: #28a745;",
          onclick = sprintf(
            "Shiny.setInputValue('%s', 'import_census', {priority: 'event'})",
            ns("mode_selected")
          ),
          shiny::h4(
            shiny::icon("file-import", style = "color: #28a745; margin-right: 10px;"),
            i18n$t("Import a Full Census"),
            shiny::tags$span(
              class = "badge",
              style = "background: #28a745; color: white; font-size: 11px; margin-left: 10px;",
              i18n$t("recommended")
            )
          ),
          shiny::p(
            i18n$t("Upload one flat table for the whole campaign — existing stems and recruits together. The wizard compares tags against the database, works out which rows are which, and writes everything in a single operation."),
            style = "color: #6c757d;"
          ),
          shiny::tags$ul(
            style = "color: #6c757d; font-size: 14px;",
            shiny::tags$li(i18n$t("No need to split your file into recruits and remeasures")),
            shiny::tags$li(i18n$t("Flags tags that look mistyped before they become duplicate trees")),
            shiny::tags$li(i18n$t("Creates the census, the recruits, the multi-stem groups and the measurements together")),
            shiny::tags$li(i18n$t("Rolls back completely if any step fails"))
          )
        )
      )
    ),

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
            i18n$t("Add feature observations (DBH, height, position, etc.) for existing tagged individuals. Upload a file with plot name, tag and feature values."),
            style = "color: #6c757d;"
          ),
          shiny::tags$ul(
            style = "color: #6c757d; font-size: 14px;",
            shiny::tags$li(i18n$t("Wide format (one column per feature) or long format")),
            shiny::tags$li(i18n$t("Column/feature name mapping with synonyms")),
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

    # ---- Row 4: Derived / computed traits ----
    shiny::h5(i18n$t("Derived / Computed Traits"),
              style = "color: #495057; margin-top: 25px; margin-bottom: 10px;"),

    shiny::fluidRow(
      # Compute Stem Status card
      shiny::column(
        6,
        shiny::div(
          id = ns("card_stem_status"),
          class = "mode-card",
          onclick = sprintf(
            "Shiny.setInputValue('%s', 'compute_stem_status', {priority: 'event'})",
            ns("mode_selected")
          ),
          shiny::h4(
            shiny::icon("heartbeat", style = "color: #dc3545; margin-right: 10px;"),
            i18n$t("Compute Stem Status")
          ),
          shiny::p(
            i18n$t("Compute or recompute vital status (alive / dead / presumed_dead) for all stems of the selected plots. Run AFTER adding new measurements and recruits for a census."),
            style = "color: #6c757d;"
          ),
          shiny::tags$ul(
            style = "color: #6c757d; font-size: 14px;",
            shiny::tags$li(i18n$t("Uses diameter, observations, and RainFor flags")),
            shiny::tags$li(i18n$t("Retroactively corrects presumed_dead → alive")),
            shiny::tags$li(i18n$t("Review table before any database write")),
            shiny::tags$li(i18n$t("Run after adding measurements for a census"))
          )
        )
      ),

      # Standardize Observations card
      shiny::column(
        6,
        shiny::div(
          id = ns("card_standardize_obs"),
          class = "mode-card",
          onclick = sprintf(
            "Shiny.setInputValue('%s', 'standardize_observations', {priority: 'event'})",
            ns("mode_selected")
          ),
          shiny::h4(
            shiny::icon("magnifying-glass-chart",
                        style = "color: #20c997; margin-right: 10px;"),
            i18n$t("Standardize Observations")
          ),
          shiny::p(
            i18n$t("Parse free-text 'observations' into standardized rows for mortality_risk_flag (multi-token) and dawkins_index (single value). Existing dawkins values are never overwritten."),
            style = "color: #6c757d;"
          ),
          shiny::tags$ul(
            style = "color: #6c757d; font-size: 14px;",
            shiny::tags$li(i18n$t("Regex ontology (editable CSV in inst/ontology/)")),
            shiny::tags$li(i18n$t("Splits 'cassé bas; beaucoup de lianes' into atomic phrases")),
            shiny::tags$li(i18n$t("Review derived rows + unresolved phrases before DB write")),
            shiny::tags$li(i18n$t("Original 'observations' trait is not modified"))
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
    all_card_ids <- c("card_census", "card_features", "card_measurements", "card_recruits", "card_multi_stems", "card_stem_status", "card_standardize_obs", "card_import_census")

    shiny::observeEvent(input$mode_selected, {
      mode <- input$mode_selected
      selected_mode(mode)

      # Map mode to card id
      card_id <- switch(mode,
        new_census          = "card_census",
        add_features        = "card_features",
        add_measurements         = "card_measurements",
        add_recruits             = "card_recruits",
        define_multi_stems       = "card_multi_stems",
        compute_stem_status      = "card_stem_status",
        standardize_observations = "card_standardize_obs",
        import_census            = "card_import_census"
      )

      shinyjs::runjs(.mode_selection_js(ns(all_card_ids), ns(card_id)))
    })

    output$mode_indicator <- shiny::renderUI({
      mode <- selected_mode()
      if (is.null(mode)) return(NULL)

      label <- switch(mode,
        new_census          = i18n()$t("New Census"),
        add_features        = i18n()$t("Add Plot Features"),
        add_measurements         = i18n()$t("Add Individual Measurements"),
        add_recruits             = i18n()$t("Add Recruits"),
        define_multi_stems       = i18n()$t("Define Multi-Stems"),
        compute_stem_status      = i18n()$t("Compute Stem Status"),
        standardize_observations = i18n()$t("Standardize Observations"),
        import_census            = i18n()$t("Import a Full Census")
      )

      shiny::div(
        class = "alert alert-success",
        style = paste("margin-top: 20px; font-size: 16px;",
                      "display: flex; align-items: center; gap: 10px;"),
        shiny::icon("check-circle", style = "font-size: 20px;"),
        shiny::strong(sprintf(i18n()$t("Selected: %s"), label)),
        shiny::span(
          style = "margin-left: auto; color: #155724;",
          i18n()$t("Click Next to continue."), " ",
          shiny::icon("arrow-right")
        )
      )
    })

    return(shiny::reactive(selected_mode()))
  })
}


#' The browser side of choosing an operation
#'
#' The eight cards fill more than one screen, so a click at the top used to be
#' invisible from the bottom: the only confirmation was an alert below the
#' fold, and the Next button that goes with it further down still. This marks
#' the chosen card, fades the rest, and brings the confirmation and the
#' buttons into view when they are not already there.
#'
#' The scroll waits a moment so the confirmation alert is on the page before
#' the browser measures where to stop, and does nothing when the buttons are
#' already on screen — no motion the user did not need.
#'
#' @param card_ids Character vector of namespaced card element ids.
#' @param chosen_id The namespaced id of the card that was clicked.
#'
#' @return A single string of JavaScript.
#' @keywords internal
.mode_selection_js <- function(card_ids, chosen_id) {
  others <- paste(
    sprintf(
      paste0("var el = document.getElementById('%s');",
             " if (el) { el.classList.remove('selected');",
             " el.classList.add('dimmed'); }"),
      card_ids
    ),
    collapse = "\n"
  )

  chosen <- sprintf(
    paste0("var chosen = document.getElementById('%s');",
           " if (chosen) { chosen.classList.remove('dimmed');",
           " chosen.classList.add('selected'); }"),
    chosen_id
  )

  reveal <- paste0(
    "setTimeout(function() {",
    " var nav = document.querySelector('.nav-buttons');",
    " if (!nav) { return; }",
    " var box = nav.getBoundingClientRect();",
    " var h = window.innerHeight || document.documentElement.clientHeight;",
    " if (box.top < 0 || box.bottom > h) {",
    " nav.scrollIntoView({behavior: 'smooth', block: 'center'}); }",
    " }, 150);"
  )

  paste(others, chosen, reveal, sep = "\n")
}
