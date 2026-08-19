# Feature Wizard Shiny App
#
# Interactive step-by-step wizard for adding features to existing plots
# in the CafriPlots database. Supports adding new census metadata and
# arbitrary plot-level features.

#' Launch Feature Wizard Shiny App
#'
#' Opens an interactive Shiny app that guides users through adding features
#' to existing plots. Supports two modes: adding a new census (with dates
#' and people) or adding arbitrary plot-level features.
#'
#' @details
#' The wizard consists of 6 steps:
#' \enumerate{
#'   \item Login & select existing plots
#'   \item Choose operation mode (New Census or Add Plot Features)
#'   \item Enter or upload plot features
#'   \item Match lookup values (people names) to database
#'   \item Validate and preview data
#'   \item Execute import (with dry-run support)
#' }
#'
#' Phase 1 covers plot-level features and census metadata.
#' Phase 2 (future) will add individual measurements and recruit handling.
#'
#' @param launch_browser Logical: Open in external browser? (default TRUE)
#' @param language Character, initial language ("en" or "fr"), default: "fr"
#'
#' @return Invisibly returns the Shiny app object
#'
#' @examples
#' \dontrun{
#' # Launch the feature wizard
#' launch_feature_wizard()
#'
#' # Launch in RStudio Viewer pane
#' launch_feature_wizard(launch_browser = FALSE)
#' }
#'
#' @export
launch_feature_wizard <- function(launch_browser = TRUE, language = "fr") {

  language <- match.arg(language, c("en", "fr"))

  required_pkgs <- c("shiny", "shinyjs", "DT", "shiny.i18n")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(sprintf(
      "Required packages missing: %s\nInstall with: install.packages(c(%s))",
      paste(missing_pkgs, collapse = ", "),
      paste(sprintf("'%s'", missing_pkgs), collapse = ", ")
    ))
  }

  translator <- init_translator()

  shiny::shinyApp(
    ui = feature_wizard_ui(translator, language),
    server = function(input, output, session) {
      feature_wizard_server(input, output, session, translator)
    },
    options = list(launch.browser = launch_browser)
  )
}


#' UI for Feature Wizard
#' @keywords internal
feature_wizard_ui <- function(translator, language = "fr") {

  shiny::tagList(
    shiny.i18n::usei18n(translator),
    shinyjs::useShinyjs(),

    shiny::fluidPage(
      # Language toggle
      shiny::absolutePanel(
        top = 10, right = 20, fixed = TRUE, draggable = FALSE,
        style = "z-index: 1000;",
        shiny::radioButtons(
          inputId = "selected_language",
          label = NULL,
          choices = c("EN" = "en", "FR" = "fr"),
          selected = language,
          inline = TRUE
        )
      ),

      # CSS
      tags$head(
        tags$style(HTML("
          .step-indicator {
            display: flex;
            justify-content: space-between;
            margin-bottom: 30px;
            padding: 20px 0;
          }
          .step {
            flex: 1;
            text-align: center;
            padding: 15px 10px;
            background: #f8f9fa;
            border-radius: 8px;
            margin: 0 5px;
            border: 2px solid #dee2e6;
            transition: all 0.3s;
            font-weight: 500;
          }
          .step.active {
            background: #007bff;
            color: white;
            border-color: #007bff;
            box-shadow: 0 4px 6px rgba(0,123,255,0.3);
          }
          .step.completed {
            background: #28a745;
            color: white;
            border-color: #28a745;
          }
          .mode-card {
            position: relative;
            border: 2px solid #dee2e6;
            border-radius: 8px;
            padding: 20px;
            margin: 15px 0;
            transition: all 0.3s;
            cursor: pointer;
          }
          /* room for the check mark, kept on every card so selecting one
             never reflows its title */
          .mode-card h4 {
            padding-right: 46px;
          }
          .mode-card:hover {
            border-color: #007bff;
            box-shadow: 0 4px 12px rgba(0,123,255,0.2);
            transform: translateY(-2px);
          }
          /* The chosen card has to be legible without scrolling anywhere: a
             thick border, a tint, a check mark, and every other card faded */
          .mode-card.selected {
            border-color: #007bff;
            border-width: 3px;
            background: #e7f3ff;
            box-shadow: 0 6px 18px rgba(0,123,255,0.28);
          }
          .mode-card.selected::after {
            content: '\\2713';
            position: absolute;
            top: 12px;
            right: 14px;
            width: 30px;
            height: 30px;
            line-height: 30px;
            text-align: center;
            border-radius: 50%;
            background: #007bff;
            color: white;
            font-size: 17px;
            font-weight: bold;
          }
          .mode-card.dimmed {
            opacity: 0.45;
            filter: grayscale(45%);
          }
          .mode-card.dimmed:hover {
            opacity: 1;
            filter: none;
          }
          /* Draws the eye to Next the moment the step is satisfied */
          @keyframes fw-pulse {
            0%   { box-shadow: 0 0 0 0 rgba(0,123,255,0.55); }
            70%  { box-shadow: 0 0 0 14px rgba(0,123,255,0); }
            100% { box-shadow: 0 0 0 0 rgba(0,123,255,0); }
          }
          .fw-attention { animation: fw-pulse 1.3s ease-out 3; }
          .alert { border-radius: 8px; border-left: 4px solid; }
          .btn { border-radius: 6px; font-weight: 500; padding: 10px 24px; }
          .nav-buttons {
            margin-top: 30px;
            padding-top: 20px;
            border-top: 2px solid #dee2e6;
          }
        "))
      ),

      # Login panel
      shiny::conditionalPanel(
        condition = "!output.authenticated",
        mod_database_login_ui("login")
      ),

      # Main app content
      shiny::conditionalPanel(
        condition = "output.authenticated",

        # Header
        div(
          style = "background: linear-gradient(135deg, #28a745 0%, #20c997 100%); padding: 20px; margin-bottom: 30px; color: white; border-radius: 0 0 12px 12px;",
          fluidRow(
            column(12,
              h2(
                icon("plus-circle", style = "margin-right: 10px;"),
                "CafriPlots Feature Wizard",
                style = "margin: 0; font-weight: bold;"
              ),
              p("Add features and census data to existing plots",
                style = "margin: 5px 0 0 0; opacity: 0.9;")
            )
          )
        ),

        # Main content container
        div(
          class = "container-fluid",
          style = "max-width: 1200px; margin: 0 auto;",

          uiOutput("fw_step_indicator"),

          div(
            style = "min-height: 400px;",
            uiOutput("fw_step_content")
          ),

          div(
            class = "nav-buttons",
            fluidRow(
              column(6, uiOutput("fw_back_button")),
              column(6, uiOutput("fw_next_button"), align = "right")
            )
          )
        ),

        hr(),
        div(
          style = "text-align: center; color: #6c757d; padding: 20px 0;",
          p("CafriplotsR Feature Wizard v1.0", style = "margin: 0;")
        )
      )
    )
  )
}


#' Server for Feature Wizard
#' @keywords internal
feature_wizard_server <- function(input, output, session, translator) {

  # Authentication

  login_output <- mod_database_login_server("login")
  pool_main_reactive <- login_output$pool_main
  # Taxon names live in the taxa database, so the revision step needs both
  pool_taxa_reactive <- login_output$pool_taxa
  authenticated_reactive <- login_output$authenticated

  shiny::observe({
    lang <- login_output$language()
    shiny::req(lang)
    shiny::updateRadioButtons(session, "selected_language", selected = lang)
  })

  output$authenticated <- shiny::reactive({ authenticated_reactive() })
  shiny::outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

  # Reactive translator
  i18n <- shiny::reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })

  # Cleanup
  session$onSessionEnded(function() {
    tryCatch({
      cleanup_connections()
    }, error = function(e) {
      cli::cli_alert_warning("Failed to cleanup connections: {e$message}")
    })
    shiny::stopApp()
  })

  # State
  rv <- shiny::reactiveValues(
    step = 1,
    selected_plots = NULL,      # data.frame of selected plots with ids
    operation_mode = NULL,       # "new_census", "add_features", "add_measurements", "add_recruits", or "define_multi_stems"
    feature_data = NULL,         # data.frame of features to add
    feature_config = NULL,       # list with feature types, column mappings
    matched_data = NULL,         # data after lookup matching
    lookup_complete = FALSE,
    validation_result = NULL,
    import_result = NULL,
    modules_initialized = FALSE
  )

  # Step labels
  step_labels <- shiny::reactive({
    c(
      i18n()$t("Select Plots"),
      i18n()$t("Choose Mode"),
      i18n()$t("Enter Features"),
      i18n()$t("Match Lookups"),
      i18n()$t("Validate"),
      i18n()$t("Import")
    )
  })

  # Step indicator
  output$fw_step_indicator <- shiny::renderUI({
    labels <- step_labels()
    # Determine which steps to show (skip step 4 for modes that don't need it)
    show_steps <- if (isTRUE(rv$operation_mode %in% skip_step4_modes)) {
      c(1, 2, 3, 5, 6)
    } else {
      1:6
    }
    div(
      class = "step-indicator",
      lapply(show_steps, function(i) {
        cls <- if (i < rv$step) "step completed"
               else if (i == rv$step) "step active"
               else "step"
        icon_el <- if (i < rv$step) icon("check-circle")
                   else if (i == rv$step) icon("arrow-circle-right")
                   else icon("circle")
        div(class = cls, tags$div(icon_el, br(), labels[i]))
      })
    )
  })

  # Step content
  output$fw_step_content <- shiny::renderUI({
    mode <- rv$operation_mode
    switch(
      as.character(rv$step),
      "1" = mod_feat_step1_select_plots_ui("fw_step1", i18n()),
      "2" = mod_feat_step2_choose_mode_ui("fw_step2", i18n()),
      "3" = {
        if (identical(mode, "import_census")) {
          shiny::tagList(
            mod_feat_step3_census_import_ui("fw_step3_census", i18n()),
            shiny::hr(),
            mod_feat_step3b_taxon_revision_ui("fw_step3b_taxon", i18n())
          )
        } else if (identical(mode, "add_measurements")) {
          mod_feat_step3_measurements_ui("fw_step3_meas", i18n())
        } else if (identical(mode, "define_multi_stems")) {
          mod_feat_step3_multi_stems_ui("fw_step3_ms", i18n())
        } else if (identical(mode, "compute_stem_status")) {
          mod_feat_step3_stem_status_ui("fw_step3_ss", i18n())
        } else if (identical(mode, "standardize_observations")) {
          mod_feat_step3_standardize_obs_ui("fw_step3_so", i18n())
        } else if (identical(mode, "add_recruits")) {
          ns_fw <- session$ns
          shiny::tagList(
            shiny::h3(
              shiny::icon("user-plus"),
              i18n()$t("Add Recruits"),
              style = "color: #495057; margin-bottom: 20px;"
            ),
            shiny::div(
              class = "alert alert-warning",
              style = "font-size: 15px;",
              shiny::icon("external-link-alt"), " ",
              i18n()$t("Adding recruits (new individuals with flat columns only, no individual features) is handled by the Import Wizard, which provides full column mapping, taxonomy matching, and validation."),
              shiny::br(), shiny::br(),
              shiny::strong(i18n()$t("Make sure recruits are added before importing their measurements.")),
              shiny::br(), shiny::br(),
              i18n()$t("Run the following command in the R console to open the Import Wizard:"),
              shiny::br(), shiny::br(),
              shiny::tags$code(
                style = "font-size: 16px; background: #f8f9fa; padding: 8px 16px; border-radius: 4px; display: inline-block;",
                "launch_import_wizard()"
              )
            )
          )
        } else {
          mod_feat_step3_plot_features_ui("fw_step3", i18n())
        }
      },
      "4" = mod_feat_step4_lookup_ui("fw_step4", i18n()),
      "5" = mod_feat_step5_validation_ui("fw_step5", i18n()),
      "6" = mod_feat_step6_import_ui("fw_step6", i18n())
    )
  })

  # Navigation
  can_proceed <- shiny::reactive({
    switch(
      as.character(rv$step),
      "1" = !is.null(rv$selected_plots) && nrow(rv$selected_plots) > 0,
      "2" = !is.null(rv$operation_mode),
      "3" = !is.null(rv$feature_data) && nrow(rv$feature_data) > 0,
      "4" = isTRUE(rv$lookup_complete),
      "5" = !is.null(rv$validation_result) && isTRUE(rv$validation_result$valid),
      "6" = FALSE  # Final step
    )
  })

  output$fw_back_button <- shiny::renderUI({
    if (rv$step > 1) {
      shiny::actionButton(
        "fw_btn_back",
        label = shiny::tagList(icon("arrow-left"), " ", i18n()$t("Back")),
        class = "btn btn-secondary btn-lg"
      )
    }
  })

  output$fw_next_button <- shiny::renderUI({
    if (rv$step >= 6) return(NULL)
    # Hide Next on step 3 for add_recruits (redirects to Import Wizard)
    if (rv$step == 3 && identical(rv$operation_mode, "add_recruits")) return(NULL)
    ready <- can_proceed()
    shiny::actionButton(
      "fw_btn_next",
      label = shiny::tagList(i18n()$t("Next"), " ", icon("arrow-right")),
      class = paste("btn btn-primary btn-lg", if (ready) "fw-attention"),
      disabled = !ready
    )
  })

  # Modes that skip step 4 (no lookup matching needed)
  skip_step4_modes <- c("add_measurements", "define_multi_stems",
                        "compute_stem_status", "standardize_observations",
                        "import_census")

  shiny::observeEvent(input$fw_btn_back, {
    new_step <- rv$step - 1
    # Skip step 4 when going back for modes that don't need it
    if (new_step == 4 && isTRUE(rv$operation_mode %in% skip_step4_modes)) {
      new_step <- 3
    }
    rv$step <- max(1, new_step)
  })

  shiny::observeEvent(input$fw_btn_next, {
    if (can_proceed()) {
      new_step <- rv$step + 1
      # Skip step 4 for modes that don't need lookup matching
      if (new_step == 4 && isTRUE(rv$operation_mode %in% skip_step4_modes)) {
        new_step <- 5
      }
      rv$step <- new_step
    } else {
      shiny::showNotification(
        i18n()$t("Please complete this step before proceeding."),
        type = "warning"
      )
    }
  })

  # Initialize modules after authentication
  shiny::observe({
    shiny::req(authenticated_reactive() == TRUE)
    shiny::req(pool_main_reactive())
    shiny::req(!rv$modules_initialized)

    cli::cli_alert_info("Initializing feature wizard modules...")

    # Step 1: Select plots
    step1_result <- mod_feat_step1_select_plots_server(
      "fw_step1",
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step1_result(), {
      shiny::req(step1_result())
      rv$selected_plots <- step1_result()
      # Reset downstream
      rv$operation_mode <- NULL
      rv$feature_data <- NULL
      rv$feature_config <- NULL
      rv$matched_data <- NULL
      rv$lookup_complete <- FALSE
      rv$validation_result <- NULL
      rv$import_result <- NULL
    })

    # Step 2: Choose mode
    step2_result <- mod_feat_step2_choose_mode_server("fw_step2", i18n = i18n)

    shiny::observeEvent(step2_result(), {
      shiny::req(step2_result())
      rv$operation_mode <- step2_result()
      # Reset downstream
      rv$feature_data <- NULL
      rv$feature_config <- NULL
      rv$matched_data <- NULL
      rv$lookup_complete <- FALSE
      rv$validation_result <- NULL
      rv$import_result <- NULL
    })

    # Step 3: Enter features (plot-level modes)
    step3_result <- mod_feat_step3_plot_features_server(
      "fw_step3",
      selected_plots = shiny::reactive(rv$selected_plots),
      operation_mode = shiny::reactive(rv$operation_mode),
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step3_result(), {
      shiny::req(step3_result())
      rv$feature_data <- step3_result()$data
      rv$feature_config <- step3_result()$config
      # Reset downstream
      rv$matched_data <- NULL
      rv$lookup_complete <- FALSE
      rv$validation_result <- NULL
      rv$import_result <- NULL
    })

    # Step 3: Individual measurements mode
    step3_meas_result <- mod_feat_step3_measurements_server(
      "fw_step3_meas",
      selected_plots = shiny::reactive(rv$selected_plots),
      operation_mode = shiny::reactive(rv$operation_mode),
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step3_meas_result(), {
      shiny::req(step3_meas_result())
      rv$feature_data <- step3_meas_result()$data
      rv$feature_config <- step3_meas_result()$config
      # Reset downstream
      rv$matched_data <- NULL
      rv$lookup_complete <- FALSE
      rv$validation_result <- NULL
      rv$import_result <- NULL
    })

    # Step 3: Full census import mode
    step3_census_result <- mod_feat_step3_census_import_server(
      "fw_step3_census",
      selected_plots = shiny::reactive(rv$selected_plots),
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step3_census_result(), {
      shiny::req(step3_census_result())
      rv$feature_data <- step3_census_result()$data
      rv$feature_config <- step3_census_result()$config
      # Reset downstream
      rv$matched_data <- NULL
      rv$lookup_complete <- FALSE
      rv$validation_result <- NULL
      rv$import_result <- NULL
    })

    # Step 3b: identifications the census table revises. Rendered under the
    # census step rather than as a seventh step, but with its own decision
    # controls — a revision overwrites a determination, so it is not something
    # to be carried along by the measurement flow unnoticed.
    step3b_revisions <- mod_feat_step3b_taxon_revision_server(
      "fw_step3b_taxon",
      split_result = shiny::reactive({
        cfg <- step3_census_result()
        if (is.null(cfg)) NULL else cfg$config$split
      }),
      con      = pool_main_reactive,
      con_taxa = pool_taxa_reactive,
      i18n     = i18n
    )

    shiny::observe({
      shiny::req(identical(rv$operation_mode, "import_census"))
      shiny::req(!is.null(rv$feature_config))
      rv$feature_config$taxon_revisions <- step3b_revisions()
    })

    # Step 3: Multi-stems mode
    step3_ms_result <- mod_feat_step3_multi_stems_server(
      "fw_step3_ms",
      selected_plots = shiny::reactive(rv$selected_plots),
      operation_mode = shiny::reactive(rv$operation_mode),
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step3_ms_result(), {
      shiny::req(step3_ms_result())
      rv$feature_data <- step3_ms_result()$data
      rv$feature_config <- step3_ms_result()$config
      # Reset downstream
      rv$matched_data <- NULL
      rv$lookup_complete <- FALSE
      rv$validation_result <- NULL
      rv$import_result <- NULL
    })

    # Step 3: Compute stem status mode
    step3_ss_result <- mod_feat_step3_stem_status_server(
      "fw_step3_ss",
      selected_plots = shiny::reactive(rv$selected_plots),
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step3_ss_result(), {
      shiny::req(step3_ss_result())
      rv$feature_data <- step3_ss_result()$data
      rv$feature_config <- step3_ss_result()$config
      # Reset downstream
      rv$matched_data <- NULL
      rv$lookup_complete <- FALSE
      rv$validation_result <- NULL
      rv$import_result <- NULL
    })

    # Step 3: Standardize observations mode
    step3_so_result <- mod_feat_step3_standardize_obs_server(
      "fw_step3_so",
      selected_plots = shiny::reactive(rv$selected_plots),
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step3_so_result(), {
      shiny::req(step3_so_result())
      rv$feature_data <- step3_so_result()$data
      rv$feature_config <- step3_so_result()$config
      rv$matched_data <- NULL
      rv$lookup_complete <- FALSE
      rv$validation_result <- NULL
      rv$import_result <- NULL
    })

    # Step 4: Lookup matching
    step4_result <- mod_feat_step4_lookup_server(
      "fw_step4",
      feature_data = shiny::reactive(rv$feature_data),
      feature_config = shiny::reactive(rv$feature_config),
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step4_result$complete(), {
      shiny::req(step4_result$complete() == TRUE)
      rv$matched_data <- step4_result$data()
      rv$lookup_complete <- TRUE
    })

    # For modes that skip step 4, pass feature data through as matched data
    shiny::observe({
      shiny::req(rv$operation_mode %in% c("add_measurements", "define_multi_stems", "compute_stem_status", "standardize_observations", "import_census"))
      shiny::req(!is.null(rv$feature_data))
      rv$matched_data <- rv$feature_data
      rv$lookup_complete <- TRUE
    })

    # Step 5: Validation
    step5_result <- mod_feat_step5_validation_server(
      "fw_step5",
      matched_data = shiny::reactive(rv$matched_data),
      feature_config = shiny::reactive(rv$feature_config),
      selected_plots = shiny::reactive(rv$selected_plots),
      operation_mode = shiny::reactive(rv$operation_mode),
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step5_result(), {
      shiny::req(step5_result())
      rv$validation_result <- step5_result()
      # Update matched_data with validated data (includes issue column from validation checks)
      if (!is.null(step5_result()$data)) {
        rv$matched_data <- step5_result()$data
      }
    })

    # Step 6: Import
    step6_result <- mod_feat_step6_import_server(
      "fw_step6",
      matched_data = shiny::reactive(rv$matched_data),
      feature_config = shiny::reactive(rv$feature_config),
      selected_plots = shiny::reactive(rv$selected_plots),
      operation_mode = shiny::reactive(rv$operation_mode),
      validation_result = shiny::reactive(rv$validation_result),
      con = pool_main_reactive,
      i18n = i18n
    )

    shiny::observeEvent(step6_result(), {
      shiny::req(step6_result())
      rv$import_result <- step6_result()
    })

    rv$modules_initialized <- TRUE
    cli::cli_alert_success("Feature wizard modules initialized!")
  })
}
