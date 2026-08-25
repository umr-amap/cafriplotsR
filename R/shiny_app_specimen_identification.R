# Shiny App: Specimen Identification Update
#
# Interactive app for updating specimen identifications.
# Two modes:
#   - Manual: search a single specimen and edit its identification
#   - Batch:  upload a file with rows of (specimen, new idtax, ...) and apply
#             via update_ident_specimens()
#
# Main function: launch_specimen_identification_app()

#' Launch Specimen Identification Update App
#'
#' Launches an interactive Shiny app for updating specimen identifications in
#' the `specimens` table. The app wraps \code{\link{update_ident_specimens}}
#' and provides two workflows:
#'
#' \itemize{
#'   \item \strong{Manual} - search a single specimen by id_specimen, or by
#'         collector + number; review current values; pick a new accepted
#'         taxon via an embedded taxonomy search; optionally update
#'         determination metadata (detd/detm/dety/detby/detvalue) and
#'         collector number / suffix; optionally edit the other specimen
#'         fields (coly/colm/cold, add_col, locality, country, ddlat, ddlon,
#'         description) which are pre-filled with the current values;
#'         preview a diff and confirm.
#'   \item \strong{Batch} - upload an Excel/CSV file with one row per
#'         specimen to update. Columns are mapped to specimen fields,
#'         collectors are matched against \code{table_colnam} (skipped if
#'         \code{id_specimen} is mapped), all rows are validated, then
#'         previewed and applied row-by-row.
#' }
#'
#' Batch mode requires that taxonomic names have already been standardized
#' to \code{idtax_n} values - typically by first using
#' \code{\link{launch_taxonomic_match_app}}.
#'
#' @param lang Character. Initial UI language: \code{"en"} or \code{"fr"}.
#'   Default: \code{"fr"}.
#'
#' @return Launches a Shiny app (does not return until the app closes).
#'
#' @examples
#' \dontrun{
#' launch_specimen_identification_app()
#' launch_specimen_identification_app(lang = "en")
#' }
#'
#' @seealso \code{\link{update_ident_specimens}},
#'   \code{\link{update_records}},
#'   \code{\link{launch_taxonomic_match_app}},
#'   \code{\link{launch_specimen_import_wizard}}
#'
#' @export
launch_specimen_identification_app <- function(lang = "fr") {

  lang <- match.arg(lang, c("fr", "en"))

  # Load translations
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file(
      "translations/translation.json",
      package = "CafriplotsR"
    )
  )
  i18n$set_translation_language(lang)

  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(spin = "fading-circle"),

    shiny::tags$style(shiny::HTML("
      .wizard-step {
        display: inline-block;
        padding: 8px 18px;
        margin: 4px;
        border-radius: 25px;
        background: #e9ecef;
        color: #6c757d;
        font-weight: 500;
      }
      .wizard-step.active { background: #007bff; color: white; }
      .wizard-step.completed { background: #28a745; color: white; }
      .section-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 18px;
        border-radius: 8px;
        margin-bottom: 18px;
      }
      .specid-mode-card {
        padding: 24px;
        border: 2px solid #dee2e6;
        border-radius: 10px;
        cursor: pointer;
        text-align: center;
        transition: all 0.2s;
        background: #fff;
      }
      .specid-mode-card:hover {
        border-color: #007bff;
        background: #f8f9ff;
      }
      .specid-mode-card.selected {
        border-color: #007bff;
        background: #e7f1ff;
      }
      .diff-table td.changed {
        background-color: #fff3cd;
        font-weight: 600;
      }
    ")),

    shiny::fluidRow(
      shiny::column(8, shiny::h2(shiny::textOutput("app_title"))),
      shiny::column(
        4,
        style = "text-align: right; padding-top: 12px;",
        mod_language_toggle_ui("lang_toggle")
      )
    ),
    shiny::hr(),

    # Login (hidden after authenticated)
    shiny::conditionalPanel(
      condition = "!output.authenticated",
      mod_database_login_ui("login")
    ),

    # Main content
    shiny::conditionalPanel(
      condition = "output.authenticated",

      shiny::div(
        class = "section-header",
        shiny::h3(shiny::icon("edit"), " ", shiny::textOutput("header_title", inline = TRUE)),
        shiny::p(shiny::textOutput("header_desc"))
      ),

      # Mode selector (always visible at top)
      mod_specid_mode_ui("mode", i18n),

      shiny::hr(),

      # MANUAL pane
      shiny::conditionalPanel(
        condition = "output.mode == 'manual'",
        mod_specid_manual_ui("manual", i18n)
      ),

      # BATCH pane (wizard inside)
      shiny::conditionalPanel(
        condition = "output.mode == 'batch'",

        # Wizard progress
        shiny::div(
          style = "text-align: center; margin: 18px 0;",
          shiny::uiOutput("batch_progress")
        ),

        # Step 1: upload
        shiny::conditionalPanel(
          condition = "output.batch_step == 1",
          mod_specid_batch_upload_ui("b_upload", i18n)
        ),
        # Step 2: mapping
        shiny::conditionalPanel(
          condition = "output.batch_step == 2",
          mod_specid_batch_mapping_ui("b_mapping", i18n)
        ),
        # Step 3: lookup
        shiny::conditionalPanel(
          condition = "output.batch_step == 3",
          mod_specid_batch_lookup_ui("b_lookup", i18n)
        ),
        # Step 4: validation
        shiny::conditionalPanel(
          condition = "output.batch_step == 4",
          mod_specid_batch_validation_ui("b_validation", i18n)
        ),
        # Step 5: preview + update
        shiny::conditionalPanel(
          condition = "output.batch_step == 5",
          mod_specid_batch_update_ui("b_update", i18n)
        ),

        # Wizard nav
        shiny::div(
          style = "margin-top: 24px; padding: 16px; border-top: 1px solid #dee2e6;",
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::actionButton(
                "batch_back",
                shiny::tagList(shiny::icon("arrow-left"), " ", i18n$t("Back")),
                class = "btn-secondary"
              )
            ),
            shiny::column(
              6, style = "text-align: right;",
              shiny::actionButton(
                "batch_next",
                shiny::tagList(i18n$t("Next"), " ", shiny::icon("arrow-right")),
                class = "btn-primary"
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    # Language
    current_lang <- mod_language_toggle_server("lang_toggle", initial = lang)
    i18n_reactive <- shiny::reactive({
      sel <- current_lang()
      i18n$set_translation_language(sel)
      i18n
    })

    # Login
    login_result <- mod_database_login_server("login")
    pool_main <- login_result$pool_main
    pool_taxa <- login_result$pool_taxa
    authenticated <- login_result$authenticated

    shiny::observe({
      l <- login_result$language()
      shiny::req(l)
      shiny::updateRadioButtons(session, "lang_toggle-language", selected = l)
    })

    output$authenticated <- shiny::reactive({ authenticated() })
    shiny::outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

    # Titles
    output$app_title <- shiny::renderText({
      i18n_reactive()$t("Specimen Identification Update")
    })
    output$header_title <- shiny::renderText({
      i18n_reactive()$t("Update Specimen Identifications")
    })
    output$header_desc <- shiny::renderText({
      i18n_reactive()$t("Update specimen taxonomy and determination metadata, either one specimen at a time or by uploading a file.")
    })

    # Mode selection
    selected_mode <- mod_specid_mode_server("mode", i18n = i18n_reactive)
    output$mode <- shiny::reactive({ selected_mode() })
    shiny::outputOptions(output, "mode", suspendWhenHidden = FALSE)

    # ============================================================
    # MANUAL MODULE
    # ============================================================
    shiny::observe({
      shiny::req(authenticated() == TRUE)
      mod_specid_manual_server(
        "manual",
        pool_main = pool_main,
        pool_taxa = pool_taxa,
        i18n = i18n_reactive,
        active = shiny::reactive({ selected_mode() == "manual" })
      )
    })

    # ============================================================
    # BATCH WIZARD
    # ============================================================
    batch_step <- shiny::reactiveVal(1)
    output$batch_step <- shiny::reactive({ batch_step() })
    shiny::outputOptions(output, "batch_step", suspendWhenHidden = FALSE)

    # Reset wizard when entering batch mode
    shiny::observeEvent(selected_mode(), {
      if (selected_mode() == "batch") {
        # don't auto-reset; user may switch back and forth
      }
    })

    # Wizard progress indicator
    output$batch_progress <- shiny::renderUI({
      step <- batch_step()
      steps <- list(
        list(num = 1, label = i18n_reactive()$t("Upload"),     icon = "upload"),
        list(num = 2, label = i18n_reactive()$t("Map Columns"), icon = "columns"),
        list(num = 3, label = i18n_reactive()$t("Match Collectors"), icon = "user"),
        list(num = 4, label = i18n_reactive()$t("Validate"),    icon = "check-double"),
        list(num = 5, label = i18n_reactive()$t("Preview & Update"), icon = "database")
      )
      shiny::div(
        lapply(steps, function(s) {
          cls <- "wizard-step"
          if (s$num <  step) cls <- paste(cls, "completed")
          if (s$num == step) cls <- paste(cls, "active")
          shiny::span(class = cls,
                      shiny::icon(if (s$num < step) "check" else s$icon),
                      " ", paste0(s$num, ". ", s$label))
        })
      )
    })

    # Storage between batch steps
    b_data       <- shiny::reactiveVal(NULL)  # uploaded data frame
    b_mappings   <- shiny::reactiveVal(NULL)  # named list user_col -> field
    b_map_valid  <- shiny::reactiveVal(FALSE)
    b_matched    <- shiny::reactiveVal(NULL)  # data with id_colnam resolved
    b_lookup_ok  <- shiny::reactiveVal(FALSE)
    b_validated  <- shiny::reactiveVal(NULL)  # data with validation flags
    b_valid_ok   <- shiny::reactiveVal(FALSE)

    shiny::observe({
      shiny::req(authenticated() == TRUE)

      # Step 1: upload
      uploaded <- mod_specid_batch_upload_server("b_upload", i18n = i18n_reactive)
      shiny::observe({ b_data(uploaded()) })

      # Step 2: mapping
      map_res <- mod_specid_batch_mapping_server(
        "b_mapping",
        data = uploaded,
        i18n = i18n_reactive
      )
      shiny::observe({
        b_mappings(map_res$mappings())
        b_map_valid(map_res$is_valid())
      })

      # Step 3: lookup (collector matching)
      lookup_res <- mod_specid_batch_lookup_server(
        "b_lookup",
        data = uploaded,
        mappings = map_res$mappings,
        pool_main = pool_main,
        i18n = i18n_reactive
      )
      shiny::observe({
        b_matched(lookup_res$matched_data())
        b_lookup_ok(lookup_res$is_complete())
      })

      # Step 4: validation
      val_res <- mod_specid_batch_validation_server(
        "b_validation",
        matched_data = lookup_res$matched_data,
        mappings    = map_res$mappings,
        pool_main   = pool_main,
        pool_taxa   = pool_taxa,
        i18n        = i18n_reactive
      )
      shiny::observe({
        b_validated(val_res$validated_data())
        b_valid_ok(val_res$is_valid())
      })

      # Step 5: preview & update
      mod_specid_batch_update_server(
        "b_update",
        validated_data = val_res$validated_data,
        mappings       = map_res$mappings,
        pool_main      = pool_main,
        i18n           = i18n_reactive
      )
    })

    # Batch navigation
    shiny::observeEvent(input$batch_back, {
      s <- batch_step()
      if (s > 1) batch_step(s - 1)
    })

    shiny::observeEvent(input$batch_next, {
      s <- batch_step()
      can_proceed <- FALSE
      msg <- NULL

      if (s == 1) {
        if (!is.null(b_data())) can_proceed <- TRUE
        else msg <- i18n_reactive()$t("Please upload a file first.")
      } else if (s == 2) {
        if (isTRUE(b_map_valid())) can_proceed <- TRUE
        else msg <- i18n_reactive()$t("Please map all required columns.")
      } else if (s == 3) {
        if (isTRUE(b_lookup_ok())) can_proceed <- TRUE
        else msg <- i18n_reactive()$t("Please resolve all collector matches first.")
      } else if (s == 4) {
        if (isTRUE(b_valid_ok())) can_proceed <- TRUE
        else msg <- i18n_reactive()$t("Please fix validation issues before continuing.")
      }

      if (can_proceed && s < 5) {
        batch_step(s + 1)
      } else if (!is.null(msg)) {
        shiny::showNotification(msg, type = "warning")
      }
    })

    shiny::observe({
      s <- batch_step()
      shinyjs::toggleState("batch_back", condition = s > 1)
      shinyjs::toggleState("batch_next", condition = s < 5)
      if (s == 5) shinyjs::hide("batch_next") else shinyjs::show("batch_next")
    })

    # Session cleanup
    session$onSessionEnded(function() {
      tryCatch(cleanup_connections(),
               error = function(e) cli::cli_alert_warning(
                 "Failed to cleanup connections: {e$message}"))
      shiny::stopApp()
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
