# Taxa Traits Import Shiny App
#
# Interactive wizard for importing taxa-level trait measurements
# into the database via add_sp_traits_measures().

#' Launch Taxa Traits Import App
#'
#' Opens an interactive Shiny app that guides users through importing
#' taxa-level trait measurements: upload data, map columns to traits
#' from the trait list, preview, and execute the import.
#'
#' @details
#' The wizard consists of 5 steps:
#' \enumerate{
#'   \item Upload data (xlsx or csv with idtax column)
#'   \item Map trait columns (select which columns contain trait observations)
#'   \item Map metadata columns (taxon ID, flat metadata, and trait features)
#'   \item Validate (check types, ranges, NAs, duplicates; auto-fix type mismatches)
#'   \item Preview & import (dry run or live)
#' }
#'
#' Prerequisites:
#' \itemize{
#'   \item Data must contain an \code{idtax} column with valid taxon IDs
#'   \item Use the taxonomic matching app first to standardize names
#'   \item Trait columns should match existing traits in \code{traitlist}
#' }
#'
#' @param launch_browser Logical: Open in external browser? (default TRUE)
#' @param language Character, initial language ("en" or "fr"), default: "fr"
#'
#' @return Invisibly returns the Shiny app object
#'
#' @examples
#' \dontrun{
#' launch_taxa_traits_import()
#' }
#'
#' @export
launch_taxa_traits_import <- function(launch_browser = TRUE, language = "fr") {

  language <- match.arg(language, c("en", "fr"))

  # Check required packages
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
    ui = taxa_traits_import_ui(translator, language),
    server = function(input, output, session) {
      taxa_traits_import_server(input, output, session, translator)
    },
    options = list(launch.browser = launch_browser)
  )
}


#' @keywords internal
taxa_traits_import_ui <- function(translator, language = "fr") {

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

      # Custom CSS
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
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
        shiny::div(
          style = "background: linear-gradient(135deg, #20c997 0%, #007bff 100%); padding: 20px; margin-bottom: 30px; color: white; border-radius: 0 0 12px 12px;",
          shiny::fluidRow(
            shiny::column(12,
              shiny::h2(
                shiny::icon("leaf", style = "margin-right: 10px;"),
                "CafriPlots - Taxa Traits Import",
                style = "margin: 0; font-weight: bold;"
              ),
              shiny::p("Import taxa-level trait measurements into the database",
                       style = "margin: 5px 0 0 0; opacity: 0.9;")
            )
          )
        ),

        # Main container
        shiny::div(
          class = "container-fluid",
          style = "max-width: 1200px; margin: 0 auto;",

          # Step indicator
          shiny::uiOutput("step_indicator"),

          # Step content
          shiny::div(
            style = "min-height: 400px;",
            shiny::uiOutput("step_content")
          ),

          # Navigation buttons
          shiny::div(
            class = "nav-buttons",
            shiny::fluidRow(
              shiny::column(6, shiny::uiOutput("back_button")),
              shiny::column(6, shiny::uiOutput("next_button"), align = "right")
            )
          )
        ),

        # Footer
        shiny::hr(),
        shiny::div(
          style = "text-align: center; color: #6c757d; padding: 20px 0;",
          shiny::p("CafriplotsR - Taxa Traits Import v1.0", style = "margin: 0;")
        )
      )
    )
  )
}


#' @keywords internal
taxa_traits_import_server <- function(input, output, session, translator) {

  # Authentication
  login_output <- mod_database_login_server("login")
  pool_main_reactive <- login_output$pool_main
  authenticated_reactive <- login_output$authenticated

  # Sync language from login module to app language selector
  shiny::observe({
    lang <- login_output$language()
    shiny::req(lang)
    shiny::updateSelectInput(session, "selected_language", selected = lang)
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

  # Cleanup on session end
  session$onSessionEnded(function() {
    tryCatch({
      cleanup_connections()
    }, error = function(e) {
      cli::cli_alert_warning("Failed to cleanup connections: {e$message}")
    })
    shiny::stopApp()
  })

  # State management
  rv <- shiny::reactiveValues(
    step = 1,
    data = NULL,
    trait_mapping_result = NULL,
    metadata_mapping_result = NULL,
    validation_result = NULL,
    import_result = NULL,
    modules_initialized = FALSE
  )

  # Step labels
  step_labels <- shiny::reactive({
    c(
      i18n()$t("Upload Data"),
      i18n()$t("Map Trait Columns"),
      i18n()$t("Map Metadata Columns"),
      i18n()$t("Validate"),
      i18n()$t("Preview & Import")
    )
  })

  # Step indicator
  output$step_indicator <- shiny::renderUI({
    labels <- step_labels()
    shiny::div(
      class = "step-indicator",
      lapply(seq_along(labels), function(i) {
        cls <- if (i < rv$step) "step completed"
        else if (i == rv$step) "step active"
        else "step"

        icon_name <- if (i < rv$step) "check-circle"
        else if (i == rv$step) "arrow-circle-right"
        else "circle"

        shiny::div(
          class = cls,
          shiny::tags$div(shiny::icon(icon_name), shiny::br(), labels[i])
        )
      })
    )
  })

  # Step content
  output$step_content <- shiny::renderUI({
    switch(
      as.character(rv$step),
      "1" = taxa_traits_upload_ui(session$ns, i18n()),
      "2" = mod_trait_column_mapping_ui("trait_mapping"),
      "3" = mod_trait_metadata_mapping_ui("meta_mapping"),
      "4" = mod_trait_validation_ui("validation", i18n()),
      "5" = mod_trait_preview_import_ui("preview")
    )
  })

  # Navigation
  can_proceed <- shiny::reactive({
    switch(
      as.character(rv$step),
      "1" = !is.null(rv$data),
      "2" = !is.null(rv$trait_mapping_result) && rv$trait_mapping_result$valid,
      "3" = !is.null(rv$metadata_mapping_result) && rv$metadata_mapping_result$valid,
      "4" = !is.null(rv$validation_result) && isTRUE(rv$validation_result$valid),
      "5" = FALSE  # Final step
    )
  })

  output$back_button <- shiny::renderUI({
    if (rv$step > 1) {
      shiny::actionButton(
        "btn_back",
        label = shiny::tagList(shiny::icon("arrow-left"), " ", i18n()$t("Back")),
        class = "btn btn-secondary btn-lg"
      )
    }
  })

  output$next_button <- shiny::renderUI({
    if (rv$step >= 5) return(NULL)

    shiny::actionButton(
      "btn_next",
      label = shiny::tagList(i18n()$t("Next"), " ", shiny::icon("arrow-right")),
      class = "btn btn-primary btn-lg",
      disabled = !can_proceed()
    )
  })

  shiny::observeEvent(input$btn_back, {
    rv$step <- max(1, rv$step - 1)
  })

  shiny::observeEvent(input$btn_next, {
    if (can_proceed()) {
      rv$step <- rv$step + 1
    }
  })

  # -- Step 1: Upload with taxonomy warning --
  # File upload handler
  shiny::observeEvent(input$file_upload, {
    shiny::req(input$file_upload)

    tryCatch({
      file_path <- input$file_upload$datapath
      file_name <- input$file_upload$name
      ext <- tolower(tools::file_ext(file_name))

      df <- if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(file_path)
      } else if (ext == "csv") {
        utils::read.csv(file_path, stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file format. Use .xlsx or .csv")
      }

      df <- as.data.frame(df)

      # Reset downstream state
      rv$trait_mapping_result <- NULL
      rv$metadata_mapping_result <- NULL
      rv$validation_result <- NULL
      rv$import_result <- NULL
      rv$data <- df

      shiny::showNotification(
        sprintf("%d rows, %d columns loaded", nrow(df), ncol(df)),
        type = "message"
      )
    }, error = function(e) {
      shiny::showNotification(
        paste("Error reading file:", e$message),
        type = "error"
      )
    })
  })

  # Initialize modules after authentication
  shiny::observe({
    shiny::req(authenticated_reactive() == TRUE)
    shiny::req(pool_main_reactive())
    shiny::req(!rv$modules_initialized)

    # Step 2: Trait column mapping module
    trait_mapping_result <- mod_trait_column_mapping_server(
      "trait_mapping",
      data = shiny::reactive(rv$data),
      pool = pool_main_reactive,
      i18n = i18n
    )

    shiny::observe({
      res <- trait_mapping_result()
      shiny::req(res)
      # Only snapshot when valid — prevents overwriting with degraded result
      # when step 2 UI is destroyed on navigation
      if (isTRUE(res$valid)) {
        rv$trait_mapping_result <- res
      }
    })

    # Step 3: Metadata mapping module
    metadata_mapping_result <- mod_trait_metadata_mapping_server(
      "meta_mapping",
      data = shiny::reactive(rv$data),
      trait_mapping = shiny::reactive(rv$trait_mapping_result),
      pool = pool_main_reactive,
      i18n = i18n
    )

    shiny::observe({
      res <- metadata_mapping_result()
      shiny::req(res)
      # Only snapshot when valid — prevents overwriting with degraded result
      # when step 3 UI is destroyed on navigation
      if (isTRUE(res$valid)) {
        rv$metadata_mapping_result <- res
      }
    })

    # Combine both mapping results into a single reactive
    combined_mapping <- shiny::reactive({
      tm <- rv$trait_mapping_result
      mm <- rv$metadata_mapping_result
      shiny::req(tm, mm)
      list(
        valid = tm$valid && mm$valid,
        idtax_col = mm$idtax_col,
        trait_cols = tm$trait_cols,
        metadata_cols = mm$metadata_cols,
        feature_cols = mm$feature_cols,
        available_traits = tm$available_traits
      )
    })

    # Step 4: Validation module
    validation_result <- mod_trait_validation_server(
      "validation",
      data = shiny::reactive(rv$data),
      mapping = combined_mapping,
      pool = pool_main_reactive,
      i18n = i18n
    )

    shiny::observe({
      res <- validation_result()
      shiny::req(res)
      rv$validation_result <- res
    })

    # Step 5: Preview & import module
    # Pass cleaned data from validation when available
    import_data <- shiny::reactive({
      vr <- rv$validation_result
      if (!is.null(vr) && !is.null(vr$cleaned_data)) vr$cleaned_data else rv$data
    })

    import_result <- mod_trait_preview_import_server(
      "preview",
      data = import_data,
      mapping = combined_mapping,
      pool = pool_main_reactive,
      i18n = i18n
    )

    shiny::observe({
      shiny::req(import_result())
      rv$import_result <- import_result()
    })

    rv$modules_initialized <- TRUE
  })

  # Upload preview (rendered after data is loaded)
  output$upload_preview <- shiny::renderUI({
    shiny::req(rv$data)
    df <- rv$data

    shiny::tagList(
      shiny::hr(),
      shiny::h4(
        shiny::icon("table"),
        sprintf(" %d rows, %d columns", nrow(df), ncol(df))
      ),
      DT::DTOutput("upload_dt_preview")
    )
  })

  output$upload_dt_preview <- DT::renderDT({
    shiny::req(rv$data)
    DT::datatable(
      utils::head(rv$data, 50),
      options = list(pageLength = 5, scrollX = TRUE, dom = "ftip"),
      rownames = FALSE
    )
  })
}


# =============================================================================
# Step 1: Upload UI (inline, not a module - simple enough)
# =============================================================================

#' @keywords internal
taxa_traits_upload_ui <- function(ns_fn, i18n) {
  shiny::tagList(
    shiny::h3(
      shiny::icon("cloud-upload-alt"),
      i18n$t("Step 1: Upload Trait Data"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    # Taxonomy warning
    shiny::div(
      style = "padding: 15px; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px; margin-bottom: 20px;",
      shiny::fluidRow(
        shiny::column(1, shiny::icon("exclamation-triangle", style = "font-size: 28px; color: #856404;")),
        shiny::column(11,
          shiny::tags$strong(
            i18n$t("Important: Standardize taxonomy first!"),
            style = "color: #856404;"
          ),
          shiny::p(
            i18n$t("Your data must contain an 'idtax_n' column with valid taxon IDs from the database. Use the Taxonomic Matching app (launch_taxonomic_match()) to standardize species names and obtain idtax_n values before importing traits."),
            style = "color: #856404; margin-bottom: 0;"
          )
        )
      )
    ),

    shiny::fluidRow(
      # Upload card
      shiny::column(
        6,
        shiny::div(
          class = "card",
          style = "padding: 20px; background: #f8f9fa; border: 2px solid #dee2e6; border-radius: 8px;",

          shiny::h4(
            shiny::icon("file-upload", style = "color: #28a745;"),
            paste0(" ", i18n$t("Upload Your Data"))
          ),

          shiny::p(
            i18n$t("Upload an Excel or CSV file containing trait measurements with an idtax column."),
            style = "color: #6c757d;"
          ),

          shiny::hr(),

          shiny::fileInput(
            "file_upload",
            i18n$t("Choose file"),
            accept = c(".xlsx", ".xls", ".csv"),
            width = "100%"
          )
        )
      ),

      # Expected format card
      shiny::column(
        6,
        shiny::div(
          class = "card",
          style = "padding: 20px; background: #f8f9fa; border: 2px solid #dee2e6; border-radius: 8px;",

          shiny::h4(
            shiny::icon("info-circle", style = "color: #007bff;"),
            paste0(" ", i18n$t("Expected Format"))
          ),

          shiny::p(
            i18n$t("Your file should contain:"),
            style = "color: #6c757d;"
          ),

          shiny::tags$ul(
            style = "color: #6c757d;",
            shiny::tags$li(
              shiny::tags$strong("idtax / idtax_n"),
              " - ", i18n$t("Taxon ID (required)")
            ),
            shiny::tags$li(
              i18n$t("One or more columns with trait values (e.g., wood_density, max_height)")
            ),
            shiny::tags$li(
              i18n$t("Optional metadata: basisofrecord, latitude, longitude, reference, etc.")
            )
          ),

          shiny::hr(),

          shiny::div(
            style = "padding: 8px; background: #e7f3ff; border-radius: 4px;",
            shiny::icon("lightbulb", style = "color: #007bff;"),
            shiny::tags$small(
              i18n$t("Column names will be automatically matched to traits in the next step."),
              style = "color: #0056b3;"
            )
          )
        )
      )
    ),

    # Data preview (shown after upload)
    shiny::uiOutput("upload_preview")
  )
}
