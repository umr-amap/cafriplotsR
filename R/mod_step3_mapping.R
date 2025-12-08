# Import Wizard - Step 3: Column Mapping
#
# Module for mapping user columns to database schema with visual interface

#' Step 3 Module: Column Mapping - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
mod_step3_mapping_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("exchange-alt"),
      i18n$t("Step 3: Map Your Columns to Database Schema"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Review and adjust the automatic column mapping. The wizard has attempted to match your columns to the database schema."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Import type-specific guidance (e.g., skip plot metadata for individuals)
    shiny::uiOutput(ns("import_guidance")),

    # Mapping summary
    shiny::uiOutput(ns("mapping_summary")),

    shiny::hr(),

    # Column mapping interface
    shiny::h4(i18n$t("Column Mappings"), style = "margin-bottom: 20px;"),

    # Button to create new feature (for individuals import)
    shiny::uiOutput(ns("create_feature_button")),

    shiny::uiOutput(ns("mapping_interface")),

    # Validation messages
    shiny::uiOutput(ns("mapping_validation")),

    # Modal for creating new feature
    shiny::uiOutput(ns("create_feature_modal"))
  )
}


#' Step 3 Module: Column Mapping - Server
#'
#' @param id Module namespace ID
#' @param data Reactive containing uploaded user data
#' @param config Reactive containing import configuration
#' @param i18n Translator object from shiny.i18n
#' @return Reactive list containing mappings and validation status
#' @keywords internal
mod_step3_mapping_server <- function(id, data, config, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Auto-generate initial mappings when data/config available
    auto_mappings <- shiny::reactive({
      shiny::req(data(), config())

      cli::cli_alert_info("Running automatic column mapping...")

      result <- map_user_columns(
        user_data = data(),
        config = config(),
        similarity_threshold = 0.6,
        interactive = FALSE  # No console prompts in Shiny
      )

      cli::cli_alert_success("Auto-mapping complete")
      result
    })

    # Store user modifications (persists across navigation)
    user_modified_mappings <- shiny::reactiveVal(NULL)

    # Initialize user_modified_mappings from auto_mappings (only once)
    shiny::observe({
      if (is.null(user_modified_mappings())) {
        shiny::req(auto_mappings())
        user_modified_mappings(auto_mappings()$mappings)
        cli::cli_alert_info("Initialized user mappings from auto-mapping")
      }
    })

    # Track dropdown changes - create observeEvents once when mappings are initialized
    observers_created <- shiny::reactiveVal(FALSE)

    shiny::observe({
      if (!observers_created() && !is.null(user_modified_mappings())) {
        user_cols <- names(user_modified_mappings())

        for (user_col in user_cols) {
          input_id <- paste0("map_", user_col)

          # Use local to capture user_col properly
          local({
            col <- user_col
            id <- input_id

            shiny::observeEvent(input[[id]], {
              current <- isolate(user_modified_mappings())
              new_value <- input[[id]]

              # Update the mapping (empty string means NA/skip)
              if (new_value == "") {
                current[[col]] <- NA
              } else {
                current[[col]] <- new_value
              }

              user_modified_mappings(current)
              cli::cli_alert_info("User modified mapping for '{col}': {new_value %||% 'NA (skipped)'}")
            }, ignoreInit = TRUE)  # Don't trigger on initial render
          })
        }

        observers_created(TRUE)
        cli::cli_alert_success("Created observers for {length(user_cols)} column mappings")
      }
    })

    # Get all valid schema columns for dropdowns
    # Use reactiveVal so we can update it when new features are created
    schema_columns <- shiny::reactiveVal(NULL)

    # Initialize schema columns when config is available
    shiny::observe({
      shiny::req(config())

      if (is.null(schema_columns())) {
        all_cols <- c(
          config()$direct_columns,
          # For plots: subplot_features, for individuals: feature_columns (traits)
          if (!is.null(config()$subplot_features)) config()$subplot_features else character(0),
          if (!is.null(config()$feature_columns)) config()$feature_columns else character(0)
        )

        schema_columns(sort(unique(all_cols)))
      }
    })

    # Import type-specific guidance
    output$import_guidance <- shiny::renderUI({
      shiny::req(config())

      # Check if this is individuals import
      is_individuals <- "idtax_n" %in% config()$import_config$required_columns

      if (is_individuals) {
        shiny::div(
          class = "alert alert-warning",
          style = "background-color: #fff3cd; border-left: 4px solid #ffc107; margin-bottom: 20px;",
          shiny::icon("info-circle", style = "color: #856404;"),
          shiny::strong(paste0(" ", i18n$t("Important: Skip Plot Metadata Columns")), style = "color: #856404;"),
          shiny::br(),
          shiny::br(),
          shiny::p(
            i18n$t("If your dataset contains plot-level information (country, coordinates, elevation, dates, etc.), "),
            shiny::strong(i18n$t("do NOT map these columns here.")),
            i18n$t(" These should be imported separately using "),
            shiny::tags$code(i18n$t("Plot Metadata")),
            i18n$t(" import."),
            style = "margin: 0; color: #856404;"
          ),
          shiny::br(),
          shiny::p(
            shiny::icon("check", style = "color: #28a745;"),
            i18n$t(" Only map: individual tree data (plot_name, tag, species) and measurements (DBH, height, etc.)"),
            style = "margin: 0; color: #856404;"
          )
        )
      } else {
        # No guidance needed for plots import
        NULL
      }
    })

    # Mapping summary statistics
    output$mapping_summary <- shiny::renderUI({
      shiny::req(auto_mappings())

      am <- auto_mappings()

      n_exact <- sum(am$methods == "exact", na.rm = TRUE)
      n_synonym <- sum(am$methods == "synonym", na.rm = TRUE)
      n_fuzzy <- sum(am$methods == "fuzzy", na.rm = TRUE)
      n_unmapped <- length(am$unmapped)
      n_total <- length(am$mappings)

      shiny::div(
        class = "alert alert-info",
        style = "background-color: #e7f3ff; border-left: 4px solid #007bff;",

        shiny::fluidRow(
          shiny::column(
            3,
            shiny::div(
              style = "text-align: center;",
              shiny::h3(n_exact + n_synonym, style = "color: #28a745; margin: 0;"),
              shiny::p(shiny::icon("check-circle"), " Auto-Mapped", style = "margin: 5px 0 0 0; color: #28a745;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              style = "text-align: center;",
              shiny::h3(n_fuzzy, style = "color: #ffc107; margin: 0;"),
              shiny::p(shiny::icon("question-circle"), " Review Suggested", style = "margin: 5px 0 0 0; color: #ffc107;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              style = "text-align: center;",
              shiny::h3(n_unmapped, style = "color: #dc3545; margin: 0;"),
              shiny::p(shiny::icon("times-circle"), " Needs Mapping", style = "margin: 5px 0 0 0; color: #dc3545;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              style = "text-align: center;",
              shiny::h3(n_total, style = "color: #6c757d; margin: 0;"),
              shiny::p(shiny::icon("list"), " Total Columns", style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          )
        )
      )
    })

    # Column mapping interface
    output$mapping_interface <- shiny::renderUI({
      shiny::req(data(), auto_mappings(), user_modified_mappings(), schema_columns(), config())

      am <- auto_mappings()
      user_mods <- user_modified_mappings()
      user_cols <- names(am$mappings)
      schema_choices <- c("(Skip this column)" = "", schema_columns())

      # Get column descriptions from config
      column_descriptions <- config()$import_config$column_descriptions
      if (is.null(column_descriptions)) {
        column_descriptions <- list()
      }

      # Create a row for each user column
      mapping_rows <- lapply(user_cols, function(user_col) {

        # Get auto-mapping info (for status display only)
        auto_mapped_to <- am$mappings[[user_col]]
        method <- am$methods[[user_col]]
        confidence <- am$confidence[[user_col]]

        # Get actual current mapping (includes user modifications)
        current_mapped_to <- user_mods[[user_col]]

        # Determine status icon and color
        status_info <- if (method == "exact") {
          list(icon = "check-circle", color = "#28a745", label = "Exact Match")
        } else if (method == "synonym") {
          list(icon = "check-circle", color = "#28a745", label = "Synonym")
        } else if (method == "fuzzy") {
          list(icon = "question-circle", color = "#ffc107", label = sprintf("Fuzzy (%.0f%%)", confidence * 100))
        } else {
          list(icon = "times-circle", color = "#dc3545", label = "Not Mapped")
        }

        # Get sample values from data
        sample_vals <- head(data()[[user_col]], 3)
        sample_text <- paste(sample_vals, collapse = ", ")
        if (nchar(sample_text) > 50) {
          sample_text <- paste0(substr(sample_text, 1, 47), "...")
        }

        # Build the row
        shiny::div(
          class = "mapping-row",
          style = sprintf("border-left-color: %s;", status_info$color),

          shiny::fluidRow(
            # User column name + samples
            shiny::column(
              4,
              shiny::strong(user_col, style = "font-size: 14px;"),
              shiny::br(),
              shiny::tags$small(
                shiny::icon("eye"),
                " Samples: ",
                shiny::tags$code(sample_text, style = "font-size: 11px;"),
                style = "color: #6c757d;"
              )
            ),

            # Arrow
            shiny::column(
              1,
              shiny::div(
                shiny::icon("arrow-right", style = "font-size: 24px; color: #007bff;"),
                style = "text-align: center; padding-top: 10px;"
              )
            ),

            # Database column dropdown + description (reactive)
            shiny::column(
              5,
              shiny::selectInput(
                session$ns(paste0("map_", user_col)),
                label = NULL,
                choices = schema_choices,
                selected = if (!is.na(current_mapped_to)) current_mapped_to else "",
                width = "100%"
              ),
              shiny::uiOutput(session$ns(paste0("desc_", user_col)))
            ),

            # Status indicator
            shiny::column(
              2,
              shiny::div(
                shiny::icon(status_info$icon, style = sprintf("color: %s;", status_info$color)),
                shiny::br(),
                shiny::tags$small(status_info$label, style = sprintf("color: %s;", status_info$color)),
                style = "text-align: center; padding-top: 10px;"
              )
            )
          )
        )
      })

      shiny::tagList(mapping_rows)
    })

    # Reactive descriptions for each column
    shiny::observe({
      shiny::req(data(), auto_mappings(), config())

      user_cols <- names(auto_mappings()$mappings)
      column_descriptions <- config()$import_config$column_descriptions
      if (is.null(column_descriptions)) {
        column_descriptions <- list()
      }

      # Create a reactive description output for each user column
      lapply(user_cols, function(user_col) {
        output_id <- paste0("desc_", user_col)

        output[[output_id]] <- shiny::renderUI({
          # Get currently selected mapping
          mapped_to <- input[[paste0("map_", user_col)]]

          # Return NULL if no mapping or skipped
          if (is.null(mapped_to) || mapped_to == "") {
            return(NULL)
          }

          # Get column info
          col_info <- column_descriptions[[mapped_to]]
          if (is.null(col_info)) {
            return(NULL)
          }

          # Build description UI
          desc_parts <- list(
            shiny::tags$small(
              shiny::icon("info-circle", style = "color: #007bff;"),
              " ",
              col_info$description,
              style = "color: #6c757d;"
            )
          )

          # Add factor levels if available (for traits)
          if (!is.null(col_info$factorlevels) && col_info$factorlevels != "") {
            desc_parts <- c(desc_parts, list(
              shiny::br(),
              shiny::tags$small(
                shiny::icon("list"),
                " Expected values: ",
                shiny::tags$code(col_info$factorlevels, style = "font-size: 10px;"),
                style = "color: #856404;"
              )
            ))
          }

          # Add expected unit if available (for traits)
          if (!is.null(col_info$expectedunit) && col_info$expectedunit != "") {
            desc_parts <- c(desc_parts, list(
              shiny::br(),
              shiny::tags$small(
                shiny::icon("ruler"),
                " Unit: ",
                shiny::tags$strong(col_info$expectedunit),
                style = "color: #28a745;"
              )
            ))
          }

          shiny::div(
            style = "margin-top: 8px; padding: 8px; background-color: #f0f8ff; border-radius: 4px; border-left: 3px solid #007bff;",
            desc_parts
          )
        })
      })
    })

    # Collect current mappings from persistent state
    current_mappings <- shiny::reactive({
      shiny::req(user_modified_mappings())

      # Filter out NA values (skipped columns)
      mappings <- user_modified_mappings()
      mappings[!is.na(mappings)]
    })

    # Validation - check if required columns are mapped
    mapping_validation <- shiny::reactive({
      shiny::req(config(), current_mappings())

      required_cols <- config()$required_columns
      mapped_db_cols <- unlist(current_mappings())

      missing_required <- setdiff(required_cols, mapped_db_cols)

      list(
        valid = length(missing_required) == 0,
        missing_required = missing_required,
        n_mapped = length(current_mappings())
      )
    })

    # Validation UI
    output$mapping_validation <- shiny::renderUI({
      shiny::req(mapping_validation(), config())

      val <- mapping_validation()

      # Check if this is individuals import (has idtax_n in required columns)
      is_individuals <- "idtax_n" %in% config()$import_config$required_columns

      # Check if tag is mapped (for individuals)
      mapped_db_cols <- unlist(current_mappings())
      tag_mapped <- "tag" %in% mapped_db_cols

      # Build UI elements
      ui_elements <- list()

      # Main validation message (required columns)
      if (val$valid) {
        ui_elements[[1]] <- shiny::div(
          class = "alert alert-success",
          style = "margin-top: 30px;",
          shiny::icon("check-circle"),
          shiny::strong(" Mapping Complete: "),
          sprintf(
            "All %d required columns are mapped. You have mapped %d columns total.",
            length(config()$required_columns),
            val$n_mapped
          )
        )
      } else {
        ui_elements[[1]] <- shiny::div(
          class = "alert alert-danger",
          style = "margin-top: 30px;",
          shiny::icon("exclamation-circle"),
          shiny::strong(" Missing Required Columns: "),
          sprintf(
            "You must map the following required columns: %s",
            paste(shiny::tags$code(val$missing_required), collapse = ", ")
          )
        )
      }

      # Warning for missing tag (individuals only)
      if (is_individuals && !tag_mapped) {
        ui_elements[[2]] <- shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 15px;",
          shiny::icon("exclamation-triangle"),
          shiny::strong(" Warning - Tag Column Not Mapped: "),
          shiny::br(),
          shiny::br(),
          "The ", shiny::tags$code("tag"), " column is ", shiny::strong("strongly recommended"),
          ", especially for permanent plots.",
          shiny::br(),
          shiny::br(),
          shiny::tags$ul(
            style = "margin-bottom: 5px;",
            shiny::tags$li("Tags allow tracking individuals across multiple censuses"),
            shiny::tags$li("If not provided, tags will be auto-generated as incremental integers"),
            shiny::tags$li("Auto-generated tags cannot be matched to future census data")
          )
        )
      }

      shiny::tagList(ui_elements)
    })

    # Create new feature button (only for individuals import)
    output$create_feature_button <- shiny::renderUI({
      shiny::req(config())

      # Check if this is individuals import
      is_individuals <- "idtax_n" %in% config()$import_config$required_columns

      if (is_individuals) {
        shiny::div(
          style = "margin-bottom: 15px;",
          shiny::actionButton(
            session$ns("show_create_feature"),
            shiny::tagList(shiny::icon("plus"), paste0(" ", i18n$t("Create New Feature/Attribute"))),
            class = "btn-success btn-sm"
          ),
          shiny::tags$small(
            paste0(" ", i18n$t("Click if you have a column that doesn't match any existing feature")),
            style = "color: #6c757d; margin-left: 10px;"
          )
        )
      } else {
        NULL
      }
    })

    # Create feature modal
    output$create_feature_modal <- shiny::renderUI({
      shiny::req(config())

      # Check if this is individuals import
      is_individuals <- "idtax_n" %in% config()$import_config$required_columns

      if (is_individuals && !is.null(input$show_create_feature) && input$show_create_feature > 0) {
        shiny::modalDialog(
          title = shiny::tagList(shiny::icon("plus-circle"), paste0(" ", i18n$t("Create New Feature/Attribute"))),
          size = "l",

          shiny::p(
            i18n$t("Create a new feature/attribute that can be linked to individual stems/trees."),
            style = "color: #6c757d; margin-bottom: 20px;"
          ),

          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                session$ns("new_feature_name"),
                i18n$t("Feature Name *"),
                placeholder = i18n$t("e.g., crown_diameter, bark_thickness")
              ),
              shiny::tags$small(
                shiny::icon("info-circle", style = "color: #007bff;"),
                paste0(" ", i18n$t("Use lowercase, underscores (not spaces), no special characters")),
                style = "color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;"
              ),
              shiny::selectInput(
                session$ns("new_feature_valuetype"),
                i18n$t("Value Type *"),
                choices = setNames(
                  c("numeric", "integer", "categorical", "character", "logical", "ordinal"),
                  c(i18n$t("Numeric (measurements)"),
                    i18n$t("Integer (counts)"),
                    i18n$t("Categorical (categories)"),
                    i18n$t("Character (text)"),
                    i18n$t("Logical (yes/no)"),
                    i18n$t("Ordinal (ordered categories)"))
                ),
                selected = "numeric"
              ),
              shiny::textInput(
                session$ns("new_feature_unit"),
                i18n$t("Expected Unit (optional)"),
                placeholder = i18n$t("e.g., cm, m, kg, %")
              )
            ),
            shiny::column(
              6,
              shiny::textAreaInput(
                session$ns("new_feature_description"),
                i18n$t("Description *"),
                placeholder = i18n$t("Describe what this feature measures or represents"),
                rows = 3
              ),
              shiny::textInput(
                session$ns("new_feature_min"),
                i18n$t("Minimum Allowed Value (optional)"),
                placeholder = i18n$t("e.g., 0")
              ),
              shiny::textInput(
                session$ns("new_feature_max"),
                i18n$t("Maximum Allowed Value (optional)"),
                placeholder = i18n$t("e.g., 100")
              )
            )
          ),

          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'categorical' || input['%s'] == 'ordinal'",
                               session$ns("new_feature_valuetype"),
                               session$ns("new_feature_valuetype")),
            shiny::textInput(
              session$ns("new_feature_levels"),
              i18n$t("Factor Levels (comma-separated)"),
              placeholder = i18n$t("e.g., small, medium, large")
            )
          ),

          footer = shiny::tagList(
            shiny::modalButton(i18n$t("Cancel")),
            shiny::actionButton(
              session$ns("create_feature_confirm"),
              shiny::tagList(shiny::icon("check"), paste0(" ", i18n$t("Create Feature"))),
              class = "btn-primary"
            )
          ),
          easyClose = FALSE
        )
      }
    })

    # Show modal when button clicked
    shiny::observeEvent(input$show_create_feature, {
      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tagList(shiny::icon("plus-circle"), " Create New Feature/Attribute"),
          size = "l",

          shiny::p(
            "Create a new feature/attribute that can be linked to individual stems/trees.",
            style = "color: #6c757d; margin-bottom: 20px;"
          ),

          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                session$ns("new_feature_name"),
                i18n$t("Feature Name *"),
                placeholder = i18n$t("e.g., crown_diameter, bark_thickness")
              ),
              shiny::tags$small(
                shiny::icon("info-circle", style = "color: #007bff;"),
                paste0(" ", i18n$t("Use lowercase, underscores (not spaces), no special characters")),
                style = "color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;"
              ),
              shiny::selectInput(
                session$ns("new_feature_valuetype"),
                i18n$t("Value Type *"),
                choices = setNames(
                  c("numeric", "integer", "categorical", "character", "logical", "ordinal"),
                  c(i18n$t("Numeric (measurements)"),
                    i18n$t("Integer (counts)"),
                    i18n$t("Categorical (categories)"),
                    i18n$t("Character (text)"),
                    i18n$t("Logical (yes/no)"),
                    i18n$t("Ordinal (ordered categories)"))
                ),
                selected = "numeric"
              ),
              shiny::textInput(
                session$ns("new_feature_unit"),
                i18n$t("Expected Unit (optional)"),
                placeholder = i18n$t("e.g., cm, m, kg, %")
              )
            ),
            shiny::column(
              6,
              shiny::textAreaInput(
                session$ns("new_feature_description"),
                i18n$t("Description *"),
                placeholder = i18n$t("Describe what this feature measures or represents"),
                rows = 3
              ),
              shiny::textInput(
                session$ns("new_feature_min"),
                i18n$t("Minimum Allowed Value (optional)"),
                placeholder = i18n$t("e.g., 0")
              ),
              shiny::textInput(
                session$ns("new_feature_max"),
                i18n$t("Maximum Allowed Value (optional)"),
                placeholder = i18n$t("e.g., 100")
              )
            )
          ),

          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'categorical' || input['%s'] == 'ordinal'",
                               session$ns("new_feature_valuetype"),
                               session$ns("new_feature_valuetype")),
            shiny::textInput(
              session$ns("new_feature_levels"),
              i18n$t("Factor Levels (comma-separated)"),
              placeholder = i18n$t("e.g., small, medium, large")
            )
          ),

          footer = shiny::tagList(
            shiny::modalButton(i18n$t("Cancel")),
            shiny::actionButton(
              session$ns("create_feature_confirm"),
              shiny::tagList(shiny::icon("check"), paste0(" ", i18n$t("Create Feature"))),
              class = "btn-primary"
            )
          ),
          easyClose = FALSE
        )
      )
    })

    # Handle feature creation
    shiny::observeEvent(input$create_feature_confirm, {
      shiny::req(input$new_feature_name, input$new_feature_valuetype, input$new_feature_description)

      # Validate inputs
      if (trimws(input$new_feature_name) == "" || trimws(input$new_feature_description) == "") {
        shiny::showNotification(
          i18n$t("Feature name and description are required"),
          type = "error",
          duration = 5
        )
        return()
      }

      # Sanitize feature name: lowercase, spaces to underscores, remove special chars
      original_name <- trimws(input$new_feature_name)
      sanitized_name <- original_name %>%
        tolower() %>%                                    # Convert to lowercase
        gsub(" ", "_", .) %>%                           # Replace spaces with underscores
        gsub("[^a-z0-9_]", "", .)                       # Remove special characters

      # Warn if name was changed
      if (sanitized_name != original_name) {
        shiny::showNotification(
          sprintf(i18n$t("Feature name auto-corrected: '%s' → '%s'"), original_name, sanitized_name),
          type = "warning",
          duration = 5
        )
      }

      # Check if name is empty after sanitization
      if (sanitized_name == "") {
        shiny::showNotification(
          i18n$t("Feature name contains only invalid characters. Please use letters, numbers, and underscores."),
          type = "error",
          duration = 5
        )
        return()
      }

      shiny::withProgress(message = i18n$t("Creating new feature..."), {
        tryCatch({
          # Prepare parameters
          new_min <- if (!is.null(input$new_feature_min) && trimws(input$new_feature_min) != "") {
            as.numeric(input$new_feature_min)
          } else {
            NULL
          }

          new_max <- if (!is.null(input$new_feature_max) && trimws(input$new_feature_max) != "") {
            as.numeric(input$new_feature_max)
          } else {
            NULL
          }

          new_unit <- if (!is.null(input$new_feature_unit) && trimws(input$new_feature_unit) != "") {
            trimws(input$new_feature_unit)
          } else {
            NULL
          }

          new_levels <- if (!is.null(input$new_feature_levels) && trimws(input$new_feature_levels) != "") {
            trimws(input$new_feature_levels)
          } else {
            NULL
          }

          # Call add_trait to create the feature (using sanitized name)
          add_trait(
            new_trait = sanitized_name,
            new_valuetype = input$new_feature_valuetype,
            new_traitdescription = trimws(input$new_feature_description),
            new_minallowedvalue = new_min,
            new_maxallowedvalue = new_max,
            new_expectedunit = new_unit,
            new_factorlevels = new_levels
          )

          # Add the new feature to schema_columns for immediate availability
          current_cols <- schema_columns()
          updated_cols <- sort(unique(c(current_cols, sanitized_name)))
          schema_columns(updated_cols)

          cli::cli_alert_success(sprintf(i18n$t("Feature '%s' added to available features"), sanitized_name))

          shiny::showNotification(
            sprintf(i18n$t("Feature '%s' created successfully! It's now available in the dropdown."), sanitized_name),
            type = "message",
            duration = 5
          )

          # Close modal
          shiny::removeModal()

        }, error = function(e) {
          shiny::showNotification(
            paste(i18n$t("Error creating feature:"), e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # Return mappings and validation status
    return(
      shiny::reactive({
        list(
          mappings = current_mappings(),
          mappings_with_skips = user_modified_mappings(),  # Includes NA for skipped columns
          validation = mapping_validation()
        )
      })
    )
  })
}
