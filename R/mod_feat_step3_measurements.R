# Feature Wizard - Step 3: Add Individual Measurements
#
# Module for uploading individual trait measurements (DBH, height, etc.)
# for existing tagged individuals. Supports wide and long formats.
# Column/trait name mapping with synonym-based auto-matching.

# Null-coalescing helper (avoids dependency on rlang::`%||%` or R >= 4.4)
.null_default <- function(x, default) if (is.null(x)) default else x

#' Feature Wizard Step 3: Individual Measurements - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step3_measurements_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("ruler"),
      i18n$t("Step 3: Upload Individual Measurements"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Upload trait observations (DBH, height, etc.) for existing tagged individuals. Match individuals by plot name and tag number."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Format selector
    shiny::radioButtons(
      ns("data_format"),
      i18n$t("Data Format"),
      choices = setNames(
        c("wide", "long"),
        c(
          i18n$t("Wide format (one column per trait)"),
          i18n$t("Long format (trait type + value columns)")
        )
      ),
      selected = "wide",
      inline = TRUE
    ),

    # Format description
    shiny::uiOutput(ns("format_description")),

    shiny::hr(),

    # File upload
    shiny::fileInput(
      ns("xlsx_file"),
      i18n$t("Choose xlsx file"),
      accept = c(".xlsx", ".xls")
    ),

    # Column mapping UI — appears after upload
    shiny::uiOutput(ns("column_mapping_ui")),

    # Button to create a new individual feature/trait (shown after file upload)
    shiny::uiOutput(ns("create_feature_button")),

    # Trait name mapping UI — appears after column mapping
    shiny::uiOutput(ns("trait_mapping_ui")),

    # Census selection
    shiny::uiOutput(ns("census_selector_ui")),
    shiny::uiOutput(ns("census_warning")),

    shiny::hr(),

    # Data preview
    shiny::h4(
      shiny::icon("table"),
      i18n$t("Data Preview"),
      style = "margin-top: 20px;"
    ),
    shiny::uiOutput(ns("data_preview_message")),
    DT::DTOutput(ns("data_preview_table"))
  )
}


#' Feature Wizard Step 3: Individual Measurements - Server
#'
#' @param id Module namespace ID
#' @param selected_plots Reactive containing data.frame of selected plots
#' @param operation_mode Reactive containing mode string
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive containing list(data, config)
#' @keywords internal
#' @export
mod_feat_step3_measurements_server <- function(id, selected_plots, operation_mode, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    prepared_data    <- shiny::reactiveVal(NULL)
    measurement_config <- shiny::reactiveVal(NULL)
    uploaded_raw     <- shiny::reactiveVal(NULL)
    available_traits <- shiny::reactiveVal(NULL)
    census_choices   <- shiny::reactiveVal(NULL)

    # Load available traits from traitlist (with category, description, unit, factorlevels)
    shiny::observe({
      shiny::req(con())
      tryCatch({
        actual_con <- if (inherits(con(), "Pool")) pool::poolCheckout(con()) else con()
        on.exit(if (inherits(con(), "Pool")) pool::poolReturn(actual_con), add = TRUE)
        traits <- DBI::dbGetQuery(actual_con,
          "SELECT id_trait, trait, valuetype, traitdescription, category,
                  expectedunit, minallowedvalue, maxallowedvalue, factorlevels
           FROM traitlist ORDER BY trait")
        available_traits(traits)
        cli::cli_alert_success("Loaded {nrow(traits)} trait types")
      }, error = function(e) {
        cli::cli_alert_warning("Could not load trait list: {e$message}")
        # Fallback to basic traits_taxa_list
        tryCatch({
          traits <- traits_taxa_list(con = con())
          available_traits(traits)
        }, error = function(e2) {
          cli::cli_alert_warning("Fallback also failed: {e2$message}")
        })
      })
    })

    # Load census choices for selected plots
    shiny::observe({
      shiny::req(con(), selected_plots())
      plots <- selected_plots()
      plot_ids <- plots$id_liste_plots[!is.na(plots$id_liste_plots)]
      if (length(plot_ids) == 0) return()

      tryCatch({
        censuses <- DBI::dbGetQuery(con(), sprintf(
          "SELECT sp.id_sub_plots, sp.id_table_liste_plots, sp.typevalue AS census_num,
                  sp.year, sp.month, sp.day,
                  p.plot_name
           FROM data_liste_sub_plots sp
           JOIN subplotype_list spl ON sp.id_type_sub_plot = spl.id_subplotype
           JOIN data_liste_plots p ON sp.id_table_liste_plots = p.id_liste_plots
           WHERE spl.type = 'census'
             AND sp.id_table_liste_plots IN (%s)
           ORDER BY p.plot_name, sp.typevalue",
          paste(plot_ids, collapse = ", ")
        ))
        census_choices(censuses)
      }, error = function(e) {
        cli::cli_alert_warning("Could not load census data: {e$message}")
      })
    })

    # Create new feature button — shown once a file is uploaded
    output$create_feature_button <- shiny::renderUI({
      shiny::req(uploaded_raw())
      shiny::div(
        style = "margin: 10px 0 15px 0;",
        shiny::actionButton(
          ns("show_create_trait"),
          shiny::tagList(shiny::icon("plus"), " ", i18n()$t("Create New Feature")),
          class = "btn-success btn-sm"
        ),
        shiny::tags$small(
          " — ",
          i18n()$t("Create a new feature entry in the traitlist. It will immediately become available in the mapping above."),
          style = "color: #6c757d;"
        )
      )
    })

    # Show modal when button clicked
    shiny::observeEvent(input$show_create_trait, {
      category_choices <- c(
        "Stem-level trait", "Stem status", "Leaf trait",
        "Wood trait", "Phenology", "Classification",
        "Vitality", "Other trait"
      )
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(
          shiny::icon("plus-circle"), " ",
          i18n()$t("Create New Feature/Attribute")
        ),
        size = "l",
        shiny::fluidRow(
          shiny::column(6,
            shiny::textInput(
              ns("new_trait_name"),
              i18n()$t("Feature Name *"),
              placeholder = "e.g., crown_diameter, bark_thickness"
            ),
            shiny::tags$small(
              i18n()$t("Use lowercase, underscores (not spaces), no special characters"),
              style = "color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;"
            ),
            shiny::selectInput(
              ns("new_trait_valuetype"),
              i18n()$t("Value Type *"),
              choices = c("numeric", "integer", "categorical", "character", "logical", "ordinal"),
              selected = "numeric"
            ),
            shiny::textInput(
              ns("new_trait_unit"),
              i18n()$t("Expected Unit (optional)"),
              placeholder = "e.g., cm, m, kg, %"
            ),
            shiny::conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'numeric' || input['%s'] == 'integer'",
                ns("new_trait_valuetype"), ns("new_trait_valuetype")
              ),
              shiny::textInput(
                ns("new_trait_min"),
                i18n()$t("Minimum Allowed Value (optional)"),
                placeholder = "e.g., 0"
              ),
              shiny::textInput(
                ns("new_trait_max"),
                i18n()$t("Maximum Allowed Value (optional)"),
                placeholder = "e.g., 100"
              )
            )
          ),
          shiny::column(6,
            shiny::textAreaInput(
              ns("new_trait_description"),
              i18n()$t("Description *"),
              placeholder = "Describe what this feature measures or represents",
              rows = 3
            ),
            shiny::selectInput(
              ns("new_trait_category"),
              i18n()$t("Category"),
              choices = category_choices,
              selected = category_choices[1]
            ),
            shiny::conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'categorical' || input['%s'] == 'ordinal'",
                ns("new_trait_valuetype"), ns("new_trait_valuetype")
              ),
              shiny::textInput(
                ns("new_trait_levels"),
                i18n()$t("Factor Levels (comma-separated)"),
                placeholder = "e.g., small, medium, large"
              )
            )
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton(i18n()$t("Cancel")),
          shiny::actionButton(
            ns("create_trait_confirm"),
            shiny::tagList(shiny::icon("check"), " ", i18n()$t("Create Feature")),
            class = "btn-primary"
          )
        )
      ))
    })

    # Handle trait creation
    shiny::observeEvent(input$create_trait_confirm, {
      shiny::req(input$new_trait_name, input$new_trait_valuetype, input$new_trait_description)

      if (trimws(input$new_trait_name) == "" || trimws(input$new_trait_description) == "") {
        shiny::showNotification(
          i18n()$t("Feature name and description are required"),
          type = "error"
        )
        return()
      }

      sanitized_name <- gsub("[^a-z0-9_]", "", gsub(" ", "_", tolower(trimws(input$new_trait_name))))

      if (nchar(sanitized_name) == 0) {
        shiny::showNotification(
          i18n()$t("Feature name contains only invalid characters. Please use letters, numbers, and underscores."),
          type = "error"
        )
        return()
      }

      new_min <- if (!is.null(input$new_trait_min) && trimws(input$new_trait_min) != "") {
        suppressWarnings(as.numeric(input$new_trait_min))
      } else {
        NULL
      }
      new_max <- if (!is.null(input$new_trait_max) && trimws(input$new_trait_max) != "") {
        suppressWarnings(as.numeric(input$new_trait_max))
      } else {
        NULL
      }
      new_unit <- if (!is.null(input$new_trait_unit) && trimws(input$new_trait_unit) != "") {
        trimws(input$new_trait_unit)
      } else {
        NULL
      }
      new_levels <- if (!is.null(input$new_trait_levels) && trimws(input$new_trait_levels) != "") {
        trimws(input$new_trait_levels)
      } else {
        NULL
      }

      shiny::withProgress(message = i18n()$t("Creating new feature..."), {
        tryCatch({
          add_trait(
            new_trait        = sanitized_name,
            new_valuetype    = input$new_trait_valuetype,
            new_traitdescription = trimws(input$new_trait_description),
            new_minallowedvalue  = new_min,
            new_maxallowedvalue  = new_max,
            new_expectedunit     = new_unit,
            new_factorlevels     = new_levels,
            new_category         = input$new_trait_category,
            con         = con(),
            interactive = FALSE
          )

          # Reload trait list so the new entry (with its DB id) is immediately available
          tryCatch({
            actual_con <- if (inherits(con(), "Pool")) pool::poolCheckout(con()) else con()
            traits_fresh <- DBI::dbGetQuery(actual_con,
              "SELECT id_trait, trait, valuetype, traitdescription, category,
                      expectedunit, minallowedvalue, maxallowedvalue, factorlevels
               FROM traitlist ORDER BY trait")
            if (inherits(con(), "Pool")) pool::poolReturn(actual_con)
            available_traits(traits_fresh)
          }, error = function(e2) {
            cli::cli_alert_warning("Could not refresh trait list: {e2$message}")
          })

          shiny::showNotification(
            sprintf(
              i18n()$t("Feature '%s' created successfully! It's now available in the dropdown."),
              sanitized_name
            ),
            type = "message",
            duration = 5
          )
          shiny::removeModal()

        }, error = function(e) {
          shiny::showNotification(
            paste(i18n()$t("Error creating feature:"), e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # Format description
    output$format_description <- shiny::renderUI({
      fmt <- input$data_format
      if (fmt == "wide") {
        shiny::div(
          class = "alert alert-info",
          shiny::icon("info-circle"), " ",
          i18n()$t("Wide format: each trait is a separate column. Expected columns: plot_name, tag, and one column per trait (e.g., DBH, height, crown_diameter).")
        )
      } else {
        shiny::div(
          class = "alert alert-info",
          shiny::icon("info-circle"), " ",
          i18n()$t("Long format: one row per measurement. Expected columns: plot_name, tag, a column for trait type/name, a column for numeric values, and optionally a column for character values. Additional metadata columns are allowed.")
        )
      }
    })

    # Handle file upload
    shiny::observeEvent(input$xlsx_file, {
      shiny::req(input$xlsx_file)
      uploaded_raw(NULL)
      prepared_data(NULL)
      measurement_config(NULL)

      tryCatch({
        raw <- as.data.frame(readxl::read_excel(input$xlsx_file$datapath, guess_max = 5000))
        if (nrow(raw) == 0) {
          shiny::showNotification(i18n()$t("Uploaded file is empty."), type = "error")
          return()
        }
        uploaded_raw(raw)
        shiny::showNotification(
          sprintf(i18n()$t("Loaded %d rows, %d columns. Please map columns below."), nrow(raw), ncol(raw)),
          type = "message", duration = 5
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
      })
    })

    # Column mapping UI
    output$column_mapping_ui <- shiny::renderUI({
      raw <- uploaded_raw()
      if (is.null(raw)) return(NULL)

      fmt       <- input$data_format
      user_cols <- names(raw)
      none      <- c("-- not mapped --" = "")

      # Auto-guess with synonyms
      synonyms <- tryCatch(
        .get_column_synonyms(),
        error = function(e) list()
      )
      # Add common trait measurement synonyms
      synonyms$plot_name <- c(synonyms$plot_name, "plotname", "plot", "parcelle", "site")
      synonyms$tag <- c(synonyms$tag, "tag_number", "individual", "tree_number",
                         "numero", "num", "tree_id", "ind", "arbre", "no_arbre")

      # Simple auto-matching
      auto_map <- .auto_match_columns(user_cols, synonyms)

      if (fmt == "wide") {
        .render_wide_mapping_ui(ns, i18n(), user_cols, none, auto_map)
      } else {
        .render_long_mapping_ui(ns, i18n(), user_cols, none, auto_map)
      }
    })

    # Trait mapping UI (wide format: map each unmapped column to a trait)
    output$trait_mapping_ui <- shiny::renderUI({
      raw <- uploaded_raw()
      fmt <- input$data_format
      traits <- available_traits()
      if (is.null(raw) || is.null(traits)) return(NULL)

      if (fmt == "wide") {
        .render_wide_trait_mapping_ui(ns, i18n(), raw, traits, input)
      } else {
        .render_long_trait_mapping_ui(ns, i18n(), raw, traits, input)
      }
    })

    # Reactive trait description outputs (update when user changes dropdown selection)
    shiny::observe({
      raw <- uploaded_raw()
      traits <- available_traits()
      fmt <- input$data_format
      if (is.null(raw) || is.null(traits)) return()

      # Determine which items have trait_desc_ outputs
      if (fmt == "wide") {
        plot_col <- input$map_plot_name
        tag_col  <- input$map_tag
        if (is.null(plot_col) || plot_col == "" || is.null(tag_col) || tag_col == "") return()
        items <- setdiff(names(raw), c(plot_col, tag_col))
      } else {
        trait_col <- input$map_trait_type
        if (is.null(trait_col) || trait_col == "" || !trait_col %in% names(raw)) return()
        items <- unique(as.character(raw[[trait_col]]))
        items <- items[!is.na(items) & items != ""]
      }

      lapply(items, function(item) {
        safe_item <- gsub("[^a-zA-Z0-9]", "_", item)
        output_id <- paste0("trait_desc_", safe_item)

        output[[output_id]] <- shiny::renderUI({
          selected <- input[[paste0("trait_map_", safe_item)]]
          if (is.null(selected) || selected == "") return(NULL)

          # Find trait info
          idx <- which(traits$trait == selected)
          if (length(idx) == 0) return(NULL)
          tinfo <- traits[idx[1], ]

          desc_parts <- list()

          # Category badge
          if ("category" %in% names(tinfo) && !is.na(tinfo$category) && nchar(tinfo$category) > 0) {
            desc_parts <- c(desc_parts, list(
              shiny::tags$span(
                class = "badge",
                style = "background-color: #6c757d; color: white; font-size: 11px; margin-right: 6px;",
                tinfo$category
              )
            ))
          }

          # Description
          if ("traitdescription" %in% names(tinfo) && !is.na(tinfo$traitdescription) && nchar(tinfo$traitdescription) > 0) {
            desc_parts <- c(desc_parts, list(
              shiny::tags$small(
                shiny::icon("info-circle", style = "color: #007bff;"),
                " ", tinfo$traitdescription,
                style = "color: #6c757d;"
              )
            ))
          }

          # Expected unit
          if ("expectedunit" %in% names(tinfo) && !is.na(tinfo$expectedunit) && nchar(tinfo$expectedunit) > 0) {
            desc_parts <- c(desc_parts, list(
              shiny::br(),
              shiny::tags$small(
                shiny::icon("ruler"),
                " Unit: ",
                shiny::tags$strong(tinfo$expectedunit),
                style = "color: #28a745;"
              )
            ))
          }

          # Factor levels (for categorical traits)
          if ("factorlevels" %in% names(tinfo) && !is.na(tinfo$factorlevels) && nchar(tinfo$factorlevels) > 0) {
            desc_parts <- c(desc_parts, list(
              shiny::br(),
              shiny::tags$small(
                shiny::icon("list"),
                " Expected values: ",
                shiny::tags$code(tinfo$factorlevels, style = "font-size: 10px;"),
                style = "color: #856404;"
              )
            ))
          }

          # Value type + range
          if ("valuetype" %in% names(tinfo) && !is.na(tinfo$valuetype)) {
            range_info <- tinfo$valuetype
            if ("minallowedvalue" %in% names(tinfo) && !is.na(tinfo$minallowedvalue)) {
              range_info <- paste0(range_info, ", min: ", tinfo$minallowedvalue)
            }
            if ("maxallowedvalue" %in% names(tinfo) && !is.na(tinfo$maxallowedvalue)) {
              range_info <- paste0(range_info, ", max: ", tinfo$maxallowedvalue)
            }
            desc_parts <- c(desc_parts, list(
              shiny::br(),
              shiny::tags$small(
                shiny::icon("sliders-h"),
                " Type: ", range_info,
                style = "color: #6c757d;"
              )
            ))
          }

          if (length(desc_parts) == 0) return(NULL)

          shiny::div(
            style = "margin-top: 6px; padding: 8px; background-color: #f0f8ff; border-radius: 4px; border-left: 3px solid #007bff;",
            desc_parts
          )
        })
      })
    })

    # Census selector
    output$census_selector_ui <- shiny::renderUI({
      censuses <- census_choices()
      if (is.null(censuses) || nrow(censuses) == 0) {
        return(shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 15px;",
          shiny::icon("exclamation-triangle"), " ",
          i18n()$t("No census records found for selected plots. Measurements will not be linked to a census.")
        ))
      }

      # Build choices: "Plot - Census N (year)"
      choices <- setNames(
        censuses$id_sub_plots,
        sprintf("%s - Census %s (%s)",
                censuses$plot_name,
                censuses$census_num,
                ifelse(is.na(censuses$year), "?", censuses$year))
      )

      # If all plots have a common latest census, pre-select it
      latest <- censuses[!is.na(censuses$census_num), ]
      if (nrow(latest) > 0) {
        latest_per_plot <- tapply(
          as.numeric(latest$census_num),
          latest$id_table_liste_plots,
          max, na.rm = TRUE
        )
        # Get IDs of latest census per plot
        selected_ids <- sapply(names(latest_per_plot), function(pid) {
          rows <- latest[latest$id_table_liste_plots == as.integer(pid) &
                           as.numeric(latest$census_num) == latest_per_plot[pid], ]
          if (nrow(rows) > 0) rows$id_sub_plots[1] else NA
        })
        selected_ids <- selected_ids[!is.na(selected_ids)]
      } else {
        selected_ids <- NULL
      }

      shiny::tagList(
        shiny::hr(),
        shiny::h4(shiny::icon("calendar"), " ", i18n()$t("Link to Census")),
        shiny::p(
          i18n()$t("Select which census these measurements belong to (one per plot). The latest census per plot is pre-selected."),
          style = "color: #6c757d;"
        ),
        shiny::selectInput(
          ns("census_ids"),
          i18n()$t("Census"),
          choices = choices,
          selected = selected_ids,
          multiple = TRUE
        ),
        shiny::div(
          style = "text-align: center; margin-top: 20px;",
          shiny::actionButton(
            ns("apply_mapping"),
            shiny::tagList(shiny::icon("check"), " ", i18n()$t("Apply Mapping & Preview")),
            class = "btn-success btn-lg"
          )
        )
      )
    })

    # Warning when no census is selected
    output$census_warning <- shiny::renderUI({
      censuses <- census_choices()
      if (is.null(censuses) || nrow(censuses) == 0) return(NULL)
      ids <- input$census_ids
      if (is.null(ids) || length(ids) == 0) {
        shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 5px;",
          shiny::icon("info-circle"), " ",
          i18n()$t("No census selected. Measurements will be added without linking to any census.")
        )
      } else {
        NULL
      }
    })

    # Apply mapping
    shiny::observeEvent(input$apply_mapping, {
      shiny::req(uploaded_raw(), selected_plots())

      raw    <- uploaded_raw()
      plots  <- selected_plots()
      fmt    <- input$data_format
      traits <- available_traits()

      tryCatch({
        # Get plot_name and tag column mappings
        plot_col <- input$map_plot_name
        tag_col  <- input$map_tag

        if (is.null(plot_col) || plot_col == "") {
          shiny::showNotification(i18n()$t("Please map the plot name column."), type = "error")
          return()
        }
        if (is.null(tag_col) || tag_col == "") {
          shiny::showNotification(i18n()$t("Please map the tag column."), type = "error")
          return()
        }

        df <- raw

        # Rename plot_name and tag
        if (plot_col != "plot_name") names(df)[names(df) == plot_col] <- "plot_name"
        if (tag_col != "tag") names(df)[names(df) == tag_col] <- "tag"

        # Filter to selected plots
        df <- df[df$plot_name %in% plots$plot_name, , drop = FALSE]
        if (nrow(df) == 0) {
          shiny::showNotification(i18n()$t("No matching plot names found in uploaded file."), type = "error")
          return()
        }

        # Add plot IDs
        df <- dplyr::left_join(
          df,
          plots[, c("plot_name", "id_liste_plots"), drop = FALSE],
          by = "plot_name"
        )

        # Get census IDs
        census_ids <- input$census_ids
        census_map <- NULL
        if (!is.null(census_ids) && length(census_ids) > 0) {
          censuses <- census_choices()
          if (!is.null(censuses)) {
            census_map <- censuses[censuses$id_sub_plots %in% as.integer(census_ids),
                                   c("id_sub_plots", "id_table_liste_plots"), drop = FALSE]
          }
        }

        if (fmt == "wide") {
          result <- .apply_wide_mapping(df, traits, census_map, input, ns, i18n())
        } else {
          result <- .apply_long_mapping(df, traits, census_map, input, ns, i18n())
        }

        if (is.null(result)) return()

        prepared_data(result$data)
        measurement_config(result$config)

        shiny::showNotification(
          sprintf(i18n()$t("%d measurement(s) prepared."), nrow(result$data)),
          type = "message", duration = 4
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })

    # Data preview
    output$data_preview_message <- shiny::renderUI({
      d <- prepared_data()
      if (is.null(d)) {
        return(shiny::div(
          class = "alert alert-secondary",
          shiny::icon("info-circle"), " ",
          i18n()$t("No data prepared yet. Upload a file and map columns above.")
        ))
      }
      shiny::div(
        class = "alert alert-info",
        shiny::icon("table"), " ",
        sprintf(i18n()$t("%d measurement rows, %d columns ready"), nrow(d), ncol(d))
      )
    })

    output$data_preview_table <- DT::renderDT({
      d <- prepared_data()
      shiny::req(d)
      display <- d[, !names(d) %in% c("id_liste_plots", "id_sub_plots"), drop = FALSE]
      DT::datatable(
        utils::head(display, 50),
        options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        class = "display cell-border stripe"
      )
    })

    # Return
    return(shiny::reactive({
      d   <- prepared_data()
      cfg <- measurement_config()
      if (is.null(d) || is.null(cfg)) return(NULL)
      list(data = d, config = cfg)
    }))
  })
}


# ============================================================
# Internal helper functions
# ============================================================

#' Simple auto-matching of user columns to known column names via synonyms
#' @keywords internal
.auto_match_columns <- function(user_cols, synonyms) {
  result <- list()
  user_lower <- tolower(user_cols)

  for (db_col in names(synonyms)) {
    # Exact match first
    idx <- which(user_lower == tolower(db_col))
    if (length(idx) > 0) {
      result[[db_col]] <- user_cols[idx[1]]
      next
    }
    # Synonym match
    syns <- tolower(synonyms[[db_col]])
    idx <- which(user_lower %in% syns)
    if (length(idx) > 0) {
      result[[db_col]] <- user_cols[idx[1]]
    }
  }
  result
}


#' Render column mapping UI for wide format
#' @keywords internal
.render_wide_mapping_ui <- function(ns, i18n, user_cols, none, auto_map) {
  shiny::tagList(
    shiny::hr(),
    shiny::h4(shiny::icon("exchange-alt"), " ", i18n$t("Map Key Columns")),
    shiny::p(
      i18n$t("Map the plot name and tag columns. Remaining columns will be treated as potential trait columns in the next section."),
      style = "color: #6c757d;"
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(
        ns("map_plot_name"), i18n$t("Plot name column *"),
        choices = c(none, user_cols),
        selected = .null_default(auto_map[["plot_name"]], "")
      )),
      shiny::column(6, shiny::selectInput(
        ns("map_tag"), i18n$t("Tag / Individual number column *"),
        choices = c(none, user_cols),
        selected = .null_default(auto_map[["tag"]], "")
      ))
    )
  )
}


#' Render column mapping UI for long format
#' @keywords internal
.render_long_mapping_ui <- function(ns, i18n, user_cols, none, auto_map) {
  # Add synonyms for long-format specific columns
  trait_type_guess <- ""
  value_num_guess  <- ""
  value_char_guess <- ""

  lower_cols <- tolower(user_cols)
  # Guess trait type column
  trait_patterns <- c("trait", "trait_name", "trait_type", "variable", "measure",
                      "observation", "type", "caractere", "mesure")
  for (pat in trait_patterns) {
    idx <- which(lower_cols == pat)
    if (length(idx) > 0) { trait_type_guess <- user_cols[idx[1]]; break }
  }
  # Guess numeric value column
  num_patterns <- c("value", "traitvalue", "trait_value", "value_num",
                     "numeric_value", "valeur", "mesure_num")
  for (pat in num_patterns) {
    idx <- which(lower_cols == pat)
    if (length(idx) > 0) { value_num_guess <- user_cols[idx[1]]; break }
  }
  # Guess character value column
  char_patterns <- c("value_char", "traitvalue_char", "char_value",
                      "text_value", "valeur_char", "categorie")
  for (pat in char_patterns) {
    idx <- which(lower_cols == pat)
    if (length(idx) > 0) { value_char_guess <- user_cols[idx[1]]; break }
  }

  shiny::tagList(
    shiny::hr(),
    shiny::h4(shiny::icon("exchange-alt"), " ", i18n$t("Map Columns")),
    shiny::p(
      i18n$t("Map key columns and the trait type/value columns. Additional metadata columns (e.g., year, month, remarks) will be preserved."),
      style = "color: #6c757d;"
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(
        ns("map_plot_name"), i18n$t("Plot name column *"),
        choices = c(none, user_cols),
        selected = .null_default(auto_map[["plot_name"]], "")
      )),
      shiny::column(6, shiny::selectInput(
        ns("map_tag"), i18n$t("Tag / Individual number column *"),
        choices = c(none, user_cols),
        selected = .null_default(auto_map[["tag"]], "")
      ))
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(
        ns("map_trait_type"), i18n$t("Trait type / name column *"),
        choices = c(none, user_cols),
        selected = trait_type_guess
      )),
      shiny::column(4, shiny::selectInput(
        ns("map_value_num"), i18n$t("Numeric value column"),
        choices = c(none, user_cols),
        selected = value_num_guess
      )),
      shiny::column(4, shiny::selectInput(
        ns("map_value_char"), i18n$t("Character value column"),
        choices = c(none, user_cols),
        selected = value_char_guess
      ))
    )
  )
}


#' Build grouped trait choices by category for selectize dropdowns
#' @keywords internal
.build_grouped_trait_choices <- function(traits) {
  has_category <- "category" %in% names(traits) &&
    any(!is.na(traits$category) & nchar(trimws(as.character(traits$category))) > 0)

  if (!has_category) {
    # Flat list if no categories
    return(c("-- skip --" = "", setNames(traits$trait, traits$trait)))
  }

  # Build labels with description hint
  labels <- sapply(seq_len(nrow(traits)), function(i) {
    label <- traits$trait[i]
    if ("traitdescription" %in% names(traits) &&
        !is.na(traits$traitdescription[i]) && nchar(traits$traitdescription[i]) > 0) {
      desc <- traits$traitdescription[i]
      if (nchar(desc) > 60) desc <- paste0(substr(desc, 1, 57), "...")
      label <- paste0(label, " - ", desc)
    }
    if ("expectedunit" %in% names(traits) &&
        !is.na(traits$expectedunit[i]) && nchar(traits$expectedunit[i]) > 0) {
      label <- paste0(label, " [", traits$expectedunit[i], "]")
    }
    label
  })

  # Group by category
  cats <- ifelse(is.na(traits$category) | nchar(trimws(as.character(traits$category))) == 0,
                 "Other", as.character(traits$category))

  # Predefined category order. The descriptor categories sit near the top:
  # they are not traits in the colloquial sense, but a census table carries
  # them (quadrat, position_x) and leaving them to fall to the bottom of a
  # 100-entry dropdown is how they get missed.
  cat_order <- c(
    "Stem-level trait", "Stem status",
    "Position", "Sampling identification", "Observation",
    "Leaf trait", "Wood trait",
    "Phenology", "Classification", "Vitality", "Reproductive trait",
    "People", "Other trait", "Other"
  )
  unique_cats <- unique(cats)
  ordered_cats <- c(
    intersect(cat_order, unique_cats),
    setdiff(unique_cats, cat_order)
  )

  grouped <- list("---" = c("(Skip)" = ""))
  for (cat in ordered_cats) {
    idx <- which(cats == cat)
    grouped[[cat]] <- setNames(traits$trait[idx], labels[idx])
  }
  grouped
}

#' Render trait mapping UI for wide format
#' @keywords internal
.render_wide_trait_mapping_ui <- function(ns, i18n, raw, traits, input) {
  # Determine which columns are NOT mapped as plot_name or tag
  plot_col <- input$map_plot_name
  tag_col  <- input$map_tag
  if (is.null(plot_col) || plot_col == "" ||
      is.null(tag_col) || tag_col == "") {
    return(shiny::div(
      class = "alert alert-secondary",
      i18n$t("Please map plot name and tag columns first.")
    ))
  }

  user_cols   <- names(raw)
  mapped_cols <- c(plot_col, tag_col)
  trait_candidates <- setdiff(user_cols, mapped_cols)

  if (length(trait_candidates) == 0) {
    return(shiny::div(
      class = "alert alert-warning",
      i18n$t("No remaining columns to map as traits.")
    ))
  }

  # Build grouped trait choices by category
  trait_choices <- .build_grouped_trait_choices(traits)

  # Auto-match trait names using fuzzy matching
  trait_lower <- tolower(traits$trait)
  auto_trait_map <- sapply(trait_candidates, function(col) {
    col_lower <- tolower(col)
    idx <- which(trait_lower == col_lower)
    if (length(idx) > 0) return(traits$trait[idx[1]])
    idx <- which(startsWith(trait_lower, col_lower) | startsWith(col_lower, trait_lower))
    if (length(idx) > 0) return(traits$trait[idx[1]])
    col_clean <- gsub("[_\\s.-]", "", col_lower)
    trait_clean <- gsub("[_\\s.-]", "", trait_lower)
    idx <- which(trait_clean == col_clean)
    if (length(idx) > 0) return(traits$trait[idx[1]])
    return("")
  }, USE.NAMES = TRUE)

  shiny::tagList(
    shiny::hr(),
    shiny::h4(shiny::icon("tags"), " ", i18n$t("Map Columns to Traits")),
    shiny::p(
      i18n$t("Map each data column to a trait from the database. Columns mapped to '-- skip --' will be ignored."),
      style = "color: #6c757d;"
    ),
    lapply(trait_candidates, function(col) {
      # Sample values from the column
      sample_vals <- utils::head(unique(raw[[col]][!is.na(raw[[col]])]), 5)
      sample_text <- paste(sample_vals, collapse = ", ")
      if (nchar(sample_text) > 60) sample_text <- paste0(substr(sample_text, 1, 57), "...")
      n_values <- sum(!is.na(raw[[col]]))

      # Auto-matched trait info
      matched_trait <- auto_trait_map[[col]]
      is_matched <- !is.null(matched_trait) && matched_trait != ""

      safe_col <- gsub("[^a-zA-Z0-9]", "_", col)

      shiny::div(
        style = sprintf(
          "margin-bottom: 12px; padding: 12px; border: 1px solid #dee2e6; border-radius: 6px; border-left: 4px solid %s; background-color: #fafafa;",
          if (is_matched) "#28a745" else "#dc3545"
        ),
        shiny::fluidRow(
          # Left: user column name + sample values
          shiny::column(4,
            shiny::strong(col, style = "font-size: 14px;"),
            shiny::br(),
            shiny::tags$small(
              shiny::icon("eye"),
              " ", i18n$t("Samples"), ": ",
              shiny::tags$code(sample_text, style = "font-size: 11px;"),
              style = "color: #6c757d;"
            ),
            shiny::br(),
            shiny::tags$small(
              shiny::icon("hashtag"),
              sprintf(" %d values", n_values),
              style = "color: #6c757d;"
            )
          ),
          # Arrow
          shiny::column(1,
            shiny::div(
              shiny::icon("arrow-right", style = "font-size: 24px; color: #007bff;"),
              style = "text-align: center; padding-top: 15px;"
            )
          ),
          # Right: trait dropdown + description output + role
          shiny::column(7,
            shiny::selectizeInput(
              ns(paste0("trait_map_", safe_col)),
              label = NULL,
              choices = trait_choices,
              selected = .null_default(matched_trait, ""),
              width = "100%",
              options = list(placeholder = "(Skip this column)", allowEmptyOption = TRUE)
            ),
            shiny::uiOutput(ns(paste0("trait_desc_", safe_col))),
            shiny::radioButtons(
              ns(paste0("col_role_", safe_col)),
              label = shiny::tags$small(
                shiny::icon("tag", style = "color: #6c757d;"),
                " ", i18n$t("Role:"),
                style = "color: #6c757d; font-weight: 600;"
              ),
              choices = setNames(
                c("trait", "feature"),
                c(i18n$t("Trait data"), i18n$t("Metadata (features_field)"))
              ),
              selected = "trait",
              inline = TRUE
            )
          )
        )
      )
    })
  )
}


#' Render trait mapping UI for long format
#' @keywords internal
.render_long_trait_mapping_ui <- function(ns, i18n, raw, traits, input) {
  trait_col <- input$map_trait_type
  if (is.null(trait_col) || trait_col == "") {
    return(shiny::div(
      class = "alert alert-secondary",
      i18n$t("Please map the trait type column first.")
    ))
  }

  if (!trait_col %in% names(raw)) return(NULL)

  # Get unique trait names from the data
  user_trait_names <- unique(as.character(raw[[trait_col]]))
  user_trait_names <- user_trait_names[!is.na(user_trait_names) & user_trait_names != ""]

  if (length(user_trait_names) == 0) {
    return(shiny::div(
      class = "alert alert-warning",
      i18n$t("No trait names found in the selected column.")
    ))
  }

  # Build grouped trait choices by category
  trait_choices <- .build_grouped_trait_choices(traits)

  # Auto-match
  trait_lower <- tolower(traits$trait)
  auto_trait_map <- sapply(user_trait_names, function(name) {
    name_lower <- tolower(name)
    idx <- which(trait_lower == name_lower)
    if (length(idx) > 0) return(traits$trait[idx[1]])
    name_clean <- gsub("[_\\s.-]", "", name_lower)
    trait_clean <- gsub("[_\\s.-]", "", trait_lower)
    idx <- which(trait_clean == name_clean)
    if (length(idx) > 0) return(traits$trait[idx[1]])
    idx <- which(startsWith(trait_lower, name_lower) | startsWith(name_lower, trait_lower))
    if (length(idx) > 0) return(traits$trait[idx[1]])
    return("")
  }, USE.NAMES = TRUE)

  n_rows_per <- table(raw[[trait_col]])

  # Get value column for sample preview
  value_col <- input$map_value_num
  has_value_col <- !is.null(value_col) && value_col != "" && value_col %in% names(raw)

  # Compute extra columns (not mapped to any key role) for optional features_field designation
  key_cols_long <- c(input$map_plot_name, input$map_tag,
                     input$map_trait_type, input$map_value_num, input$map_value_char)
  key_cols_long <- key_cols_long[!is.null(key_cols_long) &
                                   nchar(trimws(as.character(key_cols_long))) > 0]
  extra_cols_long <- setdiff(names(raw), key_cols_long)

  shiny::tagList(
    shiny::hr(),
    shiny::h4(shiny::icon("tags"), " ", i18n$t("Map Trait Names")),
    shiny::p(
      i18n$t("Map each trait name from your file to a trait in the database. Unmapped traits will be skipped."),
      style = "color: #6c757d;"
    ),
    lapply(user_trait_names, function(name) {
      count <- as.integer(n_rows_per[name])
      matched_trait <- auto_trait_map[[name]]
      is_matched <- !is.null(matched_trait) && matched_trait != ""
      safe_name <- gsub("[^a-zA-Z0-9]", "_", name)

      # Get sample values for this trait from the value column
      sample_text <- ""
      if (has_value_col) {
        trait_rows <- which(as.character(raw[[trait_col]]) == name)
        sample_vals <- utils::head(unique(raw[[value_col]][trait_rows][!is.na(raw[[value_col]][trait_rows])]), 5)
        sample_text <- paste(sample_vals, collapse = ", ")
        if (nchar(sample_text) > 60) sample_text <- paste0(substr(sample_text, 1, 57), "...")
      }

      shiny::div(
        style = sprintf(
          "margin-bottom: 12px; padding: 12px; border: 1px solid #dee2e6; border-radius: 6px; border-left: 4px solid %s; background-color: #fafafa;",
          if (is_matched) "#28a745" else "#dc3545"
        ),
        shiny::fluidRow(
          # Left: user trait name + row count + sample values
          shiny::column(4,
            shiny::strong(name, style = "font-size: 14px;"),
            shiny::br(),
            shiny::tags$small(
              shiny::icon("hashtag"),
              sprintf(" %d rows", count),
              style = "color: #6c757d;"
            ),
            if (sample_text != "") shiny::tagList(
              shiny::br(),
              shiny::tags$small(
                shiny::icon("eye"),
                " ", i18n$t("Samples"), ": ",
                shiny::tags$code(sample_text, style = "font-size: 11px;"),
                style = "color: #6c757d;"
              )
            )
          ),
          # Arrow
          shiny::column(1,
            shiny::div(
              shiny::icon("arrow-right", style = "font-size: 24px; color: #007bff;"),
              style = "text-align: center; padding-top: 15px;"
            )
          ),
          # Right: trait dropdown + description output
          shiny::column(7,
            shiny::selectizeInput(
              ns(paste0("trait_map_", safe_name)),
              label = NULL,
              choices = trait_choices,
              selected = .null_default(matched_trait, ""),
              width = "100%",
              options = list(placeholder = "(Skip this trait)", allowEmptyOption = TRUE)
            ),
            shiny::uiOutput(ns(paste0("trait_desc_", safe_name)))
          )
        )
      )
    }),
    # Extra columns: let user designate as features_field (measurement metadata)
    if (length(extra_cols_long) > 0) shiny::tagList(
      shiny::hr(),
      shiny::h5(
        shiny::icon("columns"), " ",
        i18n$t("Additional column roles"),
        style = "color: #495057;"
      ),
      shiny::p(
        i18n$t("Select the role for each additional column. Columns designated as 'features_field' will be linked to each measurement as metadata."),
        style = "color: #6c757d; font-size: 13px;"
      ),
      lapply(extra_cols_long, function(col) {
        safe_col <- gsub("[^a-zA-Z0-9]", "_", col)
        shiny::fluidRow(
          shiny::column(4, shiny::strong(col, style = "font-size: 13px;")),
          shiny::column(8,
            shiny::radioButtons(
              ns(paste0("meta_role_", safe_col)),
              label = NULL,
              choices = setNames(
                c("meta", "feature"),
                c(i18n$t("Keep as metadata"), i18n$t("Use as features_field"))
              ),
              selected = "meta",
              inline = TRUE
            )
          )
        )
      })
    )
  )
}


#' Apply wide format mapping and build prepared data
#' @keywords internal
.apply_wide_mapping <- function(df, traits, census_map, input, ns, i18n) {
  # Collect trait mappings: user_col -> trait name
  user_cols <- names(df)
  mapped_cols <- c("plot_name", "tag", "id_liste_plots")
  trait_mappings <- list()

  # Check each potential trait column
  candidate_cols <- setdiff(user_cols, mapped_cols)
  for (col in candidate_cols) {
    input_id <- paste0("trait_map_", gsub("[^a-zA-Z0-9]", "_", col))
    mapped_trait <- input[[input_id]]
    if (!is.null(mapped_trait) && mapped_trait != "") {
      trait_mappings[[col]] <- mapped_trait
    }
  }

  if (length(trait_mappings) == 0) {
    shiny::showNotification(i18n$t("No trait columns mapped. Please map at least one column to a trait."), type = "error")
    return(NULL)
  }

  # Split mapped columns into traits_field and features_field by role.
  # The role selector only applies when >1 column is mapped; a single mapped
  # column is always traits_field (no features_field possible).
  total_mapped <- length(trait_mappings)
  traits_field_cols  <- character(0)
  features_field_cols <- character(0)

  for (col in names(trait_mappings)) {
    safe_col <- gsub("[^a-zA-Z0-9]", "_", col)
    role <- input[[paste0("col_role_", safe_col)]]
    if (total_mapped > 1 && !is.null(role) && role == "feature") {
      features_field_cols <- c(features_field_cols, col)
    } else {
      traits_field_cols <- c(traits_field_cols, col)
    }
  }
  if (length(traits_field_cols) == 0) {
    shiny::showNotification(
      i18n$t("No columns designated as trait data. Please assign at least one column as 'Trait data'."),
      type = "error"
    )
    return(NULL)
  }
  features_field_final <- if (length(features_field_cols) > 0) features_field_cols else NULL

  # Build trait ID lookup
  trait_id_lookup <- setNames(traits$id_trait, traits$trait)

  # Pivot wide to long format for storage (traits_field only; features_field
  # columns are carried forward as per-row metadata on each measurement row)
  rows <- list()
  for (i in seq_len(nrow(df))) {
    for (user_col in traits_field_cols) {
      trait_name <- trait_mappings[[user_col]]
      trait_id <- trait_id_lookup[[trait_name]]
      if (is.null(trait_id)) next

      val <- df[[user_col]][i]
      if (is.na(val)) next

      trait_info <- traits[traits$trait == trait_name, ]
      vtype <- if (nrow(trait_info) > 0) trait_info$valuetype[1] else "numeric"

      row <- data.frame(
        plot_name = df$plot_name[i],
        tag = df$tag[i],
        id_liste_plots = df$id_liste_plots[i],
        trait_name = trait_name,
        traitid = as.integer(trait_id),
        traitvalue = if (vtype %in% c("numeric", "integer")) suppressWarnings(as.numeric(val)) else NA_real_,
        traitvalue_char = if (!vtype %in% c("numeric", "integer")) as.character(val) else NA_character_,
        stringsAsFactors = FALSE
      )
      # Carry features_field column values to each measurement row
      for (feat_col in features_field_cols) {
        row[[feat_col]] <- df[[feat_col]][i]
      }
      rows[[length(rows) + 1]] <- row
    }
  }

  if (length(rows) == 0) {
    shiny::showNotification(i18n$t("No valid measurements found after mapping."), type = "error")
    return(NULL)
  }

  result <- do.call(rbind, rows)

  # Add census IDs
  if (!is.null(census_map) && nrow(census_map) > 0) {
    result <- dplyr::left_join(
      result,
      census_map[, c("id_sub_plots", "id_table_liste_plots"), drop = FALSE],
      by = c("id_liste_plots" = "id_table_liste_plots")
    )
  } else {
    result$id_sub_plots <- NA_integer_
  }

  config <- list(
    mode = "add_measurements",
    format = "wide",
    trait_mappings = trait_mappings[traits_field_cols],
    features_field_mappings = if (length(features_field_cols) > 0) trait_mappings[features_field_cols] else NULL,
    traits_field = traits_field_cols,
    features_field = features_field_final,
    people_columns = character(0)
  )

  list(data = result, config = config)
}


#' Apply long format mapping and build prepared data
#' @keywords internal
.apply_long_mapping <- function(df, traits, census_map, input, ns, i18n) {
  # Get column mappings
  trait_type_col <- input$map_trait_type
  value_num_col  <- input$map_value_num
  value_char_col <- input$map_value_char

  if (is.null(trait_type_col) || trait_type_col == "") {
    shiny::showNotification(i18n$t("Please map the trait type column."), type = "error")
    return(NULL)
  }

  # Rename mapped columns
  if (trait_type_col != "trait_type" && trait_type_col %in% names(df)) {
    names(df)[names(df) == trait_type_col] <- "trait_type"
  }
  if (!is.null(value_num_col) && value_num_col != "" && value_num_col %in% names(df)) {
    if (value_num_col != "traitvalue") names(df)[names(df) == value_num_col] <- "traitvalue"
  }
  if (!is.null(value_char_col) && value_char_col != "" && value_char_col %in% names(df)) {
    if (value_char_col != "traitvalue_char") names(df)[names(df) == value_char_col] <- "traitvalue_char"
  }

  # Ensure value columns exist
  if (!"traitvalue" %in% names(df)) df$traitvalue <- NA_real_
  if (!"traitvalue_char" %in% names(df)) df$traitvalue_char <- NA_character_

  # Get unique trait names from data and their mappings
  user_trait_names <- unique(as.character(df$trait_type))
  user_trait_names <- user_trait_names[!is.na(user_trait_names) & user_trait_names != ""]

  trait_id_lookup <- setNames(traits$id_trait, traits$trait)
  trait_name_map <- list()
  for (name in user_trait_names) {
    input_id <- paste0("trait_map_", gsub("[^a-zA-Z0-9]", "_", name))
    mapped_trait <- input[[input_id]]
    if (!is.null(mapped_trait) && mapped_trait != "") {
      trait_name_map[[name]] <- mapped_trait
    }
  }

  # If no mappings found from UI inputs (e.g. dynamic inputs not yet registered),
  # fall back to auto-matching trait names directly against the database
  if (length(trait_name_map) == 0) {
    trait_lower <- tolower(traits$trait)
    for (name in user_trait_names) {
      name_lower <- tolower(name)
      # Exact match
      idx <- which(trait_lower == name_lower)
      if (length(idx) == 0) {
        # Clean match (remove underscores/spaces/dots)
        name_clean <- gsub("[_\\s.-]", "", name_lower)
        trait_clean <- gsub("[_\\s.-]", "", trait_lower)
        idx <- which(trait_clean == name_clean)
      }
      if (length(idx) == 0) {
        # Partial match
        idx <- which(startsWith(trait_lower, name_lower) | startsWith(name_lower, trait_lower))
      }
      if (length(idx) > 0) {
        trait_name_map[[name]] <- traits$trait[idx[1]]
      }
    }
    if (length(trait_name_map) > 0) {
      cli::cli_alert_info("Auto-matched {length(trait_name_map)} trait(s) from data column values")
    }
  }

  if (length(trait_name_map) == 0) {
    shiny::showNotification(i18n$t("No trait names mapped. Please map at least one trait."), type = "error")
    return(NULL)
  }

  # Filter to mapped traits only
  mapped_user_names <- names(trait_name_map)
  df <- df[df$trait_type %in% mapped_user_names, , drop = FALSE]

  if (nrow(df) == 0) {
    shiny::showNotification(i18n$t("No rows remaining after filtering to mapped traits."), type = "error")
    return(NULL)
  }

  # Replace user trait names with database trait names and add IDs
  df$trait_name <- sapply(df$trait_type, function(x) .null_default(trait_name_map[[x]], NA_character_))
  df$traitid <- sapply(df$trait_name, function(x) {
    if (is.na(x)) NA_integer_ else as.integer(trait_id_lookup[[x]])
  })

  # Remove rows with unresolved traits
  df <- df[!is.na(df$traitid), , drop = FALSE]

  # Ensure numeric values are numeric
  df$traitvalue <- suppressWarnings(as.numeric(df$traitvalue))

  # Add census IDs
  if (!is.null(census_map) && nrow(census_map) > 0) {
    df <- dplyr::left_join(
      df,
      census_map[, c("id_sub_plots", "id_table_liste_plots"), drop = FALSE],
      by = c("id_liste_plots" = "id_table_liste_plots")
    )
  } else {
    df$id_sub_plots <- NA_integer_
  }

  # Keep only essential columns + any metadata
  keep_cols <- c("plot_name", "tag", "id_liste_plots", "trait_name", "traitid",
                 "traitvalue", "traitvalue_char", "id_sub_plots")
  # Preserve metadata columns not already mapped
  already_mapped <- c("plot_name", "tag", "trait_type", "traitvalue", "traitvalue_char",
                       "id_liste_plots", "trait_name", "traitid", "id_sub_plots")
  metadata_cols <- setdiff(names(df), already_mapped)
  result <- df[, c(intersect(keep_cols, names(df)), metadata_cols), drop = FALSE]

  # Determine features_field from metadata column role inputs
  features_field_long <- character(0)
  for (col in metadata_cols) {
    safe_col <- gsub("[^a-zA-Z0-9]", "_", col)
    role <- input[[paste0("meta_role_", safe_col)]]
    if (!is.null(role) && role == "feature") {
      features_field_long <- c(features_field_long, col)
    }
  }
  features_field_final <- if (length(features_field_long) > 0) features_field_long else NULL

  config <- list(
    mode = "add_measurements",
    format = "long",
    trait_name_map = trait_name_map,
    metadata_columns = metadata_cols,
    features_field = features_field_final,
    people_columns = character(0)
  )

  list(data = result, config = config)
}
