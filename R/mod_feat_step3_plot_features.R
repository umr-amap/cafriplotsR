# Feature Wizard - Step 3: Enter/Upload Plot Features
#
# Module for entering feature data via form or xlsx upload.
# Handles both "new_census" and "add_features" modes.

#' Feature Wizard Step 3: Plot Features - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step3_plot_features_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("edit"),
      i18n$t("Step 3: Enter Feature Data"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Enter feature data manually or upload an xlsx file."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # What the selected plots already hold. Entering a feature blind is how a
    # plot ends up with two principal investigators, or with a value recorded
    # twice for the same year.
    shiny::wellPanel(
      shiny::h4(
        shiny::icon("clipboard-list"), " ",
        i18n$t("Already recorded for these plots")
      ),
      shiny::tags$p(
        class = "text-muted",
        shiny::tags$small(i18n$t("Every feature the selected plot(s) already carry, as an extraction would summarise it. A feature that already has a record is not blocked here - adding another one makes the extracted value an aggregate."))
      ),
      shiny::uiOutput(ns("existing_info")),
      shiny::checkboxInput(
        ns("existing_only_selected"),
        i18n$t("Only the feature types selected below"),
        value = FALSE
      ),
      DT::DTOutput(ns("existing_tbl"))
    ),

    # Input method toggle
    shiny::radioButtons(
      ns("input_method"),
      i18n$t("Input Method"),
      choices = setNames(
        c("form", "upload"),
        c(i18n$t("Manual Form Entry"), i18n$t("Upload xlsx File"))
      ),
      selected = "form",
      inline = TRUE
    ),

    shiny::hr(),

    # Dynamic content based on mode and input method
    shiny::uiOutput(ns("feature_input_ui")),

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


#' Feature Wizard Step 3: Plot Features - Server
#'
#' @param id Module namespace ID
#' @param selected_plots Reactive containing data.frame of selected plots
#' @param operation_mode Reactive containing mode string
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive containing list(data, config)
#' @keywords internal
#' @export
mod_feat_step3_plot_features_server <- function(id, selected_plots, operation_mode, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    prepared_data   <- shiny::reactiveVal(NULL)
    feature_config  <- shiny::reactiveVal(NULL)
    available_features <- shiny::reactiveVal(NULL)
    uploaded_raw    <- shiny::reactiveVal(NULL)   # raw xlsx, waiting for mapping

    # Load available feature types
    shiny::observe({
      shiny::req(con())
      tryCatch({
        feat_list <- subplot_list(con())
        available_features(feat_list)
      }, error = function(e) {
        cli::cli_alert_warning("Could not load feature types: {e$message}")
      })
    })

    # --- What the selected plots already hold -------------------------------
    # The same resolver the update app reads, so both apps summarise a feature
    # the way an extraction really does rather than each inventing a rule.
    existing_records <- shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::req(con())
      plots <- tryCatch(selected_plots(), error = function(e) NULL)
      ids <- if (is.null(plots) || !"id_liste_plots" %in% names(plots)) {
        integer(0)
      } else {
        plots$id_liste_plots[!is.na(plots$id_liste_plots)]
      }
      if (length(ids) == 0) {
        existing_records(NULL)
        return()
      }
      existing_records(tryCatch(
        .upd_plot_feature_records(ids, con()),
        error = function(e) {
          cli::cli_alert_warning("Could not load existing features: {e$message}")
          NULL
        }
      ))
    })

    existing_summary <- shiny::reactive({
      fr <- existing_records()
      if (is.null(fr)) return(NULL)
      .upd_feature_summary(fr)
    })

    # The table honours the "only what I selected" checkbox; the notes under
    # each input do not, since they are already about one feature.
    existing_shown <- shiny::reactive({
      s <- existing_summary()
      if (is.null(s)) return(NULL)
      if (isTRUE(input$existing_only_selected) &&
          length(input$feature_types) > 0) {
        s <- s[s$feature %in% input$feature_types, , drop = FALSE]
      }
      s
    })

    output$existing_info <- shiny::renderUI({
      s <- existing_summary()
      if (is.null(s)) {
        return(shiny::div(
          class = "alert alert-secondary",
          shiny::icon("info-circle"), " ",
          i18n()$t("Select plots in step 1 to see what they already hold.")
        ))
      }
      if (nrow(s) == 0) {
        return(shiny::div(
          class = "alert alert-info",
          shiny::icon("info-circle"), " ",
          i18n()$t("The selected plot(s) carry no feature yet.")
        ))
      }
      n_plots <- length(unique(s$plot_name))
      n_agg <- sum(s$is_aggregated)
      msg <- sprintf(
        i18n()$t("%d feature(s) across %d plot(s)."),
        nrow(s), n_plots
      )
      if (n_agg > 0) {
        shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"), " ", msg, " ",
          sprintf(
            i18n()$t("%d of them are already backed by several records."),
            n_agg
          )
        )
      } else {
        shiny::div(class = "alert alert-success", shiny::icon("check"), " ", msg)
      }
    })

    output$existing_tbl <- DT::renderDT({
      s <- existing_shown()
      shiny::req(s)
      shiny::req(nrow(s) > 0)
      .feature_overview_dt(s, i18n(), page_length = 5)
    })

    # One line under a feature's input: what the selected plots already have
    # for it. Amber whenever there is something, because a second record turns
    # the extracted value into an aggregate.
    existing_note <- function(feat_type) {
      s <- existing_summary()
      if (is.null(s) || nrow(s) == 0) return(NULL)
      rows <- s[s$feature == feat_type, , drop = FALSE]
      if (nrow(rows) == 0) {
        return(shiny::div(
          class = "text-muted",
          style = "margin: -10px 0 15px 0; font-size: 12px;",
          shiny::icon("circle-check"), " ",
          i18n()$t("Nothing recorded yet for the selected plot(s).")
        ))
      }
      per_plot <- vapply(seq_len(nrow(rows)), function(i) {
        value <- if (is.na(rows$aggregate_display[i])) {
          .feature_rule_label(rows$agg_rule[i], rows$n_records[i], i18n())
        } else {
          rows$aggregate_display[i]
        }
        sprintf("%s: %s", rows$plot_name[i], value)
      }, character(1))

      shiny::div(
        style = paste("margin: -10px 0 15px 0; font-size: 12px;",
                      "color: #856404; background: #fff3cd;",
                      "border-radius: 3px; padding: 6px 8px;"),
        shiny::icon("exclamation-triangle"), " ",
        shiny::strong(i18n()$t("Already recorded:")), " ",
        paste(per_plot, collapse = " | ")
      )
    }

    # Dynamic feature value inputs (for "add_features" mode)
    output$dynamic_feature_inputs <- shiny::renderUI({
      shiny::req(input$feature_types, available_features())

      feat_list <- available_features()
      selected <- input$feature_types
      lapply(selected, function(feat_type) {
        feat_info <- feat_list[feat_list$type == feat_type, ]
        if (nrow(feat_info) == 0) return(NULL)

        vtype <- feat_info$valuetype[1]
        label <- paste0(feat_type, " (", feat_info$category[1], ")")
        hint <- if (!is.na(feat_info$typedescription[1])) {
          paste0(" - ", feat_info$typedescription[1])
        } else ""

        control <- if (vtype == "table_colnam") {
          shiny::textInput(
            ns(paste0("feat_val_", feat_type)),
            paste0(label, hint),
            placeholder = i18n()$t("Enter person name(s), comma-separated")
          )
        } else if (vtype == "numeric" || vtype == "integer") {
          shiny::numericInput(
            ns(paste0("feat_val_", feat_type)),
            paste0(label, hint),
            value = NA,
            min = if (!is.na(feat_info$minallowedvalue[1])) feat_info$minallowedvalue[1] else NA,
            max = if (!is.na(feat_info$maxallowedvalue[1])) feat_info$maxallowedvalue[1] else NA
          )
        } else {
          shiny::textInput(
            ns(paste0("feat_val_", feat_type)),
            paste0(label, hint),
            placeholder = i18n()$t("Enter value")
          )
        }

        # What the selected plots already have for this very feature, right
        # under the field where a new value is about to be typed.
        shiny::tagList(control, existing_note(feat_type))
      })
    })

    # Dynamic UI based on mode and input method
    output$feature_input_ui <- shiny::renderUI({
      shiny::req(operation_mode(), selected_plots())

      mode <- operation_mode()
      method <- input$input_method

      if (method == "upload") {
        return(.render_upload_ui(ns, i18n()))
      }

      if (mode == "new_census") {
        .render_census_form_ui(ns, i18n(), selected_plots())
      } else {
        .render_features_form_ui(ns, i18n(), available_features())
      }
    })

    # Handle form submission for census mode
    shiny::observeEvent(input$submit_census_form, {
      shiny::req(selected_plots(), con())

      plots <- selected_plots()

      # Build census data frame
      census_num <- input$census_number
      year_val <- input$census_year
      month_val <- if (!is.na(input$census_month)) input$census_month else NA_integer_
      day_val <- if (!is.na(input$census_day)) input$census_day else NA_integer_

      data <- data.frame(
        plot_name = plots$plot_name,
        id_liste_plots = plots$id_liste_plots,
        census = rep(census_num, nrow(plots)),
        year = rep(year_val, nrow(plots)),
        month = rep(month_val, nrow(plots)),
        day = rep(day_val, nrow(plots)),
        stringsAsFactors = FALSE
      )

      # Add people fields if provided
      people_fields <- c("team_leader", "principal_investigator",
                         "data_manager", "additional_people")
      people_config <- list()

      for (field in people_fields) {
        val <- input[[field]]
        if (!is.null(val) && nchar(trimws(val)) > 0) {
          data[[field]] <- rep(trimws(val), nrow(plots))
          people_config[[field]] <- TRUE
        }
      }

      config <- list(
        mode = "new_census",
        subplottype_fields = "census",
        col_names_select = c("year", "month", "day", "census"),
        col_names_corresp = c("year", "month", "day", "census"),
        people_columns = names(people_config),
        features_field = if (length(people_config) > 0) names(people_config) else NULL
      )

      prepared_data(data)
      feature_config(config)

      shiny::showNotification(
        i18n()$t("Census data prepared successfully."),
        type = "message", duration = 3
      )
    })

    # Handle form submission for features mode
    shiny::observeEvent(input$submit_features_form, {
      shiny::req(selected_plots(), con(), available_features())

      plots <- selected_plots()
      feat_list <- available_features()

      selected_feat_types <- input$feature_types
      if (is.null(selected_feat_types) || length(selected_feat_types) == 0) {
        shiny::showNotification(
          i18n()$t("Please select at least one feature type."),
          type = "warning"
        )
        return()
      }

      # Build data with one row per plot, columns for each feature
      data <- data.frame(
        plot_name = plots$plot_name,
        id_liste_plots = plots$id_liste_plots,
        stringsAsFactors = FALSE
      )

      # Add year/month/day
      year_val <- input$feat_year
      month_val <- if (!is.null(input$feat_month) && !is.na(input$feat_month)) input$feat_month else NA_integer_
      day_val <- if (!is.null(input$feat_day) && !is.na(input$feat_day)) input$feat_day else NA_integer_

      data$year <- rep(year_val, nrow(plots))
      data$month <- rep(month_val, nrow(plots))
      data$day <- rep(day_val, nrow(plots))

      people_cols <- c()
      regular_cols <- c()

      for (feat_type in selected_feat_types) {
        val <- input[[paste0("feat_val_", feat_type)]]
        if (!is.null(val) && nchar(trimws(as.character(val))) > 0) {
          feat_info <- feat_list[feat_list$type == feat_type, ]
          if (nrow(feat_info) > 0 && feat_info$valuetype[1] == "table_colnam") {
            people_cols <- c(people_cols, feat_type)
          } else {
            regular_cols <- c(regular_cols, feat_type)
          }
          data[[feat_type]] <- rep(trimws(as.character(val)), nrow(plots))
        }
      }

      config <- list(
        mode = "add_features",
        subplottype_fields = c(regular_cols, people_cols),
        col_names_select = c("year", "month", "day"),
        col_names_corresp = c("year", "month", "day"),
        people_columns = people_cols,
        features_field = if (length(people_cols) > 0) people_cols else NULL
      )

      prepared_data(data)
      feature_config(config)

      shiny::showNotification(
        i18n()$t("Feature data prepared successfully."),
        type = "message", duration = 3
      )
    })

    # Handle xlsx upload — store raw data and show mapping UI
    shiny::observeEvent(input$xlsx_file, {
      shiny::req(input$xlsx_file)
      uploaded_raw(NULL)
      prepared_data(NULL)

      tryCatch({
        raw <- as.data.frame(readxl::read_excel(input$xlsx_file$datapath, guess_max = 5000))
        if (nrow(raw) == 0) {
          shiny::showNotification(i18n()$t("Uploaded file is empty."), type = "error")
          return()
        }
        uploaded_raw(raw)
        shiny::showNotification(
          sprintf(i18n()$t("Loaded %d rows — please map columns below."), nrow(raw)),
          type = "message", duration = 5
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
      })
    })

    # Column mapping UI shown after upload
    output$upload_mapping_ui <- shiny::renderUI({
      raw <- uploaded_raw()
      if (is.null(raw)) return(NULL)

      mode      <- operation_mode()
      user_cols <- names(raw)
      none_choice <- c("-- not in file --" = "")
      feat_list <- available_features()

      # ---- Build a lightweight config for map_user_columns ----
      if (mode == "new_census") {
        db_cols <- c("plot_name", "census", "year", "month", "day",
                     "team_leader", "principal_investigator",
                     "data_manager", "additional_people")
      } else {
        feat_choices <- if (!is.null(feat_list)) feat_list$type else character(0)
        db_cols <- c("plot_name", "year", "month", "day", feat_choices)
      }

      # Merge subplot-feature synonyms with general column synonyms
      synonyms <- tryCatch(
        c(.get_subplot_feature_synonyms(), .get_column_synonyms()),
        error = function(e) .get_subplot_feature_synonyms()
      )
      # Add "census" as a synonym target  (census_date → census)
      synonyms$census <- c("census_number", "recensement", "num_census",
                           "census_num", "no_census", "numero_recensement")

      mapping_config <- list(
        direct_columns  = db_cols,
        subplot_features = character(0),
        feature_columns  = character(0),
        import_config    = list(column_synonyms = synonyms)
      )

      # Run auto-mapping (user_col → db_col), then invert to db_col → user_col
      auto_map <- tryCatch(
        map_user_columns(user_data = raw, config = mapping_config,
                         similarity_threshold = 0.6, interactive = FALSE),
        error = function(e) list(mappings = setNames(rep(NA, length(user_cols)), user_cols))
      )

      # Invert: for each db column, which user column mapped to it?
      fwd <- auto_map$mappings                           # user → db (NA if unmatched)
      rev_map <- setNames(names(fwd)[!is.na(fwd)],       # names = db col
                          fwd[!is.na(fwd)])              # values = user col

      # Build expected list based on mode
      if (mode == "new_census") {
        expected <- list(
          list(id = "map_plot_name",              label = i18n()$t("Plot name column *")),
          list(id = "map_census",                 label = i18n()$t("Census number column (optional — auto-detected if absent)")),
          list(id = "map_year",                   label = i18n()$t("Year column")),
          list(id = "map_month",                  label = i18n()$t("Month column")),
          list(id = "map_day",                    label = i18n()$t("Day column")),
          list(id = "map_team_leader",            label = i18n()$t("Team leader column")),
          list(id = "map_principal_investigator", label = i18n()$t("Principal investigator column")),
          list(id = "map_data_manager",           label = i18n()$t("Data manager column")),
          list(id = "map_additional_people",      label = i18n()$t("Additional people column"))
        )
      } else {
        expected <- list(
          list(id = "map_plot_name", label = i18n()$t("Plot name column *")),
          list(id = "map_year",      label = i18n()$t("Year column")),
          list(id = "map_month",     label = i18n()$t("Month column")),
          list(id = "map_day",       label = i18n()$t("Day column"))
        )
      }

      shiny::tagList(
        shiny::hr(),
        shiny::h4(shiny::icon("exchange-alt"), " ", i18n()$t("Map Columns")),
        shiny::p(i18n()$t("Map your file columns to the expected fields. Auto-matched using synonyms and fuzzy matching."),
                 style = "color:#6c757d;"),
        lapply(expected, function(ex) {
          db_col  <- sub("^map_feat_", "", sub("^map_", "", ex$id))
          guessed <- if (db_col %in% names(rev_map)) rev_map[[db_col]] else ""
          shiny::fluidRow(
            shiny::column(12,
              shiny::selectInput(
                ns(ex$id),
                label = ex$label,
                choices = c(none_choice, user_cols),
                selected = guessed,
                multiple = FALSE
              )
            )
          )
        }),
        if (mode != "new_census") shiny::tagList(
          shiny::hr(),
          shiny::h4(shiny::icon("tags"), " ", i18n()$t("Map Data Columns to Feature Types")),
          shiny::p(
            i18n()$t("For each remaining column in your file, choose the feature type it contains, or skip."),
            style = "color:#6c757d;"
          ),
          shiny::uiOutput(ns("feat_col_detail_ui"))
        ) else NULL,
        shiny::div(
          style = "margin-top: 15px;",
          shiny::actionButton(
            ns("apply_mapping"),
            shiny::tagList(shiny::icon("check"), " ", i18n()$t("Apply Mapping & Preview")),
            class = "btn-success"
          )
        )
      )
    })

    # Per-xlsx-column feature type mapping (add_features mode only)
    output$feat_col_detail_ui <- shiny::renderUI({
      raw  <- uploaded_raw()
      mode <- operation_mode()
      if (is.null(raw) || mode == "new_census") return(NULL)

      feat_list <- available_features()
      user_cols <- names(raw)

      # Build grouped choices: DB feature types by category + special options
      if (!is.null(feat_list) && nrow(feat_list) > 0) {
        categories <- unique(feat_list$category)
        feat_choices_grouped <- lapply(categories, function(cat) {
          types_in_cat <- feat_list$type[feat_list$category == cat]
          setNames(types_in_cat, types_in_cat)
        })
        names(feat_choices_grouped) <- categories
        all_choices <- c(
          list("--- Skip ---" = ""),
          feat_choices_grouped
        )
      } else {
        all_choices <- c("--- Skip ---" = "")
      }

      # Synonyms for auto-matching
      synonyms <- tryCatch(
        c(.get_subplot_feature_synonyms(), .get_column_synonyms()),
        error = function(e) .get_subplot_feature_synonyms()
      )

      lapply(user_cols, function(orig_col) {
        safe_col  <- gsub("[^a-zA-Z0-9]", "_", orig_col)
        col_lower <- tolower(orig_col)

        # Auto-match: direct name or synonym lookup
        auto_feat <- ""
        if (!is.null(feat_list) && nrow(feat_list) > 0) {
          direct <- feat_list$type[tolower(feat_list$type) == col_lower]
          if (length(direct) > 0) {
            auto_feat <- direct[1]
          } else {
            for (feat_type in feat_list$type) {
              syns <- synonyms[[feat_type]]
              if (!is.null(syns) && col_lower %in% tolower(syns)) {
                auto_feat <- feat_type
                break
              }
            }
          }
        }

        sample_vals <- head(raw[[orig_col]][!is.na(raw[[orig_col]])], 3)
        sample_str  <- if (length(sample_vals) > 0) paste(sample_vals, collapse = ", ") else i18n()$t("(empty)")

        shiny::fluidRow(
          shiny::column(5,
            shiny::div(
              style = "padding: 8px; background: #f8f9fa; border-radius: 4px; margin-bottom: 5px;",
              shiny::strong(orig_col),
              shiny::br(),
              shiny::span(style = "color:#6c757d; font-size:0.85em;",
                          i18n()$t("Sample:"), " ", sample_str)
            )
          ),
          shiny::column(7,
            shiny::selectizeInput(
              ns(paste0("col_feat_", safe_col)),
              label   = NULL,
              choices = all_choices,
              selected = auto_feat,
              multiple = FALSE,
              options  = list(placeholder = i18n()$t("Skip / choose feature type..."))
            )
          )
        )
      })
    })

    # Apply mapping and build prepared data
    shiny::observeEvent(input$apply_mapping, {
      shiny::req(uploaded_raw(), selected_plots(), operation_mode())

      raw   <- uploaded_raw()
      plots <- selected_plots()
      mode  <- operation_mode()

      tryCatch({
        # Resolve plot_name column
        plot_col <- input$map_plot_name
        if (is.null(plot_col) || plot_col == "") {
          shiny::showNotification(i18n()$t("Please map the plot name column."), type = "error")
          return()
        }
        df <- raw
        if (plot_col != "plot_name") {
          names(df)[names(df) == plot_col] <- "plot_name"
        }

        # Filter to selected plots and add id_liste_plots
        df <- df %>%
          dplyr::filter(plot_name %in% plots$plot_name) %>%
          dplyr::left_join(plots %>% dplyr::select(plot_name, id_liste_plots, last_census), by = "plot_name")

        if (nrow(df) == 0) {
          shiny::showNotification(i18n()$t("No matching plot names found in uploaded file."), type = "error")
          return()
        }

        # Rename date columns (user-mapped)
        for (date_col in c("year", "month", "day")) {
          mapped <- input[[paste0("map_", date_col)]]
          if (!is.null(mapped) && mapped != "" && mapped != date_col && mapped %in% names(df)) {
            names(df)[names(df) == mapped] <- date_col
          }
        }

        feat_list    <- available_features()
        people_types <- if (!is.null(feat_list)) feat_list$type[feat_list$valuetype == "table_colnam"] else character(0)

        if (mode == "new_census") {
          # Rename people columns
          people_cols <- c()
          for (pfield in c("team_leader", "principal_investigator", "data_manager", "additional_people")) {
            mapped <- input[[paste0("map_", pfield)]]
            if (!is.null(mapped) && mapped != "") {
              if (mapped != pfield && mapped %in% names(df)) names(df)[names(df) == mapped] <- pfield
              people_cols <- c(people_cols, pfield)
            }
          }

          # Resolve census column — rename if mapped, else auto-detect
          census_mapped <- input$map_census
          if (!is.null(census_mapped) && census_mapped != "") {
            if (census_mapped != "census" && census_mapped %in% names(df)) {
              names(df)[names(df) == census_mapped] <- "census"
            }
          } else {
            # Auto-detect: next census per plot from last_census
            df$census <- ifelse(
              !is.na(df$last_census) & is.finite(df$last_census),
              as.integer(df$last_census) + 1L,
              1L
            )
            shiny::showNotification(
              i18n()$t("Census number auto-detected as next census for each plot."),
              type = "message", duration = 5
            )
          }

          # Drop helper column
          df$last_census <- NULL

          config <- list(
            mode = "new_census",
            subplottype_fields = "census",
            col_names_select = intersect(c("year", "month", "day", "census"), names(df)),
            col_names_corresp = intersect(c("year", "month", "day", "census"), names(df)),
            people_columns = people_cols,
            features_field = if (length(people_cols) > 0) people_cols else NULL
          )

        } else {
          # add_features mode: iterate over xlsx columns, read col_feat_* inputs
          df$last_census <- NULL
          people_cols  <- c()
          feature_cols <- c()

          # Key columns already remapped above — skip them
          key_cols_used <- c(plot_col, input$map_year, input$map_month, input$map_day)
          key_cols_used <- key_cols_used[!is.null(key_cols_used) & nchar(trimws(key_cols_used)) > 0]

          for (orig_col in names(raw)) {
            if (orig_col %in% key_cols_used) next
            safe_col <- gsub("[^a-zA-Z0-9]", "_", orig_col)
            ft <- input[[paste0("col_feat_", safe_col)]]
            if (is.null(ft) || ft == "") next
            if (orig_col %in% names(df) && orig_col != ft) {
              names(df)[names(df) == orig_col] <- ft
            }
            if (ft %in% people_types) people_cols <- c(people_cols, ft)
            else feature_cols <- c(feature_cols, ft)
          }

          if (length(c(feature_cols, people_cols)) == 0) {
            shiny::showNotification(
              i18n()$t("No feature columns mapped. Please assign at least one column to a feature type."),
              type = "error"
            )
            return()
          }

          date_cols_present <- intersect(c("year", "month", "day"), names(df))
          config <- list(
            mode = "add_features",
            subplottype_fields = c(feature_cols, people_cols),
            col_names_select = date_cols_present,
            col_names_corresp = date_cols_present,
            people_columns = people_cols,
            features_field = NULL
          )

          # Drop unmapped xlsx columns — keep only what will actually be used
          keep_cols <- c("plot_name", "id_liste_plots", date_cols_present,
                         feature_cols, people_cols)
          df <- df[, intersect(keep_cols, names(df)), drop = FALSE]
          cli::cli_alert_info("Upload add_features: keeping cols [{paste(names(df), collapse=', ')}]")
        }

        prepared_data(df)
        feature_config(config)
        shiny::showNotification(
          sprintf(i18n()$t("%d rows ready after mapping."), nrow(df)),
          type = "message", duration = 4
        )

      }, error = function(e) {
        shiny::showNotification(paste("Error applying mapping:", e$message), type = "error", duration = 10)
      })
    })

    # Auto-detect next census number
    next_census <- shiny::reactive({
      shiny::req(selected_plots())
      plots <- selected_plots()
      if ("n_census" %in% names(plots) && any(!is.na(plots$n_census))) {
        max_census <- max(plots$last_census, na.rm = TRUE)
        if (is.finite(max_census)) return(as.integer(max_census + 1))
      }
      return(1L)
    })

    # Data preview
    output$data_preview_message <- shiny::renderUI({
      d <- prepared_data()
      if (is.null(d)) {
        return(shiny::div(
          class = "alert alert-secondary",
          shiny::icon("info-circle"), " ",
          i18n()$t("No data prepared yet. Fill the form or upload a file above.")
        ))
      }
      shiny::div(
        class = "alert alert-info",
        shiny::icon("table"), " ",
        sprintf(i18n()$t("%d rows, %d columns ready"), nrow(d), ncol(d))
      )
    })

    output$data_preview_table <- DT::renderDT({
      d <- prepared_data()
      shiny::req(d)

      # Show without internal id column
      display <- d %>% dplyr::select(-dplyr::any_of("id_liste_plots"))

      DT::datatable(
        display,
        options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        class = "display cell-border stripe"
      )
    })

    # Return
    return(shiny::reactive({
      d <- prepared_data()
      cfg <- feature_config()
      if (is.null(d) || is.null(cfg)) return(NULL)
      list(data = d, config = cfg)
    }))
  })
}


# --- Helper UI functions (internal) ---

#' @keywords internal
.render_census_form_ui <- function(ns, i18n, plots) {
  # Compute next census
  next_census <- 1L
  if ("last_census" %in% names(plots) && any(!is.na(plots$last_census))) {
    max_c <- max(plots$last_census, na.rm = TRUE)
    if (is.finite(max_c)) next_census <- as.integer(max_c + 1)
  }

  shiny::tagList(
    shiny::div(
      class = "alert alert-info",
      shiny::icon("info-circle"), " ",
      sprintf(i18n$t("Adding census data for %d plot(s). Auto-detected next census number: %d"),
              nrow(plots), next_census)
    ),

    shiny::h4(i18n$t("Census Details"), style = "margin-bottom: 15px;"),

    shiny::fluidRow(
      shiny::column(3, shiny::numericInput(
        ns("census_number"), i18n$t("Census Number"),
        value = next_census, min = 1, step = 1
      )),
      shiny::column(3, shiny::numericInput(
        ns("census_year"), i18n$t("Year *"),
        value = as.integer(format(Sys.Date(), "%Y")),
        min = 1900, max = 2100, step = 1
      )),
      shiny::column(3, shiny::numericInput(
        ns("census_month"), i18n$t("Month"),
        value = NA, min = 1, max = 12, step = 1
      )),
      shiny::column(3, shiny::numericInput(
        ns("census_day"), i18n$t("Day"),
        value = NA, min = 1, max = 31, step = 1
      ))
    ),

    shiny::hr(),
    shiny::h4(i18n$t("Team Members"), style = "margin-bottom: 15px;"),

    shiny::fluidRow(
      shiny::column(6, shiny::textInput(
        ns("team_leader"), i18n$t("Team Leader"),
        placeholder = i18n$t("e.g., John Doe")
      )),
      shiny::column(6, shiny::textInput(
        ns("principal_investigator"), i18n$t("Principal Investigator"),
        placeholder = i18n$t("e.g., Jane Smith")
      ))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::textInput(
        ns("data_manager"), i18n$t("Data Manager"),
        placeholder = i18n$t("e.g., Bob Jones")
      )),
      shiny::column(6, shiny::textInput(
        ns("additional_people"), i18n$t("Additional People"),
        placeholder = i18n$t("Comma-separated names")
      ))
    ),

    shiny::div(
      style = "text-align: center; margin-top: 20px;",
      shiny::actionButton(
        ns("submit_census_form"),
        shiny::tagList(shiny::icon("check"), " ", i18n$t("Prepare Census Data")),
        class = "btn-primary btn-lg"
      )
    )
  )
}


#' @keywords internal
.render_features_form_ui <- function(ns, i18n, feat_list) {
  if (is.null(feat_list) || nrow(feat_list) == 0) {
    return(shiny::div(
      class = "alert alert-warning",
      i18n$t("Could not load feature types from database.")
    ))
  }

  # Build choices with category grouping
  choices <- setNames(feat_list$type, paste0(feat_list$type, " (", feat_list$category, ")"))

  shiny::tagList(
    shiny::h4(i18n$t("Select Feature Types"), style = "margin-bottom: 15px;"),

    shiny::selectizeInput(
      ns("feature_types"),
      i18n$t("Feature Types"),
      choices = choices,
      multiple = TRUE,
      options = list(placeholder = i18n$t("Select feature types..."))
    ),

    shiny::hr(),

    shiny::h4(i18n$t("Date Information"), style = "margin-bottom: 15px;"),
    shiny::fluidRow(
      shiny::column(4, shiny::numericInput(
        ns("feat_year"), i18n$t("Year"),
        value = as.integer(format(Sys.Date(), "%Y")),
        min = 1900, max = 2100, step = 1
      )),
      shiny::column(4, shiny::numericInput(
        ns("feat_month"), i18n$t("Month"),
        value = NA, min = 1, max = 12, step = 1
      )),
      shiny::column(4, shiny::numericInput(
        ns("feat_day"), i18n$t("Day"),
        value = NA, min = 1, max = 31, step = 1
      ))
    ),

    shiny::hr(),

    # Dynamic value inputs for selected features
    shiny::h4(i18n$t("Feature Values"), style = "margin-bottom: 15px;"),
    shiny::uiOutput(ns("dynamic_feature_inputs")),

    shiny::div(
      style = "text-align: center; margin-top: 20px;",
      shiny::actionButton(
        ns("submit_features_form"),
        shiny::tagList(shiny::icon("check"), " ", i18n$t("Prepare Feature Data")),
        class = "btn-primary btn-lg"
      )
    )
  )
}


#' @keywords internal
.render_upload_ui <- function(ns, i18n) {
  shiny::tagList(
    shiny::div(
      class = "alert alert-info",
      shiny::icon("info-circle"), " ",
      i18n$t("Upload an xlsx file. You will map columns to expected fields after upload. A 'plot_name' column (or equivalent) is required; census number is optional and will be auto-detected if absent.")
    ),

    shiny::fileInput(
      ns("xlsx_file"),
      i18n$t("Choose xlsx file"),
      accept = c(".xlsx", ".xls")
    ),

    # Column mapping UI — appears after upload
    shiny::uiOutput(ns("upload_mapping_ui"))
  )
}
