# Feature Wizard - Step 6: Execute Import
#
# Module for executing the import with dry-run support.

#' Feature Wizard Step 6: Import - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step6_import_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("step6_header")),

    # Action buttons
    shiny::uiOutput(ns("step6_buttons")),

    shiny::hr(),

    # Results
    shiny::uiOutput(ns("import_results"))
  )
}


#' Feature Wizard Step 6: Import - Server
#'
#' @param id Module namespace ID
#' @param matched_data Reactive containing the matched feature data
#' @param feature_config Reactive containing the feature configuration
#' @param selected_plots Reactive containing selected plots data
#' @param operation_mode Reactive containing operation mode string
#' @param validation_result Reactive containing validation result
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive containing import result
#' @keywords internal
#' @export
mod_feat_step6_import_server <- function(id, matched_data, feature_config, selected_plots,
                                          operation_mode, validation_result, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    import_result <- shiny::reactiveVal(NULL)

    # Dynamic header and buttons based on operation mode
    is_update_mode <- shiny::reactive({
      mode <- tryCatch(operation_mode(), error = function(e) NULL)
      mode %in% c("define_multi_stems", "compute_stem_status", "standardize_observations")
    })

    output$step6_header <- shiny::renderUI({
      if (is_update_mode()) {
        shiny::tagList(
          shiny::h3(
            shiny::icon("sync-alt"),
            i18n()$t("Step 6: Execute Update"),
            style = "color: #495057; margin-bottom: 20px;"
          ),
          shiny::p(
            i18n()$t("Run a dry-run first to preview the changes, then execute the update."),
            style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
          )
        )
      } else {
        shiny::tagList(
          shiny::h3(
            shiny::icon("cloud-upload-alt"),
            i18n()$t("Step 6: Execute Import"),
            style = "color: #495057; margin-bottom: 20px;"
          ),
          shiny::p(
            i18n()$t("Run a dry-run first to see what will be inserted, then execute the import."),
            style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
          )
        )
      }
    })

    output$step6_buttons <- shiny::renderUI({
      update_mode <- is_update_mode()
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::div(
            style = "text-align: center;",
            shiny::actionButton(
              ns("dry_run"),
              shiny::tagList(shiny::icon("eye"), " ", i18n()$t("Dry Run")),
              class = "btn-info btn-lg",
              style = "width: 100%;"
            ),
            shiny::p(
              if (update_mode) i18n()$t("Simulate the update without making changes.")
              else i18n()$t("Simulate the import without making changes."),
              style = "color: #6c757d; font-size: 13px; margin-top: 5px;"
            )
          )
        ),
        shiny::column(
          6,
          shiny::div(
            style = "text-align: center;",
            shiny::actionButton(
              ns("live_import"),
              shiny::tagList(
                shiny::icon("database"), " ",
                if (update_mode) i18n()$t("Update Database")
                else i18n()$t("Import to Database")
              ),
              class = "btn-success btn-lg",
              style = "width: 100%;"
            ),
            shiny::p(
              if (update_mode) i18n()$t("Execute the update for real. Changes will be saved.")
              else i18n()$t("Execute the import for real. Changes will be saved."),
              style = "color: #6c757d; font-size: 13px; margin-top: 5px;"
            )
          )
        )
      )
    })

    # Dry run
    shiny::observeEvent(input$dry_run, {
      shiny::req(matched_data(), feature_config(), con())

      n_rows <- nrow(matched_data())
      notify_id <- shiny::showNotification(
        shiny::tagList(
          shiny::icon("spinner", class = "fa-spin"), " ",
          sprintf(i18n()$t("Preparing dry run for %d row(s)..."), n_rows)
        ),
        duration = NULL, type = "message", closeButton = FALSE
      )
      on.exit(shiny::removeNotification(notify_id), add = TRUE)

      result <- .execute_feature_import(
        data = matched_data(),
        config = feature_config(),
        con = con(),
        dry_run = TRUE,
        i18n = i18n()
      )

      import_result(result)
    })

    # Live import with confirmation
    shiny::observeEvent(input$live_import, {
      shiny::req(matched_data(), feature_config(), con())

      update_mode <- is_update_mode()
      shiny::showModal(shiny::modalDialog(
        title = if (update_mode) i18n()$t("Confirm Update") else i18n()$t("Confirm Import"),
        shiny::p(
          shiny::icon("exclamation-triangle", style = "color: #ffc107;"), " ",
          if (update_mode) {
            i18n()$t("This will update records in the database. This action cannot be easily undone.")
          } else {
            i18n()$t("This will insert data into the database. This action cannot be easily undone.")
          }
        ),
        shiny::p(
          shiny::strong(sprintf(
            if (update_mode) i18n()$t("Records to update: %d") else i18n()$t("Rows to import: %d"),
            nrow(matched_data())
          ))
        ),
        footer = shiny::tagList(
          shiny::modalButton(i18n()$t("Cancel")),
          shiny::actionButton(ns("confirm_import"),
            if (update_mode) i18n()$t("Confirm Update") else i18n()$t("Confirm Import"),
            class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$confirm_import, {
      shiny::removeModal()

      n_rows <- nrow(matched_data())
      mode  <- tryCatch(operation_mode(), error = function(e) NULL)

      label <- if (identical(mode, "define_multi_stems")) {
        sprintf(i18n()$t("Updating %d record(s) in database — please wait..."), n_rows)
      } else if (identical(mode, "compute_stem_status")) {
        sprintf(i18n()$t("Writing stem_status for %d row(s) — please wait..."), n_rows)
      } else if (identical(mode, "standardize_observations")) {
        sprintf(i18n()$t("Writing %d standardized observation row(s) — please wait..."), n_rows)
      } else if (identical(mode, "add_measurements")) {
        sprintf(i18n()$t("Inserting %d measurement(s) into database — please wait..."), n_rows)
      } else {
        sprintf(i18n()$t("Inserting %d record(s) into database — please wait..."), n_rows)
      }

      notify_id <- shiny::showNotification(
        shiny::tagList(
          shiny::icon("spinner", class = "fa-spin"), " ",
          shiny::strong(label)
        ),
        duration = NULL, type = "message", closeButton = FALSE
      )
      on.exit(shiny::removeNotification(notify_id), add = TRUE)

      result <- .execute_feature_import(
        data = matched_data(),
        config = feature_config(),
        con = con(),
        dry_run = FALSE,
        i18n = i18n()
      )

      import_result(result)
    })

    # Results display
    output$import_results <- shiny::renderUI({
      res <- import_result()
      if (is.null(res)) return(NULL)

      mode <- tryCatch(operation_mode(), error = function(e) NULL)
      update_mode <- mode %in% c("define_multi_stems", "compute_stem_status")

      if (res$dry_run) {
        # Context-aware label for record count
        record_label <- if (identical(mode, "define_multi_stems")) {
          i18n()$t("Records to update: %d")
        } else if (identical(mode, "compute_stem_status")) {
          i18n()$t("stem_status records to write: %d")
        } else if (identical(mode, "standardize_observations")) {
          i18n()$t("Standardized observation records to write: %d")
        } else if (identical(mode, "add_measurements")) {
          i18n()$t("Measurement records to create: %d")
        } else {
          i18n()$t("Subplot feature records to create: %d")
        }

        shiny::tagList(
          shiny::div(
            class = "alert alert-info",
            style = "margin-top: 20px;",
            shiny::icon("info-circle"), " ",
            shiny::strong(i18n()$t("Dry Run Results")),
            shiny::tags$ul(
              shiny::tags$li(sprintf(record_label, res$n_subplot_records)),
              if (!update_mode && !is.null(res$n_people_records) && res$n_people_records > 0) {
                shiny::tags$li(sprintf(
                  i18n()$t("People feature records to create: %d"),
                  res$n_people_records
                ))
              }
            )
          ),
          if (!is.null(res$preview)) {
            shiny::tagList(
              shiny::h5(i18n()$t("Preview of records"), style = "margin-top: 15px;"),
              DT::DTOutput(ns("dry_run_preview"))
            )
          }
        )
      } else if (res$success) {
        shiny::div(
          class = "alert alert-success",
          style = "margin-top: 20px;",
          shiny::icon("check-circle"), " ",
          shiny::strong(
            if (update_mode) i18n()$t("Update Successful!")
            else i18n()$t("Import Successful!")
          ),
          shiny::p(res$message)
        )
      } else {
        shiny::div(
          class = "alert alert-danger",
          style = "margin-top: 20px;",
          shiny::icon("exclamation-circle"), " ",
          shiny::strong(
            if (update_mode) i18n()$t("Update Failed")
            else i18n()$t("Import Failed")
          ),
          shiny::p(res$message)
        )
      }
    })

    output$dry_run_preview <- DT::renderDT({
      res <- import_result()
      shiny::req(res, res$dry_run, !is.null(res$preview))

      DT::datatable(
        res$preview,
        options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE, class = "display cell-border stripe"
      )
    })

    return(shiny::reactive(import_result()))
  })
}


#' Execute feature import (internal)
#'
#' @param data Data frame of features to import
#' @param config Feature configuration list
#' @param con Database connection
#' @param dry_run Logical, if TRUE only simulate
#' @param i18n Translator object
#' @return List with import results
#' @keywords internal
.execute_feature_import <- function(data, config, con, dry_run = TRUE, i18n = NULL) {

  t <- function(x) if (!is.null(i18n)) i18n$t(x) else x

  tryCatch({
    mode <- config$mode
    people_cols <- config$people_columns
    subplottype_fields <- config$subplottype_fields

    # Separate base data from people columns for the main add_subplot_features call
    base_col_select <- config$col_names_select
    base_col_corresp <- config$col_names_corresp

    # ---- Measurements mode ----
    if (mode == "add_measurements") {
      return(.execute_measurements_import(data, config, con, dry_run, i18n))
    }

    # ---- Multi-stems mode ----
    if (mode == "define_multi_stems") {
      return(.execute_multi_stems_import(data, con, dry_run, i18n))
    }

    # ---- Compute stem status mode ----
    if (mode == "compute_stem_status") {
      return(.execute_stem_status_import(data, config, con, dry_run, i18n))
    }

    # ---- Standardize observations mode ----
    if (mode == "standardize_observations") {
      return(.execute_standardize_observations_import(data, config, con, dry_run, i18n))
    }

    # For census mode: subplottype_field is "census", and people are features
    # For features mode: each selected feature type is a subplottype_field

    if (mode == "new_census") {
      # Census mode: add census subplot record with optional people features

      # Ensure census column exists in col_names
      if (!"census" %in% base_col_select) {
        base_col_select <- c(base_col_select, "census")
        base_col_corresp <- c(base_col_corresp, "census")
      }

      # Count records
      n_subplot <- nrow(data)
      n_people <- 0

      if (!is.null(people_cols) && length(people_cols) > 0) {
        for (col in people_cols) {
          if (col %in% names(data)) {
            non_na <- sum(!is.na(data[[col]]) & trimws(as.character(data[[col]])) != "")
            # Each comma-separated ID counts as a separate record
            for (i in seq_len(nrow(data))) {
              val <- data[[col]][i]
              if (!is.na(val) && trimws(as.character(val)) != "") {
                ids <- trimws(strsplit(as.character(val), ",")[[1]])
                n_people <- n_people + length(ids[ids != ""])
              }
            }
          }
        }
      }

      if (dry_run) {
        # Build preview
        preview <- data %>%
          dplyr::select(
            dplyr::any_of(c("plot_name", "census", "year", "month", "day")),
            dplyr::any_of(people_cols)
          )

        return(list(
          dry_run = TRUE,
          success = TRUE,
          n_subplot_records = n_subplot,
          n_people_records = n_people,
          preview = preview,
          message = sprintf(t("Dry run: %d census record(s) and %d people record(s) would be created."),
                            n_subplot, n_people)
        ))
      }

      # Live import: Step 1 - Add census records
      cli::cli_alert_info("Adding census records for {nrow(data)} plot(s)...")

      census_basic <- data[, c("plot_name", "id_liste_plots",
                                intersect(c("census", "year", "month", "day"), names(data))),
                           drop = FALSE]

      add_subplot_features(
        new_data = census_basic,
        col_names_select = base_col_select,
        col_names_corresp = base_col_corresp,
        id_plot_name = "id_liste_plots",
        id_plot_name_corresp = "id_table_liste_plots_n",
        subplottype_field = "census",
        features_field = NULL,
        add_data = TRUE,
        ask_before_update = FALSE,
        interactive = FALSE,
        verbose = TRUE,
        check_existing_data = FALSE,
        con = con
      )

      cli::cli_alert_success("Census records created")

      # Step 2: Add people features to newly created census records
      people_added <- 0
      if (!is.null(people_cols) && length(people_cols) > 0) {
        people_added <- .add_people_to_census(data, people_cols, con)
      }

      return(list(
        dry_run = FALSE,
        success = TRUE,
        n_subplot_records = n_subplot,
        n_people_records = people_added,
        message = sprintf(
          t("Successfully added %d census record(s) and %d people feature record(s)."),
          n_subplot, people_added
        )
      ))

    } else {
      # Features mode: add each feature type as a subplot feature

      # Count records per feature type
      n_subplot <- nrow(data) * length(subplottype_fields)
      n_people <- 0

      if (!is.null(people_cols) && length(people_cols) > 0) {
        for (col in people_cols) {
          if (col %in% names(data)) {
            for (i in seq_len(nrow(data))) {
              val <- data[[col]][i]
              if (!is.na(val) && trimws(as.character(val)) != "") {
                n_people <- n_people + 1
              }
            }
          }
        }
      }

      if (dry_run) {
        preview <- data %>%
          dplyr::select(
            dplyr::any_of(c("plot_name", "year", "month", "day")),
            dplyr::any_of(subplottype_fields),
            dplyr::any_of(people_cols)
          )

        return(list(
          dry_run = TRUE,
          success = TRUE,
          n_subplot_records = n_subplot,
          n_people_records = n_people,
          preview = preview,
          message = sprintf(t("Dry run: %d feature record(s) would be created."),
                            n_subplot + n_people)
        ))
      }

      # Live import: separate regular features from people features
      regular_fields <- setdiff(subplottype_fields, people_cols)
      total_added <- 0

      # Add regular features
      if (length(regular_fields) > 0) {
        for (feat_type in regular_fields) {
          if (!feat_type %in% names(data)) next

          feat_data <- data[, c("plot_name", "id_liste_plots", "year", "month", "day", feat_type),
                            drop = FALSE]
          feat_data <- feat_data[!is.na(feat_data[[feat_type]]), , drop = FALSE]

          if (nrow(feat_data) == 0) next

          cli::cli_alert_info("Adding {feat_type} feature for {nrow(feat_data)} plot(s)...")

          add_subplot_features(
            new_data = feat_data,
            col_names_select = c("year", "month", "day"),
            col_names_corresp = c("year", "month", "day"),
            id_plot_name = "id_liste_plots",
            id_plot_name_corresp = "id_table_liste_plots_n",
            subplottype_field = feat_type,
            features_field = NULL,
            add_data = TRUE,
            ask_before_update = FALSE,
            interactive = FALSE,
            verbose = TRUE,
            check_existing_data = FALSE,
            con = con
          )

          total_added <- total_added + nrow(feat_data)
        }
      }

      # Add people features (need special handling via data_subplot_feat)
      if (!is.null(people_cols) && length(people_cols) > 0) {
        for (col in people_cols) {
          if (!col %in% names(data)) next

          feat_data <- data[, c("plot_name", "id_liste_plots", "year", "month", "day", col),
                            drop = FALSE]
          feat_data <- feat_data[!is.na(feat_data[[col]]), , drop = FALSE]

          if (nrow(feat_data) == 0) next

          # For people features, we need to use add_subplot_features with features_field
          # But the values are already IDs at this point
          # Create a dummy subplot record, then add the person ID as feature
          cli::cli_alert_info("Adding {col} feature for {nrow(feat_data)} plot(s)...")

          # Get subplotype info
          subplot_info <- subplot_list(con)
          subplotype_id <- subplot_info$id_subplotype[subplot_info$type == col]

          if (length(subplotype_id) == 0) {
            cli::cli_alert_warning("Could not find subplotype for {col}")
            next
          }

          for (i in seq_len(nrow(feat_data))) {
            person_ids <- trimws(strsplit(as.character(feat_data[[col]][i]), ",")[[1]])
            person_ids <- person_ids[person_ids != "" & !is.na(person_ids)]

            for (pid in person_ids) {
              feat_record <- data.frame(
                typevalue = as.numeric(pid),
                typevalue_char = NA_character_,
                id_table_liste_plots = as.integer(feat_data$id_liste_plots[i]),
                id_type_sub_plot = as.integer(subplotype_id),
                year = as.integer(feat_data$year[i]),
                month = as.integer(feat_data$month[i]),
                day = as.integer(feat_data$day[i]),
                date_modif_d = as.integer(format(Sys.Date(), "%d")),
                date_modif_m = as.integer(format(Sys.Date(), "%m")),
                date_modif_y = as.integer(format(Sys.Date(), "%Y")),
                stringsAsFactors = FALSE
              )

              DBI::dbAppendTable(con, "data_liste_sub_plots", feat_record)
              total_added <- total_added + 1
            }
          }
        }
      }

      return(list(
        dry_run = FALSE,
        success = TRUE,
        n_subplot_records = total_added,
        n_people_records = 0,
        message = sprintf(t("Successfully added %d feature record(s)."), total_added)
      ))
    }

  }, error = function(e) {
    cli::cli_alert_danger("Import failed: {e$message}")
    return(list(
      dry_run = dry_run,
      success = FALSE,
      n_subplot_records = 0,
      n_people_records = 0,
      message = paste("Error:", e$message)
    ))
  })
}


#' Add people features to newly created census records
#' @keywords internal
.add_people_to_census <- function(data, people_cols, con) {
  records_added <- 0

  tryCatch({
    # Query the newly created census records
    plot_ids <- unique(data$id_liste_plots[!is.na(data$id_liste_plots)])

    # Get census number and year to identify the correct census records
    census_nums <- unique(data$census[!is.na(data$census)])
    years <- unique(data$year[!is.na(data$year)])

    census_query <- sprintf(
      "SELECT sp.id_sub_plots, sp.id_table_liste_plots, sp.typevalue, sp.year
       FROM data_liste_sub_plots sp
       JOIN subplotype_list spl ON sp.id_type_sub_plot = spl.id_subplotype
       WHERE spl.type = 'census'
         AND sp.id_table_liste_plots IN (%s)
         AND sp.typevalue IN (%s)",
      paste(plot_ids, collapse = ","),
      paste(census_nums, collapse = ",")
    )

    if (length(years) > 0 && !all(is.na(years))) {
      census_query <- paste0(census_query,
        sprintf(" AND sp.year IN (%s)", paste(years[!is.na(years)], collapse = ",")))
    }

    census_records <- DBI::dbGetQuery(con, census_query)

    if (nrow(census_records) == 0) {
      cli::cli_alert_warning("No census records found to attach people features to")
      return(0)
    }

    # Get subplotype IDs for people features
    subplot_info <- subplot_list(con)
    people_lookup <- subplot_info[
      !is.na(subplot_info$valuetype) & subplot_info$valuetype == "table_colnam",
      c("type", "id_subplotype")
    ]

    for (feat_type in people_cols) {
      subplotype_id <- people_lookup$id_subplotype[people_lookup$type == feat_type]
      if (length(subplotype_id) == 0) next

      for (i in seq_len(nrow(data))) {
        plot_id <- data$id_liste_plots[i]
        person_val <- data[[feat_type]][i]

        if (is.na(person_val) || trimws(as.character(person_val)) == "") next

        # Find matching census record
        census_row <- census_records[census_records$id_table_liste_plots == plot_id, ]
        if (nrow(census_row) == 0) next

        subplot_id <- census_row$id_sub_plots[1]

        # Handle multiple IDs (comma-separated)
        person_ids <- trimws(strsplit(as.character(person_val), ",")[[1]])
        person_ids <- person_ids[person_ids != "" & !is.na(person_ids)]

        for (pid in person_ids) {
          feat_record <- data.frame(
            typevalue = as.numeric(pid),
            typevalue_char = NA_character_,
            id_sub_plots = as.integer(subplot_id),
            id_type_sub_plot = as.integer(subplotype_id),
            date_modif_d = as.integer(format(Sys.Date(), "%d")),
            date_modif_m = as.integer(format(Sys.Date(), "%m")),
            date_modif_y = as.integer(format(Sys.Date(), "%Y")),
            stringsAsFactors = FALSE
          )

          DBI::dbAppendTable(con, "data_subplot_feat", feat_record)
          records_added <- records_added + 1
        }
      }
    }

    cli::cli_alert_success("Added {records_added} people feature record(s)")

  }, error = function(e) {
    cli::cli_alert_warning("Error adding people features: {e$message}")
  })

  records_added
}


#' Execute individual measurements import
#'
#' @param data Data frame with measurement rows (plot_name, tag, traitid, traitvalue, traitvalue_char, id_liste_plots, id_sub_plots)
#' @param config Measurement configuration list
#' @param con Database connection
#' @param dry_run Logical
#' @param i18n Translator object
#' @return List with import results
#' @keywords internal
.execute_measurements_import <- function(data, config, con, dry_run = TRUE, i18n = NULL) {

  t <- function(x) if (!is.null(i18n)) i18n$t(x) else x

  # Resolve individuals: match plot_name + tag to id_data_individuals
  plot_ids <- unique(data$id_liste_plots[!is.na(data$id_liste_plots)])

  existing_inds <- DBI::dbGetQuery(con, sprintf(
    "SELECT di.tag, di.id_table_liste_plots_n, di.id_n, di.id_diconame_n, di.id_specimen
     FROM data_individuals di
     WHERE di.id_table_liste_plots_n IN (%s)",
    paste(plot_ids, collapse = ",")
  ))

  # Join to get individual IDs
  data$id_data_individuals <- NA_integer_
  data$id_diconame <- NA_integer_
  data$id_specimen_ind <- NA_integer_

  for (i in seq_len(nrow(data))) {
    tag_val <- data$tag[i]
    plot_id <- data$id_liste_plots[i]
    if (is.na(tag_val) || is.na(plot_id)) next

    match_rows <- existing_inds[
      as.character(existing_inds$tag) == as.character(tag_val) &
        existing_inds$id_table_liste_plots_n == plot_id, ]

    if (nrow(match_rows) > 0) {
      data$id_data_individuals[i] <- match_rows$id_n[1]
      data$id_diconame[i] <- match_rows$id_diconame_n[1]
      data$id_specimen_ind[i] <- match_rows$id_specimen[1]
    }
  }

  # Filter out unmatched individuals
  matched <- data[!is.na(data$id_data_individuals), , drop = FALSE]
  n_unmatched <- sum(is.na(data$id_data_individuals))
  n_total <- nrow(matched)

  if (n_total == 0) {
    return(list(
      dry_run = dry_run,
      success = FALSE,
      n_subplot_records = 0,
      n_people_records = 0,
      message = t("No measurements could be matched to existing individuals.")
    ))
  }

  if (dry_run) {
    preview <- matched[, intersect(
      c("plot_name", "tag", "trait_name", "traitvalue", "traitvalue_char", "issue"),
      names(matched)
    ), drop = FALSE]

    msg <- sprintf(t("Dry run: %d measurement(s) matched to individuals."), n_total)
    if (n_unmatched > 0) {
      msg <- paste0(msg, sprintf(" %d row(s) unmatched (skipped).", n_unmatched))
    }
    feat_preview <- config$features_field
    if (!is.null(feat_preview) && length(feat_preview) > 0) {
      msg <- paste0(msg, sprintf(" Metadata columns to be linked as features: %s.",
        paste(feat_preview, collapse = ", ")))
    }

    return(list(
      dry_run = TRUE,
      success = TRUE,
      n_subplot_records = n_total,
      n_people_records = 0,
      preview = preview,
      message = msg
    ))
  }

  # Live import: bulk insert into data_traits_measures inside an explicit transaction
  cli::cli_alert_info("Inserting {n_total} measurement(s) into data_traits_measures...")

  # Pool connections do not support transactions directly — check out a raw connection
  is_pool <- inherits(con, "Pool")
  actual_con <- if (is_pool) pool::poolCheckout(con) else con
  on.exit({
    if (is_pool) pool::poolReturn(actual_con)
  }, add = TRUE)

  tryCatch({
    today_d <- as.integer(format(Sys.Date(), "%d"))
    today_m <- as.integer(format(Sys.Date(), "%m"))
    today_y <- as.integer(format(Sys.Date(), "%Y"))

    # Build the full records data frame in one vectorised pass
    records <- data.frame(
      id_table_liste_plots = as.integer(matched$id_liste_plots),
      id_data_individuals  = as.integer(matched$id_data_individuals),
      id_diconame          = if ("id_diconame" %in% names(matched)) as.integer(matched$id_diconame) else NA_integer_,
      id_specimen          = if ("id_specimen_ind" %in% names(matched)) as.integer(matched$id_specimen_ind) else NA_integer_,
      id_sub_plots         = if ("id_sub_plots" %in% names(matched)) as.integer(matched$id_sub_plots) else NA_integer_,
      traitid              = as.integer(matched$traitid),
      traitvalue           = if ("traitvalue" %in% names(matched)) as.numeric(matched$traitvalue) else NA_real_,
      traitvalue_char      = if ("traitvalue_char" %in% names(matched)) as.character(matched$traitvalue_char) else NA_character_,
      original_plot_name   = as.character(matched$plot_name),
      date_modif_d         = today_d,
      date_modif_m         = today_m,
      date_modif_y         = today_y,
      stringsAsFactors     = FALSE
    )

    # Optional date columns
    if ("year"  %in% names(matched)) records$year  <- as.integer(matched$year)
    if ("month" %in% names(matched)) records$month <- as.integer(matched$month)
    if ("day"   %in% names(matched)) records$day   <- as.integer(matched$day)

    # Issue column (from validation checks)
    if ("issue" %in% names(matched)) records$issue <- as.character(matched$issue)

    DBI::dbBegin(actual_con)

    # Insert into data_traits_measures with RETURNING so we get the generated IDs
    # needed to link feature records. Row order of inserted_ids matches matched.
    inserted_ids <- .execute_trait_insert_with_returning(records, actual_con)
    inserted <- nrow(inserted_ids)
    cli::cli_alert_success("Inserted {inserted} measurement(s)")

    # Insert feature records (features_field columns) linked via id_trait_measures
    features_field <- config$features_field
    features_field_mappings <- config$features_field_mappings  # NULL for long format
    n_feat_inserted <- 0L

    if (!is.null(features_field) && length(features_field) > 0) {

      # Resolve each original column name to its traitlist name
      feat_trait_names <- vapply(features_field, function(fc) {
        if (!is.null(features_field_mappings) && fc %in% names(features_field_mappings))
          features_field_mappings[[fc]]
        else
          fc
      }, character(1))

      # Single query for all trait metadata
      quoted <- paste(
        vapply(feat_trait_names, function(nm) paste0("'", gsub("'", "''", nm), "'"), character(1)),
        collapse = ","
      )
      trait_meta <- DBI::dbGetQuery(actual_con,
        sprintf("SELECT id_trait, trait, valuetype FROM traitlist WHERE trait IN (%s)", quoted))

      for (i in seq_along(features_field)) {
        feat_col    <- features_field[i]
        trait_name  <- feat_trait_names[i]
        ti          <- trait_meta[trait_meta$trait == trait_name, , drop = FALSE]

        if (nrow(ti) == 0) {
          cli::cli_alert_warning("features_field '{feat_col}' (trait '{trait_name}') not in traitlist — skipped")
          next
        }

        trait_id  <- as.integer(ti$id_trait[1])
        is_num    <- ti$valuetype[1] %in% c("numeric", "integer", "table_colnam")

        feat_vals <- matched[[feat_col]]
        has_val   <- !is.na(feat_vals)
        if (!any(has_val)) next

        feat_records <- data.frame(
          id_trait_measures = as.integer(inserted_ids$id_trait_measures[has_val]),
          id_trait          = trait_id,
          typevalue         = if (is_num) suppressWarnings(as.numeric(feat_vals[has_val])) else NA_real_,
          typevalue_char    = if (!is_num) as.character(feat_vals[has_val]) else NA_character_,
          date_modif_d      = today_d,
          date_modif_m      = today_m,
          date_modif_y      = today_y,
          stringsAsFactors  = FALSE
        )

        DBI::dbAppendTable(actual_con, "data_ind_measures_feat", feat_records)
        n_feat_inserted <- n_feat_inserted + nrow(feat_records)
        cli::cli_alert_success("Inserted {nrow(feat_records)} feature record(s) for '{feat_col}'")
      }
    }

    DBI::dbCommit(actual_con)

    msg <- sprintf(t("Successfully inserted %d measurement(s)."), inserted)
    if (n_feat_inserted > 0) {
      msg <- paste0(msg, sprintf(" %d feature record(s) linked.", n_feat_inserted))
    }
    if (n_unmatched > 0) {
      msg <- paste0(msg, sprintf(" %d row(s) skipped (unmatched individuals).", n_unmatched))
    }

    return(list(
      dry_run = FALSE,
      success = TRUE,
      n_subplot_records = inserted,
      n_people_records = 0,
      message = msg
    ))

  }, error = function(e) {
    # Roll back on any error — leaves the database unchanged
    tryCatch(DBI::dbRollback(actual_con), error = function(e2) NULL)
    cli::cli_alert_danger("Measurement import failed, transaction rolled back: {e$message}")
    return(list(
      dry_run = FALSE,
      success = FALSE,
      n_subplot_records = 0,
      n_people_records = 0,
      message = sprintf(t("Import failed, no data was written: %s"), e$message)
    ))
  })
}


#' Execute stem vital status upsert
#'
#' Re-runs \code{compute_stem_vital_status()} with \code{add_data = TRUE} for
#' the individuals stored in \code{config$individual_ids}. Existing
#' \code{stem_status} records for those individuals are deleted and replaced.
#'
#' @param data Status tibble (already computed in step 3; used only for the
#'   dry-run preview).
#' @param config Feature configuration list; must contain
#'   \code{individual_ids} (integer vector).
#' @param con Database connection
#' @param dry_run Logical
#' @param i18n Translator object
#' @return List with import results
#' @keywords internal
.execute_stem_status_import <- function(data, config, con, dry_run = TRUE, i18n = NULL) {

  t <- function(x) if (!is.null(i18n)) i18n$t(x) else x

  individual_ids <- config$individual_ids

  if (is.null(individual_ids) || length(individual_ids) == 0) {
    return(list(
      dry_run = dry_run,
      success = FALSE,
      n_subplot_records = 0,
      n_people_records  = 0,
      message = t("No individual IDs found in configuration.")
    ))
  }

  n_stems   <- length(unique(individual_ids))
  n_rows    <- nrow(data)
  n_to_write <- sum(!is.na(data$stem_vital_status) & !is.na(data$id_sub_plots))

  if (dry_run) {
    preview <- data %>%
      dplyr::select(
        id_n, plot_name, census_name, census_date,
        stem_vital_status, missing, evidence_source
      ) %>%
      utils::head(50)

    return(list(
      dry_run           = TRUE,
      success           = TRUE,
      n_subplot_records = n_to_write,
      n_people_records  = 0,
      preview           = preview,
      message           = sprintf(
        t("Dry run: %d stem_status record(s) would be written for %d stem(s) across %d census rows."),
        n_to_write, n_stems, n_rows
      )
    ))
  }

  # Live: re-call compute_stem_vital_status with add_data = TRUE
  tryCatch({
    compute_stem_vital_status(
      individual_ids = individual_ids,
      add_data       = TRUE,
      dry_run        = FALSE,
      con            = con
    )

    return(list(
      dry_run           = FALSE,
      success           = TRUE,
      n_subplot_records = n_to_write,
      n_people_records  = 0,
      message           = sprintf(
        t("Successfully wrote stem_status for %d stem(s) (%d record(s) inserted)."),
        n_stems, n_to_write
      )
    ))
  }, error = function(e) {
    cli::cli_alert_danger("Stem status import failed: {e$message}")
    return(list(
      dry_run           = FALSE,
      success           = FALSE,
      n_subplot_records = 0,
      n_people_records  = 0,
      message           = sprintf(t("Import failed: %s"), e$message)
    ))
  })
}


#' Execute standardize_observations import
#'
#' Re-runs \code{\link{standardize_observations}} with \code{add_data = TRUE}
#' for the confirmed individuals; dawkins rows flagged \code{skip_existing}
#' are dropped, mortality_risk_flag duplicates are de-duped on (id_n,
#' id_sub_plots, std_value).
#'
#' @keywords internal
.execute_standardize_observations_import <- function(data, config, con,
                                                     dry_run = TRUE,
                                                     i18n    = NULL) {
  t <- function(x) if (!is.null(i18n)) i18n$t(x) else x

  individual_ids <- config$individual_ids
  if (is.null(individual_ids) || length(individual_ids) == 0) {
    return(list(
      dry_run           = dry_run,
      success           = FALSE,
      n_subplot_records = 0,
      n_people_records  = 0,
      message           = t("No individual IDs found in configuration.")
    ))
  }

  n_rows     <- nrow(data)
  n_writable <- sum(!data$skip_existing & !is.na(data$id_sub_plots))
  n_stems    <- length(unique(individual_ids))

  if (dry_run) {
    preview <- data %>%
      dplyr::select(id_n, plot_name, tag, census_name, census_date,
                    trait, std_value, source_phrases, skip_existing) %>%
      utils::head(50)

    return(list(
      dry_run           = TRUE,
      success           = TRUE,
      n_subplot_records = n_writable,
      n_people_records  = 0,
      preview           = preview,
      message           = sprintf(
        t("Dry run: up to %d standardized row(s) would be written for %d stem(s) (%d row(s) skipped — existing dawkins values)."),
        n_writable, n_stems, sum(data$skip_existing)
      )
    ))
  }

  tryCatch({
    standardize_observations(
      individual_ids = individual_ids,
      add_data       = TRUE,
      dry_run        = FALSE,
      con            = con
    )
    return(list(
      dry_run           = FALSE,
      success           = TRUE,
      n_subplot_records = n_writable,
      n_people_records  = 0,
      message           = sprintf(
        t("Successfully wrote standardized observations for %d stem(s) (up to %d row(s))."),
        n_stems, n_writable
      )
    ))
  }, error = function(e) {
    cli::cli_alert_danger("Standardize observations import failed: {e$message}")
    return(list(
      dry_run           = FALSE,
      success           = FALSE,
      n_subplot_records = 0,
      n_people_records  = 0,
      message           = sprintf(t("Import failed: %s"), e$message)
    ))
  })
}


#' Execute multi-stem grouping import
#'
#' Resolves tag + plot_name to id_n, then updates stem_grouping
#' via update_records().
#'
#' @keywords internal
.execute_multi_stems_import <- function(data, con, dry_run = TRUE, i18n = NULL) {

  t <- function(x) if (!is.null(i18n)) i18n$t(x) else x

  # ---- Resolve tags to database id_n ----
  # Use pre-resolved IDs from step 3 if available; otherwise resolve here
  if (!("id_n" %in% names(data) && "group_id_n" %in% names(data))) {
    plot_names <- unique(data$plot_name)
    placeholders <- paste(sprintf("'%s'", gsub("'", "''", plot_names)), collapse = ",")

    db_inds <- tryCatch({
      DBI::dbGetQuery(con, sprintf(
        "SELECT di.id_n, di.tag, dlp.plot_name
         FROM data_individuals di
         JOIN data_liste_plots dlp ON di.id_table_liste_plots_n = dlp.id_liste_plots
         WHERE dlp.plot_name IN (%s)",
        placeholders
      ))
    }, error = function(e) {
      return(list(
        dry_run = dry_run, success = FALSE,
        n_subplot_records = 0, n_people_records = 0,
        message = sprintf("Failed to query individuals: %s", e$message)
      ))
    })

    if (is.list(db_inds) && !is.null(db_inds$success)) return(db_inds)

    data$id_n <- NA_integer_
    data$group_id_n <- NA_integer_

    for (i in seq_len(nrow(data))) {
      match_row <- db_inds[as.character(db_inds$tag) == as.character(data$tag[i]) &
                           db_inds$plot_name == data$plot_name[i], ]
      if (nrow(match_row) > 0) data$id_n[i] <- match_row$id_n[1]

      match_parent <- db_inds[as.character(db_inds$tag) == as.character(data$group_tag[i]) &
                              db_inds$plot_name == data$plot_name[i], ]
      if (nrow(match_parent) > 0) data$group_id_n[i] <- match_parent$id_n[1]
    }
  }

  # Filter to non-parent rows only (parent stems don't need stem_grouping)
  update_data <- data[data$tag != data$group_tag, , drop = FALSE]
  update_data <- update_data[!is.na(update_data$id_n) & !is.na(update_data$group_id_n), , drop = FALSE]

  n_groups <- length(unique(paste(data$plot_name, data$group_tag)))
  n_updates <- nrow(update_data)

  if (n_updates == 0) {
    return(list(
      dry_run = dry_run, success = TRUE,
      n_subplot_records = 0, n_people_records = 0,
      message = t("No stem_grouping updates needed (all tags are parents or unresolved)")
    ))
  }

  # ---- Dry run ----
  if (dry_run) {
    preview_lines <- utils::head(
      sprintf("  tag %s (id_n=%d) -> stem_grouping=%d (parent tag %s)",
              update_data$tag, update_data$id_n,
              update_data$group_id_n, update_data$group_tag),
      15
    )

    cli::cli_alert_info("DRY RUN: Would update {n_updates} stem(s) in {n_groups} group(s)")
    for (line in preview_lines) cli::cli_alert(line)

    return(list(
      dry_run = TRUE,
      success = TRUE,
      n_subplot_records = n_updates,
      n_people_records = 0,
      message = sprintf(
        t("Dry run: %d stems in %d groups would be updated"),
        n_updates, n_groups
      ),
      preview = update_data[, c("plot_name", "tag", "group_tag", "id_n", "group_id_n"),
                            drop = FALSE]
    ))
  }

  # ---- Live import via update_records ----
  tryCatch({
    update_df <- data.frame(
      id_n = update_data$id_n,
      stem_grouping = update_data$group_id_n,
      stringsAsFactors = FALSE
    )

    result <- update_records(
      data = update_df,
      table_type = "individuals",
      execute = TRUE,
      method = "batch",
      con = con,
      interactive = FALSE,
      show_comparison = FALSE
    )

    cli::cli_alert_success("Updated stem_grouping for {n_updates} stem(s) in {n_groups} group(s)")

    return(list(
      dry_run = FALSE,
      success = TRUE,
      n_subplot_records = n_updates,
      n_people_records = 0,
      message = sprintf(
        t("Successfully updated %d stems in %d multi-stem groups"),
        n_updates, n_groups
      )
    ))
  }, error = function(e) {
    cli::cli_alert_danger("Multi-stem import failed: {e$message}")
    return(list(
      dry_run = FALSE,
      success = FALSE,
      n_subplot_records = 0,
      n_people_records = 0,
      message = sprintf("Error: %s", e$message)
    ))
  })
}
