# Census Information Collection Module
#
# Module for collecting census information for permanent plots after metadata import

#' Census Information Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_census_information_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("clipboard-list"),
      i18n$t("Optional: Add Census Information"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("If your plots are permanent (tagged stems that will be recensused), you can add census information now."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Ask if plots are permanent
    shiny::div(
      class = "card",
      style = "padding: 20px; background-color: #f8f9fa; margin-bottom: 30px;",
      shiny::radioButtons(
        ns("are_plots_permanent"),
        shiny::strong(i18n$t("Are these plots permanent?")),
        choices = setNames(
          c("yes", "no"),
          c(i18n$t("Yes - stems are tagged and will be recensused"),
            i18n$t("No - temporary plots or non-tagged stems"))
        ),
        selected = character(0)
      )
    ),

    # Conditional census information form
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'yes'", ns("are_plots_permanent")),

      shiny::div(
        class = "alert alert-info",
        shiny::icon("info-circle"),
        " ",
        i18n$t("Census information will be added as subplot features. This helps track measurements across time.")
      ),

      shiny::hr(),

      shiny::h4(i18n$t("Census Details"), style = "margin-bottom: 20px;"),

      shiny::fluidRow(
        shiny::column(
          3,
          shiny::numericInput(
            ns("census_number"),
            i18n$t("Census Number"),
            value = 1,
            min = 1,
            step = 1
          )
        ),
        shiny::column(
          3,
          shiny::numericInput(
            ns("census_year"),
            i18n$t("Year *"),
            value = as.integer(format(Sys.Date(), "%Y")),
            min = 1900,
            max = 2100,
            step = 1
          )
        ),
        shiny::column(
          3,
          shiny::numericInput(
            ns("census_month"),
            i18n$t("Month"),
            value = NA,
            min = 1,
            max = 12,
            step = 1
          )
        ),
        shiny::column(
          3,
          shiny::numericInput(
            ns("census_day"),
            i18n$t("Day"),
            value = NA,
            min = 1,
            max = 31,
            step = 1
          )
        )
      ),

      shiny::hr(),

      shiny::h4(i18n$t("People Information from Metadata"), style = "margin-bottom: 20px;"),

      # Show all table_colnam features from uploaded metadata (read-only)
      shiny::uiOutput(ns("people_features_display")),

      shiny::hr(),

      # Preview census data
      shiny::h4(i18n$t("Census Data Preview"), style = "margin-bottom: 20px;"),

      shiny::uiOutput(ns("census_preview")),

      shiny::hr(),

      # Add census button
      shiny::div(
        style = "text-align: center; margin-top: 30px;",
        shiny::actionButton(
          ns("add_census"),
          shiny::tagList(shiny::icon("save"), paste0(" ", i18n$t("Add Census Information"))),
          class = "btn-primary btn-lg"
        )
      )
    ),

    # Result message
    shiny::uiOutput(ns("result_message"))
  )
}


#' Census Information Module - Server
#'
#' @param id Module namespace ID
#' @param imported_plots Reactive containing the imported plot data with plot_name and id_liste_plots
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object from shiny.i18n
#' @return Reactive containing census addition result
#' @keywords internal
#' @export
mod_census_information_server <- function(id, imported_plots, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Store census addition result
    census_result <- shiny::reactiveVal(NULL)

    # Query people features that were just imported for these plots
    people_features_in_database <- shiny::reactive({
      shiny::req(imported_plots(), con())

      plots <- imported_plots()

      tryCatch({
        # Use existing query_subplots function to get all subplot features
        plot_ids <- plots$id_liste_plots

        cli::cli_alert_info("Querying subplot features for plot IDs: {paste(plot_ids, collapse=', ')}")

        subplot_results <- query_subplots(ids_plots = plot_ids, con = con(), verbose = FALSE)

        # Check if we have subplot data
        if (is.null(subplot_results) || is.null(subplot_results$all_subplots)) {
          cli::cli_alert_warning("No subplot data returned")
          return(NULL)
        }

        all_subplots <- subplot_results$all_subplots

        cli::cli_alert_info("Total subplots returned: {nrow(all_subplots)}")

        # Filter for people features (valuetype == "table_colnam")
        people_features <- all_subplots[all_subplots$valuetype == "table_colnam", ]

        cli::cli_alert_info("Found {nrow(people_features)} people feature records")

        # Debug: show sample of people_features
        if (nrow(people_features) > 0) {
          print("****************people_features sample*****************")
          print(people_features[1:min(3, nrow(people_features)), c("type", "typevalue", "valuetype", "id_table_liste_plots")])
        }

        if (nrow(people_features) == 0) {
          return(NULL)
        }

        # The typevalue column contains the id_table_colnam
        # Get the colnam values for display
        people_ids <- unique(people_features$typevalue[!is.na(people_features$typevalue)])

        if (length(people_ids) == 0) {
          return(NULL)
        }

        # Query table_colnam to get names
        people_query <- sprintf("SELECT id_table_colnam, colnam FROM table_colnam WHERE id_table_colnam IN (%s)",
                               paste(people_ids, collapse = ", "))
        people_names <- DBI::dbGetQuery(con(), people_query)

        # Merge names back to people_features
        people_features$full_name <- people_names$colnam[match(people_features$typevalue, people_names$id_table_colnam)]

        # Group by feature type for display
        features_by_type <- split(people_features, people_features$type)

        # Build summary for display
        features_summary <- list()
        for (feat_type in names(features_by_type)) {
          names_list <- unique(features_by_type[[feat_type]]$full_name)
          names_list <- names_list[!is.na(names_list) & trimws(names_list) != ""]
          if (length(names_list) > 0) {
            features_summary[[feat_type]] <- names_list
          }
        }

        if (length(features_summary) == 0) {
          return(NULL)
        }

        # Also return the raw data for copying to census
        return(list(
          summary = features_summary,
          raw_data = people_features
        ))

      }, error = function(e) {
        cli::cli_alert_warning("Could not query people features from database: {e$message}")
        return(NULL)
      })
    })

    # Display all table_colnam features from database (read-only)
    output$people_features_display <- shiny::renderUI({
      features_result <- people_features_in_database()

      if (is.null(features_result)) {
        return(
          shiny::div(
            class = "alert alert-secondary",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("No people information found in uploaded metadata.")
          )
        )
      }

      features_data <- features_result$summary

      # Build display
      shiny::div(
        class = "alert alert-success",
        style = "margin-bottom: 20px;",
        shiny::strong(i18n()$t("The following people information from your metadata will be added to the census:")),
        shiny::tags$ul(
          style = "margin-top: 10px; margin-bottom: 0;",
          lapply(names(features_data), function(col) {
            label <- switch(col,
              "team_leader" = i18n()$t("Team Leader"),
              "principal_investigator" = i18n()$t("Principal Investigator"),
              "data_manager" = i18n()$t("Data Manager"),
              "additional_people" = i18n()$t("Additional People"),
              col  # Use column name as fallback
            )
            shiny::tags$li(
              shiny::strong(paste0(label, ": ")),
              paste(features_data[[col]], collapse = ", ")
            )
          })
        ),
        shiny::div(
          style = "margin-top: 10px; font-size: 13px; color: #155724;",
          shiny::icon("check-circle"),
          " ",
          i18n()$t("These values will be automatically added from your uploaded data.")
        )
      )
    })

    # Automatically prefill date from database when plots are available
    shiny::observe({
      shiny::req(imported_plots(), con())

      # Wait for inputs to be initialized
      shiny::req(input$census_year)

      plots <- imported_plots()

      # Only prefill once (check if year is still default)
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      if (input$census_year != current_year) {
        return()
      }

      tryCatch({
        # Query date information from data_liste_plots
        query <- sprintf("
          SELECT date_y, date_m, date_d
          FROM data_liste_plots
          WHERE id_liste_plots IN (%s)
          LIMIT 1
        ", paste(plots$id_liste_plots, collapse = ", "))

        date_info <- DBI::dbGetQuery(con(), query)

        if (nrow(date_info) > 0) {
          year_val <- if (!is.na(date_info$date_y[1])) as.integer(date_info$date_y[1]) else NULL
          month_val <- if (!is.na(date_info$date_m[1])) as.integer(date_info$date_m[1]) else NULL
          day_val <- if (!is.na(date_info$date_d[1])) as.integer(date_info$date_d[1]) else NULL

          # Update inputs if values found
          if (!is.null(year_val)) {
            shiny::updateNumericInput(session, "census_year", value = year_val)
          }
          if (!is.null(month_val)) {
            shiny::updateNumericInput(session, "census_month", value = month_val)
          }
          if (!is.null(day_val)) {
            shiny::updateNumericInput(session, "census_day", value = day_val)
          }

          cli::cli_alert_info("Date fields automatically prefilled from plot database")
        }

      }, error = function(e) {
        cli::cli_alert_warning("Could not auto-prefill date: {e$message}")
      })
    })

    # Preview census data
    output$census_preview <- shiny::renderUI({
      shiny::req(imported_plots(), input$census_number, input$census_year)

      plots <- imported_plots()

      # Show preview
      shiny::tagList(
        shiny::p(
          sprintf(i18n()$t("Census information will be added for %d plot(s)"), nrow(plots)),
          style = "color: #6c757d;"
        ),
        DT::DTOutput(session$ns("census_preview_table"))
      )
    })

    output$census_preview_table <- DT::renderDT({
      shiny::req(imported_plots(), input$census_number, input$census_year)

      plots <- imported_plots()

      # Build census data preview (just census date info, people features added separately)
      census_data <- data.frame(
        plot_name = plots$plot_name,
        census = input$census_number,
        year = input$census_year,
        month = ifelse(is.na(input$census_month), NA, input$census_month),
        day = ifelse(is.na(input$census_day), NA, input$census_day),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        census_data,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE,
        class = "display cell-border stripe"
      )
    })

    # Add census information
    shiny::observeEvent(input$add_census, {
      shiny::req(imported_plots(), con())

      # Validate required fields
      if (is.na(input$census_year)) {
        shiny::showNotification(
          i18n()$t("Year is required"),
          type = "error",
          duration = 5
        )
        return()
      }

      shiny::withProgress({
        shiny::setProgress(0.3, message = i18n()$t("Adding census information..."))

        plots <- imported_plots()

        # Build basic census data
        census_data <- data.frame(
          plot_name = plots$plot_name,
          id_liste_plots = plots$id_liste_plots,
          census = input$census_number,
          year = input$census_year,
          month = ifelse(is.na(input$census_month), NA, input$census_month),
          day = ifelse(is.na(input$census_day), NA, input$census_day),
          stringsAsFactors = FALSE
        )

        # Get people features from database
        people_features_result <- people_features_in_database()
        features_to_add <- NULL

        if (!is.null(people_features_result)) {
          people_data <- people_features_result$raw_data

          # Get unique feature types (from 'type' column in query_subplots result)
          feature_types <- unique(people_data$type)
          features_to_add <- feature_types

          cli::cli_alert_info("Adding {length(features_to_add)} people feature type(s): {paste(features_to_add, collapse=', ')}")

          # Debug: show sample of raw people_data
          print("****************people_data for census*****************")
          print(people_data[1:min(3, nrow(people_data)), c("type", "typevalue", "id_table_liste_plots")])

          # For each feature type, add the typevalue (id_table_colnam) to census_data
          # Group by plot and feature type
          for (feat_type in feature_types) {
            feat_data <- people_data[people_data$type == feat_type, ]

            # For each plot, get the typevalue (id_table_colnam)
            # If multiple people per plot/feature type, we'll get multiple values
            # Take the first one for simplicity (or we could concatenate)
            for (i in 1:nrow(census_data)) {
              plot_id <- census_data$id_liste_plots[i]
              plot_feat <- feat_data[feat_data$id_table_liste_plots == plot_id, ]

              if (nrow(plot_feat) > 0) {
                # Take first typevalue (id_table_colnam) if multiple exist
                census_data[[feat_type]][i] <- plot_feat$typevalue[1]
              } else {
                census_data[[feat_type]][i] <- NA
              }
            }
          }

          # Debug: show census_data with people columns
          print("****************census_data with people features*****************")
          cols_to_show <- c("id_liste_plots", "census", "year", intersect(feature_types, names(census_data)))
          print(census_data[, cols_to_show])

          cli::cli_alert_info("Census data with people features prepared")
        }

        # Step 1: Add census records WITHOUT people features
        result <- tryCatch({
          # First, create census records with just date info
          census_basic <- census_data[, c("plot_name", "id_liste_plots", "census", "year", "month", "day")]

          add_subplot_features(
            new_data = census_basic,
            col_names_select = c("year", "month", "day", "census"),
            col_names_corresp = c("year", "month", "day", "census"),
            id_plot_name = "id_liste_plots",
            id_plot_name_corresp = "id_table_liste_plots_n",
            subplottype_field = "census",
            features_field = NULL,  # No features in this step
            add_data = TRUE,
            ask_before_update = FALSE,
            interactive = FALSE,
            verbose = TRUE,
            check_existing_data = FALSE,
            con = con()
          )

          cli::cli_alert_success("Census records created")

          # Step 2: If we have people features, add them directly to data_subplot_feat
          if (!is.null(features_to_add) && length(features_to_add) > 0) {
            cli::cli_alert_info("Adding {length(features_to_add)} people feature(s) to census records")

            # Query the newly created census records to get their id_sub_plots
            # Note: census number is stored in typevalue column
            census_query <- sprintf("
              SELECT sp.id_sub_plots, sp.id_table_liste_plots, spl.type
              FROM data_liste_sub_plots sp
              JOIN subplotype_list spl ON sp.id_type_sub_plot = spl.id_subplotype
              WHERE sp.id_table_liste_plots IN (%s)
                AND spl.type = 'census'
                AND sp.typevalue = %d
                AND sp.year = %d
            ",
            paste(plots$id_liste_plots, collapse = ", "),
            input$census_number,
            input$census_year
            )

            new_census_records <- DBI::dbGetQuery(con(), census_query)

            cli::cli_alert_info("Found {nrow(new_census_records)} new census record(s)")

            # Get subplotype IDs for people features
            subplot_info <- subplot_list(con())
            people_features_lookup <- subplot_info[subplot_info$valuetype == "table_colnam", c("type", "id_subplotype")]

            # Build data_subplot_feat records
            feat_records <- list()
            for (feat_type in features_to_add) {
              # Get the subplotype ID for this feature
              subplotype_id <- people_features_lookup$id_subplotype[people_features_lookup$type == feat_type]

              if (length(subplotype_id) == 0) {
                cli::cli_alert_warning("Could not find subplotype ID for {feat_type}")
                next
              }

              # For each census record, add the person ID
              for (i in 1:nrow(new_census_records)) {
                plot_id <- new_census_records$id_table_liste_plots[i]
                subplot_id <- new_census_records$id_sub_plots[i]

                # Get the person ID for this plot/feature
                person_id <- census_data[[feat_type]][census_data$id_liste_plots == plot_id]

                if (length(person_id) > 0 && !is.na(person_id[1])) {
                  feat_records[[length(feat_records) + 1]] <- data.frame(
                    typevalue = as.numeric(person_id[1]),
                    typevalue_char = NA_character_,
                    id_sub_plots = as.integer(subplot_id),
                    id_type_sub_plot = as.integer(subplotype_id),
                    date_modif_d = as.integer(format(Sys.Date(), "%d")),
                    date_modif_m = as.integer(format(Sys.Date(), "%m")),
                    date_modif_y = as.integer(format(Sys.Date(), "%Y")),
                    stringsAsFactors = FALSE
                  )
                }
              }
            }

            if (length(feat_records) > 0) {
              # Combine all feature records
              all_feat_records <- do.call(rbind, feat_records)

              # Insert into data_subplot_feat
              DBI::dbAppendTable(con(), "data_subplot_feat", all_feat_records)

              cli::cli_alert_success("Added {nrow(all_feat_records)} people feature record(s) to data_subplot_feat")
            }
          }

          n_features <- if (!is.null(features_to_add)) length(features_to_add) else 0
          msg <- sprintf(
            i18n()$t("Census information added successfully for %d plot(s) with %d people feature(s)"),
            nrow(plots),
            n_features
          )

          list(success = TRUE, message = msg)

        }, error = function(e) {
          cli::cli_alert_danger("Failed to add census information: {e$message}")
          list(success = FALSE, message = paste("Error:", e$message))
        })

        shiny::setProgress(1, message = i18n()$t("Complete!"))

        census_result(result)

        if (result$success) {
          shiny::showNotification(
            result$message,
            type = "message",
            duration = 5
          )
        } else {
          shiny::showNotification(
            result$message,
            type = "error",
            duration = 10
          )
        }

      }, message = i18n()$t("Adding census information..."))
    })

    # Result message
    output$result_message <- shiny::renderUI({
      shiny::req(census_result())

      result <- census_result()

      if (result$success) {
        shiny::div(
          class = "alert alert-success",
          style = "margin-top: 30px;",
          shiny::icon("check-circle"),
          " ",
          shiny::strong(i18n()$t("Success!")),
          " ",
          result$message
        )
      } else {
        shiny::div(
          class = "alert alert-danger",
          style = "margin-top: 30px;",
          shiny::icon("exclamation-circle"),
          " ",
          shiny::strong(i18n()$t("Error")),
          " ",
          result$message
        )
      }
    })

    # Return result
    return(shiny::reactive(census_result()))
  })
}
