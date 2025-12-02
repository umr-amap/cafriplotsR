# Import Wizard - Step 4: Lookup Matching
#
# Proactive module for matching user lookup values to database before validation

#' Step 4 Module: Lookup Matching - UI
#'
#' @param id Module namespace ID
#' @keywords internal
mod_step4_lookup_matching_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("link"),
      "Step 4: Match Lookup Values",
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      "Before validation, let's match your lookup values (methods, countries, people) to the database. ",
      "This step ensures your data references the correct database entries.",
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Analyze button
    shiny::actionButton(
      ns("analyze_lookups"),
      shiny::tagList(shiny::icon("search"), " Analyze Lookup Values"),
      class = "btn-primary btn-lg",
      style = "margin-bottom: 30px;"
    ),

    # Analysis results
    shiny::uiOutput(ns("analysis_results")),

    # Matching interface (shown if non-exact matches found)
    shiny::uiOutput(ns("matching_interface"))
  )
}


#' Step 4 Module: Lookup Matching - Server
#'
#' @param id Module namespace ID
#' @param data Reactive containing uploaded user data
#' @param mappings Reactive containing column mappings
#' @param con Reactive containing database connection pool
#' @return Reactive containing matched/updated data
#' @keywords internal
mod_step4_lookup_matching_server <- function(id, data, mappings, con) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for analysis and matched data
    analysis_result <- shiny::reactiveVal(NULL)
    non_exact_matches <- shiny::reactiveVal(NULL)
    matched_data <- shiny::reactiveVal(NULL)
    matching_complete <- shiny::reactiveVal(FALSE)

    # Analyze lookup values when button clicked
    shiny::observeEvent(input$analyze_lookups, {
      shiny::req(data(), mappings(), con())

      shiny::withProgress({
        shiny::setProgress(0.3, message = "Analyzing lookup values...")

        cli::cli_alert_info("Analyzing lookup columns in data...")

        # Analyze all lookup columns
        result <- tryCatch({
          .analyze_lookup_columns(data(), mappings(), con())
        }, error = function(e) {
          cli::cli_alert_danger("Analysis failed: {e$message}")
          shiny::showNotification(
            paste("Analysis error:", e$message),
            type = "error",
            duration = 10
          )
          return(NULL)
        })

        shiny::setProgress(1, message = "Analysis complete!")

        if (!is.null(result)) {
          analysis_result(result)

          # Extract values that need matching
          needs_matching <- result$non_exact_matches
          non_exact_matches(needs_matching)

          total_non_exact <- sum(sapply(needs_matching, length))

          cli::cli_alert_success(
            "Analysis complete: {result$summary$total_values} total values, ",
            "{result$summary$exact_matches} exact matches, ",
            "{total_non_exact} need matching"
          )

          if (total_non_exact == 0) {
            shiny::showNotification(
              "Great! All lookup values match exactly. You can proceed to validation.",
              type = "message",
              duration = 5
            )
            matched_data(data())  # No changes needed
            matching_complete(TRUE)
          } else {
            shiny::showNotification(
              sprintf("Found %d lookup value(s) that need matching.", total_non_exact),
              type = "warning",
              duration = 10
            )
          }
        }

      }, message = "Analyzing lookup values...")
    })

    # Render analysis results
    output$analysis_results <- shiny::renderUI({
      shiny::req(analysis_result())

      result <- analysis_result()
      total_non_exact <- sum(sapply(result$non_exact_matches, length))

      shiny::tagList(
        shiny::h4("Analysis Results", style = "margin-top: 30px; margin-bottom: 15px;"),

        # Summary cards
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
              shiny::h3(result$summary$total_values, style = "margin: 0; color: #007bff;"),
              shiny::p("Total Lookup Values", style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            4,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
              shiny::h3(result$summary$exact_matches, style = "margin: 0; color: #28a745;"),
              shiny::p("Exact Matches", style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            4,
            shiny::div(
              class = "card",
              style = sprintf(
                "padding: 20px; background-color: #f8f9fa; border-left: 4px solid %s; text-align: center;",
                if (total_non_exact == 0) "#28a745" else "#ffc107"
              ),
              shiny::h3(
                total_non_exact,
                style = sprintf("margin: 0; color: %s;", if (total_non_exact == 0) "#28a745" else "#ffc107")
              ),
              shiny::p("Need Matching", style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          )
        ),

        shiny::hr(),

        # Status message
        if (total_non_exact == 0) {
          shiny::div(
            class = "alert alert-success",
            shiny::icon("check-circle"),
            shiny::strong(" All values match! "),
            "You can proceed to the next step."
          )
        } else {
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            shiny::strong(" Matching required: "),
            sprintf(
              "Please review and match the %d value(s) below to continue.",
              total_non_exact
            )
          )
        }
      )
    })

    # Render matching interface
    output$matching_interface <- shiny::renderUI({
      shiny::req(analysis_result())

      needs_matching <- non_exact_matches()

      if (is.null(needs_matching) || length(needs_matching) == 0) {
        return(NULL)
      }

      shiny::tagList(
        shiny::hr(),
        mod_lookup_matcher_ui(session$ns("matcher"))
      )
    })

    # Initialize lookup matcher server
    matcher_result <- mod_lookup_matcher_server(
      "matcher",
      invalid_values = non_exact_matches,
      con = con
    )

    # Apply matches when user confirms
    shiny::observeEvent(matcher_result$applied(), {
      shiny::req(matcher_result$applied() == TRUE)

      matches <- matcher_result$matches()

      if (length(matches) > 0) {
        cli::cli_alert_info("Applying {length(unlist(matches))} user match(es)...")

        # Update data with matches
        updated_data <- data()

        for (col_name in names(matches)) {
          col_matches <- matches[[col_name]]

          # Get user column name from reverse mapping
          reverse_mappings <- setNames(names(mappings()), unlist(mappings()))
          user_col <- reverse_mappings[[col_name]]

          if (!is.null(user_col) && user_col %in% names(updated_data)) {

            # Check if this is a people column (can have comma-separated values)
            is_people_column <- tryCatch({
              subplot_info <- subplot_list(con())
              if (!is.null(subplot_info) && "type" %in% names(subplot_info) && "valuetype" %in% names(subplot_info)) {
                people_cols <- subplot_info$type[!is.na(subplot_info$valuetype) & subplot_info$valuetype == "table_colnam"]
                people_cols <- people_cols[!is.na(people_cols)]
                col_name %in% people_cols
              } else {
                FALSE
              }
            }, error = function(e) {
              FALSE
            })

            if (is_people_column) {
              # Handle comma-separated values for people columns
              cli::cli_alert_info("Applying matches to people column '{user_col}' (comma-separated)")

              # Process each row
              for (i in seq_len(nrow(updated_data))) {
                cell_value <- updated_data[[user_col]][i]

                if (!is.na(cell_value) && trimws(cell_value) != "") {
                  # Split by comma
                  names_list <- strsplit(as.character(cell_value), ",")[[1]]
                  names_list <- trimws(names_list)

                  # Replace each name with its matched ID
                  matched_ids <- sapply(names_list, function(name) {
                    if (name %in% names(col_matches)) {
                      as.character(col_matches[[name]])
                    } else {
                      name  # Keep original if no match
                    }
                  })

                  # Join back with commas
                  updated_data[[user_col]][i] <- paste(matched_ids, collapse = ", ")
                }
              }

            } else {
              # Simple replacement for non-people columns (method, country)
              for (user_value in names(col_matches)) {
                matched_id <- col_matches[[user_value]]
                updated_data[[user_col]][updated_data[[user_col]] == user_value] <- matched_id
              }
            }

            cli::cli_alert_success("Updated column '{user_col}' with {length(col_matches)} match(es)")
          }
        }

        matched_data(updated_data)
        matching_complete(TRUE)

        shiny::showNotification(
          sprintf("Successfully matched %d value(s)! You can proceed to validation.", length(unlist(matches))),
          type = "message",
          duration = 5
        )

      } else {
        # User skipped matching
        matched_data(data())
        matching_complete(TRUE)

        shiny::showNotification(
          "Skipped matching. Original data will be used for validation.",
          type = "warning",
          duration = 5
        )
      }
    })

    # Return matched data and completion status
    return(
      list(
        data = shiny::reactive(matched_data()),
        complete = shiny::reactive(matching_complete())
      )
    )
  })
}


#' Analyze Lookup Columns in User Data
#'
#' Checks which lookup values exist in database (exact match) and which need matching
#'
#' @param data User data frame
#' @param mappings Column mappings
#' @param con Database connection
#' @return List with analysis results
#' @keywords internal
.analyze_lookup_columns <- function(data, mappings, con) {

  # Identify plot-level lookup columns (method, country)
  plot_lookups <- c("method", "country")

  # Identify feature-level lookup columns (people fields from subplot_list)
  feature_lookups <- tryCatch({
    subplot_info <- subplot_list(con)

    # Ensure subplot_info has the required columns
    if (!is.null(subplot_info) && "type" %in% names(subplot_info) && "valuetype" %in% names(subplot_info)) {
      # Filter for table_colnam types and remove NAs
      people_cols <- subplot_info$type[!is.na(subplot_info$valuetype) & subplot_info$valuetype == "table_colnam"]
      people_cols <- people_cols[!is.na(people_cols)]
      as.character(people_cols)
    } else {
      cli::cli_alert_warning("subplot_list returned unexpected structure")
      character(0)
    }
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch subplot types: {e$message}")
    # Fallback to known people features if database query fails
    c("principal_investigator", "data_manager", "additional_people", "team_leader")
  })

  # Combine all lookup columns
  all_lookup_columns <- c(plot_lookups, feature_lookups)
  cli::cli_alert_info("All possible lookup columns: {paste(all_lookup_columns, collapse=', ')}")

  # Get reverse mappings (db_col -> user_col)
  cli::cli_alert_info("Creating reverse mappings...")
  cli::cli_alert_info("  mappings structure: {paste(names(mappings), '=', unlist(mappings), collapse=', ')}")

  reverse_mappings <- tryCatch({
    setNames(names(mappings), unlist(mappings))
  }, error = function(e) {
    cli::cli_alert_danger("Error creating reverse_mappings: {e$message}")
    stop(e)
  })

  cli::cli_alert_info("  reverse_mappings: {paste(names(reverse_mappings), '=', reverse_mappings, collapse=', ')}")

  # Filter to only process lookup columns that are actually mapped
  lookup_columns <- all_lookup_columns[all_lookup_columns %in% names(reverse_mappings)]
  cli::cli_alert_info("Lookup columns actually mapped: {paste(lookup_columns, collapse=', ')}")

  # Results storage
  exact_matches <- list()
  non_exact_matches <- list()
  total_values <- 0
  total_exact <- 0

  for (db_col in lookup_columns) {
    tryCatch({
      cli::cli_alert_info("Processing column: {db_col}")

      # Check if column is mapped
      user_col <- tryCatch({
        reverse_mappings[[db_col]]
      }, error = function(e) {
        cli::cli_alert_danger("  Error accessing reverse_mappings for '{db_col}': {e$message}")
        return(NULL)
      })

      user_col_display <- if (is.null(user_col)) "NULL" else as.character(user_col)
      cli::cli_alert_info("  User column: {user_col_display}")

      if (is.null(user_col) || !user_col %in% names(data)) {
        cli::cli_alert_info("  Skipping (not mapped or not present)")
        next  # Column not mapped or not present
      }

      # Get unique values from user data
      user_values <- unique(data[[user_col]])
      user_values <- user_values[!is.na(user_values) & trimws(user_values) != ""]

      # For people columns, split comma-separated values
      is_people_column <- db_col %in% feature_lookups
      if (is_people_column) {
        cli::cli_alert_info("  People column detected - splitting comma-separated values")
        # Split all comma-separated values and get unique individual names
        user_values_split <- unlist(strsplit(as.character(user_values), ","))
        user_values_split <- trimws(user_values_split)
        user_values_split <- user_values_split[user_values_split != ""]
        user_values <- unique(user_values_split)
        cli::cli_alert_info("  Found {length(user_values)} unique individual value(s) after splitting")
      } else {
        cli::cli_alert_info("  Found {length(user_values)} unique value(s)")
      }

      if (length(user_values) == 0) {
        cli::cli_alert_info("  Skipping (no values)")
        next  # No values to match
      }

      total_values <- total_values + length(user_values)

      # Get lookup info
      cli::cli_alert_info("  Getting lookup info...")
      lookup_info <- .get_lookup_info(db_col, con)
      cli::cli_alert_info("  Lookup info: function_name={lookup_info$function_name %||% 'NULL'}, value_col={lookup_info$value_col %||% 'NULL'}")

      if (is.null(lookup_info$function_name)) {
        # Special handling for people columns (no function, query directly)
        cli::cli_alert_info("  Querying table_colnam directly...")
        db_values <- DBI::dbGetQuery(con, "
          SELECT id_table_colnam, colnam
          FROM table_colnam
        ")
        cli::cli_alert_info("  Retrieved {nrow(db_values)} row(s)")
      } else {
        # Use existing function to get lookup data
        cli::cli_alert_info("  Calling lookup function: {lookup_info$function_name}")
        db_values <- tryCatch({
          do.call(lookup_info$function_name, list())
        }, error = function(e) {
          cli::cli_alert_warning("Failed to fetch {db_col} lookup table: {e$message}")
          return(NULL)
        })
        cli::cli_alert_info("  Retrieved {nrow(db_values) %||% 0} row(s)")
      }

      if (is.null(db_values)) {
        cli::cli_alert_info("  Skipping (db_values is NULL)")
        non_exact_matches[[db_col]] <- as.character(user_values)
        next
      }

      # Check for exact matches
      value_col <- lookup_info$value_col
      cli::cli_alert_info("  Checking value_col: {value_col %||% 'NULL'}")
      cli::cli_alert_info("  db_values columns: {paste(names(db_values), collapse=', ')}")

      # Ensure value_col is valid
      if (is.null(value_col) || !value_col %in% names(db_values)) {
        cli::cli_alert_warning("{db_col}: Invalid lookup configuration (value_col missing)")
        non_exact_matches[[db_col]] <- as.character(user_values)
        next
      }

      cli::cli_alert_info("  Normalizing values...")
      db_values_normalized <- tolower(trimws(db_values[[value_col]]))
      user_values_normalized <- tolower(trimws(user_values))

      cli::cli_alert_info("  Comparing values...")
      exact <- user_values[user_values_normalized %in% db_values_normalized]
      non_exact <- user_values[!user_values_normalized %in% db_values_normalized]

      exact_matches[[db_col]] <- as.character(exact)
      non_exact_matches[[db_col]] <- as.character(non_exact)

      total_exact <- total_exact + length(exact)

      if (length(exact) > 0) {
        cli::cli_alert_success("{db_col}: {length(exact)} exact match(es)")
      }
      if (length(non_exact) > 0) {
        cli::cli_alert_warning("{db_col}: {length(non_exact)} value(s) need matching")
      }

    }, error = function(e) {
      cli::cli_alert_danger("ERROR processing {db_col}: {e$message}")
      cli::cli_alert_danger("  Error class: {class(e)[1]}")
      cli::cli_alert_danger("  Call: {deparse(e$call)}")
      # Re-throw to be caught by outer tryCatch
      stop(e)
    })
  }

  list(
    exact_matches = exact_matches,
    non_exact_matches = non_exact_matches,
    summary = list(
      total_values = total_values,
      exact_matches = total_exact,
      non_exact = total_values - total_exact
    )
  )
}
