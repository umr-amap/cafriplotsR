# Import Wizard - Step 4: Lookup Matching
#
# Proactive module for matching user lookup values to database before validation

#' Step 4 Module: Lookup Matching - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
mod_step4_lookup_matching_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("link"),
      i18n$t("Step 4: Match Lookup Values"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      paste0(
        i18n$t("Before validation, let's match your lookup values (methods, countries, people) to the database."),
        " ",
        i18n$t("This step ensures your data references the correct database entries.")
      ),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Analyze button
    shiny::actionButton(
      ns("analyze_lookups"),
      shiny::tagList(shiny::icon("search"), paste0(" ", i18n$t("Analyze Lookup Values"))),
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
#' @param config Reactive containing import configuration
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object from shiny.i18n
#' @return Reactive containing matched/updated data
#' @keywords internal
mod_step4_lookup_matching_server <- function(id, data, mappings, config, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for analysis and matched data
    analysis_result <- shiny::reactiveVal(NULL)
    non_exact_matches <- shiny::reactiveVal(NULL)
    matched_data <- shiny::reactiveVal(NULL)
    matching_complete <- shiny::reactiveVal(FALSE)

    # Analyze lookup values when button clicked
    shiny::observeEvent(input$analyze_lookups, {
      shiny::req(data(), mappings(), con(), config())

      # Check if there are any lookup columns to match
      has_lookup_columns <- !is.null(config()$metadata_mappings) && length(config()$metadata_mappings) > 0

      if (!has_lookup_columns) {
        # No lookup columns needed - skip directly to validation
        cli::cli_alert_info("No lookup columns to match - proceeding directly")
        matched_data(data())
        matching_complete(TRUE)

        shiny::showNotification(
          i18n()$t("No lookup matching needed for this import type. You can proceed to validation."),
          type = "message",
          duration = 5
        )
        return()
      }

      shiny::withProgress({
        shiny::setProgress(0.3, message = "Analyzing lookup values...")

        cli::cli_alert_info("Analyzing lookup columns in data...")

        # Analyze all lookup columns
        result <- tryCatch({
          .analyze_lookup_columns(data(), mappings(), con(), config())
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

          # Fix: Safely compute total, handling empty lists
          total_non_exact <- if (length(needs_matching) == 0) {
            0
          } else {
            sum(sapply(needs_matching, length))
          }

          cli::cli_alert_success(
            "Analysis complete: {result$summary$total_values} total values, ",
            "{result$summary$exact_matches} exact matches, ",
            "{total_non_exact} need matching"
          )

          if (total_non_exact == 0) {
            # All values matched - convert exact matches to IDs
            cli::cli_alert_info("All values matched exactly - converting to IDs...")
            updated_data <- .convert_and_clean_lookup_values(
              data = data(),
              exact_matches = result$exact_matches,
              mappings = mappings(),
              con = con()
            )
            matched_data(updated_data)
            matching_complete(TRUE)

            shiny::showNotification(
              "Great! All lookup values match exactly. You can proceed to validation.",
              type = "message",
              duration = 5
            )
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

        # Also convert exact matches to IDs and clean up any remaining unmatched values
        cli::cli_alert_info("Converting exact matches and cleaning up...")
        final_data <- .convert_and_clean_lookup_values(
          data = updated_data,
          exact_matches = analysis_result()$exact_matches,
          mappings = mappings(),
          con = con()
        )

        matched_data(final_data)
        matching_complete(TRUE)

        shiny::showNotification(
          sprintf("Successfully matched %d value(s)! You can proceed to validation.", length(unlist(matches))),
          type = "message",
          duration = 5
        )

      } else {
        # User skipped matching - still convert exact matches and discard unmatched
        cli::cli_alert_info("User skipped matching - converting exact matches only...")
        updated_data <- .convert_and_clean_lookup_values(
          data = data(),
          exact_matches = analysis_result()$exact_matches,
          mappings = mappings(),
          con = con()
        )

        matched_data(updated_data)
        matching_complete(TRUE)

        shiny::showNotification(
          "Converted exact matches to IDs. Unmatched values were discarded.",
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
.analyze_lookup_columns <- function(data, mappings, con, config = NULL) {

  # Determine lookup columns from config's metadata_mappings
  # This ensures we only process columns that actually need lookup matching
  if (!is.null(config) && !is.null(config$metadata_mappings)) {
    all_lookup_columns <- names(config$metadata_mappings)
    cli::cli_alert_info("Lookup columns from config: {paste(all_lookup_columns, collapse=', ')}")

    # Identify which are feature-level lookups (people columns that need comma-splitting)
    # These use table_colnam lookup table
    feature_lookups <- names(config$metadata_mappings)[
      sapply(config$metadata_mappings, function(x) !is.null(x$lookup_table) && x$lookup_table == "table_colnam")
    ]
    cli::cli_alert_info("Feature-level lookup columns: {paste(feature_lookups, collapse=', ')}")
  } else {
    # Fallback: Legacy behavior (plots import without config)
    cli::cli_alert_info("No config provided, using legacy lookup detection")

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
  }

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


#' Convert matched lookup names to IDs and discard unmatched values
#'
#' Converts exact-matched names to their database IDs and removes any unmatched
#' values to ensure data contains ONLY numeric IDs (no mix of IDs and text names).
#'
#' @param data Data frame with user data
#' @param exact_matches List of exact-matched values (output from .analyze_lookup_columns)
#' @param mappings Column mappings (user_col -> db_col)
#' @param con Database connection pool
#' @return Updated data frame with names replaced by IDs and unmatched values removed
#' @keywords internal
.convert_and_clean_lookup_values <- function(data, exact_matches, mappings, con) {

  if (is.null(exact_matches) || length(exact_matches) == 0) {
    cli::cli_alert_info("No exact matches to convert")
    return(data)
  }

  cli::cli_alert_info("Converting exact matches to IDs and discarding unmatched values...")

  updated_data <- data

  # Get reverse mappings (db_col -> user_col)
  reverse_mappings <- base::setNames(names(mappings), unlist(mappings))

  for (col_name in names(exact_matches)) {
    exact_values <- exact_matches[[col_name]]

    if (length(exact_values) == 0) next

    # Get user column name
    user_col <- reverse_mappings[[col_name]]

    if (is.null(user_col) || !user_col %in% names(updated_data)) {
      cli::cli_alert_warning("Skipping {col_name} - column not found in data")
      next
    }

    cli::cli_alert_info("Processing {col_name}: {length(exact_values)} exact match(es)")

    # Get lookup info
    lookup_info <- .get_lookup_info(col_name, con)

    # Query database for name -> ID mapping
    if (is.null(lookup_info$function_name)) {
      # People columns: query table_colnam
      db_lookup <- DBI::dbGetQuery(con, "
        SELECT id_table_colnam AS id, colnam AS name
        FROM table_colnam
      ")
    } else if (col_name == "method") {
      db_lookup <- DBI::dbGetQuery(con, "
        SELECT id_method AS id, method AS name
        FROM methodslist
      ")
    } else if (col_name == "country") {
      db_lookup <- DBI::dbGetQuery(con, "
        SELECT id_country AS id, country AS name
        FROM table_countries
      ")
    } else {
      cli::cli_alert_warning("Unknown lookup type for {col_name}")
      next
    }

    # Create normalized name -> ID mapping (case-insensitive)
    name_to_id <- base::setNames(
      db_lookup$id,
      tolower(trimws(db_lookup$name))
    )

    # Check if this is a people column (can have comma-separated values)
    is_people_column <- tryCatch({
      subplot_info <- subplot_list(con)
      if (!is.null(subplot_info) && "type" %in% names(subplot_info) && "valuetype" %in% names(subplot_info)) {
        people_cols <- subplot_info$type[!is.na(subplot_info$valuetype) & subplot_info$valuetype == "table_colnam"]
        people_cols <- people_cols[!is.na(people_cols)]
        col_name %in% people_cols
      } else {
        FALSE
      }
    }, error = function(e) {
      # Fallback: check against known people columns
      col_name %in% c("principal_investigator", "data_manager", "additional_people", "team_leader")
    })

    if (is_people_column) {
      # Handle comma-separated values for people columns
      cli::cli_alert_info("  People column: converting and cleaning")

      discarded_count <- 0

      for (i in seq_len(nrow(updated_data))) {
        cell_value <- updated_data[[user_col]][i]

        if (!is.na(cell_value) && trimws(cell_value) != "") {
          # Split by comma
          names_list <- strsplit(as.character(cell_value), ",")[[1]]
          names_list <- trimws(names_list)

          # Convert names to IDs, discard unmatched
          matched_ids <- c()
          for (name in names_list) {
            name_lower <- tolower(trimws(name))

            # Check if it's already a numeric ID
            if (suppressWarnings(!is.na(as.numeric(name)))) {
              # Already an ID - keep it
              matched_ids <- c(matched_ids, name)
            } else if (name_lower %in% names(name_to_id)) {
              # Matched name - convert to ID
              matched_ids <- c(matched_ids, as.character(name_to_id[[name_lower]]))
            } else {
              # Unmatched - discard
              cli::cli_alert_info("  Discarding unmatched value: '{name}' in row {i}")
              discarded_count <- discarded_count + 1
            }
          }

          # Update cell with matched IDs only
          if (length(matched_ids) > 0) {
            updated_data[[user_col]][i] <- paste(matched_ids, collapse = ", ")
          } else {
            # All values were unmatched - set to NA
            updated_data[[user_col]][i] <- NA
          }
        }
      }

      if (discarded_count > 0) {
        cli::cli_alert_warning("  Discarded {discarded_count} unmatched value(s) from {col_name}")
      }

    } else {
      # Simple replacement for non-people columns (method, country)
      cli::cli_alert_info("  Simple column: converting to IDs")

      for (exact_value in exact_values) {
        value_lower <- tolower(trimws(exact_value))

        if (value_lower %in% names(name_to_id)) {
          matched_id <- name_to_id[[value_lower]]

          # Replace all occurrences (case-insensitive)
          mask <- tolower(trimws(updated_data[[user_col]])) == value_lower
          updated_data[[user_col]][mask] <- as.character(matched_id)

          cli::cli_alert_success("  '{exact_value}' → {matched_id}")
        }
      }
    }

    cli::cli_alert_success("Converted {col_name} - data now contains only IDs")
  }

  return(updated_data)
}
