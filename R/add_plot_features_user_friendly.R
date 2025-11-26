# User-Friendly Plot Features Addition
#
# This file provides a user-friendly interface for adding subplot features to
# existing plots. It wraps the low-level add_subplot_features() function with
# intelligent column mapping, validation, and clear user feedback.

#' Add Plot Features to Existing Plots
#'
#' User-friendly function to add subplot features (census dates, team members,
#' plot characteristics, etc.) to existing plots in the database. Automatically
#' maps column names to database feature types and validates data before import.
#'
#' @param data Data frame containing plot features to add. Must include:
#'   - A plot identifier column (either `plot_name` or `id_liste_plots`)
#'   - One or more feature columns (e.g., `team_leader`, `census_date`, etc.)
#'
#' @param plot_id_column Character: Name of the column containing plot identifiers.
#'   Options:
#'   - `"plot_name"` (default): Plot names as strings
#'   - `"id_liste_plots"`: Plot IDs as integers
#'   If `NULL`, the function will try to detect it automatically.
#'
#' @param column_mapping Named list: Optional pre-defined mapping of user column
#'   names to subplot feature types. If `NULL`, interactive mapping will be used.
#'   Example: `list(PI = "principal_investigator", TeamLead = "team_leader")`
#'
#' @param interactive Logical: If `TRUE` (default), use interactive prompts for
#'   column mapping. Set to `FALSE` for non-interactive/scripted usage.
#'
#' @param dry_run Logical: If `TRUE`, preview changes without committing to database.
#'   Always recommended to run with `dry_run = TRUE` first! (Default: `TRUE`)
#'
#' @param con Database connection. If `NULL`, will create a new connection.
#'
#' @param similarity_threshold Numeric: Threshold for fuzzy column name matching
#'   (0 to 1). Default: 0.6. Higher values require closer matches.
#'
#' @param ask_before_update Logical: If `TRUE`, ask for confirmation before
#'   updating existing records. Default: `TRUE`.
#'
#' @param verbose Logical: If `TRUE`, show detailed progress messages. Default: `TRUE`.
#'
#' @return List with import results:
#'   \item{success}{Logical: TRUE if import succeeded}
#'   \item{n_rows}{Number of feature records added}
#'   \item{n_plots}{Number of plots affected}
#'   \item{plot_names}{Vector of plot names affected}
#'   \item{feature_types}{Vector of feature types added}
#'   \item{mapping}{Column mapping used}
#'   \item{dry_run}{Was this a dry-run?}
#'   \item{message}{Summary message}
#'
#' @details
#' **What are subplot features?**
#'
#' Subplot features are attributes that describe plots or census events:
#' - **People**: `team_leader`, `principal_investigator`, `data_manager`, etc.
#' - **Dates**: `census_date` (year/month/day columns)
#' - **Plot characteristics**: Various plot-specific measurements
#'
#' Use `subplot_list()` to see all available subplot feature types and their
#' definitions.
#'
#' **Data Structure:**
#'
#' Your data should have one row per feature instance:
#' ```
#' plot_name | team_leader | principal_investigator | census_year
#' ----------|-------------|------------------------|------------
#' Plot-A    | John Doe    | Dr. Smith             | 2020
#' Plot-B    | Jane Smith  | Dr. Smith             | 2020
#' ```
#'
#' **Column Mapping:**
#'
#' The function intelligently maps your column names to database feature types:
#' 1. Exact match (e.g., `team_leader` → `team_leader`)
#' 2. Synonym match (e.g., `PI` → `principal_investigator`)
#' 3. Fuzzy string match (e.g., `TeamLeader` → `team_leader`)
#' 4. Interactive selection (if `interactive = TRUE`)
#'
#' **People Fields:**
#'
#' For people-related features (team_leader, PI, etc.), names will be:
#' - Matched against existing people in `table_colnam`
#' - You can add new people interactively during import
#' - Multiple names can be comma-separated (e.g., "John Doe, Jane Smith")
#'
#' @examples
#' \dontrun{
#' library(CafriplotsR)
#'
#' # Example 1: Add team leader and PI to plots
#' plot_features <- data.frame(
#'   plot_name = c("Plot-A", "Plot-B", "Plot-C"),
#'   team_leader = c("John Doe", "Jane Smith", "Bob Wilson"),
#'   principal_investigator = c("Dr. Smith", "Dr. Smith", "Dr. Jones"),
#'   census_year = c(2020, 2020, 2021)
#' )
#'
#' # Dry run first (preview)
#' result <- add_plot_features(
#'   data = plot_features,
#'   dry_run = TRUE
#' )
#'
#' # Check what would be added
#' print(result)
#'
#' # If satisfied, actually add the features
#' result <- add_plot_features(
#'   data = plot_features,
#'   dry_run = FALSE
#' )
#'
#' # Example 2: Add features with custom column names
#' my_data <- data.frame(
#'   PlotName = c("Plot-A", "Plot-B"),
#'   TeamLead = c("John Doe", "Jane Smith"),
#'   PI = c("Dr. Smith", "Dr. Jones")
#' )
#'
#' # Interactive mapping will help match columns
#' result <- add_plot_features(
#'   data = my_data,
#'   interactive = TRUE,
#'   dry_run = TRUE
#' )
#'
#' # Example 3: Non-interactive with pre-defined mapping
#' mapping <- list(
#'   PlotName = "plot_name",
#'   TeamLead = "team_leader",
#'   PI = "principal_investigator"
#' )
#'
#' result <- add_plot_features(
#'   data = my_data,
#'   column_mapping = mapping,
#'   interactive = FALSE,
#'   dry_run = FALSE
#' )
#'
#' # Example 4: See available subplot features
#' available_features <- subplot_list()
#' View(available_features)
#' }
#'
#' @seealso
#' [subplot_list()] to see available subplot feature types
#' [add_subplot_features()] for the low-level function
#' [query_subplot_features()] to query existing features
#'
#' @export
add_plot_features <- function(data,
                              plot_id_column = NULL,
                              column_mapping = NULL,
                              interactive = TRUE,
                              dry_run = TRUE,
                              con = NULL,
                              similarity_threshold = 0.6,
                              ask_before_update = TRUE,
                              verbose = TRUE) {

  # Initialize connection if needed
  close_on_exit <- FALSE
  if (is.null(con)) {
    con <- call.mydb()
    close_on_exit <- TRUE
  }

  if (verbose) {
    cli::cli_h1("Adding Plot Features")
    cli::cli_alert_info("Mode: {ifelse(dry_run, 'DRY RUN (preview only)', 'ACTUAL IMPORT')}")
    cli::cli_alert_info("Rows to process: {nrow(data)}")
  }

  # Step 1: Identify plot ID column
  if (verbose) cli::cli_h2("Step 1: Identifying plot ID column")

  plot_id_result <- .identify_plot_id_column(
    data = data,
    plot_id_column = plot_id_column,
    interactive = interactive,
    verbose = verbose
  )

  plot_id_column <- plot_id_result$column
  plot_id_type <- plot_id_result$type

  if (verbose) {
    cli::cli_alert_success("Using '{plot_id_column}' as plot identifier ({plot_id_type})")
  }

  # Step 2: Map columns to subplot feature types
  if (verbose) cli::cli_h2("Step 2: Mapping columns to subplot features")

  mapping_result <- .map_subplot_feature_columns(
    data = data,
    plot_id_column = plot_id_column,
    column_mapping = column_mapping,
    con = con,
    interactive = interactive,
    similarity_threshold = similarity_threshold,
    verbose = verbose
  )

  column_mappings <- mapping_result$mappings
  feature_columns <- names(column_mappings)

  if (length(feature_columns) == 0) {
    if (close_on_exit) DBI::dbDisconnect(con)
    stop("No feature columns identified. Provide data with at least one subplot feature column.")
  }

  if (verbose) {
    cli::cli_alert_success("Mapped {length(feature_columns)} feature column(s):")
    for (user_col in feature_columns) {
      cli::cli_alert_info("  {user_col} → {column_mappings[[user_col]]}")
    }
  }

  # Step 3: Validate data
  if (verbose) cli::cli_h2("Step 3: Validating data")

  validation_result <- .validate_plot_features_data(
    data = data,
    plot_id_column = plot_id_column,
    plot_id_type = plot_id_type,
    column_mappings = column_mappings,
    con = con,
    verbose = verbose
  )

  if (!validation_result$valid) {
    if (verbose) {
      cli::cli_alert_danger("Validation failed!")
      for (err in validation_result$errors) {
        cli::cli_alert_danger("  {err}")
      }
    }
    if (close_on_exit) DBI::dbDisconnect(con)
    stop("Data validation failed. Fix errors and try again.")
  }

  if (verbose) {
    cli::cli_alert_success("Validation passed!")
    if (length(validation_result$warnings) > 0) {
      for (warn in validation_result$warnings) {
        cli::cli_alert_warning("  {warn}")
      }
    }
  }

  # Step 4: Prepare data for each feature type
  if (verbose) cli::cli_h2("Step 4: Preparing features for import")

  prepared_data <- .prepare_subplot_features(
    data = data,
    plot_id_column = plot_id_column,
    plot_id_type = plot_id_type,
    column_mappings = column_mappings,
    con = con,
    interactive = interactive,
    dry_run = dry_run,
    verbose = verbose
  )

  if (dry_run) {
    if (verbose) {
      cli::cli_h2("Step 5: Preview - Would Add Features")
      for (feature_type in names(prepared_data$features)) {
        feature_df <- prepared_data$features[[feature_type]]
        cli::cli_alert_info("Would add {nrow(feature_df)} record(s) for feature: {feature_type}")
        if (nrow(feature_df) > 0 && nrow(feature_df) <= 5) {
          cat("\nPreview:\n")
          print(utils::head(feature_df, 5))
          cat("\n")
        }
      }
      cli::cli_alert_success("Dry run completed - no changes made")
      cli::cli_alert_info("Run with dry_run = FALSE to actually import")
    }

    result <- list(
      success = TRUE,
      n_rows = sum(sapply(prepared_data$features, nrow)),
      n_plots = length(unique(data[[plot_id_column]])),
      plot_names = if (plot_id_type == "name") unique(data[[plot_id_column]]) else NULL,
      feature_types = names(prepared_data$features),
      mapping = column_mappings,
      dry_run = TRUE,
      message = sprintf("Dry run completed. Would add %d feature records across %d plot(s).",
                       sum(sapply(prepared_data$features, nrow)),
                       length(unique(data[[plot_id_column]])))
    )

  } else {

    # Step 5: Actually add features using add_subplot_features()
    if (verbose) cli::cli_h2("Step 5: Adding features to database")

    import_results <- list()

    for (feature_type in names(prepared_data$features)) {
      feature_df <- prepared_data$features[[feature_type]]

      if (nrow(feature_df) == 0) {
        if (verbose) cli::cli_alert_info("Skipping {feature_type} (no records)")
        next
      }

      if (verbose) {
        cli::cli_alert_info("Adding {nrow(feature_df)} record(s) for: {feature_type}")
      }

      tryCatch({
        add_subplot_features(
          new_data = feature_df,
          id_plot_name = "id_liste_plots",
          subplottype_field = feature_type,
          add_data = TRUE,
          ask_before_update = ask_before_update,
          verbose = FALSE,  # Suppress internal verbose output
          con = con
        )

        import_results[[feature_type]] <- list(
          success = TRUE,
          n_records = nrow(feature_df)
        )

        if (verbose) {
          cli::cli_alert_success("✓ Added {nrow(feature_df)} {feature_type} record(s)")
        }

      }, error = function(e) {
        import_results[[feature_type]] <- list(
          success = FALSE,
          error = e$message
        )

        if (verbose) {
          cli::cli_alert_danger("✗ Failed to add {feature_type}: {e$message}")
        }
      })
    }

    # Summarize results
    n_success <- sum(sapply(import_results, function(x) x$success))
    n_failed <- length(import_results) - n_success
    total_records <- sum(sapply(import_results[sapply(import_results, function(x) x$success)],
                                function(x) x$n_records))

    if (verbose) {
      cli::cli_rule()
      if (n_failed == 0) {
        cli::cli_alert_success("✓ Import completed successfully!")
        cli::cli_alert_info("Added {total_records} feature record(s) across {n_success} feature type(s)")
      } else {
        cli::cli_alert_warning("Import completed with {n_failed} failure(s)")
        cli::cli_alert_info("Successfully added {total_records} record(s) for {n_success} feature type(s)")
      }
    }

    result <- list(
      success = n_failed == 0,
      n_rows = total_records,
      n_plots = length(unique(data[[plot_id_column]])),
      plot_names = if (plot_id_type == "name") unique(data[[plot_id_column]]) else NULL,
      feature_types = names(prepared_data$features),
      mapping = column_mappings,
      import_results = import_results,
      dry_run = FALSE,
      message = sprintf("Added %d feature records for %d feature type(s) across %d plot(s).",
                       total_records, n_success, length(unique(data[[plot_id_column]])))
    )
  }

  # Close connection if we opened it
  if (close_on_exit) {
    DBI::dbDisconnect(con)
  }

  if (verbose) cat("\n")

  return(invisible(result))
}


#' Identify Plot ID Column (Internal Helper)
#'
#' @keywords internal
.identify_plot_id_column <- function(data, plot_id_column, interactive, verbose) {

  # If user specified column, validate it
  if (!is.null(plot_id_column)) {
    if (!plot_id_column %in% names(data)) {
      stop(sprintf("Specified plot_id_column '%s' not found in data. Available columns: %s",
                   plot_id_column, paste(names(data), collapse = ", ")))
    }

    # Determine type
    if (plot_id_column == "id_liste_plots" || is.numeric(data[[plot_id_column]])) {
      return(list(column = plot_id_column, type = "id"))
    } else {
      return(list(column = plot_id_column, type = "name"))
    }
  }

  # Auto-detect plot ID column
  # Check for exact matches first
  if ("plot_name" %in% names(data)) {
    return(list(column = "plot_name", type = "name"))
  }

  if ("id_liste_plots" %in% names(data)) {
    return(list(column = "id_liste_plots", type = "id"))
  }

  # Check for fuzzy matches
  plot_name_candidates <- names(data)[grepl("plot.*name|name.*plot", names(data), ignore.case = TRUE)]
  plot_id_candidates <- names(data)[grepl("plot.*id|id.*plot", names(data), ignore.case = TRUE)]

  if (length(plot_name_candidates) == 1) {
    return(list(column = plot_name_candidates[1], type = "name"))
  }

  if (length(plot_id_candidates) == 1) {
    return(list(column = plot_id_candidates[1], type = "id"))
  }

  # Interactive selection
  if (interactive) {
    if (verbose) {
      cli::cli_alert_warning("Could not auto-detect plot ID column")
      cli::cli_alert_info("Available columns: {paste(names(data), collapse = ', ')}")
    }

    answer <- readline(prompt = "Which column contains plot identifiers (plot_name or id_liste_plots)? ")

    if (answer %in% names(data)) {
      type <- if (answer == "id_liste_plots" || is.numeric(data[[answer]])) "id" else "name"
      return(list(column = answer, type = type))
    } else {
      stop(sprintf("Column '%s' not found in data.", answer))
    }
  }

  # If we get here, couldn't detect
  stop("Could not identify plot ID column. Please specify 'plot_id_column' parameter or include a column named 'plot_name' or 'id_liste_plots'.")
}


#' Map Subplot Feature Columns (Internal Helper)
#'
#' Maps user column names to database subplot feature types using:
#' 1. Exact matching
#' 2. Synonym matching (reusing existing .get_column_synonyms())
#' 3. Fuzzy string matching
#' 4. Interactive selection (if enabled)
#'
#' @keywords internal
.map_subplot_feature_columns <- function(data, plot_id_column, column_mapping,
                                        con, interactive, similarity_threshold, verbose) {

  # Get available subplot features from database
  subplot_features <- subplot_list(con = con)

  # Available feature types
  valid_feature_types <- subplot_features$type

  # Columns to map (exclude plot ID column)
  columns_to_map <- setdiff(names(data), c(plot_id_column))

  # If user provided mapping, validate it
  if (!is.null(column_mapping)) {
    # Validate that all mapped-to values are valid feature types
    invalid_mappings <- setdiff(unlist(column_mapping), valid_feature_types)
    if (length(invalid_mappings) > 0) {
      stop(sprintf("Invalid feature types in column_mapping: %s. Use subplot_list() to see valid types.",
                   paste(invalid_mappings, collapse = ", ")))
    }

    # Validate that all user columns exist
    missing_cols <- setdiff(names(column_mapping), names(data))
    if (length(missing_cols) > 0) {
      stop(sprintf("Columns in column_mapping not found in data: %s",
                   paste(missing_cols, collapse = ", ")))
    }

    return(list(mappings = column_mapping))
  }

  # Otherwise, map automatically
  mappings <- list()

  # Get synonyms for subplot features
  # Reuse existing synonym infrastructure if available
  feature_synonyms <- .get_subplot_feature_synonyms()

  for (col in columns_to_map) {

    # Skip date components if they'll be handled by census_date
    if (col %in% c("year", "month", "day", "census_year", "census_month", "census_day")) {
      next  # These will be handled separately
    }

    # 1. Exact match
    if (col %in% valid_feature_types) {
      mappings[[col]] <- col
      next
    }

    # 2. Synonym match
    synonym_match <- .find_synonym_match(col, feature_synonyms)
    if (!is.null(synonym_match) && synonym_match %in% valid_feature_types) {
      mappings[[col]] <- synonym_match
      next
    }

    # 3. Fuzzy string match
    fuzzy_match <- .find_fuzzy_match(col, valid_feature_types, similarity_threshold)
    if (!is.null(fuzzy_match)) {
      if (interactive) {
        answer <- readline(prompt = sprintf("Map '%s' to '%s'? (y/n): ", col, fuzzy_match))
        if (tolower(trimws(answer)) == "y") {
          mappings[[col]] <- fuzzy_match
          next
        }
      } else {
        mappings[[col]] <- fuzzy_match
        next
      }
    }

    # 4. Interactive selection
    if (interactive) {
      if (verbose) {
        cli::cli_alert_warning("Could not auto-map column: '{col}'")
      }

      cat("\nSelect the subplot feature type for column '", col, "':\n", sep = "")
      cat("  0. Skip this column\n")

      # Show top 10 most relevant features
      relevant_features <- utils::head(valid_feature_types, 10)
      for (i in seq_along(relevant_features)) {
        cat(sprintf("  %d. %s\n", i, relevant_features[i]))
      }
      cat("  99. Show all features\n")

      choice <- readline(prompt = "Your choice: ")
      choice <- suppressWarnings(as.integer(choice))

      if (!is.na(choice)) {
        if (choice == 0) {
          next  # Skip
        } else if (choice == 99) {
          cat("\nAll available subplot features:\n")
          for (i in seq_along(valid_feature_types)) {
            cat(sprintf("  %d. %s\n", i, valid_feature_types[i]))
          }
          choice <- readline(prompt = "Your choice: ")
          choice <- suppressWarnings(as.integer(choice))
          if (!is.na(choice) && choice > 0 && choice <= length(valid_feature_types)) {
            mappings[[col]] <- valid_feature_types[choice]
          }
        } else if (choice > 0 && choice <= length(relevant_features)) {
          mappings[[col]] <- relevant_features[choice]
        }
      }
    }
  }

  return(list(mappings = mappings))
}


#' Get Subplot Feature Synonyms (Internal Helper)
#'
#' Returns common synonyms for subplot feature types.
#'
#' @keywords internal
.get_subplot_feature_synonyms <- function() {
  list(
    # People features
    team_leader = c("teamleader", "team_lead", "teamlead", "leader", "chef_equipe"),
    principal_investigator = c("pi", "princ_invest", "investigator", "lead_researcher"),
    data_manager = c("datamanager", "data_mgr", "manager", "gestionnaire"),
    data_provider = c("dataprovider", "provider", "fournisseur"),
    additional_people = c("additional_person", "other_people", "others"),

    # Date features
    census_date = c("date", "census", "survey_date", "date_census"),

    # Other common features
    plot_area = c("area", "surface", "plot_size"),
    vegetation_type = c("vegetation", "veg_type", "forest_type"),
    locality_name = c("locality", "location", "site", "lieu")
  )
}


#' Find Synonym Match (Internal Helper)
#'
#' @keywords internal
.find_synonym_match <- function(col, synonyms) {
  col_lower <- tolower(trimws(col))

  for (feature_type in names(synonyms)) {
    if (col_lower %in% tolower(synonyms[[feature_type]])) {
      return(feature_type)
    }
  }

  return(NULL)
}


#' Find Fuzzy String Match (Internal Helper)
#'
#' Uses string distance to find best match.
#'
#' @keywords internal
.find_fuzzy_match <- function(col, valid_values, threshold = 0.6) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    return(NULL)
  }

  col_lower <- tolower(trimws(col))
  valid_lower <- tolower(valid_values)

  # Calculate Jaro-Winkler distance (0 = identical, 1 = completely different)
  distances <- stringdist::stringdist(col_lower, valid_lower, method = "jw")

  # Convert to similarity (1 = identical, 0 = completely different)
  similarities <- 1 - distances

  best_match_idx <- which.max(similarities)
  best_similarity <- similarities[best_match_idx]

  if (best_similarity >= threshold) {
    return(valid_values[best_match_idx])
  }

  return(NULL)
}


#' Validate Plot Features Data (Internal Helper)
#'
#' @keywords internal
.validate_plot_features_data <- function(data, plot_id_column, plot_id_type,
                                        column_mappings, con, verbose) {

  errors <- character()
  warnings <- character()

  # 1. Check plots exist
  if (plot_id_type == "name") {
    plot_names <- unique(data[[plot_id_column]])

    existing_plots <- tryCatch({
      query_plots(plot_name = plot_names, con = con, exact_match = TRUE)
    }, error = function(e) {
      NULL
    })

    if (is.null(existing_plots) || nrow(existing_plots) == 0) {
      errors <- c(errors, sprintf("No plots found in database for provided plot_name values"))
    } else {
      missing_plots <- setdiff(plot_names, existing_plots$plot_name)
      if (length(missing_plots) > 0) {
        errors <- c(errors, sprintf("Plots not found in database: %s",
                                   paste(missing_plots, collapse = ", ")))
      }
    }
  }

  # 2. Check feature types are valid
  subplot_features <- subplot_list(con = con)
  valid_feature_types <- subplot_features$type

  invalid_features <- setdiff(unlist(column_mappings), valid_feature_types)
  if (length(invalid_features) > 0) {
    errors <- c(errors, sprintf("Invalid subplot feature types: %s",
                               paste(invalid_features, collapse = ", ")))
  }

  # 3. Check for required date components if date features present
  # (year, month, day columns may be needed)
  # This is handled by add_subplot_features which adds NA if missing

  # 4. Warn about empty values
  for (user_col in names(column_mappings)) {
    n_empty <- sum(is.na(data[[user_col]]) | trimws(as.character(data[[user_col]])) == "")
    if (n_empty > 0) {
      warnings <- c(warnings,
                   sprintf("Column '%s' has %d empty/NA values (will be skipped)", user_col, n_empty))
    }
  }

  valid <- length(errors) == 0

  return(list(
    valid = valid,
    errors = errors,
    warnings = warnings
  ))
}


#' Prepare Subplot Features for Import (Internal Helper)
#'
#' Prepares data for each feature type, handling:
#' - Plot name → plot ID linking
#' - People name → id_table_colnam linking (for people features)
#' - Data formatting for add_subplot_features()
#'
#' @keywords internal
.prepare_subplot_features <- function(data, plot_id_column, plot_id_type,
                                     column_mappings, con, interactive,
                                     dry_run, verbose) {

  # Link plots to get IDs if using plot_name
  if (plot_id_type == "name") {
    plot_ids <- data.frame(
      plot_name = unique(data[[plot_id_column]]),
      stringsAsFactors = FALSE
    )

    # Get plot IDs from database
    plot_id_lookup <- try_open_postgres_table(table = "data_liste_plots", con = con) %>%
      dplyr::select(plot_name, id_liste_plots) %>%
      dplyr::filter(plot_name %in% !!plot_ids$plot_name) %>%
      dplyr::collect()

    data_with_ids <- data %>%
      dplyr::left_join(plot_id_lookup, by = setNames("plot_name", plot_id_column))
  } else {
    # Already have IDs
    data_with_ids <- data %>%
      dplyr::rename(id_liste_plots = !!rlang::sym(plot_id_column))
  }

  # Get subplot feature info
  subplot_features <- subplot_list(con = con)

  # Prepare data for each feature type
  features_list <- list()

  for (user_col in names(column_mappings)) {
    feature_type <- column_mappings[[user_col]]

    # Get feature info
    feature_info <- subplot_features %>%
      dplyr::filter(type == feature_type)

    if (nrow(feature_info) == 0) {
      if (verbose) {
        cli::cli_alert_warning("Feature type '{feature_type}' not found in subplot_list, skipping")
      }
      next
    }

    value_type <- feature_info$valuetype[1]

    # Extract non-empty values
    feature_data <- data_with_ids %>%
      dplyr::select(id_liste_plots, !!rlang::sym(user_col)) %>%
      dplyr::filter(!is.na(!!rlang::sym(user_col)) & trimws(as.character(!!rlang::sym(user_col))) != "")

    if (nrow(feature_data) == 0) {
      if (verbose) {
        cli::cli_alert_info("No data for feature '{feature_type}', skipping")
      }
      next
    }

    # Rename column to feature type name
    feature_data <- feature_data %>%
      dplyr::rename(!!rlang::sym(feature_type) := !!rlang::sym(user_col))

    # Special handling for people features (valuetype == "table_colnam")
    if (value_type == "table_colnam") {

      if (verbose) {
        cli::cli_alert_info("Processing people feature: {feature_type}")
      }

      # Separate comma-separated names and link to table_colnam
      feature_data <- feature_data %>%
        tidyr::separate_rows(!!rlang::sym(feature_type), sep = ",") %>%
        dplyr::mutate(!!rlang::sym(feature_type) := stringr::str_squish(!!rlang::sym(feature_type))) %>%
        dplyr::filter(!!rlang::sym(feature_type) != "")

      if (!dry_run) {
        # Use .link_colnam() to match/add people
        feature_data <- .link_colnam(
          data_stand = feature_data,
          column_searched = feature_type,
          column_name = "colnam",
          id_field = feature_type,
          id_table_name = "id_table_colnam",
          db_connection = con,
          table_name = "table_colnam"
        )

        # .link_colnam() returns a column named 'feature_type' with IDs
        # and 'original_colnam' with original values
        # Remove the original_colnam column if it exists
        if ("original_colnam" %in% names(feature_data)) {
          feature_data <- feature_data %>%
            dplyr::select(-original_colnam)
        }
      } else {
        # For dry run, just add placeholder ID
        feature_data <- feature_data %>%
          dplyr::mutate(!!rlang::sym(paste0(feature_type, "_id")) := 999)
      }
    }

    features_list[[feature_type]] <- feature_data
  }

  return(list(features = features_list))
}


#' Print method for add_plot_features result
#'
#' @param x Result object from add_plot_features()
#' @param ... Additional arguments (ignored)
#'
#' @export
print.plot_features_result <- function(x, ...) {
  cat("\n")
  cat(cli::rule(
    left = "Add Plot Features Result",
    right = ifelse(x$success, "SUCCESS", "FAILED")
  ))
  cat("\n\n")

  cat(sprintf("Status: %s\n", ifelse(x$success, cli::col_green("✓ Success"), cli::col_red("✗ Failed"))))
  cat(sprintf("Mode: %s\n", ifelse(x$dry_run, "Dry Run (Preview)", "Actual Import")))
  cat(sprintf("Feature records: %d\n", x$n_rows))
  cat(sprintf("Plots affected: %d\n", x$n_plots))
  cat(sprintf("Feature types: %s\n", paste(x$feature_types, collapse = ", ")))

  if (!is.null(x$plot_names) && length(x$plot_names) <= 20) {
    cat(sprintf("Plot names: %s\n", paste(x$plot_names, collapse = ", ")))
  }

  cat("\n")
  cat("Column mappings:\n")
  for (user_col in names(x$mapping)) {
    cat(sprintf("  %s → %s\n", user_col, x$mapping[[user_col]]))
  }

  cat("\n")
  cat(x$message)
  cat("\n\n")

  if (x$dry_run) {
    cat(cli::col_yellow("This was a dry run. Run with dry_run = FALSE to actually import.\n"))
  }

  cat(cli::rule())
  cat("\n")

  invisible(x)
}
