# Import Functions with Transaction Support
#
# This file contains functions for importing plot metadata with
# transaction support (all-or-nothing imports with automatic rollback).
# Includes row-level security (RLS) admin code generation.

#' Import Plot Metadata with Transaction Support
#'
#' Imports plot metadata into the database using transactions for safety.
#' Reuses existing .link_table() and .link_colnam() for interactive matching.
#' Supports dry-run mode for preview without committing.
#'
#' **IMPORTANT**: Due to row-level security, you won't have access to imported
#' plots until an admin grants permission. The function returns R code that
#' admin needs to run.
#'
#' @param data Data frame containing plot metadata
#' @param column_mappings Named list mapping user columns to schema columns
#'   (from map_user_columns())
#' @param validation Validation result from validate_plot_metadata()
#' @param config Routing configuration from get_import_column_routing()
#' @param con Database connection (optional, will create if NULL)
#' @param dry_run Logical: If TRUE, preview changes without committing (default FALSE)
#' @param interactive Logical: If TRUE, use interactive prompts for matching (default TRUE)
#' @param progress Logical: If TRUE, show progress messages (default TRUE)
#'
#' @return List with import results:
#'   \item{success}{Logical: TRUE if import succeeded}
#'   \item{plot_names}{Vector of plot_name values imported}
#'   \item{n_plots}{Number of plots imported}
#'   \item{username}{Username who performed import}
#'   \item{admin_code}{R code for admin to grant access}
#'   \item{dry_run}{Was this a dry-run?}
#'   \item{message}{Summary message}
#'
#' @examples
#' \dontrun{
#' # Complete workflow
#' config <- get_import_column_routing("plots")
#' mapping <- map_user_columns(my_data, config)
#' validation <- validate_plot_metadata(my_data, mapping$mappings, config)
#'
#' if (!validation$valid) {
#'   stop("Fix validation errors first!")
#' }
#'
#' # Dry run first
#' preview <- import_plot_metadata(
#'   data = my_data,
#'   column_mappings = mapping$mappings,
#'   validation = validation,
#'   config = config,
#'   dry_run = TRUE
#' )
#'
#' # Actual import
#' result <- import_plot_metadata(
#'   data = my_data,
#'   column_mappings = mapping$mappings,
#'   validation = validation,
#'   config = config,
#'   dry_run = FALSE
#' )
#'
#' # Send admin code to database administrator
#' cat(result$admin_code)
#' # Or save to file
#' writeLines(result$admin_code, "admin_access_request.R")
#' }
#'
#' @export
import_plot_metadata <- function(data,
                                 column_mappings,
                                 validation,
                                 config,
                                 con = NULL,
                                 dry_run = FALSE,
                                 interactive = TRUE,
                                 progress = TRUE) {

  # Check validation passed
  if (!validation$valid) {
    stop("Data validation failed. Fix errors before importing. Use print(validation) to see issues.")
  }

  # Initialize connection if needed
  close_on_exit <- FALSE
  if (is.null(con)) {
    con <- call.mydb()
    close_on_exit <- TRUE
  }

  # Handle connection pools properly to avoid poolWithTransaction warning
  # Check if con is a pool and check out a connection for the import
  is_pool <- inherits(con, "Pool")
  if (is_pool) {
    # Check out a connection from the pool for the duration of import
    actual_con <- pool::poolCheckout(con)
    # Ensure connection is returned to pool on exit
    on.exit({
      pool::poolReturn(actual_con)
    }, add = TRUE)

    if (progress) cli::cli_alert_info("Using connection pool (checked out dedicated connection)")
  } else {
    actual_con <- con
  }

  # Get current username for RLS
  username <- tryCatch({
    DBI::dbGetQuery(actual_con, "SELECT current_user;")[[1]]
  }, error = function(e) {
    "unknown_user"
  })

  # Rename columns to schema names
  import_data <- data
  for (user_col in names(column_mappings)) {
    schema_col <- column_mappings[[user_col]]
    if (user_col %in% names(import_data)) {
      names(import_data)[names(import_data) == user_col] <- schema_col
    }
  }

  # Remove columns that were not mapped (keep only schema columns)
  # This prevents errors when trying to insert unmapped columns into database
  schema_columns <- unlist(column_mappings)
  unmapped_columns <- setdiff(names(import_data), schema_columns)

  if (length(unmapped_columns) > 0) {
    if (progress) {
      cli::cli_alert_info("Removing {length(unmapped_columns)} unmapped column(s): {paste(unmapped_columns, collapse=', ')}")
    }
    import_data <- import_data[, names(import_data) %in% schema_columns, drop = FALSE]
  }

  if (progress) {
    if (dry_run) {
      cli::cli_h1("Dry Run: Preview Import (No Changes Will Be Made)")
    } else {
      cli::cli_h1("Importing Plot Metadata")
    }
    cli::cli_alert_info("Plots to import: {nrow(import_data)}")
    cli::cli_alert_info("Columns to import: {ncol(import_data)}")
    cli::cli_alert_info("Importing as user: {username}")
  }

  # Try import with transaction support
  result <- tryCatch({

    # Begin transaction (unless dry run)
    if (!dry_run) {
      DBI::dbBegin(actual_con)
      if (progress) cli::cli_alert_info("Transaction started")
    }

    # Step 1: Link method
    if (progress) cli::cli_h2("Step 1: Linking methods")
    import_data <- .link_method_for_import(data =
      import_data,
      actual_con,
      interactive = interactive,
      dry_run = dry_run,
      progress = progress
    )

    # Step 2: Link country
    if (progress) cli::cli_h2("Step 2: Linking countries")
    import_data <- .link_country_for_import(
      import_data,
      actual_con,
      interactive = interactive,
      dry_run = dry_run,
      progress = progress
    )

    # Step 3: Extract and process ALL subplot features
    if (progress) cli::cli_h2("Step 3: Processing subplot features")
    subplot_data <- .extract_and_process_subplot_features(
      import_data,
      config,
      actual_con,
      interactive = interactive,
      dry_run = dry_run,
      progress = progress
    )

    # Step 4: Prepare data for data_liste_plots
    if (progress) cli::cli_h2("Step 4: Preparing plot data")
    plot_data <- .prepare_plot_data(
      data = 
        import_data, 
      people_columns = 
      subplot_data$all_subplot_columns,
      progress = progress
    )

    # Store plot names for result (user knows these!)
    plot_names <- plot_data$plot_name

    # Step 5: Preview or insert into data_liste_plots
    if (dry_run) {
      if (progress) {
        cli::cli_h2("Step 5: Preview - Would Insert Into data_liste_plots")
        cat("\nData preview (first 3 rows):\n")
        print(utils::head(plot_data, 3))
        cli::cli_alert_info("Columns: {paste(names(plot_data), collapse = ', ')}")
      }

      # Create mock plot IDs for dry-run so feature joins work
      plot_id_data <- data.frame(
        id_liste_plots = 999L + seq_len(nrow(plot_data)),
        plot_name = as.character(plot_data$plot_name),
        stringsAsFactors = FALSE
      )
    } else {
      if (progress) cli::cli_h2("Step 5: Inserting into data_liste_plots")

      # Build column names and values for INSERT
      cols <- names(plot_data)
      col_names <- paste(cols, collapse = ", ")

      # Build INSERT with RETURNING - works because created_by policy gives creator access
      insert_sql <- sprintf(
        "INSERT INTO data_liste_plots (%s) VALUES %s RETURNING id_liste_plots, plot_name",
        col_names,
        paste(
          apply(plot_data, 1, function(row) {
            values <- sapply(row, function(x) {
              if (is.na(x)) "NULL"
              else if (is.numeric(x)) as.character(x)
              else sprintf("'%s'", gsub("'", "''", as.character(x)))
            })
            sprintf("(%s)", paste(values, collapse = ", "))
          }),
          collapse = ", "
        )
      )

      # Execute INSERT and get back IDs (creator can SELECT via created_by policy)
      plot_id_data <- DBI::dbGetQuery(actual_con, insert_sql)
      plot_id_data$plot_name <- as.character(plot_id_data$plot_name)

      if (progress) cli::cli_alert_success("{nrow(plot_id_data)} plots inserted")
    }

    # Step 6: Preview or insert subplot features (people + other features)
    if (progress) cli::cli_h2("Step 6: {ifelse(dry_run, 'Preview', 'Inserting')} subplot features")

    if (dry_run) {
      if (progress) {
        # Preview people features
        for (feature_type in names(subplot_data$people_features)) {
          feature_df <- subplot_data$people_features[[feature_type]]
          if (!is.null(feature_df) && nrow(feature_df) > 0) {
            cli::cli_alert_info("Would insert {nrow(feature_df)} {feature_type} records (people)")
          }
        }
        # Preview other subplot features
        for (feature_type in names(subplot_data$other_features)) {
          feature_df <- subplot_data$other_features[[feature_type]]
          if (!is.null(feature_df) && nrow(feature_df) > 0) {
            cli::cli_alert_info("Would insert {nrow(feature_df)} {feature_type} records")
          }
        }
      }
    } else {
      # Insert people features (need id_table_colnam)
      for (feature_type in names(subplot_data$people_features)) {
        feature_df <- subplot_data$people_features[[feature_type]]

        if (!is.null(feature_df) && nrow(feature_df) > 0) {
          # Join with plot IDs (ensure plot_name is character to avoid type mismatches)
          feature_df$plot_name <- as.character(feature_df$plot_name)
          feature_df <- feature_df %>%
            dplyr::left_join(plot_id_data, by = "plot_name")

          # Insert using add_subplot_features
          tryCatch({
            add_subplot_features(
              new_data = feature_df,
              id_plot_name = "id_liste_plots",
              subplottype_field = feature_type,
              add_data = TRUE,
              ask_before_update = FALSE,
              interactive = interactive,
              con = actual_con
            )

            if (progress) {
              cli::cli_alert_success("{nrow(feature_df)} {feature_type} records inserted")
            }
          }, error = function(e) {
            stop("Error inserting ", feature_type, ": ", e$message, call. = FALSE)
          })
        }
      }

      # Insert other subplot features (no id_table_colnam needed)
      for (feature_type in names(subplot_data$other_features)) {
        feature_df <- subplot_data$other_features[[feature_type]]

        if (!is.null(feature_df) && nrow(feature_df) > 0) {
          # Join with plot IDs (ensure plot_name is character to avoid type mismatches)
          feature_df$plot_name <- as.character(feature_df$plot_name)
          feature_df <- feature_df %>%
            dplyr::left_join(plot_id_data, by = "plot_name")

          # Insert using add_subplot_features
          tryCatch({
            add_subplot_features(
              new_data = feature_df,
              id_plot_name = "id_liste_plots",
              subplottype_field = feature_type,
              add_data = TRUE,
              ask_before_update = FALSE,
              interactive = interactive,
              con = actual_con
            )

            if (progress) {
              cli::cli_alert_success("{nrow(feature_df)} {feature_type} records inserted")
            }
          }, error = function(e) {
            stop("Error inserting ", feature_type, ": ", e$message, call. = FALSE)
          })
        }
      }
    }

    # Commit transaction
    if (!dry_run) {
      DBI::dbCommit(actual_con)
      if (progress) cli::cli_alert_success("Transaction committed successfully")
    }

    # Generate admin code for row-level security
    if (!dry_run) {
      admin_code <- .generate_admin_access_code(
        username = username,
        plot_ids = plot_id_data$id_liste_plots,
        plot_names = plot_names
      )
    } else {
      admin_code <- NULL
    }

    # Success!
    result <- list(
      success = TRUE,
      plot_names = plot_names,
      n_plots = nrow(plot_data),
      username = username,
      admin_code = admin_code,
      dry_run = dry_run,
      imported_plots = if (!dry_run) {
        # Include plot_name and id_liste_plots for census module
        data.frame(
          plot_name = plot_id_data$plot_name,
          id_liste_plots = plot_id_data$id_liste_plots,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      },
      message = if (dry_run) {
        sprintf("Dry run completed. Would import %d plots.", nrow(plot_data))
      } else {
        sprintf("Successfully imported %d plots.", nrow(plot_data))
      }
    )

    if (progress) {
      cli::cli_rule()
      if (dry_run) {
        cli::cli_alert_success("Dry run completed - no changes made")
        cli::cli_alert_info("Run with dry_run = FALSE to actually import")
      } else {
        cli::cli_alert_success("Import completed successfully!")
        cat("\n")

        # IMPORTANT WARNING about row-level security
        cli::cli_rule(left = cli::col_yellow("⚠ IMPORTANT: Row-Level Security"))
        cat("\n")
        cli::cli_alert_warning("You may not have access to these plots yet due to row-level security!")
        cat("\n")
        cli::cli_alert_info("Imported plots: {paste(plot_names, collapse = ', ')}")
        cat("\n\n")
        cli::cli_alert_info(cli::col_cyan("Send the following R code to your database administrator:\n"))
        cat("\n")
        cat(cli::col_silver("─────────────────────────────────────────────────────────────"))
        cat("\n")
        cat(admin_code)
        cat(cli::col_silver("─────────────────────────────────────────────────────────────"))
        cat("\n\n")
        cli::cli_alert_info("You can also save this code to a file:")
        cat(cli::col_silver("  writeLines(result$admin_code, 'admin_access_request.R')"))
        cat("\n\n")
        cli::cli_rule()
      }
    }

    result

  }, error = function(e) {

    # Rollback on error
    if (!dry_run) {
      tryCatch({
        DBI::dbRollback(actual_con)
        if (progress) cli::cli_alert_danger("Transaction rolled back due to error")
      }, error = function(rollback_error) {
        if (progress) cli::cli_alert_danger("Error during rollback: {rollback_error$message}")
      })
    }

    # Extract error message with fallback
    error_msg <- e$message
    if (is.null(error_msg) || identical(error_msg, "")) {
      # Try to extract more info from the error object
      error_msg <- tryCatch(
        {
          if (!is.null(e$call)) {
            sprintf("Error in: %s", paste(deparse(e$call), collapse = " "))
          } else {
            "Unknown error (no message available)"
          }
        },
        error = function(ex) "Unknown error"
      )
    }

    # Return error
    result <- list(
      success = FALSE,
      plot_names = NULL,
      n_plots = 0,
      username = username,
      admin_code = NULL,
      dry_run = dry_run,
      message = sprintf("Import failed: %s", error_msg),
      error = e
    )

    if (progress) {
      cli::cli_alert_danger("Import failed: {error_msg}")
    }

    stop(e)
  })

  # Close connection if we opened it
  # if (close_on_exit) {
  #   DBI::dbDisconnect(con)
  # }

  return(result)
}


#' Generate Admin Access Code
#'
#' Generates R code for admin to grant row-level security access.
#' Uses plot_names (which user knows) instead of plot_ids (which user can't see).
#'
#' @param username Username who imported the plots
#' @param plot_names Vector of plot names
#'
#' @return Character string with R code
#' @keywords internal
.generate_admin_access_code <- function(username, plot_ids, plot_names) {

  # Format plot IDs for R vector
  plot_ids_str <- paste0("c(", paste(plot_ids, collapse = ", "), ")")

  # Generate R code
  admin_code <- sprintf(
'# ══════════════════════════════════════════════════════════════════════
# ROW-LEVEL SECURITY: Grant Access to Other Users
# ══════════════════════════════════════════════════════════════════════
#
# Plots: %s (IDs: %s)
# These plots were imported by: %s
#
# Instructions for Admin:
# 1. Run this code with ADMIN credentials
# 2. Specify which OTHER user(s) need access
# 3. Adjust operations if needed (SELECT, UPDATE, DELETE)
# ══════════════════════════════════════════════════════════════════════

library(CafriplotsR)

# Connect as admin
con <- call.mydb()  # Use admin credentials

# Plot IDs to grant access to
plot_ids <- %s

# !! SPECIFY THE USER WHO NEEDS ACCESS !!
target_user <- "username_here"  # Replace with actual username

# Grant access with row-level security policy
define_user_policy(
  con = con,
  user = target_user,
  ids = plot_ids,
  table = "data_liste_plots",
  operations = c("SELECT", "UPDATE"),  # Adjust as needed
  mode = "add"  # Add to existing access (use "replace" to replace)
)

# Verify policy was created
policies <- list_user_policies(con, user = target_user, table = "data_liste_plots")
print(policies)

cat("\\n✓ Access granted to user:", target_user, "\\n")

# Clean up
DBI::dbDisconnect(con)
',
    paste(plot_names, collapse = ", "),
    paste(plot_ids, collapse = ", "),
    username,
    plot_ids_str
  )

  return(admin_code)
}


#' Link Method for Import
#'
#' Uses existing .link_table() to match methods interactively.
#'
#' @keywords internal
.link_method_for_import <- function(data, con, interactive, dry_run, progress) {

  if (!"method" %in% names(data)) {
    if (progress) cli::cli_alert_warning("No method column found, skipping")
    return(data)
  }

  # Check if method values are already IDs (from Step 4 lookup matching)
  method_values <- data$method[!is.na(data$method) & trimws(data$method) != ""]
  are_numeric <- suppressWarnings(!any(is.na(as.numeric(method_values))))

  if (are_numeric && length(method_values) > 0) {
    # Values are already IDs - just rename column and validate
    if (progress) cli::cli_alert_info("Method values are already IDs from lookup matching")

    # Validate IDs against method_list
    method_lookup <- method_list()
    valid_ids <- method_lookup$id_method
    invalid_ids <- method_values[!(as.numeric(method_values) %in% valid_ids)]

    if (length(invalid_ids) > 0) {
      stop(sprintf(
        "Invalid method IDs found: %s. This should have been caught by validation!",
        paste(unique(invalid_ids), collapse = ", ")
      ))
    }

    # Rename column to id_method
    data$id_method <- as.numeric(data$method)
    # Keep method column for reference (will be removed later by .prepare_plot_data())

    if (progress) cli::cli_alert_success("Method IDs validated ({length(unique(method_values))} unique)")

    return(data)
  }

  # Values are names - use .link_table() as before
  if (dry_run) {
    if (progress) {
      unique_methods <- unique(data$method[!is.na(data$method)])
      cli::cli_alert_info("Would link {length(unique_methods)} unique methods")
      cli::cli_alert_info("Methods: {paste(unique_methods, collapse = ', ')}")
    }
    data$id_method <- 999  # Placeholder for dry run
    return(data)
  }

  # Use existing .link_table()
  data_linked <- .link_table(
    data_stand = data,
    column_searched = "method",
    column_name = "method",
    id_field = "id_method",
    id_table_name = "id_method",
    db_connection = con,
    table_name = "methodslist",
    field_label = "Method"
  )

  if (progress) cli::cli_alert_success("Methods linked")

  return(data_linked)
}


#' Link Country for Import
#'
#' Uses existing .link_table() to match countries interactively.
#'
#' @keywords internal
.link_country_for_import <- function(data, con, interactive, dry_run, progress) {

  if (!"country" %in% names(data)) {
    if (progress) cli::cli_alert_warning("No country column found, skipping")
    return(data)
  }

  # Check if country values are already IDs (from Step 4 lookup matching)
  country_values <- data$country[!is.na(data$country) & trimws(data$country) != ""]
  are_numeric <- suppressWarnings(!any(is.na(as.numeric(country_values))))

  if (are_numeric && length(country_values) > 0) {
    # Values are already IDs - just rename column and validate
    if (progress) cli::cli_alert_info("Country values are already IDs from lookup matching")

    # Validate IDs against country_list
    country_lookup <- country_list()
    valid_ids <- country_lookup$id_country
    invalid_ids <- country_values[!(as.numeric(country_values) %in% valid_ids)]

    if (length(invalid_ids) > 0) {
      stop(sprintf(
        "Invalid country IDs found: %s. This should have been caught by validation!",
        paste(unique(invalid_ids), collapse = ", ")
      ))
    }

    # Rename column to id_country
    data$id_country <- as.numeric(data$country)
    # Keep country column for reference (will be removed later by .prepare_plot_data())

    if (progress) cli::cli_alert_success("Country IDs validated ({length(unique(country_values))} unique)")

    return(data)
  }

  # Values are names - use .link_table() as before
  if (dry_run) {
    if (progress) {
      unique_countries <- unique(data$country[!is.na(data$country)])
      cli::cli_alert_info("Would link {length(unique_countries)} unique countries")
      cli::cli_alert_info("Countries: {paste(unique_countries, collapse = ', ')}")
    }
    data$id_country <- 999  # Placeholder for dry run
    return(data)
  }

  # Use existing .link_table()
  data_linked <- .link_table(
    data_stand = data,
    column_searched = "country",
    column_name = "country",
    id_field = "id_country",
    id_table_name = "id_country",
    db_connection = con,
    table_name = "table_countries",
    field_label = "Country"
  )

  if (progress) cli::cli_alert_success("Countries linked")

  return(data_linked)
}


#' Extract and Process ALL Subplot Features
#'
#' Identifies ALL subplot feature columns from the imported data by:
#' 1. Querying subplot_list() to get all defined subplot features
#' 2. Filtering to columns present in data that aren't flat table columns
#' 3. Separating into:
#'    - People features (valuetype == "table_colnam") - need linking
#'    - Other features (valuetype != "table_colnam") - direct values
#' 4. Processing each type appropriately
#'
#' @keywords internal
.extract_and_process_subplot_features <- function(data, config, con, interactive, dry_run, progress) {

  # Get ALL subplot feature definitions from database
  subplot_features <- subplot_list(con = con)

  # Define flat table columns (these go into data_liste_plots, not subplot features)
  flat_table_columns <- c(
    "plot_name", "locality", "ddlat", "ddlon", "elevation", "plot_area",
    "date_begin", "date_end", "plotshape_area", "plotshape_length",
    "id_method", "id_country", "method", "country",
    "data_modif_d", "data_modif_m", "data_modif_y"
  )

  # Identify which columns in the data are subplot features
  # (present in data, defined in subplot_features, not flat table columns)
  subplot_feature_types <- subplot_features %>%
    dplyr::filter(.data$type %in% names(data)) %>%
    dplyr::filter(!(.data$type %in% flat_table_columns))

  if (nrow(subplot_feature_types) == 0) {
    if (progress) cli::cli_alert_info("No subplot features found in data")
    return(list(
      all_subplot_columns = character(),
      people_features = list(),
      other_features = list()
    ))
  }

  if (progress) {
    cli::cli_alert_info("Found {nrow(subplot_feature_types)} subplot feature column(s): {paste(subplot_feature_types$type, collapse = ', ')}")
  }

  # Separate into people features (table_colnam) and other features
  people_feature_types <- subplot_feature_types %>%
    dplyr::filter(.data$valuetype == "table_colnam")

  other_feature_types <- subplot_feature_types %>%
    dplyr::filter(.data$valuetype != "table_colnam")

  people_features <- list()
  other_features <- list()

  # Process people features (need linking to table_colnam)
  if (nrow(people_feature_types) > 0) {
    if (progress) cli::cli_h3("Processing people features ({nrow(people_feature_types)} type(s))")

    for (i in 1:nrow(people_feature_types)) {
      feature_type <- people_feature_types$type[i]

      if (progress) cli::cli_alert_info("Processing {feature_type}")

      # Separate comma-separated names
      feature_sep <- data %>%
        dplyr::select("plot_name", dplyr::all_of(feature_type)) %>%
        tidyr::separate_rows(dplyr::all_of(feature_type), sep = ",") %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(feature_type), stringr::str_squish)) %>%
        dplyr::filter(!!rlang::sym(feature_type) != "" & !is.na(!!rlang::sym(feature_type)))

      if (nrow(feature_sep) == 0) {
        if (progress) cli::cli_alert_info("No {feature_type} entries to process")
        next
      }

      # Check if values are already IDs (from Step 4 lookup matching)
      feature_values <- feature_sep[[feature_type]][!is.na(feature_sep[[feature_type]]) & trimws(feature_sep[[feature_type]]) != ""]
      are_numeric <- suppressWarnings(!any(is.na(as.numeric(feature_values))))

      if (are_numeric && length(feature_values) > 0) {
        # Values are already IDs - just validate and rename
        if (progress) cli::cli_alert_info("{feature_type} values are already IDs from lookup matching")

        # Validate IDs against table_colnam
        people_lookup <- DBI::dbGetQuery(con, "SELECT id_table_colnam FROM table_colnam")
        valid_ids <- people_lookup$id_table_colnam
        invalid_ids <- feature_values[!(as.numeric(feature_values) %in% valid_ids)]

        if (length(invalid_ids) > 0) {
          stop(sprintf(
            "Invalid {feature_type} IDs found: %s. This should have been caught by validation!",
            paste(unique(invalid_ids), collapse = ", ")
          ))
        }

        # Rename column to id_table_colnam
        feature_sep$id_table_colnam <- as.numeric(feature_sep[[feature_type]])
        people_features[[feature_type]] <- feature_sep

        if (progress) cli::cli_alert_success("{nrow(feature_sep)} {feature_type} IDs validated")

      } else if (dry_run) {
        # Dry run with names
        if (progress) {
          cli::cli_alert_info("Would link {nrow(feature_sep)} {feature_type} names")
          unique_names <- unique(feature_sep[[feature_type]])
          cli::cli_alert_info("Names: {paste(utils::head(unique_names, 5), collapse = ', ')}{ifelse(length(unique_names) > 5, '...', '')}")
        }
        feature_sep$id_table_colnam <- 999  # Placeholder
        people_features[[feature_type]] <- feature_sep

      } else {
        # Values are names - use .link_colnam()
        feature_linked <- .link_colnam(
          data_stand = feature_sep,
          column_searched = feature_type,
          column_name = "colnam",
          id_field = feature_type,
          id_table_name = "id_table_colnam",
          db_connection = con,
          table_name = "table_colnam"
        )

        people_features[[feature_type]] <- feature_linked

        if (progress) cli::cli_alert_success("{nrow(feature_linked)} {feature_type} names linked")
      }
    }
  }

  # Process other subplot features (no linking needed, just extract values)
  if (nrow(other_feature_types) > 0) {
    if (progress) cli::cli_h3("Processing other subplot features ({nrow(other_feature_types)} type(s))")

    for (i in 1:nrow(other_feature_types)) {
      feature_type <- other_feature_types$type[i]

      if (progress) cli::cli_alert_info("Processing {feature_type}")

      # Extract feature values (one row per plot)
      feature_values <- data %>%
        dplyr::select("plot_name", dplyr::all_of(feature_type)) %>%
        dplyr::filter(!is.na(!!rlang::sym(feature_type)) & !!rlang::sym(feature_type) != "")

      if (nrow(feature_values) == 0) {
        if (progress) cli::cli_alert_info("No {feature_type} values to process")
        next
      }

      if (dry_run) {
        if (progress) {
          cli::cli_alert_info("Would insert {nrow(feature_values)} {feature_type} value(s)")
        }
      } else {
        if (progress) {
          cli::cli_alert_success("Prepared {nrow(feature_values)} {feature_type} value(s)")
        }
      }

      other_features[[feature_type]] <- feature_values
    }
  }

  # Return all subplot column names for removal from flat table
  all_subplot_columns <- subplot_feature_types$type

  list(
    all_subplot_columns = all_subplot_columns,
    people_features = people_features,
    other_features = other_features
  )
}


#' Prepare Plot Data for data_liste_plots
#'
#' Removes people columns and adds modification dates.
#'
#' @keywords internal
.prepare_plot_data <- function(data, people_columns, progress) {

  # Remove people columns (they go into subplot features)
  plot_data <- data %>%
    dplyr::select(-dplyr::any_of(people_columns))

  # Remove original method/country if still present (we have id_method/id_country)
  plot_data <- plot_data %>%
    dplyr::select(-dplyr::any_of(c("method", "country")))

  # Add modification dates
  plot_data <- plot_data %>%
    dplyr::mutate(
      data_modif_d = lubridate::day(Sys.Date()),
      data_modif_m = lubridate::month(Sys.Date()),
      data_modif_y = lubridate::year(Sys.Date())
    )

  if (progress) {
    cli::cli_alert_info("Prepared {nrow(plot_data)} rows with {ncol(plot_data)} columns")
  }

  return(plot_data)
}


#' Print Import Result
#'
#' Pretty-prints import results.
#'
#' @param result Import result from import_plot_metadata()
#'
#' @export
print_import_result <- function(result) {

  cli::cli_rule(
    left = "Import Result",
    right = ifelse(result$success, "SUCCESS", "FAILED")
  )

  cat("\n")
  cat(sprintf("Status: %s\n", ifelse(result$success, cli::col_green("✓ Success"), cli::col_red("✗ Failed"))))
  cat(sprintf("Mode: %s\n", ifelse(result$dry_run, "Dry Run (Preview)", "Actual Import")))
  cat(sprintf("Plots: %d\n", result$n_plots))
  cat(sprintf("User: %s\n", result$username))

  if (!result$dry_run && result$success && !is.null(result$plot_names)) {
    cat(sprintf("Plot names: %s\n", paste(result$plot_names, collapse = ", ")))
  }

  cat("\n")
  cat(result$message)
  cat("\n\n")

  if (!result$dry_run && result$success && !is.null(result$admin_code)) {
    cli::cli_rule(left = cli::col_yellow("⚠ Admin Access Required"))
    cat("\n")
    cat(cli::col_cyan("Admin code to grant access:\n\n"))
    cat(result$admin_code)
    cat("\n")
  }

  cli::cli_rule()

  invisible(result)
}
