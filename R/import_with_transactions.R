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

  if (is.null(con)) {
    con <- call.mydb()
  }

  # Get current username for RLS
  username <- tryCatch({
    DBI::dbGetQuery(con, "SELECT current_user;")[[1]]
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

  if (progress) {
    if (dry_run) {
      cli::cli_h1("Dry Run: Preview Import (No Changes Will Be Made)")
    } else {
      cli::cli_h1("Importing Plot Metadata")
    }
    cli::cli_alert_info("Plots to import: {nrow(import_data)}")
    cli::cli_alert_info("Importing as user: {username}")
  }

  # Try import with transaction support
  result <- tryCatch({

    # Begin transaction (unless dry run)
    if (!dry_run) {
      DBI::dbBegin(con)
      if (progress) cli::cli_alert_info("Transaction started")
    }

    # Step 1: Link method
    if (progress) cli::cli_h2("Step 1: Linking methods")
    import_data <- .link_method_for_import(data = 
      import_data,
      con,
      interactive = interactive,
      dry_run = dry_run,
      progress = progress
    )

    # Step 2: Link country
    if (progress) cli::cli_h2("Step 2: Linking countries")
    import_data <- .link_country_for_import(
      import_data,
      con,
      interactive = interactive,
      dry_run = dry_run,
      progress = progress
    )

    # Step 3: Extract and process ALL subplot features
    if (progress) cli::cli_h2("Step 3: Processing subplot features")
    subplot_data <- .extract_and_process_subplot_features(
      import_data,
      config,
      con,
      interactive = interactive,
      dry_run = dry_run,
      progress = progress
    )

    # Step 4: Prepare data for data_liste_plots
    if (progress) cli::cli_h2("Step 4: Preparing plot data")
    plot_data <- .prepare_plot_data(
      import_data,
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
      plot_id_data <- NULL
    } else {
      if (progress) cli::cli_h2("Step 5: Inserting into data_liste_plots")

      # Use INSERT ... RETURNING to get IDs during insert (bypasses RLS SELECT restriction)
      # Build column names and placeholders
      cols <- names(plot_data)
      col_names <- paste(cols, collapse = ", ")

      # Build INSERT with RETURNING clause
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

      # Execute and get returned IDs
      plot_id_data <- DBI::dbGetQuery(con, insert_sql)

      if (progress) cli::cli_alert_success("{nrow(plot_data)} plots inserted")
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
          # Join with plot IDs
          feature_df <- feature_df %>%
            dplyr::left_join(plot_id_data, by = "plot_name")

          # Insert using add_subplot_features
          add_subplot_features(
            new_data = feature_df,
            id_plot_name = "id_liste_plots",
            subplottype_field = feature_type,
            add_data = TRUE,
            ask_before_update = FALSE,
            con = con
          )

          if (progress) {
            cli::cli_alert_success("{nrow(feature_df)} {feature_type} records inserted")
          }
        }
      }

      # Insert other subplot features (no id_table_colnam needed)
      for (feature_type in names(subplot_data$other_features)) {
        feature_df <- subplot_data$other_features[[feature_type]]

        if (!is.null(feature_df) && nrow(feature_df) > 0) {
          # Join with plot IDs
          feature_df <- feature_df %>%
            dplyr::left_join(plot_id_data, by = "plot_name")

          # Insert using add_subplot_features
          add_subplot_features(
            new_data = feature_df,
            id_plot_name = "id_liste_plots",
            subplottype_field = feature_type,
            add_data = TRUE,
            ask_before_update = FALSE,
            con = con
          )

          if (progress) {
            cli::cli_alert_success("{nrow(feature_df)} {feature_type} records inserted")
          }
        }
      }
    }

    # Commit transaction
    if (!dry_run) {
      DBI::dbCommit(con)
      if (progress) cli::cli_alert_success("Transaction committed successfully")
    }

    # Generate admin code for row-level security
    if (!dry_run) {
      admin_code <- .generate_admin_access_code(
        username = username,
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
        DBI::dbRollback(con)
        if (progress) cli::cli_alert_danger("Transaction rolled back due to error")
      }, error = function(rollback_error) {
        if (progress) cli::cli_alert_danger("Error during rollback: {rollback_error$message}")
      })
    }

    # Return error
    result <- list(
      success = FALSE,
      plot_names = NULL,
      n_plots = 0,
      username = username,
      admin_code = NULL,
      dry_run = dry_run,
      message = sprintf("Import failed: %s", e$message),
      error = e
    )

    if (progress) {
      cli::cli_alert_danger("Import failed: {e$message}")
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
.generate_admin_access_code <- function(username, plot_names) {

  # Format plot names for SQL IN clause
  plot_names_sql <- paste0("'", plot_names, "'", collapse = ", ")

  # Generate R code
  admin_code <- sprintf(
'# ══════════════════════════════════════════════════════════════════════
# ROW-LEVEL SECURITY: Grant Access to User
# ══════════════════════════════════════════════════════════════════════
#
# User: %s
# Plots: %s
#
# Instructions for Admin:
# 1. Run this code with ADMIN credentials
# 2. Verify the plots exist in the database
# 3. Adjust operations if needed (SELECT, UPDATE, INSERT, DELETE, or ALL)
# ══════════════════════════════════════════════════════════════════════

library(CafriplotsR)

# Connect as admin
con <- call.mydb()  # Use admin credentials

# Get plot IDs from plot names
plot_ids <- DBI::dbGetQuery(con,
  "SELECT id_liste_plots FROM data_liste_plots
   WHERE plot_name IN (%s)")$id_liste_plots

# Verify plots found
cat(sprintf("Found %%d plots for user: %s\\n", length(plot_ids)))
cat("Plot IDs:", paste(plot_ids, collapse = ", "), "\\n\\n")

# Grant access with row-level security policy
define_user_policy(
  con = con,
  user = "%s",
  ids = plot_ids,
  table = "data_liste_plots",
  operations = c("SELECT", "UPDATE"),  # Adjust as needed
  drop_existing = TRUE  # Replace any existing policies for this user
)

# Verify policy was created
policies <- list_user_policies(con, user = "%s", table = "data_liste_plots")
print(policies)

cat("\\n✓ Access granted to user: %s\\n")

# Clean up
DBI::dbDisconnect(con)
',
    username,
    paste(plot_names, collapse = ", "),
    plot_names_sql,
    username,
    username,
    username,
    username
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
    dplyr::filter(type %in% names(data)) %>%
    dplyr::filter(!(type %in% flat_table_columns))

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
    dplyr::filter(valuetype == "table_colnam")

  other_feature_types <- subplot_feature_types %>%
    dplyr::filter(valuetype != "table_colnam")

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
        dplyr::select(plot_name, !!rlang::sym(feature_type)) %>%
        tidyr::separate_rows(!!rlang::sym(feature_type), sep = ",") %>%
        dplyr::mutate(!!rlang::sym(feature_type) := stringr::str_squish(!!rlang::sym(feature_type))) %>%
        dplyr::filter(!!rlang::sym(feature_type) != "" & !is.na(!!rlang::sym(feature_type)))

      if (nrow(feature_sep) == 0) {
        if (progress) cli::cli_alert_info("No {feature_type} entries to process")
        next
      }

      if (dry_run) {
        if (progress) {
          cli::cli_alert_info("Would link {nrow(feature_sep)} {feature_type} names")
          unique_names <- unique(feature_sep[[feature_type]])
          cli::cli_alert_info("Names: {paste(utils::head(unique_names, 5), collapse = ', ')}{ifelse(length(unique_names) > 5, '...', '')}")
        }
        feature_sep$id_table_colnam <- 999  # Placeholder
        people_features[[feature_type]] <- feature_sep

      } else {
        # Use existing .link_colnam()
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
        dplyr::select(plot_name, !!rlang::sym(feature_type)) %>%
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
