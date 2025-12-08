# Import Validation Functions
#
# This file contains functions for validating plot metadata before import.
# Validation uses database rules from subplot_list() and checks against
# lookup tables to ensure data integrity.

#' Validate Plot Metadata Before Import
#'
#' Comprehensive validation of plot metadata using database rules and lookup
#' tables. Returns structured results with severity levels (error vs warning).
#' Can interactively fix lookup mismatches using fuzzy matching. Also checks
#' for potential duplicate plots by matching method, country, and coordinates.
#'
#' @param data Data frame containing plot metadata to validate
#' @param column_mappings Named list mapping user columns to schema columns
#'   (from map_user_columns())
#' @param config Routing configuration from get_import_column_routing()
#' @param con Database connection (optional, will create if NULL)
#' @param strict Logical: If TRUE, warnings are treated as errors (default FALSE)
#' @param interactive Logical: If TRUE, allow interactive fixing of lookup mismatches (default TRUE)
#' @param fix_on_fly Logical: If TRUE, fix issues during validation (default TRUE)
#'
#' @return List with validation results:
#'   \item{valid}{Logical: TRUE if no errors (warnings allowed)}
#'   \item{errors}{Data frame of error messages}
#'   \item{warnings}{Data frame of warning messages (includes duplicate plot warnings)}
#'   \item{summary}{Summary statistics}
#'   \item{original_data}{Original input data (unchanged)}
#'   \item{cleaned_data}{Data with interactive fixes applied (if any)}
#'   \item{changes_made}{Data frame documenting what was changed}
#'
#' @examples
#' \dontrun{
#' # Map columns first
#' config <- get_import_column_routing("plots")
#' mapping_result <- map_user_columns(my_data, config)
#'
#' # Validate data with interactive fixing (default)
#' validation <- validate_plot_metadata(
#'   data = my_data,
#'   column_mappings = mapping_result$mappings,
#'   config = config,
#'   interactive = TRUE,  # Allow interactive fixing
#'   fix_on_fly = TRUE    # Fix during validation
#' )
#'
#' # Check results
#' print_validation_results(validation)
#'
#' if (!validation$valid) {
#'   stop("Data validation failed!")
#' }
#'
#' # Use cleaned data for import
#' result <- import_plot_metadata(
#'   data = validation$cleaned_data,  # Use cleaned version!
#'   column_mappings = mapping_result$mappings,
#'   validation = validation,
#'   config = config
#' )
#' }
#'
#' @export
validate_plot_metadata <- function(data,
                                   column_mappings,
                                   config,
                                   con = NULL,
                                   strict = FALSE,
                                   interactive = TRUE,
                                   fix_on_fly = TRUE) {

  # Initialize connection if needed
  close_on_exit <- FALSE
  if (is.null(con)) {
    con <- call.mydb()
    close_on_exit <- TRUE
  }

  # Store original data
  original_data <- data

  # Initialize results storage
  errors <- list()
  warnings <- list()
  all_changes <- data.frame(
    column = character(),
    row = integer(),
    original_value = character(),
    corrected_value = character(),
    method = character(),
    stringsAsFactors = FALSE
  )

  # Rename columns to schema names for validation
  # Filter out NA mappings (skipped columns)
  valid_mappings <- column_mappings[!is.na(column_mappings)]

  validated_data <- data
  for (user_col in names(valid_mappings)) {
    schema_col <- valid_mappings[[user_col]]
    if (user_col %in% names(validated_data)) {
      names(validated_data)[names(validated_data) == user_col] <- schema_col
    }
  }

  # Remove any columns that were not mapped (skipped columns)
  skipped_cols <- names(column_mappings)[is.na(column_mappings)]
  if (length(skipped_cols) > 0) {
    validated_data <- validated_data[, !(names(validated_data) %in% skipped_cols), drop = FALSE]
    cli::cli_alert_info("Removed {length(skipped_cols)} skipped column(s) from data")
  }

  # 1. Required fields validation
  required_check <- .validate_required_fields(
    validated_data,
    config$required_columns
  )
  if (length(required_check) > 0) {
    errors <- c(errors, required_check)
  }

  # 2. Column type validation
  type_check <- .validate_column_types(
    validated_data,
    config,
    con
  )
  errors <- c(errors, type_check$errors)
  warnings <- c(warnings, type_check$warnings)

  # 3. Range validation (numeric fields)
  range_check <- .validate_ranges(
    validated_data,
    config,
    con
  )
  errors <- c(errors, range_check$errors)
  warnings <- c(warnings, range_check$warnings)

  # 4. Lookup table validation WITH INTERACTIVE FIXING
  lookup_check <- .validate_lookup_values_interactive(
    validated_data,
    config,
    con,
    interactive = interactive,
    fix_on_fly = fix_on_fly
  )
  errors <- c(errors, lookup_check$errors)
  warnings <- c(warnings, lookup_check$warnings)
  validated_data <- lookup_check$cleaned_data  # Apply fixes!
  if (nrow(lookup_check$changes_made) > 0) {
    all_changes <- rbind(all_changes, lookup_check$changes_made)
  }

  # 5. Unique constraints
  unique_check <- .validate_unique_constraints(
    validated_data,
    config
  )
  errors <- c(errors, unique_check)

  # 6. Check for potential duplicate plots (same location, method, country)
  duplicate_check <- .check_duplicate_plots(
    validated_data,
    con
  )
  warnings <- c(warnings, duplicate_check$warnings)
  errors <- c(errors, duplicate_check$errors)

  # Convert to data frames
  errors_df <- if (length(errors) > 0) {
    do.call(rbind, lapply(errors, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame(
      column = character(),
      row = integer(),
      message = character(),
      value = character(),
      stringsAsFactors = FALSE
    )
  }

  warnings_df <- if (length(warnings) > 0) {
    do.call(rbind, lapply(warnings, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame(
      column = character(),
      row = integer(),
      message = character(),
      value = character(),
      stringsAsFactors = FALSE
    )
  }

  # Determine validity
  valid <- nrow(errors_df) == 0
  if (strict) {
    valid <- valid && nrow(warnings_df) == 0
  }

  # Summary
  summary <- list(
    total_rows = nrow(data),
    total_columns = ncol(data),
    mapped_columns = length(column_mappings),
    errors = nrow(errors_df),
    warnings = nrow(warnings_df),
    changes_applied = nrow(all_changes),
    valid = valid
  )

  # Close connection if we opened it
  if (close_on_exit) {
    DBI::dbDisconnect(con)
  }

  structure(
    list(
      valid = valid,
      errors = errors_df,
      warnings = warnings_df,
      summary = summary,
      original_data = original_data,
      cleaned_data = validated_data,
      changes_made = all_changes,
      strict = strict,
      interactive = interactive
    ),
    class = "plot_validation_result"
  )
}


#' Validate Required Fields
#'
#' Checks that all required columns are present and non-empty.
#'
#' @param data Data frame with schema column names
#' @param required_columns Character vector of required column names
#'
#' @return List of error objects
#' @keywords internal
.validate_required_fields <- function(data, required_columns) {
  errors <- list()

  for (col in required_columns) {
    # Check if column exists
    if (!col %in% names(data)) {
      errors <- c(errors, list(list(
        column = col,
        row = NA,
        message = sprintf("Required column '%s' is missing", col),
        value = NA
      )))
    } else {
      # Check for missing values in required column
      missing_rows <- which(is.na(data[[col]]) | trimws(as.character(data[[col]])) == "")

      if (length(missing_rows) > 0) {
        for (row in missing_rows) {
          errors <- c(errors, list(list(
            column = col,
            row = row,
            message = sprintf("Required field '%s' is missing or empty", col),
            value = NA
          )))
        }
      }
    }
  }

  errors
}


#' Validate Column Types
#'
#' Checks that column values match expected data types from database.
#'
#' @param data Data frame with schema column names
#' @param config Routing configuration
#' @param con Database connection
#'
#' @return List with errors and warnings
#' @keywords internal
.validate_column_types <- function(data, config, con) {
  errors <- list()
  warnings <- list()

  # Get subplot feature types from database
  subplot_info <- tryCatch({
    subplot_list(con)
  }, error = function(e) {
    NULL
  })

  if (is.null(subplot_info)) {
    return(list(errors = errors, warnings = warnings))
  }

  # Check each column that's a subplot feature
  for (col in names(data)) {
    # Skip if not in subplot list
    if (!col %in% subplot_info$type) next

    # Get expected type
    feature_info <- subplot_info[subplot_info$type == col, ]
    expected_type <- feature_info$valuetype[1]

    # Skip if no type info
    if (is.na(expected_type) || expected_type == "") next

    # Get non-NA values
    values <- data[[col]][!is.na(data[[col]])]
    if (length(values) == 0) next

    # Type validation based on valuetype
    if (expected_type == "numeric") {
      # Check if values can be coerced to numeric
      suppressWarnings({
        numeric_values <- as.numeric(values)
      })

      invalid_rows <- which(is.na(numeric_values) & !is.na(data[[col]]))

      for (row in invalid_rows) {
        errors <- c(errors, list(list(
          column = col,
          row = row,
          message = sprintf("Value must be numeric (expected type: %s)", expected_type),
          value = as.character(data[[col]][row])
        )))
      }

    } else if (expected_type == "integer") {
      # Check if values can be coerced to integer
      suppressWarnings({
        int_values <- as.integer(values)
      })

      invalid_rows <- which(is.na(int_values) & !is.na(data[[col]]))

      for (row in invalid_rows) {
        errors <- c(errors, list(list(
          column = col,
          row = row,
          message = sprintf("Value must be an integer (expected type: %s)", expected_type),
          value = as.character(data[[col]][row])
        )))
      }

    } else if (expected_type %in% c("character", "categorical", "ordinal")) {
      # Just ensure it's convertible to character
      # This is generally always true, so just warning if suspicious
      for (i in seq_along(values)) {
        if (nchar(trimws(as.character(values[i]))) == 0) {
          row <- which(!is.na(data[[col]]))[i]
          warnings <- c(warnings, list(list(
            column = col,
            row = row,
            message = "Empty or whitespace-only value in text field",
            value = as.character(data[[col]][row])
          )))
        }
      }
    }
  }

  # Special validation for known columns
  if ("plot_area" %in% names(data)) {
    invalid_rows <- which(!is.na(data$plot_area) & (data$plot_area <= 0))
    for (row in invalid_rows) {
      errors <- c(errors, list(list(
        column = "plot_area",
        row = row,
        message = "Plot area must be positive",
        value = as.character(data$plot_area[row])
      )))
    }
  }

  if ("elevation" %in% names(data)) {
    invalid_rows <- which(!is.na(data$elevation) & (data$elevation < -500 | data$elevation > 9000))
    for (row in invalid_rows) {
      warnings <- c(warnings, list(list(
        column = "elevation",
        row = row,
        message = "Elevation outside typical range (-500 to 9000m). Please verify.",
        value = as.character(data$elevation[row])
      )))
    }
  }

  # Coordinate validation
  if ("ddlat" %in% names(data)) {
    invalid_rows <- which(!is.na(data$ddlat) & (data$ddlat < -90 | data$ddlat > 90))
    for (row in invalid_rows) {
      errors <- c(errors, list(list(
        column = "ddlat",
        row = row,
        message = "Latitude must be between -90 and 90",
        value = as.character(data$ddlat[row])
      )))
    }
  }

  if ("ddlon" %in% names(data)) {
    invalid_rows <- which(!is.na(data$ddlon) & (data$ddlon < -180 | data$ddlon > 180))
    for (row in invalid_rows) {
      errors <- c(errors, list(list(
        column = "ddlon",
        row = row,
        message = "Longitude must be between -180 and 180",
        value = as.character(data$ddlon[row])
      )))
    }
  }

  list(errors = errors, warnings = warnings)
}


#' Validate Numeric Ranges
#'
#' Checks that numeric values fall within allowed ranges from database.
#'
#' @param data Data frame with schema column names
#' @param config Routing configuration
#' @param con Database connection
#'
#' @return List with errors and warnings
#' @keywords internal
.validate_ranges <- function(data, config, con) {
  errors <- list()
  warnings <- list()

  # First, check import_config validation rules (for flat columns like ddlat, ddlon)
  if (!is.null(config$import_config$validation_rules)) {
    for (col in names(config$import_config$validation_rules)) {
      if (!col %in% names(data)) next

      rule <- config$import_config$validation_rules[[col]]

      # Only process numeric columns with min/max rules
      if (is.null(rule$type) || rule$type != "numeric") next
      if (is.null(rule$min) && is.null(rule$max)) next

      # Get numeric values
      values <- suppressWarnings(as.numeric(data[[col]]))

      # Check for potential UTM coordinates (large values way outside range)
      is_coord_col <- col %in% c("ddlat", "ddlon")
      has_utm_hint <- !is.null(rule$utm_hint) && rule$utm_hint == TRUE

      if (is_coord_col && has_utm_hint) {
        # Detect potential UTM coordinates
        large_values <- which(!is.na(values) & (abs(values) > 1000))

        if (length(large_values) > 0) {
          # Add specific UTM warning
          errors <- c(errors, list(list(
            column = col,
            row = large_values[1],  # Just show first occurrence
            message = sprintf(
              "⚠️ Possible UTM coordinates detected! Values are very large (e.g., %.0f). Geographic coordinates should be: Latitude (-90 to 90), Longitude (-180 to 180). If these are UTM coordinates, you can convert them in the Preview step.",
              values[large_values[1]]
            ),
            value = as.character(data[[col]][large_values[1]])
          )))

          # Don't check min/max for obvious UTM values
          next
        }
      }

      # Check min value
      if (!is.null(rule$min)) {
        invalid_rows <- which(!is.na(values) & values < rule$min)
        for (row in invalid_rows) {
          item <- list(
            column = col,
            row = row,
            message = sprintf(
              "%s (value: %.4f, expected: %.2f to %.2f)",
              rule$message,
              values[row],
              rule$min,
              rule$max %||% Inf
            ),
            value = as.character(data[[col]][row])
          )

          if (!is.null(rule$severity) && rule$severity == "warning") {
            warnings <- c(warnings, list(item))
          } else {
            errors <- c(errors, list(item))
          }
        }
      }

      # Check max value
      if (!is.null(rule$max)) {
        invalid_rows <- which(!is.na(values) & values > rule$max)
        for (row in invalid_rows) {
          item <- list(
            column = col,
            row = row,
            message = sprintf(
              "%s (value: %.4f, expected: %.2f to %.2f)",
              rule$message,
              values[row],
              rule$min %||% -Inf,
              rule$max
            ),
            value = as.character(data[[col]][row])
          )

          if (!is.null(rule$severity) && rule$severity == "warning") {
            warnings <- c(warnings, list(item))
          } else {
            errors <- c(errors, list(item))
          }
        }
      }
    }
  }

  # Get subplot feature validation rules from database
  subplot_info <- tryCatch({
    subplot_list(con)
  }, error = function(e) {
    NULL
  })

  if (is.null(subplot_info)) {
    return(list(errors = errors, warnings = warnings))
  }

  # Check each numeric column
  for (col in names(data)) {
    # Skip if not in subplot list
    if (!col %in% subplot_info$type) next

    # Get validation rules
    feature_info <- subplot_info[subplot_info$type == col, ]
    min_val <- feature_info$minallowedvalue[1]
    max_val <- feature_info$maxallowedvalue[1]
    expected_unit <- feature_info$expectedunit[1]

    # Skip if no validation rules
    if (is.na(min_val) && is.na(max_val)) next

    # Get numeric values
    values <- suppressWarnings(as.numeric(data[[col]]))

    # Check min value
    if (!is.na(min_val)) {
      invalid_rows <- which(!is.na(values) & values < min_val)
      for (row in invalid_rows) {
        errors <- c(errors, list(list(
          column = col,
          row = row,
          message = sprintf(
            "Value %.2f below minimum allowed (%.2f%s)",
            values[row],
            min_val,
            ifelse(!is.na(expected_unit) && expected_unit != "none",
                   paste0(" ", expected_unit), "")
          ),
          value = as.character(data[[col]][row])
        )))
      }
    }

    # Check max value
    if (!is.na(max_val)) {
      invalid_rows <- which(!is.na(values) & values > max_val)
      for (row in invalid_rows) {
        errors <- c(errors, list(list(
          column = col,
          row = row,
          message = sprintf(
            "Value %.2f exceeds maximum allowed (%.2f%s)",
            values[row],
            max_val,
            ifelse(!is.na(expected_unit) && expected_unit != "none",
                   paste0(" ", expected_unit), "")
          ),
          value = as.character(data[[col]][row])
        )))
      }
    }
  }

  list(errors = errors, warnings = warnings)
}


#' Validate Lookup Table Values
#'
#' Checks that foreign key references exist in lookup tables.
#'
#' @param data Data frame with schema column names
#' @param config Routing configuration
#' @param con Database connection
#'
#' @return List with errors and warnings
#' @keywords internal
.validate_lookup_values <- function(data, config, con) {
  errors <- list()
  warnings <- list()

  # Method validation
  if ("method" %in% names(data)) {
    method_lookup <- tryCatch({
      method_list()
    }, error = function(e) {
      NULL
    })

    if (!is.null(method_lookup)) {
      # Check if values are IDs (numeric) or names (character)
      # After lookup matching, they should be IDs
      method_values <- data$method[!is.na(data$method) & trimws(data$method) != ""]

      cli::cli_alert_info("Method validation: checking {length(method_values)} value(s)")
      cli::cli_alert_info("  Sample values: {paste(head(method_values, 3), collapse=', ')}")
      cli::cli_alert_info("  Valid IDs available: {paste(head(method_lookup$id_method, 5), collapse=', ')}")

      # Try to determine if these are IDs or names
      are_numeric <- suppressWarnings(!any(is.na(as.numeric(method_values))))
      cli::cli_alert_info("  Are all values numeric? {are_numeric}")

      if (are_numeric) {
        # Validate against id_method (IDs)
        valid_ids <- method_lookup$id_method
        cli::cli_alert_info("  Validating against {length(valid_ids)} valid ID(s)")
        invalid_rows <- which(
          !is.na(data$method) &
          trimws(data$method) != "" &
          !(as.numeric(data$method) %in% valid_ids)
        )
        cli::cli_alert_info("  Found {length(invalid_rows)} invalid row(s)")
      } else {
        # Validate against method names (backwards compatibility)
        valid_methods <- method_lookup$method
        cli::cli_alert_info("  Validating against method names")
        invalid_rows <- which(
          !is.na(data$method) &
          trimws(data$method) != "" &
          !(data$method %in% valid_methods)
        )
      }

      for (row in invalid_rows) {
        errors <- c(errors, list(list(
          column = "method",
          row = row,
          message = sprintf(
            "Invalid method '%s'. Use method_list() to see valid methods.",
            data$method[row]
          ),
          value = as.character(data$method[row])
        )))
      }
    }
  }

  # Country validation
  if ("country" %in% names(data)) {
    country_lookup <- tryCatch({
      country_list()
    }, error = function(e) {
      NULL
    })

    if (!is.null(country_lookup)) {
      # Check if values are IDs (numeric) or names (character)
      country_values <- data$country[!is.na(data$country) & trimws(data$country) != ""]

      # Try to determine if these are IDs or names
      are_numeric <- suppressWarnings(!any(is.na(as.numeric(country_values))))

      if (are_numeric) {
        # Validate against id_country (IDs)
        valid_ids <- country_lookup$id_country
        invalid_rows <- which(
          !is.na(data$country) &
          trimws(data$country) != "" &
          !(as.numeric(data$country) %in% valid_ids)
        )
      } else {
        # Validate against country names (backwards compatibility)
        valid_countries <- country_lookup$country
        invalid_rows <- which(
          !is.na(data$country) &
          trimws(data$country) != "" &
          !(data$country %in% valid_countries)
        )
      }

      for (row in invalid_rows) {
        errors <- c(errors, list(list(
          column = "country",
          row = row,
          message = sprintf(
            "Invalid country '%s'. Use country_list() to see valid countries.",
            data$country[row]
          ),
          value = as.character(data$country[row])
        )))
      }
    }
  }

  # People fields validation (check against table_colnam)
  # Get subplot features that are people fields (valuetype == "table_colnam")
  subplot_info <- tryCatch({
    subplot_list(con)
  }, error = function(e) {
    NULL
  })

  if (!is.null(subplot_info)) {
    people_columns <- subplot_info$type[subplot_info$valuetype == "table_colnam"]

    # Get valid people IDs and names from table_colnam
    valid_people <- tryCatch({
      DBI::dbGetQuery(con, "SELECT id_table_colnam, colnam, last_name, first_name FROM table_colnam") %>%
        dplyr::mutate(
          full_name = paste(trimws(first_name), trimws(last_name)),
          full_name_rev = paste(trimws(last_name), trimws(first_name))
        )
    }, error = function(e) {
      NULL
    })

    if (!is.null(valid_people)) {
      # Get valid IDs for validation
      valid_ids <- valid_people$id_table_colnam

      # Create lookup list of valid names (handle different formats)
      valid_name_list <- c(
        valid_people$full_name,
        valid_people$full_name_rev,
        valid_people$last_name,
        valid_people$first_name,
        valid_people$colnam
      )
      valid_name_list <- unique(tolower(trimws(valid_name_list[!is.na(valid_name_list)])))

      for (col in people_columns) {
        if (!col %in% names(data)) next

        for (row in seq_len(nrow(data))) {
          if (is.na(data[[col]][row]) || trimws(data[[col]][row]) == "") next

          # Check if values are IDs (numeric) or names (character)
          value <- as.character(data[[col]][row])

          # Split comma-separated values (could be IDs or names)
          value_list <- strsplit(value, ",")[[1]]
          value_list <- trimws(value_list)

          for (val in value_list) {
            # Try to determine if this is an ID or name
            is_numeric <- suppressWarnings(!is.na(as.numeric(val)))

            if (is_numeric) {
              # Validate against IDs (after lookup matching)
              if (!(as.numeric(val) %in% valid_ids)) {
                warnings <- c(warnings, list(list(
                  column = col,
                  row = row,
                  message = sprintf(
                    "Person ID '%s' not found in table_colnam.",
                    val
                  ),
                  value = val
                )))
              }
            } else {
              # Validate against names (before lookup matching)
              val_lower <- tolower(trimws(val))
              if (!val_lower %in% valid_name_list) {
                warnings <- c(warnings, list(list(
                  column = col,
                  row = row,
                  message = sprintf(
                    "Person '%s' not found in table_colnam. During import, you will be prompted to match or add this person.",
                    val
                  ),
                  value = val
                )))
              }
            }
          }
        }
      }
    }
  }

  list(errors = errors, warnings = warnings)
}


#' Validate Lookup Values with Interactive Fixing
#'
#' Extended version of .validate_lookup_values() that can interactively fix
#' mismatches using fuzzy matching via resolve_multiple_values().
#'
#' @param data Data frame with schema column names
#' @param config Routing configuration
#' @param con Database connection
#' @param interactive Logical: Allow interactive fixing
#' @param fix_on_fly Logical: Apply fixes during validation
#'
#' @return List with:
#'   \item{errors}{List of error objects}
#'   \item{warnings}{List of warning objects}
#'   \item{cleaned_data}{Data with fixes applied}
#'   \item{changes_made}{Data frame documenting changes}
#'
#' @keywords internal
.validate_lookup_values_interactive <- function(data, config, con,
                                               interactive = TRUE,
                                               fix_on_fly = TRUE) {
  errors <- list()
  warnings <- list()
  cleaned_data <- data
  changes_made <- data.frame(
    column = character(),
    row = integer(),
    original_value = character(),
    corrected_value = character(),
    method = character(),
    stringsAsFactors = FALSE
  )

  # Method validation with interactive fixing
  if ("method" %in% names(cleaned_data)) {
    method_result <- .resolve_lookup_column(
      data = cleaned_data,
      column_name = "method",
      lookup_function = method_list,
      lookup_value_col = "method",
      lookup_id_col = "id_method",
      lookup_table_name = "methodslist",
      con = con,
      interactive = interactive,
      fix_on_fly = fix_on_fly
    )

    errors <- c(errors, method_result$errors)
    cleaned_data <- method_result$cleaned_data
    if (nrow(method_result$changes) > 0) {
      changes_made <- rbind(changes_made, method_result$changes)
    }
  }

  # Country validation with interactive fixing
  if ("country" %in% names(cleaned_data)) {
    country_result <- .resolve_lookup_column(
      data = cleaned_data,
      column_name = "country",
      lookup_function = country_list,
      lookup_value_col = "country",
      lookup_id_col = "id_country",
      lookup_table_name = "table_countries",
      con = con,
      interactive = interactive,
      fix_on_fly = fix_on_fly
    )

    errors <- c(errors, country_result$errors)
    cleaned_data <- country_result$cleaned_data
    if (nrow(country_result$changes) > 0) {
      changes_made <- rbind(changes_made, country_result$changes)
    }
  }

  # People fields - keep as warnings (handled during import)
  subplot_info <- tryCatch({
    subplot_list(con)
  }, error = function(e) {
    NULL
  })

  if (!is.null(subplot_info)) {
    people_columns <- subplot_info$type[subplot_info$valuetype == "table_colnam"]

    valid_people <- tryCatch({
      try_open_postgres_table(table = "table_colnam", con = con) %>%
        dplyr::select(id_table_colnam, last_name, first_name) %>%
        dplyr::collect() %>%
        dplyr::mutate(
          colnam = paste(trimws(first_name), trimws(last_name)),
          colnam_rev = paste(trimws(last_name), trimws(first_name))
        )
    }, error = function(e) {
      NULL
    })

    if (!is.null(valid_people)) {
      valid_name_list <- c(
        valid_people$colnam,
        valid_people$colnam_rev,
        valid_people$last_name,
        valid_people$first_name
      )
      valid_name_list <- unique(tolower(trimws(valid_name_list[!is.na(valid_name_list)])))

      for (col in people_columns) {
        if (!col %in% names(cleaned_data)) next

        for (row in seq_len(nrow(cleaned_data))) {
          if (is.na(cleaned_data[[col]][row]) || trimws(cleaned_data[[col]][row]) == "") next

          names_list <- strsplit(as.character(cleaned_data[[col]][row]), ",")[[1]]
          names_list <- trimws(names_list)

          for (person_name in names_list) {
            person_name_lower <- tolower(trimws(person_name))

            if (!person_name_lower %in% valid_name_list) {
              warnings <- c(warnings, list(list(
                column = col,
                row = row,
                message = sprintf(
                  "Person '%s' not found in table_colnam. During import, you will be prompted to match or add this person.",
                  person_name
                ),
                value = person_name
              )))
            }
          }
        }
      }
    }
  }

  list(
    errors = errors,
    warnings = warnings,
    cleaned_data = cleaned_data,
    changes_made = changes_made
  )
}


#' Resolve Lookup Column with Interactive Matching
#'
#' Helper function to validate and interactively fix a lookup column.
#'
#' @keywords internal
.resolve_lookup_column <- function(data, column_name, lookup_function,
                                  lookup_value_col, lookup_id_col,
                                  lookup_table_name, con,
                                  interactive, fix_on_fly) {

  cli::cli_alert_info("=== RESOLVE LOOKUP COLUMN: {column_name} ===")

  errors <- list()
  changes <- data.frame(
    column = character(),
    row = integer(),
    original_value = character(),
    corrected_value = character(),
    method = character(),
    stringsAsFactors = FALSE
  )

  # Get valid values
  lookup_table <- tryCatch({
    lookup_function()
  }, error = function(e) {
    NULL
  })

  cli::cli_alert_info("Lookup table fetched: {nrow(lookup_table) %||% 0} rows")

  if (is.null(lookup_table)) {
    return(list(
      errors = errors,
      cleaned_data = data,
      changes = changes
    ))
  }

  valid_values <- lookup_table[[lookup_value_col]]
  valid_ids <- lookup_table[[lookup_id_col]]

  cli::cli_alert_info("Column data sample: {paste(head(data[[column_name]], 3), collapse=', ')}")
  cli::cli_alert_info("Valid values sample: {paste(head(valid_values, 3), collapse=', ')}")
  cli::cli_alert_info("Valid IDs sample: {paste(head(valid_ids, 3), collapse=', ')}")

  # Check if data contains IDs (numeric) or names (character)
  data_values <- data[[column_name]][!is.na(data[[column_name]]) & trimws(data[[column_name]]) != ""]
  are_numeric <- suppressWarnings(!any(is.na(as.numeric(data_values))))
  cli::cli_alert_info("Are values numeric (IDs)? {are_numeric}")

  # Find invalid values
  if (are_numeric) {
    cli::cli_alert_info("Validating against IDs")
    # Values are IDs - check against valid_ids
    invalid_rows <- which(
      !is.na(data[[column_name]]) &
      trimws(data[[column_name]]) != "" &
      !(as.numeric(data[[column_name]]) %in% valid_ids)
    )
  } else {
    cli::cli_alert_info("Validating against names")
    # Values are names - check against valid_values
    invalid_rows <- which(
      !is.na(data[[column_name]]) &
      trimws(data[[column_name]]) != "" &
      !(data[[column_name]] %in% valid_values)
    )
  }

  cli::cli_alert_info("Found {length(invalid_rows)} invalid row(s)")

  if (length(invalid_rows) == 0) {
    return(list(
      errors = errors,
      cleaned_data = data,
      changes = changes
    ))
  }

  # Get unique invalid values
  invalid_values <- unique(data[[column_name]][invalid_rows])

  # Interactive fixing
  if (interactive && fix_on_fly) {

    cli::cli_h3("Found {length(invalid_values)} invalid {column_name} value(s)")
    cli::cli_alert_info("Let's match them interactively...")
    cat("\n")

    # Use existing resolve_multiple_values()!
    resolved_ids <- resolve_multiple_values(
      missing_values = invalid_values,
      lookup_table = lookup_table,
      column_name = lookup_value_col,
      id_column = lookup_id_col,
      similarity_threshold = 0.6,
      allow_add = FALSE,  # Don't allow adding to lookup tables
      table_name = lookup_table_name,
      con = con
    )

    cat("\n")

    # Apply fixes
    for (orig_value in names(resolved_ids)) {
      resolved_id <- resolved_ids[orig_value]

      if (!is.na(resolved_id)) {
        # Get correct value
        correct_value <- lookup_table %>%
          dplyr::filter(!!rlang::sym(lookup_id_col) == resolved_id) %>%
          dplyr::pull(!!rlang::sym(lookup_value_col)) %>%
          dplyr::first()

        # Find rows to update
        rows_to_update <- which(data[[column_name]] == orig_value)

        # Update data
        data[[column_name]][rows_to_update] <- correct_value

        # Track changes
        for (row in rows_to_update) {
          changes <- rbind(changes, data.frame(
            column = column_name,
            row = row,
            original_value = orig_value,
            corrected_value = correct_value,
            method = "interactive",
            stringsAsFactors = FALSE
          ))
        }

        cli::cli_alert_success("Updated {length(rows_to_update)} row(s): '{orig_value}' → '{correct_value}'")

      } else {
        # User skipped - still an error
        for (row in which(data[[column_name]] == orig_value)) {
          errors <- c(errors, list(list(
            column = column_name,
            row = row,
            message = sprintf(
              "Invalid %s '%s' (user skipped interactive matching)",
              column_name, orig_value
            ),
            value = as.character(orig_value)
          )))
        }
      }
    }

  } else {
    # Non-interactive mode - just report errors
    for (row in invalid_rows) {
      val <- data[[column_name]][row]

      # Create appropriate error message based on whether it's an ID or name
      if (are_numeric) {
        error_msg <- sprintf(
          "Invalid %s ID '%s'. Value should be a valid %s ID from %s_list().",
          column_name, val, column_name, column_name
        )
      } else {
        error_msg <- sprintf(
          "Invalid %s '%s'. Use %s_list() to see valid values.",
          column_name, val, column_name
        )
      }

      errors <- c(errors, list(list(
        column = column_name,
        row = row,
        message = error_msg,
        value = as.character(val)
      )))
    }
  }

  list(
    errors = errors,
    cleaned_data = data,
    changes = changes
  )
}


#' Validate Unique Constraints
#'
#' Checks for duplicate values in columns that should be unique.
#'
#' @param data Data frame with schema column names
#' @param config Routing configuration
#'
#' @return List of error objects
#' @keywords internal
.validate_unique_constraints <- function(data, config) {
  errors <- list()

  # plot_name should be unique
  if ("plot_name" %in% names(data)) {
    duplicated_values <- data$plot_name[duplicated(data$plot_name) & !is.na(data$plot_name)]

    if (length(duplicated_values) > 0) {
      for (dup_val in unique(duplicated_values)) {
        dup_rows <- which(data$plot_name == dup_val)
        errors <- c(errors, list(list(
          column = "plot_name",
          row = paste(dup_rows, collapse = ", "),
          message = sprintf(
            "Duplicate plot_name '%s' found in rows: %s",
            dup_val,
            paste(dup_rows, collapse = ", ")
          ),
          value = as.character(dup_val)
        )))
      }
    }
  }

  errors
}


#' Check for Duplicate Plots in Database
#'
#' Detects potential duplicate plots by matching method, country, and coordinates
#' (rounded to 3 decimal places ~111m). Helps prevent re-importing existing plots
#' with different names (e.g., "FND32" vs "Releve32").
#'
#' @param data Data frame with plot data (must have method, country, ddlat, ddlon)
#' @param con Database connection
#'
#' @return List with warnings and errors
#' @keywords internal
.check_duplicate_plots <- function(data, con) {
  warnings <- list()
  errors <- list()

  # Only check if we have required columns
  required_cols <- c("method", "country", "ddlat", "ddlon")
  if (!all(required_cols %in% names(data))) {
    return(list(warnings = warnings, errors = errors))
  }

  tryCatch({
    # Get existing plots from database (respects row-level security)
    existing_plots <- DBI::dbGetQuery(con, "
      SELECT
        id_liste_plots,
        plot_name,
        id_method,
        id_country,
        ddlat,
        ddlon
      FROM data_liste_plots
      WHERE ddlat IS NOT NULL
        AND ddlon IS NOT NULL
    ")

    if (nrow(existing_plots) == 0) {
      return(list(warnings = warnings, errors = errors))
    }

    # Get method and country lookup tables for matching IDs to names
    method_lookup <- method_list()
    country_lookup <- country_list()

    # Create reverse lookups (name -> ID)
    method_name_to_id <- setNames(method_lookup$id_method, method_lookup$method)
    country_name_to_id <- setNames(country_lookup$id_country, country_lookup$country)

    # Round existing plot coordinates (3 decimals ~111m)
    existing_plots$ddlat_rounded <- round(existing_plots$ddlat, 3)
    existing_plots$ddlon_rounded <- round(existing_plots$ddlon, 3)

    # Check each plot in the import data
    for (i in 1:nrow(data)) {
      row_data <- data[i, ]

      # Skip if coordinates are missing
      if (is.na(row_data$ddlat) || is.na(row_data$ddlon)) next

      # Convert method and country names to IDs for comparison
      method_id <- method_name_to_id[[as.character(row_data$method)]]
      country_id <- country_name_to_id[[as.character(row_data$country)]]

      if (is.null(method_id) || is.null(country_id)) next

      # Round import coordinates
      lat_rounded <- round(row_data$ddlat, 3)
      lon_rounded <- round(row_data$ddlon, 3)

      # Find matching plots (same method, country, and coordinates)
      matches <- existing_plots[
        existing_plots$id_method == method_id &
        existing_plots$id_country == country_id &
        existing_plots$ddlat_rounded == lat_rounded &
        existing_plots$ddlon_rounded == lon_rounded,
      ]

      if (nrow(matches) > 0) {
        # Found potential duplicate(s)
        match_names <- paste(matches$plot_name, collapse = ", ")

        warnings <- c(warnings, list(list(
          column = "plot_name",
          row = i,
          message = sprintf(
            "⚠️ Potential duplicate: Plot '%s' has same location (%.3f, %.3f), method, and country as existing plot(s): %s. This may be a re-import of an existing plot with a different name.",
            row_data$plot_name,
            lat_rounded,
            lon_rounded,
            match_names
          ),
          value = as.character(row_data$plot_name)
        )))
      }
    }

  }, error = function(e) {
    # Log error but don't fail validation
    cli::cli_alert_warning("Could not check for duplicate plots: {e$message}")
  })

  list(warnings = warnings, errors = errors)
}


#' Print Validation Results
#'
#' Pretty-prints validation results with color coding and clear formatting.
#'
#' @param validation Validation result object from validate_plot_metadata()
#' @param show_all Logical: Show all issues or just summary (default FALSE)
#'
#' @export
print_validation_results <- function(validation, show_all = FALSE) {
  cat("\n")
  cat(cli::rule(
    left = "Plot Metadata Validation Results",
    right = ifelse(validation$valid, "PASSED", "FAILED"),
    line = 2
  ))
  cat("\n\n")

  # Summary
  cat(cli::col_cyan("Summary:\n"))
  cat(sprintf("  Total rows: %d\n", validation$summary$total_rows))
  cat(sprintf("  Total columns: %d\n", validation$summary$total_columns))
  cat(sprintf("  Mapped columns: %d\n", validation$summary$mapped_columns))
  cat("\n")

  # Errors
  if (nrow(validation$errors) > 0) {
    cat(cli::col_red(sprintf("✗ Errors: %d\n", nrow(validation$errors))))

    if (show_all || nrow(validation$errors) <= 20) {
      cat("\n")
      for (i in seq_len(nrow(validation$errors))) {
        err <- validation$errors[i, ]
        cat(cli::col_red(sprintf(
          "  [Row %s, Column '%s'] %s\n",
          ifelse(is.na(err$row), "N/A", err$row),
          err$column,
          err$message
        )))
        if (!is.na(err$value)) {
          cat(sprintf("    Value: '%s'\n", err$value))
        }
      }
    } else {
      cat(sprintf("  (Showing first 20 of %d errors)\n\n", nrow(validation$errors)))
      for (i in 1:20) {
        err <- validation$errors[i, ]
        cat(cli::col_red(sprintf(
          "  [Row %s, Column '%s'] %s\n",
          ifelse(is.na(err$row), "N/A", err$row),
          err$column,
          err$message
        )))
      }
      cat(cli::col_red(sprintf("\n  ... and %d more errors\n", nrow(validation$errors) - 20)))
    }
  } else {
    cat(cli::col_green("✓ No errors\n"))
  }

  cat("\n")

  # Warnings
  if (nrow(validation$warnings) > 0) {
    cat(cli::col_yellow(sprintf("⚠ Warnings: %d\n", nrow(validation$warnings))))

    if (show_all || nrow(validation$warnings) <= 20) {
      cat("\n")
      for (i in seq_len(nrow(validation$warnings))) {
        warn <- validation$warnings[i, ]
        cat(cli::col_yellow(sprintf(
          "  [Row %s, Column '%s'] %s\n",
          ifelse(is.na(warn$row), "N/A", warn$row),
          warn$column,
          warn$message
        )))
        if (!is.na(warn$value)) {
          cat(sprintf("    Value: '%s'\n", warn$value))
        }
      }
    } else {
      cat(sprintf("  (Showing first 20 of %d warnings)\n\n", nrow(validation$warnings)))
      for (i in 1:20) {
        warn <- validation$warnings[i, ]
        cat(cli::col_yellow(sprintf(
          "  [Row %s, Column '%s'] %s\n",
          ifelse(is.na(warn$row), "N/A", warn$row),
          warn$column,
          warn$message
        )))
      }
      cat(cli::col_yellow(sprintf("\n  ... and %d more warnings\n", nrow(validation$warnings) - 20)))
    }
  } else {
    cat(cli::col_green("✓ No warnings\n"))
  }

  cat("\n")
  cat(cli::rule(line = 2))

  if (validation$valid) {
    cat(cli::col_green("\n✓ Validation passed! Data is ready for import.\n"))
    if (nrow(validation$warnings) > 0) {
      cat(cli::col_yellow("  Note: There are warnings that should be reviewed.\n"))
    }
  } else {
    cat(cli::col_red("\n✗ Validation failed! Please fix errors before importing.\n"))
  }

  if (validation$strict && nrow(validation$warnings) > 0) {
    cat(cli::col_yellow("  Running in strict mode: warnings treated as errors.\n"))
  }

  cat("\n")

  invisible(validation)
}


#' Print method for validation results
#'
#' @param x Validation result object
#' @param ... Additional arguments passed to print_validation_results
#'
#' @export
print.plot_validation_result <- function(x, ...) {
  print_validation_results(x, ...)
}
