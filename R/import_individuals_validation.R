# Import Validation Functions for Individual Data
#
# This file contains functions for validating individual tree data before import.
# Validation uses database rules from traits_list() and checks against
# plot access, taxonomy database, and existing individuals.

#' Validate Individual Data Before Import
#'
#' Comprehensive validation of individual tree data using database rules and checks.
#' Returns structured results with severity levels (error vs warning).
#' Can interactively fix issues using fuzzy matching.
#'
#' @param individuals_data Data frame with individual data (required)
#' @param features_data Data frame with feature/trait data (optional)
#' @param method Method type (e.g., "1ha-IRD", "Large"). Used for method-specific validation.
#' @param con Database connection (optional, will create if NULL)
#' @param strict Logical: If TRUE, warnings are treated as errors (default FALSE)
#' @param interactive Logical: If TRUE, allow interactive fixing (default TRUE)
#' @param fix_on_fly Logical: If TRUE, fix issues during validation (default TRUE)
#'
#' @return List with validation results:
#'   \item{valid}{Logical: TRUE if no errors (warnings allowed)}
#'   \item{errors}{Data frame of error messages}
#'   \item{warnings}{Data frame of warning messages}
#'   \item{summary}{Summary statistics}
#'   \item{original_data}{Original input data}
#'   \item{cleaned_data}{List with individuals and features (fixes applied)}
#'   \item{changes_made}{Data frame documenting changes}
#'
#' @section Validation Checks:
#'
#' **Individuals Sheet**:
#' - Required fields: plot_name, tag, idtax_n, original_tax_name
#' - Plot existence and access
#' - Taxonomy ID existence in taxa database
#' - Tag uniqueness within plot
#' - Tag numeric and valid
#' - No duplicate tags with existing database records
#' - Method-specific requirements
#'
#' **Features Sheet** (if provided):
#' - Linking columns present (plot_name, tag)
#' - Match to individuals in import
#' - Trait value types (numeric vs character)
#' - Min/max value ranges per trait
#' - Expected units
#'
#' @examples
#' \dontrun{
#' # After column mapping
#' mapped <- map_individual_columns(individuals, features)
#'
#' # Validate data
#' validation <- validate_individual_data(
#'   individuals_data = mapped$individuals,
#'   features_data = mapped$features,
#'   method = "1ha-IRD",
#'   interactive = TRUE
#' )
#'
#' # Check results
#' print_validation_results(validation)
#'
#' if (!validation$valid) {
#'   stop("Validation failed!")
#' }
#'
#' # Use cleaned data for import
#' import_individual_data(
#'   data = validation$cleaned_data,
#'   validation = validation
#' )
#' }
#'
#' @export
validate_individual_data <- function(individuals_data,
                                     features_data = NULL,
                                     method = NULL,
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

  cli::cli_h1("Validating Individual Data")

  # Store original data
  original_individuals <- individuals_data
  original_features <- features_data

  # Initialize results storage
  errors <- list()
  warnings <- list()
  all_changes <- data.frame(
    sheet = character(),
    column = character(),
    row = integer(),
    original_value = character(),
    corrected_value = character(),
    method = character(),
    stringsAsFactors = FALSE
  )

  # Working copies
  validated_individuals <- individuals_data
  validated_features <- features_data

  # -------------------------------------------------------------------
  # INDIVIDUALS SHEET VALIDATION
  # -------------------------------------------------------------------

  cli::cli_h2("Step 1: Validating individuals sheet")

  # 0. Auto-generate tags if missing
  cli::cli_alert_info("Checking tag column...")

  missing_all_tags <- !"tag" %in% names(validated_individuals) || all(is.na(validated_individuals$tag))
  missing_some_tags <- "tag" %in% names(validated_individuals) && any(is.na(validated_individuals$tag)) && !missing_all_tags

  if (missing_all_tags || missing_some_tags) {
    # Display warning about missing tags
    cat("\n")
    cli::cli_div(theme = list(rule = list(color = "yellow")))
    cli::cli_rule(left = "WARNING: Missing Tag Values")
    cli::cli_end()
    cat("\n")

    if (missing_all_tags) {
      cli::cli_alert_warning("Tag column is completely missing or all values are NA")
      cli::cli_ul(c(
        "Total individuals to import: {.strong {nrow(validated_individuals)}}",
        "Affected plots: {.strong {length(unique(validated_individuals$plot_name))}}"
      ))
    } else {
      na_count <- sum(is.na(validated_individuals$tag))
      cli::cli_alert_warning("{na_count} individual(s) have missing tag values")
      na_plots <- validated_individuals %>%
        dplyr::filter(is.na(tag)) %>%
        dplyr::pull(plot_name) %>%
        unique()
      cli::cli_ul(c(
        "Rows with missing tags: {.strong {na_count}}",
        "Affected plots: {.strong {paste(na_plots, collapse = ', ')}}"
      ))
    }

    cat("\n")
    cli::cli_alert_info("{.strong IMPORTANT:} Tags uniquely identify individual stems/trees per inventory")
    cli::cli_ul(c(
      "{.emph Tags are NOT mandatory} but are {.strong highly recommended}",
      "Tags allow tracking of the same individual across multiple censuses",
      "Missing tags can indicate a data entry error in your dataset"
    ))
    cat("\n")

    cli::cli_alert_info("Auto-generation behavior:")
    cli::cli_ul(c(
      "Will generate sequential numbers: {.strong 1, 2, 3, ...} for each plot",
      "Each plot gets its own independent sequence starting at 1",
      "Generated tags may {.strong not match} your field notes or previous censuses"
    ))
    cat("\n")

    # Ask for confirmation in interactive mode
    if (interactive && interactive()) {
      cli::cli_alert_warning("Please confirm this is {.strong intentional} and not a data error")
      response <- readline(prompt = "Proceed with auto-generation of tags? (yes/no): ")

      if (!tolower(trimws(response)) %in% c("yes", "y")) {
        cli::cli_alert_danger("Validation aborted by user")
        cli::cli_alert_info("Please check your data and ensure tag column is properly filled")
        stop("Validation stopped: User chose not to proceed with missing tags", call. = FALSE)
      }
      cat("\n")
    } else if (interactive) {
      # Non-interactive mode but interactive parameter is TRUE - show message
      cli::cli_alert_info("Running in non-interactive mode - proceeding with auto-generation")
      cat("\n")
    }

    # Proceed with generation
    cli::cli_alert_info("Generating sequential tags...")
    validated_individuals <- .generate_sequential_tags(validated_individuals)

    if (missing_all_tags) {
      all_changes <- rbind(all_changes, data.frame(
        step = "Tag Generation",
        change = "Generated sequential tags (1 to n) for all individuals per plot",
        rows_affected = nrow(validated_individuals),
        stringsAsFactors = FALSE
      ))
      cli::cli_alert_success("Generated tags for {nrow(validated_individuals)} individuals")
    } else {
      na_count <- sum(is.na(original_individuals$tag))
      all_changes <- rbind(all_changes, data.frame(
        step = "Tag Generation",
        change = sprintf("Generated sequential tags for %d rows with missing tags", na_count),
        rows_affected = na_count,
        stringsAsFactors = FALSE
      ))
      cli::cli_alert_success("Generated tags for {na_count} individuals with missing values")
    }

    # IMPORTANT: If features exist and tags were generated, update features with new tags
    if (!is.null(features_data)) {
      if (!"tag" %in% names(features_data) || any(is.na(features_data$tag))) {
        cli::cli_alert_info("Syncing auto-generated tags to features sheet...")

        # Create linking key from validated_individuals
        indiv_links <- validated_individuals[, c("plot_name", "tag"), drop = FALSE]

        # If features doesn't have tag column, add it
        if (!"tag" %in% names(features_data)) {
          features_data$tag <- NA
        }

        # Match features to individuals by plot_name (and tag if it exists)
        # Assuming same row order between individuals and features (common case)
        if (nrow(features_data) == nrow(validated_individuals)) {
          features_data$tag <- validated_individuals$tag
          cli::cli_alert_success("Tags synced to features sheet")
        } else {
          # Different row counts - need to match by plot_name
          cli::cli_alert_warning("Cannot auto-sync tags: row count mismatch between individuals and features")
        }

        validated_features <- features_data
      }
    }

    cat("\n")
  } else {
    cli::cli_alert_success("All individuals have tag values")
  }

  # 1. Required fields (tag is now optional - handled above)
  cli::cli_alert_info("Checking required fields...")
  required_cols <- c("plot_name", "idtax_n", "original_tax_name")
  required_check <- .validate_required_fields_individuals(
    validated_individuals,
    required_cols
  )
  if (length(required_check) > 0) {
    errors <- c(errors, required_check)
  }

  # 2. Tag validation (numeric, not 0, after auto-generation)
  cli::cli_alert_info("Checking tag values...")
  tag_check <- .validate_tag_values(validated_individuals)
  errors <- c(errors, tag_check$errors)
  warnings <- c(warnings, tag_check$warnings)

  # 3. Plot existence and access
  cli::cli_alert_info("Checking plot existence and access...")
  plot_check <- .validate_plot_access(data = validated_individuals, con)
  errors <- c(errors, plot_check$errors)
  warnings <- c(warnings, plot_check$warnings)

  # 4. Taxonomy validation (idtax_n exists in taxa database)
  cli::cli_alert_info("Checking taxonomy IDs...")
  taxa_check <- .validate_taxonomy_ids(validated_individuals, con)
  errors <- c(errors, taxa_check$errors)
  warnings <- c(warnings, taxa_check$warnings)

  # 5. Tag uniqueness within plot (in import data)
  cli::cli_alert_info("Checking tag uniqueness within plots...")
  unique_check <- .validate_tag_uniqueness_import(validated_individuals)
  warnings <- c(warnings, unique_check)  # Warning only - duplicate tags may be intentional

  # 6. Tag conflicts with existing database
  cli::cli_alert_info("Checking for conflicts with existing individuals...")
  conflict_check <- .validate_tag_conflicts_database(validated_individuals, con)
  warnings <- c(warnings, conflict_check)  # Warnings for now, user can decide

  # 7. Method-specific validation
  if (!is.null(method)) {
    cli::cli_alert_info("Checking method-specific requirements...")
    method_check <- .validate_method_requirements(validated_individuals, method)
    errors <- c(errors, method_check)
  }

  # -------------------------------------------------------------------
  # FEATURES SHEET VALIDATION (if provided)
  # -------------------------------------------------------------------

  if (!is.null(features_data)) {
    cli::cli_h2("Step 2: Validating features sheet")

    # 1. Linking columns present
    cli::cli_alert_info("Checking linking columns...")
    linking_check <- .validate_feature_linking_columns(features_data = validated_features)
    if (length(linking_check) > 0) {
      errors <- c(errors, linking_check)
    }

    # 2. Features match individuals in import
    cli::cli_alert_info("Checking feature-individual linkage...")
    linkage_check <- .validate_feature_individual_linkage(
      validated_features,
      validated_individuals
    )
    errors <- c(errors, linkage_check$errors)
    warnings <- c(warnings, linkage_check$warnings)

    # 3. Trait value types and ranges
    cli::cli_alert_info("Checking trait value types and ranges...")
    trait_check <- .validate_trait_values(validated_features, con)
    errors <- c(errors, trait_check$errors)
    warnings <- c(warnings, trait_check$warnings)

  } else {
    cli::cli_alert_info("No features sheet provided - skipping feature validation")
  }

  # -------------------------------------------------------------------
  # COMPILE RESULTS
  # -------------------------------------------------------------------

  cat("\n")
  cli::cli_rule("Validation Summary")
  cat("\n")

  # Convert lists to data frames
  errors_df <- .compile_validation_messages(errors, "error")
  warnings_df <- .compile_validation_messages(warnings, "warning")

  # Determine if valid
  is_valid <- nrow(errors_df) == 0
  if (strict && nrow(warnings_df) > 0) {
    is_valid <- FALSE
  }

  # Summary stats
  summary_stats <- list(
    total_individuals = nrow(validated_individuals),
    total_features = if (!is.null(validated_features)) nrow(validated_features) else 0,
    unique_plots = length(unique(validated_individuals$plot_name)),
    unique_taxa = length(unique(validated_individuals$idtax_n)),
    errors = nrow(errors_df),
    warnings = nrow(warnings_df),
    changes_made = nrow(all_changes),
    valid = is_valid
  )

  # Print summary
  if (is_valid) {
    cli::cli_alert_success("Validation passed!")
  } else {
    cli::cli_alert_danger("Validation failed with {nrow(errors_df)} error(s)")
  }

  if (nrow(warnings_df) > 0) {
    cli::cli_alert_warning("{nrow(warnings_df)} warning(s) found")
  }

  cat("\n")
  cli::cli_alert_info("Summary:")
  cli::cli_ul(c(
    "Individuals: {summary_stats$total_individuals}",
    "Features: {summary_stats$total_features}",
    "Unique plots: {summary_stats$unique_plots}",
    "Unique taxa: {summary_stats$unique_taxa}",
    "Errors: {summary_stats$errors}",
    "Warnings: {summary_stats$warnings}"
  ))
  cat("\n")

  # Cleanup
  if (close_on_exit) {
    DBI::dbDisconnect(con)
  }

  # Return results
  result <- list(
    valid = is_valid,
    errors = errors_df,
    warnings = warnings_df,
    summary = summary_stats,
    original_data = list(
      individuals = original_individuals,
      features = original_features
    ),
    cleaned_data = list(
      individuals = validated_individuals,
      features = validated_features
    ),
    changes_made = all_changes
  )

  invisible(result)
}


#' Validate Required Fields for Individuals (Internal)
#'
#' @param data Data frame
#' @param required_cols Required column names
#' @return List of error messages
#' @keywords internal
.validate_required_fields_individuals <- function(data, required_cols) {
  errors <- list()

  for (col in required_cols) {
    if (!col %in% names(data)) {
      errors <- c(errors, list(sprintf(
        "Missing required column: %s", col
      )))
    } else {
      # Check for NA values
      na_count <- sum(is.na(data[[col]]))
      if (na_count > 0) {
        errors <- c(errors, list(sprintf(
          "Column '%s' has %d NA value(s) (must not be empty)",
          col, na_count
        )))
      }

      # Check for empty strings (character columns)
      if (is.character(data[[col]])) {
        empty_count <- sum(data[[col]] == "" | trimws(data[[col]]) == "", na.rm = TRUE)
        if (empty_count > 0) {
          errors <- c(errors, list(sprintf(
            "Column '%s' has %d empty value(s) (must not be empty)",
            col, empty_count
          )))
        }
      }
    }
  }

  return(errors)
}


#' Validate Tag Values (Internal)
#'
#' Tags must be numeric and valid (not 0, not NA).
#'
#' @param data Data frame with tag column
#' @return List with errors and warnings
#' @keywords internal
.validate_tag_values <- function(data) {
  errors <- list()
  warnings <- list()

  if (!"tag" %in% names(data)) {
    return(list(errors = errors, warnings = warnings))
  }

  # Check if numeric
  if (!is.numeric(data$tag)) {
    # Try to convert
    tag_numeric <- suppressWarnings(as.numeric(data$tag))
    if (any(is.na(tag_numeric) & !is.na(data$tag))) {
      non_numeric_rows <- which(is.na(tag_numeric) & !is.na(data$tag))
      errors <- c(errors, list(sprintf(
        "Tag column has non-numeric values at rows: %s",
        paste(non_numeric_rows, collapse = ", ")
      )))
    }
  }

  # Check for zero values
  if (is.numeric(data$tag)) {
    zero_rows <- which(!is.na(data$tag) & data$tag == 0)
    if (length(zero_rows) > 0) {
      errors <- c(errors, list(sprintf(
        "Tag cannot be 0 (rows: %s)",
        paste(zero_rows, collapse = ", ")
      )))
    }
  }

  # Check for negative values
  if (is.numeric(data$tag)) {
    neg_rows <- which(!is.na(data$tag) & data$tag < 0)
    if (length(neg_rows) > 0) {
      warnings <- c(warnings, list(sprintf(
        "Tag has negative values at rows: %s (unusual but allowed)",
        paste(neg_rows, collapse = ", ")
      )))
    }
  }

  return(list(errors = errors, warnings = warnings))
}


#' Validate Plot Access (Internal)
#'
#' Check that plots exist in database and user has access.
#'
#' @param data Data frame with plot_name column
#' @param con Database connection
#' @return List with errors and warnings
#' @keywords internal
.validate_plot_access <- function(data, con) {
  errors <- list()
  warnings <- list()

  if (!"plot_name" %in% names(data)) {
    return(list(errors = errors, warnings = warnings))
  }

  unique_plots <- unique(data$plot_name)

  # Query ONLY the plots specified in data using EXACT matching
  # exact_match = TRUE forces exact name matching (no LIKE pattern matching)
  # This prevents '41' from matching 'Plot-41', '4100', etc.
  user_plots <- tryCatch({
    query_plots(plot_name = unique_plots, exact_match = TRUE, con = con, output_style = "full")
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(user_plots)) {
    warnings <- c(warnings, list(
      "Could not retrieve plots - skipping access check"
    ))
    return(list(errors = errors, warnings = warnings))
  }

  accessible_plot_names <- user_plots$extract$plot_name

  # Check each plot - find which ones were NOT returned (don't exist or no access)
  for (plot in unique_plots) {
    if (!plot %in% accessible_plot_names) {
      errors <- c(errors, list(sprintf(
        "Plot '%s' does not exist or user does not have access",
        plot
      )))
    }
  }

  return(list(errors = errors, warnings = warnings))
}


#' Validate Taxonomy IDs (Internal)
#'
#' Check that idtax_n values exist in taxa database.
#'
#' @param data Data frame with idtax_n column
#' @param con Database connection
#' @return List with errors and warnings
#' @keywords internal
.validate_taxonomy_ids <- function(data, con) {
  errors <- list()
  warnings <- list()

  if (!"idtax_n" %in% names(data)) {
    return(list(errors = errors, warnings = warnings))
  }

  # Check for 0 values (not allowed)
  zero_rows <- which(!is.na(data$idtax_n) & data$idtax_n == 0)
  if (length(zero_rows) > 0) {
    errors <- c(errors, list(sprintf(
      "idtax_n cannot be 0 (rows: %s)",
      paste(zero_rows, collapse = ", ")
    )))
  }

  unique_taxa <- unique(data$idtax_n)
  unique_taxa <- unique_taxa[!is.na(unique_taxa) & unique_taxa != 0]

  if (length(unique_taxa) == 0) {
    return(list(errors = errors, warnings = warnings))
  }

  # Query taxa database
  con_taxa <- tryCatch({
    call.mydb.taxa()
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(con_taxa)) {
    warnings <- c(warnings, list(
      "Could not connect to taxa database - skipping taxonomy validation"
    ))
    return(list(errors = errors, warnings = warnings))
  }

  # Only disconnect if it's not a pool (pools are managed by the app)
  is_pool <- inherits(con_taxa, "Pool")
  if (!is_pool) {
    on.exit(DBI::dbDisconnect(con_taxa), add = TRUE)
  }

  # Check which taxa exist (use dplyr to support both pools and regular connections)
  taxa_table <- tryCatch({
    dplyr::tbl(con_taxa, "table_taxa") %>%
      dplyr::select(idtax_n) %>%
      dplyr::distinct() %>%
      dplyr::collect()
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(taxa_table)) {
    warnings <- c(warnings, list(
      "Could not read table_taxa - skipping taxonomy validation"
    ))
    return(list(errors = errors, warnings = warnings))
  }

  existing_taxa <- taxa_table$idtax_n

  # Find missing taxa
  missing_taxa <- setdiff(unique_taxa, existing_taxa)

  if (length(missing_taxa) > 0) {
    errors <- c(errors, list(sprintf(
      "idtax_n values not found in taxa database: %s",
      paste(missing_taxa, collapse = ", ")
    )))
  }

  return(list(errors = errors, warnings = warnings))
}


#' Validate Tag Uniqueness in Import Data (Internal)
#'
#' Tags must be unique within each plot in the import data.
#'
#' @param data Data frame with plot_name and tag columns
#' @return List of error messages
#' @keywords internal
.validate_tag_uniqueness_import <- function(data) {
  errors <- list()

  if (!"plot_name" %in% names(data) || !"tag" %in% names(data)) {
    return(errors)
  }

  # Check for duplicates within each plot
  data_with_row <- data
  data_with_row$row_num <- seq_len(nrow(data))

  duplicates <- data_with_row %>%
    dplyr::group_by(plot_name, tag) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange(plot_name, tag) %>%
    dplyr::ungroup()

  if (nrow(duplicates) > 0) {
    dup_summary <- duplicates %>%
      dplyr::group_by(plot_name, tag) %>%
      dplyr::summarise(
        rows = paste(row_num, collapse = ", "),
        .groups = "drop"
      )

    for (i in 1:nrow(dup_summary)) {
      errors <- c(errors, list(sprintf(
        "Duplicate tag %s in plot '%s' (rows: %s)",
        dup_summary$tag[i],
        dup_summary$plot_name[i],
        dup_summary$rows[i]
      )))
    }
  }

  return(errors)
}


#' Validate Tag Conflicts with Database (Internal)
#'
#' Check if any plot+tag combinations already exist in database.
#'
#' @param data Data frame with plot_name and tag columns
#' @param con Database connection
#' @return List of warning messages
#' @keywords internal
.validate_tag_conflicts_database <- function(data, con) {
  warnings <- list()

  if (!"plot_name" %in% names(data) || !"tag" %in% names(data)) {
    return(warnings)
  }

  unique_plots <- unique(data$plot_name)

  # Query existing individuals for these plots
  for (plot in unique_plots) {
    existing_indiv <- tryCatch({
      DBI::dbGetQuery(con, sprintf(
        "SELECT tag FROM data_individuals WHERE plot_name = '%s'",
        plot
      ))
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(existing_indiv)) {
      next
    }

    import_tags <- data$tag[data$plot_name == plot]
    existing_tags <- existing_indiv$tag

    conflicts <- intersect(import_tags, existing_tags)

    if (length(conflicts) > 0) {
      warnings <- c(warnings, list(sprintf(
        "Plot '%s' has tags that already exist in database: %s (will need to handle duplicates or updates)",
        plot,
        paste(conflicts, collapse = ", ")
      )))
    }
  }

  return(warnings)
}


#' Validate Method-Specific Requirements (Internal)
#'
#' Check method-specific required fields.
#'
#' @param data Data frame
#' @param method Method name
#' @return List of error messages
#' @keywords internal
.validate_method_requirements <- function(data, method) {
  errors <- list()

  # Methods that require tag to be mandatory
  tag_required_methods <- c("1ha-IRD", "Large")

  if (method %in% tag_required_methods) {
    if (!"tag" %in% names(data)) {
      errors <- c(errors, list(sprintf(
        "Method '%s' requires 'tag' column",
        method
      )))
    } else {
      na_tags <- sum(is.na(data$tag))
      if (na_tags > 0) {
        errors <- c(errors, list(sprintf(
          "Method '%s' requires all individuals to have tags (%d missing)",
          method, na_tags
        )))
      }
    }
  }

  return(errors)
}


#' Validate Feature Linking Columns (Internal)
#'
#' Check that features sheet has required linking columns.
#'
#' @param features_data Features data frame
#' @return List of error messages
#' @keywords internal
.validate_feature_linking_columns <- function(features_data) {
  errors <- list()

  required_linking <- c("plot_name", "tag")

  for (col in required_linking) {
    if (!col %in% names(features_data)) {
      errors <- c(errors, list(sprintf(
        "Features sheet missing required linking column: %s",
        col
      )))
    }
  }

  return(errors)
}


#' Validate Feature-Individual Linkage (Internal)
#'
#' Check that features link to individuals in import.
#'
#' @param features_data Features data frame
#' @param individuals_data Individuals data frame
#' @return List with errors and warnings
#' @keywords internal
.validate_feature_individual_linkage <- function(features_data, individuals_data) {
  errors <- list()
  warnings <- list()

  if (!"plot_name" %in% names(features_data) || !"tag" %in% names(features_data)) {
    return(list(errors = errors, warnings = warnings))
  }

  # Create keys for matching
  features_keys <- paste(features_data$plot_name, features_data$tag, sep = "||")
  individuals_keys <- paste(individuals_data$plot_name, individuals_data$tag, sep = "||")

  # Find features without matching individuals
  orphan_features <- features_keys[!features_keys %in% individuals_keys]

  if (length(orphan_features) > 0) {
    orphan_indices <- which(features_keys %in% orphan_features)
    errors <- c(errors, list(sprintf(
      "Features sheet has %d row(s) that don't match individuals in import (rows: %s)",
      length(orphan_indices),
      paste(head(orphan_indices, 10), collapse = ", ")
    )))
  }

  return(list(errors = errors, warnings = warnings))
}


#' Validate Trait Values (Internal)
#'
#' Check trait value types and ranges.
#'
#' @param features_data Features data frame
#' @param con Database connection
#' @return List with errors and warnings
#' @keywords internal
.validate_trait_values <- function(features_data, con) {
  errors <- list()
  warnings <- list()

  # Get trait definitions
  all_traits <- traits_list()

  # Exclude linking columns
  linking_cols <- c("plot_name", "tag", "census_date", "census_id")
  trait_cols <- setdiff(names(features_data), linking_cols)

  for (trait_col in trait_cols) {
    # Check if trait exists in database
    trait_info <- all_traits %>%
      dplyr::filter(trait == trait_col)

    if (nrow(trait_info) == 0) {
      warnings <- c(warnings, list(sprintf(
        "Trait '%s' not found in traits_list() - skipping validation",
        trait_col
      )))
      next
    }

    trait_info <- trait_info[1, ]  # Take first if multiple

    # Check value type
    expected_type <- trait_info$valuetype
    actual_values <- features_data[[trait_col]]
    actual_values <- actual_values[!is.na(actual_values)]

    if (length(actual_values) == 0) {
      next  # No values to validate
    }

    if (expected_type == "numeric" || expected_type == "integer") {
      if (!is.numeric(actual_values)) {
        # Try conversion
        converted <- suppressWarnings(as.numeric(actual_values))
        if (any(is.na(converted) & !is.na(actual_values))) {
          errors <- c(errors, list(sprintf(
            "Trait '%s' expects numeric values but has non-numeric data",
            trait_col
          )))
          next
        }
        actual_values <- converted
      }

      # Check ranges
      if (!is.na(trait_info$minallowedvalue)) {
        below_min <- sum(actual_values < trait_info$minallowedvalue, na.rm = TRUE)
        if (below_min > 0) {
          errors <- c(errors, list(sprintf(
            "Trait '%s' has %d value(s) below minimum allowed (%s)",
            trait_col, below_min, trait_info$minallowedvalue
          )))
        }
      }

      if (!is.na(trait_info$maxallowedvalue)) {
        above_max <- sum(actual_values > trait_info$maxallowedvalue, na.rm = TRUE)
        if (above_max > 0) {
          errors <- c(errors, list(sprintf(
            "Trait '%s' has %d value(s) above maximum allowed (%s)",
            trait_col, above_max, trait_info$maxallowedvalue
          )))
        }
      }
    }
  }

  return(list(errors = errors, warnings = warnings))
}


#' Compile Validation Messages (Internal)
#'
#' Convert list of messages to data frame.
#'
#' @param messages List of message strings
#' @param severity "error" or "warning"
#' @return Data frame
#' @keywords internal
.compile_validation_messages <- function(messages, severity) {
  if (length(messages) == 0) {
    return(data.frame(
      severity = character(),
      message = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    severity = severity,
    message = unlist(messages),
    stringsAsFactors = FALSE
  )
}


#' Print Individual Validation Results
#'
#' Pretty print individual data validation results.
#'
#' @param validation Validation result from validate_individual_data()
#'
#' @export
print_individual_validation_results <- function(validation) {
  cat("\n")
  cli::cli_rule("Individual Data Validation Results")
  cat("\n")

  if (validation$valid) {
    cli::cli_alert_success("VALIDATION PASSED")
  } else {
    cli::cli_alert_danger("VALIDATION FAILED")
  }

  cat("\n")
  cli::cli_alert_info("Summary:")
  cli::cli_ul(c(
    "Individuals: {validation$summary$total_individuals}",
    "Features: {validation$summary$total_features}",
    "Errors: {validation$summary$errors}",
    "Warnings: {validation$summary$warnings}"
  ))
  cat("\n")

  if (nrow(validation$errors) > 0) {
    cli::cli_h3("Errors:")
    for (i in 1:nrow(validation$errors)) {
      cli::cli_alert_danger(validation$errors$message[i])
    }
    cat("\n")
  }

  if (nrow(validation$warnings) > 0) {
    cli::cli_h3("Warnings:")
    for (i in 1:nrow(validation$warnings)) {
      cli::cli_alert_warning(validation$warnings$message[i])
    }
    cat("\n")
  }
}


#' Generate Sequential Tags Per Plot (Internal)
#'
#' Automatically generates sequential tag numbers (1 to n) for each plot
#' when tags are missing. Each plot gets its own sequence starting from 1.
#'
#' @param data Data frame with plot_name column and optionally a tag column
#' @return Data frame with tag column populated
#' @keywords internal
.generate_sequential_tags <- function(data) {
  # Ensure tag column exists
  if (!"tag" %in% names(data)) {
    data$tag <- NA_integer_
  }

  # Convert tag to numeric if it isn't already
  if (!is.numeric(data$tag)) {
    data$tag <- as.numeric(data$tag)
  }

  # Generate sequential tags per plot
  # Only generate for rows where tag is NA
  data <- data %>%
    dplyr::group_by(plot_name) %>%
    dplyr::mutate(
      tag = dplyr::if_else(
        is.na(tag),
        as.numeric(dplyr::row_number()),
        tag
      )
    ) %>%
    dplyr::ungroup()

  return(data)
}
