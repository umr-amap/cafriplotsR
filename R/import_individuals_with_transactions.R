# Import Functions for Individual Data with Transaction Support
#
# This file contains functions for importing individual tree data with
# transaction support (all-or-nothing imports with automatic rollback).

#' Import Individual Data with Transaction Support
#'
#' Imports individual tree data into the database using transactions for safety.
#' Inserts into data_individuals table and optionally data_ind_measures_feat for traits.
#' Supports dry-run mode for preview without committing.
#'
#' @param individuals_data Data frame containing individual data (required)
#' @param features_data Data frame containing trait/feature data (optional)
#' @param validation Validation result from validate_individual_data() (optional but recommended)
#' @param method Method type (e.g., "1ha-IRD")
#' @param con Database connection (optional, will create if NULL)
#' @param dry_run Logical: If TRUE, preview changes without committing (default FALSE)
#' @param progress Logical: If TRUE, show progress messages (default TRUE)
#' @param ask_confirmation Logical: If TRUE, ask user to confirm before importing (default TRUE)
#'
#' @return List with import results:
#'   \item{success}{Logical: TRUE if import succeeded}
#'   \item{n_individuals}{Number of individuals imported}
#'   \item{n_features}{Number of feature records imported}
#'   \item{plot_names}{Unique plots affected}
#'   \item{username}{Username who performed import}
#'   \item{dry_run}{Was this a dry-run?}
#'   \item{message}{Summary message}
#'
#' @section Pre-requisites:
#' 1. **Taxonomy must be standardized** - Use `match_taxonomic_names()` or Shiny app
#' 2. **Plots must exist** - Import plots first if needed
#' 3. **Column mapping** - Use `map_individual_columns()`
#' 4. **Validation passed** - Use `validate_individual_data()`
#'
#' @section Database Tables:
#' - **data_individuals**: Stores core individual data
#' - **data_ind_measures_feat**: Stores trait measurements
#'
#' @examples
#' \dontrun{
#' # Complete workflow
#' # 1. Map columns
#' mapped <- map_individual_columns(individuals, features)
#'
#' # 2. Validate
#' validation <- validate_individual_data(
#'   individuals_data = mapped$individuals,
#'   features_data = mapped$features,
#'   method = "1ha-IRD"
#' )
#'
#' if (!validation$valid) {
#'   stop("Fix validation errors first!")
#' }
#'
#' # 3. Dry run
#' preview <- import_individual_data(
#'   individuals_data = validation$cleaned_data$individuals,
#'   features_data = validation$cleaned_data$features,
#'   validation = validation,
#'   dry_run = TRUE
#' )
#'
#' # 4. Actual import
#' result <- import_individual_data(
#'   individuals_data = validation$cleaned_data$individuals,
#'   features_data = validation$cleaned_data$features,
#'   validation = validation,
#'   dry_run = FALSE
#' )
#' }
#'
#' @export
import_individual_data <- function(individuals_data,
                                   features_data = NULL,
                                   validation = NULL,
                                   method = NULL,
                                   con = NULL,
                                   dry_run = FALSE,
                                   progress = TRUE,
                                   ask_confirmation = TRUE) {

  # Check validation if provided
  if (!is.null(validation) && !validation$valid) {
    stop("Data validation failed. Fix errors before importing. Use print_individual_validation_results(validation) to see issues.")
  }

  # Initialize connection if needed, or checkout from pool
  close_on_exit <- FALSE
  is_pool <- FALSE
  original_pool <- NULL

  if (is.null(con)) {
    con <- call.mydb()
    close_on_exit <- TRUE
  } else if (inherits(con, "Pool")) {
    # Checkout dedicated connection from pool for transaction support
    original_pool <- con
    con <- pool::poolCheckout(original_pool)
    is_pool <- TRUE
    if (progress) cli::cli_alert_info("Using connection pool (checked out dedicated connection)")
  }

  # Get current username
  username <- tryCatch({
    DBI::dbGetQuery(con, "SELECT current_user;")[[1]]
  }, error = function(e) {
    "unknown_user"
  })

  if (progress) {
    if (dry_run) {
      cli::cli_h1("Dry Run: Preview Import (No Changes Will Be Made)")
    } else {
      cli::cli_h1("Importing Individual Data")
    }
    cli::cli_alert_info("Individuals to import: {nrow(individuals_data)}")
    if (!is.null(features_data)) {
      cli::cli_alert_info("Feature records to import: {nrow(features_data)}")
    }
    cli::cli_alert_info("Importing as user: {username}")
  }

  # Confirmation before import
  if (!dry_run && ask_confirmation) {
    cat("\n")
    response <- readline("Proceed with import? (yes/no): ")
    if (tolower(trimws(response)) != "yes") {
      if (progress) {
        cli::cli_alert_warning("Import cancelled by user")
      }
      return(list(
        success = FALSE,
        message = "Import cancelled by user",
        dry_run = FALSE
      ))
    }
  }

  # Try import with transaction support
  result <- tryCatch({

    # Begin transaction (unless dry run)
    if (!dry_run) {
      DBI::dbBegin(con)
      if (progress) cli::cli_alert_info("Transaction started")
    }

    # Step 1: Get plot IDs
    if (progress) cli::cli_h2("Step 1: Linking individuals to plots")
    individuals_with_plot_id <- .link_individuals_to_plots(
      individuals_data,
      con,
      progress = progress
    )

    # Step 2: Prepare individuals data for insert
    if (progress) cli::cli_h2("Step 2: Preparing individual data")
    individuals_prepared <- .prepare_individuals_data(
      individuals_with_plot_id,
      progress = progress
    )

    # Step 3: Insert or preview individuals
    if (dry_run) {
      if (progress) {
        cli::cli_h2("Step 3: Preview - Would Insert Into data_individuals")
        cat("\nData preview (first 3 rows):\n")
        print(utils::head(individuals_prepared, 3))
        cli::cli_alert_info("Columns: {paste(names(individuals_prepared), collapse = ', ')}")
      }
      individuals_id_data <- NULL
    } else {
      if (progress) cli::cli_h2("Step 3: Inserting into data_individuals")

      # Use INSERT ... RETURNING to get IDs
      cols <- names(individuals_prepared)
      col_names <- paste(cols, collapse = ", ")

      # Build INSERT with RETURNING clause
      # Note: id_n is the primary key column name in data_individuals
      insert_sql <- sprintf(
        "INSERT INTO data_individuals (%s) VALUES %s RETURNING id_n AS id_individuals, tag",
        col_names,
        paste(
          apply(individuals_prepared, 1, function(row) {
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
      individuals_id_data <- DBI::dbGetQuery(con, insert_sql)

      # Add plot_name and row index back for linking with features.
      # Row order from INSERT RETURNING matches input order, so .row_idx
      # provides a reliable 1:1 link even when tags are duplicated within a plot.
      individuals_id_data$plot_name <- individuals_with_plot_id$plot_name
      individuals_id_data$.row_idx <- seq_len(nrow(individuals_id_data))

      if (progress) {
        cli::cli_alert_success("{nrow(individuals_prepared)} individuals inserted")
      }
    }

    # Step 4: Insert or preview features (if provided)
    if (!is.null(features_data) && nrow(features_data) > 0) {
      if (progress) cli::cli_h2("Step 4: {ifelse(dry_run, 'Preview', 'Inserting')} individual features")

      if (dry_run) {
        n_feature_records_dry <- .count_feature_records(features_data)
        if (progress) {
          cli::cli_alert_info("Would insert {n_feature_records_dry} stem attribute records (from {nrow(features_data)} individuals x trait columns)")
          cat("\nFeatures preview (first 3 rows):\n")
          print(utils::head(features_data, 3))
        }
      } else {
        # Prepare features data
        features_prepared <- .prepare_features_data(
          features_data,
          individuals_id_data,
          con,
          progress = progress
        )

        if (!is.null(features_prepared) && nrow(features_prepared) > 0) {
          # Insert features
          DBI::dbWriteTable(
            con,
            "data_traits_measures",
            features_prepared,
            append = TRUE,
            row.names = FALSE
          )

          if (progress) {
            cli::cli_alert_success("{nrow(features_prepared)} feature records inserted")
          }
        }
      }
    }

    # Commit transaction
    if (!dry_run) {
      DBI::dbCommit(con)
      if (progress) cli::cli_alert_success("Transaction committed successfully")
    }

    # Get unique plot names
    plot_names <- unique(individuals_data$plot_name)

    # Compute true feature count (long format: one record per individual x trait)
    n_feat <- if (!is.null(features_data)) {
      if (dry_run) {
        .count_feature_records(features_data)
      } else {
        if (!is.null(features_prepared)) nrow(features_prepared) else .count_feature_records(features_data)
      }
    } else {
      0L
    }

    # Success!
    result <- list(
      success = TRUE,
      n_individuals = nrow(individuals_data),
      n_features = n_feat,
      plot_names = plot_names,
      username = username,
      dry_run = dry_run,
      message = if (dry_run) {
        sprintf("Dry run completed. Would import %d individuals and %d stem attributes.",
                nrow(individuals_data), n_feat)
      } else {
        sprintf("Successfully imported %d individuals and %d stem attributes.",
                nrow(individuals_data), n_feat)
      }
    )

    if (progress) {
      cat("\n")
      cli::cli_rule()
      if (dry_run) {
        cli::cli_alert_success("Dry run completed - no changes made")
        cli::cli_alert_info("Run with dry_run = FALSE to actually import")
      } else {
        cli::cli_alert_success("Import completed successfully!")
        cat("\n")
        cli::cli_alert_info("Imported {nrow(individuals_data)} individuals")
        if (!is.null(features_data)) {
          cli::cli_alert_info("Imported {nrow(features_data)} feature records")
        }
        cli::cli_alert_info("Affected plots: {paste(plot_names, collapse = ', ')}")
        cat("\n")
      }
      cli::cli_rule()
      cat("\n")
    }

    result

  }, error = function(e) {

    # Rollback on error
    if (!dry_run) {
      tryCatch({
        DBI::dbRollback(con)
        if (progress) cli::cli_alert_danger("Transaction rolled back due to error")
      }, error = function(rollback_error) {
        if (progress) cli::cli_alert_warning("Could not rollback transaction")
      })
    }

    # Return error result
    if (progress) {
      cli::cli_alert_danger("Import failed: {e$message}")
    }

    list(
      success = FALSE,
      error = e$message,
      dry_run = dry_run,
      message = sprintf("Import failed: %s", e$message)
    )
  })

  # Cleanup
  if (close_on_exit) {
    DBI::dbDisconnect(con)
  } else if (is_pool && !is.null(original_pool)) {
    # Return connection to pool
    pool::poolReturn(con)
    if (progress) cli::cli_alert_info("Connection returned to pool")
  }

  invisible(result)
}


#' Link Individuals to Plots (Internal)
#'
#' Gets plot IDs (id_liste_plots) for each individual.
#'
#' @param individuals_data Data frame with plot_name column
#' @param con Database connection
#' @param progress Show progress
#' @return Data frame with id_liste_plots added
#' @keywords internal
.link_individuals_to_plots <- function(individuals_data, con, progress = TRUE) {

  # Get unique plot names
  unique_plots <- unique(individuals_data$plot_name)

  # Query plot IDs with correct column name for data_individuals FK
  plot_ids <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT id_liste_plots AS id_table_liste_plots_n, plot_name FROM data_liste_plots WHERE plot_name IN (%s)",
      paste(sprintf("'%s'", unique_plots), collapse = ", ")
    )
  )

  if (nrow(plot_ids) == 0) {
    stop("No plots found in database. Import plots first.")
  }

  if (nrow(plot_ids) < length(unique_plots)) {
    missing_plots <- setdiff(unique_plots, plot_ids$plot_name)
    stop(sprintf("Plots not found in database: %s", paste(missing_plots, collapse = ", ")))
  }

  # Join with individuals
  individuals_with_id <- individuals_data %>%
    dplyr::left_join(plot_ids, by = "plot_name")

  if (progress) {
    cli::cli_alert_success("Linked {nrow(individuals_data)} individuals to {length(unique_plots)} plots")
  }

  return(individuals_with_id)
}


#' Prepare Individuals Data for Insert (Internal)
#'
#' Prepares data frame for insertion into data_individuals table.
#' Selects and orders columns appropriately.
#'
#' @param individuals_data Data frame with individual data and id_liste_plots
#' @param progress Show progress
#' @return Data frame ready for insert
#' @keywords internal
.prepare_individuals_data <- function(individuals_data, progress = TRUE) {

  # Expected columns for data_individuals table
  # Core columns based on existing database structure
  expected_cols <- c(
    "id_table_liste_plots_n",  # Foreign key to plots (correct column name!)
    "tag",             # Individual tag
    "idtax_n",         # Taxonomy ID
    "original_tax_name",  # Original taxonomic name
    "herbarium_nbe_type",  # Herbarium type (optional)
    "herbarium_nbe_char",  # Herbarium code (optional)
    "multi_tiges_id"    # Multi-stem ID (optional)
  )

  # Select only columns that exist in data
  available_cols <- intersect(expected_cols, names(individuals_data))

  # Select columns
  prepared_data <- individuals_data %>%
    dplyr::select(dplyr::all_of(available_cols))

  if (progress) {
    cli::cli_alert_success("Prepared {nrow(prepared_data)} individual records")
    cli::cli_alert_info("Columns: {paste(names(prepared_data), collapse = ', ')}")
  }

  return(as.data.frame(prepared_data))
}


#' Count Expected Feature Records (Internal)
#'
#' Counts the number of non-NA trait values in wide-format features_data,
#' matching the skip-if-NA logic in .prepare_features_data(). Used for dry-run
#' reporting without executing the full prepare step.
#'
#' @param features_data Wide-format data frame (one row per individual, one
#'   column per trait plus linking columns).
#' @return Integer: number of stem attribute records that would be inserted.
#' @keywords internal
.count_feature_records <- function(features_data) {
  if (is.null(features_data) || nrow(features_data) == 0L) return(0L)
  linking_cols <- c("plot_name", "tag", "sous_plot_name", ".row_idx",
                    "census_date", "census_id", "id_individuals")
  trait_cols <- setdiff(names(features_data), linking_cols)
  if (length(trait_cols) == 0L) return(0L)
  sum(!is.na(features_data[, trait_cols, drop = FALSE]))
}


#' Prepare Features Data for Insert (Internal)
#'
#' Prepares trait/feature data for insertion into data_ind_measures_feat table.
#' Links features to individuals via id_individuals.
#'
#' @param features_data Data frame with feature data
#' @param individuals_id_data Data frame with id_individuals, plot_name, tag
#' @param con Database connection
#' @param progress Show progress
#' @return Data frame ready for insert into data_ind_measures_feat
#' @keywords internal
.prepare_features_data <- function(features_data, individuals_id_data, con, progress = TRUE) {

  # Link features to individuals (id_individuals from INSERT RETURNING)
  # Prefer .row_idx for a reliable 1:1 link (avoids cartesian products when

  # tags are duplicated within a plot, e.g. same tag in different subplots).
  if (".row_idx" %in% names(features_data) && ".row_idx" %in% names(individuals_id_data)) {
    join_keys <- ".row_idx"
    if (progress) {
      cli::cli_alert_info("Using row index (.row_idx) for reliable 1:1 individual-feature linking")
    }
  } else {
    join_keys <- c("plot_name", "tag")
    if (progress) {
      cli::cli_alert_info("Falling back to plot_name + tag for individual-feature linking")
    }
  }
  features_with_id <- features_data %>%
    dplyr::left_join(individuals_id_data, by = join_keys)

  # Get trait definitions for trait IDs
  all_traits <- traits_list()

  # Identify trait columns (exclude linking columns)
  linking_cols <- c("plot_name", "tag", "sous_plot_name", ".row_idx", "census_date", "census_id", "id_individuals")
  trait_cols <- setdiff(names(features_with_id), linking_cols)

  if (length(trait_cols) == 0) {
    if (progress) {
      cli::cli_alert_warning("No trait columns found in features data")
    }
    return(NULL)
  }

  # Build feature records (one row per trait value)
  # Convert from wide format (one column per trait) to long format (one row per measurement)
  feature_records <- list()

  for (i in 1:nrow(features_with_id)) {
    row <- features_with_id[i, ]
    id_individuals <- row$id_individuals
    census_date <- if ("census_date" %in% names(row)) row$census_date else NA

    for (trait_col in trait_cols) {
      trait_value <- row[[trait_col]]

      # Skip if NA
      if (is.na(trait_value)) {
        next
      }

      # Strip disambiguation suffix added for duplicate feature mappings
      # e.g. "plant_height__2" -> "plant_height" (see mod_step5_validation.R Step 4)
      base_trait_col <- sub("__[0-9]+$", "", trait_col)

      # Get trait info
      trait_info <- all_traits %>%
        dplyr::filter(trait == base_trait_col)

      if (nrow(trait_info) == 0) {
        if (progress) {
          cli::cli_alert_warning("Trait '{trait_col}' not found in traits_list() - skipping")
        }
        next
      }

      trait_info <- trait_info[1, ]  # Take first if multiple
      id_trait <- trait_info$id_trait
      value_type <- trait_info$valuetype

      # Build record for data_traits_measures table
      # Based on add_traits_measures() structure (lines 2736-2790 in add_functions.R)
      feature_record <- data.frame(
        id_data_individuals = id_individuals,  # FK to data_individuals.id_n
        traitid = id_trait,                    # FK to traitlist.id_trait
        stringsAsFactors = FALSE
      )

      # Add value to appropriate column based on type
      if (value_type %in% c("numeric", "integer")) {
        feature_record$traitvalue <- as.numeric(trait_value)
        feature_record$traitvalue_char <- NA_character_
      } else {
        feature_record$traitvalue <- NA_real_
        feature_record$traitvalue_char <- as.character(trait_value)
      }

      # Add optional columns (with NA if not available)
      # These are expected by data_traits_measures table
      feature_record$year <- NA_integer_
      feature_record$month <- NA_integer_
      feature_record$day <- NA_integer_

      feature_records[[length(feature_records) + 1]] <- feature_record
    }
  }

  if (length(feature_records) == 0) {
    if (progress) {
      cli::cli_alert_warning("No valid feature values to import")
    }
    return(NULL)
  }

  # Combine all records
  features_prepared <- dplyr::bind_rows(feature_records)

  if (progress) {
    cli::cli_alert_success("Prepared {nrow(features_prepared)} feature records from {length(trait_cols)} traits")
  }

  return(as.data.frame(features_prepared))
}
