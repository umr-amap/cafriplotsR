# Plot Metadata Import - Implementation Plan

## Decisions from Discussion

✅ **Metadata schema configuration** - Approved
✅ **Workflow Option A** - Interactive with Shiny app
✅ **Smart synonym mapping** - dbh = stem_diameter, etc.
✅ **Reuse existing `get_column_routing()`** - Extend for plots import
✅ **Validation layer** - Critical component
✅ **Transaction support** - All-or-nothing imports
✅ **Templates** - Standard formats for common plot types
✅ **Warnings + Errors** - Both levels depending on severity
✅ **Mapping validation** - Verify before import
✅ **Migration strategy** - New functions alongside old ones

## Architecture Overview

```
User Data (flat Excel/CSV)
    ↓
Column Mapping Layer (fuzzy + smart synonyms)
    ↓
Validation Layer (errors + warnings)
    ↓
Preview (dry-run mode)
    ↓
Transaction Begin
    ├─> data_liste_plots (flat columns)
    ├─> data_liste_sub_plots (subplot features)
    └─> People links (team_leader, PI, etc.)
    ↓
Transaction Commit (or Rollback on error)
```

## Phase 1: Extend Metadata Schema (Week 1)

### 1.1 Enhance existing `get_column_routing()` for imports

**Location**: `R/updates_tables_functions.R` or new `R/import_metadata_schema.R`

```r
# Extend existing routing config with import-specific info
get_import_column_routing <- function(table_type = "plots", con) {
  base_config <- get_column_routing(table_type, con)

  # Add import-specific metadata
  base_config$import_config <- list(
    # Required columns
    required_columns = c("plot_name", "method", "country"),

    # Optional columns
    optional_columns = c("ddlat", "ddlon", "date_y", "date_m", "date_d",
                         "locality_name", "elevation", "plot_area"),

    # People columns (become subplot features)
    people_columns = c("team_leader", "principal_investigator",
                      "data_manager", "additional_people", "data_provider"),

    # Lookup table mappings (already in base_config metadata_mappings)
    # Extend with method mapping
    lookup_mappings = c(
      base_config$metadata_mappings,
      list(
        method = list(
          id_col = "id_method",
          lookup_table = "methodslist",
          lookup_key = "id_method",
          lookup_value = "method"
        )
      )
    ),

    # Column synonyms for smart mapping
    column_synonyms = list(
      plot_name = c("plot_id", "plotname", "plot_code", "site_id", "site_name"),
      method = c("survey_method", "sampling_method", "protocol"),
      ddlat = c("latitude", "lat", "y", "coord_y"),
      ddlon = c("longitude", "lon", "long", "x", "coord_x"),
      date_y = c("year", "yyyy", "survey_year", "census_year"),
      date_m = c("month", "mm", "survey_month"),
      date_d = c("day", "dd", "survey_day"),
      team_leader = c("team_lead", "leader", "field_leader", "survey_leader"),
      principal_investigator = c("PI", "pi", "investigator", "lead_scientist"),
      data_manager = c("data_contact", "data_curator", "manager"),
      additional_people = c("collaborators", "team_members", "collectors"),
      elevation = c("altitude", "elev", "height_masl", "elevation_m"),
      plot_area = c("area", "surface", "plot_size")
    ),

    # Validation rules
    validation_rules = list(
      date_y = list(
        type = "integer",
        min = 1900,
        max = lubridate::year(Sys.Date()),
        severity = "error",
        message = "Year must be between 1900 and current year"
      ),
      date_m = list(
        type = "integer",
        min = 1,
        max = 12,
        severity = "error",
        message = "Month must be between 1 and 12"
      ),
      date_d = list(
        type = "integer",
        min = 1,
        max = 31,
        severity = "error",
        message = "Day must be between 1 and 31"
      ),
      ddlat = list(
        type = "numeric",
        min = -90,
        max = 90,
        severity = "error",
        message = "Latitude must be between -90 and 90"
      ),
      ddlon = list(
        type = "numeric",
        min = -180,
        max = 180,
        severity = "error",
        message = "Longitude must be between -180 and 180"
      ),
      elevation = list(
        type = "numeric",
        min = -500,
        max = 6000,
        severity = "warning",
        message = "Elevation seems unusual (expected -500 to 6000m)"
      ),
      plot_area = list(
        type = "numeric",
        min = 0,
        max = 100,
        severity = "warning",
        message = "Plot area seems unusual (expected 0 to 100 ha)"
      ),
      plot_name = list(
        type = "character",
        unique = TRUE,
        severity = "error",
        message = "Plot names must be unique and not already in database"
      )
    )
  )

  return(base_config)
}
```

### 1.2 Create template generator

```r
#' Get Plot Metadata Template
#'
#' Generate a template data frame with standard column names and example data
#'
#' @param template_type Character: "permanent_plot", "transect", "soil_plot", "minimal"
#' @param with_examples Logical: include example rows?
#' @param include_optional Logical: include all optional columns?
#'
#' @return A tibble with template structure
#' @export
get_plot_metadata_template <- function(template_type = "permanent_plot",
                                       with_examples = TRUE,
                                       include_optional = TRUE) {

  mydb <- call.mydb()
  config <- get_import_column_routing("plots", mydb)

  # Build base template
  template_cols <- c(
    config$import_config$required_columns,
    if (include_optional) config$import_config$optional_columns else NULL,
    config$import_config$people_columns
  )

  # Create empty tibble with correct column types
  template <- tibble::tibble(
    plot_name = character(),
    method = character(),
    country = character(),
    ddlat = numeric(),
    ddlon = numeric(),
    date_y = integer(),
    date_m = integer(),
    date_d = integer(),
    locality_name = character(),
    elevation = numeric(),
    plot_area = numeric(),
    team_leader = character(),
    principal_investigator = character(),
    data_manager = character(),
    additional_people = character()
  )

  if (with_examples) {
    # Add example row based on template type
    examples <- get_template_examples(template_type)
    template <- dplyr::bind_rows(template, examples)
  }

  return(template)
}

#' Export template to Excel
#' @export
export_plot_template <- function(file_path, template_type = "permanent_plot") {
  template <- get_plot_metadata_template(template_type, with_examples = TRUE)
  writexl::write_xlsx(template, file_path)
  cli::cli_alert_success("Template exported to {.file {file_path}}")
}
```

## Phase 2: Smart Column Mapping (Week 2)

### 2.1 Fuzzy + synonym-based mapping

```r
#' Map User Columns to Database Schema
#'
#' Uses fuzzy matching + synonym dictionary to suggest mappings
#'
#' @param user_data User's data frame
#' @param config Import configuration from get_import_column_routing()
#' @param similarity_threshold Numeric: minimum similarity for fuzzy match (0-1)
#' @param interactive Logical: allow user to review/adjust mappings?
#'
#' @return Named vector: user_col_name = database_col_name
map_user_columns <- function(user_data,
                             config,
                             similarity_threshold = 0.6,
                             interactive = TRUE) {

  user_cols <- colnames(user_data)
  schema_cols <- c(
    config$import_config$required_columns,
    config$import_config$optional_columns,
    config$import_config$people_columns
  )

  mappings <- list()

  for (user_col in user_cols) {

    # 1. Exact match
    if (user_col %in% schema_cols) {
      mappings[[user_col]] <- user_col
      next
    }

    # 2. Synonym match (SMART MAPPING)
    synonym_match <- find_synonym_match(user_col, config$import_config$column_synonyms)
    if (!is.null(synonym_match)) {
      mappings[[user_col]] <- list(
        target = synonym_match,
        method = "synonym",
        confidence = 1.0
      )
      next
    }

    # 3. Fuzzy match
    fuzzy_matches <- fuzzy_match_columns(user_col, schema_cols, similarity_threshold)
    if (nrow(fuzzy_matches) > 0) {
      mappings[[user_col]] <- list(
        target = fuzzy_matches$target[1],
        method = "fuzzy",
        confidence = fuzzy_matches$similarity[1],
        alternatives = fuzzy_matches$target[2:min(3, nrow(fuzzy_matches))]
      )
      next
    }

    # 4. No match - user must map manually
    mappings[[user_col]] <- list(
      target = NA,
      method = "none",
      confidence = 0
    )
  }

  if (interactive) {
    mappings <- review_mappings_interactive(mappings, schema_cols)
  }

  return(simplify_mappings(mappings))
}

#' Find synonym match using smart dictionary
find_synonym_match <- function(user_col, synonym_dict) {
  user_col_clean <- tolower(trimws(user_col))

  for (target_col in names(synonym_dict)) {
    synonyms <- tolower(synonym_dict[[target_col]])
    if (user_col_clean %in% synonyms) {
      return(target_col)
    }
  }

  return(NULL)
}

#' Fuzzy match columns using stringdist
fuzzy_match_columns <- function(user_col, schema_cols, threshold = 0.6) {
  similarities <- stringdist::stringsim(tolower(user_col), tolower(schema_cols))

  matches <- tibble::tibble(
    target = schema_cols,
    similarity = similarities
  ) %>%
    dplyr::filter(similarity >= threshold) %>%
    dplyr::arrange(desc(similarity))

  return(matches)
}
```

### 2.2 Interactive mapping review (Shiny module)

```r
#' Launch Interactive Column Mapping Shiny App
#'
#' @param user_data User's data frame
#' @param auto_mappings Auto-detected mappings from map_user_columns()
#' @param config Import configuration
#'
#' @return Confirmed mapping (named vector)
review_mappings_shiny <- function(user_data, auto_mappings, config) {
  # Shiny app UI with:
  # - Left panel: User columns + data preview
  # - Right panel: Suggested mapping + alternatives dropdown
  # - Color coding: green (high confidence), yellow (medium), red (unmapped)
  # - "Confirm", "Adjust", "Skip" buttons per column
  # - Final "Proceed" button

  # ... Shiny app implementation ...
}
```

## Phase 3: Validation Layer (Week 3)

### 3.1 Comprehensive validation

```r
#' Validate Plot Metadata Before Import
#'
#' @param data User data (with columns already mapped)
#' @param config Import configuration
#' @param con Database connection
#'
#' @return List with errors, warnings, and validation status
validate_plot_metadata <- function(data, config, con) {

  validation_result <- list(
    errors = list(),
    warnings = list(),
    passed = TRUE
  )

  # 1. Check required columns
  missing_required <- setdiff(
    config$import_config$required_columns,
    colnames(data)
  )
  if (length(missing_required) > 0) {
    validation_result$errors$missing_columns <- sprintf(
      "Missing required columns: %s",
      paste(missing_required, collapse = ", ")
    )
    validation_result$passed <- FALSE
  }

  # 2. Validate field values using rules
  for (col in colnames(data)) {
    if (col %in% names(config$import_config$validation_rules)) {
      rule <- config$import_config$validation_rules[[col]]
      violations <- validate_column_values(data[[col]], rule)

      if (length(violations) > 0) {
        if (rule$severity == "error") {
          validation_result$errors[[col]] <- violations
          validation_result$passed <- FALSE
        } else {
          validation_result$warnings[[col]] <- violations
        }
      }
    }
  }

  # 3. Check for duplicate plot names
  if ("plot_name" %in% colnames(data)) {
    duplicates <- data %>%
      dplyr::group_by(plot_name) %>%
      dplyr::filter(n() > 1) %>%
      dplyr::pull(plot_name) %>%
      unique()

    if (length(duplicates) > 0) {
      validation_result$errors$duplicate_plot_names <- sprintf(
        "Duplicate plot names in data: %s",
        paste(duplicates, collapse = ", ")
      )
      validation_result$passed <- FALSE
    }
  }

  # 4. Check if plot names already exist in database
  if ("plot_name" %in% colnames(data)) {
    existing_plots <- try_open_postgres_table("data_liste_plots", con) %>%
      dplyr::filter(plot_name %in% !!data$plot_name) %>%
      dplyr::collect() %>%
      dplyr::pull(plot_name)

    if (length(existing_plots) > 0) {
      validation_result$errors$existing_plots <- sprintf(
        "Plot names already exist in database: %s",
        paste(existing_plots, collapse = ", ")
      )
      validation_result$passed <- FALSE
    }
  }

  # 5. Validate lookup table references (method, country)
  for (lookup_name in names(config$import_config$lookup_mappings)) {
    if (lookup_name %in% colnames(data)) {
      lookup_config <- config$import_config$lookup_mappings[[lookup_name]]
      invalid_refs <- validate_lookup_references(
        data[[lookup_name]],
        lookup_config$lookup_table,
        lookup_config$lookup_value,
        con
      )

      if (length(invalid_refs) > 0) {
        validation_result$errors[[paste0("invalid_", lookup_name)]] <- sprintf(
          "Invalid %s values (not found in database): %s",
          lookup_name,
          paste(invalid_refs, collapse = ", ")
        )
        validation_result$passed <- FALSE
      }
    }
  }

  return(validation_result)
}

#' Validate column values against a rule
validate_column_values <- function(values, rule) {
  violations <- character()

  # Check type
  if (!is.null(rule$type)) {
    type_check <- switch(rule$type,
      "numeric" = !is.numeric(values),
      "integer" = !is.integer(values) && !all(values == as.integer(values), na.rm = TRUE),
      "character" = !is.character(values)
    )
    if (any(type_check, na.rm = TRUE)) {
      violations <- c(violations, paste("Expected type:", rule$type))
    }
  }

  # Check range
  if (!is.null(rule$min)) {
    if (any(values < rule$min, na.rm = TRUE)) {
      violations <- c(violations, paste("Values below minimum:", rule$min))
    }
  }
  if (!is.null(rule$max)) {
    if (any(values > rule$max, na.rm = TRUE)) {
      violations <- c(violations, paste("Values above maximum:", rule$max))
    }
  }

  if (length(violations) > 0) {
    return(paste(c(rule$message, violations), collapse = "; "))
  }

  return(NULL)
}
```

### 3.2 Validation report UI

```r
#' Display validation results with colors
display_validation_results <- function(validation_result) {

  if (validation_result$passed) {
    cli::cli_alert_success("All validation checks passed!")
  } else {
    cli::cli_alert_danger("Validation failed - cannot proceed with import")
  }

  if (length(validation_result$errors) > 0) {
    cli::cli_h2("Errors (must fix):")
    for (err_name in names(validation_result$errors)) {
      cli::cli_alert_danger("{err_name}: {validation_result$errors[[err_name]]}")
    }
  }

  if (length(validation_result$warnings) > 0) {
    cli::cli_h2("Warnings (review recommended):")
    for (warn_name in names(validation_result$warnings)) {
      cli::cli_alert_warning("{warn_name}: {validation_result$warnings[[warn_name]]}")
    }
  }

  return(invisible(validation_result))
}
```

## Phase 4: Import with Transactions (Week 4)

### 4.1 Safe import function

```r
#' Import Plot Metadata Safely (with transactions)
#'
#' @param data Validated and mapped data
#' @param config Import configuration
#' @param dry_run Logical: if TRUE, validate but don't import
#'
#' @return List with import results
import_plot_metadata_safe <- function(data, config, dry_run = FALSE) {

  mydb <- call.mydb()

  if (dry_run) {
    cli::cli_alert_info("DRY RUN MODE - No data will be imported")
    preview_import(data, config)
    return(invisible(list(dry_run = TRUE)))
  }

  # Prepare data for import
  flat_data <- prepare_flat_data(data, config)
  people_data <- prepare_people_data(data, config)

  # Start transaction
  DBI::dbBegin(mydb)
  cli::cli_alert_info("Starting transaction...")

  import_result <- tryCatch({

    # Step 1: Insert into data_liste_plots
    cli::cli_progress_step("Inserting into data_liste_plots...")
    DBI::dbWriteTable(mydb, "data_liste_plots", flat_data, append = TRUE, row.names = FALSE)

    # Get inserted plot IDs
    inserted_plots <- try_open_postgres_table("data_liste_plots", mydb) %>%
      dplyr::filter(plot_name %in% !!flat_data$plot_name) %>%
      dplyr::collect() %>%
      dplyr::select(id_liste_plots, plot_name)

    cli::cli_alert_success("Inserted {nrow(inserted_plots)} plots")

    # Step 2: Insert subplot features (people)
    if (!is.null(people_data) && nrow(people_data) > 0) {
      cli::cli_progress_step("Inserting subplot features...")

      for (people_type in unique(people_data$feature_type)) {
        people_subset <- people_data %>%
          dplyr::filter(feature_type == !!people_type) %>%
          dplyr::left_join(inserted_plots, by = "plot_name")

        add_subplot_features(
          new_data = people_subset,
          id_plot_name = "id_liste_plots",
          subplottype_field = people_type,
          add_data = TRUE,
          ask_before_update = FALSE
        )
      }

      cli::cli_alert_success("Inserted {nrow(people_data)} subplot features")
    }

    # Commit transaction
    DBI::dbCommit(mydb)
    cli::cli_alert_success("Transaction committed successfully!")

    list(
      success = TRUE,
      plots_imported = nrow(inserted_plots),
      plot_ids = inserted_plots$id_liste_plots
    )

  }, error = function(e) {
    # Rollback on error
    DBI::dbRollback(mydb)
    cli::cli_alert_danger("Import failed: {e$message}")
    cli::cli_alert_info("All changes have been rolled back (no data was imported)")

    list(
      success = FALSE,
      error = e$message
    )
  })

  return(import_result)
}
```

## Phase 5: Shiny App (Week 5)

### 5.1 Full workflow app

```r
#' Launch Plot Metadata Import App
#'
#' Interactive Shiny app for importing plot metadata
#'
#' @export
launch_plot_import_app <- function() {

  # UI with 5 steps:
  # 1. Upload data (Excel/CSV)
  # 2. Map columns (auto + manual adjustment)
  # 3. Validate (show errors/warnings)
  # 4. Preview import (dry-run)
  # 5. Confirm and import

  # ... Full Shiny app implementation ...
  # Similar structure to taxonomic matching app
}
```

## Implementation Priority

### Week 1-2: Core Foundation
1. ✅ Extend `get_column_routing()` with import config
2. ✅ Add column synonyms dictionary
3. ✅ Create template generator

### Week 3: Smart Mapping
4. ✅ Implement fuzzy + synonym matching
5. ✅ Build command-line mapping review

### Week 4: Validation
6. ✅ Comprehensive validation function
7. ✅ Validation report display

### Week 5: Transaction Support
8. ✅ Safe import with rollback
9. ✅ Dry-run mode

### Week 6: Shiny App
10. ✅ Interactive workflow app
11. ✅ Testing with real data

## Testing Strategy

1. **Unit tests** for each component
2. **Integration tests** with test database
3. **User acceptance testing** with real researchers
4. **Performance testing** with large datasets (1000+ plots)

## Documentation

- Function documentation (roxygen2)
- Vignette: "Importing Plot Metadata"
- Video tutorial
- Troubleshooting guide
