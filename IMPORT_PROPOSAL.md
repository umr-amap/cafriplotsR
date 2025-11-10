# Data Import Improvement Proposal

## Problem Statement

Users want to import plot metadata as flat data (Excel/CSV), but the database stores it in a normalized structure:
- Some columns in `data_liste_plots` (flat storage)
- Other columns as `data_liste_sub_plots` features (normalized storage)

Users typically don't understand (or care about) this distinction, leading to:
- Complex manual column mapping
- Error-prone workflows
- Steep learning curve

## Proposed Solution Architecture

### 1. Metadata Schema Configuration

Create a configuration system that defines the plot metadata structure:

```r
# Internal configuration: R/plot_metadata_schema.R
get_plot_metadata_schema <- function() {
  list(
    # Columns that go directly into data_liste_plots
    flat_columns = list(
      required = c("plot_name", "method", "country"),
      optional = c("ddlat", "ddlon", "date_y", "date_m", "date_d",
                   "locality_name", "province", "elevation", "plot_area", ...)
    ),

    # Columns that become subplot features
    feature_columns = list(
      people = c("team_leader", "principal_investigator", "data_manager",
                 "additional_people", "data_provider"),
      metadata = c("vegetation_type", "soil_type", "plot_description", ...)
    ),

    # Lookup table mappings
    lookup_mappings = list(
      method = list(table = "methodslist", id_col = "id_method", name_col = "method"),
      country = list(table = "table_countries", id_col = "id_country", name_col = "country"),
      province = list(table = "table_provinces", id_col = "id_province", name_col = "province")
    ),

    # Validation rules
    validation = list(
      date_y = list(type = "integer", min = 1900, max = year(Sys.Date())),
      date_m = list(type = "integer", min = 1, max = 12),
      date_d = list(type = "integer", min = 1, max = 31),
      ddlat = list(type = "numeric", min = -90, max = 90),
      ddlon = list(type = "numeric", min = -180, max = 180)
    )
  )
}
```

### 2. User-Friendly Import Workflow

**Option A: Interactive Import (Guided)**
```r
# User provides flat data, function guides them through the process
import_plot_metadata_interactive(data = my_excel_data)

# Workflow:
# 1. Display data preview
# 2. Ask user to map columns (with fuzzy matching suggestions)
# 3. Validate data
# 4. Show preview of what will be imported
# 5. Confirm and import
```

**Option B: Template-Based Import**
```r
# User downloads a template with standard column names
template <- get_plot_metadata_template()
# Returns: tibble with expected columns and example data

# User fills template and imports
import_plot_metadata(
  data = filled_template,
  validate = TRUE,  # Check data before import
  dry_run = FALSE   # If TRUE, show what would be imported without doing it
)
```

**Option C: Automatic Mapping (Smart)**
```r
# Function attempts to auto-map columns using fuzzy matching and ML
import_plot_metadata_auto(
  data = messy_data,
  confirm_mappings = TRUE  # Let user review auto-mapped columns
)
```

### 3. Column Mapping Helper

Instead of manual vectors, provide a smart mapper:

```r
map_plot_columns <- function(user_data, interactive = TRUE) {
  schema <- get_plot_metadata_schema()

  # Attempt automatic mapping using:
  # - Exact name matches
  # - Fuzzy string matching (team_leader vs team_lead vs teamleader)
  # - Common synonyms (PI vs principal_investigator)
  # - Column content analysis (dates, coordinates, etc.)

  auto_mapping <- attempt_auto_mapping(user_data, schema)

  if (interactive) {
    # Show mapping results
    # Let user confirm/adjust
    confirmed_mapping <- review_column_mapping(auto_mapping)
  }

  return(confirmed_mapping)
}
```

### 4. Validation Layer

Comprehensive validation before import:

```r
validate_plot_metadata <- function(data, mapping, schema) {
  validation_results <- list()

  # Check required fields
  validation_results$missing_required <- check_required_fields(data, mapping, schema)

  # Validate data types and ranges
  validation_results$invalid_values <- validate_field_values(data, mapping, schema)

  # Check for duplicates
  validation_results$duplicates <- check_duplicate_plot_names(data)

  # Validate foreign keys (method, country exist in lookup tables)
  validation_results$invalid_references <- validate_lookup_references(data, mapping, schema)

  # Check for warnings (e.g., missing optional fields)
  validation_results$warnings <- generate_warnings(data, mapping, schema)

  return(validation_results)
}
```

### 5. Transaction Support

Ensure atomic imports (all-or-nothing):

```r
import_plot_metadata_safe <- function(data, mapping) {
  mydb <- call.mydb()

  # Start transaction
  DBI::dbBegin(mydb)

  tryCatch({
    # 1. Insert into data_liste_plots
    inserted_plots <- insert_plots_table(data, mapping)

    # 2. Insert subplot features
    insert_subplot_features(data, mapping, inserted_plots$id_liste_plots)

    # 3. Insert people links
    insert_people_links(data, mapping, inserted_plots$id_liste_plots)

    # Commit if all succeeded
    DBI::dbCommit(mydb)
    cli::cli_alert_success("Successfully imported {nrow(data)} plots")

  }, error = function(e) {
    # Rollback on any error
    DBI::dbRollback(mydb)
    cli::cli_alert_danger("Import failed: {e$message}")
    cli::cli_alert_info("No data was imported (transaction rolled back)")
    stop(e)
  })

  return(inserted_plots)
}
```

### 6. Improved User Interface

```r
#' Import Plot Metadata (User-Friendly)
#'
#' @param data Data frame with plot metadata
#' @param template_name Optional: use predefined template ("standard", "transect", "soil_plot")
#' @param column_mapping Optional: named vector for custom mapping
#' @param interactive If TRUE, guide user through mapping/validation
#' @param dry_run If TRUE, validate and show preview without importing
#' @param auto_map_columns If TRUE, attempt automatic column mapping
#'
#' @examples
#' # Using template
#' template <- get_plot_metadata_template("standard")
#' my_data <- read_excel("my_plots.xlsx")
#' import_plot_metadata(my_data, template_name = "standard")
#'
#' # Interactive mode
#' import_plot_metadata(my_data, interactive = TRUE)
#'
#' # Auto-mapping with dry run
#' import_plot_metadata(my_data, auto_map_columns = TRUE, dry_run = TRUE)
#'
#' # Custom mapping
#' import_plot_metadata(
#'   my_data,
#'   column_mapping = c(
#'     "Plot ID" = "plot_name",
#'     "Latitude" = "ddlat",
#'     "Team Leader" = "team_leader"
#'   )
#' )
import_plot_metadata <- function(data,
                                  template_name = NULL,
                                  column_mapping = NULL,
                                  interactive = FALSE,
                                  dry_run = FALSE,
                                  auto_map_columns = TRUE) {
  # Implementation
}
```

## Implementation Roadmap

### Phase 1: Foundation (Week 1-2)
- [ ] Create `R/plot_metadata_schema.R` with schema configuration
- [ ] Implement `get_plot_metadata_schema()`
- [ ] Create `get_plot_metadata_template()` for different plot types
- [ ] Write unit tests for schema system

### Phase 2: Column Mapping (Week 2-3)
- [ ] Implement `map_plot_columns()` with fuzzy matching
- [ ] Create interactive column mapper UI
- [ ] Build synonym dictionary for common field names
- [ ] Test with real user data

### Phase 3: Validation (Week 3-4)
- [ ] Implement `validate_plot_metadata()` with comprehensive checks
- [ ] Create validation report generator
- [ ] Add helpful error messages with suggestions
- [ ] Build validation testing suite

### Phase 4: Import Logic (Week 4-5)
- [ ] Refactor `add_plots()` to use new system
- [ ] Implement transaction support
- [ ] Create `import_plot_metadata()` wrapper
- [ ] Add dry_run mode for preview

### Phase 5: User Experience (Week 5-6)
- [ ] Build interactive import wizard
- [ ] Create progress indicators
- [ ] Add rollback and retry mechanisms
- [ ] Generate import summary reports

### Phase 6: Documentation (Week 6)
- [ ] Write comprehensive vignette
- [ ] Create video tutorial
- [ ] Update function documentation
- [ ] Add examples for common scenarios

## Benefits

### For Users:
- ✅ **Simpler API**: No manual column mapping vectors
- ✅ **Guided workflow**: Interactive mode helps avoid errors
- ✅ **Templates**: Standard formats for common use cases
- ✅ **Auto-mapping**: Smart column detection
- ✅ **Better errors**: Clear validation messages
- ✅ **Preview mode**: Dry-run before actual import
- ✅ **Safety**: Transactions prevent partial imports

### For Maintainers:
- ✅ **Centralized schema**: Easy to update structure
- ✅ **Consistent validation**: Reusable across functions
- ✅ **Testable**: Clear units for testing
- ✅ **Extensible**: Easy to add new plot types/fields
- ✅ **Documented**: Self-documenting schema

## Backward Compatibility

Keep existing `add_plots()` function with deprecation warning:
```r
add_plots <- function(...) {
  .Deprecated("import_plot_metadata",
              msg = "add_plots() is deprecated. Use import_plot_metadata() for improved workflow.")
  # Keep old implementation for now
}
```

## Questions for Discussion

1. **Template formats**: Should we support multiple plot types (permanent plots, transects, soil plots)?
2. **Validation strictness**: Should we allow warnings vs. errors?
3. **Auto-mapping confidence**: What similarity threshold for auto-mapping?
4. **Interactive UI**: Command-line prompts vs. Shiny app?
5. **Migration strategy**: How to transition existing users?

## Next Steps

1. Review and refine this proposal
2. Create proof-of-concept for schema system
3. Test with real user data
4. Iterate based on feedback
5. Full implementation following roadmap
