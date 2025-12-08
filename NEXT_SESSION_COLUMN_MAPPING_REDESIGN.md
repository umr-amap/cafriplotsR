# Column Mapping Redesign - Next Session Plan

**Status**: Ready to implement
**Branch**: feature/add-individual-import
**Estimated Work**: 3-4 hours with testing

## Current State

### Function Signature (OLD)
```r
map_individual_columns(
  individuals_data,      # Required: individual identification columns
  features_data = NULL,  # Optional: trait measurements
  ...
)
```

### Problem
- Users must manually separate their flat table into TWO sheets
- Most users have ONE table with all columns mixed together
- Not intuitive which columns belong where
- Limited interactive guidance

## Requirements (User Specified)

### 1. Accept Single Flat Table (Default)
```r
my_data <- readxl::read_excel("trees.xlsx")
# Columns: Plot, TreeID, Species, DBH, Height, WoodDensity, Family, etc.

mapped <- map_individual_columns(data = my_data)
```

### 2. Interactive Column Classification
For each column WITHOUT a good automatic match, ask:
```
Column: 'DBH_cm'
Sample values: 15.3, 22.1, 8.5

Is this a FEATURE/TRAIT measurement? (yes/no/skip):
```

### 3. If NO → Individual Column Selection
Use `.find_cat()` to show numbered list:
```
Select which individual column this represents:

1. plot_name           - Plot identifier (required)
2. tag                 - Tree tag/number (optional - auto-generated if missing)
3. idtax_n             - Taxonomy ID (required)
4. original_tax_name   - Original taxonomic name (required)
5. herbarium_nbe_type  - Herbarium type specimen
6. herbarium_nbe_char  - Herbarium specimen number
7. multi_tiges_id      - Multi-stem identifier
8. Skip this column

Your choice:
```

### 4. If YES → Trait Selection
Use `.find_cat()` to show available traits from `traits_list()`:
```
Select which trait/feature this represents:

[Shows 93+ traits with fuzzy search, pagination]
Common traits shown first:
1. stem_diameter - DBH or stem diameter (cm)
2. stem_height - Tree/stem height (m)
3. wood_density - Wood density (g/cm³)
...

Type number, 'G' to search, 'ENTER' for more, or '0' to skip:
```

### 5. Reuse Existing Functions
- `.find_cat()` from `link_table_functions.R` for interactive selection
- Fuzzy matching, pagination, pattern search all built-in

### 6. Backward Compatibility
Old two-table approach must still work:
```r
mapped <- map_individual_columns(
  individuals_data = individuals,
  features_data = features
)
```

## Implementation Plan

### Step 1: Update Function Signature (30 min)
**File**: `R/import_individuals_column_mapping.R`

```r
map_individual_columns <- function(
  data = NULL,                # NEW: Single flat table (recommended)
  individuals_data = NULL,    # OLD: Backward compatible
  features_data = NULL,       # OLD: Backward compatible
  method = NULL,
  similarity_threshold = 0.6,
  interactive = TRUE,
  con = NULL
) {
  # Parameter validation
  if (!is.null(data) && !is.null(individuals_data)) {
    stop("Cannot provide both 'data' and 'individuals_data'")
  }

  # Detect workflow
  if (!is.null(data)) {
    # NEW WORKFLOW
    result <- .map_flat_table_interactive(...)
  } else {
    # OLD WORKFLOW
    result <- .map_two_tables(...)
  }
}
```

### Step 2: Create Helper Functions (2 hours)

#### `.map_flat_table_interactive()` - Main new workflow
```r
.map_flat_table_interactive <- function(data, con, similarity_threshold, interactive) {
  # 1. Get available individual columns and traits
  # 2. Try automatic mapping for all columns
  # 3. For unmapped columns:
  #    - Ask if feature or individual column
  #    - Use appropriate selection function
  # 4. Separate into individuals and features dataframes
  # 5. Apply column mappings
  # 6. Return both
}
```

#### `.classify_column_interactive()` - Ask yes/no/skip
```r
.classify_column_interactive <- function(column_name, sample_values) {
  cat("\n")
  cli::cli_rule(paste("Column:", column_name))
  cli::cli_alert_info("Sample values: {paste(sample_values[1:3], collapse = ', ')}")

  response <- readline(prompt = "Is this a FEATURE/TRAIT measurement? (yes/no/skip): ")
  return(tolower(trimws(response)))
}
```

#### `.select_individual_column()` - Use .find_cat()
```r
.select_individual_column <- function(column_name) {
  # Prepare individual columns list
  individual_cols <- data.frame(
    column_name = c("plot_name", "tag", "idtax_n", "original_tax_name",
                    "herbarium_nbe_type", "herbarium_nbe_char", "multi_tiges_id"),
    description = c("Plot identifier (required)",
                   "Tree tag/number (optional)",
                   "Taxonomy ID (required)",
                   "Original taxonomic name (required)",
                   "Herbarium type specimen",
                   "Herbarium specimen number",
                   "Multi-stem identifier"),
    stringsAsFactors = FALSE
  )

  # Use .find_cat() for selection
  selected_idx <- .find_cat(
    value_to_search = column_name,
    compared_table = individual_cols,
    column_name = "column_name",
    field_label = "Individual Column"
  )

  if (selected_idx > 0) {
    return(individual_cols$column_name[selected_idx])
  } else {
    return(NA)  # User skipped
  }
}
```

#### `.select_trait_column()` - Use .find_cat()
```r
.select_trait_column <- function(column_name, con) {
  # Get all available traits
  all_traits <- traits_list(con = con)

  traits_table <- data.frame(
    trait = all_traits$trait,
    description = all_traits$description,
    stringsAsFactors = FALSE
  )

  # Use .find_cat() for selection with fuzzy matching
  selected_idx <- .find_cat(
    value_to_search = column_name,
    compared_table = traits_table,
    column_name = "trait",
    field_label = "Trait/Feature"
  )

  if (selected_idx > 0) {
    return(traits_table$trait[selected_idx])
  } else {
    return(NA)  # User skipped
  }
}
```

#### `.separate_individuals_features()` - Split data
```r
.separate_individuals_features <- function(data, column_mappings) {
  # Based on mappings, separate into:
  # - individuals_data: only individual columns
  # - features_data: plot_name, tag, census_date, + trait columns

  individual_col_names <- c("plot_name", "tag", "idtax_n", "original_tax_name",
                           "herbarium_nbe_type", "herbarium_nbe_char", "multi_tiges_id")

  is_individual <- column_mappings$mapped_to %in% individual_col_names
  is_feature <- !is_individual & !is.na(column_mappings$mapped_to)

  # Create individuals dataframe
  individuals <- data[, column_mappings$original[is_individual]]
  names(individuals) <- column_mappings$mapped_to[is_individual]

  # Create features dataframe (if any features found)
  if (any(is_feature)) {
    # Include linking columns + feature columns
    linking_cols <- c("plot_name", "tag")
    feature_original_cols <- column_mappings$original[is_feature]

    features <- data[, c(which(column_mappings$mapped_to %in% linking_cols),
                        which(is_feature))]
    # Rename
    # ... apply mappings
  } else {
    features <- NULL
  }

  return(list(individuals = individuals, features = features))
}
```

### Step 3: Refactor Old Workflow (30 min)

Move existing code into `.map_two_tables()`:
```r
.map_two_tables <- function(individuals_data, features_data,
                            con, similarity_threshold, interactive) {
  # This is the existing workflow - just extract current code into this function
  # Lines 260-362 of current file
}
```

### Step 4: Update Documentation (30 min)

Update roxygen docs:
- New `@param data` description
- Update examples to show both workflows
- Add section explaining the difference
- Update return value documentation

### Step 5: Testing (1-2 hours)

#### Test Case 1: Single Flat Table
```r
# Create test data
test_data <- data.frame(
  Plot = c("PLOT-A", "PLOT-A", "PLOT-B"),
  TreeTag = c(1, 2, 1),
  Species = c("Beilschmiedia mannii", "Coula edulis", "Dacryodes edulis"),
  idtax = c(12345, 67890, 11111),
  DBH = c(15.3, 22.1, 8.5),
  Height = c(12.0, 18.5, 6.2)
)

# Test
mapped <- map_individual_columns(data = test_data, interactive = TRUE)

# Verify
stopifnot(!is.null(mapped$individuals))
stopifnot(!is.null(mapped$features))
stopifnot("plot_name" %in% names(mapped$individuals))
stopifnot("stem_diameter" %in% names(mapped$features))
```

#### Test Case 2: Two Tables (Backward Compatibility)
```r
individuals <- data.frame(
  plot_name = c("PLOT-A", "PLOT-A"),
  tag = c(1, 2),
  idtax_n = c(12345, 67890),
  original_tax_name = c("Beilschmiedia mannii", "Coula edulis")
)

features <- data.frame(
  plot_name = c("PLOT-A", "PLOT-A"),
  tag = c(1, 2),
  stem_diameter = c(15.3, 22.1)
)

mapped <- map_individual_columns(individuals_data = individuals,
                                 features_data = features)
# Verify old workflow still works
```

#### Test Case 3: Non-Interactive Mode
```r
# Should use automatic matching only, no prompts
mapped <- map_individual_columns(data = test_data, interactive = FALSE)
```

## Files to Modify

1. **R/import_individuals_column_mapping.R**
   - Lines to add: ~400
   - Main function modification
   - 6 new helper functions

2. **NAMESPACE** (auto-generated)

3. **man/map_individual_columns.Rd** (auto-generated from roxygen)

## Success Criteria

- ✅ Single flat table workflow works
- ✅ Interactive prompts guide user through classification
- ✅ `.find_cat()` provides good selection interface
- ✅ Automatic separation into individuals and features
- ✅ Backward compatibility maintained
- ✅ All tests pass
- ✅ Documentation updated

## Notes for Implementation

- **Priority**: Only prompt for columns without good automatic matches
- **User Experience**: Clear, concise prompts with examples
- **Error Handling**: Validate user input, handle edge cases
- **Performance**: Efficient for large datasets (100+ columns)
- **Maintainability**: Clean, well-documented helper functions

## Ready to Start Next Session

When you begin, load this document and proceed with:
1. Step 1: Update function signature
2. Create helper functions one by one
3. Test each component
4. Integration testing
5. Documentation update

Estimated total time: 3-4 hours with comprehensive testing.
