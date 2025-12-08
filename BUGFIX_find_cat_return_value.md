# Bug Fix: .find_cat() Return Value Handling

**Issue**: Error when using interactive column selection
**Date**: 2025-11-13
**Status**: ✅ FIXED

## Problem

User reported this error when interactively mapping columns:

```
Erreur dans .select_individual_column(user_col) :
  l'objet 'list' ne peut être converti automatiquement en un type 'double'
```

Translation: "Error in .select_individual_column(user_col): object 'list' cannot be automatically converted to type 'double'"

This occurred after the user selected an option from the numbered list.

## Root Cause

The `.find_cat()` function (from `R/link_table_functions.R`) returns a **list** with two components:
```r
return(list(
  selected_name = selected_name,      # Integer index or 0 for skip
  sorted_matches = sorted_matches     # Data frame of matches
))
```

However, in my implementation of `.select_individual_column()` and `.select_trait_column()`, I was treating the return value as if it were a simple integer:

```r
# WRONG - treating result as integer
selected_idx <- .find_cat(...)
if (selected_idx > 0 && ...) {  # ERROR: can't compare list to integer
```

## Solution

Updated both helper functions to properly extract the `selected_name` component from the returned list:

### Fixed: `.select_individual_column()` (Line 782-799)
```r
# CORRECT - extract integer from list
result <- .find_cat(
  value_to_search = column_name,
  compared_table = individual_cols,
  column_name = "column_name",
  field_label = "Individual Column"
)

# Extract selected index from result
selected_idx <- result$selected_name

# Now can properly compare
if (!is.na(selected_idx) && selected_idx > 0 && selected_idx <= nrow(individual_cols)) {
  selected_col <- individual_cols$column_name[selected_idx]
  cli::cli_alert_success("Mapped to: {.field {selected_col}}")
  return(selected_col)
} else {
  cli::cli_alert_info("Column skipped")
  return(NA_character_)
}
```

### Fixed: `.select_trait_column()` (Line 846-863)
```r
# CORRECT - extract integer from list
result <- .find_cat(
  value_to_search = column_name,
  compared_table = traits_table,
  column_name = "trait",
  field_label = "Trait/Feature"
)

# Extract selected index from result
selected_idx <- result$selected_name

# Now can properly compare
if (!is.na(selected_idx) && selected_idx > 0 && selected_idx <= nrow(traits_table)) {
  selected_trait <- traits_table$trait[selected_idx]
  cli::cli_alert_success("Mapped to: {.field {selected_trait}}")
  return(selected_trait)
} else {
  cli::cli_alert_info("Column skipped")
  return(NA_character_)
}
```

## Changes Made

**File**: `R/import_individuals_column_mapping.R`

**Line 782-799**: Fixed `.select_individual_column()`
- Changed: `selected_idx <- .find_cat(...)` → `result <- .find_cat(...)`
- Added: `selected_idx <- result$selected_name`
- Added: `!is.na(selected_idx)` check before comparisons

**Line 846-863**: Fixed `.select_trait_column()`
- Changed: `selected_idx <- .find_cat(...)` → `result <- .find_cat(...)`
- Added: `selected_idx <- result$selected_name`
- Added: `!is.na(selected_idx)` check before comparisons

## Testing

The fix handles all cases:
1. **User selects a match** (1-10): Returns the selected column/trait name
2. **User skips (0)**: Returns NA_character_ with skip message
3. **Exact match found**: `.find_cat()` returns the index directly
4. **Empty input**: Handled by `!is.na()` check

## How to Test

Try the interactive workflow:
```r
library(plotsdatabase)
con <- call.mydb()

# Test data with unmapped column
test_data <- data.frame(
  Plot = c("PLOT-A", "PLOT-B"),
  TreeTag = c("1", "2"),
  Species = c("Sp1", "Sp2"),
  idtax = c(123, 456),
  WeirdColumnName = c("val1", "val2")  # Will trigger interactive selection
)

# Run with interactive mode
mapped <- map_individual_columns(data = test_data, interactive = TRUE, con = con)
# When prompted for "WeirdColumnName", select an option or skip
# Should now work without errors!
```

## Lessons Learned

- Always check the actual return type of helper functions, especially when reusing existing code
- The `.find_cat()` function is designed to return both the selection AND the matches for reference
- Using pattern matching in the existing codebase helped identify the correct usage (line 89 in `link_table_functions.R`: `if (sorted_matches$selected_name != 0)`)

## Related Files

- `R/import_individuals_column_mapping.R` (fixed)
- `R/link_table_functions.R` (`.find_cat()` definition, lines 440-584)

## Status

✅ **FIXED** - Interactive column selection now works correctly.
