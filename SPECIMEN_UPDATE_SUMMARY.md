# Specimen Identification Update - Implementation Summary

## What Was Done

Extended the `update_records()` function to support specimen identification updates using the modern, unified update architecture.

## Changes Made

### 1. Added Metadata Mapping Configuration (`R/updates_tables_functions.R:4162`)

Created `get_metadata_mappings_specimens()` function that:
- Maps `colnam` (collector name) → `id_colnam` (collector ID) via `table_colnam` lookup
- Documents that `idtax_n` should be pre-resolved using taxonomic matching functions
- Returns proper metadata configuration for the update system

### 2. Extended Column Routing (`R/updates_tables_functions.R:4021`)

Updated `get_table_columns()` to define editable columns for specimens:
- **Identification fields**: `idtax_n`, `original_tax_name`, `detby`, `dety/detm/detd`, `detvalue`
- **Collection fields**: `id_colnam`, `colnbr`, `suffix`, `coly/colm/cold`
- **Location fields**: `country`, `locality`, `ddlat`, `ddlon`
- **Other fields**: `add_col`

### 3. Added Specimens Configuration (`R/updates_tables_functions.R:3988`)

Added specimens to `get_column_routing()`:
```r
specimens = list(
  table = "specimens",
  id_column = "id_specimen",
  backup_table = "followup_updates_specimens",
  direct_columns = get_table_columns("specimens", con),
  feature_columns = c(),
  metadata_mappings = get_metadata_mappings_specimens(con)
)
```

### 4. Updated Function Signature (`R/updates_tables_functions.R:4682`)

Added "specimens" to `update_records()` table_type parameter with improved documentation.

### 5. Created Example Workflow (`workflow_update_ident_specimens_NEW.R`)

Comprehensive example demonstrating:
- Loading identification data from Excel
- Resolving specimen IDs (if needed)
- Resolving taxonomy IDs (if needed)
- Dry run to preview changes
- Executing batch updates
- Verifying results

## How to Use

### Basic Usage

```r
library(CafriplotsR)

# Prepare your data with id_specimen + fields to update
update_data <- tibble(
  id_specimen = c(1234, 5678),
  idtax_n = c(98765, 43210),
  detby = c("Smith, J.", "Doe, A."),
  dety = c(2025, 2025),
  detm = c(1, 1),
  detd = c(15, 20)
)

# Dry run - preview changes
update_records(
  data = update_data,
  table_type = "specimens",
  execute = FALSE
)

# Execute - apply changes
update_records(
  data = update_data,
  table_type = "specimens",
  execute = TRUE,
  method = "batch"
)
```

### With Collector Name Matching

```r
# If you have collector names instead of IDs
update_data <- tibble(
  id_specimen = c(1234, 5678),
  colnam = c("Dauby, G.", "Leblanc, H."),  # Will be auto-matched to id_colnam
  idtax_n = c(98765, 43210)
)

update_records(
  data = update_data,
  table_type = "specimens",
  execute = TRUE,
  interactive = TRUE,  # Enables fuzzy matching with user confirmation
  similarity_threshold = 0.7
)
```

### Complete Workflow from Excel

See `workflow_update_ident_specimens_NEW.R` for a complete example including:
1. Loading data from Excel
2. Querying specimens by collector + number (if no id_specimen)
3. Resolving taxonomy from genus/species names (if no idtax_n)
4. Preparing update data
5. Dry run preview
6. User confirmation
7. Batch execution
8. Verification

## Benefits Over Old Workflow

| Feature | Old Workflow | New Workflow |
|---------|--------------|--------------|
| **Efficiency** | Loop + individual updates | Batch updates |
| **Preview** | Manual verification each record | Single dry run preview |
| **Collector matching** | Manual `.link_colnam` call | Automatic with `colnam` column |
| **Change detection** | Updates all fields | Only updates changed fields |
| **Backups** | Manual control | Automatic to `followup_updates_specimens` |
| **Consistency** | Custom logic | Same pattern as plots/individuals |
| **Error handling** | Manual checks | Built-in validation |

## Column Mapping Details

### Direct Update Columns
These columns are updated directly in the `specimens` table:
- `idtax_n`, `detby`, `dety`, `detm`, `detd`, `detvalue`
- `colnbr`, `suffix`, `coly`, `colm`, `cold`
- `original_tax_name`, `country`, `locality`
- `ddlat`, `ddlon`, `add_col`

### Metadata Mapping Columns
These columns require lookup table matching:
- `colnam` → Automatically mapped to `id_colnam` via `table_colnam`
  - Uses fuzzy matching (similarity_threshold)
  - Interactive prompts for ambiguous matches
  - Can add new collectors (if admin user)

### Pre-Resolution Required
- `idtax_n`: Should be resolved using `query_taxa()` or `match_tax()` before calling `update_records()`
  - If you have genus/species names, use taxonomic matching functions first
  - See workflow example for complete pattern

## Files Modified

1. `R/updates_tables_functions.R`
   - Added `get_metadata_mappings_specimens()` (line 4162)
   - Updated `get_table_columns()` for specimens (line 4021)
   - Added specimens to `get_column_routing()` (line 3988)
   - Updated `update_records()` documentation (line 4670)

2. `man/update_records.Rd`
   - Auto-regenerated with updated documentation

## Suffix Handling

The workflow includes robust handling of collection number suffixes:

### Automatic Parsing
- Input: `"95bis"` → Database: `colnbr = 95`, `suffix = "bis"`
- Input: `"424bis"` → Database: `colnbr = 424`, `suffix = "bis"`
- Input: `"123"` → Database: `colnbr = 123`, `suffix = ""`

### Fuzzy Matching for Suffixes
Handles spelling variations in suffixes using two-tier matching:

1. **Exact match (normalized)**: Case-insensitive, trimmed
2. **Fuzzy match**: Levenshtein distance with 70% similarity threshold

**Examples:**
- `"bis"` ↔ `"Bis"` = 100% match (case variation)
- `"bis"` ↔ `"biss"` = 75% match (typo)
- `"ter"` ↔ `"Ter"` = 100% match (case variation)

This ensures robust matching even when:
- Data entry has case inconsistencies
- Suffixes have minor typos
- Excel auto-formatting changes capitalization

### Implementation Details
```r
# Step 1: Parse suffix from colnbr
colnbr_numeric = as.numeric(str_extract(colnbr, "^[0-9]+"))
suffix_parsed = str_extract(colnbr, "[^0-9]+$")

# Step 2: Normalize for matching
suffix_normalized = trimws(tolower(suffix_parsed))

# Step 3: Try exact match
specimens %>% join(by = c("colnam", "colnbr_numeric", "suffix_normalized"))

# Step 4: If no match, try fuzzy matching (similarity >= 0.7)
stringdist(suffix_new, suffix_db, method = "lv")
```

## Testing

The implementation follows the same patterns as existing table types (individuals, plots) which are already in production use. To test with your data:

1. Use the dry run mode first (`execute = FALSE`)
2. Review the changes preview
3. Execute with a small subset of data
4. Verify results with `query_specimens()`
5. Scale to full dataset

## Migration from Old Workflow

**Old workflow:**
```r
new_ident <- .link_colnam(new_ident, ...)
for (i in 1:nrow(new_ident)) {
  update_ident_specimens(
    id_colnam = new_ident$colnam[i],
    number = new_ident$colnbr[i],
    id_new_taxa = new_ident$ID.dico.name[i],
    # ... other parameters
    ask_before_update = TRUE
  )
}
```

**New workflow:**
```r
update_records(
  data = new_ident %>%
    select(id_specimen, colnam, idtax_n, detby, dety, detm, detd),
  table_type = "specimens",
  execute = FALSE  # Preview first, then set to TRUE
)
```

## Next Steps

1. **Testing**: Test with your actual data using the workflow script
2. **Shiny App**: The backend is now ready for a Shiny app interface
3. **Documentation**: Add vignette/tutorial if needed
4. **Batch Operations**: Consider adding batch taxonomy resolution helpers

## Questions?

The implementation follows the established patterns in the package. For reference, see:
- `get_metadata_mappings_plots()` for similar metadata mapping
- `get_column_routing()` for configuration structure
- Existing usage of `update_records()` for individuals/plots
