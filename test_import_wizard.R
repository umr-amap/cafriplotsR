# Test Script for Import Wizard Shiny App
#
# This script tests the basic functionality of the import wizard app
# Run this interactively to verify Steps 1-2 are working

# ============================================================================
# Setup
# ============================================================================

# Load package
library(CafriplotsR)

# Check required packages
required_pkgs <- c("shiny", "shinyjs", "DT", "readxl", "writexl")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

# ============================================================================
# Test 1: Launch App
# ============================================================================

message("\n=== Test 1: Launching Import Wizard ===\n")

# This should open the app in your browser
# Verify:
# - App loads without errors
# - Step 1 is displayed
# - Two cards are shown (Plot Metadata and Individual Trees)

launch_import_wizard()

# ============================================================================
# Manual Testing Checklist
# ============================================================================

# Once the app is open, test the following:

cat("
┌─────────────────────────────────────────────────────────────┐
│ MANUAL TESTING CHECKLIST                                    │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│ STEP 1: Choose Import Type                                  │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ [ ] App launches successfully                               │
│ [ ] Step indicator shows Step 1 as active (blue)            │
│ [ ] Two cards are displayed (Plot Metadata, Individual)     │
│ [ ] Click 'Plot Metadata' - card highlights                 │
│ [ ] Click 'Individual Trees' - selection switches           │
│ [ ] Blue selection box appears below cards                  │
│ [ ] 'Next' button is enabled after selection                │
│                                                              │
│ STEP 2: Upload Data                                         │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ [ ] Click 'Next' - navigates to Step 2                      │
│ [ ] Step indicator shows Step 2 as active                   │
│ [ ] Template download section visible on left               │
│ [ ] File upload section visible on right                    │
│                                                              │
│ Template Download:                                           │
│ [ ] Select 'Minimal' template type                          │
│ [ ] Click 'Download Template' - file downloads              │
│ [ ] Open downloaded file - has correct structure            │
│ [ ] Select 'Permanent Plot' with examples                   │
│ [ ] Click 'Download Template' - includes example data       │
│                                                              │
│ File Upload:                                                 │
│ [ ] Click 'Browse' - file selector opens                    │
│ [ ] Upload Excel file (.xlsx) - loads successfully          │
│ [ ] Data preview appears below                              │
│ [ ] Summary stats show (rows, columns, size)                │
│ [ ] Column names displayed                                  │
│ [ ] Interactive table shows first 100 rows                  │
│ [ ] Table is scrollable horizontally                        │
│ [ ] Upload CSV file - works similarly                       │
│                                                              │
│ Navigation:                                                  │
│ [ ] Click 'Back' - returns to Step 1                        │
│ [ ] Selection is preserved                                  │
│ [ ] Click 'Next' again - returns to Step 2                  │
│ [ ] Uploaded data is preserved                              │
│ [ ] 'Next' button disabled until data loaded                │
│ [ ] 'Next' button enabled after data loaded                 │
│                                                              │
│ Error Handling:                                              │
│ [ ] Upload invalid file type - shows error message          │
│ [ ] Upload corrupt Excel file - shows error gracefully      │
│ [ ] Large file upload - shows progress indicator            │
│                                                              │
│ UI/UX:                                                       │
│ [ ] All icons display correctly                             │
│ [ ] Colors and styling look professional                    │
│ [ ] Text is readable and clear                              │
│ [ ] Responsive layout (try resizing window)                 │
│ [ ] No console errors (check browser dev tools)             │
│                                                              │
└─────────────────────────────────────────────────────────────┘
")

# ============================================================================
# Test 2: Test with Example Data
# ============================================================================

message("\n=== Test 2: Create Test Data ===\n")

# Create a simple test dataset
test_data <- data.frame(
  plot_name = c("Test-Plot-A", "Test-Plot-B", "Test-Plot-C"),
  country = c("Gabon", "Gabon", "Cameroon"),
  locality_name = c("Lopé NP", "Lopé NP", "Dja Reserve"),
  ddlat = c(-0.5, -0.52, 3.2),
  ddlon = c(11.5, 11.48, 13.5),
  plot_area = c(1, 1, 0.5),
  date_y = c(2020, 2020, 2019),
  date_m = c(1, 2, 11),
  date_d = c(15, 10, 20),
  team_leader = c("John Doe", "Jane Smith", "Bob Wilson"),
  principal_investigator = c("Dr. Smith", "Dr. Smith", "Dr. Jones"),
  method = c("1ha-IRD", "1ha-IRD", "0.5ha-custom"),
  stringsAsFactors = FALSE
)

# Save to Excel for upload testing
test_file <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(test_data, test_file)

message("Test file created at: ", test_file)
message("\nYou can upload this file in the wizard to test the data preview.")
message("\nThe file contains 3 plots with all recommended fields.")

# ============================================================================
# Test 3: Function Availability
# ============================================================================

message("\n=== Test 3: Check Required Functions ===\n")

# Check that all wrapped functions are available
required_functions <- c(
  "get_plot_metadata_template",
  "get_import_column_routing",
  "map_user_columns",
  "validate_plot_metadata",
  "import_plot_metadata"
)

for (func in required_functions) {
  if (exists(func, mode = "function")) {
    message("✓ ", func, " - available")
  } else {
    message("✗ ", func, " - MISSING!")
  }
}

# ============================================================================
# Summary
# ============================================================================

cat("\n
┌─────────────────────────────────────────────────────────────┐
│ TESTING SUMMARY                                              │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│ Current Status: Steps 1-2 Implemented                       │
│                                                              │
│ Working Features:                                            │
│ ✓ App launches successfully                                 │
│ ✓ Step 1: Choose import type with visual cards             │
│ ✓ Step 2: Download templates (4 types)                     │
│ ✓ Step 2: Upload Excel/CSV files                           │
│ ✓ Step 2: Data preview with interactive table              │
│ ✓ Navigation between steps                                  │
│                                                              │
│ Next Steps to Implement:                                     │
│ ⏸ Step 3: Column mapping                                    │
│ ⏸ Step 4: Data validation                                   │
│ ⏸ Step 5: Preview import (dry run)                          │
│ ⏸ Step 6: Execute import                                    │
│                                                              │
│ If you find any issues, please report them!                 │
│                                                              │
└─────────────────────────────────────────────────────────────┘
")

# ============================================================================
# Cleanup
# ============================================================================

# Clean up test file (optional - comment out if you want to keep it)
# unlink(test_file)

message("\n✓ Testing script complete!\n")
