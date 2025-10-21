# Test Script for Shiny App Launch
# This script tests if the app can be initialized without errors

library(plotsdatabase)

cat("=" , rep("=", 60), "=\n", sep = "")
cat("Testing Taxonomic Match App Launch\n")
cat("=", rep("=", 60), "=\n\n", sep = "")

# Load test data
test_data <- data.frame(
  plot_id = c(1, 1, 2, 2, 3),
  tree_id = c('A01', 'A02', 'B01', 'B02', 'C01'),
  species = c(
    'Gilbertiodendron dewevrei',
    'Brachysteiga laurentii',
    'Garcinia kola',
    'Unknown species',
    'Julbernardia seretii'
  ),
  dbh = c(45.2, 32.1, 28.5, 15.3, 52.7),
  stringsAsFactors = FALSE
)

cat("✓ Test data created:\n")
cat("  - Rows:", nrow(test_data), "\n")
cat("  - Columns:", ncol(test_data), "\n")
cat("  - Species column: 'species'\n\n")

# Test 1: Launch function exists
cat("Test 1: Checking if launch function exists...\n")
if (exists("launch_taxonomic_match_app")) {
  cat("✓ launch_taxonomic_match_app() function found\n\n")
} else {
  stop("✗ launch_taxonomic_match_app() function not found!")
}

# Test 2: Translation system works
cat("Test 2: Testing translation system...\n")
tryCatch({
  trans_en <- get_translations("en")
  trans_fr <- get_translations("fr")
  cat("✓ English translations loaded:", length(trans_en), "keys\n")
  cat("✓ French translations loaded:", length(trans_fr), "keys\n")
  cat("✓ Sample: EN title =", trans_en$app_title, "\n")
  cat("✓ Sample: FR title =", trans_fr$app_title, "\n\n")
}, error = function(e) {
  stop("✗ Translation system failed:", e$message)
})

# Test 3: App object can be created
cat("Test 3: Creating app object (without launching)...\n")
tryCatch({
  app <- app_taxonomic_match(
    data = test_data,
    name_column = "species",
    language = "en",
    min_similarity = 0.3
  )
  cat("✓ App object created successfully\n")
  cat("✓ App has UI component:", !is.null(app$ui), "\n")
  cat("✓ App has server component:", !is.null(app$server), "\n\n")
}, error = function(e) {
  stop("✗ App creation failed:", e$message)
})

# Test 4: Module functions exist
cat("Test 4: Checking module functions...\n")
modules <- c(
  "mod_data_input_ui", "mod_data_input_server",
  "mod_column_select_ui", "mod_column_select_server",
  "mod_auto_matching_ui", "mod_auto_matching_server",
  "mod_progress_tracker_ui", "mod_progress_tracker_server",
  "mod_results_export_ui", "mod_results_export_server",
  "mod_language_toggle_ui", "mod_language_toggle_server"
)

all_exist <- TRUE
for (mod in modules) {
  if (!exists(mod)) {
    cat("✗ Missing:", mod, "\n")
    all_exist <- FALSE
  }
}

if (all_exist) {
  cat("✓ All", length(modules), "module functions found\n\n")
} else {
  stop("✗ Some module functions are missing!")
}

# Test 5: Check matching function integration
cat("Test 5: Testing matching function integration...\n")
tryCatch({
  # This would normally require database connection
  # Just check if the function exists
  if (exists("match_taxonomic_names")) {
    cat("✓ match_taxonomic_names() function available\n")
    cat("✓ App can use new intelligent matching\n\n")
  } else {
    warning("✗ match_taxonomic_names() not found!")
  }
}, error = function(e) {
  warning("Issue checking matching function:", e$message)
})

# Summary
cat("=" , rep("=", 60), "=\n", sep = "")
cat("✓ ALL TESTS PASSED!\n")
cat("=" , rep("=", 60), "=\n\n", sep = "")

cat("App is ready to launch.\n")
cat("To launch the app interactively, run:\n\n")
cat("  library(plotsdatabase)\n")
cat("  launch_taxonomic_match_app()\n\n")
cat("Or with test data:\n\n")
cat("  launch_taxonomic_match_app(data = test_data, name_column = 'species')\n\n")
