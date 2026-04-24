# Simple test - just focus on dbh mapping with full output

devtools::load_all()

# Create minimal test data
test_data <- data.frame(dbh = c(10, 20))

# Get config
config <- get_import_column_routing("individuals", con = NULL)

# Run mapping
cat("\n=== RUNNING MAPPING TEST ===\n")
result <- map_user_columns(test_data, config)

cat("\n=== RESULTS ===\n")
cat("dbh mapped to:", result$mappings["dbh"], "\n")
