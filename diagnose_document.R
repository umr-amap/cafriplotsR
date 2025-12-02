# Diagnostic script for devtools::document() hanging issue
# Run this in a fresh R session

# Step 1: Clear environment
rm(list = ls())
gc()

# Step 2: Check for loaded packages
cat("Currently loaded packages:\n")
print(sessionInfo()$otherPkgs)

# Step 3: Try to identify problematic files
cat("\n\nScanning R files for potential roxygen issues...\n")
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

for (f in r_files) {
  cat("Checking:", basename(f), "...")
  tryCatch({
    # Try to parse the file
    parse(f)
    cat(" OK\n")
  }, error = function(e) {
    cat(" ERROR:", e$message, "\n")
  })
}

# Step 4: Try document with verbose output
cat("\n\nAttempting devtools::document() with timeout...\n")
library(devtools)

# Set a timeout
setTimeLimit(cpu = 30, elapsed = 30, transient = TRUE)

tryCatch({
  document(roclets = c('rd', 'collate', 'namespace'))
}, error = function(e) {
  cat("Error during documentation:\n")
  print(e)
}, finally = {
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
})

cat("\n\nDiagnostic complete.\n")
