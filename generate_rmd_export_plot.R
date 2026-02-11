
# ===================================================================
# Script: Generate PDF reports for plot quadrats
# ===================================================================
# This script generates individual PDF reports for each quadrat in
# specified plots, then merges them into a single PDF per plot.
#
# Requirements:
#   - table_recensus.Rmd template file must exist
#   - qpdf package for PDF merging
#   - Database connections configured
# ===================================================================

library(CafriplotsR)
library(dplyr)
library(cli)

# ===== CONFIGURATION =====
cli_h1("Configuration")

# Define plots to process
plot_names <- c("mbalmayo010", "mbalmayo011", "mbalmayo012",
                "mbalmayo013", "mbalmayo014")

# Output directory for PDFs
pdf_dir <- "D:/Papiers-Bourses-colloques/sejours_etrangers_bourses_financements_missions/mission_mbalmayo_fevrier_2026/"

# Rmd template path
rmd_template <- "table_recensus.Rmd"

# Quadrat column name (adjust if different)
quadrat_col <- "quadrat"

# ===== VALIDATION =====
cli_h1("Validation")

# Check output directory exists
if (!dir.exists(pdf_dir)) {
  cli_alert_danger("Output directory does not exist: {pdf_dir}")
  stop("Please create the directory first", call. = FALSE)
}
cli_alert_success("Output directory exists")

# Check Rmd template exists
if (!file.exists(rmd_template)) {
  cli_alert_danger("Rmd template not found: {rmd_template}")
  stop("Please ensure table_recensus.Rmd is in working directory", call. = FALSE)
}
cli_alert_success("Rmd template found")

# Check qpdf package
if (!requireNamespace("qpdf", quietly = TRUE)) {
  cli_alert_danger("Package 'qpdf' is required for PDF merging")
  stop("Install with: install.packages('qpdf')", call. = FALSE)
}
cli_alert_success("qpdf package available")

# ===== DATABASE CONNECTIONS =====
cli_h1("Database Connections")

cli_alert_info("Connecting to databases...")
mydb <- call.mydb(use_env_credentials = TRUE)
mydb_taxa <- call.mydb.taxa(use_env_credentials = TRUE)
cli_alert_success("Connected to databases")

# ===== QUERY PLOTS =====
cli_h1("Querying Plot Data")

cli_alert_info("Querying {length(plot_names)} plot(s)...")
datset <- query_plots(
  plot_name = plot_names,
  extract_individuals = TRUE,
  extract_individual_features = TRUE,
  show_multiple_census = TRUE,
  extract_traits = FALSE,
  output_style = "full"
)
cli_alert_success("Data retrieved")

# ===== PROCESS EACH PLOT =====
cli_h1("Generating Reports")

plots_to_process <- unique(datset$extract$plot_name)
cli_alert_info("Found {length(plots_to_process)} plot(s) with data")

# Track results
results <- list(
  generated_files = character(),
  merged_files = character(),
  errors = list()
)

for (j in seq_along(plots_to_process)) {
  plot_n <- plots_to_process[j]
  cli_h2("Processing: {plot_n} ({j}/{length(plots_to_process)})")

  tryCatch({
    # Filter data for this plot
    plot_data <- 
      datset$extract %>% 
      dplyr::filter(plot_name == plot_n)

    # Get quadrats
    all_quad <- unique(plot_data[[quadrat_col]])
    all_quad <- all_quad[!is.na(all_quad)]

    if (length(all_quad) == 0) {
      cli_alert_warning("No quadrats found for '{plot_n}' - skipping")
      next
    }

    cli_alert_info("Found {length(all_quad)} quadrat(s)")

    # Generate PDF for each quadrat
    quadrat_pdfs <- character()

    for (i in seq_along(all_quad)) {
      quadrat_id <- all_quad[i]
      output_file <- paste0(plot_n, "_", quadrat_id, "_report.pdf")

      cli_alert_info("  Generating report {i}/{length(all_quad)}: {quadrat_id}")

      tryCatch({
        rmarkdown::render(
          input = rmd_template,
          output_format = "pdf_document",
          output_file = output_file,
          output_dir = pdf_dir,
          params = list(
            plot = plot_n,
            quadrat = quadrat_id,
            dataset = plot_data,
            con = mydb,
            con_taxa = mydb_taxa
          ),
          quiet = TRUE,
          envir = new.env()
        )

        generated_file <- file.path(pdf_dir, output_file)
        quadrat_pdfs <- c(quadrat_pdfs, generated_file)
        results$generated_files <- c(results$generated_files, generated_file)
        cli_alert_success("    ✔ Generated: {output_file}")

      }, error = function(e) {
        cli_alert_danger("    ✖ Failed to generate {output_file}: {e$message}")
        results$errors[[output_file]] <- e$message
      })
    }

    # Merge PDFs for this plot
    if (length(quadrat_pdfs) > 0) {
      cli_alert_info("Merging {length(quadrat_pdfs)} PDFs...")

      merged_file <- file.path(pdf_dir, paste0(plot_n, "_merged_output.pdf"))

      tryCatch({
        qpdf::pdf_combine(input = quadrat_pdfs, output = merged_file)
        results$merged_files <- c(results$merged_files, merged_file)
        cli_alert_success("  ✔ Merged PDF: {basename(merged_file)}")

        # Optional: Delete individual PDFs to save space
        # Uncomment the next two lines if you want to keep only merged PDFs
        # unlink(quadrat_pdfs)
        # cli_alert_info("  Deleted {length(quadrat_pdfs)} individual PDF(s)")

      }, error = function(e) {
        cli_alert_danger("  ✖ Failed to merge PDFs: {e$message}")
        results$errors[[paste0(plot_n, "_merge")]] <- e$message
      })
    }

  }, error = function(e) {
    cli_alert_danger("Failed to process plot '{plot_n}': {e$message}")
    results$errors[[plot_n]] <- e$message
  })
}

# ===== SUMMARY =====
cli_h1("Summary")

cli_alert_success("Generated {length(results$generated_files)} individual PDF(s)")
cli_alert_success("Created {length(results$merged_files)} merged PDF(s)")

if (length(results$errors) > 0) {
  cli_alert_warning("{length(results$errors)} error(s) occurred")
  cli_h3("Errors:")
  for (name in names(results$errors)) {
    cli_alert_danger("  {name}: {results$errors[[name]]}")
  }
} else {
  cli_alert_success("All reports generated successfully!")
}

cli_alert_info("Output directory: {pdf_dir}")