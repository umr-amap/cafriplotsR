# Shared helpers for Shiny app tests

# Skip when Chrome / chromote is unavailable (needed for Tier 2 browser tests)
skip_if_no_chromote <- function() {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if(
    inherits(try(chromote::default_chromote_object(), silent = TRUE), "try-error"),
    "Chromote / Chrome not available"
  )
}

# Skip when no live DB credentials are present in the environment
skip_if_no_db <- function() {
  testthat::skip_if(
    Sys.getenv("CAFRI_TEST_DB_USER") == "" ||
      Sys.getenv("CAFRI_TEST_DB_PASS") == "",
    "No test DB credentials (set CAFRI_TEST_DB_USER / CAFRI_TEST_DB_PASS)"
  )
}
