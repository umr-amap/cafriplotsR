# The shared login UI offers two alternatives to a personal account: the
# read-only public account, and offline mode against the cached backbone.
# Neither suits every app, so both are opt-in. These tests pin the defaults
# and the opt-in, so an app cannot pick up an entry point it cannot honour.

.login_html <- function(...) {
  paste(as.character(mod_database_login_ui(...)), collapse = "")
}

test_that("the login UI hides public and offline entry points by default", {
  html <- .login_html("login")

  expect_false(grepl("login-public_connect_button", html, fixed = TRUE))
  expect_false(grepl("login-offline_connect_button", html, fixed = TRUE))
  expect_false(grepl("login-offline_access_notice", html, fixed = TRUE))

  # The ordinary account form is still there
  expect_true(grepl("login-credentials_form", html, fixed = TRUE))
})

test_that("allow_public shows only the public entry point", {
  html <- .login_html("login", allow_public = TRUE)

  expect_true(grepl("login-public_connect_button", html, fixed = TRUE))
  expect_false(grepl("login-offline_connect_button", html, fixed = TRUE))
})

test_that("allow_offline shows only the offline entry point", {
  html <- .login_html("login", allow_offline = TRUE)

  expect_true(grepl("login-offline_connect_button", html, fixed = TRUE))
  expect_true(grepl("login-offline_access_notice", html, fixed = TRUE))
  expect_false(grepl("login-public_connect_button", html, fixed = TRUE))
})

test_that("only the taxonomic matching app opts into offline mode", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package sources not available")

  app_files <- list.files(r_dir, pattern = "^shiny_app_.*\\.R$",
                          full.names = TRUE)
  skip_if(length(app_files) == 0, "no app sources found")

  opts_in <- vapply(app_files, function(f) {
    any(grepl("allow_offline\\s*=\\s*TRUE", readLines(f, warn = FALSE)))
  }, logical(1))

  expect_equal(
    basename(app_files[opts_in]),
    "shiny_app_taxonomic_match.R"
  )
})
