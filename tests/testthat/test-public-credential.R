# The resolver decides whether the "Connect as public user" button can be
# offered. Every path that is not an intact, enabled descriptor must come back
# unavailable, and no path may ever produce a credential the package carried
# itself.

test_that("environment variables win over the descriptor", {
  withr::with_envvar(
    c(CAFRI_PUBLIC_USER = "env_user", CAFRI_PUBLIC_PASS = "env_pass"),
    {
      # An unreachable URL proves the descriptor was not consulted at all.
      result <- CafriplotsR:::.public_credential(
        url = "http://127.0.0.1:1/never", timeout = 1, force = TRUE
      )
      expect_true(result$available)
      expect_identical(result$user, "env_user")
      expect_identical(result$password, "env_pass")
    }
  )
})

test_that("an unreachable descriptor means unavailable, not an error", {
  withr::with_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""), {
    result <- suppressMessages(CafriplotsR:::.public_credential(
      url = "http://127.0.0.1:1/never", timeout = 1, force = TRUE
    ))
    expect_false(result$available)
    expect_identical(result$user, "")
    expect_identical(result$password, "")
  })
})

test_that("a well-formed enabled descriptor yields the credential", {
  descriptor <- list(enabled = TRUE, user = "u", password = "p", message = "")
  result <- CafriplotsR:::.public_credential_from(descriptor)
  expect_true(result$available)
  expect_identical(result$user, "u")
  expect_identical(result$password, "p")
})

test_that("enabled = false is the kill switch and keeps its message", {
  descriptor <- list(enabled = FALSE, user = "u", password = "p",
                     message = "Back on Monday.")
  result <- CafriplotsR:::.public_credential_from(descriptor)
  expect_false(result$available)
  expect_identical(result$message, "Back on Monday.")
})

test_that("a descriptor missing either half of the credential is unavailable", {
  expect_false(CafriplotsR:::.public_credential_from(
    list(enabled = TRUE, user = "u", password = ""))$available)
  expect_false(CafriplotsR:::.public_credential_from(
    list(enabled = TRUE, user = "", password = "p"))$available)
  expect_false(CafriplotsR:::.public_credential_from(
    list(enabled = TRUE, password = "p"))$available)
  expect_false(CafriplotsR:::.public_credential_from(
    list(user = "u", password = "p"))$available)
})

test_that("NA fields do not leak into the credential", {
  result <- CafriplotsR:::.public_credential_from(
    list(enabled = TRUE, user = NA, password = "p", message = NA)
  )
  expect_false(result$available)
  expect_identical(result$message, "")
})

test_that("a NULL descriptor is unavailable with no message", {
  result <- CafriplotsR:::.public_credential_from(NULL)
  expect_false(result$available)
  expect_identical(result$message, "")
})

test_that("no public credential is embedded in the package source", {
  # The point of the whole change. Runs against the source tree when there is
  # one (devtools::test()), so a future edit cannot quietly restore a literal.
  source_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(source_dir), "Source tree not available")

  content <- unlist(lapply(
    list.files(source_dir, pattern = "[.]R$", full.names = TRUE),
    readLines, warn = FALSE
  ))
  offenders <- grep("CafriPublic|CafriP_public", content, value = TRUE)

  # A comment or a doc reference naming the account is fine; an assignment
  # holding the value is not.
  offenders <- grep("^\\s*#", offenders, value = TRUE, invert = TRUE)
  expect_identical(offenders, character(0))
})


# --- The login module -------------------------------------------------------

# Never the live descriptor: a test must not depend on what is published, and
# R CMD check must not reach the network.
local_offline_descriptor <- function(env = parent.frame()) {
  withr::local_options(
    list(CafriplotsR.public_access_url = "http://127.0.0.1:1/never"),
    .local_envir = env
  )
  CafriplotsR:::.public_credential_forget()
  withr::defer(CafriplotsR:::.public_credential_forget(), envir = env)
}

test_that("the login server does not consult the network unless asked", {
  # allow_public defaults to FALSE, and an app that never offers public login
  # must not pay for a lookup it will not use.
  local_offline_descriptor()
  withr::local_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""))

  expect_no_error(
    shiny::testServer(mod_database_login_server, {
      expect_false(session$getReturned()$is_public())
    })
  )
})

test_that("the public button renders when a credential resolved", {
  local_offline_descriptor()
  withr::local_envvar(
    c(CAFRI_PUBLIC_USER = "env_user", CAFRI_PUBLIC_PASS = "env_pass")
  )

  shiny::testServer(mod_database_login_server, args = list(allow_public = TRUE), {
    session$setInputs(language = "en")
    expect_match(as.character(output$public_connect_button$html), "connect_public")
    # The read-only warning belongs with a button that exists
    expect_match(as.character(output$public_access_notice$html), "alert-warning")
  })
})

test_that("no button and no dangling separator when nothing resolved", {
  local_offline_descriptor()
  withr::local_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""))

  suppressMessages(
    shiny::testServer(mod_database_login_server, args = list(allow_public = TRUE), {
      session$setInputs(language = "en")
      # The separator and the "or" label travel with the button, so an app
      # that asked for public login is not left with a rule across an empty
      # space where a button used to be.
      expect_null(output$public_connect_button)
      # Nothing to say: an unreachable descriptor carries no message, and a
      # network failure is not the user's problem to read about.
      expect_null(output$public_access_notice)
    })
  )
})
