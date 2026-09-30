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

test_that("the result says which kind of unavailable it is", {
  # The login screen shows different things for the two, so they must not be
  # distinguishable only by the absence of a message.
  expect_identical(
    CafriplotsR:::.public_credential_from(NULL)$reason, "unreachable"
  )
  expect_identical(
    CafriplotsR:::.public_credential_from(
      list(enabled = FALSE, message = "Back on Monday.")
    )$reason,
    "withdrawn"
  )
  # A descriptor that was read but is unusable is still a descriptor that was
  # read: nothing here is a local network fault.
  expect_identical(
    CafriplotsR:::.public_credential_from(
      list(enabled = TRUE, user = "u", password = "")
    )$reason,
    "withdrawn"
  )
  expect_identical(
    CafriplotsR:::.public_credential_from(
      list(enabled = TRUE, user = "u", password = "p")
    )$reason,
    "ok"
  )
})

test_that("a failed fetch is cached briefly, a resolution for the full TTL", {
  # Five minutes is right for a withdrawal and wrong for a failure: it strands
  # someone who has just fixed their proxy and relaunched.
  expect_lt(
    CafriplotsR:::.public_credential_ttl_unreachable,
    CafriplotsR:::.public_credential_ttl
  )

  withr::local_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""))
  CafriplotsR:::.public_credential_forget()
  withr::defer(CafriplotsR:::.public_credential_forget())

  # One location throughout, so what is measured is the TTL and not the cache
  # key: it starts unreadable and becomes readable, as a blocked host does
  # when the block is lifted.
  path <- withr::local_tempfile(fileext = ".json")
  url <- paste0("file://", normalizePath(path, winslash = "/", mustWork = FALSE))

  expect_identical(
    suppressMessages(
      CafriplotsR:::.public_credential(url = url, timeout = 1)
    )$reason,
    "unreachable"
  )

  writeLines(
    '{"enabled": true, "user": "u", "password": "p", "message": ""}', path
  )

  # Still cached, so a relaunching app is not asking on every keystroke.
  expect_false(CafriplotsR:::.public_credential(url = url, timeout = 1)$available)

  # Backdated past the short TTL but far inside the long one: a failure has to
  # be re-asked here, where a withdrawal would still be held.
  # Bound first: `:::` cannot head a replacement chain.
  cache <- CafriplotsR:::.public_credential_cache
  cached <- cache[[url]]
  cached$at <- Sys.time() - (CafriplotsR:::.public_credential_ttl_unreachable + 5)
  cache[[url]] <- cached

  expect_true(CafriplotsR:::.public_credential(url = url, timeout = 1)$available)
})

test_that("a resolution is cached per location, not globally", {
  # `CafriplotsR.public_access_url` is the documented escape hatch for a site
  # whose network cannot reach the published descriptor. A cache that ignored
  # the location would answer the redirected lookup with the failure from the
  # location just abandoned, for five minutes - which is precisely when
  # someone is trying one thing after another.
  withr::local_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""))
  CafriplotsR:::.public_credential_forget()
  withr::defer(CafriplotsR:::.public_credential_forget())

  blocked <- suppressMessages(CafriplotsR:::.public_credential(
    url = "http://127.0.0.1:1/never", timeout = 1
  ))
  expect_identical(blocked$reason, "unreachable")

  path <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '{"enabled": true, "user": "u", "password": "p", "message": ""}', path
  )

  # No force, no forget: only a different location.
  mirrored <- CafriplotsR:::.public_credential(
    url = paste0("file://", normalizePath(path, winslash = "/"))
  )
  expect_true(mirrored$available)
})

test_that("an unreachable location falls through to the next", {
  # The descriptor is served under two hostnames precisely so that a network
  # filtering one of them still resolves. One site's network resets the TLS
  # handshake to *.github.io and leaves raw.githubusercontent.com alone.
  withr::local_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""))
  CafriplotsR:::.public_credential_forget()
  withr::defer(CafriplotsR:::.public_credential_forget())

  path <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '{"enabled": true, "user": "u", "password": "p", "message": ""}', path
  )

  result <- suppressMessages(CafriplotsR:::.public_credential(
    url = c("http://127.0.0.1:1/never",
            paste0("file://", normalizePath(path, winslash = "/"))),
    timeout = 1
  ))
  expect_true(result$available)
  expect_identical(result$user, "u")
})

test_that("a withdrawal at the first location is not overridden by the second", {
  # The kill switch is the only control over the public login on a host where
  # no per-role connection limit can be set. Falling through on
  # `enabled: false` would demote it to a suggestion: whoever could not reach
  # the first location would carry on getting in.
  withr::local_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""))
  CafriplotsR:::.public_credential_forget()
  withr::defer(CafriplotsR:::.public_credential_forget())

  withdrawn <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '{"enabled": false, "message": "Public access is paused."}', withdrawn
  )
  live <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '{"enabled": true, "user": "u", "password": "p", "message": ""}', live
  )

  result <- CafriplotsR:::.public_credential(url = c(
    paste0("file://", normalizePath(withdrawn, winslash = "/")),
    paste0("file://", normalizePath(live, winslash = "/"))
  ))
  expect_false(result$available)
  expect_identical(result$reason, "withdrawn")
  expect_identical(result$message, "Public access is paused.")
})

test_that("the published locations are the same file by two routes", {
  # Two locations, one file, so a rotation or a withdrawal still takes one
  # commit and nothing has to be kept in step. If a location is ever added
  # that is a *separate* file, inst/public-access/README.md needs a procedure
  # for writing both of them, and this test is the reminder.
  urls <- CafriplotsR:::.public_credential_urls
  expect_length(urls, 2)
  expect_true(all(grepl("umr-amap", urls, fixed = TRUE)))
  expect_true(all(endsWith(urls, "public-access.json")))
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

test_that("an unreachable descriptor removes the button but says so", {
  local_offline_descriptor()
  withr::local_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""))

  suppressMessages(
    shiny::testServer(mod_database_login_server, args = list(allow_public = TRUE), {
      session$setInputs(language = "en")
      # The separator and the "or" label travel with the button, so an app
      # that asked for public login is not left with a rule across an empty
      # space where a button used to be.
      expect_null(output$public_connect_button)
      # But the space must not be silent. The button vanishing with no
      # explanation is indistinguishable from the app being broken, and it
      # sent at least one user asking why their access had been taken away.
      notice <- as.character(output$public_access_notice$html)
      expect_match(notice, "could not be checked")
      expect_match(notice, "network, proxy or firewall")
    })
  )
})

test_that("a withdrawal shows the upstream message, not the network one", {
  # The kill switch has its own voice and must keep it.
  path <- withr::local_tempfile(fileext = ".json")
  writeLines('{"enabled": false, "message": "Public access is paused."}', path)
  withr::local_options(list(
    CafriplotsR.public_access_url =
      paste0("file://", normalizePath(path, winslash = "/"))
  ))
  withr::local_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""))
  CafriplotsR:::.public_credential_forget()
  withr::defer(CafriplotsR:::.public_credential_forget())

  shiny::testServer(mod_database_login_server, args = list(allow_public = TRUE), {
    session$setInputs(language = "en")
    expect_null(output$public_connect_button)
    notice <- as.character(output$public_access_notice$html)
    expect_match(notice, "Public access is paused.")
    expect_false(grepl("network, proxy or firewall", notice))
  })
})

test_that("an app that never offers public login shows no notice", {
  local_offline_descriptor()
  withr::local_envvar(c(CAFRI_PUBLIC_USER = "", CAFRI_PUBLIC_PASS = ""))

  shiny::testServer(mod_database_login_server, {
    session$setInputs(language = "en")
    expect_null(output$public_connect_button)
    expect_null(output$public_access_notice)
  })
})

test_that("a descriptor served without an HTTP status line is accepted", {
  # file:// reports status 0. It is how a site behind a firewall points
  # CafriplotsR.public_access_url at a local mirror, so it must not be
  # mistaken for a failed request.
  path <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '{"enabled": true, "user": "u", "password": "p", "message": ""}', path
  )

  result <- CafriplotsR:::.public_credential(
    url = paste0("file://", normalizePath(path, winslash = "/")), force = TRUE
  )
  expect_true(result$available)
  expect_identical(result$user, "u")

  CafriplotsR:::.public_credential_forget()
})
