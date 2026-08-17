# Tests for R/connection_diagnostics.R
# All pure: the classifier and the verdict logic, with probes mocked so no
# test ever touches the network.

# =============================================================================
# .classify_connect_error()
# =============================================================================

test_that(".classify_connect_error recognises the institutional firewall case", {
  # The exact message users report from filtered networks
  msg <- paste0(
    'connection to server at "dg474899-001.dbaas.ovh.net" (141.94.103.170), ',
    "port 35699 failed: timeout expired"
  )
  expect_equal(.classify_connect_error(msg), "timeout")
  expect_true(.is_network_error("timeout"))
})

test_that(".classify_connect_error separates authentication from network failures", {
  expect_equal(
    .classify_connect_error('FATAL:  password authentication failed for user "gdauby"'),
    "auth"
  )
  expect_equal(.classify_connect_error('FATAL:  role "nobody" does not exist'), "auth")
  expect_false(.is_network_error("auth"))
})

test_that(".classify_connect_error covers the other libpq failure modes", {
  cases <- list(
    dns = 'could not translate host name "dg474899-001.dbaas.ovh.net" to address: Name or service not known',
    refused = "could not connect to server: Connection refused",
    unreachable = "could not connect to server: Network is unreachable",
    too_many_clients = "FATAL:  sorry, too many clients already",
    no_database = 'FATAL:  database "plots_transect" does not exist',
    server_closed = "server closed the connection unexpectedly"
  )

  for (code in names(cases)) {
    expect_equal(.classify_connect_error(cases[[code]]), code)
  }
})

test_that(".classify_connect_error reads a dropped SSL handshake as a dropped connection", {
  # "SSL SYSCALL error: EOF detected" is a severed connection, not a
  # certificate problem, and the advice differs
  expect_equal(.classify_connect_error("SSL SYSCALL error: EOF detected"), "server_closed")
  expect_equal(.classify_connect_error("server does not support SSL, but SSL was required"), "ssl")
})

test_that(".classify_connect_error handles empty and missing input", {
  expect_equal(.classify_connect_error(NULL), "unknown")
  expect_equal(.classify_connect_error(""), "unknown")
  expect_equal(.classify_connect_error("something nobody has seen before"), "unknown")
})

# =============================================================================
# Messages
# =============================================================================

test_that("every error code has a label and at least one remedy", {
  codes <- c("auth", "dns", "timeout", "refused", "unreachable",
             "too_many_clients", "no_database", "ssl", "server_closed", "unknown")

  for (code in codes) {
    label <- .connect_error_label(code)
    expect_type(label, "character")
    expect_true(nzchar(label))

    hints <- .connect_error_hints(code, "db.example.org", 35699)
    expect_named(hints, c("diagnosis", "steps"))
    expect_gte(length(hints$diagnosis), 1)
    expect_gte(length(hints$steps), 1)
    expect_true(all(nzchar(unlist(hints))))
  }
})

test_that("network diagnoses name the port and the firewall, auth ones do not", {
  blocked <- .connect_error_hints("timeout", "db.example.org", 35699)
  expect_match(paste(blocked$diagnosis, collapse = " "), "35699", fixed = TRUE)
  expect_match(paste(blocked$diagnosis, collapse = " "), "firewall")
  expect_match(paste(blocked$steps, collapse = " "), "hotspot")

  auth <- .connect_error_hints("auth", "db.example.org", 35699)
  expect_false(any(grepl("firewall", unlist(auth))))
  expect_match(paste(auth$steps, collapse = " "), "reset = TRUE", fixed = TRUE)
})

test_that("the thrown message names the target and points at the diagnostic", {
  msg <- .connect_failure_message("timeout expired", "db.example.org", 35699)

  expect_match(msg, "db.example.org:35699", fixed = TRUE)
  expect_match(msg, "check_db_network()", fixed = TRUE)
  expect_match(msg, "blocking outbound traffic")
  expect_match(msg, "Original error: timeout expired", fixed = TRUE)
})

test_that("the thrown message does not blame the network for a bad password", {
  msg <- .connect_failure_message(
    'FATAL:  password authentication failed for user "x"',
    "db.example.org", 35699
  )

  expect_match(msg, "credentials rejected", fixed = TRUE)
  expect_false(grepl("blocking outbound traffic", msg))
})

test_that("short hints exist for the codes users actually hit, and are translated", {
  translation_file <- system.file("translations", "translation.json",
                                  package = "CafriplotsR")
  skip_if(!nzchar(translation_file) || !file.exists(translation_file),
          "translation file not installed")
  json <- paste(readLines(translation_file, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")

  for (code in c("timeout", "unreachable", "refused", "server_closed",
                 "dns", "auth", "too_many_clients")) {
    hint <- .connect_error_short_hint(code)
    expect_true(nzchar(hint), info = code)
    # The Shiny login looks the hint up by its English text
    expect_true(grepl(hint, json, fixed = TRUE),
                info = paste("missing translation for", code))
  }

  expect_equal(.connect_error_short_hint("unknown"), "")
})

# =============================================================================
# check_db_network() verdicts
# =============================================================================

test_that("check_db_network reports the port as reachable without probing the control host", {
  control_probed <- FALSE

  testthat::local_mocked_bindings(.package = "CafriplotsR",
    .probe_tcp = function(host, port, timeout = 5) {
      if (port != 35699) control_probed <<- TRUE
      list(ok = TRUE, elapsed = 0.1, error = NULL)
    }
  )

  res <- check_db_network(host = "db.example.org", port = 35699, verbose = FALSE)

  expect_equal(res$verdict, "reachable")
  expect_null(res$control)
  expect_false(control_probed)
})

test_that("check_db_network calls a blocked port a blocked port", {
  testthat::local_mocked_bindings(.package = "CafriplotsR",
    .probe_tcp = function(host, port, timeout = 5) {
      # Database port dropped, general internet fine
      list(ok = port == 443, elapsed = 1, error = if (port != 443) "cannot open" else NULL)
    }
  )

  res <- check_db_network(host = "db.example.org", port = 35699, verbose = FALSE)

  expect_equal(res$verdict, "port_blocked")
  expect_false(res$database$ok)
  expect_true(res$control$ok)
})

test_that("check_db_network distinguishes a dead network from a blocked port", {
  testthat::local_mocked_bindings(.package = "CafriplotsR",
    .probe_tcp = function(host, port, timeout = 5) {
      list(ok = FALSE, elapsed = 5, error = "cannot open")
    }
  )

  res <- check_db_network(host = "db.example.org", port = 35699, verbose = FALSE)

  expect_equal(res$verdict, "no_connectivity")
  expect_false(res$control$ok)
})

test_that(".probe_tcp fails cleanly on a closed local port", {
  # Loopback only - no network required, and nothing listens on this port
  res <- .probe_tcp("127.0.0.1", 1L, timeout = 1)

  expect_false(res$ok)
  expect_type(res$error, "character")
  expect_type(res$elapsed, "double")
})
