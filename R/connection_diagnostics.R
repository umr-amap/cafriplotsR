# Connection failure diagnostics ------------------------------------------
#
# The database listens on a non-standard port (35699). Institutional and
# corporate networks routinely allow only 80/443 outbound, so a connection
# that works from home fails from the office with nothing but
# "timeout expired" to go on. The helpers here name that situation instead
# of leaving users to guess.

#' Classify a libpq connection error message
#'
#' @description
#' Maps the raw error text returned by `RPostgres`/libpq to a short code so
#' the package can say *why* a connection failed rather than echoing the
#' driver message.
#'
#' @param msg Character. The error message from a failed `DBI::dbConnect()`.
#'
#' @return One of `"auth"`, `"dns"`, `"timeout"`, `"refused"`,
#'   `"unreachable"`, `"too_many_clients"`, `"no_database"`, `"ssl"`,
#'   `"server_closed"` or `"unknown"`.
#'
#' @keywords internal
#' @noRd
.classify_connect_error <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) return("unknown")

  # Authentication is already detected elsewhere and takes precedence: a
  # rejected password also produces a "connection ... failed" wrapper.
  if (.is_auth_error(msg)) return("auth")

  # Order matters: "SSL SYSCALL error: EOF detected" would otherwise be
  # reported as an SSL problem when it is really a dropped connection.
  patterns <- list(
    dns = c("could not translate host name",
            "Name or service not known",
            "nodename nor servname",
            "Temporary failure in name resolution",
            "unknown host",
            "no address associated with"),
    timeout = c("timeout expired",
                "connection timed out",
                "operation timed out"),
    refused = c("connection refused"),
    unreachable = c("network is unreachable",
                    "no route to host",
                    "host is unreachable"),
    too_many_clients = c("too many clients already",
                         "remaining connection slots are reserved"),
    no_database = c("database \".*\" does not exist"),
    server_closed = c("server closed the connection unexpectedly",
                      "could not receive data from server",
                      "could not send data to server",
                      "EOF detected"),
    ssl = c("SSL error",
            "server does not support SSL",
            "certificate verify failed")
  )

  for (code in names(patterns)) {
    if (any(vapply(patterns[[code]],
                   function(p) grepl(p, msg, ignore.case = TRUE),
                   logical(1)))) {
      return(code)
    }
  }

  "unknown"
}

#' Is this failure a network-level problem rather than a database problem?
#'
#' @keywords internal
#' @noRd
.is_network_error <- function(code) {
  code %in% c("dns", "timeout", "refused", "unreachable", "server_closed")
}

#' One-line plain-language label for a connection error code
#'
#' Used in the "attempt N failed" messages so a retry says what went wrong.
#'
#' @keywords internal
#' @noRd
.connect_error_label <- function(code) {
  switch(
    code,
    auth             = "credentials rejected",
    dns              = "host name could not be resolved",
    timeout          = "no answer from the server",
    refused          = "server actively refused the connection",
    unreachable      = "network unreachable",
    too_many_clients = "server has no free connection slots",
    no_database      = "database does not exist",
    ssl              = "SSL negotiation problem",
    server_closed    = "connection dropped mid-handshake",
    "unknown error"
  )
}

#' Explanation and remedies for a connection error code
#'
#' @return A list with `diagnosis` (one or two sentences saying what happened
#'   and why) and `steps` (what to do, in order). Both are character vectors.
#'
#' @keywords internal
#' @noRd
.connect_error_hints <- function(code, host, port) {
  target <- paste0(host, ":", port)

  firewall_note <- paste0(
    "The database listens on port ", port, ", which is not a standard web ",
    "port. Institutional, campus, hotel and corporate networks (and some ",
    "VPNs) commonly allow only ports 80 and 443 outbound, which blocks it."
  )

  firewall_steps <- c(
    "Run `check_db_network()` to confirm whether the port is blocked.",
    "Retry from another network - a phone hotspot is the quickest test.",
    paste0("If the hotspot works, the network is the cause: ask your IT ",
           "service to allow outbound TCP to ", target, "."),
    "If every network fails, the server may be down - contact the maintainer."
  )

  switch(
    code,
    auth = list(
      diagnosis = "The server was reached, but it rejected the username or password. The network is not the problem here.",
      steps = c(
        "Check for typos, and for a stale password saved in ~/.Renviron.",
        "Use `call.mydb(reset = TRUE)` to re-enter your credentials.",
        "If you never had an account, ask the maintainer for one."
      )
    ),
    dns = list(
      diagnosis = c(
        paste0("The host name ", host, " could not be resolved to an address, ",
               "so no connection was even attempted."),
        "You are most likely offline, behind a captive portal (hotel or airport Wi-Fi that wants a login page first), or on a DNS server that filters the name."
      ),
      steps = c(
        "Open any web page first, to clear a captive portal, then retry.",
        "Run `check_db_network()` to confirm.",
        "If a web page opens but this still fails, ask your IT service whether DNS is filtered."
      )
    ),
    timeout = list(
      diagnosis = c(
        paste0("The connection to ", target, " was opened but the server never ",
               "answered. Packets are being dropped silently, which is what a ",
               "firewall does."),
        firewall_note
      ),
      steps = firewall_steps
    ),
    unreachable = list(
      diagnosis = c(
        paste0("No network route to ", target, " exists from this machine."),
        firewall_note
      ),
      steps = c(
        "Check that you are connected to a network and that any VPN is up.",
        firewall_steps
      )
    ),
    refused = list(
      diagnosis = c(
        paste0("Something answered at ", target, " and refused the connection ",
               "immediately."),
        "That is usually a proxy or firewall rejecting the port, rather than the database itself."
      ),
      steps = c(
        "Run `check_db_network()` to see whether the port is reachable at all.",
        "Retry from another network - a phone hotspot is the quickest test.",
        "If it is refused on every network, the database service may be stopped - contact the maintainer."
      )
    ),
    server_closed = list(
      diagnosis = c(
        "The connection was established and then dropped before the session was ready.",
        "That is typical of a network appliance inspecting traffic on non-standard ports, and of unstable Wi-Fi."
      ),
      steps = c(
        "Retry once - a single dropped connection is often just the network.",
        "Retry from another network to confirm.",
        "If it happens repeatedly on a stable network, contact the maintainer."
      )
    ),
    too_many_clients = list(
      diagnosis = "The server is up and your credentials are fine, but all its connection slots are in use.",
      steps = c(
        "Wait a minute and retry.",
        "Call `cleanup_connections()` in any other R sessions you have left open - forgotten sessions are the usual cause."
      )
    ),
    no_database = list(
      diagnosis = "The server was reached and accepted your credentials, but the requested database does not exist.",
      steps = c(
        "Check the database name in ~/.mydb_config.R.",
        "Delete that file to restore the defaults, then retry."
      )
    ),
    ssl = list(
      diagnosis = c(
        "The connection failed while negotiating encryption.",
        "Some networks intercept encrypted traffic with their own certificate, which PostgreSQL will not accept."
      ),
      steps = c(
        "Retry from another network to confirm.",
        "If it is reproducible, send the full error message to the maintainer."
      )
    ),
    list(
      diagnosis = "The failure could not be matched to a known cause.",
      steps = c(
        "Run `check_db_network()` to rule out a network problem.",
        "If the network is fine, send the full error message to the maintainer."
      )
    )
  )
}

#' Print a diagnosis for a failed connection attempt
#'
#' Called after the last retry, so the user gets an explanation instead of a
#' bare libpq message.
#'
#' @keywords internal
#' @noRd
.report_connect_failure <- function(err_msg, db_type, attempts, host, port) {
  code <- .classify_connect_error(err_msg)
  label <- .connect_error_label(code)
  hints <- .connect_error_hints(code, host, port)
  target <- paste0(host, ":", port)

  cli::cli_alert_danger(
    "Failed to connect to {db_type} database after {attempts} attempt{?s}: {label}"
  )
  cli::cli_bullets(c(" " = "Target: {target} (TCP)"))
  cli::cli_bullets(c(" " = "Server said: {err_msg}"))
  cli::cli_verbatim("")

  for (line in hints$diagnosis) cli::cli_alert_info("{line}")

  if (length(hints$steps) > 0) {
    cli::cli_verbatim("")
    cli::cli_text("What to try, in order:")
    cli::cli_ol(hints$steps)
  }

  invisible(code)
}

#' Short, self-explanatory message for the error thrown to the caller
#'
#' `stop()` output is what ends up in bug reports and in Shiny notifications,
#' so it carries the diagnosis rather than just the driver text.
#'
#' @keywords internal
#' @noRd
.connect_failure_message <- function(err_msg, host, port) {
  code <- .classify_connect_error(err_msg)
  target <- paste0(host, ":", port)

  headline <- if (.is_network_error(code)) {
    paste0(
      "could not reach ", target, " (", .connect_error_label(code), "). ",
      "The network you are on is probably blocking outbound traffic to port ",
      port, " - this is common on institutional connections. ",
      "Run check_db_network() for a diagnosis, or retry from another network."
    )
  } else {
    paste0(.connect_error_label(code), " (", target, ").")
  }

  paste0("Database connection failed: ", headline, "\n  Original error: ", err_msg)
}

#' One-sentence hint for UI surfaces that cannot show the full report
#'
#' Every string returned here must exist in `inst/translations/translation.json`
#' so the Shiny login can translate it.
#'
#' @return A character scalar, possibly `""` when there is nothing useful to
#'   add beyond the driver message.
#'
#' @keywords internal
#' @noRd
.connect_error_short_hint <- function(code) {
  switch(
    code,
    timeout = ,
    unreachable = ,
    refused = ,
    server_closed = "The database server could not be reached. This network is probably blocking the connection - try another network, a phone hotspot is the quickest test.",
    dns = "The database server name could not be resolved. Check your internet connection, or open a web page first if you are on a Wi-Fi network that requires a login.",
    auth = "The server rejected this username or password.",
    too_many_clients = "The server currently has no free connection slots. Wait a moment and try again.",
    ""
  )
}

#' Run a connection expression, diagnosing any failure
#'
#' Wraps pool creation (and any other one-shot connect) so that Shiny logins
#' get the same explanation as `connect_database()` instead of the raw driver
#' message.
#'
#' @keywords internal
#' @noRd
.with_connect_diagnostics <- function(expr, db_type, host, port) {
  tryCatch(
    expr,
    error = function(e) {
      err_msg <- conditionMessage(e)
      .report_connect_failure(err_msg, db_type, 1L, host, port)
      stop(.connect_failure_message(err_msg, host, port), call. = FALSE)
    }
  )
}

#' Test whether a TCP port can be reached
#'
#' @param host Character. Host name or address.
#' @param port Integer. TCP port.
#' @param timeout Numeric. Seconds to wait.
#'
#' @return A list with `ok` (logical), `elapsed` (seconds) and `error`
#'   (message or `NULL`).
#'
#' @keywords internal
#' @noRd
.probe_tcp <- function(host, port, timeout = 5) {
  started <- Sys.time()
  sock <- NULL
  err <- NULL

  ok <- tryCatch({
    suppressWarnings(
      sock <- socketConnection(
        host = host, port = port,
        blocking = TRUE, open = "r+", timeout = timeout
      )
    )
    TRUE
  }, error = function(e) {
    err <<- conditionMessage(e)
    FALSE
  })

  if (!is.null(sock)) try(close(sock), silent = TRUE)

  list(
    ok = isTRUE(ok),
    elapsed = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1),
    error = err
  )
}

#' Diagnose why the database cannot be reached
#'
#' @description
#' Distinguishes the three situations that all surface as the same unhelpful
#' "timeout expired" error when connecting:
#'
#' 1. the network is fine and the problem lies with the credentials or the
#'    server itself;
#' 2. the network works in general but blocks the database port - the usual
#'    case on institutional, campus and corporate connections, which often
#'    allow only ports 80 and 443 outbound;
#' 3. there is no working internet connection at all.
#'
#' It opens a raw TCP connection to the database host and port, and, if that
#' fails, a second one to a control host on port 443 to tell case 2 from
#' case 3. No credentials are used and nothing is sent, so it is safe to run
#' and safe to ask a user to run.
#'
#' @param host Character. Database host. Defaults to the configured host.
#' @param port Integer. Database port. Defaults to the configured port.
#' @param timeout Numeric. Seconds to wait for each probe. Default 5. A
#'   blocked port typically takes the full timeout; note that on some systems
#'   the operating system's own TCP timeout (around 20 seconds) applies
#'   instead, so the check can take longer than requested.
#' @param control_host Character. Host used to verify general connectivity.
#'   Default `"cran.r-project.org"`.
#' @param control_port Integer. Port for the control probe. Default 443.
#' @param verbose Logical. Print the report. Default `TRUE`.
#'
#' @return Invisibly, a list with `host`, `port`, `database` (the probe
#'   result), `control` (the control probe, or `NULL` if it was not needed)
#'   and `verdict`, one of `"reachable"`, `"port_blocked"`,
#'   `"no_connectivity"`.
#'
#' @examples
#' \dontrun{
#' # Why did call.mydb() just time out?
#' check_db_network()
#' }
#'
#' @export
check_db_network <- function(host = NULL,
                             port = NULL,
                             timeout = 5,
                             control_host = "cran.r-project.org",
                             control_port = 443,
                             verbose = TRUE) {

  # Only touch the config when we actually need the defaults from it
  if (is.null(host) || is.null(port)) create_db_config()

  if (is.null(host)) {
    host <- if (exists("db_host", envir = .GlobalEnv)) {
      get("db_host", envir = .GlobalEnv)
    } else {
      "dg474899-001.dbaas.ovh.net"
    }
  }
  if (is.null(port)) {
    port <- if (exists("db_port", envir = .GlobalEnv)) {
      get("db_port", envir = .GlobalEnv)
    } else {
      35699
    }
  }

  target <- paste0(host, ":", port)

  if (verbose) {
    cli::cli_h1("Database network check")
    cli::cli_alert_info("Probing {target} (up to {timeout}s)...")
  }

  db_probe <- .probe_tcp(host, port, timeout)
  control_probe <- NULL

  if (db_probe$ok) {
    verdict <- "reachable"
    if (verbose) {
      cli::cli_alert_success("Port {port} on {host} is reachable ({db_probe$elapsed}s)")
      cli::cli_text("")
      cli::cli_alert_info(
        "The network path is fine. If connecting still fails, the cause is your credentials or the database server itself, not the network."
      )
      cli::cli_bullets(c(
        "*" = "Wrong password? {.code call.mydb(reset = TRUE)}",
        "*" = "Everything else: {.code db_diagnostic()}"
      ))
    }
  } else {
    if (verbose) {
      cli::cli_alert_danger("Port {port} on {host} is NOT reachable ({db_probe$elapsed}s)")
      cli::cli_alert_info("Checking general connectivity via {control_host}:{control_port}...")
    }

    control_probe <- .probe_tcp(control_host, control_port, timeout)

    if (control_probe$ok) {
      verdict <- "port_blocked"
      if (verbose) {
        cli::cli_alert_success("{control_host}:{control_port} is reachable ({control_probe$elapsed}s)")
        cli::cli_text("")
        cli::cli_alert_warning("Diagnosis: this network blocks the database port.")
        cli::cli_text(
          "Your internet connection works, but outbound traffic to port {port} does not get through. Institutional, campus and corporate networks (and some VPNs) commonly allow only ports 80 and 443. The database is almost certainly running fine."
        )
        cli::cli_text("")
        cli::cli_text("What to do:")
        cli::cli_ol(c(
          "Retry from another network - a phone hotspot is the quickest test.",
          "If the hotspot works, ask your IT service to allow outbound TCP to {target}.",
          "Meanwhile, work from a network that is not filtered."
        ))
      }
    } else {
      verdict <- "no_connectivity"
      if (verbose) {
        cli::cli_alert_danger("{control_host}:{control_port} is not reachable either ({control_probe$elapsed}s)")
        cli::cli_text("")
        cli::cli_alert_warning("Diagnosis: no usable internet connection from this machine.")
        cli::cli_ol(c(
          "Check your Wi-Fi or cable connection.",
          "If you are on hotel or airport Wi-Fi, open any web page first to get past the login portal.",
          "If you are behind a corporate proxy, note that PostgreSQL cannot use an HTTP proxy - you need direct outbound access.",
          "Then run {.code check_db_network()} again."
        ))
      }
    }
  }

  if (verbose) {
    cli::cli_text("")
    cli::cli_alert_info("Verdict: {verdict}")
  }

  invisible(list(
    host = host,
    port = port,
    database = db_probe,
    control = control_probe,
    verdict = verdict
  ))
}
