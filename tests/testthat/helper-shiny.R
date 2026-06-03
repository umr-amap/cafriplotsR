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

# Strict icon validation, independent of the installed shiny version.
#
# Some shiny UI constructors (actionButton, actionLink, tabPanel, ...) accept an
# `icon` argument. A non-icon object accidentally bound to `icon` — e.g. via a
# positional-argument mistake — is rejected by shiny <= 1.11.1 (validateIcon)
# but slips through silently on shiny >= 1.12.0, which dropped that check. To
# catch this class of bug regardless of which shiny the developer has installed,
# `local_strict_icons()` temporarily wraps those constructors so that whatever
# binds to their `icon` formal must be NULL or a real `shiny::icon()` (an <i>
# tag). The wrapper re-uses R's own argument matching (match.call against the
# original signature), so it sees exactly what the constructor would bind to
# `icon`. Restoration is deferred to the calling test frame.
local_strict_icons <- function(env = parent.frame()) {
  shiny_ns <- asNamespace("shiny")

  strict_validate <- function(icon, fname) {
    if (is.null(icon) || identical(icon, character(0))) return(invisible())
    if (inherits(icon, "shiny.tag") && isTRUE(icon$name == "i")) return(invisible())
    got <- if (inherits(icon, "shiny.tag")) {
      paste0("<", icon$name, "> tag")
    } else {
      paste(class(icon), collapse = "/")
    }
    stop(sprintf(
      paste0("Non-icon bound to the `icon` argument of shiny::%s() ",
             "(got %s). This is usually a positional-argument mistake: ",
             "an unnamed UI element falling onto `icon`. Name the argument ",
             "(e.g. label = ...) so it does not land on `icon`."),
      fname, got
    ), call. = FALSE)
  }

  targets <- c("actionButton", "actionLink", "tabPanel", "navbarMenu",
               "downloadButton", "downloadLink")
  targets <- Filter(function(f) {
    exists(f, envir = shiny_ns, inherits = FALSE) &&
      "icon" %in% names(formals(get(f, envir = shiny_ns)))
  }, targets)

  originals <- mget(targets, envir = shiny_ns)

  for (fname in targets) {
    wrapper <- local({
      o <- originals[[fname]]
      fn <- fname
      function(...) {
        matched <- as.list(match.call(definition = o))[-1]
        if ("icon" %in% names(matched)) {
          icon_val <- tryCatch(eval.parent(matched[["icon"]]),
                               error = function(e) NULL)
          strict_validate(icon_val, fn)
        }
        o(...)
      }
    })
    if (bindingIsLocked(fname, shiny_ns)) unlockBinding(fname, shiny_ns)
    assign(fname, wrapper, envir = shiny_ns)
  }

  withr::defer({
    for (fname in names(originals)) {
      if (bindingIsLocked(fname, shiny_ns)) unlockBinding(fname, shiny_ns)
      assign(fname, originals[[fname]], envir = shiny_ns)
      lockBinding(fname, shiny_ns)
    }
  }, envir = env)

  invisible()
}
