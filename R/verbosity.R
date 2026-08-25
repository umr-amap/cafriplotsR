# Console verbosity for long-running queries
#
# A full query_plots() call runs a dozen internal steps, each narrating itself
# with cli. That log is invaluable while debugging and overwhelming otherwise:
# roughly fifty lines scroll past, and the three that matter - what was found,
# what was silently dropped - are buried among connection notices and section
# headers.
#
# Every cli call signals a `cli_message` condition carrying the alert type
# ("alert_info", "alert_warning", "h2", ...). A calling handler can therefore
# mute whole severity classes without touching a single call site: muted
# messages are dismissed with the `cli_message_handled` restart, which stops
# cli's default handler from rendering them. Nothing about how the internals
# report their work changes; only what reaches the console does.

.verb_env <- new.env(parent = emptyenv())
.verb_env$level <- NULL
.verb_env$unmuted <- FALSE
.verb_env$tally <- list()

#' Verbosity levels, from quietest to loudest
#' @keywords internal
#' @noRd
.verbosity_levels <- c("quiet", "normal", "debug")

#' cli alert types that survive at each verbosity level
#'
#' "debug" is handled by the caller (no filtering at all), so only the two
#' filtered levels are listed. Warnings and failures always get through: they
#' report data being dropped or a step going wrong, which is never noise.
#'
#' @keywords internal
#' @noRd
.cli_allowed_types <- function(level) {
  switch(
    level,
    quiet  = c("alert_warning", "alert_danger"),
    normal = c("alert_warning", "alert_danger"),
    character(0)
  )
}

#' Resolve a verbosity argument to a level name
#'
#' Accepts the level names, `NULL` (fall back to the `CafriplotsR.verbose`
#' option, then to "normal"), and logicals for convenience: `FALSE` is "quiet"
#' and `TRUE` is "debug", matching what a `verbose = TRUE` argument usually
#' means elsewhere.
#'
#' @param verbose Level name, logical, or NULL.
#'
#' @return One of "quiet", "normal", "debug".
#' @keywords internal
#' @noRd
.resolve_verbosity <- function(verbose = NULL) {

  if (is.null(verbose)) {
    verbose <- getOption("CafriplotsR.verbose", "normal")
  }

  if (is.logical(verbose) && length(verbose) == 1 && !is.na(verbose)) {
    return(if (verbose) "debug" else "quiet")
  }

  if (!is.character(verbose) || length(verbose) != 1) {
    cli::cli_abort(
      "{.arg verbose} must be one of {.val {.verbosity_levels}}, or a logical."
    )
  }

  match.arg(verbose, .verbosity_levels)
}

#' Current verbosity level
#'
#' @return One of "quiet", "normal", "debug".
#' @keywords internal
#' @noRd
.verbosity <- function() {
  .verb_env$level %||% .resolve_verbosity(NULL)
}

#' Is the current verbosity at least this loud?
#'
#' @param level Level name to compare against.
#'
#' @return Logical.
#' @keywords internal
#' @noRd
.verbosity_at_least <- function(level) {
  match(.verbosity(), .verbosity_levels) >= match(level, .verbosity_levels)
}

#' Evaluate an expression with cli output filtered to a verbosity level
#'
#' @param level One of "quiet", "normal", "debug".
#' @param expr Expression to evaluate.
#'
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
.with_cli_verbosity <- function(level, expr) {

  old_level <- .verb_env$level
  .verb_env$level <- level
  on.exit(.verb_env$level <- old_level, add = TRUE)

  if (identical(level, "debug")) {
    return(expr)
  }

  allowed <- .cli_allowed_types(level)

  withCallingHandlers(
    expr,
    cli_message = function(cnd) {
      # .verbose_output() lifts the filter for interactive prompts, whose
      # options are printed with cli and are useless unseen
      if (isTRUE(.verb_env$unmuted)) return()
      if (!isTRUE(cnd$type %in% allowed)) invokeRestart("cli_message_handled")
    }
  )
}

#' Evaluate an expression with cli filtering lifted
#'
#' Interactive helpers print the choices with cli and then read the answer with
#' `readline()`. Muting the choices would leave the user staring at a bare
#' prompt, so any code that asks a question runs unfiltered.
#'
#' @param expr Expression to evaluate.
#'
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
.verbose_output <- function(expr) {
  old <- .verb_env$unmuted
  .verb_env$unmuted <- TRUE
  on.exit(.verb_env$unmuted <- old, add = TRUE)
  expr
}

#' Record something worth reporting in the query summary
#'
#' Counts of dropped rows are computed deep inside the pipeline, where they are
#' reported at info level and therefore muted outside "debug". The tally
#' carries them back up so the summary can state them once, at the end.
#'
#' @param key Name of the tally entry.
#' @param value Number to add, or character vector to accumulate.
#'
#' @return Invisibly, the updated entry.
#' @keywords internal
#' @noRd
.tally_add <- function(key, value) {

  if (length(value) == 0) return(invisible(NULL))

  current <- .verb_env$tally[[key]]

  .verb_env$tally[[key]] <- if (is.character(value)) {
    unique(c(current, value))
  } else {
    sum(c(current, value), na.rm = TRUE)
  }

  invisible(.verb_env$tally[[key]])
}

#' Read a tally entry
#'
#' @param key Name of the tally entry.
#'
#' @return The stored value, or NULL when nothing was recorded.
#' @keywords internal
#' @noRd
.tally_get <- function(key) .verb_env$tally[[key]]

#' Clear the tally
#'
#' @return Invisibly, NULL.
#' @keywords internal
#' @noRd
.tally_reset <- function() {
  .verb_env$tally <- list()
  invisible(NULL)
}

#' Report what a query returned, and which argument shaped it
#'
#' Replaces the muted step-by-step log with the facts a caller acts on: how much
#' came back, what was dropped on the way, and which tables the result holds.
#' Every exclusion names the argument that controls it - a count of removed rows
#' is only actionable if you know which knob puts them back. Wrapped defensively:
#' a summary that fails must never take the query with it.
#'
#' @param res Result of [query_plots()]: a data frame or a named list of tables.
#' @param opts Named list of the arguments the summary explains:
#'   `census_strategy`, `show_multiple_census`, `issues`, `traits_to_genera`,
#'   `wd_fam_level`.
#'
#' @return Invisibly, NULL.
#' @keywords internal
#' @noRd
.report_query_summary <- function(res, opts = list()) {

  tryCatch({

    tables <- if (is.data.frame(res)) NULL else res
    n_row <- function(x) if (is.data.frame(x)) nrow(x) else NA_integer_

    n_individuals <- n_row(tables[["individuals"]] %||% tables[["extract"]])
    n_plots <- n_row(tables[["metadata"]] %||% tables[["meta_data"]] %||% tables[["plots"]])

    if (is.data.frame(res)) {
      # Unstructured result: one table, whatever it holds
      n_plots <- nrow(res)
      n_individuals <- NA_integer_
    }

    counts <- c(
      if (!is.na(n_plots)) "{n_plots} plot{?s}",
      if (!is.na(n_individuals)) "{n_individuals} individual{?s}"
    )

    # The style decides the shape of what came back, and "auto" means the query
    # picked it, so name it here rather than leaving it to be discovered
    style <- attr(res, "style")
    if (!is.null(style) && is.character(style)) {
      counts <- c(counts, "{style} style")
    }

    if (length(counts) > 0) {
      cli::cli_alert_success(paste(counts, collapse = ", "))
    }

    # Bind to locals first: cli reads a leading dot in `{...}` as a style name
    n_dead <- .tally_get("dead_individuals")
    n_flagged <- .tally_get("flagged_measurements")
    strategy <- opts$census_strategy %||% "selected"
    issues <- opts$issues %||% "remove"

    excluded <- c(
      if (!is.null(n_dead))
        "{n_dead} dead individual{?s}, absent from the {strategy} census: {.code census_strategy}, or {.code show_multiple_census = TRUE} to keep every census",
      if (!is.null(n_flagged))
        "{n_flagged} measurement{?s} flagged with an issue: {.code issues = \"{issues}\"}, or {.code issues = \"include\"} to keep them"
    )

    if (length(excluded) > 0) {
      cli::cli_alert_info("Excluded:")
      cli::cli_bullets(stats::setNames(excluded, rep("*", length(excluded))))
    }

    if (isTRUE(opts$traits_to_genera)) {
      # cli_bullets() re-wraps to the console width; cli_alert_info() does not
      cli::cli_bullets(c(i = "Taxonomic traits are genus-level aggregates, not values of the taxon itself: {.code traits_to_genera = TRUE}, provenance in the {.field source_*} columns"))
    }

    if (isTRUE(opts$wd_fam_level)) {
      cli::cli_bullets(c(i = "Wood density falls back to the family mean where the genus has none: {.code wd_fam_level = TRUE}"))
    }

    if (!is.null(tables) && !is.null(names(tables))) {
      cli::cli_alert_info("Tables: {.field {names(tables)}}")
    }

  }, error = function(e) {
    cli::cli_alert_info("Query summary unavailable ({e$message})")
  })

  invisible(NULL)
}
