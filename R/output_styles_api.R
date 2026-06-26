#' List available output styles
#'
#' @description
#' Returns a table summarising the output styles available to
#' [query_plots()]. Each row corresponds to one style and is intended to
#' help users pick the right value for the `output_style` argument.
#'
#' @return A [tibble][tibble::tibble-package] with one row per style and
#'   the columns:
#'   \describe{
#'     \item{`name`}{Character. Style identifier to pass as
#'       `output_style = "<name>"`.}
#'     \item{`description`}{Character. Short description of the style.}
#'     \item{`additional_tables`}{Character. Comma-separated list of
#'       extra tables the style produces (besides `metadata` and
#'       `individuals`), or `""` if none.}
#'     \item{`n_metadata_columns`}{Integer. Number of explicitly listed
#'       metadata columns, or `NA` when the style keeps `"all"`.}
#'     \item{`n_individuals_columns`}{Integer. Same for the individuals
#'       table.}
#'     \item{`n_keep_patterns`}{Integer. Number of regex patterns added
#'       on top of the explicit columns.}
#'     \item{`n_remove_patterns`}{Integer. Number of regex patterns used
#'       to drop columns.}
#'   }
#'
#' @seealso [get_output_style()] to retrieve a single style's full
#'   configuration; [query_plots()] to use a style.
#'
#' @examples
#' list_output_styles()
#'
#' @export
list_output_styles <- function() {

  styles <- .plot_output_styles

  count_or_na <- function(x) {
    if (is.null(x)) {
      return(0L)
    }
    if (length(x) == 1 && identical(x, "all")) {
      return(NA_integer_)
    }
    length(x)
  }

  rows <- lapply(names(styles), function(nm) {
    s <- styles[[nm]]
    dplyr::tibble(
      name                  = nm,
      description           = s$description %||% "",
      additional_tables     = paste(s$additional_tables %||% character(),
                                    collapse = ", "),
      n_metadata_columns    = count_or_na(s$metadata_columns),
      n_individuals_columns = count_or_na(s$individuals_columns),
      n_keep_patterns       = length(s$keep_patterns %||% character()),
      n_remove_patterns     = length(s$remove_patterns %||% character())
    )
  })

  do.call(rbind, rows)
}


#' Retrieve the configuration of an output style
#'
#' @description
#' Returns the configuration list backing a built-in output style. The
#' returned object has class `"plot_output_style"` and a dedicated print
#' method ([print.plot_output_style()]) that summarises the fields in a
#' readable form. Use [unclass()] on the result to see the raw list.
#'
#' @param name Character scalar. The name of the style to retrieve. Must
#'   be one of the names returned by [list_output_styles()].
#'
#' @return An object of class `"plot_output_style"`. Internally a named
#'   list whose fields are documented in [output_style()].
#'
#' @seealso [list_output_styles()] for the list of available styles;
#'   [output_style()] for the meaning of each field.
#'
#' @examples
#' # Inspect the default standard style
#' get_output_style("standard")
#'
#' # Programmatic access to the underlying list
#' cfg <- unclass(get_output_style("permanent_plot"))
#' cfg$remove_patterns
#'
#' @export
get_output_style <- function(name) {

  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop("`name` must be a single non-NA character string.", call. = FALSE)
  }

  available <- names(.plot_output_styles)

  if (!name %in% available) {
    stop(
      sprintf(
        "Unknown output style '%s'. Available styles: %s.",
        name,
        paste(available, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  config <- .plot_output_styles[[name]]
  attr(config, "style_name") <- name
  class(config) <- c("plot_output_style", "list")
  config
}


#' Print an output style configuration
#'
#' @description
#' Pretty-prints a `plot_output_style` object as returned by
#' [get_output_style()] or built by [output_style()]. The output groups
#' fields by purpose: column selection, pattern filters, renames,
#' additional tables, and flags. Custom styles built with
#' [output_style()] show the parent they were derived from, when any.
#'
#' @param x A `plot_output_style` object.
#' @param ... Currently ignored.
#'
#' @return `x`, invisibly.
#'
#' @export
print.plot_output_style <- function(x, ...) {

  style_name <- attr(x, "style_name") %||% "<unnamed>"
  parent     <- attr(x, "based_on")
  desc       <- x$description %||% ""

  cli::cli_h1("Output style: {.strong {style_name}}")
  if (!is.null(parent) && nzchar(parent)) {
    cli::cli_text("Based on: {.strong {parent}}")
  }
  if (nzchar(desc)) {
    cli::cli_text("{desc}")
  }
  cli::cli_text("")

  # --- Column selections -------------------------------------------------
  cli::cli_h2("Column selection")
  .print_style_columns("metadata_columns",    x$metadata_columns)
  .print_style_columns("individuals_columns", x$individuals_columns)

  # --- Pattern filters ---------------------------------------------------
  if (length(x$keep_patterns) > 0 || length(x$remove_patterns) > 0) {
    cli::cli_h2("Pattern filters (Perl regex)")
    .print_style_patterns("keep_patterns",   x$keep_patterns)
    .print_style_patterns("remove_patterns", x$remove_patterns)
  }

  # --- Renames -----------------------------------------------------------
  if (!is.null(x$rename_columns) &&
      (length(x$rename_columns$metadata) > 0 ||
       length(x$rename_columns$individuals) > 0)) {
    cli::cli_h2("Column renames")
    .print_style_renames("metadata",    x$rename_columns$metadata)
    .print_style_renames("individuals", x$rename_columns$individuals)
  }
  if (length(x$census_column_renames) > 0) {
    cli::cli_h2("Census column renames")
    .print_style_renames("census prefix", x$census_column_renames)
  }

  # --- Additional tables -------------------------------------------------
  if (length(x$additional_tables) > 0) {
    cli::cli_h2("Additional tables")
    cli::cli_ul(x$additional_tables)
  }

  # --- Flags -------------------------------------------------------------
  flags <- c(
    keep_common_features = isTRUE(x$keep_common_features),
    keep_census_columns  = isTRUE(x$keep_census_columns),
    keep_all_features    = isTRUE(x$keep_all_features)
  )
  if (any(flags)) {
    cli::cli_h2("Flags")
    for (nm in names(flags)[flags]) {
      cli::cli_li("{.field {nm}}: TRUE")
    }
  }

  cli::cli_text("")
  cli::cli_text("{.emph Use unclass() to see the raw configuration list.}")

  invisible(x)
}


# ---- internal print helpers -------------------------------------------------

#' @keywords internal
#' @noRd
.print_style_columns <- function(label, value) {

  if (is.null(value)) {
    cli::cli_li("{.field {label}}: {.emph (unset)}")
    return(invisible(NULL))
  }

  if (length(value) == 1 && identical(value, "all")) {
    cli::cli_li("{.field {label}}: {.strong all columns}")
    return(invisible(NULL))
  }

  cli::cli_li("{.field {label}} ({length(value)}): {paste(value, collapse = ', ')}")
}

#' @keywords internal
#' @noRd
.print_style_patterns <- function(label, value) {

  if (length(value) == 0) {
    return(invisible(NULL))
  }
  cli::cli_li("{.field {label}} ({length(value)}):")
  for (pat in value) {
    cli::cli_text("    {.code {pat}}")
  }
}

#' @keywords internal
#' @noRd
.print_style_renames <- function(label, value) {

  if (length(value) == 0) {
    return(invisible(NULL))
  }
  cli::cli_li("{.field {label}} ({length(value)}):")
  for (i in seq_along(value)) {
    old_name <- names(value)[[i]]
    new_name <- value[[i]]
    cli::cli_text("    {.code {old_name}} → {.code {new_name}}")
  }
}


# Note: `%||%` is defined once in R/individual_features_function.R and is
# available to all functions in this file via the package namespace.


# ============================================================================
# Custom output styles -- Phase 2 (pass-by-object)
# ============================================================================

#' Build a custom output style
#'
#' @description
#' Constructs a `plot_output_style` object that can be passed directly to
#' the `output_style` argument of [query_plots()]. The returned object
#' lives only in the current R session -- it is *not* registered or
#' cached. To reuse it, assign it to a variable, save it with
#' [saveRDS()], or put the constructor call in `.Rprofile` / a project
#' script.
#'
#' Provide `based_on` to inherit from an existing style and override
#' only the fields you care about. **Override semantics are
#' "replace, not append"**: any field you pass replaces the parent's
#' value entirely. To clear a vector field while inheriting the rest,
#' pass an empty vector (e.g. `remove_patterns = character()`); to
#' inherit unchanged, leave the argument unspecified.
#'
#' @param description Character scalar. Short human-readable label.
#' @param metadata_columns Character vector of plot-level columns to keep
#'   in the `$metadata` table, or the literal string `"all"` to keep
#'   every plot-level column. Required when `based_on` is not provided.
#' @param individuals_columns Character vector of columns to keep in the
#'   `$individuals` table, or `"all"`. Required when `based_on` is not
#'   provided.
#' @param keep_patterns Character vector of Perl-compatible regexes. Any
#'   column matching any of these is added to the keep list.
#' @param remove_patterns Character vector of Perl-compatible regexes
#'   applied **after** `keep_patterns` to drop columns.
#' @param rename_columns Named list with optional elements `metadata` and
#'   `individuals`, each a named character vector
#'   `c(old_name = "new_name")`.
#' @param additional_tables Character vector of extra tables to attach.
#'   Recognised values: `"censuses"`, `"height_diameter"`.
#' @param keep_common_features Logical scalar. If `TRUE`, columns
#'   starting with `feat_` that are non-NA in more than 10% of rows are
#'   added to the metadata table.
#' @param keep_census_columns Logical scalar. If `TRUE` and
#'   `show_multiple_census = TRUE` in the query, every column ending in
#'   `_census_<N>` is kept on the individuals table.
#' @param keep_all_features Logical scalar. Reserved flag indicating that
#'   features should remain on the main table rather than being moved to
#'   the census table.
#' @param census_column_renames Named character vector mapping
#'   `c(old_prefix = "new_prefix")`. Applied to columns matching
#'   `^<old_prefix>_census_\\d+$` when census columns are kept.
#' @param based_on Optional. Either the name of a built-in style (e.g.
#'   `"permanent_plot"`) or another `plot_output_style` object to inherit
#'   from. When supplied, missing arguments are inherited from the
#'   parent.
#'
#' @return A `plot_output_style` object: a named list with class
#'   `"plot_output_style"` and an attached print method. Use [unclass()]
#'   to see the raw list, or pass it straight to [query_plots()].
#'
#' @seealso [list_output_styles()], [get_output_style()].
#'
#' @examples
#' # Inherit from "permanent_plot" and drop trait_ columns
#' my_style <- output_style(
#'   based_on        = "permanent_plot",
#'   remove_patterns = c("^id_(?!n|liste_plots)", "^date_modif",
#'                       "_census_\\d+$", "^trait_")
#' )
#' my_style
#'
#' # Build a style from scratch
#' tiny <- output_style(
#'   description         = "Just IDs and species",
#'   metadata_columns    = c("plot_name", "country", "id_liste_plots"),
#'   individuals_columns = c("id_n", "tag", "tax_fam", "tax_gen", "tax_sp_level")
#' )
#'
#' @export
output_style <- function(description           = NULL,
                         metadata_columns      = NULL,
                         individuals_columns   = NULL,
                         keep_patterns         = NULL,
                         remove_patterns       = NULL,
                         rename_columns        = NULL,
                         additional_tables     = NULL,
                         keep_common_features  = NULL,
                         keep_census_columns   = NULL,
                         keep_all_features     = NULL,
                         census_column_renames = NULL,
                         based_on              = NULL) {

  # -- Resolve parent ---------------------------------------------------------
  base <- if (is.null(based_on)) {
    list()
  } else if (inherits(based_on, "plot_output_style")) {
    unclass(based_on)
  } else if (is.character(based_on) && length(based_on) == 1L && !is.na(based_on)) {
    unclass(get_output_style(based_on))
  } else {
    stop(
      "`based_on` must be a single style name (character) or a `plot_output_style` object.",
      call. = FALSE
    )
  }

  # -- Collect user-supplied overrides (drop NULL = "inherit from parent") ----
  supplied <- list(
    description           = description,
    metadata_columns      = metadata_columns,
    individuals_columns   = individuals_columns,
    keep_patterns         = keep_patterns,
    remove_patterns       = remove_patterns,
    rename_columns        = rename_columns,
    additional_tables     = additional_tables,
    keep_common_features  = keep_common_features,
    keep_census_columns   = keep_census_columns,
    keep_all_features     = keep_all_features,
    census_column_renames = census_column_renames
  )
  supplied <- supplied[!vapply(supplied, is.null, logical(1))]

  # Replace semantics (not deep-merge): user-supplied fields overwrite parent
  config <- utils::modifyList(base, supplied, keep.null = FALSE)

  # -- Required fields when no parent -----------------------------------------
  if (is.null(based_on)) {
    if (is.null(config$metadata_columns)) {
      stop("`metadata_columns` is required when `based_on` is not provided.",
           call. = FALSE)
    }
    if (is.null(config$individuals_columns)) {
      stop("`individuals_columns` is required when `based_on` is not provided.",
           call. = FALSE)
    }
  }

  validate_output_style(config)

  parent_name <- if (is.null(based_on)) {
    NULL
  } else if (is.character(based_on)) {
    based_on
  } else {
    attr(based_on, "style_name") %||% "<custom>"
  }

  attr(config, "style_name") <- "<custom>"
  attr(config, "based_on")   <- parent_name
  class(config) <- c("plot_output_style", "list")
  config
}


#' Validate an output style configuration
#'
#' @description
#' Checks that the fields of an output style configuration have the
#' expected types and shapes. Used internally by [output_style()] and by
#' [query_plots()] when a user passes a raw list. Exported so power
#' users can pre-validate a config before passing it on.
#'
#' @param config A list (or `plot_output_style` object) holding the
#'   configuration fields documented in [output_style()].
#'
#' @return The validated `config`, invisibly. Throws an error if any
#'   field is malformed and a warning when `additional_tables` contains
#'   values that are not currently recognised by the package.
#'
#' @keywords internal
validate_output_style <- function(config) {

  if (!is.list(config)) {
    stop("`config` must be a list.", call. = FALSE)
  }

  ok_char <- function(x, name) {
    if (is.null(x)) return(invisible())
    if (!is.character(x)) {
      stop(sprintf("`%s` must be a character vector.", name), call. = FALSE)
    }
  }

  ok_char(config$metadata_columns,    "metadata_columns")
  ok_char(config$individuals_columns, "individuals_columns")
  ok_char(config$keep_patterns,       "keep_patterns")
  ok_char(config$remove_patterns,     "remove_patterns")
  ok_char(config$additional_tables,   "additional_tables")

  if (!is.null(config$description) &&
      !(is.character(config$description) && length(config$description) == 1L)) {
    stop("`description` must be a single character string.", call. = FALSE)
  }

  if (!is.null(config$rename_columns)) {
    if (!is.list(config$rename_columns)) {
      stop("`rename_columns` must be a list with optional `metadata` and `individuals` elements.",
           call. = FALSE)
    }
    for (key in c("metadata", "individuals")) {
      v <- config$rename_columns[[key]]
      if (is.null(v)) next
      if (!is.character(v) || (length(v) > 0L && is.null(names(v)))) {
        stop(sprintf("`rename_columns$%s` must be a named character vector.", key),
             call. = FALSE)
      }
    }
  }

  if (!is.null(config$census_column_renames)) {
    v <- config$census_column_renames
    if (!is.character(v) || (length(v) > 0L && is.null(names(v)))) {
      stop("`census_column_renames` must be a named character vector.", call. = FALSE)
    }
  }

  for (flag in c("keep_common_features", "keep_census_columns", "keep_all_features")) {
    v <- config[[flag]]
    if (!is.null(v) && !(is.logical(v) && length(v) == 1L && !is.na(v))) {
      stop(sprintf("`%s` must be TRUE or FALSE.", flag), call. = FALSE)
    }
  }

  # Soft check: warn on unknown additional_tables values
  if (length(config$additional_tables) > 0L) {
    known   <- c("censuses", "height_diameter")
    unknown <- setdiff(config$additional_tables, known)
    if (length(unknown) > 0L) {
      warning(
        sprintf("Unknown `additional_tables` value(s): %s. Recognised values: %s.",
                paste(unknown, collapse = ", "),
                paste(known,   collapse = ", ")),
        call. = FALSE
      )
    }
  }

  invisible(config)
}


#' Resolve an output style argument into name + config
#'
#' Accepts either a character style name or a `plot_output_style`
#' object. Returns a list with `name` (used for messages and the
#' `style` attribute on results) and `config` (the underlying list).
#'
#' @keywords internal
#' @noRd
.resolve_output_style <- function(x) {

  if (inherits(x, "plot_output_style")) {
    return(list(
      name   = attr(x, "style_name") %||% "<custom>",
      config = unclass(x)
    ))
  }

  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    available <- names(.plot_output_styles)
    if (!x %in% available) {
      stop(sprintf(
        "Unknown output style '%s'. Available styles: %s. To use a custom style, build one with output_style() and pass the object directly.",
        x, paste(available, collapse = ", ")
      ), call. = FALSE)
    }
    return(list(name = x, config = .plot_output_styles[[x]]))
  }

  stop(
    "`output_style` must be a single style name (character) or a `plot_output_style` object built with output_style().",
    call. = FALSE
  )
}


#' Validate the `output_style` argument of query_plots()
#'
#' Accepts:
#'   * the special string `"auto"` (resolved later from `method`),
#'   * a built-in style name,
#'   * a `plot_output_style` object,
#'   * a raw list (validated then promoted to `plot_output_style`).
#'
#' @keywords internal
#' @noRd
.validate_query_plots_output_style <- function(x) {

  if (inherits(x, "plot_output_style")) {
    return(x)
  }

  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    valid_strings <- c("auto", names(.plot_output_styles))
    if (!x %in% valid_strings) {
      stop(sprintf(
        "`output_style` must be one of %s, or a `plot_output_style` object. Got '%s'.",
        paste0("'", valid_strings, "'", collapse = ", "),
        x
      ), call. = FALSE)
    }
    return(x)
  }

  # Raw list -> validate and promote
  if (is.list(x) && !is.data.frame(x)) {
    validate_output_style(x)
    attr(x, "style_name") <- attr(x, "style_name") %||% "<custom>"
    class(x) <- c("plot_output_style", "list")
    return(x)
  }

  stop(
    "`output_style` must be a character scalar (a style name or \"auto\"), a `plot_output_style` object, or a list configuration.",
    call. = FALSE
  )
}
