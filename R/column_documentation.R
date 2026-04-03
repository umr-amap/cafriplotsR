#' Describe columns in query results
#'
#' @description
#' Generates a documentation table explaining the content of each column in
#' \code{query_plots()} or \code{query_individual_features()} output.
#' Reverse-maps renamed, pivoted, and feature columns back to their original
#' meaning using output style configurations and database metadata.
#'
#' @param result A \code{plot_query_list} (from \code{query_plots()} with an
#'   output style applied) or a plain \code{data.frame}.
#' @param con A database connection (DBI connection or pool). If \code{NULL}
#'   (the default), uses the active connection from \code{call.mydb()}.
#'
#' @return An object of class \code{"column_documentation"}:
#'   \itemize{
#'     \item For a \code{plot_query_list}: a named list of data.frames (one per
#'       table: metadata, individuals, etc.)
#'     \item For a plain \code{data.frame}: a single data.frame
#'   }
#'   Each data.frame has columns: \code{column_name}, \code{original_name},
#'   \code{description}, \code{category}, \code{unit}, \code{notes}.
#'
#' @examples
#' \dontrun{
#' result <- query_plots(output_style = "standard")
#' docs <- describe_columns(result)
#' print(docs)
#' }
#'
#' @export
describe_columns <- function(result, con = NULL) {

  mydb <- if (!is.null(con)) con else call.mydb()

  # Fetch all available descriptions from flat + DB
  all_descriptions <- .get_column_descriptions(mydb, table_type = "plots")
  all_descriptions_ind <- .get_column_descriptions(mydb, table_type = "individuals")
  # Merge: individual descriptions take precedence for shared names
  combined_desc <- c(all_descriptions, all_descriptions_ind)
  # Deduplicate: keep last (individuals overrides plots for shared keys)
  combined_desc <- combined_desc[!duplicated(names(combined_desc), fromLast = TRUE)]

  # Determine style
  style <- attr(result, "style")
  if (is.null(style)) style <- "full"

  if (inherits(result, "plot_query_list")) {
    # Process each table in the list
    doc_list <- list()

    for (table_name in names(result)) {
      tbl <- result[[table_name]]
      if (!is.data.frame(tbl)) next

      table_type <- if (table_name == "metadata") "metadata" else "individuals"

      doc_list[[table_name]] <- .describe_table_columns(
        col_names = names(tbl),
        style = style,
        table_type = table_type,
        descriptions = combined_desc
      )
    }

    class(doc_list) <- c("column_documentation", "list")
    attr(doc_list, "style") <- style
    return(doc_list)

  } else if (is.data.frame(result)) {
    doc <- .describe_table_columns(
      col_names = names(result),
      style = style,
      table_type = "individuals",
      descriptions = combined_desc
    )
    class(doc) <- c("column_documentation_table", "data.frame")
    return(doc)

  } else {
    stop("'result' must be a plot_query_list or data.frame.")
  }
}


#' Generate documentation for a single table's columns
#'
#' @param col_names Character vector of column names in the output table
#' @param style Output style name (e.g., "standard", "permanent_plot")
#' @param table_type Either "metadata" or "individuals"
#' @param descriptions Named list from .get_column_descriptions()
#'
#' @return A data.frame with documentation for each column
#' @keywords internal
#' @noRd
.describe_table_columns <- function(col_names, style, table_type, descriptions) {

  # Build inverted rename map for this style
  inverted <- .invert_style_renames(style, table_type)

  rows <- lapply(col_names, function(col) {
    .document_single_column(col, inverted, descriptions)
  })

  result_df <- do.call(rbind, rows)
  rownames(result_df) <- NULL
  result_df
}


#' Document a single column
#'
#' @param col_name The output column name
#' @param inverted Named character vector mapping output names to original names
#' @param descriptions Named list of column descriptions
#'
#' @return A one-row data.frame
#' @keywords internal
#' @noRd
.document_single_column <- function(col_name, inverted, descriptions) {

  original_name <- col_name
  notes <- ""
  unit <- ""

  # Step 1: Check style renames (direct match)
  if (col_name %in% names(inverted)) {
    original_name <- inverted[[col_name]]
    notes <- paste0("Renamed from '", original_name, "'")
  }

  # Step 2: Check census column renames (pattern-based: dbh_census_1 -> stem_diameter_census_1)
  if (original_name == col_name) {
    # Try all census rename inversions
    inv_style <- attr(inverted, "style")
    if (is.null(inv_style)) inv_style <- "full"
    census_inv <- .invert_census_renames(inv_style)
    for (new_prefix in names(census_inv)) {
      old_prefix <- census_inv[[new_prefix]]
      pattern <- paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", new_prefix), "(_census_\\d+)$")
      if (grepl(pattern, col_name)) {
        suffix <- sub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", new_prefix)), "", col_name)
        original_name <- paste0(old_prefix, suffix)
        notes <- paste0("Renamed from '", original_name, "'")
        break
      }
    }
  }

  # Step 3: Parse pivot pattern on the (possibly un-renamed) column
  parsed <- .parse_pivot_pattern(original_name)

  # Determine the lookup key for descriptions
  if (!is.null(parsed)) {
    lookup_key <- parsed$trait_name
    # Build notes about the pivot suffix
    pivot_note <- switch(parsed$suffix_type,
      "census" = paste0("Census: ", parsed$census_id),
      "char" = "Character/categorical value",
      "char_census" = paste0("Character/categorical value, Census: ", parsed$census_id),
      "mean" = "Mean value across measurements",
      "sd" = "Standard deviation across measurements",
      "n" = "Number of measurements",
      "min" = "Minimum value",
      "max" = "Maximum value",
      "pair_0" = "Earlier census in pair",
      "pair_1" = "Later census in pair",
      "issue" = "Data quality issues",
      "issue_census" = paste0("Data quality issues, Census: ", parsed$census_id),
      "ids" = "Measurement IDs (aggregated)",
      "ids_census" = paste0("Measurement IDs, Census: ", parsed$census_id),
      ""
    )
    if (nzchar(pivot_note)) {
      notes <- if (nzchar(notes)) paste0(notes, ". ", pivot_note) else pivot_note
    }
  } else {
    lookup_key <- original_name
  }

  # Step 4: Look up description
  desc_info <- descriptions[[lookup_key]]

  if (!is.null(desc_info)) {
    description <- if (!is.null(desc_info$description)) desc_info$description else ""
    category <- if (!is.null(desc_info$category)) desc_info$category else "Other"
    if (!is.null(desc_info$expectedunit) && nzchar(if (!is.null(desc_info$expectedunit)) desc_info$expectedunit else "")) {
      unit <- desc_info$expectedunit
    }
  } else {
    # Fallback: use the lookup key as a readable description
    description <- gsub("_", " ", lookup_key)
    description <- paste0(toupper(substring(description, 1, 1)), substring(description, 2))
    category <- "Other"
  }

  data.frame(
    column_name = col_name,
    original_name = if (original_name != col_name) original_name else "",
    description = description,
    category = category,
    unit = unit,
    notes = notes,
    stringsAsFactors = FALSE
  )
}


#' Invert output style rename mappings
#'
#' @description
#' Given a style name and table type, returns a named character vector
#' mapping output (renamed) column names back to their original database names.
#'
#' @param style Output style name
#' @param table_type "metadata" or "individuals"
#'
#' @return Named character vector: names are output names, values are original names
#' @keywords internal
#' @noRd
.invert_style_renames <- function(style, table_type) {

  style_config <- .plot_output_styles[[style]]

  if (is.null(style_config) || is.null(style_config$rename_columns)) {
    result <- character(0)
    attr(result, "style") <- style
    return(result)
  }

  renames <- style_config$rename_columns[[table_type]]
  if (is.null(renames) || length(renames) == 0) {
    result <- character(0)
    attr(result, "style") <- style
    return(result)
  }

  # Original format: c("old_name" = "new_name")
  # Inverted: c("new_name" = "old_name")
  inverted <- stats::setNames(names(renames), renames)

  # Always add the hardcoded plot_id -> id_liste_plots rename
  inverted["plot_id"] <- "id_liste_plots"

  attr(inverted, "style") <- style
  inverted
}


#' Invert census column rename prefixes
#'
#' @param style Output style name
#' @return Named character vector: names are new prefixes, values are original prefixes
#' @keywords internal
#' @noRd
.invert_census_renames <- function(style) {

  style_config <- .plot_output_styles[[style]]

  if (is.null(style_config) || is.null(style_config$census_column_renames)) {
    return(character(0))
  }

  # Original: c("stem_diameter" = "dbh")
  # Inverted: c("dbh" = "stem_diameter")
  renames <- style_config$census_column_renames
  stats::setNames(names(renames), renames)
}


#' Parse pivot column name patterns
#'
#' @description
#' Extracts the base trait name and suffix information from pivoted column names.
#'
#' @param col_name Column name to parse
#'
#' @return A list with \code{trait_name}, \code{suffix_type}, and optionally
#'   \code{census_id}, or NULL if no pivot pattern matches.
#' @keywords internal
#' @noRd
.parse_pivot_pattern <- function(col_name) {

  # Order matters: most specific patterns first


  # issue_agg_{trait}_census_{name}
  # The census name part can contain underscores, digits, letters
  if (grepl("^issue_agg_.+_.+$", col_name)) {
    # Try to match with a known census suffix first
    m <- regmatches(col_name, regexec("^issue_agg_(.+)_([^_]+)$", col_name))[[1]]
    if (length(m) == 3) {
      return(list(trait_name = m[2], suffix_type = "issue_census", census_id = m[3]))
    }
  }

  # issue_agg_{trait}
  if (grepl("^issue_agg_", col_name)) {
    trait <- sub("^issue_agg_", "", col_name)
    return(list(trait_name = trait, suffix_type = "issue"))
  }

  # ids_agg_{trait}_{census_name}
  if (grepl("^ids_agg_.+_.+$", col_name)) {
    m <- regmatches(col_name, regexec("^ids_agg_(.+)_([^_]+)$", col_name))[[1]]
    if (length(m) == 3) {
      return(list(trait_name = m[2], suffix_type = "ids_census", census_id = m[3]))
    }
  }

  # ids_agg_{trait}
  if (grepl("^ids_agg_", col_name)) {
    trait <- sub("^ids_agg_", "", col_name)
    return(list(trait_name = trait, suffix_type = "ids"))
  }

  # char_{trait}_{census_name} — census name is the last segment
  if (grepl("^char_.+_.+$", col_name)) {
    m <- regmatches(col_name, regexec("^char_(.+)_([^_]+)$", col_name))[[1]]
    if (length(m) == 3) {
      return(list(trait_name = m[2], suffix_type = "char_census", census_id = m[3]))
    }
  }

  # char_{trait}
  if (grepl("^char_", col_name)) {
    trait <- sub("^char_", "", col_name)
    return(list(trait_name = trait, suffix_type = "char"))
  }

  # {trait}_census_{N} — explicit _census_ separator
  if (grepl("_census_\\d+$", col_name)) {
    m <- regmatches(col_name, regexec("^(.+)_census_(\\d+)$", col_name))[[1]]
    if (length(m) == 3) {
      return(list(trait_name = m[2], suffix_type = "census", census_id = m[3]))
    }
  }

  # {trait}_{stat} where stat is mean/sd/n/min/max
  if (grepl("_(mean|sd|n|min|max)$", col_name)) {
    m <- regmatches(col_name, regexec("^(.+)_(mean|sd|n|min|max)$", col_name))[[1]]
    if (length(m) == 3) {
      return(list(trait_name = m[2], suffix_type = m[3]))
    }
  }

  # {trait}_0 or {trait}_1 — census pair suffixes
  if (grepl("_(0|1)$", col_name)) {
    m <- regmatches(col_name, regexec("^(.+)_(0|1)$", col_name))[[1]]
    if (length(m) == 3) {
      return(list(trait_name = m[2], suffix_type = paste0("pair_", m[3])))
    }
  }

  # No pattern matched
  NULL
}


#' Print method for column_documentation objects
#'
#' @param x A column_documentation object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.column_documentation <- function(x, ...) {

  style <- attr(x, "style")
  cli::cli_h1("Column Documentation")

  if (!is.null(style)) {
    cli::cli_text("Output style: {.strong {style}}")
  }
  cli::cli_text("")

  for (table_name in names(x)) {
    tbl <- x[[table_name]]
    if (!is.data.frame(tbl)) next

    cli::cli_h2("{table_name}")
    cli::cli_text("  {nrow(tbl)} columns documented")
    cli::cli_text("")

    for (i in seq_len(nrow(tbl))) {
      row <- tbl[i, ]
      col_display <- row$column_name
      if (nzchar(row$original_name)) {
        col_display <- paste0(col_display, " (was: ", row$original_name, ")")
      }
      cli::cli_text("  {.strong {col_display}}")
      cli::cli_text("    {row$description}")
      if (nzchar(row$unit)) {
        cli::cli_text("    Unit: {row$unit}")
      }
      if (nzchar(row$notes)) {
        cli::cli_text("    {.emph {row$notes}}")
      }
      cli::cli_text("")
    }
  }

  invisible(x)
}
