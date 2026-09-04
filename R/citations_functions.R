# Citations
#
# Reading and maintaining `table_citations` and the trait rows that point at it.
# The one-shot migration that created the table lives in
# inst/migrations/add_citations_table.R.

# =============================================================================
# CRUD functions
# =============================================================================

#' Query citations from table_citations
#'
#' Returns rows from `table_citations`, optionally filtered by ID, citation key,
#' dataset name, or a free-text pattern matched against `citation_key`, `authors`,
#' `title`, and `dataset_name`.
#'
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param ids Integer vector of `id_citation` values to retrieve.
#' @param keys Character vector of `citation_key` values to retrieve.
#' @param dataset_names Character vector of `dataset_name` values to filter on.
#' @param pattern Character string. Case-insensitive substring matched against
#'   `citation_key`, `authors`, `title`, and `dataset_name`.
#'
#' @return A data frame of matching rows, or all rows when no filter is supplied.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # All citations
#' query_citations(con)
#'
#' # By key
#' query_citations(con, keys = "TRY_v6")
#'
#' # Free-text search
#' query_citations(con, pattern = "TRY")
#' }
#'
#' @export
query_citations <- function(con = NULL,
                            ids           = NULL,
                            keys          = NULL,
                            dataset_names = NULL,
                            pattern       = NULL) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  # Build WHERE clauses
  clauses <- character(0)

  if (!is.null(ids)) {
    ids_sql <- paste(as.integer(ids), collapse = ", ")
    clauses <- c(clauses, paste0("id_citation IN (", ids_sql, ")"))
  }

  if (!is.null(keys)) {
    keys_sql <- paste0("'", gsub("'", "''", keys), "'", collapse = ", ")
    clauses <- c(clauses, paste0("citation_key IN (", keys_sql, ")"))
  }

  if (!is.null(dataset_names)) {
    ds_sql <- paste0("'", gsub("'", "''", dataset_names), "'", collapse = ", ")
    clauses <- c(clauses, paste0("dataset_name IN (", ds_sql, ")"))
  }

  if (!is.null(pattern)) {
    p <- gsub("'", "''", pattern)
    clauses <- c(clauses, paste0(
      "(citation_key ILIKE '%", p, "%'",
      " OR authors ILIKE '%", p, "%'",
      " OR title ILIKE '%", p, "%'",
      " OR dataset_name ILIKE '%", p, "%')"
    ))
  }

  sql <- if (length(clauses) > 0) {
    paste("SELECT * FROM table_citations WHERE", paste(clauses, collapse = " AND "))
  } else {
    "SELECT * FROM table_citations ORDER BY id_citation"
  }

  result <- func_try_fetch(con = actual_con, sql = sql)

  cli::cli_alert_info("{nrow(result)} citation(s) found")
  result
}

#' Add one or more citations to table_citations
#'
#' Inserts new rows into `table_citations`. Rows whose `citation_key` already
#' exists in the database are skipped with a warning.
#'
#' @param new_data Data frame with citation fields. The column `citation_key`
#'   is mandatory. Optional columns: `authors`, `year`, `title`, `journal`,
#'   `volume`, `pages`, `doi`, `url`, `dataset_name`, `notes`.
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param interactive Logical. If TRUE (default), shows a preview and asks for
#'   confirmation before inserting.
#'
#' @return Invisible data frame of actually inserted rows, or NULL if nothing
#'   was inserted.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' add_citation(
#'   data.frame(
#'     citation_key = "TRY_v6",
#'     authors      = "Kattge et al.",
#'     year         = 2020,
#'     title        = "TRY plant trait database - enhanced coverage and open access",
#'     journal      = "Global Change Biology",
#'     doi          = "10.1111/gcb.14904",
#'     dataset_name = "TRY"
#'   ),
#'   con = con
#' )
#' }
#'
#' @export
add_citation <- function(new_data, con = NULL, interactive = TRUE) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  if (!"citation_key" %in% names(new_data)) {
    cli::cli_abort("new_data must contain a 'citation_key' column")
  }

  # Add modification date
  today <- Sys.Date()
  new_data$date_modif_d <- as.integer(format(today, "%d"))
  new_data$date_modif_m <- as.integer(format(today, "%m"))
  new_data$date_modif_y <- as.integer(format(today, "%Y"))

  # Check for existing keys
  existing <- tryCatch({
    DBI::dbGetQuery(actual_con,
      "SELECT citation_key FROM table_citations"
    )$citation_key
  }, error = function(e) character(0))

  duplicates <- new_data$citation_key[new_data$citation_key %in% existing]

  if (length(duplicates) > 0) {
    cli::cli_alert_warning(
      "Skipping {length(duplicates)} citation(s) with existing key(s): {paste(duplicates, collapse=', ')}"
    )
    new_data <- new_data[!new_data$citation_key %in% duplicates, ]
  }

  if (nrow(new_data) == 0) {
    cli::cli_alert_info("No new citations to insert")
    return(invisible(NULL))
  }

  # Keep only valid columns
  valid_cols <- c("citation_key", "authors", "year", "title", "journal",
                  "volume", "pages", "doi", "url", "dataset_name", "notes",
                  "date_modif_d", "date_modif_m", "date_modif_y")
  new_data <- new_data[, intersect(names(new_data), valid_cols), drop = FALSE]

  cli::cli_h3("Citations to insert:")
  print(new_data)

  do_insert <- if (interactive) {
    choose_prompt(message = paste0("Confirm inserting ", nrow(new_data), " citation(s)?"))
  } else {
    TRUE
  }

  if (do_insert) {
    tryCatch({
      DBI::dbWriteTable(actual_con, "table_citations", new_data,
                        append = TRUE, row.names = FALSE)
      cli::cli_alert_success("{nrow(new_data)} citation(s) inserted")
    }, error = function(e) {
      cli::cli_alert_danger("Insert failed: {e$message}")
      stop(e)
    })
  } else {
    cli::cli_alert_info("Insert cancelled")
    return(invisible(NULL))
  }

  invisible(new_data)
}

#' Update fields of an existing citation
#'
#' Updates one or more columns of a single row in `table_citations`, identified
#' by its `id_citation`.
#'
#' @param id_citation Integer. The `id_citation` of the row to update.
#' @param fields Named list of field-value pairs to update. Names must be valid
#'   column names of `table_citations` (excluding `id_citation`).
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param execute Logical. If FALSE (default), shows changes without applying
#'   them. Set to TRUE to apply.
#'
#' @return Invisible TRUE on success, or invisible FALSE if not executed.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Preview
#' update_citation(1, list(doi = "10.1111/gcb.14904", url = "https://..."), con)
#'
#' # Apply
#' update_citation(1, list(doi = "10.1111/gcb.14904"), con, execute = TRUE)
#' }
#'
#' @export
update_citation <- function(id_citation, fields, con = NULL, execute = FALSE) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  if (!is.numeric(id_citation) || length(id_citation) != 1) {
    cli::cli_abort("id_citation must be a single integer")
  }

  valid_cols <- c("citation_key", "authors", "year", "title", "journal",
                  "volume", "pages", "doi", "url", "dataset_name", "notes",
                  "date_modif_d", "date_modif_m", "date_modif_y")

  invalid <- setdiff(names(fields), valid_cols)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid field(s): {paste(invalid, collapse=', ')}")
  }

  # Always update modification date
  today <- Sys.Date()
  fields$date_modif_d <- as.integer(format(today, "%d"))
  fields$date_modif_m <- as.integer(format(today, "%m"))
  fields$date_modif_y <- as.integer(format(today, "%Y"))

  # Fetch current values for comparison
  current <- tryCatch({
    DBI::dbGetQuery(actual_con,
      paste0("SELECT * FROM table_citations WHERE id_citation = ", as.integer(id_citation))
    )
  }, error = function(e) {
    cli::cli_abort("Could not fetch current record: {e$message}")
  })

  if (nrow(current) == 0) {
    cli::cli_abort("No citation found with id_citation = {id_citation}")
  }

  # Show comparison
  cli::cli_h3("Proposed changes for id_citation = {id_citation}:")
  for (nm in setdiff(names(fields), c("date_modif_d", "date_modif_m", "date_modif_y"))) {
    old_val <- if (nm %in% names(current)) current[[nm]][1] else NA
    cli::cli_alert_info("{.field {nm}}: {.val {old_val}} -> {.val {fields[[nm]]}}")
  }

  if (!execute) {
    cli::cli_alert_warning("DRY RUN - no changes applied (rerun with execute = TRUE)")
    return(invisible(FALSE))
  }

  # Build SET clause
  set_parts <- mapply(function(col, val) {
    if (is.na(val) || is.null(val)) {
      paste0(col, " = NULL")
    } else if (is.character(val)) {
      paste0(col, " = '", gsub("'", "''", val), "'")
    } else {
      paste0(col, " = ", val)
    }
  }, names(fields), fields, SIMPLIFY = TRUE)

  sql <- paste0(
    "UPDATE table_citations SET ",
    paste(set_parts, collapse = ", "),
    " WHERE id_citation = ", as.integer(id_citation)
  )

  tryCatch({
    DBI::dbExecute(actual_con, sql)
    cli::cli_alert_success("Citation {id_citation} updated")
  }, error = function(e) {
    cli::cli_alert_danger("Update failed: {e$message}")
    stop(e)
  })

  invisible(TRUE)
}

# =============================================================================
# Plot-level citations
# =============================================================================
#
# `data_liste_plots.id_citation` is the plot-level counterpart of
# `taxa_traits_measures.id_citation`: one citation per plot recording which
# dataset/study the plot's inventory data comes from. See
# inst/migrations/add_plot_citations.R for the schema change.

#' Export plots for citation backfill
#'
#' Exports `data_liste_plots` to a data frame (optionally saved as Excel)
#' with a blank `id_citation` column ready to be filled in manually. Once
#' filled, pass the result to `apply_plot_citation_backfill()`.
#'
#' The export contains only the columns needed to identify each plot and
#' assign a citation: `id_liste_plots`, `plot_name`, `country`, `method`, and
#' the current `id_citation` (NA where unset).
#'
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param file Path to an `.xlsx` file to write. If NULL (default), returns
#'   the data frame without writing.
#' @param only_missing Logical. If TRUE (default), export only rows where
#'   `id_citation IS NULL`.
#'
#' @return A data frame with columns `id_liste_plots`, `plot_name`,
#'   `country`, `method`, `id_citation`.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Return as data frame
#' df <- export_plots_for_citation_backfill(con)
#'
#' # Write to Excel for manual editing
#' export_plots_for_citation_backfill(con, file = "plots_to_cite.xlsx")
#' }
#'
#' @export
export_plots_for_citation_backfill <- function(con = NULL,
                                               file = NULL,
                                               only_missing = TRUE) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  where_sql <- if (only_missing) "WHERE lp.id_citation IS NULL" else ""

  result <- DBI::dbGetQuery(actual_con, paste0("
    SELECT
      lp.id_liste_plots,
      lp.plot_name,
      tc_country.country,
      ml.method,
      lp.id_citation
    FROM data_liste_plots lp
    LEFT JOIN table_countries tc_country ON lp.id_country = tc_country.id_country
    LEFT JOIN methodslist ml ON lp.id_method = ml.id_method
    ", where_sql, "
    ORDER BY lp.plot_name
  "))

  cli::cli_alert_info(
    "{nrow(result)} plot(s) exported ({if (only_missing) 'missing citation only' else 'all rows'})"
  )

  if (!is.null(file)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      cli::cli_abort("Package 'openxlsx' required to write Excel. Install with: install.packages('openxlsx')")
    }
    citations <- tryCatch(
      DBI::dbGetQuery(actual_con,
        "SELECT id_citation, citation_key, authors, year, dataset_name
         FROM table_citations ORDER BY citation_key"),
      error = function(e) data.frame()
    )
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "plots")
    openxlsx::writeData(wb, "plots", result)
    if (nrow(citations) > 0) {
      openxlsx::addWorksheet(wb, "citations_reference")
      openxlsx::writeData(wb, "citations_reference", citations)
    }
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    cli::cli_alert_success("Written to {.file {file}}")
    cli::cli_alert_info("Fill in the 'id_citation' column in the 'plots' sheet, then run apply_plot_citation_backfill()")
  }

  invisible(result)
}

#' Apply plot citation backfill from a manually filled data frame
#'
#' Takes a data frame (typically the output of
#' `export_plots_for_citation_backfill()` after manual editing) and updates
#' `id_citation` in `data_liste_plots` for each row where `id_citation` is not
#' NA. Rows with NA are skipped.
#'
#' @param data Data frame with at minimum two columns: `id_liste_plots`
#'   (integer, primary key) and `id_citation` (integer, FK to
#'   `table_citations`). Additional columns are ignored.
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param execute Logical. If FALSE (default), shows a preview of what would
#'   be updated without modifying the database.
#' @param batch_size Integer. Number of rows per UPDATE batch (default 1000).
#'
#' @return Invisible integer: number of rows updated.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Export, fill manually, apply
#' df <- export_plots_for_citation_backfill(con)
#' # ... fill df$id_citation ...
#' apply_plot_citation_backfill(df, con = con)              # dry run
#' apply_plot_citation_backfill(df, con = con, execute = TRUE)
#' }
#'
#' @export
apply_plot_citation_backfill <- function(data,
                                         con        = NULL,
                                         execute    = FALSE,
                                         batch_size = 1000L) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  required <- c("id_liste_plots", "id_citation")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("data must contain columns: {paste(missing_cols, collapse=', ')}")
  }

  # Keep only rows where id_citation is filled in
  to_update <- data[!is.na(data$id_citation), c("id_liste_plots", "id_citation")]
  to_update$id_liste_plots <- as.integer(to_update$id_liste_plots)
  to_update$id_citation    <- as.integer(to_update$id_citation)

  n_skip <- nrow(data) - nrow(to_update)

  cli::cli_alert_info("{nrow(to_update)} plot(s) to update, {n_skip} skipped (NA id_citation)")

  if (nrow(to_update) == 0) {
    cli::cli_alert_info("Nothing to update")
    return(invisible(0L))
  }

  # Preview: citation breakdown
  cit_summary <- as.data.frame(table(to_update$id_citation))
  names(cit_summary) <- c("id_citation", "n_plots")

  cit_keys <- tryCatch(
    DBI::dbGetQuery(actual_con,
      "SELECT id_citation, citation_key FROM table_citations"),
    error = function(e) data.frame(id_citation = integer(), citation_key = character())
  )
  cit_summary$id_citation <- as.integer(as.character(cit_summary$id_citation))
  cit_summary <- merge(cit_summary, cit_keys, by = "id_citation", all.x = TRUE)

  cli::cli_h3("Citation assignment preview:")
  for (i in seq_len(nrow(cit_summary))) {
    r <- cit_summary[i, ]
    key <- if (!is.na(r$citation_key)) r$citation_key else paste0("id=", r$id_citation)
    cli::cli_alert_info("  {key}: {r$n_plots} plot(s)")
  }

  if (!execute) {
    cli::cli_alert_warning("DRY RUN - no changes applied (rerun with execute = TRUE)")
    return(invisible(nrow(to_update)))
  }

  # Batch UPDATE
  batches <- split(to_update, ceiling(seq_len(nrow(to_update)) / batch_size))
  n_updated <- 0L

  cli::cli_progress_bar("Updating batches", total = length(batches))

  for (batch in batches) {
    values_sql <- paste(
      apply(batch, 1, function(r) paste0("(", r["id_liste_plots"], ",", r["id_citation"], ")")),
      collapse = ", "
    )
    sql <- paste0(
      "UPDATE data_liste_plots AS lp
       SET id_citation = v.id_citation
       FROM (VALUES ", values_sql, ") AS v(id_liste_plots, id_citation)
       WHERE lp.id_liste_plots = v.id_liste_plots"
    )
    tryCatch({
      n_updated <- n_updated + DBI::dbExecute(actual_con, sql)
    }, error = function(e) {
      cli::cli_alert_danger("Batch failed: {e$message}")
      stop(e)
    })
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success("{n_updated} plot(s) updated")

  invisible(n_updated)
}

#' Build a plot data sources summary table (citations × country pivot)
#'
#' Creates a wide pivot table showing how many plots each data source
#' contributes per country. Used by \code{\link{query_plots}} to populate the
#' `plot_sources` element of its result - the plot-level counterpart of
#' \code{\link{build_data_sources_table}}, which does the same thing for
#' taxon-level trait citations.
#'
#' @param plots_raw Data frame of plots enriched with citation info, as
#'   returned internally by `query_plots()` before individual extraction.
#'   Must contain columns \code{id_liste_plots} and \code{citation_key}.
#'   \code{country}, when present, becomes the pivoted dimension.
#'
#' @return A data frame with one row per citation (rows) and, when
#'   \code{country} is available, one column per country (plot counts),
#'   preceded by citation metadata columns and an \code{n_plots} column.
#'   Returns \code{NULL} when \code{plots_raw} is \code{NULL}, empty, or lacks
#'   the required columns.
#'
#' @export
build_plot_data_sources_table <- function(plots_raw) {
  if (is.null(plots_raw) || !is.data.frame(plots_raw) || nrow(plots_raw) == 0)
    return(NULL)
  if (!all(c("id_liste_plots", "citation_key") %in% names(plots_raw)))
    return(NULL)

  plots_raw <- plots_raw %>%
    dplyr::distinct(.data$id_liste_plots, .keep_all = TRUE) %>%
    dplyr::mutate(
      citation_key = dplyr::if_else(
        is.na(.data$citation_key) | .data$citation_key == "",
        "(no citation)", .data$citation_key
      )
    )

  citation_meta_cols <- intersect(
    c("citation_key", "citation_authors", "citation_year",
      "citation_title", "citation_dataset_name"),
    names(plots_raw)
  )

  citation_meta <- plots_raw %>%
    dplyr::select(dplyr::all_of(citation_meta_cols)) %>%
    dplyr::distinct()

  n_plots_summary <- plots_raw %>%
    dplyr::group_by(.data$citation_key) %>%
    dplyr::summarise(n_plots = dplyr::n(), .groups = "drop")

  pivot <- n_plots_summary
  if (length(citation_meta_cols) > 1) {
    pivot <- citation_meta %>%
      dplyr::left_join(pivot, by = "citation_key")
  }

  if ("country" %in% names(plots_raw)) {
    country_pivot <- plots_raw %>%
      dplyr::group_by(.data$citation_key, .data$country) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(
        id_cols     = "citation_key",
        names_from  = "country",
        values_from = "n",
        values_fill = 0L
      )

    pivot <- pivot %>%
      dplyr::left_join(country_pivot, by = "citation_key")
  }

  country_cols <- setdiff(names(pivot), c(citation_meta_cols, "n_plots"))
  col_order    <- c(citation_meta_cols, "n_plots", country_cols)
  pivot %>% dplyr::select(dplyr::any_of(col_order))
}

# =============================================================================
# Backfill helpers
# =============================================================================

#' Export taxa trait measurements for citation backfill
#'
#' Exports `taxa_traits_measures` to a data frame (optionally saved as Excel)
#' with a blank `id_citation` column ready to be filled in manually. Once
#' filled, pass the result to `apply_citation_backfill()`.
#'
#' The export contains only the columns needed to identify each row and assign
#' a citation: `id_trait_measures`, `idtax`, `fk_id_trait`, `basisofrecord`,
#' `measurementremarks`, and the current `id_citation` (NA where unset).
#' Trait names from `traitlist` are joined for readability.
#'
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param file Path to an `.xlsx` file to write. If NULL (default), returns
#'   the data frame without writing.
#' @param only_missing Logical. If TRUE (default), export only rows where
#'   `id_citation IS NULL`.
#'
#' @return A data frame with columns `id_trait_measures`, `idtax`, `trait`,
#'   `fk_id_trait`, `basisofrecord`, `measurementremarks`, `id_citation`.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Return as data frame
#' df <- export_taxa_traits_for_citation_backfill(con)
#'
#' # Write to Excel for manual editing
#' export_taxa_traits_for_citation_backfill(con, file = "traits_to_cite.xlsx")
#' }
#'
#' @export
export_taxa_traits_for_citation_backfill <- function(con = NULL,
                                                      file = NULL,
                                                      only_missing = TRUE) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  where_sql <- if (only_missing) "WHERE tm.id_citation IS NULL" else ""

  result <- DBI::dbGetQuery(actual_con, paste0("
    SELECT
      tm.id_trait_measures,
      tm.idtax,
      tl.trait,
      tm.fk_id_trait,
      tm.basisofrecord,
      tm.measurementremarks,
      tm.id_citation
    FROM taxa_traits_measures tm
    LEFT JOIN traitlist tl ON tm.fk_id_trait = tl.id_trait
    ", where_sql, "
    ORDER BY tl.trait, tm.idtax
  "))

  cli::cli_alert_info(
    "{nrow(result)} row(s) exported ({if (only_missing) 'missing citation only' else 'all rows'})"
  )

  if (!is.null(file)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      cli::cli_abort("Package 'openxlsx' required to write Excel. Install with: install.packages('openxlsx')")
    }
    # Also export available citations as a second sheet for reference
    citations <- tryCatch(
      DBI::dbGetQuery(actual_con,
        "SELECT id_citation, citation_key, authors, year, dataset_name
         FROM table_citations ORDER BY citation_key"),
      error = function(e) data.frame()
    )
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "traits")
    openxlsx::writeData(wb, "traits", result)
    if (nrow(citations) > 0) {
      openxlsx::addWorksheet(wb, "citations_reference")
      openxlsx::writeData(wb, "citations_reference", citations)
    }
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    cli::cli_alert_success("Written to {.file {file}}")
    cli::cli_alert_info("Fill in the 'id_citation' column in the 'traits' sheet, then run apply_citation_backfill()")
  }

  invisible(result)
}

#' Apply citation backfill from a manually filled data frame
#'
#' Takes a data frame (typically the output of
#' `export_taxa_traits_for_citation_backfill()` after manual editing) and
#' updates `id_citation` in `taxa_traits_measures` for each row where
#' `id_citation` is not NA. Rows with NA are skipped.
#'
#' @param data Data frame with at minimum two columns: `id_trait_measures`
#'   (integer, primary key) and `id_citation` (integer, FK to
#'   `table_citations`). Additional columns are ignored.
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param execute Logical. If FALSE (default), shows a preview of what would
#'   be updated without modifying the database.
#' @param batch_size Integer. Number of rows per UPDATE batch (default 1000).
#'
#' @return Invisible integer: number of rows updated.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Export, fill manually, apply
#' df <- export_taxa_traits_for_citation_backfill(con)
#' # ... fill df$id_citation ...
#' apply_citation_backfill(df, con = con)              # dry run
#' apply_citation_backfill(df, con = con, execute = TRUE)
#'
#' # Or from Excel
#' df <- readxl::read_excel("traits_to_cite.xlsx", sheet = "traits")
#' apply_citation_backfill(df, con = con, execute = TRUE)
#' }
#'
#' @export
apply_citation_backfill <- function(data,
                                    con        = NULL,
                                    execute    = FALSE,
                                    batch_size = 1000L) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  required <- c("id_trait_measures", "id_citation")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("data must contain columns: {paste(missing_cols, collapse=', ')}")
  }

  # Keep only rows where id_citation is filled in
  to_update <- data[!is.na(data$id_citation), c("id_trait_measures", "id_citation")]
  to_update$id_trait_measures <- as.integer(to_update$id_trait_measures)
  to_update$id_citation        <- as.integer(to_update$id_citation)

  n_skip <- nrow(data) - nrow(to_update)

  cli::cli_alert_info("{nrow(to_update)} row(s) to update, {n_skip} skipped (NA id_citation)")

  if (nrow(to_update) == 0) {
    cli::cli_alert_info("Nothing to update")
    return(invisible(0L))
  }

  # Preview: citation breakdown
  cit_summary <- as.data.frame(table(to_update$id_citation))
  names(cit_summary) <- c("id_citation", "n_rows")

  # Enrich with citation_key for readability
  cit_keys <- tryCatch(
    DBI::dbGetQuery(actual_con,
      "SELECT id_citation, citation_key FROM table_citations"),
    error = function(e) data.frame(id_citation = integer(), citation_key = character())
  )
  cit_summary$id_citation <- as.integer(as.character(cit_summary$id_citation))
  cit_summary <- merge(cit_summary, cit_keys, by = "id_citation", all.x = TRUE)

  cli::cli_h3("Citation assignment preview:")
  for (i in seq_len(nrow(cit_summary))) {
    r <- cit_summary[i, ]
    key <- if (!is.na(r$citation_key)) r$citation_key else paste0("id=", r$id_citation)
    cli::cli_alert_info("  {key}: {r$n_rows} row(s)")
  }

  if (!execute) {
    cli::cli_alert_warning("DRY RUN - no changes applied (rerun with execute = TRUE)")
    return(invisible(nrow(to_update)))
  }

  # Batch UPDATE
  batches <- split(to_update, ceiling(seq_len(nrow(to_update)) / batch_size))
  n_updated <- 0L

  cli::cli_progress_bar("Updating batches", total = length(batches))

  for (batch in batches) {
    # Build VALUES list for a single UPDATE ... FROM (VALUES ...) approach
    # Efficient: one SQL per batch instead of one per row
    values_sql <- paste(
      apply(batch, 1, function(r) paste0("(", r["id_trait_measures"], ",", r["id_citation"], ")")),
      collapse = ", "
    )
    sql <- paste0(
      "UPDATE taxa_traits_measures AS tm
       SET id_citation = v.id_citation
       FROM (VALUES ", values_sql, ") AS v(id_trait_measures, id_citation)
       WHERE tm.id_trait_measures = v.id_trait_measures"
    )
    tryCatch({
      n_updated <- n_updated + DBI::dbExecute(actual_con, sql)
    }, error = function(e) {
      cli::cli_alert_danger("Batch failed: {e$message}")
      stop(e)
    })
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success("{n_updated} row(s) updated")

  invisible(n_updated)
}

#' Build a data sources summary table (citations × traits pivot)
#'
#' Creates a wide pivot table showing how many measurements each data source
#' contributes per trait. Used by \code{\link{query_plots}} when
#' \code{extract_traits = TRUE} and by the query plots Shiny app to display
#' the "Data Sources" panel.
#'
#' @param traits_raw Long-format data frame returned by
#'   \code{query_taxa_traits(include_citation = TRUE, format = "long")}.
#'   Must contain columns \code{trait}, \code{citation_key}, and \code{idtax}.
#'
#' @return A data frame with one row per citation (rows) and one column per
#'   trait (measurement counts), preceded by citation metadata columns and a
#'   \code{n_taxa} column. Returns \code{NULL} when \code{traits_raw} is
#'   \code{NULL}, empty, or lacks the required columns.
#'
#' @export
build_data_sources_table <- function(traits_raw) {
  if (is.null(traits_raw) || !is.data.frame(traits_raw) || nrow(traits_raw) == 0)
    return(NULL)
  if (!all(c("trait", "idtax") %in% names(traits_raw)))
    return(NULL)

  if (!"citation_key" %in% names(traits_raw))
    return(NULL)

  traits_raw <- traits_raw %>%
    dplyr::mutate(
      citation_key = dplyr::if_else(
        is.na(.data$citation_key) | .data$citation_key == "",
        "(no citation)", .data$citation_key
      )
    )

  citation_meta_cols <- intersect(
    c("citation_key", "citation_authors", "citation_year",
      "citation_title", "citation_dataset_name"),
    names(traits_raw)
  )

  citation_meta <- traits_raw %>%
    dplyr::select(dplyr::all_of(citation_meta_cols)) %>%
    dplyr::distinct()

  pivot <- traits_raw %>%
    dplyr::group_by(.data$citation_key, .data$trait) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(
      id_cols     = "citation_key",
      names_from  = "trait",
      values_from = "n",
      values_fill = 0L
    )

  n_taxa_summary <- traits_raw %>%
    dplyr::group_by(.data$citation_key) %>%
    dplyr::summarise(n_taxa = dplyr::n_distinct(.data$idtax), .groups = "drop")

  pivot <- pivot %>%
    dplyr::left_join(n_taxa_summary, by = "citation_key")

  if (length(citation_meta_cols) > 1) {
    pivot <- citation_meta %>%
      dplyr::left_join(pivot, by = "citation_key")
  }

  trait_cols <- setdiff(names(pivot), c(citation_meta_cols, "n_taxa"))
  col_order  <- c(citation_meta_cols, "n_taxa", trait_cols)
  pivot %>% dplyr::select(dplyr::any_of(col_order))
}
