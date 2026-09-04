# =============================================================================
# BUILDING THE PLOT QUERY
# =============================================================================
#
# `query_plots()` turns its filter arguments into SQL conditions over
# `data_liste_plots` and runs them as a single SELECT. Each argument is
# translated by one `.plot_condition_*()` below, which returns that argument's
# condition(s) as a character vector: none when the argument is `NULL`, and the
# unsatisfiable `FALSE` when its value matched nothing in the database.
#
# Returning `FALSE` rather than no condition is deliberate. A filter whose
# value matched nothing must return no plots; dropping the condition instead
# silently widens the query, so asking for a country that does not exist would
# return every plot the user is allowed to see and read as success.
#
# The conditions are assembled once by `.assemble_plot_query()`, so the database
# is touched exactly as often as before: the lookups that resolve names to ids,
# and then one SELECT.

#' A condition no row can satisfy
#'
#' @keywords internal
#' @noRd
.sql_impossible <- function() "FALSE"

#' Conditions selecting plots by country
#'
#' @param country Character vector of country name(s).
#' @param con A DBI connection or pool.
#' @param interactive Logical. If `TRUE`, resolve the names through
#'   `.link_table()` fuzzy matching.
#' @return A character vector of SQL conditions.
#' @keywords internal
#' @noRd
.plot_condition_country <- function(country, con, interactive = FALSE) {

  if (is.null(country)) return(character(0))

  if (interactive) {

    linked_data <- .verbose_output(.link_table(
      data_stand = dplyr::tibble(country = country),
      column_searched = "country",
      column_name = "country",
      id_field = "id_country",
      id_table_name = "id_country",
      db_connection = con,
      table_name = "table_countries"
    ))

    keep <- !is.na(linked_data$id_country) & linked_data$id_country != 0
    country_ids <- linked_data$id_country[keep]

    if (length(country_ids) == 0) {
      cli::cli_alert_warning("No valid countries selected")
      return(.sql_impossible())
    }

  } else {

    countries_tbl <- dplyr::collect(try_open_postgres_table("table_countries", con))
    countries_tbl <-
      countries_tbl[tolower(countries_tbl$country) %in% tolower(country), , drop = FALSE]

    if (nrow(countries_tbl) == 0) {
      cli::cli_alert_warning("No countries found matching: {paste(country, collapse = ', ')}")
      cli::cli_alert_info("Tip: Use interactive = TRUE for fuzzy matching")
      return(.sql_impossible())
    }

    country_ids <- countries_tbl$id_country
  }

  as.character(glue::glue_sql(
    "id_country IN ({ids*})", ids = country_ids, .con = con
  ))
}

#' Conditions selecting plots by name
#'
#' A single name is matched as a substring unless `exact_match` is `TRUE`;
#' several names are always matched exactly, a list of names being far more
#' often a list of plots than a list of patterns.
#'
#' @param plot_name Character vector of plot name(s).
#' @param con A DBI connection or pool.
#' @param interactive Logical. If `TRUE`, resolve the names through
#'   `.link_table()` fuzzy matching.
#' @param exact_match Logical. If `TRUE`, match names exactly.
#' @return A character vector of SQL conditions.
#' @keywords internal
#' @noRd
.plot_condition_plot_name <- function(plot_name, con, interactive = FALSE,
                                      exact_match = FALSE) {

  if (is.null(plot_name)) return(character(0))

  if (interactive) {

    linked_data <- .verbose_output(.link_table(
      data_stand = dplyr::tibble(plot_name = plot_name),
      column_searched = "plot_name",
      column_name = "plot_name",
      id_field = "id_liste_plots",
      id_table_name = "id_liste_plots",
      db_connection = con,
      table_name = "data_liste_plots"
    ))

    keep <- !is.na(linked_data$id_liste_plots) & linked_data$id_liste_plots != 0
    plot_ids <- linked_data$id_liste_plots[keep]

    if (length(plot_ids) == 0) {
      cli::cli_alert_warning("No valid plots selected")
      return(.sql_impossible())
    }

    return(as.character(glue::glue_sql(
      "id_liste_plots IN ({ids*})", ids = plot_ids, .con = con
    )))
  }

  if (exact_match || length(plot_name) > 1) {
    as.character(glue::glue_sql(
      "LOWER(plot_name) IN ({names*})", names = tolower(plot_name), .con = con
    ))
  } else {
    as.character(glue::glue_sql(
      "LOWER(plot_name) LIKE LOWER({pattern})",
      pattern = paste0("%", plot_name, "%"), .con = con
    ))
  }
}

#' Conditions selecting plots by method
#'
#' Method names are resolved to `methodslist` ids first, so the plots query
#' itself stays a comparison of integers.
#'
#' @param method Character vector of method name(s).
#' @param con A DBI connection or pool.
#' @param interactive Logical. If `TRUE`, resolve the names through
#'   `.link_table()` fuzzy matching.
#' @return A character vector of SQL conditions.
#' @keywords internal
#' @noRd
.plot_condition_method <- function(method, con, interactive = FALSE) {

  if (is.null(method)) return(character(0))

  if (interactive) {

    linked_data <- .verbose_output(.link_table(
      data_stand = dplyr::tibble(method = method),
      column_searched = "method",
      column_name = "method",
      id_field = "id_method",
      id_table_name = "id_method",
      db_connection = con,
      table_name = "methodslist"
    ))

    keep <- !is.na(linked_data$id_method) & linked_data$id_method != 0
    method_ids <- linked_data$id_method[keep]

    if (length(method_ids) == 0) {
      cli::cli_alert_warning("No valid methods selected")
      return(.sql_impossible())
    }

  } else {

    patterns <- lapply(method, function(x) {
      glue::glue_sql(
        "LOWER(method) LIKE LOWER({pattern})",
        pattern = paste0("%", x, "%"), .con = con
      )
    })

    query_method <- glue::glue_sql(
      "SELECT id_method, method FROM methodslist WHERE {DBI::SQL(glue::glue_collapse(patterns, sep = ' OR '))}",
      .con = con
    )

    methods_found <- func_try_fetch(con = con, sql = query_method)

    if (nrow(methods_found) == 0) {
      cli::cli_alert_warning("No methods found matching: {paste(method, collapse = ', ')}")
      cli::cli_alert_info("Tip: Use interactive = TRUE for fuzzy matching")
      return(.sql_impossible())
    }

    cli::cli_alert_info("Using methods: {paste(methods_found$method, collapse = ', ')}")

    method_ids <- methods_found$id_method
  }

  as.character(glue::glue_sql(
    "id_method IN ({ids*})", ids = method_ids, .con = con
  ))
}

#' Conditions selecting plots by locality
#'
#' Always a substring match: locality names are free text, so a value is a hint
#' rather than a key. Several localities are OR'ed into one condition.
#'
#' @param locality_name Character vector of locality name(s).
#' @param con A DBI connection or pool.
#' @return A character vector of SQL conditions.
#' @keywords internal
#' @noRd
.plot_condition_locality <- function(locality_name, con) {

  if (is.null(locality_name)) return(character(0))

  parts <- lapply(locality_name, function(loc) {
    glue::glue_sql(
      "LOWER(locality_name) LIKE LOWER({pattern})",
      pattern = paste0("%", loc, "%"), .con = con
    )
  })

  if (length(parts) == 1) {
    as.character(parts[[1]])
  } else {
    paste0("(", paste(parts, collapse = " OR "), ")")
  }
}

#' Assemble plot conditions into a query
#'
#' @param conditions Character vector of SQL conditions, possibly empty.
#' @param con A DBI connection or pool.
#' @param operator Character. Join operator between conditions, `"AND"`
#'   (default) or `"OR"`.
#' @return A `SQL` query object selecting from `data_liste_plots`.
#' @keywords internal
#' @noRd
.assemble_plot_query <- function(conditions, con, operator = "AND") {

  base_query <- "SELECT * FROM data_liste_plots"

  if (length(conditions) == 0) {
    return(glue::glue_sql("{DBI::SQL(base_query)}", .con = con))
  }

  operator <- match.arg(toupper(operator), c("AND", "OR"))

  if (operator == "OR") {
    cli::cli_alert_info("Using OR operator between filter conditions")
  }

  where_clause <- paste(conditions, collapse = paste0(" ", operator, " "))

  glue::glue_sql(
    "{DBI::SQL(base_query)} WHERE {DBI::SQL(where_clause)}", .con = con
  )
}

#' The query behind `query_plots()`
#'
#' Translates the filter arguments of `query_plots()` into one SELECT over
#' `data_liste_plots`.
#'
#' @param con A DBI connection or pool.
#' @param country,plot_name,method,locality_name Filter values, each `NULL`
#'   when not asked for.
#' @param feature_filters Named list of plot features, see
#'   `plot_feature_filters()`.
#' @param interactive Logical. If `TRUE`, resolve names through
#'   `.link_table()` fuzzy matching.
#' @param exact_match Logical. If `TRUE`, match values exactly rather than as
#'   substrings.
#' @param operator Character. Join operator between conditions.
#' @return A `SQL` query object.
#' @keywords internal
#' @noRd
.plot_filter_query <- function(con,
                               country = NULL,
                               plot_name = NULL,
                               method = NULL,
                               locality_name = NULL,
                               feature_filters = NULL,
                               interactive = FALSE,
                               exact_match = FALSE,
                               operator = "AND") {

  conditions <- c(
    .plot_condition_country(country, con, interactive = interactive),
    .plot_condition_plot_name(plot_name, con, exact_match = exact_match),
    .plot_condition_method(method, con, interactive = interactive),
    .plot_condition_locality(locality_name, con),
    .feature_conditions(feature_filters, con, exact_match = exact_match)
  )

  .assemble_plot_query(conditions, con, operator = operator)
}

#' Add the readable value of the plot lookup ids
#'
#' `data_liste_plots` stores country and method as ids; the caller wants the
#' names. A lookup table that cannot be read is a warning rather than an error,
#' since the plots themselves are still worth returning.
#'
#' @param plots A data frame of plots.
#' @param con A DBI connection or pool.
#' @return `plots`, with `country` and `method` joined in where available.
#' @keywords internal
#' @noRd
.enrich_plot_metadata <- function(plots, con) {

  metadata_tables <- list(
    id_country = list(table = "table_countries", keep = "country"),
    id_method  = list(table = "methodslist",     keep = "method")
  )

  for (id_col in names(metadata_tables)) {

    if (!id_col %in% colnames(plots)) next

    meta_info <- metadata_tables[[id_col]]

    meta_tbl <- tryCatch({
      try_open_postgres_table(meta_info$table, con) %>%
        dplyr::select(dplyr::all_of(c(id_col, meta_info$keep))) %>%
        dplyr::collect()
    }, error = function(e) {
      cli::cli_alert_warning("Could not fetch {meta_info$table}: {e$message}")
      NULL
    })

    if (is.null(meta_tbl)) next

    # Columns already present are left alone rather than duplicated.
    keep_cols <- setdiff(meta_info$keep, names(plots))
    if (length(keep_cols) == 0) next

    meta_tbl <- dplyr::select(meta_tbl, dplyr::all_of(c(id_col, keep_cols)))
    plots <- dplyr::left_join(plots, meta_tbl, by = id_col)
  }

  plots
}

#' Add readable citation info for a plot's data source
#'
#' `data_liste_plots` stores the source dataset as `id_citation`; the caller
#' wants the readable bibliographic fields. Columns are prefixed with
#' `citation_` (except `citation_key`, kept bare like `fetch_taxa_trait_measurements()`
#' does for taxon-level trait citations) so they cannot collide with plot
#' columns. A lookup table that cannot be read is a warning rather than an
#' error, since the plots themselves are still worth returning.
#'
#' `query_plots()` can reach this through more than one path for the same
#' plots (e.g. `.fetch_plots_by_ids()` calls it, and the shared flow calls it
#' again downstream) - any citation columns already present are dropped
#' before rejoining, the same idiom `.link_metadata_tables()` uses for
#' `country`/`method`, so repeat calls stay idempotent instead of producing
#' `.x`/`.y` duplicates.
#'
#' @param plots A data frame of plots.
#' @param con A DBI connection or pool.
#' @return `plots`, with `citation_key`, `citation_authors`, `citation_year`,
#'   `citation_title`, `citation_journal`, `citation_doi`, and
#'   `citation_dataset_name` joined in where `id_citation` is set.
#' @keywords internal
#' @noRd
.enrich_plot_citation <- function(plots, con) {

  if (!"id_citation" %in% colnames(plots)) return(plots)

  citation_cols <- c("citation_key", "citation_authors", "citation_year",
                     "citation_title", "citation_journal", "citation_doi",
                     "citation_dataset_name")

  cit_tbl <- tryCatch({
    try_open_postgres_table("table_citations", con) %>%
      dplyr::select(
        id_citation,
        citation_key,
        citation_authors      = authors,
        citation_year         = year,
        citation_title        = title,
        citation_journal      = journal,
        citation_doi          = doi,
        citation_dataset_name = dataset_name
      ) %>%
      dplyr::collect()
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch table_citations: {e$message}")
    NULL
  })

  if (is.null(cit_tbl)) return(plots)

  plots <- plots %>% dplyr::select(-dplyr::any_of(citation_cols))

  dplyr::left_join(plots, cit_tbl, by = "id_citation")
}

#' Fetch plots by their identifiers
#'
#' @param plot_ids Integer vector of `data_liste_plots.id_liste_plots`.
#' @param con A DBI connection or pool.
#' @return A tibble of plots, enriched with country, method, and citation.
#' @keywords internal
#' @noRd
.fetch_plots_by_ids <- function(plot_ids, con) {

  if (is.null(plot_ids) || length(plot_ids) == 0) return(dplyr::tibble())

  cli::cli_alert_info("Fetching {length(plot_ids)} plots by ID")

  sql <- glue::glue_sql(
    "SELECT * FROM data_liste_plots WHERE id_liste_plots IN ({ids*})",
    ids = plot_ids, .con = con
  )

  plots <- func_try_fetch(con = con, sql = sql) %>%
    dplyr::select(-dplyr::any_of("id_old"))

  plots <- .enrich_plot_metadata(plots, con)
  .enrich_plot_citation(plots, con)
}
