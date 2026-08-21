# =============================================================================
# FILTERING PLOTS BY THEIR FEATURES
# =============================================================================
#
# A plot carries three different kinds of value, stored in three different
# ways (see CLAUDE.md, "Plot Data Storage Architecture"):
#
#   flat column   data_liste_plots.<column>              plot_name, locality_name
#   lookup id     data_liste_plots.id_country/id_method  read as country/method
#   feature       a row of data_liste_sub_plots, typed by subplotype_list
#
# `query_plots()` has always been able to filter on the first two. The helpers
# below add the third: a feature is matched by a subquery over
# `data_liste_sub_plots`, so a plot is kept when *some* of its subplot records
# carry the value asked for.
#
# Only features that read as text can be filtered this way -- `character`
# features, whose value sits in `typevalue_char`, and lookup features such as
# `table_colnam`, whose value is the id of a lookup row held in `typevalue`.
# Numeric features (`census`, `ddlat`, ...) are measurements and are refused
# rather than silently matched as strings.

#' Is this valuetype one we can filter as text?
#' @keywords internal
#' @noRd
.is_filterable_valuetype <- function(valuetype) {
  !is.na(valuetype) & (valuetype == "character" | grepl("^table_", valuetype))
}

#' Feature types and their valuetypes, one row per feature name
#'
#' A feature name can appear on several `subplotype_list` rows. Filterable
#' valuetypes are kept in front of the deduplication so a name that is
#' filterable through any of its rows resolves to the filterable one.
#'
#' @keywords internal
#' @noRd
.fetch_feature_types <- function(con) {
  types <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT type, valuetype FROM subplotype_list WHERE type IS NOT NULL"
  )
  types <- types[order(!.is_filterable_valuetype(types$valuetype), types$type), , drop = FALSE]
  types[!duplicated(types$type), , drop = FALSE]
}

#' Check feature names against the database and return their valuetypes
#'
#' Errors on a name the database does not have, and on a feature that exists
#' but holds measurements. Both are user mistakes worth stopping for: a
#' silently empty result would read as "no plot has that value" rather than
#' "that is not a thing you can ask for".
#'
#' @param features Character vector of feature names.
#' @param con A DBI connection or pool.
#' @return A data frame with `type` and `valuetype`, in the order asked for.
#' @keywords internal
#' @noRd
.validate_feature_names <- function(features, con) {

  available <- .fetch_feature_types(con)

  unknown <- setdiff(features, available$type)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown plot feature: {.val {unknown}}.",
      i = "{.fn plot_feature_filters} lists the features that can be filtered."
    ))
  }

  spec <- available[match(features, available$type), , drop = FALSE]

  bad <- spec[!.is_filterable_valuetype(spec$valuetype), , drop = FALSE]
  if (nrow(bad) > 0) {
    detail <- paste0(bad$type, " (", bad$valuetype, ")", collapse = ", ")
    cli::cli_abort(c(
      "Only character and lookup features can be filtered by value.",
      x = "Not filterable: {detail}.",
      i = "{.fn plot_feature_filters} lists the features that are."
    ))
  }

  spec
}

#' Validate the `feature_filters` argument of [query_plots()]
#'
#' @param feature_filters A named list, names being feature types.
#' @param con A DBI connection or pool.
#' @return A data frame with `type` and `valuetype` for the named features.
#' @keywords internal
#' @noRd
.validate_feature_filters <- function(feature_filters, con) {

  if (!is.list(feature_filters)) {
    cli::cli_abort(c(
      "{.arg feature_filters} must be a named list.",
      x = "Got {.cls {class(feature_filters)[1]}}.",
      i = "For example {.code list(data_provider = \"IRD\")}."
    ))
  }

  nms <- names(feature_filters)
  if (is.null(nms) || any(is.na(nms)) || !all(nzchar(nms))) {
    cli::cli_abort(c(
      "Every element of {.arg feature_filters} must be named after a plot feature.",
      i = "For example {.code list(data_provider = \"IRD\")}."
    ))
  }

  dup <- unique(nms[duplicated(nms)])
  if (length(dup) > 0) {
    cli::cli_abort(c(
      "Each feature may appear only once in {.arg feature_filters}.",
      x = "Repeated: {.val {dup}}.",
      i = "Pass several values in one element instead."
    ))
  }

  .validate_feature_names(nms, con)
}

#' Where the readable values of a lookup feature live
#'
#' `table_colnam` is the only lookup valuetype in use. Anything else following
#' the same `table_<name>` convention is derived and then verified against the
#' schema, so an unmet convention fails loudly instead of building SQL against
#' columns that do not exist.
#'
#' @keywords internal
#' @noRd
.feature_lookup_spec <- function(valuetype, con) {

  known <- list(
    table_colnam = list(
      table = "table_colnam", id_column = "id_table_colnam", value_column = "colnam"
    )
  )
  if (!is.null(known[[valuetype]])) return(known[[valuetype]])

  spec <- list(
    table        = valuetype,
    id_column    = paste0("id_", valuetype),
    value_column = sub("^table_", "", valuetype)
  )

  fields <- tryCatch(DBI::dbListFields(con, spec$table), error = function(e) character(0))
  if (!all(c(spec$id_column, spec$value_column) %in% fields)) {
    cli::cli_abort(c(
      "Cannot resolve the values of lookup feature type {.val {valuetype}}.",
      i = "Expected table {.val {spec$table}} with columns {.field {spec$id_column}} and {.field {spec$value_column}}."
    ))
  }

  spec
}

#' Plot ids whose features satisfy a set of feature filters
#'
#' Used when [query_plots()] was given explicit ids and so never built a filter
#' query: the feature filters then narrow that set rather than being ignored.
#'
#' @param feature_filters A named list, names being feature types.
#' @param con A DBI connection or pool.
#' @param exact_match Logical, passed through to the builder.
#' @return An integer vector of `data_liste_plots.id_liste_plots`.
#' @keywords internal
#' @noRd
.plot_ids_matching_features <- function(feature_filters, con, exact_match = FALSE) {
  builder <- PlotFilterBuilder$new(con)
  builder$filter_features(feature_filters, exact_match = exact_match)
  res <- func_try_fetch(con = con, sql = builder$build())
  if (nrow(res) == 0) return(integer(0))
  as.integer(res$id_liste_plots)
}

#' Plot features that can be used as a filter
#'
#' @description
#' Lists the plot features [query_plots()] accepts in `feature_filters`: the
#' features whose value reads as text. A plot feature is a row of
#' `data_liste_sub_plots` typed by `subplotype_list`, not a column of
#' `data_liste_plots` -- see [subplot_list()] for every feature type, filterable
#' or not.
#'
#' @param con Optional database connection. If `NULL`, [call.mydb()] is called.
#'
#' @return A tibble with one row per filterable feature: `feature`,
#'   `valuetype` (`"character"`, or `"table_colnam"` for people), `category`
#'   and `description`.
#'
#' @seealso [plot_feature_values()] for the values a feature actually holds,
#'   [query_plots()] to filter with them, [subplot_list()] for all feature types.
#'
#' @examples
#' \dontrun{
#'   plot_feature_filters(con = mydb)
#' }
#'
#' @export
plot_feature_filters <- function(con = NULL) {

  if (is.null(con)) con <- call.mydb()

  feats <- subplot_list(con)

  feats %>%
    dplyr::filter(.is_filterable_valuetype(.data$valuetype)) %>%
    dplyr::transmute(
      feature     = .data$type,
      valuetype   = .data$valuetype,
      category    = .data$category,
      description = .data$typedescription
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$category, .data$feature)
}

#' Values a plot feature actually holds
#'
#' @description
#' The distinct stored values of one filterable feature, with the number of
#' plots carrying each. Meant for discovering what can be asked for before
#' passing it to `query_plots(feature_filters = ...)`, and for populating a
#' dropdown in the query app.
#'
#' Lookup features (`table_colnam`) are resolved to readable names, so the
#' values returned are the ones to filter on.
#'
#' @param feature A single feature name, as listed by [plot_feature_filters()].
#' @param con Optional database connection. If `NULL`, [call.mydb()] is called.
#'
#' @return A tibble with `value` and `n_plots`, most widespread value first.
#'
#' @seealso [plot_feature_filters()], [query_plots()]
#'
#' @examples
#' \dontrun{
#'   plot_feature_values("data_provider", con = mydb)
#'   plot_feature_values("principal_investigator", con = mydb)
#' }
#'
#' @export
plot_feature_values <- function(feature, con = NULL) {

  if (is.null(con)) con <- call.mydb()

  if (length(feature) != 1 || is.na(feature) || !nzchar(feature)) {
    cli::cli_abort("{.arg feature} must be a single feature name.")
  }

  spec <- .validate_feature_names(feature, con)
  valuetype <- spec$valuetype[1]

  sql <- if (grepl("^table_", valuetype)) {

    lk <- .feature_lookup_spec(valuetype, con)

    glue::glue_sql(
      "SELECT lk.{`lk$value_column`} AS value,
              COUNT(DISTINCT sp.id_table_liste_plots) AS n_plots
         FROM data_liste_sub_plots sp
         JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
         JOIN {`lk$table`} lk ON lk.{`lk$id_column`} = sp.typevalue::integer
        WHERE spt.type = {feature}
          AND lk.{`lk$value_column`} IS NOT NULL
        GROUP BY 1
        ORDER BY 2 DESC, 1",
      feature = feature, .con = con
    )

  } else {

    glue::glue_sql(
      "SELECT BTRIM(sp.typevalue_char) AS value,
              COUNT(DISTINCT sp.id_table_liste_plots) AS n_plots
         FROM data_liste_sub_plots sp
         JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
        WHERE spt.type = {feature}
          AND sp.typevalue_char IS NOT NULL
          AND BTRIM(sp.typevalue_char) <> ''
        GROUP BY 1
        ORDER BY 2 DESC, 1",
      feature = feature, .con = con
    )
  }

  dplyr::as_tibble(func_try_fetch(con = con, sql = sql))
}
