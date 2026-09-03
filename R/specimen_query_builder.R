# =============================================================================
# BUILDING THE SPECIMEN QUERY
# =============================================================================
#
# The specimen counterpart of `R/plot_query_builder.R`: `query_specimens()`
# translates each of its filter arguments into SQL conditions over `specimens`,
# and `.assemble_specimen_query()` assembles them into one SELECT.

#' Conditions selecting specimens by collector
#'
#' A collector is given either by name, resolved against `table_colnam`, or
#' directly by id.
#'
#' @param collector Character vector of collector name(s).
#' @param id_colnam Integer vector of `table_colnam` id(s), used instead of
#'   resolving `collector`.
#' @param con A DBI connection or pool.
#' @param interactive Logical. If `TRUE`, resolve the names through
#'   `.link_table()` fuzzy matching.
#' @return A character vector of SQL conditions.
#' @keywords internal
#' @noRd
.specimen_condition_collector <- function(collector = NULL, id_colnam = NULL,
                                          con, interactive = FALSE) {

  if (is.null(collector) && is.null(id_colnam)) return(character(0))

  if (!is.null(id_colnam)) {
    return(as.character(glue::glue_sql(
      "id_colnam IN ({ids*})", ids = id_colnam, .con = con
    )))
  }

  if (interactive) {

    linked_data <- .verbose_output(.link_table(
      data_stand = dplyr::tibble(colnam = collector),
      column_searched = "colnam",
      column_name = "colnam",
      id_field = "id_table_colnam",
      id_table_name = "id_table_colnam",
      db_connection = con,
      table_name = "table_colnam"
    ))

    keep <- !is.na(linked_data$id_table_colnam) & linked_data$id_table_colnam != 0
    collector_ids <- linked_data$id_table_colnam[keep]

    if (length(collector_ids) == 0) {
      cli::cli_alert_warning("No valid collectors selected")
      return(character(0))
    }

  } else {

    collectors_tbl <- dplyr::collect(try_open_postgres_table("table_colnam", con))
    collectors_tbl <-
      collectors_tbl[tolower(collectors_tbl$colnam) %in% tolower(collector), , drop = FALSE]

    if (nrow(collectors_tbl) == 0) {
      cli::cli_alert_warning("No collectors found matching: {paste(collector, collapse = ', ')}")
      cli::cli_alert_info("Tip: Use interactive = TRUE for fuzzy matching")
      return(character(0))
    }

    collector_ids <- collectors_tbl$id_table_colnam
  }

  as.character(glue::glue_sql(
    "id_colnam IN ({ids*})", ids = collector_ids, .con = con
  ))
}

#' Conditions selecting specimens by collection number
#'
#' An exact set of numbers and a range are independent conditions, so passing
#' both narrows to their intersection.
#'
#' @param number Vector of collection number(s) to match exactly.
#' @param number_min,number_max Bounds of a collection number range.
#' @param con A DBI connection or pool.
#' @return A character vector of SQL conditions.
#' @keywords internal
#' @noRd
.specimen_condition_number <- function(number = NULL, number_min = NULL,
                                       number_max = NULL, con) {

  conditions <- character(0)

  if (!is.null(number)) {
    conditions <- c(conditions, as.character(glue::glue_sql(
      "colnbr IN ({nums*})", nums = number, .con = con
    )))
  }

  if (!is.null(number_min)) {
    conditions <- c(conditions, as.character(glue::glue_sql(
      "colnbr >= {num}", num = number_min, .con = con
    )))
  }

  if (!is.null(number_max)) {
    conditions <- c(conditions, as.character(glue::glue_sql(
      "colnbr <= {num}", num = number_max, .con = con
    )))
  }

  conditions
}

#' Conditions selecting specimens by taxonomy
#'
#' Only `idtax_n` is a column of `specimens`. Genus, species and family live in
#' the taxa database and would need a join to be honoured here; they have never
#' been applied, so they are refused loudly rather than dropped in silence.
#'
#' @param genus,species,family Character vectors, currently not supported.
#' @param idtax_n Integer vector of taxon id(s).
#' @param con A DBI connection or pool.
#' @return A character vector of SQL conditions.
#' @keywords internal
#' @noRd
.specimen_condition_taxonomy <- function(genus = NULL, species = NULL,
                                         family = NULL, idtax_n = NULL, con) {

  unsupported <- c(
    genus = !is.null(genus), species = !is.null(species), family = !is.null(family)
  )
  if (any(unsupported)) {
    ignored <- names(unsupported)[unsupported]
    cli::cli_alert_warning(
      "Not applied to the specimen query: {.arg {ignored}}."
    )
    cli::cli_alert_info(
      "Resolve the names with {.fn query_taxa} and pass {.arg idtax_n} instead."
    )
  }

  if (is.null(idtax_n)) return(character(0))

  as.character(glue::glue_sql(
    "idtax_n IN ({ids*})", ids = idtax_n, .con = con
  ))
}

#' Assemble specimen conditions into a query
#'
#' @param conditions Character vector of SQL conditions, possibly empty.
#' @param con A DBI connection or pool.
#' @param operator Character. Join operator between conditions, `"AND"`
#'   (default) or `"OR"`.
#' @return A `SQL` query object selecting from `specimens`.
#' @keywords internal
#' @noRd
.assemble_specimen_query <- function(conditions, con, operator = "AND") {

  base_query <- "SELECT * FROM specimens"

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

#' The query behind `query_specimens()`
#'
#' @param con A DBI connection or pool.
#' @param collector,id_colnam Collector, by name or by id.
#' @param number,number_min,number_max Collection number, exact or range.
#' @param genus,species,family,idtax_n Taxonomic filters.
#' @param interactive Logical. If `TRUE`, resolve names through
#'   `.link_table()` fuzzy matching.
#' @param operator Character. Join operator between conditions.
#' @return A `SQL` query object.
#' @keywords internal
#' @noRd
.specimen_filter_query <- function(con,
                                   collector = NULL,
                                   id_colnam = NULL,
                                   number = NULL,
                                   number_min = NULL,
                                   number_max = NULL,
                                   genus = NULL,
                                   species = NULL,
                                   family = NULL,
                                   idtax_n = NULL,
                                   interactive = FALSE,
                                   operator = "AND") {

  conditions <- c(
    .specimen_condition_collector(
      collector = collector, id_colnam = id_colnam,
      con = con, interactive = interactive
    ),
    .specimen_condition_number(
      number = number, number_min = number_min, number_max = number_max, con = con
    ),
    .specimen_condition_taxonomy(
      genus = genus, species = species, family = family,
      idtax_n = idtax_n, con = con
    )
  )

  .assemble_specimen_query(conditions, con, operator = operator)
}

#' Add collector names to specimens
#'
#' A collector table that cannot be read is a warning rather than an error: the
#' specimens are still worth returning.
#'
#' @param specimens A data frame of specimens.
#' @param con A DBI connection or pool.
#' @return `specimens`, with collector columns joined in where available.
#' @keywords internal
#' @noRd
.enrich_specimen_collectors <- function(specimens, con) {

  collectors_tbl <- tryCatch({
    try_open_postgres_table("table_colnam", con) %>%
      dplyr::select("id_table_colnam", "colnam", "surname", "family_name") %>%
      dplyr::collect()
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch table_colnam: {e$message}")
    NULL
  })

  if (is.null(collectors_tbl) || !"id_colnam" %in% colnames(specimens)) {
    return(specimens)
  }

  dplyr::left_join(specimens, collectors_tbl, by = c("id_colnam" = "id_table_colnam"))
}

#' Fetch specimens by their identifiers
#'
#' @param specimen_ids Integer vector of `specimens.id_specimen`.
#' @param con A DBI connection or pool.
#' @return A tibble of specimens, enriched with collector names.
#' @keywords internal
#' @noRd
.fetch_specimens_by_ids <- function(specimen_ids, con) {

  if (is.null(specimen_ids) || length(specimen_ids) == 0) return(dplyr::tibble())

  cli::cli_alert_info("Fetching {length(specimen_ids)} specimens by ID")

  sql <- glue::glue_sql(
    "SELECT * FROM specimens WHERE id_specimen IN ({ids*})",
    ids = specimen_ids, .con = con
  )

  specimens <- func_try_fetch(con = con, sql = sql)

  .enrich_specimen_collectors(specimens, con)
}
