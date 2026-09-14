# Where a taxon sits in the table_taxa tree
#
# table_taxa records a classification twice. The flat columns (tax_famclass,
# tax_order, tax_fam, tax_gen, tax_esp, tax_nam01) name it; the tree gives each
# row a rank in `tax_level` and a parent in `id_parent`. The hierarchy view,
# get_taxon_ancestors() and get_taxon_children() walk the tree only.
#
# A taxon added from launch_taxo_backbone_app() used to be written with the flat
# columns alone. With no rank and no parent it sat outside the tree: shown
# alone in the hierarchy view, missing from its genus's children, and invisible
# to check_hierarchy_consistency(), whose every check filters on tax_level.
# These helpers are what the insert now uses to place it.


#' Rank of a taxon, from the most precise flat column filled
#'
#' Vectorised over its arguments. The top rank is written `"higher"`, the
#' spelling the name matching code looks for; the hierarchy migration also
#' wrote `"class"`, and parent lookups accept both.
#'
#' @param tax_famclass,tax_order,tax_fam,tax_gen,tax_esp,tax_nam01 Flat
#'   columns of `table_taxa`. A blank string counts as empty.
#'
#' @return Character vector: `"infraspecific"`, `"species"`, `"genus"`,
#'   `"family"`, `"order"`, `"higher"`, or `NA` when every column is empty.
#' @keywords internal
#' @noRd
.taxon_level <- function(tax_famclass = NA, tax_order = NA, tax_fam = NA,
                         tax_gen = NA, tax_esp = NA, tax_nam01 = NA) {
  filled <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))

  dplyr::case_when(
    filled(tax_nam01) ~ "infraspecific",
    filled(tax_esp) ~ "species",
    filled(tax_gen) ~ "genus",
    filled(tax_fam) ~ "family",
    filled(tax_order) ~ "order",
    filled(tax_famclass) ~ "higher",
    TRUE ~ NA_character_
  )
}


#' Rank one step up the tree
#'
#' @param level Character vector of ranks.
#'
#' @return Character vector; `NA` for the top rank and for unknown ranks.
#' @keywords internal
#' @noRd
.parent_level <- function(level) {
  up <- c(infraspecific = "species", species = "genus", genus = "family",
          family = "order", order = "higher")
  unname(up[level])
}


#' Flat columns that name a taxon's parent
#'
#' A species' genus is the `genus` entry with the same family and genus, as the
#' hierarchy migration matched it. When one of those columns is empty the
#' parent cannot be named, and guessing would attach the taxon to the wrong
#' branch, so nothing is returned.
#'
#' @param level Rank of the child.
#' @inheritParams .taxon_level
#'
#' @return Named list of the columns and values identifying the parent, or
#'   `NULL` when the child has no parent (top rank) or cannot name it.
#' @keywords internal
#' @noRd
.parent_keys <- function(level, tax_famclass = NA, tax_order = NA, tax_fam = NA,
                         tax_gen = NA, tax_esp = NA) {
  if (length(level) != 1 || is.na(level)) return(NULL)

  keys <- switch(
    level,
    infraspecific = list(tax_fam = tax_fam, tax_gen = tax_gen, tax_esp = tax_esp),
    species = list(tax_fam = tax_fam, tax_gen = tax_gen),
    genus = list(tax_fam = tax_fam),
    family = list(tax_order = tax_order),
    order = list(tax_famclass = tax_famclass),
    NULL
  )
  if (is.null(keys)) return(NULL)

  missing <- vapply(keys, function(x) {
    is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))
  }, logical(1))
  if (any(missing)) return(NULL)

  keys
}


#' SQL finding the parent entry of a taxon
#'
#' An accepted entry is preferred, but a synonym is still returned when it is
#' the only one: creating a second, accepted entry for a name the backbone
#' already treats as a synonym would be worse than linking to it.
#'
#' @param parent_level Rank of the parent.
#' @param key_names Names returned by [.parent_keys()], bound as `$1`, `$2`...
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
.parent_lookup_sql <- function(parent_level, key_names) {
  stopifnot(parent_level %in% c("species", "genus", "family", "order", "higher"),
            all(key_names %in% c("tax_famclass", "tax_order", "tax_fam",
                                 "tax_gen", "tax_esp")))

  level_clause <- if (parent_level == "higher") {
    "tax_level IN ('class', 'higher')"
  } else {
    sprintf("tax_level = '%s'", parent_level)
  }
  key_clause <- paste(sprintf("%s = $%d", key_names, seq_along(key_names)),
                      collapse = " AND ")

  paste("SELECT idtax_n FROM table_taxa WHERE", level_clause, "AND", key_clause,
        "ORDER BY (idtax_good_n IS NOT NULL), idtax_n LIMIT 1")
}


#' Insert one row into table_taxa and return its idtax_n
#'
#' The id is read back from the column's own sequence, which is private to the
#' connection that inserted the row. `SELECT MAX(idtax_n)` - what the insert
#' used before - returns someone else's row when two people add a taxon at
#' once; it remains only as the fallback for a column without an owned
#' sequence.
#'
#' @param con A single DBI connection. A pool is refused: the insert and the
#'   read-back must share one connection.
#' @param row One-row data frame of `table_taxa` columns.
#'
#' @return Integer idtax_n of the new row.
#' @keywords internal
#' @noRd
.append_taxa_row <- function(con, row) {
  if (inherits(con, "Pool")) {
    stop(".append_taxa_row() needs one connection, not a pool: the new ",
         "idtax_n is read back on the connection that inserted it")
  }

  DBI::dbWriteTable(con, "table_taxa", as.data.frame(row),
                    append = TRUE, row.names = FALSE)

  new_id <- DBI::dbGetQuery(
    con,
    "SELECT currval(pg_get_serial_sequence('table_taxa', 'idtax_n')) AS idtax_n"
  )$idtax_n[1]

  if (is.null(new_id) || is.na(new_id)) {
    new_id <- DBI::dbGetQuery(
      con, "SELECT MAX(idtax_n) AS idtax_n FROM table_taxa"
    )$idtax_n[1]
  }

  as.integer(new_id)
}
