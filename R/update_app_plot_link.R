# =============================================================================
# PLOT HIERARCHY BACKEND FOR THE DATA UPDATE APP
# =============================================================================
#
# `data_liste_plots.id_parent_plot` / `parent_relation` cannot be edited through
# the app's generic flat-column form, for two reasons:
#
#   1. The pair is all-or-nothing. `chk_plot_parent_relation_paired` refuses a
#      row holding one without the other, and `execute_direct_updates_single()`
#      writes one column per statement - so attaching a parent would fail on the
#      first statement, before the relation it needs was written. Both columns
#      have to move in a single UPDATE.
#   2. Neither is a value a user can sensibly type. The parent is a plot, picked
#      by name; the relation is a closed vocabulary whose two values invert the
#      arithmetic of every aggregation over the pair.
#
# So the app gets a dedicated section, and these are the queries behind it.
# Everything here takes a plain DBI connection; callers holding a pool wrap with
# `.upd_with_con()`.
#
# See inst/migrations/plot_hierarchy.R for the schema, and
# `check_plot_hierarchy_consistency()` for the checks that span rows.

# How far up or down a hierarchy walk goes before giving up. Real hierarchies
# are one or two levels; anything approaching this is a cycle the consistency
# check should be reporting.
.UPD_LINK_MAX_DEPTH <- 50L


#' Everything the app needs to show and edit one plot's parent link
#'
#' One call, because the section is rendered as a unit: where this plot sits,
#' what sits under it, and which plots it may be attached to.
#'
#' The candidate parents deliberately exclude the plot itself and everything
#' below it. That is the cycle prevention: `chk_plot_not_own_parent` stops
#' A -> A, but nothing in the schema stops A -> B -> A, so the only reliable
#' moment to refuse one is before it is offered.
#'
#' @param id Integer, `data_liste_plots.id_liste_plots`.
#' @param con A DBI connection to the main database.
#'
#' @return A list with:
#'   \describe{
#'     \item{available}{`FALSE` when the hierarchy migration has not run, in
#'       which case every other element is empty and the app hides the section.}
#'     \item{id_parent_plot, parent_name, parent_relation}{The current link,
#'       `NA` when the plot has no parent.}
#'     \item{chain}{The ancestor chain, this plot first: `id_liste_plots`,
#'       `plot_name`, `parent_relation` (how that plot sits in *its* parent),
#'       `depth`.}
#'     \item{children}{Direct children: `id_liste_plots`, `plot_name`,
#'       `parent_relation`.}
#'     \item{candidates}{Named character vector, plot name -> id, of the plots
#'       this one may be attached to.}
#'   }
#' @keywords internal
.upd_plot_link <- function(id, con) {

  empty <- list(
    available       = FALSE,
    id_parent_plot  = NA_integer_,
    parent_name     = NA_character_,
    parent_relation = NA_character_,
    chain           = .upd_empty_plot_rows(),
    children        = .upd_empty_plot_rows(),
    candidates      = character(0)
  )

  id <- suppressWarnings(as.integer(id))
  if (is.na(id)) return(empty)
  if (!.has_plot_hierarchy(con)) return(empty)

  self <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT c.id_liste_plots, c.id_parent_plot, c.parent_relation,
            p.plot_name AS parent_name
       FROM data_liste_plots c
       LEFT JOIN data_liste_plots p ON p.id_liste_plots = c.id_parent_plot
      WHERE c.id_liste_plots = {id}",
    .con = con
  ))
  if (nrow(self) == 0) return(empty)

  children <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT id_liste_plots, plot_name, parent_relation
       FROM data_liste_plots
      WHERE id_parent_plot = {id}
      ORDER BY plot_name",
    .con = con
  ))

  parent_id   <- .upd_na_int(self$id_parent_plot[1])
  parent_name <- .upd_na_chr(self$parent_name[1])

  candidates <- .upd_plot_parent_choices(id, con)
  # The parent this plot already has is always offerable, even where the
  # exclusion above would have dropped it. It only can where the stored
  # hierarchy already holds a cycle, and there the select must still show what
  # is stored - a blank box would read as "no parent" and offer to detach.
  if (!is.na(parent_id) && !as.character(parent_id) %in% candidates &&
      !is.na(parent_name)) {
    candidates <- c(candidates,
                    stats::setNames(as.character(parent_id), parent_name))
    candidates <- candidates[order(names(candidates))]
  }

  list(
    available       = TRUE,
    id_parent_plot  = parent_id,
    parent_name     = parent_name,
    parent_relation = .upd_na_chr(self$parent_relation[1]),
    chain           = .upd_plot_ancestors(id, con),
    children        = .upd_plot_rows(children),
    candidates      = candidates
  )
}


#' The ancestor chain of a plot, the plot itself first
#'
#' `parent_relation` on each row is how *that* plot sits inside its own parent,
#' so the rows read as a path: row 1 is `parent_relation` of row 2, and so on.
#'
#' Carries the visited-path array for the same reason
#' [.descendant_plot_ids()] does: a cycle would otherwise be walked until the
#' depth limit, and a cycle is exactly what an unchecked hierarchy may hold.
#'
#' @param id Integer plot id.
#' @param con A DBI connection.
#' @return Data frame with `id_liste_plots`, `plot_name`, `parent_relation`,
#'   `depth` (1 for the plot itself), ordered from the plot upwards.
#' @keywords internal
.upd_plot_ancestors <- function(id, con) {
  id <- suppressWarnings(as.integer(id))
  if (is.na(id)) return(.upd_empty_plot_rows())

  sql <- sprintf("
    WITH RECURSIVE anc AS (
      SELECT c.id_liste_plots, c.plot_name, c.id_parent_plot, c.parent_relation,
             1 AS depth, ARRAY[c.id_liste_plots] AS visited
      FROM data_liste_plots c
      WHERE c.id_liste_plots = %d

      UNION ALL

      SELECT p.id_liste_plots, p.plot_name, p.id_parent_plot, p.parent_relation,
             a.depth + 1, a.visited || p.id_liste_plots
      FROM data_liste_plots p
      JOIN anc a ON p.id_liste_plots = a.id_parent_plot
      WHERE a.depth < %d
        AND NOT (p.id_liste_plots = ANY(a.visited))
    )
    SELECT id_liste_plots, plot_name, parent_relation, depth
    FROM anc
    ORDER BY depth", id, .UPD_LINK_MAX_DEPTH)

  .upd_plot_rows(DBI::dbGetQuery(con, sql), depth = TRUE)
}


#' Plots a given plot may be attached to
#'
#' Every plot except this one and its descendants. Excluding the subtree is what
#' makes a cycle unreachable from the app.
#'
#' @param id Integer plot id.
#' @param con A DBI connection.
#' @return Named character vector, plot name -> id as character.
#' @keywords internal
.upd_plot_parent_choices <- function(id, con) {
  id <- suppressWarnings(as.integer(id))
  if (is.na(id)) return(character(0))

  all_plots <- DBI::dbGetQuery(
    con, "SELECT id_liste_plots, plot_name FROM data_liste_plots ORDER BY plot_name"
  )
  if (nrow(all_plots) == 0) return(character(0))

  excluded <- c(id, .descendant_plot_ids(con, id)$id_liste_plots)
  keep <- !(all_plots$id_liste_plots %in% excluded) &
    !is.na(all_plots$plot_name) & nzchar(as.character(all_plots$plot_name))

  stats::setNames(
    as.character(all_plots$id_liste_plots[keep]),
    as.character(all_plots$plot_name[keep])
  )
}


#' What is wrong with a proposed parent link
#'
#' Returns problem codes rather than sentences, so the app can say them in the
#' user's language and the console callers can say them in English.
#'
#' Without `con` only the checks that need no database run - the pairing rule
#' and the vocabulary - which is what the form uses for live feedback as the
#' user types. With `con` the link is also checked against the stored hierarchy,
#' which is the gate [.upd_apply_plot_link()] will not write past.
#'
#' @param id Integer plot id being edited.
#' @param parent_id Proposed parent id, `NA` for none.
#' @param relation Proposed relation, `NA` for none.
#' @param con Optional DBI connection.
#'
#' @return Character vector of codes, empty when the link is sound:
#'   `"relation_missing"`, `"parent_missing"`, `"unknown_relation"`,
#'   `"self_parent"`, `"parent_not_found"`, `"cycle"`.
#' @keywords internal
.upd_validate_plot_link <- function(id, parent_id, relation, con = NULL) {
  parent_id <- .upd_na_int(parent_id)
  relation  <- .upd_na_chr(relation)

  problems <- character(0)

  # chk_plot_parent_relation_paired: both or neither.
  if (!is.na(parent_id) && is.na(relation)) {
    problems <- c(problems, "relation_missing")
  }
  if (is.na(parent_id) && !is.na(relation)) {
    problems <- c(problems, "parent_missing")
  }

  # chk_plot_parent_relation: a closed vocabulary.
  if (!is.na(relation) && !relation %in% .plot_parent_relations()) {
    problems <- c(problems, "unknown_relation")
  }

  # chk_plot_not_own_parent.
  id <- suppressWarnings(as.integer(id))
  if (!is.na(parent_id) && !is.na(id) && parent_id == id) {
    problems <- c(problems, "self_parent")
  }

  if (is.null(con) || is.na(parent_id) || is.na(id)) return(unique(problems))

  exists <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT COUNT(*) AS n FROM data_liste_plots WHERE id_liste_plots = {parent_id}",
    .con = con
  ))$n[1]
  if (is.na(exists) || exists == 0) {
    return(unique(c(problems, "parent_not_found")))
  }

  # Nothing in the schema forbids A -> B -> A, so the only defence is refusing
  # to point a plot at something already below it.
  if (parent_id %in% .descendant_plot_ids(con, id)$id_liste_plots) {
    problems <- c(problems, "cycle")
  }

  unique(problems)
}


#' Write a plot's parent link
#'
#' Both columns move in one UPDATE. Writing them separately - which is what the
#' generic direct-update path does, one statement per column - cannot work here:
#' the row between the two statements holds a parent without a relation, or a
#' relation without a parent, and `chk_plot_parent_relation_paired` rejects both.
#'
#' Nothing is written unless the link actually differs from what is stored, and
#' nothing is written at all if [.upd_validate_plot_link()] finds a problem.
#'
#' @param id Integer plot id.
#' @param parent_id New parent id, `NA` to detach.
#' @param relation New relation, `NA` to detach.
#' @param con A DBI connection to the main database.
#'
#' @return `1L` when the link was written, `0L` when it already matched.
#' @keywords internal
.upd_apply_plot_link <- function(id, parent_id, relation, con) {
  id <- suppressWarnings(as.integer(id))
  if (is.na(id)) return(0L)
  if (!.has_plot_hierarchy(con)) return(0L)

  parent_id <- .upd_na_int(parent_id)
  relation  <- .upd_na_chr(relation)

  current <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT id_parent_plot, parent_relation FROM data_liste_plots
      WHERE id_liste_plots = {id}",
    .con = con
  ))
  if (nrow(current) == 0) {
    stop(sprintf("No plot with id_liste_plots = %d.", id), call. = FALSE)
  }

  unchanged <- .upd_same(parent_id, .upd_na_int(current$id_parent_plot[1])) &&
    .upd_same(relation, .upd_na_chr(current$parent_relation[1]))
  if (unchanged) return(0L)

  problems <- .upd_validate_plot_link(id, parent_id, relation, con)
  if (length(problems) > 0) {
    stop(sprintf("Cannot write the parent link: %s.",
                 paste(problems, collapse = ", ")), call. = FALSE)
  }

  # The columns are backed up under both names, so the follow-up table records
  # which pair moved rather than one half of it.
  config <- .upd_routing("plots", c("id_parent_plot", "parent_relation"), con)
  backup <- dplyr::tibble(
    id_liste_plots = rep(id, 2L),
    column         = c("id_parent_plot", "parent_relation")
  )
  backup_direct_records(backup, config, con)

  DBI::dbExecute(con, glue::glue_sql(
    "UPDATE data_liste_plots
        SET id_parent_plot = {parent_id}, parent_relation = {relation}
      WHERE id_liste_plots = {id}",
    .con = con
  ))

  1L
}


# -----------------------------------------------------------------------------
# SMALL HELPERS
# -----------------------------------------------------------------------------

#' An empty plot-rows frame, the shape both walks return
#' @keywords internal
.upd_empty_plot_rows <- function() {
  data.frame(
    id_liste_plots  = integer(0),
    plot_name       = character(0),
    parent_relation = character(0),
    depth           = integer(0),
    stringsAsFactors = FALSE
  )
}

#' Normalise a plot-rows query result to the shape the UI renders
#' @keywords internal
.upd_plot_rows <- function(df, depth = FALSE) {
  if (is.null(df) || nrow(df) == 0) return(.upd_empty_plot_rows())
  out <- data.frame(
    id_liste_plots  = as.integer(df$id_liste_plots),
    plot_name       = as.character(df$plot_name),
    parent_relation = as.character(df$parent_relation),
    depth           = if (depth) as.integer(df$depth) else NA_integer_,
    stringsAsFactors = FALSE
  )
  out
}

#' A scalar as an integer, with every flavour of absent read as NA
#' @keywords internal
.upd_na_int <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  x <- x[1]
  if (is.na(x)) return(NA_integer_)
  if (is.character(x) && !nzchar(trimws(x))) return(NA_integer_)
  suppressWarnings(as.integer(x))
}

#' A scalar as a character, with every flavour of absent read as NA
#' @keywords internal
.upd_na_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- x[1]
  if (is.na(x)) return(NA_character_)
  x <- trimws(as.character(x))
  if (!nzchar(x)) return(NA_character_) else x
}
