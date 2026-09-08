# Plot links in an extraction
#
# `data_liste_plots.id_parent_plot` records that two plot records describe the
# same piece of ground, and `parent_relation` says how. An extraction that
# ignores it can hand back a parent and its child side by side with nothing
# saying they overlap - and then every total computed over those rows is wrong,
# in a way no amount of looking at the numbers reveals.
#
# So two things live here: the edges a query can report when asked
# (`extract_plot_links = TRUE`), and the warning that fires whether or not it
# was asked, when a result set holds both ends of an edge.
#
# See inst/migrations/plot_hierarchy.R for the schema and
# `check_plot_hierarchy_consistency()` for the checks that span rows.


#' Parent and child links touching a set of plots
#'
#' One row per edge with at least one end among `plot_ids`, in both directions:
#' the parent a queried plot sits in, and every plot sitting in a queried one.
#'
#' `parent_relation` is always read off the child row, because that is where it
#' is stored and what it describes - how the child sits inside the parent - in
#' both directions of the walk.
#'
#' Returns an empty frame, without querying, on a database where
#' `inst/migrations/plot_hierarchy.R` has not been applied.
#'
#' @param plot_ids Integer vector of `data_liste_plots.id_liste_plots`.
#' @param con A DBI connection or pool to the main database.
#'
#' @return A tibble with `plot_id`, `plot_name`, `role` (`"parent"` when the
#'   linked plot is the parent of the queried one, `"child"` when it sits
#'   inside it), `linked_plot_id`, `linked_plot_name`, `parent_relation`, and
#'   `linked_in_query` - whether the linked plot is itself among `plot_ids`.
#' @keywords internal
.plot_link_edges <- function(plot_ids, con) {

  empty <- dplyr::tibble(
    plot_id          = integer(0),
    plot_name        = character(0),
    role             = character(0),
    linked_plot_id   = integer(0),
    linked_plot_name = character(0),
    parent_relation  = character(0),
    linked_in_query  = logical(0)
  )

  plot_ids <- unique(stats::na.omit(suppressWarnings(as.integer(plot_ids))))
  if (length(plot_ids) == 0) return(empty)
  if (!.has_plot_hierarchy(con)) return(empty)

  ids_sql <- paste(plot_ids, collapse = ",")

  sql <- sprintf("
    SELECT p.id_liste_plots AS plot_id,
           p.plot_name      AS plot_name,
           'child'          AS role,
           c.id_liste_plots AS linked_plot_id,
           c.plot_name      AS linked_plot_name,
           c.parent_relation AS parent_relation
    FROM data_liste_plots c
    JOIN data_liste_plots p ON p.id_liste_plots = c.id_parent_plot
    WHERE c.id_parent_plot IN (%1$s)

    UNION ALL

    SELECT c.id_liste_plots,
           c.plot_name,
           'parent',
           p.id_liste_plots,
           p.plot_name,
           c.parent_relation
    FROM data_liste_plots c
    JOIN data_liste_plots p ON p.id_liste_plots = c.id_parent_plot
    WHERE c.id_liste_plots IN (%1$s)
    ", ids_sql)

  edges <- tryCatch({
    dplyr::as_tibble(DBI::dbGetQuery(con, sql))
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch plot links: {e$message}")
    NULL
  })

  if (is.null(edges) || nrow(edges) == 0) return(empty)

  edges$plot_id         <- as.integer(edges$plot_id)
  edges$linked_plot_id  <- as.integer(edges$linked_plot_id)
  edges$linked_in_query <- edges$linked_plot_id %in% plot_ids

  edges[order(edges$plot_name, edges$role, edges$linked_plot_name), , drop = FALSE]
}


#' Add the parent and child summary columns to a plot table
#'
#' `parent_relation` is rewritten rather than kept, so that the value in the
#' table always agrees with `parent_plot_name` beside it: a query that selected
#' its own columns may not have carried it at all.
#'
#' @param plots A tibble of plots carrying `id_liste_plots`.
#' @param edges The result of [.plot_link_edges()].
#'
#' @return `plots` with `parent_plot_name`, `parent_relation` and
#'   `n_child_plots` added.
#' @keywords internal
.enrich_plot_links <- function(plots, edges) {

  if (!"id_liste_plots" %in% names(plots)) return(plots)

  plots <- plots %>%
    dplyr::select(-dplyr::any_of(c("parent_plot_name", "parent_relation",
                                   "n_child_plots")))

  parents <- edges[edges$role == "parent", , drop = FALSE]
  parents <- dplyr::tibble(
    id_liste_plots   = parents$plot_id,
    parent_plot_name = parents$linked_plot_name,
    parent_relation  = parents$parent_relation
  )
  # A plot has at most one parent, but a hierarchy nobody has checked may say
  # otherwise; keeping the first is better than silently multiplying the rows
  # of the extraction.
  parents <- parents[!duplicated(parents$id_liste_plots), , drop = FALSE]

  children <- edges[edges$role == "child", , drop = FALSE]
  child_counts <- dplyr::tibble(
    id_liste_plots = unique(children$plot_id),
    n_child_plots  = as.integer(table(children$plot_id)[
      as.character(unique(children$plot_id))])
  )

  plots %>%
    dplyr::left_join(parents, by = "id_liste_plots") %>%
    dplyr::left_join(child_counts, by = "id_liste_plots") %>%
    dplyr::mutate(
      n_child_plots = dplyr::coalesce(.data$n_child_plots, 0L)
    )
}


#' Warn when an extraction holds both ends of a plot link
#'
#' Not gated on `extract_plot_links`. A parent and its child in the same result
#' describe overlapping ground under either relation - a `block_member` child
#' tiles its parent, a `nested_subsample` child overlaps it - so any total taken
#' across those rows counts the same stems twice. Nothing in the numbers says
#' so, which is the whole reason to say it here.
#'
#' @param edges The result of [.plot_link_edges()].
#' @param max_show Integer, how many pairs to name before summarising.
#'
#' @return Invisibly, the pairs that were reported.
#' @keywords internal
.warn_overlapping_plot_links <- function(edges, max_show = 5L) {

  if (is.null(edges) || nrow(edges) == 0) return(invisible(NULL))

  # One row per pair: the "child" direction already names both ends.
  pairs <- edges[edges$linked_in_query & edges$role == "child", , drop = FALSE]
  if (nrow(pairs) == 0) return(invisible(NULL))

  n_pairs <- nrow(pairs)
  cli::cli_alert_warning(
    "{n_pairs} returned plot{?s} sit{?s/} inside another returned plot. Totals
     taken across all of them count the same ground twice."
  )

  shown <- utils::head(pairs, max_show)
  cli::cli_ul(vapply(seq_len(nrow(shown)), function(i) {
    sprintf("%s is a %s of %s", shown$linked_plot_name[i],
            shown$parent_relation[i], shown$plot_name[i])
  }, character(1)))

  if (n_pairs > max_show) {
    n_more <- n_pairs - max_show
    cli::cli_alert_info("... and {n_more} more.")
  }

  cli::cli_alert_info(
    "Use {.code extract_plot_links = TRUE} to get the whole link table, or drop
     one side before aggregating."
  )

  invisible(pairs)
}
