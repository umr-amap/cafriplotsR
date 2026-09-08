# Plot hierarchy consistency
#
# data_liste_plots.id_parent_plot records that two plot records describe the
# same piece of ground - typically a nested regeneration inventory sitting
# inside its parent 1 ha plot - and parent_relation says how, because the
# arithmetic inverts between the two relations an edge can mean:
#
#   nested_subsample : the child overlaps the parent. Never sum them.
#   block_member     : the child tiles the parent. Summing is correct.
#
# The database enforces what a single row can say (a plot is not its own
# parent; a parent and a relation appear together or not at all). It cannot
# see a chain. This file checks the things that span rows, and repairs the
# ones with only one sensible repair.
#
# See inst/migrations/plot_hierarchy.R for the schema this checks.


#' Valid plot parent relations
#'
#' The closed vocabulary of `data_liste_plots.parent_relation`, mirroring the
#' `chk_plot_parent_relation` CHECK constraint added by
#' `inst/migrations/plot_hierarchy.R`. Kept here so the package can report an
#' unknown value without asking the database what it allows.
#'
#' @return Character vector of allowed relations
#' @keywords internal
.plot_parent_relations <- function() {
  c("nested_subsample", "block_member")
}


#' Check plot hierarchy consistency
#'
#' @description
#' Validates the plot parent hierarchy in `data_liste_plots`. The database
#' constraints added by `inst/migrations/plot_hierarchy.R` check one row at a
#' time: they stop a plot from being its own parent, and they refuse a parent
#' without a relation. Nothing in the schema can see a *chain*, so nothing
#' stops A -> B -> A. That is the main thing this function looks for.
#'
#' Checks performed:
#'
#' \describe{
#'   \item{`cycles`}{A plot that is its own ancestor. Walks the chain with a
#'     recursive query. **Not auto-fixable** - which edge to break is a
#'     judgement call.}
#'   \item{`self_parent`}{A plot pointing at itself. Blocked by
#'     `chk_plot_not_own_parent`, so this can only appear in a database where
#'     the constraint was never added. Auto-fixable.}
#'   \item{`dangling_parent`}{`id_parent_plot` pointing at a plot that does not
#'     exist. Blocked by the foreign key; possible in a restored copy that
#'     predates it. Auto-fixable.}
#'   \item{`relation_without_parent`}{A `parent_relation` with no parent, which
#'     means nothing. Auto-fixable.}
#'   \item{`parent_without_relation`}{A parent with no relation. **Not
#'     auto-fixable** - only a human knows whether the child tiles the parent
#'     or overlaps it, and guessing wrong corrupts every aggregation over that
#'     pair.}
#'   \item{`unknown_relation`}{A `parent_relation` outside the vocabulary.
#'     **Not auto-fixable.**}
#' }
#'
#' When the hierarchy is clean the function reports its shape - how many plots
#' have a parent, under which relation, and how deep the deepest chain runs -
#' because a chain deeper than two is worth a second look.
#'
#' If `inst/migrations/plot_hierarchy.R` has not been applied there is nothing
#' to check, and the function says so and returns `NULL`.
#'
#' @param con Database connection or pool to the main database. If NULL,
#'   connects with [call.mydb()].
#' @param fix Logical. Attempt to repair the auto-fixable issues? Default
#'   FALSE. Writes to the database, so it asks for confirmation first unless
#'   `force = TRUE`.
#' @param force Logical. Skip the confirmation prompt when `fix = TRUE`.
#'   Default FALSE.
#' @param limit Integer. Maximum rows returned per issue type. Default 100.
#' @param max_depth Integer. How far the chain walk follows a parent link
#'   before giving up. Default 100, which is far beyond any real hierarchy and
#'   exists only so a cycle cannot spin forever.
#'
#' @return `NULL` if the hierarchy is consistent (or unmigrated), invisibly
#'   when `fix = TRUE`. Otherwise a named list of data frames, one per issue
#'   type found. When `fix = TRUE`, the list of remaining issues after repair.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Report only
#' issues <- check_plot_hierarchy_consistency(con)
#'
#' # Repair what can be repaired unambiguously
#' check_plot_hierarchy_consistency(con, fix = TRUE)
#' }
#'
#' @seealso [check_hierarchy_consistency()] for the taxonomic equivalent,
#'   [safe_delete_plot()] which refuses to orphan a child plot.
#'
#' @export
check_plot_hierarchy_consistency <- function(con = NULL,
                                             fix = FALSE,
                                             force = FALSE,
                                             limit = 100,
                                             max_depth = 100) {

  if (is.null(con)) {
    con <- call.mydb()
  }

  limit     <- max(1L, as.integer(limit))
  max_depth <- max(2L, as.integer(max_depth))

  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Checking plot hierarchy consistency")

  if (!.has_plot_hierarchy(actual_con)) {
    cli::cli_alert_info(
      "This database has no plot hierarchy - {.field id_parent_plot} and \\
       {.field parent_relation} are absent."
    )
    cli::cli_alert_info(
      "Nothing to check. Apply {.file inst/migrations/plot_hierarchy.R} first."
    )
    return(invisible(NULL))
  }

  issues <- list()

  # ---- 1. Plots carrying a parent at all -------------------------------
  n_linked <- DBI::dbGetQuery(actual_con, "
    SELECT COUNT(*) AS n
    FROM data_liste_plots
    WHERE id_parent_plot IS NOT NULL
  ")$n

  if (n_linked == 0) {
    cli::cli_alert_success("No plot has a parent - the hierarchy is empty")
  } else {
    cli::cli_alert_info("{n_linked} plot{?s} carr{?ies/y} a parent link")
  }

  # ---- 2. Self-parent --------------------------------------------------
  cli::cli_h2("Checking for self-parents")
  self_parent <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT id_liste_plots, plot_name, id_parent_plot, parent_relation,
           'self_parent' AS issue_type
    FROM data_liste_plots
    WHERE id_parent_plot = id_liste_plots
    LIMIT %d
  ", limit))

  if (nrow(self_parent) > 0) {
    cli::cli_alert_danger(
      "{nrow(self_parent)} plot{?s} {?is/are} {?its/their} own parent \\
       (chk_plot_not_own_parent is missing from this database)"
    )
    issues$self_parent <- self_parent
  } else {
    cli::cli_alert_success("No plot is its own parent")
  }

  # ---- 3. Dangling parent reference ------------------------------------
  cli::cli_h2("Checking that every parent exists")
  dangling <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT c.id_liste_plots, c.plot_name, c.id_parent_plot, c.parent_relation,
           'dangling_parent' AS issue_type
    FROM data_liste_plots c
    LEFT JOIN data_liste_plots p ON p.id_liste_plots = c.id_parent_plot
    WHERE c.id_parent_plot IS NOT NULL
      AND p.id_liste_plots IS NULL
    LIMIT %d
  ", limit))

  if (nrow(dangling) > 0) {
    cli::cli_alert_danger(
      "{nrow(dangling)} plot{?s} point{?s/} at a parent that does not exist"
    )
    issues$dangling_parent <- dangling
  } else {
    cli::cli_alert_success("Every parent link points at a real plot")
  }

  # ---- 4. Pairing: relation without parent -----------------------------
  cli::cli_h2("Checking parent/relation pairing")
  relation_orphan <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT id_liste_plots, plot_name, id_parent_plot, parent_relation,
           'relation_without_parent' AS issue_type
    FROM data_liste_plots
    WHERE id_parent_plot IS NULL
      AND parent_relation IS NOT NULL
    LIMIT %d
  ", limit))

  if (nrow(relation_orphan) > 0) {
    cli::cli_alert_danger(
      "{nrow(relation_orphan)} plot{?s} carr{?ies/y} a relation but no parent"
    )
    issues$relation_without_parent <- relation_orphan
  }

  # ---- 5. Pairing: parent without relation -----------------------------
  parent_orphan <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT c.id_liste_plots, c.plot_name, c.id_parent_plot,
           p.plot_name AS parent_plot_name,
           'parent_without_relation' AS issue_type
    FROM data_liste_plots c
    LEFT JOIN data_liste_plots p ON p.id_liste_plots = c.id_parent_plot
    WHERE c.id_parent_plot IS NOT NULL
      AND c.parent_relation IS NULL
    LIMIT %d
  ", limit))

  if (nrow(parent_orphan) > 0) {
    cli::cli_alert_danger(
      "{nrow(parent_orphan)} plot{?s} carr{?ies/y} a parent but no relation"
    )
    cli::cli_alert_info(
      "Not repairable here: only you know whether each child tiles its parent \\
       ({.val block_member}) or overlaps it ({.val nested_subsample})."
    )
    issues$parent_without_relation <- parent_orphan
  }

  if (nrow(relation_orphan) == 0 && nrow(parent_orphan) == 0) {
    cli::cli_alert_success("Parent and relation always appear together")
  }

  # ---- 6. Vocabulary ---------------------------------------------------
  cli::cli_h2("Checking the relation vocabulary")
  allowed     <- .plot_parent_relations()
  allowed_sql <- paste(sprintf("'%s'", allowed), collapse = ", ")

  unknown_relation <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT id_liste_plots, plot_name, id_parent_plot, parent_relation,
           'unknown_relation' AS issue_type
    FROM data_liste_plots
    WHERE parent_relation IS NOT NULL
      AND parent_relation NOT IN (%s)
    LIMIT %d
  ", allowed_sql, limit))

  if (nrow(unknown_relation) > 0) {
    cli::cli_alert_danger(
      "{nrow(unknown_relation)} plot{?s} use{?s/} a relation outside \\
       {.val {allowed}}"
    )
    issues$unknown_relation <- unknown_relation
  } else {
    cli::cli_alert_success("Every relation is one of {.val {allowed}}")
  }

  # ---- 7. Cycles -------------------------------------------------------
  # The whole reason this function exists. chk_plot_not_own_parent stops
  # A -> A; nothing in the schema stops A -> B -> A, and a cycle makes every
  # upward traversal non-terminating.
  cli::cli_h2("Checking for cycles")

  cycles <- if (n_linked == 0) {
    data.frame()
  } else {
    .find_plot_hierarchy_cycles(actual_con, limit = limit, max_depth = max_depth)
  }

  if (nrow(cycles) > 0) {
    cli::cli_alert_danger("{nrow(cycles)} plot{?s} {?is/are} {?its/their} own ancestor")
    for (i in seq_len(min(nrow(cycles), 10L))) {
      cli::cli_li("{cycles$cycle_path[i]}")
    }
    if (nrow(cycles) > 10) {
      cli::cli_alert_info("  ... and {nrow(cycles) - 10} more")
    }
    cli::cli_alert_info(
      "Not repairable here: breaking a cycle means choosing which link is the \\
       wrong one. Fix it with an explicit UPDATE once you know."
    )
    issues$cycles <- cycles
  } else {
    cli::cli_alert_success("No cycles")
  }

  # ---- 8. Shape, when there is nothing wrong ---------------------------
  if (length(issues) == 0 && n_linked > 0) {
    cli::cli_h2("Hierarchy shape")

    by_relation <- DBI::dbGetQuery(actual_con, "
      SELECT parent_relation, COUNT(*) AS n
      FROM data_liste_plots
      WHERE id_parent_plot IS NOT NULL
      GROUP BY parent_relation
      ORDER BY parent_relation
    ")
    for (i in seq_len(nrow(by_relation))) {
      cli::cli_li("{by_relation$n[i]} plot{?s} as {.val {by_relation$parent_relation[i]}}")
    }

    depth <- .plot_hierarchy_depth(actual_con, max_depth = max_depth)
    if (!is.na(depth)) {
      cli::cli_alert_info("Deepest chain: {depth} level{?s} of parenthood")
      if (depth > 2) {
        cli::cli_alert_warning(
          "A chain deeper than 2 is unusual for plots - worth confirming it is \\
           intended before anything aggregates across it."
        )
      }
    }
  }

  # ---- 9. Summary and optional repair ----------------------------------
  cli::cli_h2("Summary")

  total_issues <- sum(vapply(issues, nrow, integer(1)))

  if (total_issues == 0) {
    cli::cli_alert_success("Plot hierarchy is consistent")
    return(invisible(NULL))
  }

  cli::cli_alert_warning("Found {total_issues} issue{?s} across {length(issues)} check{?s}")

  fixable   <- intersect(names(issues), .plot_hierarchy_fixable_types())
  unfixable <- setdiff(names(issues), fixable)

  if (length(unfixable) > 0) {
    cli::cli_alert_info("Needs a human decision: {.val {unfixable}}")
  }

  if (!fix) {
    if (length(fixable) > 0) {
      cli::cli_alert_info(
        "Run with {.code fix = TRUE} to repair {.val {fixable}}"
      )
    }
    return(issues)
  }

  if (length(fixable) == 0) {
    cli::cli_alert_info("Nothing here can be repaired automatically")
    return(issues)
  }

  fixed <- .fix_plot_hierarchy_issues(
    actual_con,
    issues[fixable],
    force = force
  )

  if (is.null(fixed)) {
    return(issues)
  }

  # Re-check so the caller gets the state that actually exists now, not the
  # state we set out to repair.
  cli::cli_h2("Re-checking after repair")
  check_plot_hierarchy_consistency(
    con       = actual_con,
    fix       = FALSE,
    limit     = limit,
    max_depth = max_depth
  )
}


#' Issue types that can be repaired without a judgement call
#'
#' @return Character vector of issue type names
#' @keywords internal
.plot_hierarchy_fixable_types <- function() {
  c("self_parent", "dangling_parent", "relation_without_parent")
}


#' Find cycles in the plot parent hierarchy
#'
#' Walks upward from every plot that has a parent, carrying the path visited
#' so far, and flags the step that lands on a plot already in that path. The
#' walk stops expanding a row once it is flagged, so the recursion terminates
#' even though the data does not.
#'
#' @param con Raw database connection (not a pool)
#' @param limit Integer, maximum cycles reported
#' @param max_depth Integer, hard stop on chain length
#'
#' @return Data frame with `id_liste_plots`, `plot_name`, `depth`, `cycle_path`
#' @keywords internal
.find_plot_hierarchy_cycles <- function(con, limit = 100, max_depth = 100) {

  sql <- sprintf("
    WITH RECURSIVE walk AS (
      SELECT
        c.id_liste_plots               AS start_id,
        c.id_parent_plot               AS next_id,
        ARRAY[c.id_liste_plots]        AS id_path,
        ARRAY[c.plot_name]             AS name_path,
        1                              AS depth,
        FALSE                          AS is_cycle
      FROM data_liste_plots c
      WHERE c.id_parent_plot IS NOT NULL

      UNION ALL

      SELECT
        w.start_id,
        p.id_parent_plot,
        w.id_path   || p.id_liste_plots,
        w.name_path || p.plot_name,
        w.depth + 1,
        p.id_liste_plots = ANY(w.id_path)
      FROM walk w
      JOIN data_liste_plots p ON p.id_liste_plots = w.next_id
      WHERE NOT w.is_cycle
        AND w.depth < %d
    )
    SELECT
      w.start_id                              AS id_liste_plots,
      s.plot_name                             AS plot_name,
      w.depth                                 AS depth,
      array_to_string(w.name_path, ' -> ')    AS cycle_path
    FROM walk w
    JOIN data_liste_plots s ON s.id_liste_plots = w.start_id
    WHERE w.is_cycle
    ORDER BY w.start_id
    LIMIT %d
  ", max_depth, limit)

  tryCatch({
    DBI::dbGetQuery(con, sql)
  }, error = function(e) {
    cli::cli_alert_warning("Cycle check failed: {e$message}")
    data.frame()
  })
}


#' Depth of the deepest parent chain
#'
#' Only meaningful on an acyclic hierarchy; call it after the cycle check has
#' come back clean.
#'
#' @param con Raw database connection (not a pool)
#' @param max_depth Integer, hard stop on chain length
#'
#' @return Integer depth, or NA on failure
#' @keywords internal
.plot_hierarchy_depth <- function(con, max_depth = 100) {

  sql <- sprintf("
    WITH RECURSIVE anc AS (
      SELECT c.id_liste_plots, c.id_parent_plot, 1 AS depth
      FROM data_liste_plots c
      WHERE c.id_parent_plot IS NOT NULL

      UNION ALL

      SELECT a.id_liste_plots, p.id_parent_plot, a.depth + 1
      FROM anc a
      JOIN data_liste_plots p ON p.id_liste_plots = a.id_parent_plot
      WHERE p.id_parent_plot IS NOT NULL
        AND a.depth < %d
    )
    SELECT COALESCE(MAX(depth), 0) AS max_depth FROM anc
  ", max_depth)

  tryCatch({
    as.integer(DBI::dbGetQuery(con, sql)$max_depth)
  }, error = function(e) {
    NA_integer_
  })
}


#' Repair the unambiguous plot hierarchy issues
#'
#' Each repair here has exactly one sensible outcome, which is why it is
#' allowed to run unattended:
#'
#' - a self-parent and a parent that does not exist are both links that say
#'   nothing, so the link is cleared (both columns, to keep the pairing);
#' - a relation with no parent describes a relationship that is not there, so
#'   the relation is cleared.
#'
#' Everything else - a parent with no relation, an unknown relation, a cycle -
#' needs someone who knows the plots.
#'
#' @param con Raw database connection (not a pool)
#' @param issues Named list of data frames, fixable types only
#' @param force Logical, skip the confirmation prompt
#'
#' @return Named list of row counts, or NULL if the user declined
#' @keywords internal
.fix_plot_hierarchy_issues <- function(con, issues, force = FALSE) {

  cli::cli_h2("Repairing")
  cli::cli_alert_warning("This writes to {.field data_liste_plots}:")
  cli::cli_ul(c(
    if (!is.null(issues$self_parent))
      "{nrow(issues$self_parent)} self-parent link{?s} cleared" else NULL,
    if (!is.null(issues$dangling_parent))
      "{nrow(issues$dangling_parent)} dangling parent link{?s} cleared" else NULL,
    if (!is.null(issues$relation_without_parent))
      "{nrow(issues$relation_without_parent)} orphan relation{?s} cleared" else NULL
  ))

  if (!force) {
    confirm <- choose_prompt(message = "Apply these repairs?")
    if (!isTRUE(confirm)) {
      cli::cli_alert_info("Repair cancelled - nothing was written")
      return(NULL)
    }
  }

  fixed <- list()

  tryCatch({
    DBI::dbBegin(con)

    # Clearing a meaningless link means clearing BOTH columns: leaving the
    # relation behind would trip chk_plot_parent_relation_paired.
    if (!is.null(issues$self_parent)) {
      ids <- paste(issues$self_parent$id_liste_plots, collapse = ",")
      fixed$self_parent <- DBI::dbExecute(con, sprintf("
        UPDATE data_liste_plots
        SET id_parent_plot = NULL, parent_relation = NULL
        WHERE id_liste_plots IN (%s)
      ", ids))
    }

    if (!is.null(issues$dangling_parent)) {
      ids <- paste(issues$dangling_parent$id_liste_plots, collapse = ",")
      fixed$dangling_parent <- DBI::dbExecute(con, sprintf("
        UPDATE data_liste_plots
        SET id_parent_plot = NULL, parent_relation = NULL
        WHERE id_liste_plots IN (%s)
      ", ids))
    }

    if (!is.null(issues$relation_without_parent)) {
      ids <- paste(issues$relation_without_parent$id_liste_plots, collapse = ",")
      fixed$relation_without_parent <- DBI::dbExecute(con, sprintf("
        UPDATE data_liste_plots
        SET parent_relation = NULL
        WHERE id_liste_plots IN (%s)
          AND id_parent_plot IS NULL
      ", ids))
    }

    DBI::dbCommit(con)

    for (nm in names(fixed)) {
      n_nm <- fixed[[nm]]
      cli::cli_alert_success("Repaired {n_nm} row{?s}: {.val {nm}}")
    }

  }, error = function(e) {
    tryCatch(DBI::dbRollback(con), error = function(e2) {})
    cli::cli_alert_danger("Repair failed and was rolled back: {e$message}")
    fixed <<- NULL
  })

  fixed
}
