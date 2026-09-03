# MIGRATION - corrects links mistyped `reference_plot`
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
# To run it:
#   source(system.file("migrations", "reference_plot_mistyped_links.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#   run_reference_plot_mistyped_migration(con)                  # rehearsal
#   run_reference_plot_mistyped_migration(con, dry_run = FALSE) # apply
#
# Background
# ----------
# `reference_plot_linktype.R` read the 74 rows with `id_liste_plots` set as the
# whole population of the `reference_plot` label. It was not: 517 rows carried
# the string, all written on 2026-01-06 in one session, under one label meaning
# two different things.
#
#   74 rows: id_n NULL, id_liste_plots set
#            - a specimen collected in a plot, tree unknown. A real plot link.
#
#  443 rows: id_n set, id_liste_plots NULL
#            - one specimen serving as the identification reference for several
#              trees of one plot (specimen 39793 -> four trees of somalomo002,
#              specimen 39789 -> seven trees of somalomo004, and so on). That is
#              individual-level data: the link says something about each tree.
#              It is what `referenced_individual` already means, and the
#              `reference_plot` label on them was an error.
#
# The earlier migration's backfill reported those 443 as anomalies and stamped
# them anyway, giving them a plot-scope type while they carry an `id_n` - the
# combination `.check_link_scope()` rejects. This migration retypes them
# `referenced_individual` and fixes the free-text `type` to match, so the two
# columns stay in step.
#
# Their priority rises from 10 to 50. That cannot cost them a determination -
# `type_individual` still outranks them at 100 - but it turns a loss against an
# existing `referenced_individual` link into a tie broken by determination date.
# Phase 1 counts the individuals where that changes the winning specimen, and
# the correction refuses to run blind: read its report first.


.rpm_checkout <- function(con) {
  if (inherits(con, "Pool")) pool::poolCheckout(con) else con
}

.rpm_return <- function(con, actual_con) {
  if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
}

# The rows this migration is about: labelled reference_plot, carrying an
# individual, carrying no plot.
.rpm_where <- "type = 'reference_plot' AND id_n IS NOT NULL AND id_liste_plots IS NULL"


#' Report What Retyping Would Move
#'
#' Counts the individuals whose governing specimen changes when the mistyped
#' links go from priority 10 to priority 50, and lists them.
#'
#' @param con Database connection
#'
#' @return Data frame of affected individuals, empty if nothing moves
#' @keywords internal
report_reference_plot_mistyped_impact <- function(con = NULL) {
  if (is.null(con)) con <- call.mydb()
  actual_con <- .rpm_checkout(con)
  on.exit(.rpm_return(con, actual_con), add = TRUE)

  cli::cli_h1("Impact of retyping the mistyped reference_plot links")

  composition <- DBI::dbGetQuery(actual_con, "
    SELECT
      CASE WHEN id_n IS NULL THEN 'sans individu' ELSE 'avec individu' END AS individu,
      CASE WHEN id_liste_plots IS NULL THEN 'sans plot' ELSE 'avec plot' END AS plot,
      COUNT(*) AS n
    FROM data_link_specimens
    WHERE type = 'reference_plot'
    GROUP BY 1, 2 ORDER BY n DESC
  ")
  cli::cli_alert_info("Current composition of the 'reference_plot' label:")
  print(composition)

  # A row carrying both an individual and a plot means neither reading applies.
  ambiguous <- DBI::dbGetQuery(actual_con, "
    SELECT id_link_specimens, id_specimen, id_n, id_liste_plots
    FROM data_link_specimens
    WHERE type = 'reference_plot' AND id_n IS NOT NULL AND id_liste_plots IS NOT NULL
    ORDER BY id_link_specimens
  ")
  if (nrow(ambiguous) > 0) {
    cli::cli_alert_danger(
      "{nrow(ambiguous)} row(s) carry both an individual and a plot - decide what they are first:"
    )
    print(utils::head(ambiguous, 20))
  }

  # The winning link per individual, before and after the priority change.
  # id_link_specimens breaks the final tie so that the two rankings are
  # comparable; the resolver's own LIMIT 1 is arbitrary at that point, so this
  # is an upper bound on what actually moves.
  affected <- DBI::dbGetQuery(actual_con, sprintf("
    WITH cible AS (
      SELECT id_link_specimens FROM data_link_specimens WHERE %s
    ),
    classement AS (
      SELECT l.id_n, l.id_link_specimens, l.id_specimen,
             COALESCE(lt.priority, 0) AS priority_avant,
             CASE WHEN l.id_link_specimens IN (SELECT id_link_specimens FROM cible)
                  THEN 50 ELSE COALESCE(lt.priority, 0) END AS priority_apres,
             COALESCE(s.dety, 1900) AS dety,
             COALESCE(s.detm, 1)    AS detm,
             COALESCE(s.detd, 1)    AS detd
      FROM data_link_specimens l
      LEFT JOIN linktypelist lt ON lt.id_linktype = l.id_linktype
      LEFT JOIN specimens s     ON s.id_specimen  = l.id_specimen
      WHERE l.id_n IN (SELECT id_n FROM data_link_specimens WHERE %s)
    ),
    avant AS (
      SELECT DISTINCT ON (id_n) id_n,
             id_link_specimens AS lien_avant, id_specimen AS specimen_avant
      FROM classement
      ORDER BY id_n, priority_avant DESC, dety DESC, detm DESC, detd DESC,
               id_link_specimens DESC
    ),
    apres AS (
      SELECT DISTINCT ON (id_n) id_n,
             id_link_specimens AS lien_apres, id_specimen AS specimen_apres
      FROM classement
      ORDER BY id_n, priority_apres DESC, dety DESC, detm DESC, detd DESC,
               id_link_specimens DESC
    )
    SELECT a.id_n, a.lien_avant, a.specimen_avant, b.lien_apres, b.specimen_apres
    FROM avant a JOIN apres b USING (id_n)
    WHERE a.lien_avant <> b.lien_apres
    ORDER BY a.id_n
  ", .rpm_where, .rpm_where))

  if (nrow(affected) == 0) {
    cli::cli_alert_success(
      "No individual changes its governing specimen. The retype is inert for taxonomy."
    )
  } else {
    cli::cli_alert_warning(
      "{nrow(affected)} individual(s) would change governing specimen:"
    )
    print(utils::head(affected, 30))
    cli::cli_alert_info(
      "Each is an individual holding both a mistyped link and a real referenced_individual link, where the mistyped one has the later determination."
    )
  }

  invisible(affected)
}


#' Retype the Mistyped reference_plot Links
#'
#' Sets `id_linktype` and `type` to `referenced_individual` on the links that
#' carry an individual and no plot.
#'
#' @param con Database connection
#' @param dry_run If TRUE (the default), report and change nothing
#'
#' @return TRUE if successful, FALSE if ambiguous rows block the correction
#' @keywords internal
migration_retype_reference_plot_links <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) con <- call.mydb()
  actual_con <- .rpm_checkout(con)
  on.exit(.rpm_return(con, actual_con), add = TRUE)

  cli::cli_h1("Migration: retype mistyped reference_plot links")

  target <- DBI::dbGetQuery(actual_con, sprintf(
    "SELECT COUNT(*) AS n FROM data_link_specimens WHERE %s", .rpm_where
  ))
  cli::cli_alert_info("Links to retype: {target$n[1]}")

  if (target$n[1] == 0) {
    cli::cli_alert_success("Nothing to retype. Already done, or never applicable.")
    return(TRUE)
  }

  ambiguous <- DBI::dbGetQuery(actual_con, "
    SELECT COUNT(*) AS n FROM data_link_specimens
    WHERE type = 'reference_plot' AND id_n IS NOT NULL AND id_liste_plots IS NOT NULL
  ")
  if (ambiguous$n[1] > 0) {
    cli::cli_alert_danger(
      "{ambiguous$n[1]} row(s) carry both an individual and a plot. Resolve them before retyping."
    )
    return(FALSE)
  }

  target_type <- DBI::dbGetQuery(
    actual_con,
    "SELECT id_linktype FROM linktypelist WHERE linktype = 'referenced_individual'"
  )
  if (nrow(target_type) == 0) {
    cli::cli_alert_danger("'referenced_individual' not in linktypelist.")
    stop("referenced_individual link type required")
  }
  cli::cli_alert_info(
    "Target: referenced_individual (id_linktype {target_type$id_linktype[1]}, priority 50)"
  )

  sql_retype <- sprintf("
    UPDATE data_link_specimens
    SET id_linktype = (SELECT id_linktype FROM linktypelist WHERE linktype = 'referenced_individual'),
        type        = 'referenced_individual'
    WHERE %s
  ", .rpm_where)

  if (dry_run) {
    cli::cli_h2("Dry run - SQL that would be executed:")
    cli::cli_code(sql_retype)
    cli::cli_alert_info("{target$n[1]} row(s) would be updated.")
    cli::cli_alert_info(
      "Run report_reference_plot_mistyped_impact() to see what it moves before applying."
    )
    return(TRUE)
  }

  tryCatch({
    n <- DBI::dbExecute(actual_con, sql_retype)
    cli::cli_alert_success("Retyped {n} link(s) as referenced_individual")
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Run the Full Mistyped-Links Correction
#'
#' Reports the impact, then retypes.
#'
#' @param con Database connection
#' @param dry_run If TRUE (the default), rehearse without changing anything
#'
#' @return List with the result of each phase
#' @keywords internal
run_reference_plot_mistyped_migration <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) con <- call.mydb()

  cli::cli_h1("Correction of mistyped reference_plot links")
  if (dry_run) cli::cli_alert_warning("DRY RUN - no changes will be made")

  results <- list()
  results$impact <- report_reference_plot_mistyped_impact(con)
  results$retype <- migration_retype_reference_plot_links(con, dry_run = dry_run)

  cli::cli_h1("Correction complete")
  results
}


#' Verify the Mistyped-Links Correction
#'
#' @param con Database connection
#'
#' @return A one-row data frame of checks
#' @keywords internal
verify_reference_plot_mistyped_migration <- function(con = NULL) {
  if (is.null(con)) con <- call.mydb()
  actual_con <- .rpm_checkout(con)
  on.exit(.rpm_return(con, actual_con), add = TRUE)

  cli::cli_h1("Verifying the mistyped-links correction")
  checks <- list()

  remaining <- DBI::dbGetQuery(actual_con, sprintf(
    "SELECT COUNT(*) AS n FROM data_link_specimens WHERE %s", .rpm_where
  ))
  checks$n_still_mistyped <- remaining$n[1]
  if (checks$n_still_mistyped == 0) {
    cli::cli_alert_success("No reference_plot link carries an individual any more")
  } else {
    cli::cli_alert_warning("{checks$n_still_mistyped} link(s) still mistyped")
  }

  # Every remaining reference_plot link must be a real plot link.
  plot_links <- DBI::dbGetQuery(actual_con, "
    SELECT COUNT(*) AS n_total,
           COUNT(id_liste_plots) AS n_avec_plot,
           COUNT(*) FILTER (WHERE id_n IS NULL) AS n_sans_individu
    FROM data_link_specimens WHERE type = 'reference_plot'
  ")
  checks$n_plot_links <- plot_links$n_total[1]
  checks$plot_links_coherent <-
    plot_links$n_total[1] == plot_links$n_avec_plot[1] &&
    plot_links$n_total[1] == plot_links$n_sans_individu[1]
  cli::cli_alert_info("reference_plot links remaining: {checks$n_plot_links}")
  if (checks$plot_links_coherent) {
    cli::cli_alert_success("All of them carry a plot and no individual")
  } else {
    cli::cli_alert_warning("Some do not match the plot scope")
  }

  # type and id_linktype must agree everywhere.
  drift <- DBI::dbGetQuery(actual_con, "
    SELECT COUNT(*) AS n
    FROM data_link_specimens l
    JOIN linktypelist lt ON lt.id_linktype = l.id_linktype
    WHERE l.type IS NOT NULL AND l.type <> lt.linktype
  ")
  checks$n_type_drift <- drift$n[1]
  if (checks$n_type_drift == 0) {
    cli::cli_alert_success("type and id_linktype agree on every link")
  } else {
    cli::cli_alert_warning("{checks$n_type_drift} link(s) where type and id_linktype disagree")
  }

  checks$correction_complete <-
    checks$n_still_mistyped == 0 && checks$plot_links_coherent && checks$n_type_drift == 0
  if (checks$correction_complete) {
    cli::cli_alert_success("Correction is complete")
  } else {
    cli::cli_alert_warning("Correction is incomplete")
  }

  as.data.frame(checks)
}
