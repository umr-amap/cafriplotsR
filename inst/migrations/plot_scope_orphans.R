# MIGRATION - not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
#   source(system.file("migrations", "plot_scope_orphans.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#   report_plot_scope_orphans(con)                   # read-only, look first
#   migrate_plot_scope_orphans(con)                  # rehearsal: prints, changes nothing
#   migrate_plot_scope_orphans(con, dry_run = FALSE) # apply


#' Report the rows that row-level security would orphan
#'
#' Read-only. A row-level-security policy can only find a plot by following a
#' key, so a row whose key is NULL matches no policy and becomes invisible to
#' everyone except the table owner — including to the person whose plot it
#' belongs to.
#'
#' The audit of 2026-09-26 found 26 such rows:
#'
#' \itemize{
#'   \item `data_liste_sub_plots` — 23 rows with NULL `id_table_liste_plots`
#'   \item `data_traits_measures` — 3 rows with NULL `id_data_individuals`
#' }
#'
#' plus whatever `data_subplot_feat` and `data_ind_measures_feat` rows hang off
#' them. They have to be resolved before RLS is enabled, and resolved in the
#' data — an `OR key IS NULL` clause in the policy would make every future
#' orphan world-readable, which is the leak this work exists to close.
#'
#' @details
#' This also answers a question left open by the audit: whether
#' `data_ind_measures_feat.id_sub_plots` is populated well enough to serve as a
#' one-hop key. Today that table can only reach a plot in three hops
#' (`id_trait_measures` → measurement → individual → plot). If `id_sub_plots`
#' turns out to be reliable it is one hop, which matters on 476 k rows.
#'
#' @param con Database connection to `plots_transects`.
#' @return Invisibly, a list of the orphan sets and what can be inferred.
#' @keywords internal
report_plot_scope_orphans <- function(con) {

  cli::cli_h1("Rows that RLS would orphan")

  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  show <- function(x) {
    if (is.null(x) || nrow(x) == 0) cat("  (no rows)\n") else print(x, row.names = FALSE)
  }
  out <- list()

  # -- subplots with no plot -------------------------------------------------
  cli::cli_h2("data_liste_sub_plots with no plot")

  out$subplots <- DBI::dbGetQuery(con, "
    SELECT sp.*,
           spt.type AS feature_type,
           (SELECT count(*)::int FROM data_traits_measures m
             WHERE m.id_sub_plots = sp.id_sub_plots) AS n_measures,
           (SELECT count(*)::int FROM data_subplot_feat f
             WHERE f.id_sub_plots = sp.id_sub_plots) AS n_subplot_feat
      FROM data_liste_sub_plots sp
      LEFT JOIN subplotype_list spt ON spt.id_subplotype = sp.id_type_sub_plot
     WHERE sp.id_table_liste_plots IS NULL
     ORDER BY sp.id_sub_plots")
  cli::cli_alert_info("{nrow(out$subplots)} subplot row{?s} with no plot")
  show(out$subplots)

  # Can the plot be recovered? The measurements attached to a subplot reach an
  # individual, and an individual always has a plot (that column is 100%
  # populated with a real foreign key). If every measurement on a subplot
  # agrees on one plot, that is the answer.
  cli::cli_h2("Can the plot be inferred from the measurements?")

  out$inferred <- DBI::dbGetQuery(con, "
    SELECT sp.id_sub_plots,
           count(*)::int                              AS n_measures,
           count(DISTINCT i.id_table_liste_plots_n)::int AS n_distinct_plots,
           min(i.id_table_liste_plots_n)               AS inferred_plot,
           max(i.id_table_liste_plots_n)               AS other_plot
      FROM data_liste_sub_plots sp
      JOIN data_traits_measures m ON m.id_sub_plots = sp.id_sub_plots
      JOIN data_individuals i     ON i.id_n = m.id_data_individuals
     WHERE sp.id_table_liste_plots IS NULL
     GROUP BY 1 ORDER BY 1")
  show(out$inferred)

  n_ok  <- sum(out$inferred$n_distinct_plots == 1)
  n_bad <- sum(out$inferred$n_distinct_plots > 1)
  n_none <- nrow(out$subplots) - nrow(out$inferred)

  cli::cli_alert_success("{n_ok} can be resolved unambiguously")
  if (n_bad > 0) {
    cli::cli_alert_danger(
      "{n_bad} span{?s/} several plots - these need a human decision")
  }
  if (n_none > 0) {
    cli::cli_alert_warning(
      "{n_none} carr{?ies/y} no measurement at all - nothing to infer from. \\
       If {?it holds/they hold} no data either, {?it is/they are} deletable.")
  }

  # -- measurements with no individual ---------------------------------------
  cli::cli_h2("data_traits_measures with no individual")

  out$measures <- DBI::dbGetQuery(con, "
    SELECT m.id_trait_measures, m.id_table_liste_plots, m.id_specimen,
           m.id_sub_plots, m.traitid, tl.trait, m.traitvalue, m.traitvalue_char,
           m.country, m.decimallatitude, m.decimallongitude,
           m.original_plot_name, m.basisofrecord,
           m.date_modif_y, m.date_modif_m, m.date_modif_d,
           (SELECT count(*)::int FROM data_ind_measures_feat f
             WHERE f.id_trait_measures = m.id_trait_measures) AS n_feat
      FROM data_traits_measures m
      LEFT JOIN traitlist tl ON tl.id_trait = m.traitid
     WHERE m.id_data_individuals IS NULL
     ORDER BY m.id_trait_measures")
  cli::cli_alert_info("{nrow(out$measures)} measurement{?s} with no individual")
  show(out$measures)

  if (nrow(out$measures) > 0) {
    cli::cli_alert_info(paste(
      "A measurement with a specimen but no individual is a herbarium record,",
      "not plot data - those belong outside the plot policies. One with",
      "neither is unattributable and should be deleted or repaired by hand."))
  }

  # -- the open question about data_ind_measures_feat -------------------------
  cli::cli_h2("Could data_ind_measures_feat be keyed in one hop?")

  out$feat_keys <- DBI::dbGetQuery(con, "
    SELECT count(*)::int                 AS n_rows,
           count(id_trait_measures)::int AS has_measure,
           count(id_sub_plots)::int      AS has_subplot,
           (count(*) - count(id_sub_plots))::int AS subplot_null
      FROM data_ind_measures_feat")
  show(out$feat_keys)

  if (out$feat_keys$subplot_null == 0) {
    cli::cli_alert_success(
      "id_sub_plots is fully populated - a one-hop key via the subplot is possible")
  } else {
    cli::cli_alert_info(
      "id_sub_plots is NULL on {out$feat_keys$subplot_null} of \\
       {out$feat_keys$n_rows} rows - keep the three-hop key via id_trait_measures")
  }

  # -- would the missing foreign key be addable? -----------------------------
  # data_liste_sub_plots.id_table_liste_plots and
  # data_traits_measures.id_table_liste_plots have no FK constraint, so nothing
  # stops them naming a plot that does not exist. Worth knowing before relying
  # on either for anything.
  cli::cli_h2("Do the unconstrained plot columns point at real plots?")

  out$dangling <- DBI::dbGetQuery(con, "
    SELECT 'data_liste_sub_plots' AS table_name, count(*)::int AS dangling_rows
      FROM data_liste_sub_plots sp
     WHERE sp.id_table_liste_plots IS NOT NULL
       AND NOT EXISTS (SELECT 1 FROM data_liste_plots p
                        WHERE p.id_liste_plots = sp.id_table_liste_plots)
    UNION ALL
    SELECT 'data_traits_measures', count(*)::int
      FROM data_traits_measures m
     WHERE m.id_table_liste_plots IS NOT NULL
       AND NOT EXISTS (SELECT 1 FROM data_liste_plots p
                        WHERE p.id_liste_plots = m.id_table_liste_plots)")
  show(out$dangling)

  if (all(out$dangling$dangling_rows == 0)) {
    cli::cli_alert_success(
      "No dangling references - a foreign key could be added to either column")
  } else {
    cli::cli_alert_warning(
      "Dangling references present - a foreign key would be rejected until they are fixed")
  }

  invisible(out)
}


#' Migration: give every plot-scoped row a key RLS can follow
#'
#' Repairs only what the data settles beyond doubt: a subplot whose
#' measurements all point at one plot gets that plot. Everything ambiguous is
#' reported and left alone, because guessing which plot a row belongs to is how
#' data gets attributed to the wrong team.
#'
#' Run [report_plot_scope_orphans()] first and read it.
#'
#' @details
#' Nothing here is a schema change and nothing touches a policy or a privilege.
#' It is an `UPDATE` of `data_liste_sub_plots.id_table_liste_plots` on rows
#' where that column is currently NULL, and it is a no-op on a second run.
#'
#' The remainder needs decisions this migration will not make:
#'
#' \itemize{
#'   \item a subplot whose measurements span several plots — which plot is it?
#'   \item a subplot with no measurements — does it hold anything worth keeping?
#'   \item a measurement with a specimen but no individual — herbarium record,
#'         so out of scope for the plot policies rather than broken
#'   \item a measurement with neither — unattributable
#' }
#'
#' @param con Database connection to `plots_transects`, as the table owner.
#' @param dry_run If TRUE (the default), report what would change and change
#'   nothing.
#' @return Invisibly, a list with the repairs made and the rows left over.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' report_plot_scope_orphans(con)
#' migrate_plot_scope_orphans(con)                   # rehearse
#' migrate_plot_scope_orphans(con, dry_run = FALSE)  # apply
#' }
#'
#' @keywords internal
migrate_plot_scope_orphans <- function(con, dry_run = TRUE) {

  cli::cli_h1("Migration: resolve the rows RLS would orphan")

  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  whoami <- DBI::dbGetQuery(con, "SELECT current_database() AS db, current_user AS usr")
  if (whoami$db != "plots_transects") {
    cli::cli_abort("This migration belongs to plots_transects, not {.val {whoami$db}}")
  }
  cli::cli_alert_info("Connected to {.val {whoami$db}} as {.val {whoami$usr}}")

  # -- Step 1: what can be resolved -----------------------------------------
  cli::cli_h2("Step 1: Subplots whose plot the measurements agree on")

  resolvable <- DBI::dbGetQuery(con, "
    SELECT sp.id_sub_plots,
           min(i.id_table_liste_plots_n) AS inferred_plot,
           count(*)::int                 AS n_measures
      FROM data_liste_sub_plots sp
      JOIN data_traits_measures m ON m.id_sub_plots = sp.id_sub_plots
      JOIN data_individuals i     ON i.id_n = m.id_data_individuals
     WHERE sp.id_table_liste_plots IS NULL
     GROUP BY 1
    HAVING count(DISTINCT i.id_table_liste_plots_n) = 1
     ORDER BY 1")

  if (nrow(resolvable) == 0) {
    cli::cli_alert_info("Nothing can be resolved from the measurements")
  } else {
    print(resolvable, row.names = FALSE)
    cli::cli_alert_success("{nrow(resolvable)} subplot{?s} resolvable")
  }

  # -- Step 2: what cannot ---------------------------------------------------
  cli::cli_h2("Step 2: Left for a human")

  leftover <- DBI::dbGetQuery(con, "
    SELECT sp.id_sub_plots,
           (SELECT count(DISTINCT i.id_table_liste_plots_n)::int
              FROM data_traits_measures m
              JOIN data_individuals i ON i.id_n = m.id_data_individuals
             WHERE m.id_sub_plots = sp.id_sub_plots)      AS n_distinct_plots,
           (SELECT count(*)::int FROM data_traits_measures m
             WHERE m.id_sub_plots = sp.id_sub_plots) AS n_measures,
           (SELECT count(*)::int FROM data_subplot_feat f
             WHERE f.id_sub_plots = sp.id_sub_plots) AS n_subplot_feat
      FROM data_liste_sub_plots sp
     WHERE sp.id_table_liste_plots IS NULL
       AND sp.id_sub_plots NOT IN (
             SELECT sp2.id_sub_plots
               FROM data_liste_sub_plots sp2
               JOIN data_traits_measures m2 ON m2.id_sub_plots = sp2.id_sub_plots
               JOIN data_individuals i2     ON i2.id_n = m2.id_data_individuals
              WHERE sp2.id_table_liste_plots IS NULL
              GROUP BY sp2.id_sub_plots
             HAVING count(DISTINCT i2.id_table_liste_plots_n) = 1)
     ORDER BY 1")

  if (nrow(leftover) == 0) {
    cli::cli_alert_success("No subplot is left unresolved")
  } else {
    print(leftover, row.names = FALSE)
    cli::cli_alert_warning(
      "{nrow(leftover)} subplot{?s} cannot be resolved from the data. \\
       RLS must not be enabled on data_liste_sub_plots until {?it is/they are} \\
       dealt with, or {?it/they} will be invisible to everyone but the owner.")
  }

  orphan_measures <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n,
           (count(*) FILTER (WHERE id_specimen IS NOT NULL))::int AS with_specimen
      FROM data_traits_measures WHERE id_data_individuals IS NULL")
  if (orphan_measures$n > 0) {
    cli::cli_alert_warning(
      "{orphan_measures$n} measurement{?s} still ha{?s/ve} no individual \\
       ({orphan_measures$with_specimen} with a specimen). Not repaired here - \\
       see {.fn report_plot_scope_orphans}.")
  }

  # -- Step 3: apply --------------------------------------------------------
  cli::cli_h2("Step 3: Applying")

  if (nrow(resolvable) == 0) {
    cli::cli_alert_info("Nothing to apply")
    return(invisible(list(repaired = resolvable, leftover = leftover)))
  }

  if (dry_run) {
    for (i in seq_len(nrow(resolvable))) {
      cli::cli_alert_info(
        "Would set data_liste_sub_plots.id_table_liste_plots = \\
         {resolvable$inferred_plot[i]} for id_sub_plots = {resolvable$id_sub_plots[i]} \\
         (from {resolvable$n_measures[i]} measurement{?s})")
    }
    cli::cli_alert_info(
      "Dry run - nothing was changed. Re-run with {.code dry_run = FALSE}.")
    return(invisible(list(repaired = resolvable, leftover = leftover)))
  }

  DBI::dbExecute(con, "SET lock_timeout = '30s'")

  DBI::dbBegin(con)
  ok <- tryCatch({
    n_updated <- 0L
    for (i in seq_len(nrow(resolvable))) {
      n_updated <- n_updated + DBI::dbExecute(con, glue::glue_sql("
        UPDATE data_liste_sub_plots
           SET id_table_liste_plots = {resolvable$inferred_plot[i]}
         WHERE id_sub_plots = {resolvable$id_sub_plots[i]}
           AND id_table_liste_plots IS NULL", .con = con))
    }
    if (n_updated != nrow(resolvable)) {
      cli::cli_abort(
        "Updated {n_updated} row{?s} but expected {nrow(resolvable)} - rolling back")
    }
    DBI::dbCommit(con)
    cli::cli_alert_success("{n_updated} subplot{?s} given their plot")
    TRUE
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Rolled back: {e$message}")
    FALSE
  })
  if (!ok) stop("Migration failed - no change was committed.", call. = FALSE)

  # -- Step 4: verify -------------------------------------------------------
  cli::cli_h2("Step 4: Verifying")

  still <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n FROM data_liste_sub_plots
     WHERE id_table_liste_plots IS NULL")$n

  cli::cli_alert_info("{still} subplot{?s} still without a plot \\
                       (expected {nrow(leftover)})")
  if (still == nrow(leftover)) {
    cli::cli_alert_success("Exactly the rows that could not be inferred remain")
  } else {
    cli::cli_alert_danger("Unexpected count - investigate before going further")
  }

  invisible(list(repaired = resolvable, leftover = leftover))
}
