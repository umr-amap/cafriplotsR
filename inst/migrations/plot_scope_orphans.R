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

  # A second route, for the subplots that carry no measurement at all. The
  # transect-geometry rows found on 2026-09-28 are all of this kind: no
  # measurement to infer from, but an original_subplot_name (GD0301, GD0311 ...)
  # that looks like a plot code. If it matches a plot name, that is the plot.
  cli::cli_h2("Or from original_subplot_name, for those with no measurement?")

  out$by_name <- DBI::dbGetQuery(con, "
    SELECT sp.id_sub_plots,
           sp.original_subplot_name,
           sp.original_plot_name,
           p.id_liste_plots  AS name_match_plot,
           p.plot_name       AS name_match_plot_name,
           count(p.id_liste_plots) OVER (PARTITION BY sp.id_sub_plots)::int AS n_matches
      FROM data_liste_sub_plots sp
      LEFT JOIN data_liste_plots p
             ON p.plot_name = sp.original_subplot_name
             OR p.plot_name = sp.original_plot_name
     WHERE sp.id_table_liste_plots IS NULL
     ORDER BY sp.id_sub_plots")
  show(out$by_name)

  n_named <- sum(!is.na(out$by_name$name_match_plot) & out$by_name$n_matches == 1)
  if (n_named > 0) {
    cli::cli_alert_success(
      "{n_named} can be resolved by name - one matching plot each")
  }
  if (any(out$by_name$n_matches > 1)) {
    cli::cli_alert_danger(
      "{sum(out$by_name$n_matches > 1)} match several plots by name - ambiguous")
  }
  if (all(is.na(out$by_name$name_match_plot))) {
    cli::cli_alert_warning(
      "No original_subplot_name or original_plot_name matches any plot_name. \\
       The plots these belong to may have been deleted - check \\
       followup_updates_liste_plots for the names below.")
  }

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

  # -- the dangling subplots, characterised ----------------------------------
  # Found 2026-09-28: 867 rows, 38x the 23 NULLs, and the same failure under
  # RLS - a key naming a plot that does not exist matches no policy, so the row
  # is invisible to everyone but the owner. Almost certainly debris from plots
  # deleted before safe_delete_plot() existed to clear their subplots.
  n_dang <- out$dangling$dangling_rows[out$dangling$table_name == "data_liste_sub_plots"]

  if (length(n_dang) == 1 && n_dang > 0) {
    cli::cli_h2("The {n_dang} subplots whose plot no longer exists")

    out$dangling_detail <- DBI::dbGetQuery(con, "
      SELECT sp.id_table_liste_plots            AS missing_plot_id,
             count(*)::int                      AS n_subplots,
             count(DISTINCT sp.id_type_sub_plot)::int AS n_feature_types,
             min(sp.original_subplot_name)      AS an_original_name,
             min(sp.date_modif_y)::int          AS first_modif_y,
             max(sp.date_modif_y)::int          AS last_modif_y
        FROM data_liste_sub_plots sp
       WHERE sp.id_table_liste_plots IS NOT NULL
         AND NOT EXISTS (SELECT 1 FROM data_liste_plots p
                          WHERE p.id_liste_plots = sp.id_table_liste_plots)
       GROUP BY 1 ORDER BY 2 DESC")

    # How much hangs off them, in one flat query rather than per group.
    out$dangling_payload <- DBI::dbGetQuery(con, "
      SELECT count(*)::int AS n_subplots,
             (SELECT count(*)::int FROM data_subplot_feat f
               WHERE f.id_sub_plots IN (
                 SELECT sp2.id_sub_plots FROM data_liste_sub_plots sp2
                  WHERE sp2.id_table_liste_plots IS NOT NULL
                    AND NOT EXISTS (SELECT 1 FROM data_liste_plots p2
                                     WHERE p2.id_liste_plots = sp2.id_table_liste_plots)))
                  AS n_subplot_feat_rows,
             (SELECT count(*)::int FROM data_traits_measures m
               WHERE m.id_sub_plots IN (
                 SELECT sp3.id_sub_plots FROM data_liste_sub_plots sp3
                  WHERE sp3.id_table_liste_plots IS NOT NULL
                    AND NOT EXISTS (SELECT 1 FROM data_liste_plots p3
                                     WHERE p3.id_liste_plots = sp3.id_table_liste_plots)))
                  AS n_measures_on_them
        FROM data_liste_sub_plots sp
       WHERE sp.id_table_liste_plots IS NOT NULL
         AND NOT EXISTS (SELECT 1 FROM data_liste_plots p
                          WHERE p.id_liste_plots = sp.id_table_liste_plots)")
    show(out$dangling_payload)

    cli::cli_alert_info(
      "{nrow(out$dangling_detail)} distinct missing plot id{?s}, \\
       {sum(out$dangling_detail$n_subplots)} subplot row{?s} between them")
    print(utils::head(out$dangling_detail, 20), row.names = FALSE)
    if (nrow(out$dangling_detail) > 20) {
      cli::cli_alert_info("(showing the 20 largest of {nrow(out$dangling_detail)})")
    }

    # Do the missing plots appear in the audit trail? If so their names are
    # recoverable, and with them what these subplots described.
    has_audit <- DBI::dbGetQuery(con, "
      SELECT count(*)::int AS n FROM information_schema.columns
       WHERE table_schema='public' AND table_name='followup_updates_liste_plots'
         AND column_name='id_liste_plots'")$n

    if (has_audit == 1) {
      cli::cli_h2("Are the missing plots in followup_updates_liste_plots?")
      out$dangling_audit <- DBI::dbGetQuery(con, "
        SELECT count(DISTINCT sp.id_table_liste_plots)::int AS missing_ids,
               count(DISTINCT f.id_liste_plots)::int        AS found_in_audit
          FROM data_liste_sub_plots sp
          LEFT JOIN followup_updates_liste_plots f
                 ON f.id_liste_plots = sp.id_table_liste_plots
         WHERE sp.id_table_liste_plots IS NOT NULL
           AND NOT EXISTS (SELECT 1 FROM data_liste_plots p
                            WHERE p.id_liste_plots = sp.id_table_liste_plots)")
      show(out$dangling_audit)
      if (out$dangling_audit$found_in_audit > 0) {
        cli::cli_alert_success(
          "{out$dangling_audit$found_in_audit} of {out$dangling_audit$missing_ids} \\
           missing plot id{?s} appear{?s/} in the audit trail - their names and \\
           what they held can be recovered from there")
      } else {
        cli::cli_alert_warning(
          "None of the missing plot ids appear in the audit trail. These subplots \\
           cannot be reattached from inside the database.")
      }
    } else {
      cli::cli_alert_info(
        "followup_updates_liste_plots has no id_liste_plots column - \\
         cannot check the audit trail for the missing plots")
    }
  }

  # -- can the 3 orphan measurements be reattached? --------------------------
  # They carry original_tag_plot, which names the tree they were measured on.
  # If exactly one individual in the named plot carries that tag, the link is
  # repairable rather than the row being unattributable.
  if (nrow(out$measures) > 0) {
    cli::cli_h2("Can the orphan measurements be reattached by tag?")

    out$measures_by_tag <- DBI::dbGetQuery(con, "
      SELECT m.id_trait_measures,
             m.id_table_liste_plots,
             m.original_tag_plot,
             (SELECT count(*)::int FROM data_individuals i
               WHERE i.id_table_liste_plots_n = m.id_table_liste_plots
                 AND i.tag = m.original_tag_plot)   AS candidates_in_plot,
             (SELECT min(i.id_n) FROM data_individuals i
               WHERE i.id_table_liste_plots_n = m.id_table_liste_plots
                 AND i.tag = m.original_tag_plot)   AS candidate_id_n
        FROM data_traits_measures m
       WHERE m.id_data_individuals IS NULL
       ORDER BY m.id_trait_measures")
    show(out$measures_by_tag)

    n_fixable <- sum(out$measures_by_tag$candidates_in_plot == 1, na.rm = TRUE)
    if (n_fixable > 0) {
      cli::cli_alert_success(
        "{n_fixable} can be reattached - exactly one individual in the plot \\
         carries that tag")
    }
    if (any(is.na(out$measures_by_tag$original_tag_plot))) {
      cli::cli_alert_warning(
        "{sum(is.na(out$measures_by_tag$original_tag_plot))} carr{?ies/y} no \\
         original_tag_plot - nothing identifies the tree")
    }
  }

  invisible(out)
}


#' Migration: give every plot-scoped row a key RLS can follow
#'
#' Repairs only what the data settles beyond doubt: a subplot whose
#' measurements all point at one plot, or whose `original_subplot_name` matches
#' exactly one plot name, gets that plot. Everything ambiguous is reported and
#' left alone, because guessing which plot a row belongs to is how data gets
#' attributed to the wrong team.
#'
#' Run [report_plot_scope_orphans()] first and read it.
#'
#' @details
#' Nothing here is a schema change and nothing touches a policy or a privilege.
#' It is an `UPDATE` of `data_liste_sub_plots.id_table_liste_plots` on rows
#' where that column is currently NULL, and it is a no-op on a second run.
#'
#' Two inference routes, because the 23 rows found on 2026-09-28 defeated the
#' first. They are transect endpoint coordinates and elevations carrying no
#' measurement at all, so there was nothing to infer a plot from — but they do
#' carry `original_subplot_name` values that look like plot codes.
#'
#' @section What this does NOT fix:
#' **867 subplot rows name a plot that does not exist** — `id_table_liste_plots`
#' is set but no such `id_liste_plots` remains, because the column has no
#' foreign key and plots were deleted without clearing their subplots. Under RLS
#' they fail exactly as a NULL key does: they match no policy and become
#' invisible to everyone but the owner. That is 38 times the NULL problem and it
#' is not repairable by inference — the plot they belonged to is gone. Deciding
#' between recovering the names from `followup_updates_liste_plots` and deleting
#' the rows as debris is not this migration's call.
#'
#' The rest also needs decisions this migration will not make:
#'
#' \itemize{
#'   \item a subplot whose measurements span several plots — which plot is it?
#'   \item a subplot with neither a measurement nor a matching name
#'   \item the 3 measurements with no individual. They are not herbarium
#'         records, as first assumed: all three are `stem_diameter` on a
#'         `LivingSpecimen` with a plot and a subplot but no specimen, so they
#'         are plot data whose tree row is missing. [report_plot_scope_orphans()]
#'         checks whether `original_tag_plot` identifies exactly one individual
#'         in the named plot, which would make the link repairable
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
    cli::cli_alert_success("{nrow(resolvable)} subplot{?s} resolvable by measurement")
  }

  # -- Step 1b: the second route, for rows with no measurement ---------------
  # The transect-geometry rows carry no measurement but do carry an
  # original_subplot_name that reads as a plot code. Exactly one matching plot
  # name is as unambiguous as one matching measurement plot.
  cli::cli_h2("Step 1b: Subplots whose original name matches exactly one plot")

  by_name <- DBI::dbGetQuery(con, "
    SELECT sp.id_sub_plots,
           min(p.id_liste_plots)        AS inferred_plot,
           min(p.plot_name)             AS matched_plot_name,
           min(sp.original_subplot_name) AS original_subplot_name
      FROM data_liste_sub_plots sp
      JOIN data_liste_plots p
             ON p.plot_name = sp.original_subplot_name
             OR p.plot_name = sp.original_plot_name
     WHERE sp.id_table_liste_plots IS NULL
     GROUP BY sp.id_sub_plots
    HAVING count(DISTINCT p.id_liste_plots) = 1
     ORDER BY 1")

  # A row already resolved by measurement is not resolved twice, and the
  # measurement route wins: it is evidence from the data rather than from a name.
  by_name <- by_name[!by_name$id_sub_plots %in% resolvable$id_sub_plots, , drop = FALSE]

  if (nrow(by_name) == 0) {
    cli::cli_alert_info("No further subplot resolves by name")
  } else {
    print(by_name, row.names = FALSE)
    cli::cli_alert_success("{nrow(by_name)} subplot{?s} resolvable by name")
  }

  resolvable <- rbind(
    data.frame(id_sub_plots = resolvable$id_sub_plots,
               inferred_plot = resolvable$inferred_plot,
               how = "measurement", stringsAsFactors = FALSE),
    data.frame(id_sub_plots = by_name$id_sub_plots,
               inferred_plot = by_name$inferred_plot,
               how = "name", stringsAsFactors = FALSE)
  )

  # -- Step 1c: the group this migration cannot touch ------------------------
  cli::cli_h2("Step 1c: Subplots naming a plot that no longer exists")

  dangling <- DBI::dbGetQuery(con, "
    SELECT count(*)::int                             AS n_rows,
           count(DISTINCT id_table_liste_plots)::int  AS n_missing_plots
      FROM data_liste_sub_plots sp
     WHERE sp.id_table_liste_plots IS NOT NULL
       AND NOT EXISTS (SELECT 1 FROM data_liste_plots p
                        WHERE p.id_liste_plots = sp.id_table_liste_plots)")

  if (dangling$n_rows > 0) {
    cli::cli_alert_danger(
      "{dangling$n_rows} subplot row{?s} name {dangling$n_missing_plots} plot id{?s} \\
       that no longer exist. Under RLS these fail exactly as a NULL key does - \\
       they match no policy and become invisible to everyone but the owner.")
    cli::cli_alert_info(
      "Not repairable by inference: the plot is gone. Run \\
       {.fn report_plot_scope_orphans} for whether the names survive in \\
       followup_updates_liste_plots, and decide between recovering them and \\
       deleting the rows as debris.")
    cli::cli_alert_warning(
      "RLS must not be enabled on data_liste_sub_plots until this is settled.")
  } else {
    cli::cli_alert_success("No subplot names a missing plot")
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
           (SELECT min(sp4.original_subplot_name) FROM data_liste_sub_plots sp4
             WHERE sp4.id_sub_plots = sp.id_sub_plots) AS original_subplot_name
      FROM data_liste_sub_plots sp
     WHERE sp.id_table_liste_plots IS NULL
     ORDER BY 1")

  # Whatever the two inference routes claimed is no longer leftover. Filtering
  # in R rather than repeating both HAVING clauses in SQL keeps one definition
  # of "resolvable" instead of three that can drift apart.
  leftover <- leftover[!leftover$id_sub_plots %in% resolvable$id_sub_plots, , drop = FALSE]

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
         (inferred by {resolvable$how[i]})")
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

  if (dangling$n_rows > 0) {
    cli::cli_alert_warning(
      "Separately, {dangling$n_rows} row{?s} still name a plot that does not \\
       exist. This migration does not touch those, and RLS on \\
       data_liste_sub_plots is still blocked by them.")
  }

  invisible(list(repaired = resolvable, leftover = leftover, dangling = dangling))
}
