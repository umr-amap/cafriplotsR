# =============================================================================
# Which plot does a trait measurement belong to, and do the three answers agree?
#
# data_traits_measures can reach a plot three ways:
#
#   1. tm.id_table_liste_plots                    - denormalised, written directly
#   2. tm.id_sub_plots  -> data_liste_sub_plots    - the census's plot
#   3. tm.id_data_individuals -> data_individuals  - the individual's plot
#
# (3) is definitional: a measurement is of an individual, and an individual is in
# a plot. (2) is the census. (1) is a copy that something has to keep correct.
#
# WHY THIS MATTERS BEYOND ROW-LEVEL SECURITY
#
# (1) is read at runtime. enrich_census_info() (R/individual_features_function.R)
# fills it from the subplot when NULL:
#
#     id_table_liste_plots = dplyr::coalesce(id_table_liste_plots, .subplot_plot_id)
#
# and its own comment says the subplot table "always carries the correct plot
# foreign key". But coalesce prefers the first non-NULL argument, so a wrong value
# in (1) beats the correct value from (2). filter_to_census() then groups by it to
# choose each plot's first or last census, so a measurement attributed to the wrong
# plot is filtered against that plot's censuses - dropped if its census_name does
# not match, kept if it happens to.
#
# So a disagreement is not only an RLS keying question. It changes what
# query_individual_features(census = "first" | "last") returns.
#
# This script counts the disagreements and characterises them. It writes nothing
# and runs in a READ ONLY transaction.
#
# Run from the package root as the owner, after devtools::load_all(".").
# =============================================================================

library(DBI)

#' @title Do the three routes from a measurement to a plot agree?
#' @param con A connection to plots_transects.
#' @param sample_n How many disagreeing rows to show. Default 15.
#' @return Invisibly a list of data frames.
report_traits_plot_key <- function(con, sample_n = 15L) {

  stopifnot("Invalid connection" = DBI::dbIsValid(con))

  cli::cli_h1("The plot key on data_traits_measures")
  cli::cli_alert_info("Read-only; nothing is written.")

  DBI::dbExecute(con, "BEGIN")
  DBI::dbExecute(con, "SET TRANSACTION READ ONLY")
  on.exit({
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
  }, add = TRUE)

  # --- 1. which routes are even available? ---------------------------------
  present <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n_rows,
           (count(*) FILTER (WHERE id_table_liste_plots IS NOT NULL))::int AS has_denormalised,
           (count(*) FILTER (WHERE id_sub_plots          IS NOT NULL))::int AS has_subplot,
           (count(*) FILTER (WHERE id_data_individuals   IS NOT NULL))::int AS has_individual
      FROM data_traits_measures")

  cli::cli_h2("Routes available, out of {present$n_rows} rows")
  print(present, row.names = FALSE)

  # --- 2. pairwise disagreement -------------------------------------------
  # One pass, both joins. Counted only where both sides of a comparison exist -
  # a NULL is a missing answer, not a wrong one.
  cli::cli_h2("Where two routes both answer and disagree")
  dis <- DBI::dbGetQuery(con, "
    SELECT
      (count(*) FILTER (WHERE tm.id_table_liste_plots IS NOT NULL
                          AND i.id_table_liste_plots_n IS NOT NULL
                          AND tm.id_table_liste_plots <> i.id_table_liste_plots_n))::int
        AS denorm_vs_individual,
      (count(*) FILTER (WHERE tm.id_table_liste_plots IS NOT NULL
                          AND sp.id_table_liste_plots IS NOT NULL
                          AND tm.id_table_liste_plots <> sp.id_table_liste_plots))::int
        AS denorm_vs_subplot,
      (count(*) FILTER (WHERE sp.id_table_liste_plots IS NOT NULL
                          AND i.id_table_liste_plots_n IS NOT NULL
                          AND sp.id_table_liste_plots <> i.id_table_liste_plots_n))::int
        AS subplot_vs_individual
      FROM data_traits_measures tm
      LEFT JOIN data_individuals     i  ON i.id_n         = tm.id_data_individuals
      LEFT JOIN data_liste_sub_plots sp ON sp.id_sub_plots = tm.id_sub_plots")
  print(dis, row.names = FALSE)

  if (dis$subplot_vs_individual > 0) {
    cli::cli_alert_danger(c(
      "{dis$subplot_vs_individual} row{?s} where the census's plot and the
       individual's plot disagree. That is a deeper problem than a stale copy:
       both are normalised foreign keys, so one of them is genuinely wrong."))
  } else {
    cli::cli_alert_success(
      "The census's plot and the individual's plot never disagree - so the
       individual's plot can be treated as authoritative")
  }

  if (dis$denorm_vs_individual == 0 && dis$denorm_vs_subplot == 0) {
    cli::cli_alert_success(
      "The denormalised column never contradicts either normalised route")
    return(invisible(list(present = present, disagreement = dis)))
  }

  cli::cli_alert_danger(
    "The denormalised column contradicts the individual on
     {dis$denorm_vs_individual} row{?s} and the subplot on
     {dis$denorm_vs_subplot}")

  # --- 3. what do the wrong rows look like? -------------------------------
  cli::cli_h2("A sample of the rows where the copy disagrees with the individual")
  smp <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT tm.id_trait_measures,
           tm.id_table_liste_plots  AS denormalised_plot,
           i.id_table_liste_plots_n AS individual_plot,
           sp.id_table_liste_plots  AS census_plot,
           tm.id_data_individuals,
           tm.original_plot_name,
           tm.date_modif_y AS modified_year
      FROM data_traits_measures tm
      JOIN data_individuals     i  ON i.id_n         = tm.id_data_individuals
      LEFT JOIN data_liste_sub_plots sp ON sp.id_sub_plots = tm.id_sub_plots
     WHERE tm.id_table_liste_plots IS NOT NULL
       AND tm.id_table_liste_plots <> i.id_table_liste_plots_n
     ORDER BY tm.id_trait_measures
     LIMIT {sample_n}", .con = con))
  print(smp, row.names = FALSE)

  # --- 4. how concentrated is it? -----------------------------------------
  cli::cli_h2("Which plots are involved")
  shape <- DBI::dbGetQuery(con, "
    SELECT count(*)::int                                   AS n_rows,
           count(DISTINCT tm.id_table_liste_plots)::int     AS n_denormalised_plots,
           count(DISTINCT i.id_table_liste_plots_n)::int    AS n_individual_plots,
           count(DISTINCT tm.id_data_individuals)::int      AS n_individuals,
           (count(*) FILTER (WHERE tm.id_sub_plots IS NOT NULL))::int AS with_a_census,
           min(tm.date_modif_y)::int                        AS first_year,
           max(tm.date_modif_y)::int                        AS last_year
      FROM data_traits_measures tm
      JOIN data_individuals i ON i.id_n = tm.id_data_individuals
     WHERE tm.id_table_liste_plots IS NOT NULL
       AND tm.id_table_liste_plots <> i.id_table_liste_plots_n")
  print(shape, row.names = FALSE)

  top <- DBI::dbGetQuery(con, "
    SELECT i.id_table_liste_plots_n AS individual_plot,
           tm.id_table_liste_plots  AS denormalised_plot,
           count(*)::int            AS n_rows
      FROM data_traits_measures tm
      JOIN data_individuals i ON i.id_n = tm.id_data_individuals
     WHERE tm.id_table_liste_plots IS NOT NULL
       AND tm.id_table_liste_plots <> i.id_table_liste_plots_n
     GROUP BY 1, 2 ORDER BY 3 DESC LIMIT 20")
  cli::cli_h2("Most affected (individual's plot -> what the copy says)")
  print(top, row.names = FALSE)

  # --- 5. does it change what a census filter returns? --------------------
  # Only rows carrying a census can be filtered by census_name, and only plots
  # with more than one census have anything to choose between. Both conditions
  # have to hold for filter_to_census() to return something different.
  cli::cli_h2("Impact on filter_to_census()")
  impact <- DBI::dbGetQuery(con, "
    WITH wrong AS (
      SELECT tm.id_trait_measures, tm.id_sub_plots,
             tm.id_table_liste_plots AS denorm_plot,
             i.id_table_liste_plots_n AS true_plot
        FROM data_traits_measures tm
        JOIN data_individuals i ON i.id_n = tm.id_data_individuals
       WHERE tm.id_table_liste_plots IS NOT NULL
         AND tm.id_table_liste_plots <> i.id_table_liste_plots_n
         AND tm.id_sub_plots IS NOT NULL
    ), census_counts AS (
      SELECT id_table_liste_plots, count(DISTINCT id_sub_plots)::int AS n_census
        FROM data_liste_sub_plots GROUP BY 1
    )
    SELECT count(*)::int AS rows_with_a_census,
           (count(*) FILTER (WHERE cd.n_census > 1))::int
             AS rows_whose_wrong_plot_has_several_censuses,
           (count(*) FILTER (WHERE ct.n_census > 1))::int
             AS rows_whose_true_plot_has_several_censuses
      FROM wrong w
      LEFT JOIN census_counts cd ON cd.id_table_liste_plots = w.denorm_plot
      LEFT JOIN census_counts ct ON ct.id_table_liste_plots = w.true_plot")
  print(impact, row.names = FALSE)

  if (impact$rows_with_a_census == 0) {
    cli::cli_alert_info(
      "None of the wrong rows carry a census, so filter_to_census() cannot act on
       them today. The keying question stands; the query bug does not bite yet.")
  } else {
    cli::cli_alert_danger(c(
      "{impact$rows_with_a_census} wrong row{?s} carr{?ies/y} a census, so
       {.fn filter_to_census} groups {?it/them} under the wrong plot and selects a
       census that is not {?its/their} own."))
  }

  cli::cli_h2("What to do about it")
  cli::cli_alert_info(
    "The one-line half: reverse the coalesce in {.fn enrich_census_info} so the
     normalised route wins -
     {.code coalesce(.subplot_plot_id, id_table_liste_plots)} - which makes the
     query layer correct without touching any data.")
  cli::cli_alert_info(
    "The durable half: backfill the column from the individual and keep it that way
     with a trigger, which also makes the table 0-hop for row-level security.")

  invisible(list(present = present, disagreement = dis, sample = smp,
                 shape = shape, top = top, impact = impact))
}

# ---------------------------------------------------------------------------
if (!interactive()) {
  cat("Source this and call report_traits_plot_key(con).\n")
} else {
  cat("Loaded. Run:\n",
      "  con <- CafriplotsR::call.mydb()\n",
      "  report_traits_plot_key(con)\n", sep = "")
}
