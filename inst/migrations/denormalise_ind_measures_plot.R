# =============================================================================
# Give data_ind_measures_feat its own plot key (step 5 groundwork)
#
# WHY THIS TABLE AND NOT THE OTHERS
#
# inst/scripts/check_plot_access_cost.R measured the cost of a plot-scope
# predicate across all seven tables and three predicate shapes. The finding was
# that cost tracks *hops* between a row and its plot, not the shape of the
# predicate:
#
#   0 hops (data_liste_plots)      : under 3 ms at every grant size
#   1 hop  (data_liste_sub_plots)  : under 17 ms
#   2 hops (data_traits_measures)  : 56-520 ms
#   3 hops (data_ind_measures_feat): 395-1931 ms
#
# data_ind_measures_feat is three hops from a plot - through
# data_traits_measures, then data_individuals - and is the only table where the
# number is bad enough to matter. It is also the smallest of the candidates at
# 476,701 rows, and the only one with no plot column at all. So it gets one, and
# the other six keep their existing routes.
#
# WHY THREE TRIGGERS
#
# A denormalised key that row-level security depends on must not be able to go
# quietly stale: stale here means wrong visibility, not just a wrong number. Three
# things can change the answer, and all three are reachable:
#
#   1. a feature row is inserted, or re-pointed at another measurement
#   2. a measurement is re-pointed at another individual
#   3. an individual is moved to another plot - R/mod_update_record.R:376 does
#      exactly this from the record-update module
#
# so there is one trigger for each. Each is BEFORE/AFTER on the narrowest column
# list, with a WHEN clause so it does not fire on unrelated updates, and each pins
# search_path (P4.4). They are SECURITY DEFINER because the value has to be the
# true plot, not the plot as the writer happens to be able to see it - which
# matters precisely once row-level security reaches these tables.
#
# check_ind_measures_plot_key() recomputes the column from scratch and reports any
# row where the stored value disagrees, so drift is detectable even if something
# bypasses all three.
#
# WHAT IT DOES NOT FIX
#
# data_traits_measures.id_table_liste_plots stays as it is: correct on 1,788,724
# rows, wrong on 8,575, NULL on 256,179 (see
# inst/scripts/check_traits_plot_key.R). Nothing reads it for a decision today and
# step 5 will not key on it - the individual is the authoritative route, verified
# against 2 million rows with zero contradictions. This migration derives from the
# individual too, never from that column.
#
# TO ROLL BACK
#   DROP TRIGGER trg_ind_measures_plot_sync ON data_ind_measures_feat;
#   DROP TRIGGER trg_ind_measures_plot_from_measure ON data_traits_measures;
#   DROP TRIGGER trg_ind_measures_plot_from_individual ON data_individuals;
#   DROP FUNCTION ind_measures_plot_sync();
#   DROP FUNCTION ind_measures_plot_from_measure();
#   DROP FUNCTION ind_measures_plot_from_individual();
#   ALTER TABLE data_ind_measures_feat DROP COLUMN id_table_liste_plots;
#
# Run as the owner. Steps 4-6 cannot run inside a transaction, so this is not a
# single atomic change - each step is separately safe and separately verified.
# =============================================================================


#' What the backfill would find, read-only
#'
#' @param con A connection to plots_transects.
#' @return Invisibly a list of data frames.
report_ind_measures_plot_key <- function(con) {

  stopifnot("Invalid connection" = DBI::dbIsValid(con))

  cli::cli_h1("The plot key data_ind_measures_feat does not have yet")

  have <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n FROM pg_attribute
     WHERE attrelid = 'public.data_ind_measures_feat'::regclass
       AND attname = 'id_table_liste_plots' AND NOT attisdropped")$n
  if (have > 0) {
    cli::cli_alert_warning("The column already exists.")
    cli::cli_alert_info("Run {.code check_ind_measures_plot_key(con)} instead.")
  }

  # Can every row reach a plot through the three-hop route? A row that cannot is
  # a row the policy would hide from everyone but the owner.
  reach <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n_rows,
           (count(*) FILTER (WHERE f.id_trait_measures IS NULL))::int
             AS no_measurement,
           (count(*) FILTER (WHERE m.id_trait_measures IS NULL))::int
             AS measurement_missing,
           (count(*) FILTER (WHERE m.id_data_individuals IS NULL))::int
             AS no_individual,
           (count(*) FILTER (WHERE i.id_n IS NULL
                               AND m.id_data_individuals IS NOT NULL))::int
             AS individual_missing,
           (count(*) FILTER (WHERE i.id_table_liste_plots_n IS NULL))::int
             AS individual_has_no_plot,
           (count(*) FILTER (WHERE i.id_table_liste_plots_n IS NOT NULL))::int
             AS resolvable
      FROM data_ind_measures_feat f
      LEFT JOIN data_traits_measures m ON m.id_trait_measures = f.id_trait_measures
      LEFT JOIN data_individuals     i ON i.id_n = m.id_data_individuals")

  cli::cli_h2("Reaching a plot through measurement -> individual")
  print(reach, row.names = FALSE)

  unresolved <- reach$n_rows - reach$resolvable
  if (unresolved == 0) {
    cli::cli_alert_success(
      "All {reach$n_rows} rows resolve to a plot - the column can be NOT NULL")
  } else {
    cli::cli_alert_warning(
      "{unresolved} row{?s} cannot reach a plot. The column cannot be NOT NULL,
       and under row-level security {?that row/those rows} would be invisible to
       everyone but the owner.")
  }

  spread <- DBI::dbGetQuery(con, "
    SELECT count(DISTINCT i.id_table_liste_plots_n)::int AS n_plots,
           count(DISTINCT m.id_data_individuals)::int     AS n_individuals
      FROM data_ind_measures_feat f
      JOIN data_traits_measures m ON m.id_trait_measures = f.id_trait_measures
      JOIN data_individuals     i ON i.id_n = m.id_data_individuals")
  cli::cli_alert_info(
    "{spread$n_plots} plots and {spread$n_individuals} individuals involved")

  invisible(list(column_exists = have > 0, reach = reach, spread = spread))
}


#' Add, backfill and maintain data_ind_measures_feat.id_table_liste_plots
#'
#' @param con A connection to plots_transects, as the owner.
#' @param set_not_null Logical. Set `NOT NULL` once the backfill is complete.
#'   Refuses rather than fails if any row is unresolved. Default `TRUE`.
#' @param dry_run Logical. `TRUE` (the default) prints every statement and
#'   changes nothing.
#' @return Invisibly `TRUE` when applied.
migrate_denormalise_ind_measures_plot <- function(con, set_not_null = TRUE,
                                                 dry_run = TRUE) {

  state <- report_ind_measures_plot_key(con)

  owner <- DBI::dbGetQuery(con, "
    SELECT pg_get_userbyid(relowner) AS owner_name,
           pg_get_userbyid(relowner) = current_user AS i_am_owner
      FROM pg_class WHERE oid = 'public.data_ind_measures_feat'::regclass")
  if (!isTRUE(owner$i_am_owner)) {
    cli::cli_abort(c(
      "Only the owner can alter data_ind_measures_feat.",
      i = "Connect as {.val {owner$owner_name}}."))
  }
  if (state$column_exists) {
    cli::cli_alert_warning("Nothing to do - the column is already there.")
    return(invisible(FALSE))
  }

  unresolved <- state$reach$n_rows - state$reach$resolvable
  if (set_not_null && unresolved > 0) {
    cli::cli_abort(c(
      "{unresolved} row{?s} cannot reach a plot, so NOT NULL would fail.",
      i = "Either resolve them, or call with {.code set_not_null = FALSE}."))
  }

  # --- the statements, grouped by what can share a transaction -------------
  #
  # The triggers are created BEFORE the backfill on purpose: anything written
  # while the backfill runs then gets its key from the trigger rather than being
  # missed between the two steps.
  step1 <- c(
    "ALTER TABLE public.data_ind_measures_feat
       ADD COLUMN id_table_liste_plots integer;",
    "COMMENT ON COLUMN public.data_ind_measures_feat.id_table_liste_plots IS
       'The plot this measurement feature belongs to, denormalised from
        data_traits_measures -> data_individuals. Maintained by three triggers;
        never edit it by hand. Exists because the three-hop route cost 395-1931 ms
        as a row-level security predicate and this costs under 3 ms.';",

    # 1. the row's own parent changed, or the row is new
    "CREATE OR REPLACE FUNCTION public.ind_measures_plot_sync()
     RETURNS trigger LANGUAGE plpgsql
     SECURITY DEFINER SET search_path = pg_catalog, public
     AS $fn$
     BEGIN
       SELECT i.id_table_liste_plots_n
         INTO NEW.id_table_liste_plots
         FROM public.data_traits_measures m
         JOIN public.data_individuals i ON i.id_n = m.id_data_individuals
        WHERE m.id_trait_measures = NEW.id_trait_measures;
       RETURN NEW;
     END $fn$;",
    "REVOKE ALL ON FUNCTION public.ind_measures_plot_sync() FROM PUBLIC;",
    "DROP TRIGGER IF EXISTS trg_ind_measures_plot_sync
       ON public.data_ind_measures_feat;",
    "CREATE TRIGGER trg_ind_measures_plot_sync
       BEFORE INSERT OR UPDATE OF id_trait_measures
       ON public.data_ind_measures_feat
       FOR EACH ROW EXECUTE FUNCTION public.ind_measures_plot_sync();",

    # 2. a measurement was re-pointed at a different individual
    "CREATE OR REPLACE FUNCTION public.ind_measures_plot_from_measure()
     RETURNS trigger LANGUAGE plpgsql
     SECURITY DEFINER SET search_path = pg_catalog, public
     AS $fn$
     BEGIN
       UPDATE public.data_ind_measures_feat f
          SET id_table_liste_plots =
                (SELECT i.id_table_liste_plots_n FROM public.data_individuals i
                  WHERE i.id_n = NEW.id_data_individuals)
        WHERE f.id_trait_measures = NEW.id_trait_measures;
       RETURN NULL;
     END $fn$;",
    "REVOKE ALL ON FUNCTION public.ind_measures_plot_from_measure() FROM PUBLIC;",
    "DROP TRIGGER IF EXISTS trg_ind_measures_plot_from_measure
       ON public.data_traits_measures;",
    "CREATE TRIGGER trg_ind_measures_plot_from_measure
       AFTER UPDATE OF id_data_individuals ON public.data_traits_measures
       FOR EACH ROW
       WHEN (OLD.id_data_individuals IS DISTINCT FROM NEW.id_data_individuals)
       EXECUTE FUNCTION public.ind_measures_plot_from_measure();",

    # 3. an individual was moved to another plot (mod_update_record.R:376)
    "CREATE OR REPLACE FUNCTION public.ind_measures_plot_from_individual()
     RETURNS trigger LANGUAGE plpgsql
     SECURITY DEFINER SET search_path = pg_catalog, public
     AS $fn$
     BEGIN
       UPDATE public.data_ind_measures_feat f
          SET id_table_liste_plots = NEW.id_table_liste_plots_n
         FROM public.data_traits_measures m
        WHERE m.id_trait_measures = f.id_trait_measures
          AND m.id_data_individuals = NEW.id_n;
       RETURN NULL;
     END $fn$;",
    "REVOKE ALL ON FUNCTION public.ind_measures_plot_from_individual() FROM PUBLIC;",
    "DROP TRIGGER IF EXISTS trg_ind_measures_plot_from_individual
       ON public.data_individuals;",
    "CREATE TRIGGER trg_ind_measures_plot_from_individual
       AFTER UPDATE OF id_table_liste_plots_n ON public.data_individuals
       FOR EACH ROW
       WHEN (OLD.id_table_liste_plots_n IS DISTINCT FROM NEW.id_table_liste_plots_n)
       EXECUTE FUNCTION public.ind_measures_plot_from_individual();"
  )

  backfill <- "
    UPDATE data_ind_measures_feat f
       SET id_table_liste_plots = i.id_table_liste_plots_n
      FROM data_traits_measures m
      JOIN data_individuals i ON i.id_n = m.id_data_individuals
     WHERE m.id_trait_measures = f.id_trait_measures
       AND f.id_table_liste_plots IS DISTINCT FROM i.id_table_liste_plots_n;"

  after <- c(
    "ALTER TABLE public.data_ind_measures_feat
       ADD CONSTRAINT fk_ind_measures_feat_liste_plots
       FOREIGN KEY (id_table_liste_plots)
       REFERENCES public.data_liste_plots (id_liste_plots)
       ON DELETE NO ACTION NOT VALID;",
    "ALTER TABLE public.data_ind_measures_feat
       VALIDATE CONSTRAINT fk_ind_measures_feat_liste_plots;",
    "CREATE INDEX CONCURRENTLY IF NOT EXISTS ind_measures_feat_plot_idx
       ON public.data_ind_measures_feat (id_table_liste_plots);",
    "VACUUM (ANALYZE) public.data_ind_measures_feat;"
  )
  if (set_not_null) {
    after <- c(
      "ALTER TABLE public.data_ind_measures_feat
         ALTER COLUMN id_table_liste_plots SET NOT NULL;",
      after)
  }

  show <- function(title, xs) {
    cli::cli_h2(title)
    for (s in xs) cli::cli_verbatim(paste0("  ", gsub("[[:space:]]+", " ", trimws(s))))
  }
  show("Step 1 - column and triggers (one transaction)", step1)
  show("Step 2 - backfill {state$reach$resolvable} rows (one statement)", backfill)
  show("Steps 3-6 - constraint, index, vacuum (each on its own)", after)

  cli::cli_alert_info(c(
    "{.strong Not atomic.} VALIDATE CONSTRAINT, CREATE INDEX CONCURRENTLY and
     VACUUM cannot run inside a transaction. Each step is separately safe and
     separately verified, and the triggers go in before the backfill so nothing
     written in between is missed."))
  cli::cli_alert_warning(
    "The backfill rewrites every one of {state$reach$n_rows} rows, so the table
     roughly doubles on disk until the VACUUM. It takes a ROW EXCLUSIVE lock,
     which does not block readers.")

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed.")
    cli::cli_alert_info("Re-run with {.code dry_run = FALSE} to apply.")
    return(invisible(FALSE))
  }

  # --- step 1 --------------------------------------------------------------
  DBI::dbBegin(con)
  ok <- FALSE
  on.exit({
    if (!ok) {
      try(DBI::dbRollback(con), silent = TRUE)
      cli::cli_alert_danger("Step 1 rolled back - nothing was changed.")
    }
  }, add = TRUE)
  for (s in step1) DBI::dbExecute(con, s)
  DBI::dbCommit(con)
  ok <- TRUE
  cli::cli_alert_success("Step 1: column added, three triggers in place")

  # --- step 2 --------------------------------------------------------------
  cli::cli_alert_info("Step 2: backfilling - this is the slow one")
  n <- DBI::dbExecute(con, backfill)
  cli::cli_alert_success("Step 2: {n} row{?s} backfilled")

  still <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n FROM data_ind_measures_feat
     WHERE id_table_liste_plots IS NULL")$n
  if (still > 0) {
    cli::cli_alert_warning("{still} row{?s} left NULL")
    if (set_not_null) {
      cli::cli_abort(c(
        "Cannot set NOT NULL with {still} NULL row{?s}.",
        i = "The column and triggers are in place and correct; only the
             constraint, index and vacuum were skipped.",
        i = "Investigate with {.code report_ind_measures_plot_key(con)}."))
    }
  }

  # --- steps 3-6 -----------------------------------------------------------
  for (s in after) {
    label <- sub("^\\s*([A-Z ]+).*", "\\1", gsub("[[:space:]]+", " ", trimws(s)))
    cli::cli_alert_info("Running: {substr(gsub('[[:space:]]+', ' ', trimws(s)), 1, 70)}...")
    DBI::dbExecute(con, s)
  }
  cli::cli_alert_success("Steps 3-6 complete")

  check_ind_measures_plot_key(con)
  invisible(TRUE)
}


#' Verify the column, and recompute it to catch drift
#'
#' @param con A connection to plots_transects.
#' @return Invisibly `TRUE` if every check passes.
check_ind_measures_plot_key <- function(con) {

  cli::cli_h1("Verifying data_ind_measures_feat.id_table_liste_plots")
  pass <- TRUE
  say <- function(ok, msg) {
    if (ok) cli::cli_alert_success(msg) else {
      cli::cli_alert_danger(msg); pass <<- FALSE
    }
  }

  col <- DBI::dbGetQuery(con, "
    SELECT a.attname, format_type(a.atttypid, a.atttypmod) AS col_type,
           a.attnotnull AS not_null
      FROM pg_attribute a
     WHERE a.attrelid = 'public.data_ind_measures_feat'::regclass
       AND a.attname = 'id_table_liste_plots' AND NOT a.attisdropped")
  say(nrow(col) == 1, "The column exists")
  if (nrow(col) == 0) return(invisible(FALSE))
  print(col, row.names = FALSE)

  cons <- DBI::dbGetQuery(con, "
    SELECT conname, convalidated, pg_get_constraintdef(oid) AS definition
      FROM pg_constraint
     WHERE conrelid = 'public.data_ind_measures_feat'::regclass
       AND conname = 'fk_ind_measures_feat_liste_plots'")
  say(nrow(cons) == 1 && isTRUE(cons$convalidated[1]),
      "Foreign key to data_liste_plots, validated")

  idx <- DBI::dbGetQuery(con, "
    SELECT c.relname, i.indisvalid
      FROM pg_index i JOIN pg_class c ON c.oid = i.indexrelid
     WHERE i.indrelid = 'public.data_ind_measures_feat'::regclass
       AND c.relname = 'ind_measures_feat_plot_idx'")
  say(nrow(idx) == 1 && isTRUE(idx$indisvalid[1]), "Index present and valid")

  trg <- DBI::dbGetQuery(con, "
    SELECT c.relname AS on_table, t.tgname, t.tgenabled
      FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid
     WHERE NOT t.tgisinternal
       AND t.tgname IN ('trg_ind_measures_plot_sync',
                        'trg_ind_measures_plot_from_measure',
                        'trg_ind_measures_plot_from_individual')
     ORDER BY 2")
  cli::cli_h2("Triggers")
  print(trg, row.names = FALSE)
  say(nrow(trg) == 3, "All three triggers present")
  say(nrow(trg) == 3 && all(trg$tgenabled == "O"), "All three enabled")

  fns <- DBI::dbGetQuery(con, "
    SELECT p.proname, p.prosecdef AS security_definer,
           COALESCE(array_to_string(p.proconfig, ', '), '(none)') AS settings
      FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
     WHERE n.nspname = 'public'
       AND p.proname LIKE 'ind_measures_plot%' ORDER BY 1")
  say(nrow(fns) == 3 && all(grepl("search_path", fns$settings)),
      "search_path pinned on all three functions (P4.4)")

  # --- the one that matters: does the stored value still match? ------------
  cli::cli_h2("Drift: stored value vs recomputed")
  drift <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n_rows,
           (count(*) FILTER (WHERE f.id_table_liste_plots IS NULL))::int AS n_null,
           (count(*) FILTER (WHERE f.id_table_liste_plots
                                   IS DISTINCT FROM i.id_table_liste_plots_n))::int
             AS n_wrong
      FROM data_ind_measures_feat f
      LEFT JOIN data_traits_measures m ON m.id_trait_measures = f.id_trait_measures
      LEFT JOIN data_individuals     i ON i.id_n = m.id_data_individuals")
  print(drift, row.names = FALSE)
  say(drift$n_wrong == 0,
      "Every stored plot key matches the individual it derives from")
  if (drift$n_wrong > 0) {
    cli::cli_alert_info(
      "Re-run the backfill statement from
       {.fn migrate_denormalise_ind_measures_plot} to repair, then find what wrote
       around the triggers.")
  }

  if (pass) cli::cli_alert_success("The denormalised plot key is sound")
  else      cli::cli_alert_danger("Verification failed - see above")

  invisible(pass)
}
