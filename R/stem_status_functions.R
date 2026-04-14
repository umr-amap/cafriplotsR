# =============================================================================
# Stem vital status computation
# =============================================================================
# Functions for computing and upserting per-census stem vital status.
# The logic mirrors workflow_stem_status.R but targets specific individuals,
# making it suitable for incremental updates triggered by the feature wizard
# or by modifications to key individual features (observations, diameters,
# rainfor flags).
#
# Evidence hierarchy (dead > presumed_dead > missing > alive):
#   A. observations trait   : text patterns → dead / missing / alive
#   B. flag2_rainfor trait  : non-k = dead; k = presumed_dead/missing
#   B2. flag1_rainfor trait : presence = alive indicator (informational)
#   C. stem_diameter trait  : consecutive missing censuses → presumed_dead/dead
#
# Retroactive correction: a presumed_dead census is set to alive if the stem
# is remeasured at a later census.
# =============================================================================


#' Compute stem vital status for specified individuals
#'
#' Derives a per-census vital status (\code{"alive"}, \code{"dead"},
#' \code{"presumed_dead"}, or \code{NA}) for each specified stem, applying the
#' same evidence hierarchy and retroactive correction used in the bulk
#' \file{workflow_stem_status.R}. Intended for incremental updates after new
#' measurements or observations are added, and as a backend for the feature
#' wizard Shiny app.
#'
#' @section Evidence sources (in priority order):
#' \enumerate{
#'   \item \strong{observations} trait: regex patterns on free-text notes
#'     (\code{dead_patterns}, \code{missing_patterns}, \code{alive_patterns})
#'   \item \strong{flag2_rainfor}: non-\code{"k"} = dead;
#'     \code{"k"} = presumed_dead / missing
#'   \item \strong{stem_diameter}: \eqn{\ge 2} consecutive missing censuses =
#'     dead; 1 consecutive missing census = presumed_dead
#'   \item \strong{flag1_rainfor}: presence is an alive indicator
#'     (informational; does not override stronger dead evidence)
#' }
#'
#' @section Retroactive correction:
#' If a stem is classified as \code{"presumed_dead"} at census \emph{N} but
#' has a recorded diameter at a later census \emph{N+k}, that earlier status
#' is corrected to \code{"alive"} (the stem was just temporarily unmeasured).
#'
#' @param individual_ids Integer vector of individual IDs (\code{id_n} /
#'   \code{id_data_individuals}).
#' @param add_data Logical. If \code{TRUE}, upsert the computed statuses into
#'   the database: existing \code{stem_status} records for these individuals
#'   are deleted, then the new statuses are inserted. Default \code{FALSE}.
#' @param dry_run Logical. When \code{add_data = TRUE}, preview the upsert
#'   without committing any changes. Default \code{TRUE}.
#' @param dead_patterns Character vector of case-insensitive regexes
#'   identifying "dead" observation text.
#' @param missing_patterns Character vector of case-insensitive regexes
#'   identifying "missing" observation text.
#' @param alive_patterns Character vector of case-insensitive regexes
#'   identifying "alive" observation text.
#' @param con Database connection. Defaults to \code{call.mydb()}.
#'
#' @return A tibble with one row per individual \eqn{\times} census:
#' \describe{
#'   \item{\code{id_n}}{Individual ID}
#'   \item{\code{id_table_liste_plots}}{Plot ID}
#'   \item{\code{id_sub_plots}}{Census subplot ID (used for DB linking)}
#'   \item{\code{plot_name}}{Plot name}
#'   \item{\code{census_name}}{Census label, e.g. \code{"census_1"}}
#'   \item{\code{census_date}}{Date of census}
#'   \item{\code{stem_vital_status}}{\code{"alive"}, \code{"dead"},
#'     \code{"presumed_dead"}, or \code{NA} (before first measurement)}
#'   \item{\code{missing}}{Logical — stem was reported missing at this census}
#'   \item{\code{evidence_source}}{Human-readable summary of evidence used}
#' }
#' Returned invisibly when \code{add_data = TRUE} and \code{dry_run = FALSE}.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Compute status for a few stems (read-only, no DB write)
#' result <- compute_stem_vital_status(individual_ids = c(101, 102, 103), con = con)
#' print(result)
#'
#' # Compute and preview what would be written to the DB
#' compute_stem_vital_status(
#'   individual_ids = c(101, 102, 103),
#'   add_data = TRUE,
#'   dry_run  = TRUE,
#'   con      = con
#' )
#'
#' # Compute and commit to the DB
#' compute_stem_vital_status(
#'   individual_ids = c(101, 102, 103),
#'   add_data = TRUE,
#'   dry_run  = FALSE,
#'   con      = con
#' )
#' }
#'
#' @export
compute_stem_vital_status <- function(
    individual_ids,
    add_data = FALSE,
    dry_run  = TRUE,
    dead_patterns    = c("\\bdead\\b", "\\bmort\\b"),
    missing_patterns = c("\\bmissing\\b", "\\bnon\\s*vu\\b", "\\bdisparu\\b",
                         "\\bmanquant\\b", "suppos[eé]\\s*mort"),
    alive_patterns   = c("\\balive\\b", "\\bvivant\\b"),
    con = NULL
) {

  if (is.null(con)) con <- call.mydb()

  if (length(individual_ids) == 0) {
    message("No individual IDs provided.")
    return(invisible(tibble::tibble()))
  }
  individual_ids <- unique(as.integer(individual_ids))

  # ── 1. Resolve evidence trait IDs ──────────────────────────────────────────

  evidence_trait_names <- c("stem_diameter", "observations", "flag2_rainfor", "flag1_rainfor")
  trait_ids_df <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT id_trait, trait FROM traitlist WHERE trait IN ({evidence_trait_names*})",
    evidence_trait_names = evidence_trait_names, .con = con
  ))
  evidence_trait_ids <- trait_ids_df$id_trait
  if (length(evidence_trait_ids) == 0)
    stop("Could not resolve trait IDs for evidence traits. Check the traitlist table.")

  # ── 2. Fetch individual metadata (plot ID, tag, plot name) ─────────────────

  ind_base <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT i.id_n,
            i.id_table_liste_plots_n AS id_table_liste_plots,
            i.tag,
            p.plot_name
     FROM data_individuals i
     LEFT JOIN data_liste_plots p ON i.id_table_liste_plots_n = p.id_liste_plots
     WHERE i.id_n IN ({individual_ids*})",
    individual_ids = individual_ids, .con = con
  )) %>% tibble::as_tibble()

  if (nrow(ind_base) == 0) {
    message("No individuals found for the provided IDs.")
    return(invisible(tibble::tibble()))
  }
  plot_ids <- unique(ind_base$id_table_liste_plots)
  message(sprintf(
    "%d individual(s) across %d plot(s)",
    nrow(ind_base), length(plot_ids)
  ))

  # ── 3. Fetch census subplots for all relevant plots ─────────────────────────
  # Defines all available censuses per plot; drives the full individual × census
  # grid and provides id_sub_plots for DB linking.

  census_subplot_ids <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT sp.id_sub_plots,
            sp.id_table_liste_plots,
            sp.typevalue,
            CONCAT(spt.type, '_', sp.typevalue) AS census_name,
            sp.year  AS census_year,
            sp.month AS census_month,
            sp.day   AS census_day
     FROM data_liste_sub_plots sp
     LEFT JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
     WHERE sp.id_table_liste_plots IN ({plot_ids*})
       AND spt.type = 'census'",
    plot_ids = plot_ids, .con = con
  )) %>% tibble::as_tibble()

  if (nrow(census_subplot_ids) == 0) {
    message("No census subplots found for these plots. Cannot compute stem status.")
    return(invisible(tibble::tibble()))
  }

  # ── 4. Fetch evidence traits (long format with census info) ────────────────

  data_long <- query_individual_features(
    individual_ids       = individual_ids,
    trait_ids            = evidence_trait_ids,
    include_multi_census = TRUE,
    format               = "long",
    issues               = "include",
    con                  = con
  ) %>%
    dplyr::rename(id_n = id_data_individuals) %>%
    # char(300) columns are right-padded with spaces; trim before any matching
    dplyr::mutate(traitvalue_char = trimws(traitvalue_char))

  # ── 5. Census chronological order per plot ─────────────────────────────────
  # Built from census subplot metadata so all censuses are covered, even those
  # where a specific individual was never measured.

  census_order <- census_subplot_ids %>%
    dplyr::mutate(
      census_date = suppressWarnings(lubridate::dmy(paste(
        dplyr::coalesce(census_day,   1L),
        dplyr::coalesce(census_month, 1L),
        census_year,
        sep = "-"
      )))
    ) %>%
    dplyr::group_by(id_table_liste_plots, census_name) %>%
    dplyr::summarise(census_date = min(census_date, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      census_date = dplyr::if_else(is.infinite(census_date), as.Date(NA), census_date)
    ) %>%
    dplyr::group_by(id_table_liste_plots) %>%
    dplyr::arrange(census_date, census_name, .by_group = TRUE) %>%
    dplyr::mutate(
      census_rank     = dplyr::row_number(),
      n_censuses_plot = dplyr::n()
    ) %>%
    dplyr::ungroup()

  # ── 6. Full individual × census grid ──────────────────────────────────────

  full_grid <- ind_base %>%
    dplyr::select(id_n, id_table_liste_plots) %>%
    dplyr::left_join(
      census_order %>%
        dplyr::select(id_table_liste_plots, census_name, census_date,
                      census_rank, n_censuses_plot),
      by           = "id_table_liste_plots",
      relationship = "many-to-many"
    )

  # ── 7. Evidence A: observation notes ──────────────────────────────────────

  dead_regex    <- paste(dead_patterns,    collapse = "|")
  missing_regex <- paste(missing_patterns, collapse = "|")
  alive_regex   <- paste(alive_patterns,   collapse = "|")

  obs_status <- data_long %>%
    dplyr::filter(trait == "observations", !is.na(census_name)) %>%
    dplyr::mutate(
      obs_text    = tolower(dplyr::coalesce(traitvalue_char, "")),
      obs_missing = stringr::str_detect(obs_text, missing_regex),
      # Exclude texts already matched as missing so "supposé mort" (containing
      # "mort") is not also flagged as dead
      obs_dead    = stringr::str_detect(obs_text, dead_regex) & !obs_missing,
      obs_alive   = stringr::str_detect(obs_text, alive_regex)
    ) %>%
    dplyr::filter(obs_dead | obs_missing | obs_alive) %>%
    dplyr::group_by(id_n, census_name) %>%
    dplyr::summarise(
      obs_dead    = any(obs_dead),
      obs_missing = any(obs_missing),
      obs_alive   = any(obs_alive),
      obs_text    = paste(unique(dplyr::coalesce(traitvalue_char, "")), collapse = "; "),
      .groups     = "drop"
    )

  message(sprintf(
    "  Evidence A (observations): %d dead, %d missing, %d alive",
    sum(obs_status$obs_dead), sum(obs_status$obs_missing), sum(obs_status$obs_alive)
  ))

  # ── 8. Evidence B: flag2_rainfor ──────────────────────────────────────────

  flag_status <- data_long %>%
    dplyr::filter(
      trait == "flag2_rainfor",
      !is.na(census_name),
      !is.na(traitvalue_char) | !is.na(traitvalue)
    ) %>%
    dplyr::mutate(
      flag_val      = dplyr::coalesce(traitvalue_char, as.character(traitvalue)),
      flag_val_norm = tolower(trimws(flag_val))
    ) %>%
    dplyr::group_by(id_n, census_name) %>%
    dplyr::summarise(
      flag_dead  = any(flag_val_norm != "k"),  # non-k = confirmed dead
      flag_k     = any(flag_val_norm == "k"),  # k = not found / missing
      flag_value = paste(unique(flag_val), collapse = "; "),
      .groups    = "drop"
    )

  message(sprintf(
    "  Evidence B (flag2_rainfor): %d dead (non-k), %d missing/presumed_dead (k)",
    sum(flag_status$flag_dead), sum(flag_status$flag_k)
  ))

  # ── 8b. Evidence B2: flag1_rainfor (alive indicator) ──────────────────────

  flag1_status <- data_long %>%
    dplyr::filter(
      trait == "flag1_rainfor",
      !is.na(census_name),
      !is.na(traitvalue_char) | !is.na(traitvalue)
    ) %>%
    dplyr::mutate(
      flag1_val = dplyr::coalesce(traitvalue_char, as.character(traitvalue))
    ) %>%
    dplyr::group_by(id_n, census_name) %>%
    dplyr::summarise(
      flag1_alive = TRUE,
      flag1_value = paste(unique(flag1_val), collapse = "; "),
      .groups     = "drop"
    )

  message(sprintf(
    "  Evidence B2 (flag1_rainfor): %d individual x census rows", nrow(flag1_status)
  ))

  # ── 9. Evidence C: diameter presence & consecutive-missing logic ───────────

  diam_presence <- data_long %>%
    dplyr::filter(
      trait == "stem_diameter",
      !is.na(census_name),
      !is.na(id_table_liste_plots)
    ) %>%
    dplyr::mutate(has_diam = !is.na(traitvalue) & as.numeric(traitvalue) > 0) %>%
    dplyr::group_by(id_n, id_table_liste_plots, census_name) %>%
    dplyr::summarise(has_diam = any(has_diam, na.rm = TRUE), .groups = "drop")

  # Join to the full grid; absent rows → has_diam = FALSE
  diam_grid <- full_grid %>%
    dplyr::left_join(
      diam_presence,
      by = c("id_n", "id_table_liste_plots", "census_name")
    ) %>%
    dplyr::mutate(has_diam = dplyr::coalesce(has_diam, FALSE)) %>%
    dplyr::arrange(id_n, census_rank)

  # First census rank where the individual was measured
  diam_grid <- diam_grid %>%
    dplyr::group_by(id_n) %>%
    dplyr::mutate(
      first_measured_rank = min(census_rank[has_diam], na.rm = TRUE),
      first_measured_rank = dplyr::if_else(
        is.infinite(first_measured_rank), NA_integer_, as.integer(first_measured_rank)
      )
    ) %>%
    dplyr::ungroup()

  # Running count of consecutive missing censuses (only after first measurement)
  diam_grid <- diam_grid %>%
    dplyr::group_by(id_n) %>%
    dplyr::mutate(
      eligible = !is.na(first_measured_rank) & census_rank > first_measured_rank,
      consec_missing = {
        n  <- dplyr::n()
        cm <- integer(n)
        for (i in seq_len(n)) {
          if (eligible[i] && !has_diam[i]) {
            cm[i] <- if (i == 1L) 1L else cm[i - 1L] + 1L
          } else {
            cm[i] <- 0L
          }
        }
        cm
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      diam_status = dplyr::case_when(
        !eligible            ~ NA_character_,   # before first measurement
        has_diam             ~ NA_character_,   # measured: no negative evidence
        consec_missing >= 2L ~ "dead",          # 2+ consecutive missing
        consec_missing == 1L ~ "presumed_dead", # 1st consecutive missing
        TRUE                 ~ NA_character_
      )
    )

  message(sprintf(
    "  Evidence C (diameter): %d presumed_dead, %d dead",
    sum(diam_grid$diam_status == "presumed_dead", na.rm = TRUE),
    sum(diam_grid$diam_status == "dead",          na.rm = TRUE)
  ))

  # ── 10. Combine all evidence per individual × census ──────────────────────

  status_per_census <- diam_grid %>%
    dplyr::select(
      id_n, id_table_liste_plots, census_name, census_date, census_rank,
      n_censuses_plot, has_diam, consec_missing, diam_status, eligible
    ) %>%
    dplyr::left_join(obs_status,   by = c("id_n", "census_name")) %>%
    dplyr::left_join(flag_status,  by = c("id_n", "census_name")) %>%
    dplyr::left_join(flag1_status, by = c("id_n", "census_name")) %>%
    dplyr::mutate(
      obs_dead    = dplyr::coalesce(obs_dead,    FALSE),
      obs_missing = dplyr::coalesce(obs_missing, FALSE),
      obs_alive   = dplyr::coalesce(obs_alive,   FALSE),
      flag_dead   = dplyr::coalesce(flag_dead,   FALSE),
      flag_k      = dplyr::coalesce(flag_k,      FALSE),
      flag1_alive = dplyr::coalesce(flag1_alive, FALSE)
    )

  # Assign final status (hierarchy: dead > presumed_dead > missing > alive)
  status_per_census <- status_per_census %>%
    dplyr::mutate(
      strong_dead = obs_dead | flag_dead | (!is.na(diam_status) & diam_status == "dead"),

      stem_vital_status = dplyr::case_when(
        strong_dead                                              ~ "dead",
        (!is.na(diam_status) & diam_status == "presumed_dead") |
          flag_k | obs_missing                                   ~ "presumed_dead",
        has_diam                                                 ~ "alive",
        !eligible                                                ~ NA_character_,
        TRUE                                                     ~ "alive"
      ),

      evidence_source = {
        n   <- dplyr::n()
        src <- character(n)
        for (i in seq_len(n)) {
          parts <- character(0)
          # Dead evidence
          if (obs_dead[i])
            parts <- c(parts, paste0("observations: ", obs_text[i]))
          if (flag_dead[i])
            parts <- c(parts, paste0("flag2_rainfor(non-k): ", flag_value[i]))
          if (!is.na(diam_status[i]) && diam_status[i] == "dead")
            parts <- c(parts, paste0("missing_diam(", consec_missing[i], " consecutive)"))
          # Presumed-dead / missing evidence
          if (flag_k[i])
            parts <- c(parts, paste0("flag2_rainfor(k): ", flag_value[i]))
          if (!is.na(diam_status[i]) && diam_status[i] == "presumed_dead")
            parts <- c(parts, paste0("missing_diam(", consec_missing[i], " consecutive)"))
          if (obs_missing[i])
            parts <- c(parts, paste0("observations: ", obs_text[i]))
          # Alive evidence
          if (has_diam[i])
            parts <- c(parts, "diameter_measured")
          if (obs_alive[i])
            parts <- c(parts, paste0("observations: ", obs_text[i]))
          if (flag1_alive[i])
            parts <- c(parts, paste0("flag1_rainfor: ", flag1_value[i]))
          src[i] <- if (length(parts) == 0) "none" else paste(parts, collapse = "; ")
        }
        src
      }
    )

  # ── 10b. Retroactive correction ───────────────────────────────────────────
  # If a stem was presumed_dead at census N but has a diameter at a later
  # census, that earlier classification was wrong — the stem was just
  # temporarily unmeasured. Correct to "alive".

  status_per_census <- status_per_census %>%
    dplyr::group_by(id_n) %>%
    dplyr::mutate(
      remeasured_later = {
        n  <- dplyr::n()
        rl <- logical(n)
        for (i in seq_len(n)) {
          rl[i] <- any(has_diam[seq_len(n) > i & census_rank > census_rank[i]])
        }
        rl
      },
      stem_vital_status = dplyr::case_when(
        stem_vital_status == "presumed_dead" & remeasured_later ~ "alive",
        TRUE                                                    ~ stem_vital_status
      ),
      evidence_source = dplyr::if_else(
        stem_vital_status == "alive" & remeasured_later & !has_diam,
        paste0(evidence_source, " [corrected: remeasured at later census]"),
        evidence_source
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(missing = obs_missing | flag_k)

  # ── 11. Add census subplot IDs and build output table ─────────────────────

  status_for_db <- status_per_census %>%
    dplyr::left_join(
      census_subplot_ids %>%
        dplyr::select(id_sub_plots, id_table_liste_plots, census_name),
      by = c("id_table_liste_plots", "census_name")
    ) %>%
    dplyr::left_join(
      ind_base %>% dplyr::select(id_n, plot_name, tag),
      by = "id_n"
    ) %>%
    dplyr::select(
      id_n, id_table_liste_plots, id_sub_plots, plot_name, tag,
      census_name, census_date, stem_vital_status, missing, evidence_source
    ) %>%
    dplyr::mutate(evidence_source = stringr::str_squish(evidence_source))

  message(sprintf(
    "Status summary — alive: %d | dead: %d | presumed_dead: %d | NA: %d",
    sum(status_for_db$stem_vital_status == "alive",         na.rm = TRUE),
    sum(status_for_db$stem_vital_status == "dead",          na.rm = TRUE),
    sum(status_for_db$stem_vital_status == "presumed_dead", na.rm = TRUE),
    sum(is.na(status_for_db$stem_vital_status))
  ))

  if (!add_data) return(status_for_db)

  # ── 12. DB upsert (delete existing + insert new) ──────────────────────────

  stem_status_trait <- DBI::dbGetQuery(
    con, "SELECT id_trait FROM traitlist WHERE trait = 'stem_status'"
  )
  if (nrow(stem_status_trait) == 0)
    stop("Trait 'stem_status' not found in traitlist. Run add_trait() first.")
  stem_status_trait_id <- stem_status_trait$id_trait

  rows_to_insert <- status_for_db %>%
    dplyr::filter(!is.na(stem_vital_status), !is.na(id_sub_plots))

  if (dry_run) {
    existing <- query_individual_features(
      individual_ids = individual_ids,
      trait_ids      = stem_status_trait_id,
      format         = "long",
      issues         = "include",
      con            = con
    )
    message(sprintf(
      "[dry_run] Would delete %d existing stem_status record(s) and insert %d new one(s).",
      nrow(existing),
      nrow(rows_to_insert)
    ))
    return(invisible(status_for_db))
  }

  message("Deleting existing stem_status records for the given individuals...")
  safe_delete_individual_features(
    individual_ids = individual_ids,
    trait_ids      = stem_status_trait_id,
    dry_run        = FALSE,
    force          = TRUE,
    con            = con
  )

  if (nrow(rows_to_insert) == 0) {
    message("No rows to insert (all statuses are NA or lack a census subplot link).")
    return(invisible(status_for_db))
  }

  message(sprintf("Inserting %d stem_status record(s)...", nrow(rows_to_insert)))
  add_traits_measures(
    new_data          = rows_to_insert %>%
      dplyr::select(id_n, stem_vital_status, evidence_source, id_sub_plots),
    id_individual_col = "id_n",
    traits_field      = "stem_vital_status",
    features_field    = "evidence_source",
    id_sub_plots_col  = "id_sub_plots",
    add_data          = TRUE
  )

  invisible(status_for_db)
}
