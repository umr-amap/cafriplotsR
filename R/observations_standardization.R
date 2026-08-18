# =============================================================================
# Observations standardization
# =============================================================================
# Turn the free-text 'observations' trait (id_trait = 13) into standardized
# rows for two target traits:
#   - mortality_risk_flag : categorical, multi-valued per individual x census
#                           (one DB row per token)
#   - dawkins_index       : id_trait = 15, single value per individual x census
#                           (existing values are NEVER overwritten)
#
# The raw 'observations' rows are left untouched. Standardization is driven
# by an editable regex ontology shipped with the package
# (inst/ontology/observations_ontology.csv).
#
# Mirrors the workflow of compute_stem_vital_status(): compute -> review ->
# upsert, with dry_run support.
# =============================================================================


#' Standardize free-text observations into mortality and dawkins flags
#'
#' Parses the free-text \code{observations} trait (id_trait = 13) of the given
#' individuals, splits multi-observation entries into atomic phrases, and
#' matches each phrase against a regex ontology to derive standardized rows
#' for the \code{mortality_risk_flag} trait (multi-valued) and the
#' \code{dawkins_index} trait (single-valued). The original \code{observations}
#' trait is not modified.
#'
#' Existing \code{dawkins_index} measurements are never overwritten; derived
#' dawkins values for individual x census combinations already present in the
#' DB are dropped from the output of the DB write (they remain in the returned
#' tibble flagged as \code{skip_existing = TRUE}).
#'
#' @section Ontology:
#' By default the function reads
#' \code{system.file("ontology", "observations_ontology.csv", package = "CafriplotsR")}.
#' Columns expected: \code{trait}, \code{std_value}, \code{pattern}. Patterns
#' are case-insensitive Perl regexes. Provide \code{ontology} (a data frame or
#' a path) to override.
#'
#' @param individual_ids Integer vector of individual IDs.
#' @param ontology A data frame with columns
#'   \code{trait, std_value, pattern}, or a path to a CSV with those columns.
#'   Defaults to the package ontology.
#' @param add_data Logical. If \code{TRUE}, upsert the derived rows into the
#'   database. Default \code{FALSE}.
#' @param dry_run Logical. When \code{add_data = TRUE}, preview without
#'   committing changes. Default \code{TRUE}.
#' @param mortality_trait_name Name of the categorical trait that receives
#'   mortality risk tokens. Default \code{"mortality_risk_flag"}.
#' @param dawkins_trait_id Trait ID of the dawkins trait. Default \code{15L}.
#' @param obs_trait_id Trait ID of the free-text observations source.
#'   Default \code{13L}.
#' @param flag1_trait_id Trait ID of \code{flag1_rainfor} (single-letter
#'   alive-stem condition code). Default \code{19L}. Codes are decoded with
#'   the OpenForis mapping (\code{.default_observation_flags}) and appended to
#'   the mortality rows derived from free text. Rows are de-duplicated per
#'   (id_n, id_sub_plots, std_value); the \code{source_phrases} column records
#'   whether a row came from text, from a flag, or both.
#' @param con Database connection. Defaults to \code{call.mydb()}.
#'
#' @return A tibble with one row per (\code{id_n}, \code{census_name},
#'   \code{trait}, \code{std_value}):
#' \describe{
#'   \item{\code{id_n}}{Individual ID.}
#'   \item{\code{id_table_liste_plots}}{Plot ID.}
#'   \item{\code{id_sub_plots}}{Census subplot ID (used for DB linking).}
#'   \item{\code{plot_name}, \code{tag}}{Plot name and stem tag.}
#'   \item{\code{census_name}, \code{census_date}}{Census label and date.}
#'   \item{\code{trait}}{Target trait — \code{"mortality_risk_flag"} or
#'     \code{"dawkins_index"}.}
#'   \item{\code{std_value}}{Standardized token.}
#'   \item{\code{source_phrases}}{The raw phrase(s) that triggered the match.}
#'   \item{\code{full_observation}}{The full original observations string.}
#'   \item{\code{skip_existing}}{Logical — \code{TRUE} for dawkins rows whose
#'     individual x census already has a dawkins value in the DB (these rows
#'     are skipped on write).}
#' }
#' Additionally, the attribute \code{"unresolved"} on the returned tibble
#' holds a tibble of phrases (with counts) that matched no pattern — useful
#' for growing the ontology.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' plots <- query_plots(method = "1ha-IRD", extract_individuals = TRUE,
#'                      country = "CAMEROON")
#' res <- standardize_observations(individual_ids = plots$individuals$id_n,
#'                                 con = con)
#' attr(res, "unresolved")
#'
#' # Preview the upsert
#' standardize_observations(individual_ids = plots$individuals$id_n,
#'                          add_data = TRUE, dry_run = TRUE, con = con)
#'
#' # Commit
#' standardize_observations(individual_ids = plots$individuals$id_n,
#'                          add_data = TRUE, dry_run = FALSE, con = con)
#' }
#'
#' @export
standardize_observations <- function(
    individual_ids,
    ontology              = NULL,
    add_data              = FALSE,
    dry_run               = TRUE,
    mortality_trait_name  = "mortality_risk_flag",
    dawkins_trait_id      = 15L,
    obs_trait_id          = 13L,
    flag1_trait_id        = 19L,
    con                   = NULL
) {

  if (is.null(con)) con <- call.mydb()

  if (length(individual_ids) == 0) {
    message("No individual IDs provided.")
    return(invisible(dplyr::tibble()))
  }
  individual_ids <- unique(as.integer(individual_ids))

  # -- 1. Load ontology -------------------------------------------------------

  onto <- .load_observations_ontology(ontology)
  if (nrow(onto) == 0)
    stop("Ontology is empty. Check the CSV file.")

  # Resolve mortality_risk_flag trait id (may not exist yet)
  mortality_trait_id <- .resolve_mortality_trait_id(con, mortality_trait_name)

  # -- 2. Fetch raw observations + individual metadata -----------------------

  ind_base <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT i.id_n,
            i.id_table_liste_plots_n AS id_table_liste_plots,
            i.tag,
            p.plot_name
     FROM data_individuals i
     LEFT JOIN data_liste_plots p ON i.id_table_liste_plots_n = p.id_liste_plots
     WHERE i.id_n IN ({individual_ids*})",
    individual_ids = individual_ids, .con = con
  )) %>% dplyr::as_tibble()

  if (nrow(ind_base) == 0) {
    message("No individuals found for the provided IDs.")
    return(invisible(dplyr::tibble()))
  }

  obs_long <- query_individual_features(
    individual_ids       = individual_ids,
    trait_ids            = obs_trait_id,
    include_multi_census = TRUE,
    format               = "long",
    issues               = "include",
    con                  = con
  )

  if (nrow(obs_long) == 0) {
    message("No 'observations' records found for these individuals.")
    return(invisible(dplyr::tibble()))
  }

  obs_long <- obs_long %>%
    dplyr::rename(id_n = id_data_individuals) %>%
    dplyr::mutate(
      traitvalue_char = trimws(traitvalue_char),
      census_date = suppressWarnings(lubridate::dmy(paste(
        dplyr::coalesce(census_day,   1L),
        dplyr::coalesce(census_month, 1L),
        census_year,
        sep = "-"
      )))
    ) %>%
    dplyr::filter(
      !is.na(traitvalue_char),
      nchar(traitvalue_char) > 0,
      !is.na(census_name)
    )

  if (nrow(obs_long) == 0) {
    message("No non-empty observations to standardize.")
    return(invisible(dplyr::tibble()))
  }

  message(sprintf("Parsing %d observation rows across %d individual(s).",
                  nrow(obs_long), dplyr::n_distinct(obs_long$id_n)))

  # -- 3. Split into atomic phrases ------------------------------------------

  phrases <- .parse_obs_phrases(obs_long)

  # -- 4. Classify each phrase ------------------------------------------------

  classified <- .classify_phrases(phrases, onto)

  matched   <- classified$matched
  unmatched <- classified$unmatched

  if (nrow(matched) == 0) {
    message("No phrases matched the ontology.")
    out <- dplyr::tibble(
      id_n = integer(), id_table_liste_plots = integer(),
      id_sub_plots = integer(), plot_name = character(), tag = character(),
      census_name = character(), census_date = as.Date(character()),
      trait = character(), std_value = character(),
      source_phrases = character(), full_observation = character(),
      skip_existing = logical()
    )
    attr(out, "unresolved") <- .summarize_unresolved(unmatched)
    return(out)
  }

  # -- 5. Aggregate to one row per (id_n, census, trait, std_value) ---------

  derived <- matched %>%
    dplyr::group_by(id_n, id_sub_plots, census_name, trait, std_value) %>%
    dplyr::summarise(
      source_phrases   = paste0("text: ",
                                paste(unique(phrase), collapse = "; ")),
      full_observation = paste(unique(full_text), collapse = " || "),
      .groups          = "drop"
    )

  # Dawkins: keep one value per (id_n, census). If several were derived from
  # the same observation, keep the first in ontology priority (i.e. the row
  # order of the ontology CSV).
  ontology_order <- onto %>%
    dplyr::filter(trait == "dawkins_class") %>%
    dplyr::mutate(prio = dplyr::row_number()) %>%
    dplyr::select(std_value, prio)

  derived_dawkins <- derived %>%
    dplyr::filter(trait == "dawkins_class") %>%
    dplyr::left_join(ontology_order, by = "std_value") %>%
    dplyr::group_by(id_n, id_sub_plots, census_name) %>%
    dplyr::arrange(prio, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-prio) %>%
    dplyr::mutate(trait = "dawkins_index")  # use the canonical trait name

  derived_mort <- derived %>%
    dplyr::filter(trait == "mortality_risk_flag")

  # -- 5b. Fold in flag1_rainfor (trait 19) ----------------------------------
  # Codes are single letters mapped via the OpenForis observation-flag table.
  # Flag-derived rows are tagged in source_phrases so the review panel shows
  # provenance; text + flag agreeing on the same token collapse to one row.

  derived_flag <- .derive_mortality_from_flag1(
    individual_ids = individual_ids,
    flag_trait_id  = flag1_trait_id,
    con            = con
  )

  if (nrow(derived_flag) > 0) {
    message(sprintf(
      "Fetched flag1_rainfor: %d row(s) decoded into mortality_risk_flag tokens.",
      nrow(derived_flag)
    ))
    derived_mort <- dplyr::bind_rows(derived_mort, derived_flag) %>%
      dplyr::group_by(id_n, id_sub_plots, census_name, trait, std_value) %>%
      dplyr::summarise(
        source_phrases   = paste(unique(source_phrases), collapse = " | "),
        full_observation = paste(unique(stats::na.omit(full_observation)),
                                 collapse = " || "),
        .groups          = "drop"
      )
  }

  derived <- dplyr::bind_rows(derived_mort, derived_dawkins)

  # -- 6. Enrich with plot / tag / date / id_table_liste_plots ---------------

  enrich <- obs_long %>%
    dplyr::distinct(id_n, id_table_liste_plots, id_sub_plots,
                    census_name, census_date)

  out <- derived %>%
    dplyr::left_join(enrich, by = c("id_n", "id_sub_plots", "census_name")) %>%
    dplyr::left_join(ind_base %>% dplyr::select(id_n, plot_name, tag),
                     by = "id_n")

  # -- 7. Flag skip_existing for dawkins (never overwrite) -------------------

  existing_dawkins <- query_individual_features(
    individual_ids = individual_ids,
    trait_ids      = dawkins_trait_id,
    format         = "long",
    issues         = "include",
    include_multi_census = TRUE,
    con            = con
  )

  if (nrow(existing_dawkins) > 0) {
    have_dawkins <- existing_dawkins %>%
      dplyr::rename(id_n = id_data_individuals) %>%
      dplyr::filter(!is.na(id_sub_plots)) %>%
      dplyr::distinct(id_n, id_sub_plots) %>%
      dplyr::mutate(has_existing = TRUE)
  } else {
    have_dawkins <- dplyr::tibble(id_n = integer(), id_sub_plots = integer(),
                                   has_existing = logical())
  }

  out <- out %>%
    dplyr::left_join(have_dawkins, by = c("id_n", "id_sub_plots")) %>%
    dplyr::mutate(
      skip_existing = dplyr::if_else(
        trait == "dawkins_index" & dplyr::coalesce(has_existing, FALSE),
        TRUE, FALSE
      )
    ) %>%
    dplyr::select(-has_existing) %>%
    dplyr::select(id_n, id_table_liste_plots, id_sub_plots, plot_name, tag,
                  census_name, census_date, trait, std_value,
                  source_phrases, full_observation, skip_existing)

  attr(out, "unresolved") <- .summarize_unresolved(unmatched)

  n_mort   <- sum(out$trait == "mortality_risk_flag")
  n_daw    <- sum(out$trait == "dawkins_index")
  n_skip   <- sum(out$skip_existing)
  message(sprintf(
    "Derived: %d mortality_risk_flag token(s) | %d dawkins_index value(s) (%d would be skipped — existing) | %d unresolved phrase pattern(s)",
    n_mort, n_daw, n_skip,
    nrow(attr(out, "unresolved"))
  ))

  if (!add_data) return(out)

  # -- 8. DB upsert ----------------------------------------------------------

  .write_standardized_observations(
    out                = out,
    mortality_trait_id = mortality_trait_id,
    dawkins_trait_id   = dawkins_trait_id,
    obs_trait_id       = obs_trait_id,
    dry_run            = dry_run,
    con                = con
  )

  invisible(out)
}


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' @keywords internal
.load_observations_ontology <- function(ontology = NULL) {
  if (is.null(ontology)) {
    ontology <- system.file("ontology", "observations_ontology.csv",
                            package = "CafriplotsR")
    if (!nzchar(ontology) || !file.exists(ontology)) {
      # Fallback for devtools::load_all()
      ontology <- file.path("inst", "ontology",
                            "observations_ontology.csv")
    }
  }
  if (is.character(ontology) && length(ontology) == 1L) {
    if (!file.exists(ontology))
      stop("Ontology file not found: ", ontology)
    onto <- utils::read.csv(ontology, stringsAsFactors = FALSE,
                            encoding = "UTF-8")
  } else if (is.data.frame(ontology)) {
    onto <- ontology
  } else {
    stop("`ontology` must be NULL, a file path, or a data frame.")
  }
  required <- c("trait", "std_value", "pattern")
  missing  <- setdiff(required, names(onto))
  if (length(missing) > 0)
    stop("Ontology is missing column(s): ",
         paste(missing, collapse = ", "))

  # PCRE's default `\b` treats accented chars (é, è, à, …) as non-word, so
  # `\bcouché\b` does NOT match. Rewrite \b into a unicode-safe boundary
  # using POSIX [[:alpha:]] lookarounds. Authors can still write `\b` in the
  # CSV.
  onto$pattern <- gsub("\\\\b", "(?<![[:alpha:]])",
                       onto$pattern, perl = FALSE, fixed = FALSE)
  # The above replaces every \b — but the closing \b must be a lookahead, not
  # a lookbehind. Detect closing boundaries (those followed by a non-`[`
  # character or end-of-pattern) by rewriting in two passes:
  # Step 1 already replaced ALL \b with (?<![[:alpha:]]).
  # Step 2: any (?<![[:alpha:]]) that follows a literal letter / closing
  # bracket / closing paren should become (?![[:alpha:]]).
  # Simpler: do it in one tokenized pass.
  onto$pattern <- vapply(onto$pattern, .fix_word_boundaries,
                         character(1), USE.NAMES = FALSE)

  dplyr::as_tibble(onto)
}


#' Convert `\b` to unicode-safe (?<![[:alpha:]]) / (?![[:alpha:]])
#' depending on whether the boundary is at the start or end of a token.
#' Called from \code{.load_observations_ontology}.
#' @keywords internal
.fix_word_boundaries <- function(pattern) {
  # The previous gsub turned every \b into (?<![[:alpha:]]). For boundaries
  # that should be lookaheads (end-of-token), replace them.
  # A trailing boundary is one that comes AFTER a token-end char:
  #   ] ) literal letter * + ? or the end of the (?<!) we just inserted.
  # Practical heuristic: scan once and toggle leading/trailing.
  open  <- "(?<![[:alpha:]])"
  close <- "(?![[:alpha:]])"
  parts <- strsplit(pattern, open, fixed = TRUE)[[1]]
  if (length(parts) <= 1) return(pattern)
  out <- parts[1]
  # Boundaries alternate: position-0 (start) is leading, position-1 trailing,
  # etc. But alternation inside the pattern (e.g. `a|\bb\b`) breaks the simple
  # toggle. Robust approach: look at the character immediately before the
  # boundary. If it's a letter/]/)/quantifier, it's a trailing boundary.
  for (i in seq_along(parts)[-1L]) {
    prev_chunk <- parts[i - 1L]
    last_ch <- substr(prev_chunk, nchar(prev_chunk), nchar(prev_chunk))
    is_trailing <- nzchar(last_ch) &&
      grepl("[[:alpha:]0-9\\])\\?\\+\\*]", last_ch, perl = TRUE)
    boundary <- if (is_trailing) close else open
    out <- paste0(out, boundary, parts[i])
  }
  out
}


#' @keywords internal
.resolve_mortality_trait_id <- function(con, trait_name) {
  res <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT id_trait FROM traitlist WHERE trait = {trait_name}",
    trait_name = trait_name, .con = con
  ))
  if (nrow(res) == 0) return(NA_integer_)
  as.integer(res$id_trait[1])
}


#' Split free-text observations into atomic phrases
#'
#' Splits on \code{;} or \code{, } (comma followed by whitespace) to keep
#' decimals such as \code{2,5} intact.
#'
#' @keywords internal
.parse_obs_phrases <- function(obs_long) {
  obs_long %>%
    dplyr::transmute(
      id_n,
      id_sub_plots,
      census_name,
      full_text = traitvalue_char,
      text_low  = tolower(traitvalue_char)
    ) %>%
    dplyr::mutate(
      phrase = strsplit(text_low, "\\s*;\\s*|,\\s+", perl = TRUE)
    ) %>%
    tidyr::unnest(phrase) %>%
    dplyr::mutate(phrase = trimws(phrase)) %>%
    dplyr::filter(nchar(phrase) > 0)
}


#' Classify phrases against the ontology
#' @keywords internal
.classify_phrases <- function(phrases, onto) {
  if (nrow(phrases) == 0)
    return(list(
      matched   = phrases[FALSE, ],
      unmatched = phrases[FALSE, ]
    ))

  # Build a long phrase x pattern result by running every pattern.
  matches <- vector("list", nrow(onto))
  for (i in seq_len(nrow(onto))) {
    hit <- grepl(onto$pattern[i], phrases$phrase,
                 perl = TRUE, ignore.case = TRUE)
    if (any(hit)) {
      matches[[i]] <- phrases[hit, , drop = FALSE]
      matches[[i]]$trait     <- onto$trait[i]
      matches[[i]]$std_value <- onto$std_value[i]
    }
  }
  matched <- dplyr::bind_rows(matches)

  if (nrow(matched) == 0) {
    return(list(matched = phrases[FALSE, ], unmatched = phrases))
  }

  # Drop duplicates: same (id_n, census, phrase, trait, std_value)
  matched <- dplyr::distinct(
    matched,
    id_n, id_sub_plots, census_name, phrase, full_text, trait, std_value
  )

  # Suppress wounded_unknown when the same phrase also matched a specific
  # wound (wounded_elephant / _exploitation / _human).
  specific_wounds <- matched %>%
    dplyr::filter(grepl("^wounded_", std_value),
                  std_value != "wounded_unknown") %>%
    dplyr::distinct(id_n, id_sub_plots, census_name, phrase) %>%
    dplyr::mutate(.has_specific_wound = TRUE)
  if (nrow(specific_wounds) > 0) {
    matched <- matched %>%
      dplyr::left_join(specific_wounds,
                       by = c("id_n", "id_sub_plots",
                              "census_name", "phrase")) %>%
      dplyr::filter(!(std_value == "wounded_unknown" &
                        dplyr::coalesce(.has_specific_wound, FALSE))) %>%
      dplyr::select(-.has_specific_wound)
  }

  matched_phrase_keys <- unique(
    paste(matched$id_n, matched$census_name, matched$phrase, sep = "|@|")
  )
  phrase_keys <- paste(phrases$id_n, phrases$census_name, phrases$phrase,
                       sep = "|@|")
  unmatched <- phrases[!phrase_keys %in% matched_phrase_keys, , drop = FALSE]

  list(matched = matched, unmatched = unmatched)
}


#' Mapping from flag1_rainfor codes to mortality_risk_flag tokens
#'
#' Derived from \code{.default_observation_flags()} in
#' \code{openforis_processing.R}, inverted so single-letter codes map to
#' standardized tokens. Codes not in this table are kept as unresolved.
#'
#' @keywords internal
.flag1_to_mortality_map <- function() {
  c(
    z = "dying",
    c = "leaning",
    b = "broken_stem",
    d = "lying",
    y = "termites",
    f = "hollow",
    l = "large_liana_high_load",
    w = "wounded_human",
    m = "small_liana_high_load",
    s = "strangler",
    i = "defoliated_high"
  )
}


#' Decode flag1_rainfor (trait 19) into mortality_risk_flag rows
#'
#' Fetches the flag1_rainfor measurements for the given individuals and turns
#' each single-letter code into a mortality_risk_flag row. Returned rows have
#' the same column structure as the text-derived ones, with
#' \code{source_phrases} prefixed by \code{"flag1_rainfor: "}.
#'
#' @keywords internal
.derive_mortality_from_flag1 <- function(individual_ids, flag_trait_id, con) {

  empty <- dplyr::tibble(
    id_n             = integer(),
    id_sub_plots     = integer(),
    census_name      = character(),
    trait            = character(),
    std_value        = character(),
    source_phrases   = character(),
    full_observation = character()
  )

  flag_long <- tryCatch(
    query_individual_features(
      individual_ids       = individual_ids,
      trait_ids            = flag_trait_id,
      include_multi_census = TRUE,
      format               = "long",
      issues               = "include",
      con                  = con
    ),
    error = function(e) {
      message("Could not fetch flag1_rainfor (", e$message,
              "). Skipping flag-derived rows.")
      NULL
    }
  )

  if (is.null(flag_long) || nrow(flag_long) == 0) return(empty)

  flag_long <- flag_long %>%
    dplyr::rename(id_n = id_data_individuals) %>%
    dplyr::mutate(
      code = tolower(trimws(dplyr::coalesce(traitvalue_char,
                                            as.character(traitvalue))))
    ) %>%
    dplyr::filter(!is.na(census_name),
                  !is.na(id_sub_plots),
                  !is.na(code),
                  nchar(code) > 0)

  if (nrow(flag_long) == 0) return(empty)

  # A single flag1 cell can hold multiple letters (e.g. "bd") — split on any
  # non-letter and also into individual characters.
  flag_long <- flag_long %>%
    dplyr::mutate(letter = strsplit(code, "[^a-z]*", perl = TRUE)) %>%
    tidyr::unnest(letter) %>%
    dplyr::filter(nchar(letter) > 0) %>%
    dplyr::mutate(letter = strsplit(letter, "")) %>%
    tidyr::unnest(letter)

  mapping <- .flag1_to_mortality_map()
  flag_long <- flag_long %>%
    dplyr::mutate(std_value = unname(mapping[letter])) %>%
    dplyr::filter(!is.na(std_value))

  if (nrow(flag_long) == 0) return(empty)

  flag_long %>%
    dplyr::group_by(id_n, id_sub_plots, census_name, std_value) %>%
    dplyr::summarise(
      codes_seen = paste(sort(unique(letter)), collapse = ""),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      trait            = "mortality_risk_flag",
      source_phrases   = paste0("flag1_rainfor: ", codes_seen),
      full_observation = NA_character_
    ) %>%
    dplyr::select(id_n, id_sub_plots, census_name, trait, std_value,
                  source_phrases, full_observation)
}


#' Aggregate unresolved phrases for the review panel
#' @keywords internal
.summarize_unresolved <- function(unmatched) {
  if (nrow(unmatched) == 0)
    return(dplyr::tibble(phrase = character(), n = integer()))
  unmatched %>%
    dplyr::count(phrase, name = "n", sort = TRUE)
}


#' Upsert standardized values into data_traits_measures
#'
#' Skips dawkins rows flagged as \code{skip_existing}. Skips mortality rows
#' that already exist (same id_n + id_sub_plots + traitvalue_char). Inserts
#' the remainder.
#'
#' @keywords internal
.write_standardized_observations <- function(out,
                                             mortality_trait_id,
                                             dawkins_trait_id,
                                             obs_trait_id,
                                             dry_run,
                                             con) {

  to_write <- out %>%
    dplyr::filter(!skip_existing, !is.na(id_sub_plots),
                  !is.na(id_table_liste_plots))

  if (nrow(to_write) == 0) {
    message("Nothing to write.")
    return(invisible(NULL))
  }

  # Split per target trait
  mort <- to_write %>% dplyr::filter(trait == "mortality_risk_flag")
  daw  <- to_write %>% dplyr::filter(trait == "dawkins_index")

  # ── mortality_risk_flag: skip duplicates already in DB ───────────────────
  if (nrow(mort) > 0) {
    if (is.na(mortality_trait_id)) {
      message(
        "Trait 'mortality_risk_flag' is not yet defined in `traitlist`. ",
        "Run `bootstrap_mortality_risk_flag_trait(con)` once, then retry."
      )
      mort <- mort[FALSE, ]
    } else {
      existing_mort <- query_individual_features(
        individual_ids       = unique(mort$id_n),
        trait_ids            = mortality_trait_id,
        format               = "long",
        issues               = "include",
        include_multi_census = TRUE,
        con                  = con
      )
      if (nrow(existing_mort) > 0) {
        key_existing <- paste(existing_mort$id_data_individuals,
                              existing_mort$id_sub_plots,
                              tolower(trimws(existing_mort$traitvalue_char)),
                              sep = "|")
        key_new <- paste(mort$id_n, mort$id_sub_plots,
                         tolower(mort$std_value), sep = "|")
        mort <- mort[!key_new %in% key_existing, ]
      }
    }
  }

  # ── dawkins: skip rows where the individual x census already has any
  # dawkins value (handled by skip_existing upstream). Nothing more to do.

  n_mort_insert <- nrow(mort)
  n_daw_insert  <- nrow(daw)

  if (dry_run) {
    message(sprintf(
      "[dry_run] Would insert %d mortality_risk_flag row(s) and %d dawkins_index row(s). No DB changes made.",
      n_mort_insert, n_daw_insert
    ))
    return(invisible(NULL))
  }

  is_pool   <- inherits(con, "Pool")
  actual_con <- if (is_pool) pool::poolCheckout(con) else con
  on.exit(if (is_pool) pool::poolReturn(actual_con), add = TRUE)

  today_d <- as.integer(format(Sys.Date(), "%d"))
  today_m <- as.integer(format(Sys.Date(), "%m"))
  today_y <- as.integer(format(Sys.Date(), "%Y"))

  inserted_ids_all <- list()

  if (n_mort_insert > 0 && !is.na(mortality_trait_id)) {
    new_records <- data.frame(
      id_table_liste_plots = as.integer(mort$id_table_liste_plots),
      id_data_individuals  = as.integer(mort$id_n),
      id_sub_plots         = as.integer(mort$id_sub_plots),
      traitid              = as.integer(mortality_trait_id),
      traitvalue           = NA_real_,
      traitvalue_char      = as.character(mort$std_value),
      date_modif_d         = today_d,
      date_modif_m         = today_m,
      date_modif_y         = today_y,
      stringsAsFactors     = FALSE
    )
    inserted <- .execute_trait_insert_with_returning(new_records, actual_con)
    inserted_ids_all$mort <- list(ids = inserted, src = mort)
    message(sprintf("Inserted %d mortality_risk_flag row(s).", nrow(inserted)))
  }

  if (n_daw_insert > 0) {
    new_records <- data.frame(
      id_table_liste_plots = as.integer(daw$id_table_liste_plots),
      id_data_individuals  = as.integer(daw$id_n),
      id_sub_plots         = as.integer(daw$id_sub_plots),
      traitid              = as.integer(dawkins_trait_id),
      traitvalue           = NA_real_,
      traitvalue_char      = as.character(daw$std_value),
      date_modif_d         = today_d,
      date_modif_m         = today_m,
      date_modif_y         = today_y,
      stringsAsFactors     = FALSE
    )
    inserted <- .execute_trait_insert_with_returning(new_records, actual_con)
    inserted_ids_all$daw <- list(ids = inserted, src = daw)
    message(sprintf("Inserted %d dawkins_index row(s).", nrow(inserted)))
  }

  # Record provenance in data_ind_measures_feat as 'observations' (id 13)
  for (nm in names(inserted_ids_all)) {
    ids <- inserted_ids_all[[nm]]$ids
    src <- inserted_ids_all[[nm]]$src
    if (nrow(ids) == 0) next
    # Align order: .execute_trait_insert_with_returning returns rows in the
    # same order as the temp table input.
    feat_records <- data.frame(
      id_trait_measures = as.integer(ids$id_trait_measures),
      id_trait          = as.integer(obs_trait_id),
      typevalue         = NA_real_,
      typevalue_char    = as.character(
        paste0("standardize_observations: ", src$source_phrases)
      ),
      date_modif_d      = today_d,
      date_modif_m      = today_m,
      date_modif_y      = today_y,
      stringsAsFactors  = FALSE
    )
    DBI::dbWriteTable(actual_con, "data_ind_measures_feat", feat_records,
                      append = TRUE, row.names = FALSE)
  }

  invisible(NULL)
}



# bootstrap_mortality_risk_flag_trait <- function(con = NULL) {
# 
#   if (is.null(con)) con <- call.mydb()
# 
#   existing <- .resolve_mortality_trait_id(con, "mortality_risk_flag")
#   if (!is.na(existing)) {
#     message(sprintf(
#       "Trait 'mortality_risk_flag' already exists (id_trait = %d).", existing
#     ))
#     return(invisible(existing))
#   }
# 
#   factor_levels <- paste(
#     "broken_stem", "broken_crown",
#     "dying", "leaning", "lying", "uprooted",
#     "defoliated_high", "defoliated_low",
#     "large_liana_high_load", "small_liana_high_load",
#     "large_liana_low_load", "small_liana_low_load",
#     "strangler", "damaged_leaves", "hollow",
#     "wounded_elephant", "wounded_exploitation",
#     "wounded_human", "wounded_unknown",
#     "termites", "fungi", "burnt",
#     sep = ", "
#   )
# 
#   description <- paste(
#     "Mortality risk indicator derived from standardized free-text",
#     "observations. Multiple rows per individual x census allowed (one row",
#     "per token). Source phrases are recorded in data_ind_measures_feat",
#     "as 'observations'."
#   )
# 
#   is_pool    <- inherits(con, "Pool")
#   actual_con <- if (is_pool) pool::poolCheckout(con) else con
#   on.exit(if (is_pool) pool::poolReturn(actual_con), add = TRUE)
# 
#   insert_sql <- glue::glue_sql(
#     "INSERT INTO traitlist (trait, valuetype, factorlevels, traitdescription)
#      VALUES ({trait}, {vt}, {fl}, {desc}) RETURNING id_trait",
#     trait = "mortality_risk_flag",
#     vt    = "categorical",
#     fl    = factor_levels,
#     desc  = description,
#     .con  = actual_con
#   )
#   new_id <- DBI::dbGetQuery(actual_con, insert_sql)$id_trait[1]
#   message(sprintf(
#     "Inserted trait 'mortality_risk_flag' with id_trait = %d.", new_id
#   ))
#   invisible(as.integer(new_id))
# }
