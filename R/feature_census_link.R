# Which individual features belong to a census
#
# A measurement in `data_traits_measures` may carry `id_sub_plots` — the census
# it was taken during — or leave it NULL. The choice is not cosmetic:
# `query_plots(show_multiple_census = TRUE)` pivots census-linked features into
# one column per census (`stem_diameter_census1`, `stem_diameter_census2`, ...),
# so linking a feature declares that its value belongs to a moment in time.
#
# For a diameter that is exactly right. For a quadrat it is wrong: the tree does
# not move between censuses, and linking it would produce `quadrat_census1`,
# `quadrat_census2` ... repeating one unchanging value, while making the stem
# look as though it were re-located each campaign.


#' Features that are never attached to a census
#'
#' Where a stem sits, what kind of plant it is and what was determined from it
#' in a laboratory are properties of the tree, not of the campaign that
#' measured it. This is the built-in default used when the database does not
#' state a policy of its own.
#'
#' Every feature listed is also unlinked in every row already recorded —
#' `quadrat`, for instance, 0 of 147,894 — and the classification was confirmed
#' against [census_link_evidence()] rather than inferred from the counts alone.
#' Anything absent from this list is attached to the census.
#'
#' @return Named character vector, feature name to `"never"`.
#' @keywords internal
#' @export
.default_census_link_policy <- function() {
  never <- c(
    # position within the plot
    "quadrat", "position_x", "position_y",
    "position_x_iphone", "position_y_iphone",
    "position_x_moasure", "position_y_moasure",
    # position along a transect
    "position_transect", "transect_part", "transect_section",
    # what kind of plant it is
    "strate_cat", "abundance_coeff", "reproductive_state",
    "growth_form_level_1", "growth_form_level_2", "growth_form_level_3",
    "life_history_level_1", "life_history_level_2",
    # measured once, off the plot
    "plant_height", "spinescence", "twig_fresh_volume",
    "leaf_C_percentage", "leaf_N_percentage", "leaf_P_percentage",
    "leaf_K_percentage", "leaf_Ca_percentage", "leaf_delta_carbon_13",
    # who recorded it
    "colnam"
  )
  stats::setNames(rep("never", length(never)), never)
}


#' Should a feature be attached to the census that recorded it?
#'
#' Answers for each named feature. The database has the last word: if
#' `traitlist` carries a `census_link` column, that is the policy, and the
#' built-in default fills in only where the column says nothing. Without the
#' column — or without a connection — the default stands alone.
#'
#' Anything not named is `"always"`. A census import exists to record what was
#' measured during a campaign, so attaching the measurement is the behaviour
#' that has to be argued out of, not into.
#'
#' @param features Character vector of feature (trait) names.
#' @param con Optional database connection or pool.
#'
#' @return Named character vector parallel to `features`, each `"always"` or
#'   `"never"`.
#' @keywords internal
#' @export
.feature_census_link <- function(features, con = NULL) {
  features <- as.character(features)
  policy <- stats::setNames(rep("always", length(features)), features)
  if (length(features) == 0) return(policy)

  defaults <- .default_census_link_policy()
  known <- features %in% names(defaults)
  policy[known] <- unname(defaults[features[known]])

  if (is.null(con)) return(policy)

  declared <- tryCatch({
    actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
    on.exit({
      if (inherits(con, "Pool")) pool::poolReturn(actual_con)
    }, add = TRUE)

    has_column <- nrow(DBI::dbGetQuery(actual_con, "
      SELECT 1 FROM information_schema.columns
       WHERE table_schema = 'public' AND table_name = 'traitlist'
         AND column_name = 'census_link'")) > 0

    if (!has_column) {
      NULL
    } else {
      DBI::dbGetQuery(actual_con, "
        SELECT trait, census_link FROM traitlist
         WHERE census_link IS NOT NULL")
    }
  }, error = function(e) {
    message("Note: could not read the census link policy (", conditionMessage(e),
            "). Using the built-in default.")
    NULL
  })

  if (is.null(declared) || nrow(declared) == 0) return(policy)

  declared <- declared[declared$census_link %in% c("always", "never"), , drop = FALSE]
  hit <- match(features, declared$trait)
  policy[!is.na(hit)] <- declared$census_link[hit[!is.na(hit)]]

  policy
}


#' What the recorded data says about census links, feature by feature
#'
#' Reports how often each feature is already attached to a census, so the
#' policy can be settled against the data rather than from memory. A feature at
#' 0% has never been treated as belonging to a campaign; one at 100% always
#' has; anything in between is usually old data loaded before censuses were
#' recorded rather than a genuine ambiguity.
#'
#' @param con Database connection or pool.
#'
#' @return Data frame with `trait`, `category`, `n`, `n_linked`, `pct_linked`
#'   and the `policy` currently in force, ordered by volume.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' census_link_evidence(con)
#' }
#'
#' @export
census_link_evidence <- function(con) {
  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool")) pool::poolReturn(actual_con)
  }, add = TRUE)

  res <- DBI::dbGetQuery(actual_con, "
    SELECT t.trait, t.category,
           count(*) AS n,
           count(m.id_sub_plots) AS n_linked
      FROM data_traits_measures m
      JOIN traitlist t ON t.id_trait = m.traitid
     GROUP BY t.trait, t.category
     ORDER BY count(*) DESC")

  if (nrow(res) == 0) return(res)

  res$pct_linked <- round(100 * res$n_linked / res$n, 1)
  res$policy <- unname(.feature_census_link(res$trait, actual_con))
  res
}


#' Drop the census link from features that do not belong to a census
#'
#' Sets `id_sub_plots` back to `NA` on every row whose `trait_name` the policy
#' calls `"never"`, and says which features were left out. Rows for any other
#' feature, and rows that carry no census to begin with, are returned
#' untouched.
#'
#' @param data Data frame with `trait_name` and `id_sub_plots` columns.
#' @param policy Named character vector as returned by [.feature_census_link()].
#' @param quiet Logical. Suppress the message naming the features left out.
#'
#' @return `data`, with `id_sub_plots` cleared where the policy says `"never"`.
#' @keywords internal
#' @export
.unlink_never_features <- function(data, policy, quiet = FALSE) {
  if (!all(c("trait_name", "id_sub_plots") %in% names(data))) return(data)
  if (length(policy) == 0 || !any(!is.na(data$id_sub_plots))) return(data)

  never <- names(policy)[policy == "never"]
  if (length(never) == 0) return(data)

  unlinked <- data$trait_name %in% never & !is.na(data$id_sub_plots)
  if (!any(unlinked)) return(data)

  data$id_sub_plots[unlinked] <- NA_integer_

  if (!quiet) {
    left_out <- paste(sort(unique(data$trait_name[unlinked])), collapse = ", ")
    cli::cli_alert_info(
      "{sum(unlinked)} measurement{?s} recorded without a census link ({left_out})")
  }

  data
}


#' The features in a set that are never attached to a census
#'
#' A convenience over an already-read policy, for callers deciding what to
#' offer rather than what to write. An empty policy — not read yet, or
#' unreadable — names nothing, leaving the caller's default in place.
#'
#' @param features Character vector of feature (trait) names.
#' @param policy Named character vector as returned by [.feature_census_link()].
#'
#' @return Character vector, the subset of `features` the policy calls
#'   `"never"`.
#' @keywords internal
#' @export
.never_linked_features <- function(features, policy) {
  features <- unique(as.character(features))
  if (length(features) == 0 || length(policy) == 0) return(character(0))

  known <- intersect(features, names(policy))
  known[unname(policy[known]) == "never"]
}
