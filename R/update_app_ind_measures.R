# Data Update App - adding a measurement to an individual
#
# A missing value (a diameter not recorded at one census, a height measured
# later) is a measurement that is not in data_traits_measures yet, so section 4
# has nothing to edit. Section 4b of the individual page adds it: one value at a
# time, staged in the app and written by section 5 in the same transaction as
# the other edits. Several individuals at once stay the feature wizard's job.
#
# The row is built the way the feature wizard builds it
# (.execute_measurements_import()), and checked the way its validation step
# checks a file: no second value for the same feature and census, no value
# outside the feature's allowed range, and a warning for a numeric value lower
# than at an earlier census.

#' Features offered when adding a measurement to an individual
#'
#' Features already measured on this individual come first, then every other
#' feature of `traitlist`.
#'
#' @param traits Data frame of `traitlist` rows, with at least `id_trait`,
#'   `trait` and `valuetype`.
#' @param measured Character vector of the features already recorded for the
#'   individual.
#' @return A tibble with the `traitlist` columns and `group` (`"measured"` or
#'   `"other"`), in display order.
#' @keywords internal
.upd_ind_trait_choices <- function(traits, measured = character(0)) {
  traits <- dplyr::as_tibble(traits)
  traits <- traits[!is.na(traits$trait), , drop = FALSE]
  traits$group <- ifelse(traits$trait %in% measured, "measured", "other")
  traits[order(match(traits$group, c("measured", "other")), traits$trait), ,
         drop = FALSE]
}

#' Censuses of a plot
#'
#' @param id_plot Integer(1), `data_liste_plots.id_liste_plots`.
#' @param con A DBI connection.
#' @return A data frame with `id_sub_plots`, `census` (its number), `year`,
#'   `month` and `day`, ordered by census number.
#' @keywords internal
.upd_plot_censuses <- function(id_plot, con) {
  sql <- glue::glue_sql(
    "SELECT sp.id_sub_plots, sp.typevalue AS census, sp.year, sp.month, sp.day
       FROM data_liste_sub_plots sp
       JOIN subplotype_list st ON sp.id_type_sub_plot = st.id_subplotype
      WHERE sp.id_table_liste_plots = {id_plot} AND st.type = 'census'
      ORDER BY sp.typevalue, sp.id_sub_plots",
    .con = con
  )
  DBI::dbGetQuery(con, sql)
}

#' Measurements already stored for an individual
#'
#' The ids the checks need, which the feature records of section 4 do not
#' carry: the feature id and the census of each measurement.
#'
#' @param id_ind Integer(1), `data_individuals.id_n`.
#' @param con A DBI connection.
#' @return A data frame with `id_trait_measures`, `traitid`, `id_sub_plots`,
#'   `traitvalue` and `traitvalue_char`.
#' @keywords internal
.upd_ind_stored_measures <- function(id_ind, con) {
  sql <- glue::glue_sql(
    "SELECT id_trait_measures, traitid, id_sub_plots, traitvalue, traitvalue_char
       FROM data_traits_measures
      WHERE id_data_individuals = {id_ind}",
    .con = con
  )
  DBI::dbGetQuery(con, sql)
}

#' Build the data_traits_measures row for a new measurement
#'
#' The value goes where the feature wizard puts it: `traitvalue` for
#' `numeric`, `integer` and `table_colnam` features, `traitvalue_char` for the
#' others. The plot, taxon and specimen links are copied from the individual,
#' as the wizard does.
#'
#' @param individual One-row data frame of `data_individuals` (`id_n`,
#'   `id_table_liste_plots_n`, and `id_diconame_n`, `id_specimen` when present).
#' @param trait One-row data frame of `traitlist` (`id_trait`, `valuetype`).
#' @param value The value, as typed.
#' @param id_sub_plots Integer(1) or `NA`. The census the value belongs to.
#' @param year,month,day Integer(1) or `NA`. Measurement date, when it differs
#'   from the census's.
#' @param issue Character(1) or `NA`.
#' @param plot_name Character(1) or `NA`, kept as `original_plot_name`.
#' @return A one-row data frame. Stops on an empty value, or on a value a
#'   numeric feature cannot hold.
#' @keywords internal
.upd_ind_measure_row <- function(individual, trait, value,
                                 id_sub_plots = NA_integer_,
                                 year = NA_integer_, month = NA_integer_,
                                 day = NA_integer_, issue = NA_character_,
                                 plot_name = NA_character_) {
  value <- if (is.null(value) || length(value) == 0) NA else value[[1]]
  value <- trimws(as.character(value))
  if (is.na(value) || !nzchar(value)) stop("No value given.", call. = FALSE)

  numeric_store <- !is.na(trait$valuetype) &&
    trait$valuetype %in% c("numeric", "integer", "table_colnam")
  num <- NA_real_
  if (numeric_store) {
    num <- suppressWarnings(as.numeric(value))
    if (is.na(num)) stop("Not a number: ", value, call. = FALSE)
  }

  col_or_na <- function(col, na) {
    if (col %in% names(individual)) individual[[col]][1] else na
  }
  int_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_integer_)
    suppressWarnings(as.integer(x[[1]]))
  }
  issue <- if (is.null(issue) || length(issue) == 0) NA_character_ else issue[[1]]
  if (!is.na(issue) && !nzchar(trimws(issue))) issue <- NA_character_

  today <- Sys.Date()
  data.frame(
    id_table_liste_plots = as.integer(individual$id_table_liste_plots_n[1]),
    id_data_individuals  = as.integer(individual$id_n[1]),
    id_diconame          = as.integer(col_or_na("id_diconame_n", NA_integer_)),
    id_specimen          = as.integer(col_or_na("id_specimen", NA_integer_)),
    id_sub_plots         = int_or_na(id_sub_plots),
    traitid              = as.integer(trait$id_trait[1]),
    traitvalue           = if (numeric_store) num else NA_real_,
    traitvalue_char      = if (numeric_store) NA_character_ else value,
    original_plot_name   = as.character(plot_name[1]),
    year                 = int_or_na(year),
    month                = int_or_na(month),
    day                  = int_or_na(day),
    issue                = as.character(issue),
    date_modif_d         = as.integer(format(today, "%d")),
    date_modif_m         = as.integer(format(today, "%m")),
    date_modif_y         = as.integer(format(today, "%Y")),
    stringsAsFactors     = FALSE
  )
}

#' Check a new measurement before it is staged
#'
#' Problems that stop the value from being staged:
#' * `census_missing`: the feature belongs to a census and none was chosen.
#' * `duplicate`: a value of this feature is already stored, or staged, for
#'   the same census (or, for a feature never linked to a census, at all).
#'   Edit the stored one in section 4 instead.
#' * `below_min` / `above_max`: outside the feature's allowed range.
#'
#' And one that only warns, recorded in `issue` the way `add_traits_measures()`
#' records it:
#' * `lower_than_previous`: a numeric value lower than the largest value of
#'   the same feature at an earlier census.
#'
#' @param row One-row data frame from [.upd_ind_measure_row()].
#' @param trait One-row data frame of `traitlist` (`valuetype`, and
#'   `minallowedvalue`, `maxallowedvalue` when present).
#' @param policy `"always"` or `"never"`, from [.feature_census_link()].
#' @param stored Data frame from [.upd_ind_stored_measures()], or `NULL`.
#' @param staged Measurements already staged, or `NULL`.
#' @param censuses Data frame from [.upd_plot_censuses()], or `NULL`.
#' @return A list with `errors` and `warnings` (character vectors of the codes
#'   above) and `row`, with `id_sub_plots` cleared for a feature never linked
#'   to a census and `issue` completed for a warning.
#' @keywords internal
.upd_ind_measure_check <- function(row, trait, policy = "always",
                                   stored = NULL, staged = NULL,
                                   censuses = NULL) {
  errors <- character(0)
  warnings <- character(0)

  if (identical(policy, "never")) {
    row$id_sub_plots <- NA_integer_
  } else if (is.na(row$id_sub_plots)) {
    errors <- c(errors, "census_missing")
  }

  keys <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(data.frame(traitid = integer(0),
                                                      id_sub_plots = integer(0)))
    data.frame(traitid = as.integer(x$traitid),
               id_sub_plots = as.integer(x$id_sub_plots))
  }
  known <- rbind(keys(stored), keys(staged))
  same_trait <- known[known$traitid == row$traitid, , drop = FALSE]
  if (nrow(same_trait) > 0) {
    clash <- if (identical(policy, "never")) {
      TRUE
    } else {
      any(!is.na(same_trait$id_sub_plots) &
            same_trait$id_sub_plots == row$id_sub_plots, na.rm = TRUE)
    }
    if (clash) errors <- c(errors, "duplicate")
  }

  numeric_value <- !is.na(row$traitvalue)
  if (numeric_value) {
    bound <- function(col) {
      if (!col %in% names(trait)) return(NA_real_)
      suppressWarnings(as.numeric(trait[[col]][1]))
    }
    lo <- bound("minallowedvalue")
    hi <- bound("maxallowedvalue")
    if (!is.na(lo) && row$traitvalue < lo) errors <- c(errors, "below_min")
    if (!is.na(hi) && row$traitvalue > hi) errors <- c(errors, "above_max")
  }

  if (numeric_value && !is.na(row$id_sub_plots) && !is.null(stored) &&
      nrow(stored) > 0 && !is.null(censuses) && nrow(censuses) > 0) {
    number <- stats::setNames(as.numeric(censuses$census),
                              as.character(censuses$id_sub_plots))
    this <- number[as.character(row$id_sub_plots)]
    prev <- stored[as.integer(stored$traitid) == row$traitid &
                     !is.na(stored$id_sub_plots) & !is.na(stored$traitvalue), ,
                   drop = FALSE]
    if (!is.na(this) && nrow(prev) > 0) {
      prev_number <- number[as.character(prev$id_sub_plots)]
      earlier <- prev$traitvalue[!is.na(prev_number) & prev_number < this]
      if (length(earlier) > 0 && row$traitvalue < max(earlier)) {
        warnings <- c(warnings, "lower_than_previous")
        note <- "value lower than previous census"
        row$issue <- if (is.na(row$issue)) note else paste(row$issue, note, sep = "|")
      }
    }
  }

  list(errors = errors, warnings = warnings, row = row)
}

#' Label a staged measurement for display
#'
#' @param row One-row data frame from [.upd_ind_measure_check()].
#' @param trait Character(1). The feature name.
#' @param census Character(1). How the census reads, `NA` for none.
#' @param label Character(1). How the value reads (a person's name for a
#'   `table_colnam` feature), `NA` to show the stored value.
#' @param staged Measurements already staged, to continue their `.key`.
#' @return `row` with `.trait`, `.census`, `.label` and a unique `.key`.
#' @keywords internal
.upd_ind_measure_label <- function(row, trait, census = NA_character_,
                                   label = NA_character_, staged = NULL) {
  row$.trait <- trait
  row$.census <- census
  row$.label <- if (!is.na(label)) {
    label
  } else if (!is.na(row$traitvalue)) {
    format(row$traitvalue, trim = TRUE, scientific = FALSE)
  } else {
    row$traitvalue_char
  }
  last <- if (is.null(staged) || nrow(staged) == 0) 0L else max(staged$.key)
  row$.key <- last + 1L
  row
}

#' Write staged measurements
#'
#' Runs inside the caller's transaction ([.upd_apply_all()]).
#'
#' @param add `NULL`, or staged rows (display columns, prefixed with `.`,
#'   included).
#' @param con A DBI connection (not a pool).
#' @return The number of rows inserted.
#' @keywords internal
.upd_apply_ind_measures <- function(add, con) {
  if (is.null(add) || nrow(add) == 0) return(0L)
  add <- as.data.frame(add)[, !startsWith(names(add), "."), drop = FALSE]
  as.integer(.db_append_table(con, "data_traits_measures", add))
}
