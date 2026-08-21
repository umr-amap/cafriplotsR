# Shared rendering of "what features this record already has".
#
# Both the update app (one plot or one individual) and the feature wizard (the
# plots picked in step 1) show the same thing: one row per feature, how many
# records back it, and what an extraction would show for it. The data comes
# from .upd_feature_summary(); the wording and the table live here so the two
# apps cannot drift apart.

#' How an extraction treats one feature, in words
#'
#' The rule comes from `.upd_agg_rule()` and says what the extraction *does*.
#' A feature backed by a single record still goes through that rule, so the
#' single-record case is worded separately here rather than given its own rule.
#'
#' @param rule One of the `.upd_agg_rule()` values.
#' @param n Number of records backing the feature.
#' @param i18n A `shiny.i18n` translator (already resolved, not the reactive).
#' @return A single string.
#' @keywords internal
.feature_rule_label <- function(rule, n, i18n) {
  if (n == 1 && rule %in% c("mean", "concat", "other")) {
    return(i18n$t("one record, shown as it is"))
  }
  switch(
    rule,
    mean       = sprintf(i18n$t("mean of %d records"), n),
    concat     = sprintf(i18n$t("%d records joined into one text"), n),
    per_census = sprintf(i18n$t("one value per census, from %d records"), n),
    census     = i18n$t("not a value: n_census, first_census, last_census, date_census_N"),
    not_extracted = i18n$t("not carried into extracted tables"),
    sprintf(i18n$t("%d record(s)"), n)
  )
}

#' `.feature_rule_label()` over vectors
#' @keywords internal
.feature_rule_labels <- function(rules, ns_records, i18n) {
  vapply(seq_along(rules),
         function(i) .feature_rule_label(rules[i], ns_records[i], i18n),
         character(1))
}

#' The feature overview table
#'
#' @param summary A `.upd_feature_summary()` result. A `plot_name` column, when
#'   present, becomes the first column so several plots can be shown at once.
#' @param i18n A `shiny.i18n` translator (already resolved).
#' @param page_length Rows per page.
#' @return A `DT::datatable`, with rows backed by several records highlighted.
#' @keywords internal
.feature_overview_dt <- function(summary, i18n, page_length = 10) {
  shown <- summary
  shown$stored_as <- .feature_rule_labels(shown$agg_rule, shown$n_records, i18n)
  shown$aggregate_display <- ifelse(is.na(shown$aggregate_display), "-",
                                    shown$aggregate_display)

  cols <- c("feature", "valuetype", "unit", "n_records",
            "aggregate_display", "stored_as")
  labels <- c(i18n$t("Feature"), i18n$t("Value type"), i18n$t("Unit"),
              i18n$t("Records"), i18n$t("Value in extracted table"),
              i18n$t("In an extracted table"))

  if ("plot_name" %in% names(shown)) {
    cols <- c("plot_name", cols)
    labels <- c(i18n$t("Plot"), labels)
  }

  DT::datatable(
    shown[, cols, drop = FALSE],
    selection = "none", rownames = FALSE,
    colnames = labels,
    options = list(pageLength = page_length, scrollX = TRUE, dom = "tip")
  ) %>%
    DT::formatStyle(
      "n_records", target = "row",
      backgroundColor = DT::styleInterval(1, c("transparent", "#fff3cd"))
    )
}
