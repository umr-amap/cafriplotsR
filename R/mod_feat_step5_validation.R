# Feature Wizard - Step 5: Validation & Preview
#
# Module for validating prepared feature data before import.

#' Feature Wizard Step 5: Validation - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step5_validation_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("clipboard-check"),
      i18n$t("Step 5: Validation & Preview"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::uiOutput(ns("validation_description")),

    shiny::actionButton(
      ns("run_validation"),
      shiny::tagList(shiny::icon("check-double"), " ", i18n$t("Run Validation")),
      class = "btn-primary btn-lg",
      style = "margin-bottom: 30px;"
    ),

    # Validation summary
    shiny::uiOutput(ns("validation_summary")),

    shiny::hr(),

    # Error/warning details
    shiny::uiOutput(ns("validation_details")),

    shiny::hr(),

    # Issue summary by trait (only shown for measurements mode)
    shiny::uiOutput(ns("issue_summary_ui")),

    # Offer to drop what the database already holds
    shiny::uiOutput(ns("existing_filter_ui")),

    # Data preview
    shiny::uiOutput(ns("preview_header")),
    DT::DTOutput(ns("import_preview_table"))
  )
}


#' Feature Wizard Step 5: Validation - Server
#'
#' @param id Module namespace ID
#' @param matched_data Reactive containing the matched feature data
#' @param feature_config Reactive containing the feature configuration
#' @param selected_plots Reactive containing selected plots data
#' @param operation_mode Reactive containing operation mode string
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive containing validation result
#' @keywords internal
#' @export
mod_feat_step5_validation_server <- function(id, matched_data, feature_config, selected_plots,
                                              operation_mode, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    validation_result <- shiny::reactiveVal(NULL)

    # For compute_stem_status and standardize_observations modes: auto-validate
    # as soon as data is ready (the user already reviewed and confirmed the
    # table in step 3)
    shiny::observe({
      shiny::req(matched_data(), feature_config())
      shiny::req(tryCatch(operation_mode(), error = function(e) NULL)
                 %in% c("compute_stem_status", "standardize_observations"))
      mode_val <- operation_mode()
      data <- matched_data()
      empty_errors   <- data.frame(row = integer(), column = character(),
                                   issue = character(), stringsAsFactors = FALSE)
      empty_warnings <- data.frame(row = integer(), column = character(),
                                   warning = character(), stringsAsFactors = FALSE)
      validation_result(list(
        valid    = TRUE,
        data     = data,
        errors   = empty_errors,
        warnings = empty_warnings,
        summary  = list(
          total_rows = nrow(data),
          errors     = 0L,
          warnings   = 0L
        ),
        mode     = mode_val
      ))
    })

    shiny::observeEvent(input$run_validation, {
      shiny::req(matched_data(), feature_config(), selected_plots(), con())

      shiny::withProgress({
        shiny::setProgress(0.3, message = i18n()$t("Validating data..."))

        data <- matched_data()
        config <- feature_config()
        plots <- selected_plots()
        mode <- operation_mode()

        errors <- data.frame(row = integer(), column = character(),
                             issue = character(), stringsAsFactors = FALSE)
        warnings <- data.frame(row = integer(), column = character(),
                               warning = character(), stringsAsFactors = FALSE)

        # Rows repeating a measurement the database already holds for the same
        # individual, feature and census. Kept as row numbers rather than a
        # count so the user can choose to drop them before importing.
        existing_rows <- integer(0)

        # 1. Validate plots exist and are accessible (skip for modes that use their own linking)
        if (mode %in% c("define_multi_stems", "compute_stem_status", "standardize_observations")) {
          # These modes don't use id_liste_plots in the feature data
        } else if (!"id_liste_plots" %in% names(data)) {
          errors <- rbind(errors, data.frame(
            row = 0, column = "id_liste_plots",
            issue = i18n()$t("Missing plot ID column. Data may not be properly linked."),
            stringsAsFactors = FALSE
          ))
        } else {
          missing_ids <- data$id_liste_plots[is.na(data$id_liste_plots)]
          if (length(missing_ids) > 0) {
            errors <- rbind(errors, data.frame(
              row = which(is.na(data$id_liste_plots)),
              column = "id_liste_plots",
              issue = i18n()$t("Plot ID is missing"),
              stringsAsFactors = FALSE
            ))
          }
        }

        # 2a. Measurements-specific validation
        if (mode == "add_measurements") {
          # Validate tag column exists
          if (!"tag" %in% names(data)) {
            errors <- rbind(errors, data.frame(
              row = 0, column = "tag",
              issue = i18n()$t("Missing tag column for individual matching."),
              stringsAsFactors = FALSE
            ))
          }

          # Validate trait IDs
          if ("traitid" %in% names(data)) {
            missing_traits <- which(is.na(data$traitid))
            if (length(missing_traits) > 0) {
              errors <- rbind(errors, data.frame(
                row = missing_traits, column = "traitid",
                issue = i18n()$t("Trait ID is missing (unmapped trait)"),
                stringsAsFactors = FALSE
              ))
            }
          } else {
            errors <- rbind(errors, data.frame(
              row = 0, column = "traitid",
              issue = i18n()$t("Missing trait ID column."),
              stringsAsFactors = FALSE
            ))
          }

          # Validate at least one value column has data
          has_num <- "traitvalue" %in% names(data) && any(!is.na(data$traitvalue))
          has_char <- "traitvalue_char" %in% names(data) && any(!is.na(data$traitvalue_char))
          if (!has_num && !has_char) {
            errors <- rbind(errors, data.frame(
              row = 0, column = "traitvalue",
              issue = i18n()$t("No measurement values found in data."),
              stringsAsFactors = FALSE
            ))
          }

          # Match individuals by plot_name + tag
          if ("tag" %in% names(data) && "id_liste_plots" %in% names(data)) {
            tryCatch({
              plot_ids <- unique(data$id_liste_plots[!is.na(data$id_liste_plots)])
              if (length(plot_ids) > 0) {
                existing_inds <- DBI::dbGetQuery(con(), sprintf(
                  "SELECT di.tag, di.id_table_liste_plots_n, di.id_n
                   FROM data_individuals di
                   WHERE di.id_table_liste_plots_n IN (%s)",
                  paste(plot_ids, collapse = ",")
                ))

                # Check each row
                unmatched <- integer(0)
                for (i in seq_len(nrow(data))) {
                  tag_val <- data$tag[i]
                  plot_id <- data$id_liste_plots[i]
                  if (is.na(tag_val) || is.na(plot_id)) next
                  match_rows <- existing_inds[
                    as.character(existing_inds$tag) == as.character(tag_val) &
                    existing_inds$id_table_liste_plots_n == plot_id, ]
                  if (nrow(match_rows) == 0) {
                    unmatched <- c(unmatched, i)
                  }
                }

                if (length(unmatched) > 0) {
                  # Show up to 10 unmatched as errors
                  show_rows <- utils::head(unmatched, 10)
                  errors <- rbind(errors, data.frame(
                    row = show_rows,
                    column = "tag",
                    issue = sprintf(
                      i18n()$t("Individual with tag '%s' not found in plot"),
                      data$tag[show_rows]
                    ),
                    stringsAsFactors = FALSE
                  ))
                  if (length(unmatched) > 10) {
                    warnings <- rbind(warnings, data.frame(
                      row = 0, column = "tag",
                      warning = sprintf(
                        i18n()$t("%d more unmatched individuals not shown"),
                        length(unmatched) - 10
                      ),
                      stringsAsFactors = FALSE
                    ))
                  }
                }
              }
            }, error = function(e) {
              cli::cli_alert_warning("Could not validate individuals: {e$message}")
              warnings <- rbind(warnings, data.frame(
                row = 0, column = "tag",
                warning = sprintf("Could not validate individuals: %s", e$message),
                stringsAsFactors = FALSE
              ))
            })
          }

          # Validate numeric trait values against min/max ranges
          if ("traitid" %in% names(data) && "traitvalue" %in% names(data)) {
            tryCatch({
              trait_ids <- unique(data$traitid[!is.na(data$traitid)])
              if (length(trait_ids) > 0) {
                trait_info <- DBI::dbGetQuery(con(), sprintf(
                  "SELECT id_trait, trait, minallowedvalue, maxallowedvalue
                   FROM traitlist
                   WHERE id_trait IN (%s)",
                  paste(trait_ids, collapse = ",")
                ))
                for (i in seq_len(nrow(data))) {
                  tid <- data$traitid[i]
                  val <- data$traitvalue[i]
                  if (is.na(tid) || is.na(val)) next
                  tinfo <- trait_info[trait_info$id_trait == tid, ]
                  if (nrow(tinfo) == 0) next
                  if (!is.na(tinfo$minallowedvalue[1]) && val < tinfo$minallowedvalue[1]) {
                    warnings <- rbind(warnings, data.frame(
                      row = i, column = "traitvalue",
                      warning = sprintf(
                        i18n()$t("Value %.2f below minimum %.2f for trait '%s'"),
                        val, tinfo$minallowedvalue[1], tinfo$trait[1]
                      ),
                      stringsAsFactors = FALSE
                    ))
                  }
                  if (!is.na(tinfo$maxallowedvalue[1]) && val > tinfo$maxallowedvalue[1]) {
                    warnings <- rbind(warnings, data.frame(
                      row = i, column = "traitvalue",
                      warning = sprintf(
                        i18n()$t("Value %.2f above maximum %.2f for trait '%s'"),
                        val, tinfo$maxallowedvalue[1], tinfo$trait[1]
                      ),
                      stringsAsFactors = FALSE
                    ))
                  }
                }
              }
            }, error = function(e) {
              cli::cli_alert_warning("Could not validate trait ranges: {e$message}")
            })
          }

          # Check for zero values
          if ("traitvalue" %in% names(data)) {
            zero_rows <- which(!is.na(data$traitvalue) & data$traitvalue == 0)
            if (length(zero_rows) > 0) {
              warnings <- rbind(warnings, data.frame(
                row = 0, column = "traitvalue",
                warning = sprintf(
                  i18n()$t("%d measurement(s) have a value of 0"),
                  length(zero_rows)
                ),
                stringsAsFactors = FALSE
              ))
            }
          }

          # Check for duplicated individuals (same individual + same trait) — numeric traits only
          if ("tag" %in% names(data) && "traitid" %in% names(data) && "id_liste_plots" %in% names(data)) {
            # Initialize issue column if not present
            if (!"issue" %in% names(data)) data$issue <- NA_character_

            # Restrict duplicate check to numeric traits only.
            # In the prepared data, numeric traits populate traitvalue (not traitvalue_char).
            if ("traitvalue" %in% names(data) && "traitvalue_char" %in% names(data)) {
              data_numeric <- data[!is.na(data$traitvalue), , drop = FALSE]
            } else if ("traitvalue" %in% names(data)) {
              data_numeric <- data
            } else {
              data_numeric <- data[integer(0), , drop = FALSE]  # skip entirely for non-numeric data
            }

            dup_key_all  <- paste(data$id_liste_plots, data$tag, data$traitid, sep = "_")
            dup_key_num  <- paste(data_numeric$id_liste_plots, data_numeric$tag, data_numeric$traitid, sep = "_")
            dup_counts   <- table(dup_key_num)
            dup_keys     <- names(dup_counts[dup_counts > 1])

            if (length(dup_keys) > 0) {
              dup_detail <- data_numeric[dup_key_num %in% dup_keys, , drop = FALSE]
              dup_check <- dup_detail %>%
                dplyr::group_by(id_liste_plots, tag, traitid) %>%
                dplyr::summarise(
                  n = dplyr::n(),
                  n_distinct_val = dplyr::n_distinct(traitvalue, na.rm = TRUE),
                  .groups = "drop"
                )
              n_diff_vals <- sum(dup_check$n_distinct_val > 1)
              n_same_vals <- sum(dup_check$n_distinct_val <= 1)

              # Annotate issue column on affected rows (using index into full data)
              for (dk in dup_keys) {
                affected_rows <- which(dup_key_all == dk)
                vals <- data$traitvalue[affected_rows]
                if (length(unique(stats::na.omit(vals))) > 1) {
                  issue_msg <- "more than one observation for a single individual carrying different value"
                } else {
                  issue_msg <- "more than one observation for a single individual carrying identical value"
                }
                for (r in affected_rows) {
                  if (is.na(data$issue[r])) {
                    data$issue[r] <- issue_msg
                  } else {
                    data$issue[r] <- paste(data$issue[r], issue_msg, sep = " | ")
                  }
                }
              }

              if (n_diff_vals > 0) {
                warnings <- rbind(warnings, data.frame(
                  row = 0, column = "tag",
                  warning = sprintf(
                    i18n()$t("%d individual(s) have multiple observations for the same trait with DIFFERENT values"),
                    n_diff_vals
                  ),
                  stringsAsFactors = FALSE
                ))
              }
              if (n_same_vals > 0) {
                warnings <- rbind(warnings, data.frame(
                  row = 0, column = "tag",
                  warning = sprintf(
                    i18n()$t("%d individual(s) have multiple observations for the same trait with identical values"),
                    n_same_vals
                  ),
                  stringsAsFactors = FALSE
                ))
              }
            }
          }

          # Check for existing measurements in DB (same individual + trait + census)
          if ("tag" %in% names(data) && "traitid" %in% names(data) && "id_liste_plots" %in% names(data)) {
            tryCatch({
              plot_ids <- unique(data$id_liste_plots[!is.na(data$id_liste_plots)])
              trait_ids <- unique(data$traitid[!is.na(data$traitid)])
              if (length(plot_ids) > 0 && length(trait_ids) > 0) {
                # Get existing individuals and their trait measurements
                existing_inds <- DBI::dbGetQuery(con(), sprintf(
                  "SELECT di.tag, di.id_table_liste_plots_n, di.id_n
                   FROM data_individuals di
                   WHERE di.id_table_liste_plots_n IN (%s)",
                  paste(plot_ids, collapse = ",")
                ))

                if (nrow(existing_inds) > 0) {
                  existing_measures <- DBI::dbGetQuery(con(), sprintf(
                    "SELECT dtm.id_data_individuals, dtm.traitid, dtm.id_sub_plots, dtm.traitvalue
                     FROM data_traits_measures dtm
                     WHERE dtm.traitid IN (%s)
                       AND dtm.id_data_individuals IN (%s)",
                    paste(trait_ids, collapse = ","),
                    paste(existing_inds$id_n, collapse = ",")
                  ))

                  if (nrow(existing_measures) > 0) {
                    # Rows carrying a census are compared within that
                    # census; rows carrying none - a position, a quadrat -
                    # are compared against every value recorded for the
                    # feature, there being no campaign to narrow them to.
                    matches <- .existing_measurement_rows(
                      data, existing_inds, existing_measures)

                    existing_rows <- sort(unique(
                      c(matches$with_census, matches$without_census)))

                    annotate <- function(data, rows, msg) {
                      if (length(rows) == 0) return(data)
                      if (!"issue" %in% names(data)) data$issue <- NA_character_
                      data$issue[rows] <- ifelse(
                        is.na(data$issue[rows]), msg,
                        paste(data$issue[rows], msg, sep = " | ")
                      )
                      data
                    }

                    if (length(matches$with_census) > 0) {
                      warnings <- rbind(warnings, data.frame(
                        row = 0, column = "traitvalue",
                        warning = sprintf(
                          i18n()$t("%d measurement(s) already exist in the database for the same individual, trait and census"),
                          length(matches$with_census)
                        ),
                        stringsAsFactors = FALSE
                      ))
                      data <- annotate(data, matches$with_census,
                                       "already recorded in the database for this census")
                    }

                    if (length(matches$without_census) > 0) {
                      warnings <- rbind(warnings, data.frame(
                        row = 0, column = "traitvalue",
                        warning = sprintf(
                          i18n()$t("%d measurement(s) carry no census and the same individual already has a value recorded for that feature"),
                          length(matches$without_census)
                        ),
                        stringsAsFactors = FALSE
                      ))
                      data <- annotate(data, matches$without_census,
                                       "already recorded in the database for this individual")
                    }
                  }
                }
              }
            }, error = function(e) {
              cli::cli_alert_warning("Could not check existing measurements: {e$message}")
            })
          }

          # Compare with previous census values (for numeric traits like DBH)
          if ("tag" %in% names(data) && "traitid" %in% names(data) &&
              "traitvalue" %in% names(data) && "id_liste_plots" %in% names(data)) {
            tryCatch({
              plot_ids <- unique(data$id_liste_plots[!is.na(data$id_liste_plots)])
              trait_ids <- unique(data$traitid[!is.na(data$traitid)])

              if (length(plot_ids) > 0 && length(trait_ids) > 0) {
                # Get individuals
                db_inds <- DBI::dbGetQuery(con(), sprintf(
                  "SELECT di.tag, di.id_table_liste_plots_n, di.id_n
                   FROM data_individuals di
                   WHERE di.id_table_liste_plots_n IN (%s)",
                  paste(plot_ids, collapse = ",")
                ))

                if (nrow(db_inds) > 0) {
                  # Get max previous trait values per individual per trait
                  prev_measures <- DBI::dbGetQuery(con(), sprintf(
                    "SELECT dtm.id_data_individuals, dtm.traitid, MAX(dtm.traitvalue) AS max_prev_value
                     FROM data_traits_measures dtm
                     WHERE dtm.traitid IN (%s)
                       AND dtm.id_data_individuals IN (%s)
                       AND dtm.traitvalue IS NOT NULL
                     GROUP BY dtm.id_data_individuals, dtm.traitid",
                    paste(trait_ids, collapse = ","),
                    paste(db_inds$id_n, collapse = ",")
                  ))

                  if (nrow(prev_measures) > 0) {
                    # Build lookup: tag+plot -> id_n
                    db_inds_lookup <- db_inds
                    names(db_inds_lookup)[names(db_inds_lookup) == "id_table_liste_plots_n"] <- "id_liste_plots"

                    # Initialize issue and prev_census_value columns if not present
                    if (!"issue" %in% names(data)) data$issue <- NA_character_
                    if (!"prev_census_value" %in% names(data)) data$prev_census_value <- NA_real_

                    n_lower <- 0
                    for (i in seq_len(nrow(data))) {
                      tag_val <- data$tag[i]
                      plot_id <- data$id_liste_plots[i]
                      tid <- data$traitid[i]
                      new_val <- data$traitvalue[i]
                      if (is.na(tag_val) || is.na(plot_id) || is.na(tid) || is.na(new_val)) next
                      if (!"id_sub_plots" %in% names(data) || is.na(data$id_sub_plots[i])) next

                      ind_match <- db_inds_lookup[
                        as.character(db_inds_lookup$tag) == as.character(tag_val) &
                        db_inds_lookup$id_liste_plots == plot_id, ]
                      if (nrow(ind_match) == 0) next

                      prev_match <- prev_measures[
                        prev_measures$id_data_individuals %in% ind_match$id_n &
                        prev_measures$traitid == tid, ]
                      if (nrow(prev_match) == 0) next

                      max_prev <- prev_match$max_prev_value[1]
                      # Always store the previous census value (for preview context)
                      if (!is.na(max_prev)) {
                        data$prev_census_value[i] <- max_prev
                      }
                      if (!is.na(max_prev) && new_val < max_prev) {
                        n_lower <- n_lower + 1
                        # Annotate issue column
                        issue_msg <- "value lower than previous census"
                        if (is.na(data$issue[i])) {
                          data$issue[i] <- issue_msg
                        } else {
                          data$issue[i] <- paste(data$issue[i], issue_msg, sep = " | ")
                        }
                      }
                    }

                    if (n_lower > 0) {
                      warnings <- rbind(warnings, data.frame(
                        row = 0, column = "traitvalue",
                        warning = sprintf(
                          i18n()$t("%d measurement(s) have values lower than the maximum from previous censuses — check consistency"),
                          n_lower
                        ),
                        stringsAsFactors = FALSE
                      ))
                    }
                  }
                }
              }
            }, error = function(e) {
              cli::cli_alert_warning("Could not compare with previous census: {e$message}")
            })
          }
        }

        # 2c. Multi-stems validation
        if (mode == "define_multi_stems") {
          # Required columns
          for (req_col in c("plot_name", "tag", "group_tag")) {
            if (!req_col %in% names(data)) {
              errors <- rbind(errors, data.frame(
                row = 0, column = req_col,
                issue = sprintf(i18n()$t("Missing required column: %s"), req_col),
                stringsAsFactors = FALSE
              ))
            }
          }

          if (all(c("plot_name", "tag", "group_tag") %in% names(data))) {
            # Check for duplicate tag assignments (a tag in multiple groups)
            dup_keys <- paste(data$plot_name, data$tag)
            dup_idx <- which(duplicated(dup_keys))
            if (length(dup_idx) > 0) {
              show_dup <- utils::head(dup_idx, 10)
              errors <- rbind(errors, data.frame(
                row = show_dup, column = "tag",
                issue = sprintf(
                  i18n()$t("Tag %s appears in multiple groups"),
                  data$tag[show_dup]
                ),
                stringsAsFactors = FALSE
              ))
            }

            # Check group_tag exists as a tag in the uploaded data OR in the database
            for (i in seq_len(nrow(data))) {
              gt <- data$group_tag[i]
              pl <- data$plot_name[i]
              if (is.na(gt) || is.na(pl)) next
              in_data <- any(data$plot_name == pl & data$tag == gt)
              # Also check if group_id_n was resolved (meaning parent exists in DB)
              in_db <- "group_id_n" %in% names(data) && !is.na(data$group_id_n[i])
              if (!in_data && !in_db) {
                errors <- rbind(errors, data.frame(
                  row = i, column = "group_tag",
                  issue = sprintf(
                    i18n()$t("Group tag %s not found as a tag in this plot's data or in the database"),
                    gt
                  ),
                  stringsAsFactors = FALSE
                ))
              }
            }

            # Check groups have at least 2 members
            group_keys <- paste(data$plot_name, data$group_tag)
            group_counts <- table(group_keys)
            singletons <- names(group_counts[group_counts < 2])
            if (length(singletons) > 0) {
              for (sg in singletons) {
                rows_sg <- which(group_keys == sg)
                warnings <- rbind(warnings, data.frame(
                  row = rows_sg[1], column = "group_tag",
                  warning = i18n()$t("Group has only 1 member — will be ignored"),
                  stringsAsFactors = FALSE
                ))
              }
            }

            # Use pre-resolved id_n from step 3 if available; otherwise resolve here
            has_resolved <- "id_n" %in% names(data) && "group_id_n" %in% names(data)

            if (has_resolved) {
              # Tags not found in DB (id_n is NA) — warning, not error
              unmatched <- which(is.na(data$id_n) & data$tag != data$group_tag)
              if (length(unmatched) > 0) {
                show_rows <- utils::head(unmatched, 10)
                warnings <- rbind(warnings, data.frame(
                  row = show_rows, column = "tag",
                  warning = sprintf(
                    i18n()$t("Tag %s not found in database — will be skipped"),
                    data$tag[show_rows]
                  ),
                  stringsAsFactors = FALSE
                ))
                if (length(unmatched) > 10) {
                  warnings <- rbind(warnings, data.frame(
                    row = 0, column = "tag",
                    warning = sprintf(
                      i18n()$t("%d more unmatched tags not shown"),
                      length(unmatched) - 10
                    ),
                    stringsAsFactors = FALSE
                  ))
                }
              }

              # Parent id_n not resolved
              unmatched_parents <- which(is.na(data$group_id_n) & data$tag != data$group_tag & !is.na(data$id_n))
              if (length(unmatched_parents) > 0) {
                show_rows <- utils::head(unmatched_parents, 10)
                errors <- rbind(errors, data.frame(
                  row = show_rows, column = "group_tag",
                  issue = sprintf(
                    i18n()$t("Parent tag %s not found in database — cannot set stem_grouping"),
                    data$group_tag[show_rows]
                  ),
                  stringsAsFactors = FALSE
                ))
              }

              # Warn about existing stem_grouping that will be overwritten
              non_parent <- data[data$tag != data$group_tag & !is.na(data$id_n), , drop = FALSE]
              if (nrow(non_parent) > 0 && "db_current_group" %in% names(non_parent)) {
                n_overwrite <- sum(!is.na(non_parent$db_current_group))
                if (n_overwrite > 0) {
                  warnings <- rbind(warnings, data.frame(
                    row = 0, column = "stem_grouping",
                    warning = sprintf(
                      i18n()$t("%d stem(s) already have a stem_grouping that will be overwritten"),
                      n_overwrite
                    ),
                    stringsAsFactors = FALSE
                  ))
                }
              }

              # Summary: how many will actually be updated
              updatable <- sum(
                data$tag != data$group_tag &
                !is.na(data$id_n) &
                !is.na(data$group_id_n),
                na.rm = TRUE
              )
              if (updatable == 0) {
                errors <- rbind(errors, data.frame(
                  row = 0, column = "data",
                  issue = i18n()$t("No valid stem groupings to update (all tags unmatched or missing parents)"),
                  stringsAsFactors = FALSE
                ))
              }
            } else {
              # Fallback: no pre-resolved IDs — just check tags exist via DB query
              tryCatch({
                plot_names <- unique(data$plot_name)
                if (length(plot_names) > 0) {
                  placeholders <- paste(sprintf("'%s'", gsub("'", "''", plot_names)), collapse = ",")
                  db_inds <- DBI::dbGetQuery(con(), sprintf(
                    "SELECT di.tag, dlp.plot_name, di.id_n, di.stem_grouping
                     FROM data_individuals di
                     JOIN data_liste_plots dlp ON di.id_table_liste_plots_n = dlp.id_liste_plots
                     WHERE dlp.plot_name IN (%s)",
                    placeholders
                  ))

                  unmatched <- integer(0)
                  for (i in seq_len(nrow(data))) {
                    tag_val <- as.character(data$tag[i])
                    pl <- data$plot_name[i]
                    if (is.na(tag_val) || is.na(pl)) next
                    match_rows <- db_inds[as.character(db_inds$tag) == tag_val & db_inds$plot_name == pl, ]
                    if (nrow(match_rows) == 0) unmatched <- c(unmatched, i)
                  }

                  if (length(unmatched) > 0) {
                    show_rows <- utils::head(unmatched, 10)
                    warnings <- rbind(warnings, data.frame(
                      row = show_rows, column = "tag",
                      warning = sprintf(
                        i18n()$t("Tag %s not found in database — will be skipped"),
                        data$tag[show_rows]
                      ),
                      stringsAsFactors = FALSE
                    ))
                  }
                }
              }, error = function(e) {
                cli::cli_alert_warning("Could not validate multi-stem tags: {e$message}")
              })
            }

            # Check flagged rows from upload
            if ("flag" %in% names(data)) {
              flagged <- which(!is.na(data$flag) & data$flag != "")
              if (length(flagged) > 0) {
                for (fi in flagged) {
                  warnings <- rbind(warnings, data.frame(
                    row = fi, column = "flag",
                    warning = sprintf(
                      i18n()$t("Flagged: %s"), data$flag[fi]
                    ),
                    stringsAsFactors = FALSE
                  ))
                }
              }
            }
          }
        }

        # 2d. Full census import validation
        #
        # The step 3 split already resolved which rows are recruits, so tags
        # missing from the database are expected here rather than an error.
        # What is checked instead is everything the split deliberately left
        # for a human: the census identity, the recruits' taxonomy, and the
        # multi-stem grouping.
        if (mode == "import_census") {
          census_check <- tryCatch(
            .validate_census_import(data, config, con()),
            error = function(e) {
              cli::cli_alert_warning("Census validation failed: {e$message}")
              list(
                errors = list(sprintf("Could not validate the census: %s", e$message)),
                warnings = list()
              )
            }
          )

          if (length(census_check$errors) > 0) {
            errors <- rbind(errors, data.frame(
              row = 0, column = "census",
              issue = unlist(census_check$errors),
              stringsAsFactors = FALSE
            ))
          }
          if (length(census_check$warnings) > 0) {
            warnings <- rbind(warnings, data.frame(
              row = 0, column = "census",
              warning = unlist(census_check$warnings),
              stringsAsFactors = FALSE
            ))
          }
        }

        # 2b. Census-specific validation
        if (mode == "new_census") {
          # Check census column
          if ("census" %in% names(data)) {
            if (any(is.na(data$census))) {
              errors <- rbind(errors, data.frame(
                row = which(is.na(data$census)),
                column = "census",
                issue = i18n()$t("Census number is required"),
                stringsAsFactors = FALSE
              ))
            }

            # Check for duplicate census numbers per plot
            if ("id_liste_plots" %in% names(data)) {
              tryCatch({
                existing_censuses <- DBI::dbGetQuery(con(), sprintf(
                  "SELECT sp.id_table_liste_plots, sp.typevalue as census
                   FROM data_liste_sub_plots sp
                   JOIN subplotype_list spl ON sp.id_type_sub_plot = spl.id_subplotype
                   WHERE spl.type = 'census'
                     AND sp.id_table_liste_plots IN (%s)",
                  paste(unique(data$id_liste_plots[!is.na(data$id_liste_plots)]), collapse = ",")
                ))

                for (i in seq_len(nrow(data))) {
                  plot_id <- data$id_liste_plots[i]
                  census_num <- data$census[i]
                  if (!is.na(plot_id) && !is.na(census_num)) {
                    existing <- existing_censuses[
                      existing_censuses$id_table_liste_plots == plot_id &
                      existing_censuses$census == census_num, ]
                    if (nrow(existing) > 0) {
                      warnings <- rbind(warnings, data.frame(
                        row = i, column = "census",
                        warning = sprintf(
                          i18n()$t("Census %d already exists for this plot. It will be duplicated."),
                          census_num),
                        stringsAsFactors = FALSE
                      ))
                    }
                  }
                }
              }, error = function(e) {
                cli::cli_alert_warning("Could not check existing censuses: {e$message}")
              })
            }
          } else {
            errors <- rbind(errors, data.frame(
              row = 0, column = "census",
              issue = i18n()$t("Census column is missing"),
              stringsAsFactors = FALSE
            ))
          }
        }

        # 3. Date validation
        if ("year" %in% names(data)) {
          invalid_years <- which(!is.na(data$year) & (data$year < 1900 | data$year > 2100))
          if (length(invalid_years) > 0) {
            errors <- rbind(errors, data.frame(
              row = invalid_years, column = "year",
              issue = i18n()$t("Year must be between 1900 and 2100"),
              stringsAsFactors = FALSE
            ))
          }
          if (all(is.na(data$year))) {
            warnings <- rbind(warnings, data.frame(
              row = 0, column = "year",
              warning = i18n()$t("No year provided"),
              stringsAsFactors = FALSE
            ))
          }
        }

        if ("month" %in% names(data)) {
          invalid_months <- which(!is.na(data$month) & (data$month < 1 | data$month > 12))
          if (length(invalid_months) > 0) {
            errors <- rbind(errors, data.frame(
              row = invalid_months, column = "month",
              issue = i18n()$t("Month must be between 1 and 12"),
              stringsAsFactors = FALSE
            ))
          }
        }

        if ("day" %in% names(data)) {
          invalid_days <- which(!is.na(data$day) & (data$day < 1 | data$day > 31))
          if (length(invalid_days) > 0) {
            errors <- rbind(errors, data.frame(
              row = invalid_days, column = "day",
              issue = i18n()$t("Day must be between 1 and 31"),
              stringsAsFactors = FALSE
            ))
          }
        }

        # 4. Check required fields present
        if (nrow(data) == 0) {
          errors <- rbind(errors, data.frame(
            row = 0, column = "data",
            issue = i18n()$t("No data rows to import"),
            stringsAsFactors = FALSE
          ))
        }

        is_valid <- nrow(errors) == 0

        result <- list(
          valid = is_valid,
          summary = list(
            total_rows = nrow(data),
            errors = nrow(errors),
            warnings = nrow(warnings)
          ),
          errors = errors,
          warnings = warnings,
          data = data,
          existing_rows = existing_rows
        )

        validation_result(result)

        shiny::setProgress(1, message = i18n()$t("Validation complete!"))

      }, message = i18n()$t("Validating..."))
    })

    # What the rest of the wizard sees, once the user has decided what to do
    # about the rows the database already holds.
    #
    # Re-importing them is sometimes exactly right - a second measurement of
    # the same stem during the same census is legitimate - so nothing is
    # removed unless the box is ticked. Dropping every row is caught here
    # rather than at the import, which would otherwise write nothing and
    # report success.
    effective_result <- shiny::reactive({
      res <- validation_result()
      if (is.null(res)) return(NULL)

      dup <- res$existing_rows
      if (is.null(dup) || length(dup) == 0 || !isTRUE(input$drop_existing)) {
        return(res)
      }

      res$data <- res$data[setdiff(seq_len(nrow(res$data)), dup), , drop = FALSE]
      res$summary$total_rows <- nrow(res$data)
      res$dropped_existing <- length(dup)

      if (nrow(res$data) == 0) {
        res$errors <- rbind(res$errors, data.frame(
          row = 0, column = "data",
          issue = i18n()$t("No data rows to import"),
          stringsAsFactors = FALSE
        ))
        res$summary$errors <- nrow(res$errors)
        res$valid <- FALSE
      }

      res
    })

    # The offer itself. Driven by the unfiltered result, so ticking the box
    # does not make the box disappear.
    output$existing_filter_ui <- shiny::renderUI({
      res <- validation_result()
      if (is.null(res)) return(NULL)
      n <- length(res$existing_rows)
      if (n == 0) return(NULL)

      shiny::div(
        class = "alert alert-warning",
        style = "margin-top: 20px;",
        shiny::checkboxInput(
          ns("drop_existing"),
          label = sprintf(
            i18n()$t("Remove the %d measurement(s) already in the database from this import"),
            n
          ),
          value = FALSE
        ),
        shiny::p(
          shiny::icon("info-circle"), " ",
          i18n()$t("Leave this unticked to import them anyway: recording a second measurement of the same individual, feature and census is sometimes intended. Tick it to import only what is new."),
          style = "color: #856404; margin: 0 0 0 20px; font-size: 13px;"
        ),
        shiny::uiOutput(ns("existing_filter_effect"))
      )
    })

    output$existing_filter_effect <- shiny::renderUI({
      res <- effective_result()
      if (is.null(res) || is.null(res$dropped_existing)) return(NULL)

      shiny::p(
        shiny::icon("filter"), " ",
        sprintf(
          i18n()$t("%d row(s) removed. %d row(s) will be imported."),
          res$dropped_existing, nrow(res$data)
        ),
        style = "color: #856404; font-weight: 600; margin: 8px 0 0 20px;"
      )
    })

    # Validation summary
    output$validation_summary <- shiny::renderUI({
      res <- effective_result()
      if (is.null(res)) return(NULL)

      shiny::fluidRow(
        shiny::column(4, shiny::div(
          class = "card",
          style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
          shiny::h3(res$summary$total_rows, style = "margin: 0; color: #007bff;"),
          shiny::p(i18n()$t("Total Rows"), style = "margin: 5px 0 0 0; color: #6c757d;")
        )),
        shiny::column(4, shiny::div(
          class = "card",
          style = sprintf(
            "padding: 20px; background-color: #f8f9fa; border-left: 4px solid %s; text-align: center;",
            if (res$summary$errors == 0) "#28a745" else "#dc3545"),
          shiny::h3(res$summary$errors,
            style = sprintf("margin: 0; color: %s;",
              if (res$summary$errors == 0) "#28a745" else "#dc3545")),
          shiny::p(i18n()$t("Errors"), style = "margin: 5px 0 0 0; color: #6c757d;")
        )),
        shiny::column(4, shiny::div(
          class = "card",
          style = sprintf(
            "padding: 20px; background-color: #f8f9fa; border-left: 4px solid %s; text-align: center;",
            if (res$summary$warnings == 0) "#28a745" else "#ffc107"),
          shiny::h3(res$summary$warnings,
            style = sprintf("margin: 0; color: %s;",
              if (res$summary$warnings == 0) "#28a745" else "#ffc107")),
          shiny::p(i18n()$t("Warnings"), style = "margin: 5px 0 0 0; color: #6c757d;")
        ))
      )
    })

    # Validation details
    output$validation_details <- shiny::renderUI({
      res <- effective_result()
      if (is.null(res)) return(NULL)

      ui_elements <- list()

      if (res$valid) {
        ui_elements[[1]] <- shiny::div(
          class = "alert alert-success",
          shiny::icon("check-circle"),
          shiny::strong(paste0(" ", i18n()$t("Validation passed!"))), " ",
          i18n()$t("You can proceed to import.")
        )
      } else {
        ui_elements[[1]] <- shiny::div(
          class = "alert alert-danger",
          shiny::icon("exclamation-circle"),
          shiny::strong(paste0(" ", i18n()$t("Validation failed."))), " ",
          i18n()$t("Please fix the errors below before proceeding.")
        )
      }

      if (nrow(res$errors) > 0) {
        ui_elements[[length(ui_elements) + 1]] <- shiny::tagList(
          shiny::h5(shiny::icon("times-circle", style = "color: #dc3545;"), " ",
                    i18n()$t("Errors"), style = "margin-top: 15px;"),
          DT::DTOutput(ns("errors_table"))
        )
      }

      if (nrow(res$warnings) > 0) {
        ui_elements[[length(ui_elements) + 1]] <- shiny::tagList(
          shiny::h5(shiny::icon("exclamation-triangle", style = "color: #ffc107;"), " ",
                    i18n()$t("Warnings"), style = "margin-top: 15px;"),
          DT::DTOutput(ns("warnings_table"))
        )
      }

      shiny::tagList(ui_elements)
    })

    output$errors_table <- DT::renderDT({
      res <- effective_result()
      shiny::req(res, nrow(res$errors) > 0)

      DT::datatable(
        res$errors,
        options = list(pageLength = 10, dom = "t"),
        rownames = FALSE, class = "display cell-border"
      ) %>% DT::formatStyle(
        columns = 1:3,
        backgroundColor = "#f8d7da"
      )
    })

    output$warnings_table <- DT::renderDT({
      res <- effective_result()
      shiny::req(res, nrow(res$warnings) > 0)

      DT::datatable(
        res$warnings,
        options = list(pageLength = 10, dom = "t"),
        rownames = FALSE, class = "display cell-border"
      ) %>% DT::formatStyle(
        columns = 1:3,
        backgroundColor = "#fff3cd"
      )
    })

    # Issue summary table by trait (measurements mode only)
    output$issue_summary_ui <- shiny::renderUI({
      res <- effective_result()
      if (is.null(res)) return(NULL)

      d <- res$data
      if (!"issue" %in% names(d) || !"trait_name" %in% names(d)) return(NULL)
      if (all(is.na(d$issue))) return(NULL)

      shiny::tagList(
        shiny::h4(
          shiny::icon("flag"),
          i18n()$t("Issues by Trait"),
          style = "margin-top: 20px; margin-bottom: 10px; color: #856404;"
        ),
        DT::DTOutput(ns("issue_summary_table")),
        shiny::hr()
      )
    })

    output$issue_summary_table <- DT::renderDT({
      res <- effective_result()
      shiny::req(res)

      d <- res$data
      shiny::req("issue" %in% names(d), "trait_name" %in% names(d))
      shiny::req(any(!is.na(d$issue)))

      # Split combined issues (separated by " | ") into individual rows
      rows_with_issue <- d[!is.na(d$issue), , drop = FALSE]

      # Build summary: for each trait × issue combination, count rows
      summary_list <- lapply(seq_len(nrow(rows_with_issue)), function(i) {
        issues <- trimws(strsplit(rows_with_issue$issue[i], " | ", fixed = TRUE)[[1]])
        data.frame(
          trait = rows_with_issue$trait_name[i],
          issue = issues,
          stringsAsFactors = FALSE
        )
      })

      long_df <- do.call(rbind, summary_list)

      # Count by trait × issue
      summary_df <- long_df %>%
        dplyr::group_by(trait, issue) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(trait, issue) %>%
        as.data.frame()

      names(summary_df) <- c(
        i18n()$t("Trait"),
        i18n()$t("Issue"),
        i18n()$t("Count")
      )

      DT::datatable(
        summary_df,
        options = list(pageLength = 15, dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "display cell-border"
      ) %>% DT::formatStyle(
        i18n()$t("Count"),
        backgroundColor = "#fff3cd",
        fontWeight = "bold"
      )
    })

    # Dynamic description and preview header based on operation mode
    output$validation_description <- shiny::renderUI({
      mode <- tryCatch(operation_mode(), error = function(e) NULL)
      desc <- if (identical(mode, "define_multi_stems")) {
        i18n()$t("Review the stem groupings that will be updated. Fix any errors before proceeding.")
      } else {
        i18n()$t("Review the data that will be imported. Fix any errors before proceeding.")
      }
      shiny::p(desc, style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;")
    })

    output$preview_header <- shiny::renderUI({
      mode <- tryCatch(operation_mode(), error = function(e) NULL)
      label <- if (identical(mode, "define_multi_stems")) {
        i18n()$t("Data to Update")
      } else {
        i18n()$t("Data to Import")
      }
      shiny::h4(
        shiny::icon("table"), label,
        style = "margin-top: 20px; margin-bottom: 15px;"
      )
    })

    # Import preview
    output$import_preview_table <- DT::renderDT({
      res <- effective_result()
      shiny::req(res)

      # Columns to always hide from preview
      hide_cols <- c("id_liste_plots", "id_sub_plots", "id_data_individuals",
                     "id_diconame", "id_specimen_ind", "traitid")

      # Keep prev_census_value only if it has any non-NA values
      has_prev <- "prev_census_value" %in% names(res$data) &&
        any(!is.na(res$data$prev_census_value))
      if (!has_prev) hide_cols <- c(hide_cols, "prev_census_value")

      display <- res$data %>%
        dplyr::select(-dplyr::any_of(hide_cols))

      DT::datatable(
        display,
        options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE, class = "display cell-border stripe"
      ) %>% {
        # Highlight rows with issues in pale orange
        if ("issue" %in% names(display)) {
          DT::formatStyle(., "issue",
            backgroundColor = DT::styleInterval(
              cuts = "",
              values = c("white", "#fff3cd")
            )
          )
        } else .
      }
    })

    return(effective_result)
  })
}

#' Rows that repeat a measurement the database already holds
#'
#' Compares each row of prepared measurement data against what is already
#' recorded, and answers on the terms the row itself sets:
#'
#' * A row carrying a census is a repeat only of a measurement recorded
#'   **during that same census**. The same feature measured during another
#'   campaign is a new measurement, not a duplicate, which is the whole point
#'   of a census.
#' * A row carrying no census - a position, a quadrat, anything the census
#'   link policy keeps off a campaign - is a repeat of **any** recorded value
#'   of that feature for that individual. There is no campaign to narrow the
#'   comparison to, and the tree has one position, not one per census.
#'
#' The two are returned separately because they are not the same claim, and a
#' single count would misreport one of them.
#'
#' Values are not compared, only the existence of a measurement: whether
#' re-recording the same feature is a mistake or a deliberate second reading
#' is the user's call, not this function's.
#'
#' @param data Data frame of prepared measurements, with `tag`, `traitid` and
#'   `id_liste_plots`, and optionally `id_sub_plots`.
#' @param individuals Data frame of the individuals recorded for the selected
#'   plots, with `tag`, `id_table_liste_plots_n` and `id_n`.
#' @param measures Data frame of the measurements already recorded for those
#'   individuals, with `id_data_individuals`, `traitid` and `id_sub_plots`.
#'
#' @return List of two integer vectors of row numbers into `data`:
#'   `with_census` (matched on individual, feature and census) and
#'   `without_census` (matched on individual and feature alone).
#' @keywords internal
#' @export
.existing_measurement_rows <- function(data, individuals, measures) {
  empty <- list(with_census = integer(0), without_census = integer(0))

  if (is.null(data) || is.null(individuals) || is.null(measures)) return(empty)
  if (nrow(data) == 0 || nrow(individuals) == 0 || nrow(measures) == 0) return(empty)
  if (!all(c("tag", "traitid", "id_liste_plots") %in% names(data))) return(empty)

  chr <- function(x) {
    x <- trimws(as.character(x))
    x[x == ""] <- NA_character_
    x
  }

  rows <- data.frame(
    stringsAsFactors = FALSE,
    .row     = seq_len(nrow(data)),
    .tag     = chr(data$tag),
    .plot    = chr(data$id_liste_plots),
    .traitid = chr(data$traitid),
    .census  = if ("id_sub_plots" %in% names(data)) {
      chr(data$id_sub_plots)
    } else {
      rep(NA_character_, nrow(data))
    }
  )
  rows <- rows[!is.na(rows$.tag) & !is.na(rows$.plot) & !is.na(rows$.traitid), ,
               drop = FALSE]
  if (nrow(rows) == 0) return(empty)

  inds <- data.frame(
    stringsAsFactors = FALSE,
    .tag  = chr(individuals$tag),
    .plot = chr(individuals$id_table_liste_plots_n),
    .id_n = chr(individuals$id_n)
  )
  inds <- inds[!is.na(inds$.tag) & !is.na(inds$.plot) & !is.na(inds$.id_n), ,
               drop = FALSE]
  if (nrow(inds) == 0) return(empty)

  # One row of `data` can name more than one individual when a plot holds a
  # repeated tag, so the comparison is done per candidate and folded back.
  candidates <- merge(rows, inds, by = c(".tag", ".plot"))
  if (nrow(candidates) == 0) return(empty)

  stored_ind   <- chr(measures$id_data_individuals)
  stored_trait <- chr(measures$traitid)
  stored_census <- if ("id_sub_plots" %in% names(measures)) {
    chr(measures$id_sub_plots)
  } else {
    rep(NA_character_, nrow(measures))
  }

  any_key    <- paste(stored_ind, stored_trait)
  linked     <- !is.na(stored_census)
  census_key <- paste(stored_ind[linked], stored_trait[linked],
                      stored_census[linked])

  has_census <- !is.na(candidates$.census)

  hit_census <- has_census & paste(
    candidates$.id_n, candidates$.traitid, candidates$.census) %in% census_key
  hit_any <- !has_census & paste(
    candidates$.id_n, candidates$.traitid) %in% any_key

  list(
    with_census    = sort(unique(candidates$.row[hit_census])),
    without_census = sort(unique(candidates$.row[hit_any]))
  )
}
