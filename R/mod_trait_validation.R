# =============================================================================
# Taxa Traits Import - Validation Module
#
# Validates mapped data before import: type compatibility, NA counts,
# out-of-range values, zero values, duplicate checks, and auto-fixes
# (e.g. converting numeric columns mapped to categorical traits to character).
# =============================================================================

#' Trait Validation Module - UI
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_trait_validation_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("check-circle"),
      i18n$t("Validate Trait Data"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Run validation checks on your mapped data before importing. This will check for type mismatches, missing values, out-of-range values, and more."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    shiny::actionButton(
      ns("run_validation"),
      shiny::tagList(shiny::icon("play"), paste0(" ", i18n$t("Run Validation"))),
      class = "btn-primary btn-lg",
      style = "margin-bottom: 30px;"
    ),

    shiny::uiOutput(ns("validation_results"))
  )
}


#' Trait Validation Module - Server
#'
#' @param id Module namespace ID
#' @param data Reactive: uploaded data frame
#' @param mapping Reactive: combined mapping result (trait_cols, metadata_cols, feature_cols, idtax_col, available_traits)
#' @param pool Reactive: database connection pool
#' @param i18n Reactive: shiny.i18n translator
#'
#' @return Reactive list: valid, cleaned_data, errors, warnings, changes_made, summary
#' @keywords internal
#' @export
mod_trait_validation_server <- function(id, data, mapping, pool, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    validation_result <- shiny::reactiveVal(NULL)

    # ---- Run validation ----
    shiny::observeEvent(input$run_validation, {
      shiny::req(data(), mapping())
      m <- mapping()
      shiny::req(m$valid)

      shiny::withProgress({
        shiny::setProgress(0.1, message = i18n()$t("Validating data..."))

        df <- data()
        traits_info <- m$available_traits
        trait_cols <- m$trait_cols       # named: user_col = trait_name
        feature_cols <- m$feature_cols   # named: user_col = trait_name
        meta_cols <- m$metadata_cols     # named: user_col = db_col
        idtax_col <- m$idtax_col

        errors <- data.frame(
          column = character(), check = character(), message = character(),
          details = character(), stringsAsFactors = FALSE
        )
        warnings <- data.frame(
          column = character(), check = character(), message = character(),
          details = character(), stringsAsFactors = FALSE
        )
        changes <- data.frame(
          column = character(), row = character(), original_value = character(),
          corrected_value = character(), method = character(), stringsAsFactors = FALSE
        )

        cleaned_df <- df

        add_error <- function(col, check, msg, det = "") {
          errors[nrow(errors) + 1, ] <<- list(col, check, msg, det)
        }
        add_warning <- function(col, check, msg, det = "") {
          warnings[nrow(warnings) + 1, ] <<- list(col, check, msg, det)
        }
        add_change <- function(col, row_desc, orig, corrected, method) {
          changes[nrow(changes) + 1, ] <<- list(col, row_desc, orig, corrected, method)
        }

        # ==== 1. Validate idtax column ====
        shiny::setProgress(0.2, message = i18n()$t("Checking taxon IDs..."))

        idtax_vals <- df[[idtax_col]]
        n_na_idtax <- sum(is.na(idtax_vals))
        if (n_na_idtax > 0) {
          add_error(idtax_col, "missing_values",
            sprintf("Taxon ID column has %d missing value(s)", n_na_idtax),
            "Rows with missing taxon IDs cannot be imported")
        }

        # Check if idtax values are integers
        if (!is.na(n_na_idtax)) {
          non_na_vals <- idtax_vals[!is.na(idtax_vals)]
          numeric_vals <- suppressWarnings(as.numeric(non_na_vals))
          n_non_numeric <- sum(is.na(numeric_vals))
          if (n_non_numeric > 0) {
            add_error(idtax_col, "type_mismatch",
              sprintf("Taxon ID column has %d non-numeric value(s)", n_non_numeric),
              "Taxon IDs must be integers")
          } else {
            n_non_int <- sum(numeric_vals != round(numeric_vals), na.rm = TRUE)
            if (n_non_int > 0) {
              add_warning(idtax_col, "type_mismatch",
                sprintf("Taxon ID column has %d non-integer value(s)", n_non_int),
                "Taxon IDs should be whole numbers")
            }
          }
        }

        # ==== 2. Validate trait columns ====
        shiny::setProgress(0.4, message = i18n()$t("Checking trait columns..."))

        all_trait_like <- c(trait_cols, feature_cols)

        for (user_col in names(all_trait_like)) {
          trait_name <- all_trait_like[user_col]
          role <- if (user_col %in% names(trait_cols)) "trait" else "feature"
          info <- traits_info[traits_info$trait == trait_name, ]
          if (nrow(info) == 0) next

          col_vals <- df[[user_col]]
          valuetype <- info$valuetype[1]
          n_total <- length(col_vals)
          n_na <- sum(is.na(col_vals))
          non_na_vals <- col_vals[!is.na(col_vals)]

          # -- NA check --
          na_pct <- round(n_na / n_total * 100, 1)
          if (n_na == n_total) {
            add_warning(user_col, "all_na",
              sprintf("Column '%s' (%s) is entirely NA", user_col, trait_name),
              sprintf("All %d values are missing — this %s will produce no data", n_total, role))
          } else if (na_pct > 50) {
            add_warning(user_col, "high_na",
              sprintf("Column '%s' has %.0f%% missing values (%d/%d)", user_col, na_pct, n_na, n_total),
              "")
          }

          # -- Type compatibility & conversion --
          if (valuetype %in% c("numeric", "integer")) {
            # Trait expects numeric — convert all; non-parseable become NA
            all_numeric <- suppressWarnings(as.numeric(col_vals))
            n_coerce_na <- sum(is.na(all_numeric)) - sum(is.na(col_vals))
            if (n_coerce_na > 0) {
              bad_vals <- col_vals[!is.na(col_vals) & is.na(all_numeric)]
              add_warning(user_col, "type_mismatch",
                sprintf("Column '%s' mapped to numeric trait '%s': %d non-numeric value(s) set to NA",
                        user_col, trait_name, n_coerce_na),
                paste("Values set to NA:", paste(utils::head(bad_vals, 5), collapse = ", ")))
              cleaned_df[[user_col]] <- all_numeric
              add_change(user_col,
                paste(utils::head(bad_vals, 5), collapse = ", "),
                "non-numeric", "NA",
                sprintf("Non-numeric values in '%s' replaced with NA", trait_name))
            } else if (!is.numeric(col_vals)) {
              cleaned_df[[user_col]] <- all_numeric
              add_change(user_col, "all rows", class(col_vals)[1], "numeric",
                sprintf("Converted to numeric for trait '%s'", trait_name))
            }

            {
              numeric_vals <- suppressWarnings(as.numeric(non_na_vals))
              valid_nums <- numeric_vals[!is.na(numeric_vals)]

              # -- Zero check --
              n_zeros <- sum(valid_nums == 0)
              if (n_zeros > 0) {
                add_warning(user_col, "zero_values",
                  sprintf("Column '%s' has %d zero value(s)", user_col, n_zeros),
                  "Verify that zero is a valid measurement")
              }

              # -- Range check (min) --
              min_allowed <- info$minallowedvalue[1]
              if (!is.na(min_allowed)) {
                n_below <- sum(valid_nums < min_allowed)
                if (n_below > 0) {
                  below_examples <- utils::head(valid_nums[valid_nums < min_allowed], 5)
                  add_warning(user_col, "below_min",
                    sprintf("Column '%s': %d value(s) below min allowed (%.4g)",
                            user_col, n_below, min_allowed),
                    paste("Examples:", paste(below_examples, collapse = ", ")))
                }
              }

              # -- Range check (max) --
              max_allowed <- info$maxallowedvalue[1]
              if (!is.na(max_allowed)) {
                n_above <- sum(valid_nums > max_allowed)
                if (n_above > 0) {
                  above_examples <- utils::head(valid_nums[valid_nums > max_allowed], 5)
                  add_warning(user_col, "above_max",
                    sprintf("Column '%s': %d value(s) above max allowed (%.4g)",
                            user_col, n_above, max_allowed),
                    paste("Examples:", paste(above_examples, collapse = ", ")))
                }
              }

              # -- Integer check --
              if (valuetype == "integer") {
                n_non_int <- sum(valid_nums != round(valid_nums))
                if (n_non_int > 0) {
                  add_warning(user_col, "non_integer",
                    sprintf("Column '%s' mapped to integer trait has %d non-integer value(s)",
                            user_col, n_non_int),
                    "Values will be stored as-is but trait expects integers")
                }
              }
            }
          } else if (valuetype %in% c("categorical", "ordinal")) {
            # Trait expects categorical — convert column to character if needed
            if (is.numeric(col_vals) || is.integer(col_vals)) {
              cleaned_df[[user_col]] <- as.character(col_vals)
              add_change(user_col, "all rows", class(col_vals)[1], "character",
                sprintf("Converted to character for categorical trait '%s'", trait_name))
            }

            # -- Factor levels check --
            fl <- info$factorlevels[1]
            if (!is.na(fl) && nchar(trimws(fl)) > 0) {
              allowed_levels <- trimws(strsplit(fl, ",")[[1]])
              char_vals <- as.character(non_na_vals)
              unmatched <- setdiff(unique(char_vals), allowed_levels)
              if (length(unmatched) > 0) {
                add_warning(user_col, "invalid_levels",
                  sprintf("Column '%s': %d value(s) not in allowed factor levels",
                          user_col, sum(char_vals %in% unmatched)),
                  paste("Unknown levels:", paste(utils::head(unmatched, 10), collapse = ", "),
                        "| Allowed:", paste(utils::head(allowed_levels, 10), collapse = ", ")))
              }
            }
          } else if (valuetype %in% c("character", "logical")) {
            # Ensure character
            if (!is.character(col_vals)) {
              cleaned_df[[user_col]] <- as.character(col_vals)
              add_change(user_col, "all rows", class(col_vals)[1], "character",
                sprintf("Converted to character for trait '%s'", trait_name))
            }
          }
        }

        # ==== 3. Validate metadata columns ====
        shiny::setProgress(0.6, message = i18n()$t("Checking metadata columns..."))

        for (user_col in names(meta_cols)) {
          db_col <- meta_cols[user_col]
          col_vals <- df[[user_col]]
          non_na_vals <- col_vals[!is.na(col_vals)]

          if (db_col %in% c("decimallatitude", "decimallongitude", "elevation")) {
            # Should be numeric — convert all; non-parseable become NA
            all_numeric <- suppressWarnings(as.numeric(col_vals))
            n_non_num <- sum(is.na(all_numeric)) - sum(is.na(col_vals))
            if (n_non_num > 0) {
              bad_vals <- col_vals[!is.na(col_vals) & is.na(all_numeric)]
              add_warning(user_col, "type_mismatch",
                sprintf("Column '%s' mapped to '%s': %d non-numeric value(s) set to NA",
                        user_col, db_col, n_non_num),
                paste("Values set to NA:", paste(utils::head(bad_vals, 5), collapse = ", ")))
              cleaned_df[[user_col]] <- all_numeric
              add_change(user_col,
                paste(utils::head(bad_vals, 5), collapse = ", "),
                "non-numeric", "NA",
                sprintf("Non-numeric values in '%s' replaced with NA", db_col))
            } else if (!is.numeric(col_vals)) {
              cleaned_df[[user_col]] <- all_numeric
              add_change(user_col, "all rows", class(col_vals)[1], "numeric",
                sprintf("Converted to numeric for '%s'", db_col))
            }

            # Range check for coordinates
            if (db_col == "decimallatitude" && length(non_na_vals) > 0) {
              num_vals <- suppressWarnings(as.numeric(non_na_vals))
              n_out <- sum(num_vals < -90 | num_vals > 90, na.rm = TRUE)
              if (n_out > 0)
                add_warning(user_col, "coordinate_range",
                  sprintf("%d latitude value(s) outside [-90, 90]", n_out), "")
            }
            if (db_col == "decimallongitude" && length(non_na_vals) > 0) {
              num_vals <- suppressWarnings(as.numeric(non_na_vals))
              n_out <- sum(num_vals < -180 | num_vals > 180, na.rm = TRUE)
              if (n_out > 0)
                add_warning(user_col, "coordinate_range",
                  sprintf("%d longitude value(s) outside [-180, 180]", n_out), "")
            }
          }

          if (db_col %in% c("year", "month", "day")) {
            all_numeric <- suppressWarnings(as.numeric(col_vals))
            numeric_vals <- suppressWarnings(as.numeric(non_na_vals))
            n_non_num <- sum(is.na(all_numeric)) - sum(is.na(col_vals))
            if (n_non_num > 0) {
              bad_vals <- col_vals[!is.na(col_vals) & is.na(all_numeric)]
              add_warning(user_col, "type_mismatch",
                sprintf("Column '%s' mapped to '%s': %d non-numeric value(s) set to NA",
                        user_col, db_col, n_non_num),
                paste("Values set to NA:", paste(utils::head(bad_vals, 5), collapse = ", ")))
              cleaned_df[[user_col]] <- all_numeric
              add_change(user_col,
                paste(utils::head(bad_vals, 5), collapse = ", "),
                "non-numeric", "NA",
                sprintf("Non-numeric values in '%s' replaced with NA", db_col))
            }
            if (db_col == "year" && length(numeric_vals) > 0) {
              bad_years <- numeric_vals[!is.na(numeric_vals) & (numeric_vals < 1900 | numeric_vals > 2100)]
              if (length(bad_years) > 0)
                add_warning(user_col, "year_range",
                  sprintf("%d year value(s) outside [1900, 2100]", length(bad_years)),
                  paste("Examples:", paste(utils::head(bad_years, 5), collapse = ", ")))
            }
            if (db_col == "month" && length(numeric_vals) > 0) {
              bad <- numeric_vals[!is.na(numeric_vals) & (numeric_vals < 1 | numeric_vals > 12)]
              if (length(bad) > 0)
                add_warning(user_col, "month_range",
                  sprintf("%d month value(s) outside [1, 12]", length(bad)), "")
            }
            if (db_col == "day" && length(numeric_vals) > 0) {
              bad <- numeric_vals[!is.na(numeric_vals) & (numeric_vals < 1 | numeric_vals > 31)]
              if (length(bad) > 0)
                add_warning(user_col, "day_range",
                  sprintf("%d day value(s) outside [1, 31]", length(bad)), "")
            }
          }
        }

        # ==== 4. Duplicate check ====
        shiny::setProgress(0.8, message = i18n()$t("Checking for duplicates..."))

        if (length(trait_cols) > 0) {
          for (user_col in names(trait_cols)) {
            trait_name <- trait_cols[user_col]
            # Check for duplicate idtax + trait value combinations
            dup_df <- df[!is.na(df[[user_col]]), c(idtax_col, user_col)]
            if (nrow(dup_df) > 0 && any(duplicated(dup_df))) {
              n_dups <- sum(duplicated(dup_df))
              add_warning(user_col, "duplicates",
                sprintf("Column '%s' (%s): %d duplicate idtax+value combination(s)",
                        user_col, trait_name, n_dups),
                "Duplicate rows may cause issues during import")
            }
          }
        }

        shiny::setProgress(1, message = i18n()$t("Validation complete!"))

        # ==== Build result ====
        n_errors <- nrow(errors)
        n_warnings <- nrow(warnings)
        n_changes <- nrow(changes)
        is_valid <- n_errors == 0

        result <- list(
          valid = is_valid,
          errors = errors,
          warnings = warnings,
          changes_made = changes,
          cleaned_data = cleaned_df,
          summary = list(
            total_rows = nrow(df),
            errors = n_errors,
            warnings = n_warnings,
            changes_applied = n_changes,
            valid = is_valid
          )
        )

        validation_result(result)

        if (is_valid) {
          shiny::showNotification(
            i18n()$t("Validation passed! Your data is ready to import."),
            type = "message", duration = 5
          )
        } else {
          shiny::showNotification(
            sprintf("%d error(s) found. Please review and fix your data.", n_errors),
            type = "warning", duration = 10
          )
        }

      }, message = i18n()$t("Running validation..."))
    })

    # ---- Render results ----
    output$validation_results <- shiny::renderUI({
      shiny::req(validation_result())
      result <- validation_result()

      shiny::tagList(
        # Summary cards
        shiny::fluidRow(
          shiny::column(3, shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
            shiny::h3(result$summary$total_rows, style = "margin: 0; color: #007bff;"),
            shiny::p(i18n()$t("Total Rows"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )),
          shiny::column(3, shiny::div(
            class = "card",
            style = sprintf("padding: 20px; background-color: #f8f9fa; border-left: 4px solid %s; text-align: center;",
                            if (result$summary$errors == 0) "#28a745" else "#dc3545"),
            shiny::h3(result$summary$errors,
                      style = sprintf("margin: 0; color: %s;",
                                      if (result$summary$errors == 0) "#28a745" else "#dc3545")),
            shiny::p(i18n()$t("Errors"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )),
          shiny::column(3, shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #ffc107; text-align: center;",
            shiny::h3(result$summary$warnings, style = "margin: 0; color: #ffc107;"),
            shiny::p(i18n()$t("Warnings"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )),
          shiny::column(3, shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #17a2b8; text-align: center;",
            shiny::h3(result$summary$changes_applied, style = "margin: 0; color: #17a2b8;"),
            shiny::p(i18n()$t("Auto-Fixed"), style = "margin: 5px 0 0 0; color: #6c757d;")
          ))
        ),

        shiny::hr(),

        # Overall status
        if (result$valid) {
          shiny::div(
            class = "alert alert-success", style = "font-size: 16px;",
            shiny::icon("check-circle", style = "font-size: 24px;"),
            shiny::strong(paste0(" ", i18n()$t("Validation Passed!"), " ")),
            i18n()$t("Your data meets all requirements and is ready to import."),
            if (result$summary$warnings > 0) {
              shiny::tagList(
                shiny::br(),
                shiny::tags$small(
                  sprintf(i18n()$t("Note: %d warning(s) found but do not prevent import."), result$summary$warnings),
                  style = "color: #856404;"
                )
              )
            }
          )
        } else {
          shiny::div(
            class = "alert alert-danger", style = "font-size: 16px;",
            shiny::icon("exclamation-circle", style = "font-size: 24px;"),
            shiny::strong(paste0(" ", i18n()$t("Validation Failed"), " ")),
            sprintf(i18n()$t("Found %d error(s) that must be fixed before import."), result$summary$errors)
          )
        },

        # Errors table
        if (nrow(result$errors) > 0) {
          shiny::tagList(
            shiny::h4(
              shiny::icon("times-circle", style = "color: #dc3545;"),
              paste0(" ", i18n()$t("Errors")),
              style = "color: #dc3545; margin-top: 30px;"
            ),
            DT::DTOutput(ns("errors_table"))
          )
        },

        # Warnings table
        if (nrow(result$warnings) > 0) {
          shiny::tagList(
            shiny::h4(
              shiny::icon("exclamation-triangle", style = "color: #ffc107;"),
              paste0(" ", i18n()$t("Warnings")),
              style = "color: #ffc107; margin-top: 30px;"
            ),
            DT::DTOutput(ns("warnings_table"))
          )
        },

        # Changes table
        if (nrow(result$changes_made) > 0) {
          shiny::tagList(
            shiny::h4(
              shiny::icon("wrench", style = "color: #17a2b8;"),
              paste0(" ", i18n()$t("Auto-Applied Fixes")),
              style = "color: #17a2b8; margin-top: 30px;"
            ),
            shiny::p(
              i18n()$t("The following values were automatically corrected during validation:"),
              style = "color: #6c757d;"
            ),
            DT::DTOutput(ns("changes_table"))
          )
        }
      )
    })

    # ---- DT tables (separate outputs, not nested in renderUI) ----
    output$errors_table <- DT::renderDT({
      shiny::req(validation_result())
      DT::datatable(
        validation_result()$errors,
        options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE, class = "display cell-border stripe"
      ) %>% DT::formatStyle(
        columns = 1:ncol(validation_result()$errors),
        backgroundColor = "#fff5f5"
      )
    })

    output$warnings_table <- DT::renderDT({
      shiny::req(validation_result())
      DT::datatable(
        validation_result()$warnings,
        options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE, class = "display cell-border stripe"
      ) %>% DT::formatStyle(
        columns = 1:ncol(validation_result()$warnings),
        backgroundColor = "#fffbf0"
      )
    })

    output$changes_table <- DT::renderDT({
      shiny::req(validation_result())
      DT::datatable(
        validation_result()$changes_made,
        options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE, class = "display cell-border stripe"
      ) %>% DT::formatStyle(
        columns = 1:ncol(validation_result()$changes_made),
        backgroundColor = "#f0f8ff"
      )
    })

    # ---- Return ----
    shiny::reactive(validation_result())
  })
}
