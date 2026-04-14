# Feature Wizard - Step 4: Lookup Matching
#
# Module for matching people names and other lookup values to database entries.
# Uses mod_lookup_matcher (same as import wizard) for the interactive matching UI.

#' Feature Wizard Step 4: Lookup Matching - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step4_lookup_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("link"),
      i18n$t("Step 4: Match Lookup Values"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Match people names and other lookup values to database entries."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    shiny::actionButton(
      ns("analyze_lookups"),
      shiny::tagList(shiny::icon("search"), " ", i18n$t("Analyze Lookup Values")),
      class = "btn-primary btn-lg",
      style = "margin-bottom: 30px;"
    ),

    shiny::uiOutput(ns("analysis_results")),
    shiny::uiOutput(ns("matching_interface"))
  )
}


#' Feature Wizard Step 4: Lookup Matching - Server
#'
#' @param id Module namespace ID
#' @param feature_data Reactive containing the prepared feature data
#' @param feature_config Reactive containing the feature configuration
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object
#' @return List of reactives: data (matched data), complete (boolean)
#' @keywords internal
#' @export
mod_feat_step4_lookup_server <- function(id, feature_data, feature_config, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    analysis_result  <- shiny::reactiveVal(NULL)
    non_exact        <- shiny::reactiveVal(NULL)   # list(col = c("name1", ...))
    matched_data     <- shiny::reactiveVal(NULL)
    matching_complete <- shiny::reactiveVal(FALSE)

    # ---- Analyze lookup values ----
    shiny::observeEvent(input$analyze_lookups, {
      shiny::req(feature_data(), feature_config(), con())

      config      <- feature_config()
      data        <- feature_data()
      people_cols <- config$people_columns

      if (is.null(people_cols) || length(people_cols) == 0) {
        matched_data(data)
        matching_complete(TRUE)
        shiny::showNotification(
          i18n()$t("No lookup matching needed. You can proceed."),
          type = "message", duration = 5
        )
        return()
      }

      shiny::withProgress({
        shiny::setProgress(0.3, message = i18n()$t("Analyzing lookup values..."))

        tryCatch({
          colnam_data <- DBI::dbGetQuery(con(),
            "SELECT id_table_colnam, colnam FROM table_colnam ORDER BY colnam"
          )

          exact_matches <- list()
          needs_matching <- list()

          for (col in people_cols) {
            if (!col %in% names(data)) next

            raw_vals <- data[[col]]
            raw_vals <- raw_vals[!is.na(raw_vals) & trimws(raw_vals) != ""]
            split_vals <- unique(trimws(unlist(strsplit(as.character(raw_vals), ","))))
            split_vals <- split_vals[split_vals != ""]

            if (length(split_vals) == 0) next

            exact   <- character(0)
            no_match <- character(0)

            for (nm in split_vals) {
              idx <- which(tolower(colnam_data$colnam) == tolower(nm))
              if (length(idx) > 0) {
                exact_matches[[nm]] <- colnam_data$id_table_colnam[idx[1]]
                exact <- c(exact, nm)
              } else {
                no_match <- c(no_match, nm)
              }
            }

            if (length(no_match) > 0) {
              needs_matching[[col]] <- no_match
            }
          }

          total_exact     <- length(exact_matches)
          total_names     <- total_exact + sum(lengths(needs_matching))
          total_non_exact <- sum(lengths(needs_matching))

          result <- list(
            total_names     = total_names,
            exact_count     = total_exact,
            non_exact_count = total_non_exact,
            exact_matches   = exact_matches,
            colnam_data     = colnam_data
          )

          analysis_result(result)
          non_exact(needs_matching)

          if (total_non_exact == 0) {
            updated <- .apply_people_id_conversion(data, people_cols, exact_matches)
            matched_data(updated)
            matching_complete(TRUE)
            shiny::showNotification(
              i18n()$t("All names matched. You can proceed."),
              type = "message", duration = 5
            )
          }

        }, error = function(e) {
          cli::cli_alert_danger("Lookup analysis failed: {e$message}")
          shiny::showNotification(paste("Error:", e$message), type = "error", duration = 10)
        })

        shiny::setProgress(1, message = i18n()$t("Analysis complete!"))
      }, message = i18n()$t("Analyzing..."))
    })

    # ---- Analysis summary cards ----
    output$analysis_results <- shiny::renderUI({
      res <- analysis_result()
      if (is.null(res)) return(NULL)

      shiny::tagList(
        shiny::h4(i18n()$t("Analysis Results"),
                  style = "margin-top: 30px; margin-bottom: 15px;"),

        shiny::fluidRow(
          shiny::column(4, shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
            shiny::h3(res$total_names, style = "margin: 0; color: #007bff;"),
            shiny::p(i18n()$t("Total Names"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )),
          shiny::column(4, shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
            shiny::h3(res$exact_count, style = "margin: 0; color: #28a745;"),
            shiny::p(i18n()$t("Exact Matches"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )),
          shiny::column(4, shiny::div(
            class = "card",
            style = sprintf(
              "padding: 20px; background-color: #f8f9fa; border-left: 4px solid %s; text-align: center;",
              if (res$non_exact_count == 0) "#28a745" else "#ffc107"
            ),
            shiny::h3(res$non_exact_count,
              style = sprintf("margin: 0; color: %s;",
                if (res$non_exact_count == 0) "#28a745" else "#ffc107")),
            shiny::p(i18n()$t("Need Matching"), style = "margin: 5px 0 0 0; color: #6c757d;")
          ))
        ),

        shiny::hr(),

        if (res$non_exact_count == 0) {
          shiny::div(
            class = "alert alert-success",
            shiny::icon("check-circle"),
            shiny::strong(paste0(" ", i18n()$t("All names matched!"))), " ",
            i18n()$t("You can proceed to the next step.")
          )
        } else {
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            shiny::strong(sprintf(" %d ", res$non_exact_count)),
            i18n()$t("name(s) not found in database. Please resolve below.")
          )
        }
      )
    })

    # ---- Matching interface: delegate entirely to mod_lookup_matcher ----
    output$matching_interface <- shiny::renderUI({
      res <- analysis_result()
      nm  <- non_exact()
      if (is.null(res) || res$non_exact_count == 0 || is.null(nm) || length(nm) == 0) {
        return(NULL)
      }
      shiny::tagList(
        shiny::hr(),
        mod_lookup_matcher_ui(ns("matcher"))
      )
    })

    # Initialize matcher server (always, regardless of whether it's visible).
    # Pass people_columns from feature_config explicitly so that mod_lookup_matcher
    # never needs to auto-detect them — this guarantees "Create New Entry" appears.
    matcher_result <- mod_lookup_matcher_server(
      "matcher",
      invalid_values       = non_exact,
      con                  = con,
      people_cols_override = shiny::reactive(feature_config()$people_columns)
    )

    # ---- Apply matches when user confirms in matcher ----
    shiny::observeEvent(matcher_result$applied(), {
      shiny::req(matcher_result$applied() == TRUE)
      shiny::req(feature_data(), feature_config(), analysis_result())

      data        <- feature_data()
      config      <- feature_config()
      res         <- analysis_result()
      user_matches <- matcher_result$matches()

      # Merge exact matches with user-resolved matches
      all_matches <- res$exact_matches

      for (col_name in names(user_matches)) {
        for (user_val in names(user_matches[[col_name]])) {
          all_matches[[user_val]] <- as.integer(user_matches[[col_name]][[user_val]])
        }
      }

      updated <- .apply_people_id_conversion(data, config$people_columns, all_matches)
      matched_data(updated)
      matching_complete(TRUE)

      shiny::showNotification(
        i18n()$t("Lookup matching complete. You can proceed."),
        type = "message", duration = 5
      )
    })

    return(list(
      data     = shiny::reactive(matched_data()),
      complete = shiny::reactive(matching_complete())
    ))
  })
}


#' Convert people name strings to database IDs in data
#' @keywords internal
.apply_people_id_conversion <- function(data, people_columns, name_to_id_map) {
  if (is.null(people_columns) || length(people_columns) == 0) return(data)

  for (col in people_columns) {
    if (!col %in% names(data)) next

    data[[col]] <- sapply(seq_len(nrow(data)), function(i) {
      val <- data[[col]][i]
      if (is.na(val) || trimws(as.character(val)) == "") return(NA_character_)

      names_list <- trimws(strsplit(as.character(val), ",")[[1]])
      ids <- sapply(names_list, function(nm) {
        if (nm %in% names(name_to_id_map)) as.character(name_to_id_map[[nm]])
        else NA_character_
      })
      ids <- ids[!is.na(ids)]
      if (length(ids) == 0) return(NA_character_)
      paste(ids, collapse = ",")
    })
  }

  data
}
