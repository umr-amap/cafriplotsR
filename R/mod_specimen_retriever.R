# Specimen Retriever Module
#
# Module for retrieving matching specimens from database based on
# matched collectors and specimen numbers

#' Specimen Retriever Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_specimen_retriever_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      class = "section-card",
      shiny::h3(
        shiny::icon("database"),
        " ",
        i18n$t("Step 4: Retrieve Matching Specimens"),
        style = "color: #495057; margin-bottom: 20px;"
      ),

      shiny::p(
        i18n$t("Query the database to find specimens matching the collector and number combinations."),
        style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
      ),

      # Retrieve button
      shiny::actionButton(
        ns("retrieve_specimens"),
        shiny::tagList(shiny::icon("download"), paste0(" ", i18n$t("Retrieve Specimens"))),
        class = "btn-primary btn-lg",
        style = "margin-bottom: 30px;"
      ),

      # Retrieval results
      shiny::uiOutput(ns("retrieval_summary")),

      # Preview retrieved data
      shiny::uiOutput(ns("retrieved_data_preview"))
    )
  )
}


#' Specimen Retriever Module - Server
#'
#' @param id Module namespace ID
#' @param parsed_data Reactive containing parsed herbarium data from Step 2
#' @param collector_matches Reactive containing collector ID matches from Step 3
#' @param con Reactive database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive list containing retrieved specimens
#' @keywords internal
#' @export
mod_specimen_retriever_server <- function(id, parsed_data, collector_matches, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for retrieved data
    retrieved_specimens <- shiny::reactiveVal(NULL)
    retrieval_complete <- shiny::reactiveVal(FALSE)

    # Retrieve specimens when button clicked
    shiny::observeEvent(input$retrieve_specimens, {
      shiny::req(parsed_data(), collector_matches(), con())

      parsed <- parsed_data()
      matches <- collector_matches()

      cli::cli_alert_info("Retrieve specimens button clicked")
      cli::cli_alert_info("  parsed data: {nrow(parsed)} rows")
      cli::cli_alert_info("  matches: {paste(names(matches), collapse=', ')}")

      shiny::withProgress(message = i18n()$t("Retrieving specimens from database..."), value = 0, {

        shiny::incProgress(0.2, detail = i18n()$t("Applying collector matches..."))

        tryCatch({
          # Extract collector matches
          collector_map <- if (is.list(matches)) matches$collector else NULL

          # Convert to data frame, dropping anything that is not a usable id.
          # Built defensively so an empty or malformed match list yields an
          # empty two-column frame rather than breaking the join below.
          collector_names <- as.character(names(collector_map))
          collector_ids <- suppressWarnings(as.integer(unlist(collector_map)))

          if (length(collector_names) != length(collector_ids)) {
            cli::cli_alert_warning("Unexpected collector match structure, ignoring it")
            collector_names <- character(0)
            collector_ids <- integer(0)
          }

          match_df <- data.frame(
            extracted_collector = collector_names,
            id_colnam = collector_ids,
            stringsAsFactors = FALSE
          )
          match_df <- match_df[!is.na(match_df$id_colnam), , drop = FALSE]

          if (nrow(match_df) == 0) {
            cli::cli_alert_warning("No collector was matched to the database")
            shiny::showNotification(
              i18n()$t("No collector has been matched to the database. Go back to the collector matching step (Step 3) and apply the matches."),
              type = "warning",
              duration = 10
            )
          }

          # Join with parsed data
          parsed_with_ids <- parsed %>%
            dplyr::left_join(match_df, by = "extracted_collector")

          # The parser always supplies idtax_n, but guard so a malformed
          # upstream table degrades instead of erroring in the join below
          if (!"idtax_n" %in% names(parsed_with_ids)) {
            parsed_with_ids$idtax_n <- NA_integer_
          }
          parsed_with_ids <- parsed_with_ids %>%
            dplyr::rename(individual_idtax_n = idtax_n)

          cli::cli_alert_success("Applied collector matches: {nrow(parsed_with_ids)} rows")

          shiny::incProgress(0.4, detail = i18n()$t("Querying specimens table..."))

          # Get collector summaries with min/max specimen numbers
          queryable <- parsed_with_ids %>%
            dplyr::filter(!is.na(id_colnam), !is.na(extracted_number))

          if (nrow(queryable) == 0) {
            # summarise() would still evaluate min()/max() once on an empty
            # frame, warning about "no non-missing arguments"
            collector_ranges <- data.frame(
              id_colnam = integer(0),
              min_number = numeric(0),
              max_number = numeric(0)
            )
          } else {
            collector_ranges <- queryable %>%
              dplyr::group_by(id_colnam) %>%
              dplyr::summarise(
                min_number = min(extracted_number, na.rm = TRUE),
                max_number = max(extracted_number, na.rm = TRUE),
                .groups = "drop"
              )
          }

          if (nrow(collector_ranges) == 0) {
            # Nothing queryable: no collector was matched, or no number parsed
            cli::cli_alert_warning(
              "No collector/number combination available to query the specimens table"
            )
            shiny::showNotification(
              i18n()$t("No collector and number combination is available to query. Check the collector matching step (Step 3)."),
              type = "warning",
              duration = 10
            )
            all_specimens <- .empty_retrieved_specimens()
          } else {
            cli::cli_alert_info("Querying specimens for {nrow(collector_ranges)} collectors")

            # Query all specimens for each collector in the number range
            all_specimens <- purrr::pmap_dfr(
              list(
                collector_ranges$id_colnam,
                collector_ranges$min_number,
                collector_ranges$max_number
              ),
              function(collector_id, min_num, max_num) {
                cli::cli_alert_info("  Querying collector {collector_id}: specimens {min_num}-{max_num}")

                found <- query_specimens(
                  id_colnam = collector_id,
                  number_min = min_num,
                  number_max = max_num,
                  subset_columns = TRUE,
                  con = con()
                )

                # query_specimens() returns the raw, un-enriched table when it
                # finds nothing, so normalise before the rows are bound
                .normalize_retrieved_specimens(found)
              }
            )
          }

          all_specimens <- .normalize_retrieved_specimens(all_specimens)

          cli::cli_alert_success("Retrieved {nrow(all_specimens)} specimens from database")

          shiny::incProgress(0.6, detail = i18n()$t("Matching to individuals..."))

          # Match specimens to parsed data
          # Join on collector ID and specimen number
          specimens_side <- all_specimens %>%
            dplyr::select(
              id_colnam,
              colnbr,
              id_specimen,
              specimen_idtax_n = idtax_f,
              collector_name = colnam
            )

          preliminary_links <- parsed_with_ids %>%
            dplyr::left_join(
              specimens_side,
              by = c("id_colnam" = "id_colnam", "extracted_number" = "colnbr")
            ) %>%
            dplyr::mutate(
              match_status = ifelse(!is.na(id_specimen), "found", "not_found")
            ) %>%
            dplyr::select(
              id_n, tag, code_individu, plot_name,
              extracted_collector, collector_name, extracted_number,
              id_colnam, id_specimen,
              link_type, source_column, original_value,
              individual_idtax_n, specimen_idtax_n, match_status
            )

          shiny::incProgress(0.8, detail = i18n()$t("Finalizing..."))

          n_found <- sum(!is.na(preliminary_links$id_specimen))
          n_total <- nrow(preliminary_links)

          if (n_total == 0) {
            cli::cli_alert_warning("No preliminary links found")
            retrieved_specimens(NULL)
            shiny::showNotification(
              i18n()$t("No matching specimens found in the database."),
              type = "warning",
              duration = 10
            )
            retrieval_complete(FALSE)
          } else if (n_found == 0) {
            # The parsed collector/number combinations simply are not in the
            # specimens table: show what was searched instead of failing
            cli::cli_alert_warning(
              "None of the {n_total} collector/number combination(s) exist in the specimens table"
            )
            retrieved_specimens(preliminary_links)
            retrieval_complete(FALSE)

            shiny::incProgress(1, detail = i18n()$t("Complete!"))

            shiny::showNotification(
              sprintf(
                i18n()$t("None of the %d collector and number combinations exist in the specimens table. Review the table below, then correct the collector matching or add the missing specimens."),
                n_total
              ),
              type = "warning",
              duration = NULL
            )
          } else {
            retrieved_specimens(preliminary_links)
            retrieval_complete(TRUE)

            shiny::incProgress(1, detail = i18n()$t("Complete!"))

            cli::cli_alert_success("Found {n_found}/{n_total} matching specimens")

            shiny::showNotification(
              sprintf(
                i18n()$t("Found %d matching specimens out of %d potential links."),
                n_found,
                n_total
              ),
              type = "message",
              duration = 5
            )
          }
        }, error = function(e) {
          cli::cli_alert_danger("Error retrieving specimens: {e$message}")
          cli::cli_alert_danger("Error class: {class(e)}")
          cli::cli_alert_danger("Full error: {paste(capture.output(print(e)), collapse=' ')}")
          shiny::showNotification(
            paste(i18n()$t("Error retrieving specimens:"), e$message),
            type = "error",
            duration = NULL
          )
          retrieval_complete(FALSE)
        })
      })
    })

    # Render retrieval summary
    output$retrieval_summary <- shiny::renderUI({
      shiny::req(retrieved_specimens())

      retrieved <- retrieved_specimens()
      n_total <- nrow(retrieved)
      n_found <- sum(!is.na(retrieved$id_specimen))
      n_not_found <- sum(is.na(retrieved$id_specimen))
      match_rate <- round(n_found / n_total * 100, 1)

      shiny::tagList(
        shiny::h4(i18n()$t("Retrieval Results"), style = "margin-top: 30px; margin-bottom: 15px;"),

        shiny::fluidRow(
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
              shiny::h3(n_total, style = "margin: 0; color: #007bff;"),
              shiny::p(i18n()$t("Potential Links"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
              shiny::h3(n_found, style = "margin: 0; color: #28a745;"),
              shiny::p(i18n()$t("Specimens Found"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #dc3545; text-align: center;",
              shiny::h3(n_not_found, style = "margin: 0; color: #dc3545;"),
              shiny::p(i18n()$t("Not Found"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #17a2b8; text-align: center;",
              shiny::h3(paste0(match_rate, "%"), style = "margin: 0; color: #17a2b8;"),
              shiny::p(i18n()$t("Match Rate"), style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          )
        ),

        shiny::hr(),

        if (n_found > 0) {
          shiny::div(
            class = "alert alert-success",
            shiny::icon("check-circle"),
            " ",
            i18n()$t("Specimens retrieved! You can proceed to taxonomic validation.")
          )
        } else {
          shiny::div(
            class = "alert alert-danger",
            shiny::icon("times-circle"),
            " ",
            i18n()$t("No matching specimens found. Check collector names and numbers.")
          )
        }
      )
    })

    # Preview retrieved data
    output$retrieved_data_preview <- shiny::renderUI({
      shiny::req(retrieved_specimens())

      retrieved <- retrieved_specimens()

      shiny::tagList(
        shiny::h4(i18n()$t("Retrieved Specimens Preview"), style = "margin-top: 30px; margin-bottom: 15px;"),
        DT::dataTableOutput(session$ns("retrieved_table"))
      )
    })

    output$retrieved_table <- DT::renderDataTable({
      shiny::req(retrieved_specimens())

      retrieved <- retrieved_specimens()

      # Select columns to display
      display_data <- retrieved %>%
        dplyr::select(
          id_n, tag, plot_name,
          collector_name, extracted_number,
          id_specimen, specimen_idtax_n,
          link_type, match_status
        )

      DT::datatable(
        display_data,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle(
          "match_status",
          backgroundColor = DT::styleEqual(
            c("found", "not_found"),
            c("#d4edda", "#f8d7da")
          )
        )
    })

    # Return retrieved data and status
    return(list(
      retrieved_specimens = retrieved_specimens,
      is_complete = retrieval_complete
    ))
  })
}


#' Empty specimen table with the columns the retriever needs
#'
#' `query_specimens()` returns the raw, un-enriched (and possibly zero-column)
#' table when nothing matches, which breaks the downstream join and mutate.
#' This gives a zero-row table carrying the columns the retriever relies on.
#'
#' @return A zero-row tibble with columns `id_specimen`, `id_colnam`, `colnbr`,
#'   `idtax_n`, `idtax_f` and `colnam`.
#' @keywords internal
.empty_retrieved_specimens <- function() {
  dplyr::tibble(
    id_specimen = integer(0),
    id_colnam = integer(0),
    colnbr = numeric(0),
    idtax_n = integer(0),
    idtax_f = integer(0),
    colnam = character(0)
  )
}


#' Ensure a retrieved specimen table carries the expected columns
#'
#' Adds any column missing from a `query_specimens()` result - which happens
#' when no specimen matches, or when collector/taxonomy enrichment was skipped
#' - filled with `NA` of the right type, so callers can join and select on
#' them unconditionally.
#'
#' @param specimens Data frame returned by `query_specimens()`, or `NULL`.
#'
#' @return A tibble with at least the columns of
#'   [.empty_retrieved_specimens()].
#' @keywords internal
.normalize_retrieved_specimens <- function(specimens) {

  template <- .empty_retrieved_specimens()

  if (is.null(specimens) || !is.data.frame(specimens) || nrow(specimens) == 0) {
    return(template)
  }

  for (col in names(template)) {
    if (!col %in% names(specimens)) {
      specimens[[col]] <- template[[col]][NA_integer_]
    }
  }

  dplyr::as_tibble(specimens)
}
