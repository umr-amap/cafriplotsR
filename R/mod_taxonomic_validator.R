# Taxonomic Validator Module
#
# Module for validating taxonomic matches between individuals and specimens
# and allowing user review of potential links

#' Taxonomic Validator Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_taxonomic_validator_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      class = "section-card",
      shiny::h3(
        shiny::icon("check-circle"),
        " ",
        i18n$t("Step 5: Validate Taxonomic Matches"),
        style = "color: #495057; margin-bottom: 20px;"
      ),

      shiny::p(
        i18n$t("Review taxonomic matches between individuals and specimens. Links with different genus identifications require manual review."),
        style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
      ),

      # Validate button
      shiny::actionButton(
        ns("validate_taxonomy"),
        shiny::tagList(shiny::icon("microscope"), paste0(" ", i18n$t("Validate Taxonomy"))),
        class = "btn-primary btn-lg",
        style = "margin-bottom: 30px;"
      ),

      # Validation results
      shiny::uiOutput(ns("validation_summary")),

      # Filter controls
      shiny::uiOutput(ns("filter_controls")),

      # Interactive review table
      shiny::uiOutput(ns("review_table_ui"))
    )
  )
}


#' Taxonomic Validator Module - Server
#'
#' @param id Module namespace ID
#' @param preliminary_links Reactive containing retrieved specimens from Step 4
#' @param con_taxa Reactive taxa database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive list containing validated links
#' @keywords internal
#' @export
mod_taxonomic_validator_server <- function(id, preliminary_links, con_taxa, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for validation data
    validated_data <- shiny::reactiveVal(NULL)
    user_decisions <- shiny::reactiveValues()
    validation_complete <- shiny::reactiveVal(FALSE)
    # Trigger to force table re-render when decisions change
    decisions_changed <- shiny::reactiveVal(0)
    # Track current page to preserve position when table re-renders
    current_page_start <- shiny::reactiveVal(0)

    # Validate taxonomy when button clicked
    shiny::observeEvent(input$validate_taxonomy, {
      shiny::req(preliminary_links(), con_taxa())

      links <- preliminary_links()

      cli::cli_alert_info("Starting taxonomic validation with {nrow(links)} preliminary links")

      # Filter to only found specimens (handle NA in match_status)
      links_found <- links %>%
        dplyr::filter(!is.na(match_status) & match_status == "found")

      cli::cli_alert_info("Filtered to {nrow(links_found)} found specimens")

      if (nrow(links_found) == 0) {
        shiny::showNotification(
          i18n()$t("No specimens found to validate."),
          type = "warning",
          duration = 5
        )
        return()
      }

      shiny::withProgress(message = i18n()$t("Validating taxonomy..."), value = 0, {

        shiny::incProgress(0.3, detail = i18n()$t("Querying taxonomic information..."))

        tryCatch({
          cli::cli_alert_info("Step 1: Enriching with taxonomy")

          # Enrich with taxonomic information
          links_with_taxonomy <- .enrich_with_taxonomy(links_found, con_taxa())

          cli::cli_alert_success("Step 1 complete: {nrow(links_with_taxonomy)} links enriched")

          shiny::incProgress(0.6, detail = i18n()$t("Comparing taxonomy..."))

          cli::cli_alert_info("Step 2: Validating taxonomic matches")

          # Validate taxonomic matches
          links_validated <- .validate_taxonomic_matches(links_with_taxonomy)

          cli::cli_alert_success("Step 2 complete: {nrow(links_validated)} links validated")

          shiny::incProgress(0.9, detail = i18n()$t("Finalizing..."))

          # Initialize user decisions - reject different_family and duplicate type links by default
          for (i in seq_len(nrow(links_validated))) {
            row_id <- paste0("row_", i)
            # Auto-reject links with different family or duplicate type_individual
            if ((!is.na(links_validated$taxonomic_match[i]) &&
                 links_validated$taxonomic_match[i] == "different_family") ||
                (!is.na(links_validated$has_duplicate_type[i]) &&
                 links_validated$has_duplicate_type[i] == TRUE)) {
              user_decisions[[paste0(row_id, "_decision")]] <- "reject"
            } else {
              user_decisions[[paste0(row_id, "_decision")]] <- "accept"
            }
          }

          validated_data(links_validated)
          validation_complete(FALSE)  # User needs to review

          shiny::incProgress(1, detail = i18n()$t("Complete!"))

          n_exact <- sum(links_validated$taxonomic_match == "exact", na.rm = TRUE)
          n_genus <- sum(links_validated$taxonomic_match == "same_genus", na.rm = TRUE)
          n_family <- sum(links_validated$taxonomic_match == "same_family", na.rm = TRUE)
          n_diff <- sum(links_validated$taxonomic_match == "different_family", na.rm = TRUE)
          n_duplicates <- sum(links_validated$has_duplicate_type, na.rm = TRUE)

          # Build notification message
          notif_msg <- sprintf(
            i18n()$t("Validation complete: %d exact, %d same genus, %d same family, %d different family"),
            n_exact, n_genus, n_family, n_diff
          )

          if (n_duplicates > 0) {
            notif_msg <- paste0(
              notif_msg,
              sprintf(i18n()$t("\n⚠️ WARNING: %d duplicate type_individual links found and auto-rejected!"), n_duplicates)
            )
          }

          shiny::showNotification(
            notif_msg,
            type = if (n_duplicates > 0) "warning" else "message",
            duration = if (n_duplicates > 0) 10 else 5
          )

        }, error = function(e) {
          shiny::showNotification(
            paste(i18n()$t("Error validating taxonomy:"), e$message),
            type = "error",
            duration = NULL
          )
        })
      })
    })

    # Render validation summary
    output$validation_summary <- shiny::renderUI({
      shiny::req(validated_data())

      validated <- validated_data()
      n_total <- nrow(validated)
      n_exact <- sum(validated$taxonomic_match == "exact", na.rm = TRUE)
      n_genus <- sum(validated$taxonomic_match == "same_genus", na.rm = TRUE)
      n_family <- sum(validated$taxonomic_match == "same_family", na.rm = TRUE)
      n_diff <- sum(validated$taxonomic_match == "different_family", na.rm = TRUE)

      shiny::tagList(
        shiny::h4(i18n()$t("Validation Results"), style = "margin-top: 30px; margin-bottom: 15px;"),

        shiny::fluidRow(
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
              shiny::h4(n_total, style = "margin: 0; color: #007bff;"),
              shiny::p(i18n()$t("Total Links"), style = "margin: 5px 0 0 0; color: #6c757d; font-size: 12px;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
              shiny::h4(n_exact, style = "margin: 0; color: #28a745;"),
              shiny::p(i18n()$t("Exact Match"), style = "margin: 5px 0 0 0; color: #6c757d; font-size: 12px;")
            )
          ),
          shiny::column(
            2,
            shiny::div(
              class = "card",
              style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #ffc107; text-align: center;",
              shiny::h4(n_genus, style = "margin: 0; color: #ffc107;"),
              shiny::p(i18n()$t("Same Genus"), style = "margin: 5px 0 0 0; color: #6c757d; font-size: 12px;")
            )
          ),
          shiny::column(
            2,
            shiny::div(
              class = "card",
              style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #ff9800; text-align: center;",
              shiny::h4(n_family, style = "margin: 0; color: #ff9800;"),
              shiny::p(i18n()$t("Same Family"), style = "margin: 5px 0 0 0; color: #6c757d; font-size: 12px;")
            )
          ),
          shiny::column(
            2,
            shiny::div(
              class = "card",
              style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #dc3545; text-align: center;",
              shiny::h4(n_diff, style = "margin: 0; color: #dc3545;"),
              shiny::p(i18n()$t("Diff. Family"), style = "margin: 5px 0 0 0; color: #6c757d; font-size: 12px;")
            )
          )
        ),

        shiny::hr()
      )
    })

    # Filter controls
    output$filter_controls <- shiny::renderUI({
      shiny::req(validated_data())

      ns <- session$ns

      shiny::div(
        style = "margin-bottom: 20px;",
        shiny::h5(i18n()$t("Filter Links")),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(
              ns("filter_match"),
              i18n()$t("Taxonomic Match"),
              choices = c(
                setNames("all", i18n()$t("All")),
                setNames("exact", i18n()$t("Exact Match")),
                setNames("same_genus", i18n()$t("Same Genus")),
                setNames("same_family", i18n()$t("Same Family")),
                setNames("different_family", i18n()$t("Different Family"))
              ),
              selected = "all"
            )
          ),
          shiny::column(
            4,
            shiny::selectInput(
              ns("filter_priority"),
              i18n()$t("Priority"),
              choices = c(
                setNames("all", i18n()$t("All")),
                setNames("auto_approve", i18n()$t("Auto-approve")),
                setNames("review_recommended", i18n()$t("Review Recommended")),
                setNames("review_required", i18n()$t("Review Required"))
              ),
              selected = "all"
            )
          ),
          shiny::column(
            6,
            shiny::div(
              style = "margin-top: 25px;",
              shiny::actionButton(
                ns("select_all"),
                i18n()$t("Select All"),
                class = "btn-secondary btn-sm"
              ),
              shiny::actionButton(
                ns("deselect_all"),
                i18n()$t("Deselect All"),
                class = "btn-secondary btn-sm",
                style = "margin-left: 5px;"
              ),
              shiny::actionButton(
                ns("reject_different_family"),
                i18n()$t("Reject Different Family"),
                class = "btn-warning btn-sm",
                style = "margin-left: 10px;"
              ),
              shiny::actionButton(
                ns("confirm_selection"),
                i18n()$t("Confirm Selection"),
                class = "btn-primary",
                style = "margin-left: 10px;"
              )
            )
          )
        )
      )
    })

    # Select all links
    shiny::observeEvent(input$select_all, {
      shiny::req(validated_data())

      validated <- validated_data()
      for (i in seq_len(nrow(validated))) {
        row_id <- paste0("row_", i)
        user_decisions[[paste0(row_id, "_decision")]] <- "accept"
      }

      # Save current page position before re-render
      if (!is.null(input$review_table_state)) {
        current_page_start(input$review_table_state$start)
      }

      # Trigger table update
      decisions_changed(decisions_changed() + 1)

      shiny::showNotification(
        i18n()$t("All links selected."),
        type = "message",
        duration = 3
      )
    })

    # Deselect all links
    shiny::observeEvent(input$deselect_all, {
      shiny::req(validated_data())

      validated <- validated_data()
      for (i in seq_len(nrow(validated))) {
        row_id <- paste0("row_", i)
        user_decisions[[paste0(row_id, "_decision")]] <- "reject"
      }

      # Save current page position before re-render
      if (!is.null(input$review_table_state)) {
        current_page_start(input$review_table_state$start)
      }

      # Trigger table update
      decisions_changed(decisions_changed() + 1)

      shiny::showNotification(
        i18n()$t("All links deselected."),
        type = "message",
        duration = 3
      )
    })

    # Reject links with different family
    shiny::observeEvent(input$reject_different_family, {
      shiny::req(validated_data())

      validated <- validated_data()
      n_rejected <- 0
      for (i in seq_len(nrow(validated))) {
        if (!is.na(validated$taxonomic_match[i]) && validated$taxonomic_match[i] == "different_family") {
          row_id <- paste0("row_", i)
          user_decisions[[paste0(row_id, "_decision")]] <- "reject"
          n_rejected <- n_rejected + 1
        }
      }

      # Save current page position before re-render
      if (!is.null(input$review_table_state)) {
        current_page_start(input$review_table_state$start)
      }

      # Trigger table update
      decisions_changed(decisions_changed() + 1)

      shiny::showNotification(
        sprintf(i18n()$t("%d links with different family rejected."), n_rejected),
        type = "message",
        duration = 3
      )
    })

    # Toggle individual row selection
    shiny::observeEvent(input$toggle_row, {
      shiny::req(validated_data(), input$toggle_row)

      row_idx <- input$toggle_row
      row_id <- paste0("row_", row_idx)

      # Toggle the decision
      current_decision <- user_decisions[[paste0(row_id, "_decision")]]
      if (is.null(current_decision) || current_decision == "accept") {
        user_decisions[[paste0(row_id, "_decision")]] <- "reject"
      } else {
        user_decisions[[paste0(row_id, "_decision")]] <- "accept"
      }

      # Save current page position before re-render
      if (!is.null(input$review_table_state)) {
        current_page_start(input$review_table_state$start)
      }

      # Trigger table re-render to update button
      decisions_changed(decisions_changed() + 1)
    })

    # Confirm selection
    shiny::observeEvent(input$confirm_selection, {
      shiny::req(validated_data())

      validated <- validated_data()

      # Collect user decisions
      accepted_rows <- c()
      for (i in seq_len(nrow(validated))) {
        row_id <- paste0("row_", i)
        decision <- user_decisions[[paste0(row_id, "_decision")]]
        if (!is.null(decision) && decision == "accept") {
          accepted_rows <- c(accepted_rows, i)
        }
      }

      if (length(accepted_rows) == 0) {
        shiny::showNotification(
          i18n()$t("No links selected. Please approve at least one link."),
          type = "warning",
          duration = 5
        )
        return()
      }

      # Filter to accepted links
      validated_links <- validated[accepted_rows, ]
      validated_data(validated_links)
      validation_complete(TRUE)

      shiny::showNotification(
        sprintf(i18n()$t("%d links confirmed and ready to create."), nrow(validated_links)),
        type = "message",
        duration = 5
      )
    })

    # Review table
    output$review_table_ui <- shiny::renderUI({
      shiny::req(validated_data())

      shiny::tagList(
        shiny::h4(i18n()$t("Review Links"), style = "margin-top: 30px; margin-bottom: 15px;"),
        DT::dataTableOutput(session$ns("review_table"))
      )
    })

    output$review_table <- DT::renderDataTable({
      shiny::req(validated_data())

      # Depend on decisions_changed to trigger re-render
      decisions_changed()

      validated <- validated_data()

      # Add original row index before filtering
      validated$original_row_index <- seq_len(nrow(validated))

      # Apply filters
      if (!is.null(input$filter_match) && input$filter_match != "all") {
        validated <- validated %>%
          dplyr::filter(taxonomic_match == input$filter_match)
      }

      if (!is.null(input$filter_priority) && input$filter_priority != "all") {
        validated <- validated %>%
          dplyr::filter(validation_status == input$filter_priority)
      }

      # Add selection status and action buttons based on user decisions
      display_data <- validated
      display_data$action <- sapply(seq_len(nrow(validated)), function(i) {
        original_idx <- validated$original_row_index[i]
        row_id <- paste0("row_", original_idx)
        decision <- user_decisions[[paste0(row_id, "_decision")]]

        # Create toggle button with current state
        if (is.null(decision) || decision == "accept") {
          # Currently selected - show button to reject
          sprintf('<button class="btn btn-xs btn-warning" onclick="Shiny.setInputValue(\'validator-toggle_row\', %d, {priority: \'event\'})">✗ Reject</button>',
                  original_idx)
        } else {
          # Currently rejected - show button to accept
          sprintf('<button class="btn btn-xs btn-success" onclick="Shiny.setInputValue(\'validator-toggle_row\', %d, {priority: \'event\'})">✓ Accept</button>',
                  original_idx)
        }
      })

      display_data$selection_status <- sapply(seq_len(nrow(validated)), function(i) {
        original_idx <- validated$original_row_index[i]
        row_id <- paste0("row_", original_idx)
        decision <- user_decisions[[paste0(row_id, "_decision")]]
        if (is.null(decision) || decision == "accept") {
          "✓ Selected"
        } else {
          "✗ Rejected"
        }
      })

      # Select columns to display with all taxonomic details
      # Use any_of() to handle missing columns gracefully
      display_cols <- c(
        "action",
        "selection_status",
        "id_n", "tag", "plot_name",
        "collector_name", "extracted_number",
        "individual_family", "individual_genus", "individual_species",
        "specimen_family", "specimen_genus", "specimen_species",
        "difference_indicator",
        "taxonomic_match", "validation_status",
        "link_type"
      )

      display_data <- display_data %>%
        dplyr::select(dplyr::any_of(display_cols))

      # Check if display_data is empty
      if (nrow(display_data) == 0) {
        return(DT::datatable(data.frame(Message = "No data to display")))
      }

      # Rename columns for display with translations
      colnames_translated <- colnames(display_data)
      colnames_translated[colnames_translated == "action"] <- i18n()$t("Action")
      colnames_translated[colnames_translated == "selection_status"] <- i18n()$t("Status")
      colnames_translated[colnames_translated == "id_n"] <- "ID"
      colnames_translated[colnames_translated == "tag"] <- i18n()$t("Tag")
      colnames_translated[colnames_translated == "plot_name"] <- i18n()$t("Plot")
      colnames_translated[colnames_translated == "collector_name"] <- i18n()$t("Collector")
      colnames_translated[colnames_translated == "extracted_number"] <- i18n()$t("Number")
      colnames_translated[colnames_translated == "individual_family"] <- i18n()$t("Ind. Family")
      colnames_translated[colnames_translated == "individual_genus"] <- i18n()$t("Ind. Genus")
      colnames_translated[colnames_translated == "individual_species"] <- i18n()$t("Ind. Species")
      colnames_translated[colnames_translated == "specimen_family"] <- i18n()$t("Spec. Family")
      colnames_translated[colnames_translated == "specimen_genus"] <- i18n()$t("Spec. Genus")
      colnames_translated[colnames_translated == "specimen_species"] <- i18n()$t("Spec. Species")
      colnames_translated[colnames_translated == "difference_indicator"] <- i18n()$t("Difference")
      colnames_translated[colnames_translated == "taxonomic_match"] <- i18n()$t("Match Type")
      colnames_translated[colnames_translated == "validation_status"] <- i18n()$t("Validation")
      colnames_translated[colnames_translated == "link_type"] <- i18n()$t("Link Type")

      colnames(display_data) <- colnames_translated

      DT::datatable(
        display_data,
        options = list(
          pageLength = 10,
          displayStart = current_page_start(),  # Preserve page position
          scrollX = TRUE,
          dom = 'frtip',
          columnDefs = list(
            list(width = '90px', targets = 0),   # action button
            list(width = '100px', targets = 1),  # selection_status
            list(width = '80px', targets = 2),   # id_n
            list(width = '100px', targets = c(3, 4)),  # tag, plot_name
            list(width = '100px', targets = c(7, 8, 9, 10, 11, 12)),  # taxonomy columns
            list(width = '200px', targets = 13)  # difference_indicator
          )
        ),
        rownames = FALSE,
        escape = FALSE,  # Allow HTML in action column
        class = 'cell-border stripe compact hover',
        selection = 'none'  # Disable row selection - use buttons instead
      ) %>%
        # Style selection status column (using translated name)
        DT::formatStyle(
          i18n()$t("Status"),
          backgroundColor = DT::styleEqual(
            c("✓ Selected", "✗ Rejected"),
            c("#d4edda", "#f8d7da")
          ),
          fontWeight = "bold",
          color = DT::styleEqual(
            c("✓ Selected", "✗ Rejected"),
            c("#155724", "#721c24")
          )
        ) %>%
        # Style family columns (using translated names)
        DT::formatStyle(
          c(i18n()$t("Ind. Family"), i18n()$t("Spec. Family")),
          fontWeight = "bold"
        ) %>%
        # Style taxonomic match column (using translated name)
        DT::formatStyle(
          i18n()$t("Match Type"),
          backgroundColor = DT::styleEqual(
            c("exact", "same_genus", "same_family", "different_family"),
            c("#d4edda", "#fff3cd", "#ffeaa7", "#f8d7da")
          )
        ) %>%
        # Style validation status column (using translated name)
        DT::formatStyle(
          i18n()$t("Validation"),
          backgroundColor = DT::styleEqual(
            c("auto_approve", "review_recommended", "review_required"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        )
    })

    # Return validated links and status
    return(list(
      validated_links = validated_data,
      is_complete = validation_complete
    ))
  })
}


#' Enrich with Taxonomy (Internal Helper)
#'
#' Queries taxa database to get taxonomic names for individuals and specimens.
#' Uses add_taxa_table_taxa() which handles its own database connection.
#'
#' @param links_found Data frame with individual_idtax_n and specimen_idtax_n
#' @param con_taxa Taxa database connection pool (not used, kept for compatibility)
#'
#' @return Data frame enriched with taxonomic names
#' @keywords internal
.enrich_with_taxonomy <- function(links_found, con_taxa) {

  # Note: con_taxa parameter is kept for API compatibility but not used
  # add_taxa_table_taxa() creates its own connection

  # Get unique idtax_n values
  all_idtax <- unique(c(links_found$individual_idtax_n, links_found$specimen_idtax_n))
  all_idtax <- all_idtax[!is.na(all_idtax)]

  # Ensure idtax values are integers
  all_idtax <- as.integer(all_idtax)
  all_idtax <- all_idtax[!is.na(all_idtax)]  # Remove any NAs created by coercion

  if (length(all_idtax) == 0) {
    cli::cli_alert_warning("No valid taxonomy IDs found")
    # No taxonomy IDs to enrich - return as is
    links_found$individual_genus <- NA_character_
    links_found$individual_species <- NA_character_
    links_found$individual_family <- NA_character_
    links_found$individual_taxon <- NA_character_
    links_found$specimen_genus <- NA_character_
    links_found$specimen_species <- NA_character_
    links_found$specimen_family <- NA_character_
    links_found$specimen_taxon <- NA_character_
    return(links_found)
  }

  # Query taxa info using the add_taxa_table_taxa function
  # This function queries taxa DB and returns taxonomy info
  cli::cli_alert_info("Querying taxonomy for {length(all_idtax)} unique taxa IDs")
  cli::cli_alert_info("Taxa IDs: {paste(head(all_idtax, 10), collapse=', ')}...")

  taxa_info <- tryCatch({
    # Use the existing function that handles taxa database queries
    cli::cli_alert_info("Calling add_taxa_table_taxa()...")
    result <- add_taxa_table_taxa(ids = all_idtax)

    cli::cli_alert_info("Got result, checking type: {paste(class(result), collapse=', ')}")

    # Check if result needs to be collected
    if (inherits(result, "tbl_dbi") || inherits(result, "tbl_lazy") || inherits(result, "tbl_Pool")) {
      cli::cli_alert_info("Result is lazy query, collecting...")
      result <- dplyr::collect(result)
      cli::cli_alert_info("After collect, result type: {paste(class(result), collapse=', ')}")
    }

    # Check if result is valid
    if (is.null(result)) {
      cli::cli_alert_warning("Result is NULL after collection")
      return(NULL)
    }

    # Check if result is a data frame
    if (!is.data.frame(result)) {
      cli::cli_alert_warning("Result is not a data frame, type: {paste(class(result), collapse=', ')}")
      cli::cli_alert_warning("Result structure:")
      print(str(result))
      return(NULL)
    }

    # Get row count safely
    row_count <- tryCatch(nrow(result), error = function(e) {
      cli::cli_alert_warning("Error getting nrow: {e$message}")
      return(NA)
    })

    cli::cli_alert_info("Result collected, {row_count} rows, {ncol(result)} columns")

    # Validate row count
    if (is.na(row_count) || row_count == 0) {
      cli::cli_alert_warning("Result has no rows (row_count={row_count})")
      cli::cli_alert_warning("Result columns: {paste(colnames(result), collapse=', ')}")
      return(NULL)
    }

    result
  }, error = function(e) {
    cli::cli_alert_danger("Error querying taxonomy: {conditionMessage(e)}")
    cli::cli_alert_danger("Error class: {paste(class(e), collapse=', ')}")
    cli::cli_alert_danger("Full traceback:")
    print(traceback())
    stop("Failed to query taxonomic information: ", conditionMessage(e))
  })

  if (is.null(taxa_info)) {
    cli::cli_alert_warning("Taxonomy query returned NULL")
    # Return with NA values
    links_found$individual_genus <- NA_character_
    links_found$individual_species <- NA_character_
    links_found$individual_family <- NA_character_
    links_found$individual_taxon <- NA_character_
    links_found$specimen_genus <- NA_character_
    links_found$specimen_species <- NA_character_
    links_found$specimen_family <- NA_character_
    links_found$specimen_taxon <- NA_character_
    return(links_found)
  }

  cli::cli_alert_success("Retrieved taxonomy for {nrow(taxa_info)} taxa")

  # Construct full taxonomic name from components
  # Pattern: genus + species + (rank01 + nam01) + (rank02 + nam02)
  cli::cli_alert_info("Constructing full taxonomic names...")
  taxa_info <- taxa_info %>%
    dplyr::mutate(
      full_name_no_auth = dplyr::case_when(
        # Infraspecific level with tax_nam01
        !is.na(tax_esp) & !is.na(tax_nam01) & tax_nam01 != "" ~
          paste(tax_gen, tax_esp, tax_rank01, tax_nam01),
        # Species level
        !is.na(tax_esp) & tax_esp != "" ~
          paste(tax_gen, tax_esp),
        # Genus level only
        !is.na(tax_gen) & tax_gen != "" ~
          tax_gen,
        # Fallback
        TRUE ~ NA_character_
      )
    )

  cli::cli_alert_info("Full names constructed for {sum(!is.na(taxa_info$full_name_no_auth))} taxa")

  # Create individual taxonomy lookup
  cli::cli_alert_info("Creating individual taxonomy lookup...")
  taxa_individual <- taxa_info %>%
    dplyr::select(idtax_n, tax_gen, tax_esp, tax_fam, full_name_no_auth) %>%
    dplyr::rename(
      individual_genus = tax_gen,
      individual_species = tax_esp,
      individual_family = tax_fam,
      individual_taxon = full_name_no_auth
    )

  cli::cli_alert_info("Individual lookup: {nrow(taxa_individual)} rows")

  # Create specimen taxonomy lookup
  cli::cli_alert_info("Creating specimen taxonomy lookup...")
  taxa_specimen <- taxa_info %>%
    dplyr::select(idtax_n, tax_gen, tax_esp, tax_fam, full_name_no_auth) %>%
    dplyr::rename(
      specimen_genus = tax_gen,
      specimen_species = tax_esp,
      specimen_family = tax_fam,
      specimen_taxon = full_name_no_auth
    )

  cli::cli_alert_info("Specimen lookup: {nrow(taxa_specimen)} rows")

  # Join with links for individual taxonomy
  cli::cli_alert_info("Joining individual taxonomy ({nrow(links_found)} links)...")
  links_enriched <- tryCatch({
    links_found %>%
      dplyr::left_join(
        taxa_individual,
        by = c("individual_idtax_n" = "idtax_n")
      )
  }, error = function(e) {
    cli::cli_alert_danger("Error in individual join: {conditionMessage(e)}")
    stop(e)
  })

  cli::cli_alert_success("Individual join complete: {nrow(links_enriched)} rows")

  # Join with links for specimen taxonomy
  cli::cli_alert_info("Joining specimen taxonomy...")
  links_enriched <- tryCatch({
    links_enriched %>%
      dplyr::left_join(
        taxa_specimen,
        by = c("specimen_idtax_n" = "idtax_n")
      )
  }, error = function(e) {
    cli::cli_alert_danger("Error in specimen join: {conditionMessage(e)}")
    stop(e)
  })

  cli::cli_alert_success("Specimen join complete: {nrow(links_enriched)} rows")
  cli::cli_alert_success("Enrichment complete with {ncol(links_enriched)} columns")

  return(links_enriched)
}


#' Validate Taxonomic Matches (Internal Helper)
#'
#' Compares individual and specimen taxonomy and assigns validation status.
#' Creates detailed difference indicators with priority on family-level differences.
#'
#' @param links_with_taxonomy Data frame with taxonomic information
#'
#' @return Data frame with taxonomic_match, validation_status, and difference_indicator columns
#' @keywords internal
.validate_taxonomic_matches <- function(links_with_taxonomy) {

  tryCatch({
    cli::cli_alert_info("  Checking for duplicate type_individual links...")

    # Step 0: Check for duplicate type_individual links per specimen
    # A specimen can only be collected from ONE tree
    duplicate_type_specimens <- links_with_taxonomy %>%
      dplyr::filter(link_type == "type_individual") %>%
      dplyr::group_by(id_specimen) %>%
      dplyr::summarise(n_type_links = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n_type_links > 1) %>%
      dplyr::pull(id_specimen)

    if (length(duplicate_type_specimens) > 0) {
      cli::cli_alert_warning("Found {length(duplicate_type_specimens)} specimens with duplicate type_individual links!")
      cli::cli_alert_warning("Specimen IDs: {paste(duplicate_type_specimens, collapse=', ')}")
    }

    # Add duplicate flag
    links_step0 <- links_with_taxonomy %>%
      dplyr::mutate(
        has_duplicate_type = id_specimen %in% duplicate_type_specimens & link_type == "type_individual"
      )

    cli::cli_alert_info("  Checking family matches...")

    # Step 1: Check family match
    links_step1 <- links_step0 %>%
      dplyr::mutate(
        family_match = dplyr::case_when(
          is.na(individual_family) | is.na(specimen_family) ~ NA_character_,
          individual_family == specimen_family ~ "match",
          TRUE ~ "differ"
        )
      )

    cli::cli_alert_info("  Checking genus matches...")

    # Step 2: Check genus match
    links_step2 <- links_step1 %>%
      dplyr::mutate(
        genus_match = dplyr::case_when(
          is.na(individual_genus) | is.na(specimen_genus) ~ NA_character_,
          individual_genus == specimen_genus ~ "match",
          TRUE ~ "differ"
        )
      )

    cli::cli_alert_info("  Checking species matches...")

    # Step 3: Check species match
    links_step3 <- links_step2 %>%
      dplyr::mutate(
        species_match = dplyr::case_when(
          is.na(individual_species) | is.na(specimen_species) ~ NA_character_,
          individual_species == specimen_species ~ "match",
          TRUE ~ "differ"
        )
      )

    cli::cli_alert_info("  Classifying taxonomic matches...")

    # Step 4: Overall classification
    links_step4 <- links_step3 %>%
      dplyr::mutate(
        taxonomic_match = dplyr::case_when(
          # Exact taxon ID match
          !is.na(individual_idtax_n) & !is.na(specimen_idtax_n) &
            individual_idtax_n == specimen_idtax_n ~ "exact",
          # Genus AND species both match (even if idtax_n differs - synonyms)
          !is.na(genus_match) & genus_match == "match" &
            !is.na(species_match) & species_match == "match" ~ "exact",
          # Genus matches but species differs or is unknown
          !is.na(genus_match) & genus_match == "match" ~ "same_genus",
          # Family matches but genus differs or is unknown
          !is.na(family_match) & family_match == "match" ~ "same_family",
          # Everything else (family differs or all NA)
          TRUE ~ "different_family"
        )
      )

    cli::cli_alert_info("  Creating difference indicators...")

    # Step 5: Difference indicators
    links_step5 <- links_step4 %>%
      dplyr::mutate(
        difference_indicator = dplyr::case_when(
          # CRITICAL: Duplicate type_individual links
          has_duplicate_type ~ paste0("🚨 ERROR: Duplicate type_individual link for specimen ID ", id_specimen),
          # Normal taxonomy checks
          taxonomic_match == "exact" ~ "✓ Exact match",
          !is.na(family_match) & family_match == "differ" ~
            paste0("⚠ FAMILY DIFFERS: ",
                   dplyr::coalesce(individual_family, "?"), " ≠ ",
                   dplyr::coalesce(specimen_family, "?")),
          !is.na(genus_match) & genus_match == "differ" ~
            paste0("⚠ Genus differs: ",
                   dplyr::coalesce(individual_genus, "?"), " ≠ ",
                   dplyr::coalesce(specimen_genus, "?")),
          !is.na(species_match) & species_match == "differ" ~
            paste0("• Species differs: ",
                   dplyr::coalesce(individual_species, "?"), " ≠ ",
                   dplyr::coalesce(specimen_species, "?")),
          TRUE ~ "? Unknown"
        )
      )

    cli::cli_alert_info("  Setting validation status...")

    # Step 6: Validation status
    links_validated <- links_step5 %>%
      dplyr::mutate(
        validation_status = dplyr::case_when(
          # CRITICAL: Duplicate type links must be reviewed and rejected
          has_duplicate_type ~ "review_required",
          # Normal validation status
          taxonomic_match == "exact" ~ "auto_approve",
          taxonomic_match == "same_genus" ~ "review_recommended",
          taxonomic_match == "same_family" ~ "review_required",
          taxonomic_match == "different_family" ~ "review_required",
          TRUE ~ "review_required"
        )
      )

    cli::cli_alert_success("  All validation steps complete")

    return(links_validated)

  }, error = function(e) {
    cli::cli_alert_danger("Error in .validate_taxonomic_matches: {conditionMessage(e)}")
    cli::cli_alert_danger("Error occurred at: {deparse(sys.calls()[[sys.nframe()-1]])}")
    stop(e)
  })
}
