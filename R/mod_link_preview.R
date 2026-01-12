# Link Preview Module
#
# Preview and validate specimen-individual links before adding to database
# Includes taxonomic validation to flag mismatched identifications
#
# Part of the specimen linking Shiny app system

#' Link Preview Module - UI
#'
#' @param id Character, module namespace ID
#' @param i18n shiny.i18n translator object
#'
#' @return Shiny UI element
#' @export
mod_link_preview_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      style = "margin-bottom: 15px;",
      shiny::uiOutput(ns("links_summary"))
    ),
    shiny::div(
      class = "alert alert-warning",
      style = "display: none;",
      id = ns("taxonomy_warning"),
      shiny::icon("exclamation-triangle"),
      " ",
      shiny::span(
        id = ns("taxonomy_warning_text"),
        i18n$t("Some links have mismatched taxonomic identifications - review required")
      )
    ),
    DT::dataTableOutput(ns("links_table")),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::selectInput(
          inputId = ns("default_linktype"),
          label = i18n$t("Default Link Type"),
          choices = c(
            "type_individual" = 1,
            "referenced_individual" = 2
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::div(
          style = "margin-top: 25px;",
          shiny::actionButton(
            inputId = ns("btn_apply_linktype"),
            label = i18n$t("Apply to all selected"),
            icon = shiny::icon("check"),
            class = "btn-info"
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::div(
          style = "margin-top: 25px;",
          shiny::checkboxInput(
            inputId = ns("show_only_mismatched"),
            label = i18n$t("Show only mismatched taxa"),
            value = FALSE
          )
        )
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::actionButton(
          inputId = ns("btn_remove_selected"),
          label = i18n$t("Remove selected"),
          icon = shiny::icon("trash"),
          class = "btn-danger"
        )
      ),
      shiny::column(
        width = 3,
        shiny::actionButton(
          inputId = ns("btn_clear_all"),
          label = i18n$t("Clear all"),
          icon = shiny::icon("times"),
          class = "btn-warning"
        )
      ),
      shiny::column(
        width = 3,
        shiny::actionButton(
          inputId = ns("btn_validate"),
          label = i18n$t("Validate links"),
          icon = shiny::icon("check-circle"),
          class = "btn-primary"
        )
      ),
      shiny::column(
        width = 3,
        shiny::actionButton(
          inputId = ns("btn_execute"),
          label = i18n$t("Create links"),
          icon = shiny::icon("database"),
          class = "btn-success",
          disabled = TRUE
        )
      )
    ),
    shiny::hr(),
    shiny::uiOutput(ns("validation_results"))
  )
}


#' Link Preview Module - Server
#'
#' @param id Character, module namespace ID
#' @param pool_main Reactive returning database connection pool
#' @param selected_specimens Reactive returning selected specimens data
#' @param selected_individuals Reactive returning selected individuals data
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return Reactive with link creation results
#' @export
mod_link_preview_server <- function(id, pool_main, selected_specimens,
                                    selected_individuals, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    pending_links <- shiny::reactiveVal(data.frame())
    validation_status <- shiny::reactiveVal(NULL)
    linktypes <- shiny::reactiveVal(NULL)
    execution_result <- shiny::reactiveVal(NULL)

    # Load link types from database
    shiny::observe({
      shiny::req(pool_main())

      tryCatch({
        types <- get_linktypes(pool_main())
        linktypes(types)

        # Update dropdown with database values
        choices <- stats::setNames(types$id_linktype, types$linktype)
        shiny::updateSelectInput(
          session,
          "default_linktype",
          choices = choices,
          selected = types$id_linktype[which.max(types$priority)]
        )
      }, error = function(e) {
        cli::cli_alert_warning("Could not load link types: {e$message}")
      })
    })

    # Build links when specimens are selected
    shiny::observeEvent(selected_specimens(), {
      specimens <- selected_specimens()
      shiny::req(specimens)
      shiny::req(nrow(specimens) > 0)

      individuals <- selected_individuals()

      current <- pending_links()

      # If we have both specimens and individuals, create pairings
      if (!is.null(individuals) && nrow(individuals) > 0) {
        # Create all combinations
        new_links <- expand.grid(
          specimen_idx = seq_len(nrow(specimens)),
          individual_idx = seq_len(nrow(individuals))
        ) %>%
          dplyr::mutate(
            id_specimen = specimens$id_specimen[specimen_idx],
            specimen_label = specimens$specimen_label[specimen_idx],
            specimen_taxon = specimens$full_name_no_auth[specimen_idx],
            specimen_genus = specimens$tax_gen[specimen_idx],
            id_n = individuals$id_n[individual_idx],
            individual_label = individuals$individual_label[individual_idx],
            individual_taxon = individuals$full_name_no_auth[individual_idx],
            individual_genus = individuals$tax_gen[individual_idx],
            id_linktype = as.integer(input$default_linktype)
          ) %>%
          # Calculate taxonomy match status
          dplyr::mutate(
            taxo_status = dplyr::case_when(
              is.na(specimen_taxon) | is.na(individual_taxon) ~ "unknown",
              specimen_taxon == individual_taxon ~ "match",
              specimen_genus == individual_genus ~ "same_genus",
              TRUE ~ "mismatch"
            )
          ) %>%
          dplyr::select(-specimen_idx, -individual_idx)

        # Add to existing links (avoid duplicates)
        if (nrow(current) > 0) {
          new_links <- new_links %>%
            dplyr::anti_join(
              current,
              by = c("id_specimen", "id_n")
            )
        }

        pending_links(dplyr::bind_rows(current, new_links))

      } else {
        # Just add specimens without individual pairing
        new_links <- specimens %>%
          dplyr::mutate(
            specimen_label = specimen_label,
            specimen_taxon = full_name_no_auth,
            specimen_genus = tax_gen,
            id_n = NA_integer_,
            individual_label = NA_character_,
            individual_taxon = NA_character_,
            individual_genus = NA_character_,
            id_linktype = as.integer(input$default_linktype),
            taxo_status = "pending"
          ) %>%
          dplyr::select(
            id_specimen, specimen_label, specimen_taxon, specimen_genus,
            id_n, individual_label, individual_taxon, individual_genus,
            id_linktype, taxo_status
          )

        # Add to existing links
        if (nrow(current) > 0) {
          new_links <- new_links %>%
            dplyr::anti_join(
              current,
              by = "id_specimen"
            )
        }

        pending_links(dplyr::bind_rows(current, new_links))
      }

      # Show taxonomy warning if mismatches exist
      shinyjs::runjs(sprintf(
        "if (%s) { $('#%s').show(); } else { $('#%s').hide(); }",
        tolower(any(pending_links()$taxo_status == "mismatch")),
        session$ns("taxonomy_warning"),
        session$ns("taxonomy_warning")
      ))
    })

    # Build links when individuals are selected
    shiny::observeEvent(selected_individuals(), {
      individuals <- selected_individuals()
      shiny::req(individuals)
      shiny::req(nrow(individuals) > 0)

      specimens <- selected_specimens()
      current <- pending_links()

      # If we have both, create pairings
      if (!is.null(specimens) && nrow(specimens) > 0) {
        # Create all combinations
        new_links <- expand.grid(
          specimen_idx = seq_len(nrow(specimens)),
          individual_idx = seq_len(nrow(individuals))
        ) %>%
          dplyr::mutate(
            id_specimen = specimens$id_specimen[specimen_idx],
            specimen_label = specimens$specimen_label[specimen_idx],
            specimen_taxon = specimens$full_name_no_auth[specimen_idx],
            specimen_genus = specimens$tax_gen[specimen_idx],
            id_n = individuals$id_n[individual_idx],
            individual_label = individuals$individual_label[individual_idx],
            individual_taxon = individuals$full_name_no_auth[individual_idx],
            individual_genus = individuals$tax_gen[individual_idx],
            id_linktype = as.integer(input$default_linktype)
          ) %>%
          # Calculate taxonomy match status
          dplyr::mutate(
            taxo_status = dplyr::case_when(
              is.na(specimen_taxon) | is.na(individual_taxon) ~ "unknown",
              specimen_taxon == individual_taxon ~ "match",
              specimen_genus == individual_genus ~ "same_genus",
              TRUE ~ "mismatch"
            )
          ) %>%
          dplyr::select(-specimen_idx, -individual_idx)

        # Add to existing links (avoid duplicates)
        if (nrow(current) > 0) {
          new_links <- new_links %>%
            dplyr::anti_join(
              current,
              by = c("id_specimen", "id_n")
            )
        }

        pending_links(dplyr::bind_rows(current, new_links))

      } else {
        # Just add individuals without specimen pairing
        new_links <- individuals %>%
          dplyr::mutate(
            id_specimen = NA_integer_,
            specimen_label = NA_character_,
            specimen_taxon = NA_character_,
            specimen_genus = NA_character_,
            individual_label = individual_label,
            individual_taxon = full_name_no_auth,
            individual_genus = tax_gen,
            id_linktype = as.integer(input$default_linktype),
            taxo_status = "pending"
          ) %>%
          dplyr::select(
            id_specimen, specimen_label, specimen_taxon, specimen_genus,
            id_n, individual_label, individual_taxon, individual_genus,
            id_linktype, taxo_status
          )

        # Add to existing links
        if (nrow(current) > 0) {
          new_links <- new_links %>%
            dplyr::anti_join(
              current,
              by = "id_n"
            )
        }

        pending_links(dplyr::bind_rows(current, new_links))
      }

      # Show taxonomy warning if mismatches exist
      shinyjs::runjs(sprintf(
        "if (%s) { $('#%s').show(); } else { $('#%s').hide(); }",
        tolower(any(pending_links()$taxo_status == "mismatch")),
        session$ns("taxonomy_warning"),
        session$ns("taxonomy_warning")
      ))
    })

    # Links summary
    output$links_summary <- shiny::renderUI({
      links <- pending_links()

      if (is.null(links) || nrow(links) == 0) {
        return(shiny::div(
          class = "alert alert-secondary",
          i18n()$t("No links pending. Select specimens and individuals to create links.")
        ))
      }

      complete_links <- sum(!is.na(links$id_specimen) & !is.na(links$id_n))
      mismatched <- sum(links$taxo_status == "mismatch", na.rm = TRUE)
      same_genus <- sum(links$taxo_status == "same_genus", na.rm = TRUE)
      exact_match <- sum(links$taxo_status == "match", na.rm = TRUE)

      shiny::div(
        class = "alert alert-info",
        shiny::strong(nrow(links)), " ", i18n()$t("links pending"),
        shiny::br(),
        shiny::tags$small(
          shiny::icon("check-circle", style = "color: green;"),
          " ", exact_match, " ", i18n()$t("exact taxon match"),
          shiny::br(),
          shiny::icon("info-circle", style = "color: blue;"),
          " ", same_genus, " ", i18n()$t("same genus"),
          shiny::br(),
          shiny::icon("exclamation-triangle", style = "color: orange;"),
          " ", mismatched, " ", i18n()$t("different genus - review needed")
        )
      )
    })

    # Links table
    output$links_table <- DT::renderDataTable({
      links <- pending_links()
      shiny::req(nrow(links) > 0)

      # Filter if showing only mismatched
      if (input$show_only_mismatched) {
        links <- links %>%
          dplyr::filter(taxo_status == "mismatch")
      }

      # Get linktype labels
      types <- linktypes()
      if (!is.null(types)) {
        links <- links %>%
          dplyr::left_join(
            types %>% dplyr::select(id_linktype, linktype),
            by = "id_linktype"
          )
      } else {
        links$linktype <- links$id_linktype
      }

      # Create status display with colors
      links <- links %>%
        dplyr::mutate(
          taxo_display = dplyr::case_when(
            taxo_status == "match" ~ '<span style="color: green;"><i class="fa fa-check-circle"></i> Match</span>',
            taxo_status == "same_genus" ~ '<span style="color: blue;"><i class="fa fa-info-circle"></i> Same genus</span>',
            taxo_status == "mismatch" ~ '<span style="color: orange;"><i class="fa fa-exclamation-triangle"></i> Mismatch</span>',
            TRUE ~ '<span style="color: gray;"><i class="fa fa-question-circle"></i> Unknown</span>'
          )
        )

      display_data <- links %>%
        dplyr::select(
          specimen_label, specimen_taxon,
          individual_label, individual_taxon,
          taxo_display, linktype
        ) %>%
        dplyr::rename(
          Specimen = specimen_label,
          `Specimen Taxon` = specimen_taxon,
          Individual = individual_label,
          `Individual Taxon` = individual_taxon,
          `Taxon Status` = taxo_display,
          `Link Type` = linktype
        )

      DT::datatable(
        display_data,
        selection = "multiple",
        escape = FALSE,  # Allow HTML in taxo_display
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(
            search = i18n()$t("Search:"),
            lengthMenu = paste(i18n()$t("Show"), "_MENU_", i18n()$t("entries"))
          )
        ),
        rownames = FALSE
      )
    })

    # Apply link type to selected rows
    shiny::observeEvent(input$btn_apply_linktype, {
      selected_rows <- input$links_table_rows_selected
      shiny::req(length(selected_rows) > 0)

      links <- pending_links()
      links$id_linktype[selected_rows] <- as.integer(input$default_linktype)
      pending_links(links)

      shiny::showNotification(
        paste(i18n()$t("Link type updated for"), length(selected_rows), i18n()$t("links")),
        type = "message"
      )
    })

    # Remove selected links
    shiny::observeEvent(input$btn_remove_selected, {
      selected_rows <- input$links_table_rows_selected
      shiny::req(length(selected_rows) > 0)

      links <- pending_links()
      links <- links[-selected_rows, ]
      pending_links(links)

      shiny::showNotification(
        paste(length(selected_rows), i18n()$t("links removed")),
        type = "message"
      )
    })

    # Clear all links
    shiny::observeEvent(input$btn_clear_all, {
      pending_links(data.frame())
      validation_status(NULL)
      shinyjs::disable("btn_execute")

      shiny::showNotification(
        i18n()$t("All links cleared"),
        type = "message"
      )
    })

    # Validate links
    shiny::observeEvent(input$btn_validate, {
      links <- pending_links()
      shiny::req(nrow(links) > 0)
      shiny::req(pool_main())

      # Check for incomplete links
      incomplete <- links %>%
        dplyr::filter(is.na(id_specimen) | is.na(id_n))

      if (nrow(incomplete) > 0) {
        validation_status(list(
          valid = FALSE,
          message = paste(
            i18n()$t("Cannot validate:"),
            nrow(incomplete),
            i18n()$t("links are missing specimen or individual")
          )
        ))
        return()
      }

      # Check for duplicates with database
      con <- pool_main()

      existing <- tryCatch({
        query_all_specimen_links(
          id_specimen = unique(links$id_specimen),
          include_specimen_info = FALSE,
          include_linktype_info = TRUE,
          con = con
        )
      }, error = function(e) {
        data.frame()
      })

      if (nrow(existing) > 0) {
        duplicates <- links %>%
          dplyr::inner_join(
            existing %>% dplyr::select(id_specimen, id_n),
            by = c("id_specimen", "id_n")
          )

        if (nrow(duplicates) > 0) {
          validation_status(list(
            valid = FALSE,
            message = paste(
              nrow(duplicates),
              i18n()$t("links already exist in database")
            ),
            duplicates = duplicates
          ))
          return()
        }
      }

      # Check for taxonomy mismatches that need confirmation
      mismatches <- links %>%
        dplyr::filter(taxo_status == "mismatch")

      if (nrow(mismatches) > 0) {
        validation_status(list(
          valid = TRUE,
          warning = TRUE,
          message = paste(
            i18n()$t("Validation passed with warnings:"),
            nrow(mismatches),
            i18n()$t("links have different genus identification")
          ),
          mismatches = mismatches
        ))
        shinyjs::enable("btn_execute")
        return()
      }

      # All valid
      validation_status(list(
        valid = TRUE,
        warning = FALSE,
        message = paste(
          i18n()$t("Validation passed:"),
          nrow(links),
          i18n()$t("links ready to create")
        )
      ))
      shinyjs::enable("btn_execute")
    })

    # Validation results display
    output$validation_results <- shiny::renderUI({
      status <- validation_status()

      if (is.null(status)) {
        return(NULL)
      }

      if (!status$valid) {
        return(shiny::div(
          class = "alert alert-danger",
          shiny::icon("times-circle"),
          " ",
          status$message
        ))
      }

      if (status$warning) {
        return(shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"),
          " ",
          status$message,
          shiny::br(),
          shiny::tags$small(
            i18n()$t("Review the mismatched identifications before proceeding.")
          )
        ))
      }

      shiny::div(
        class = "alert alert-success",
        shiny::icon("check-circle"),
        " ",
        status$message
      )
    })

    # Execute link creation
    shiny::observeEvent(input$btn_execute, {
      links <- pending_links()
      shiny::req(nrow(links) > 0)
      shiny::req(pool_main())

      # Prepare data for insertion
      data_to_add <- links %>%
        dplyr::select(id_specimen, id_n, id_linktype) %>%
        dplyr::filter(!is.na(id_specimen), !is.na(id_n))

      if (nrow(data_to_add) == 0) {
        shiny::showNotification(
          i18n()$t("No valid links to create"),
          type = "error"
        )
        return()
      }

      # Call the add function
      tryCatch({
        result <- .add_link_specimens(
          new_data = data_to_add,
          col_names_corresp = c("id_specimen", "id_n", "id_linktype"),
          launch_adding_data = TRUE,
          validate = TRUE,
          con = pool_main()
        )

        execution_result(list(
          success = TRUE,
          n_added = nrow(data_to_add),
          message = paste(nrow(data_to_add), i18n()$t("links created successfully"))
        ))

        # Clear pending links
        pending_links(data.frame())
        validation_status(NULL)
        shinyjs::disable("btn_execute")

        shiny::showNotification(
          paste(nrow(data_to_add), i18n()$t("links created successfully")),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        execution_result(list(
          success = FALSE,
          message = paste(i18n()$t("Error creating links:"), e$message)
        ))

        shiny::showNotification(
          paste(i18n()$t("Error creating links:"), e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Return execution result
    return(execution_result)
  })
}
