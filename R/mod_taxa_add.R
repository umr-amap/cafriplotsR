#' Taxa Add Module - UI
#'
#' UI component for adding new taxonomic entries
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_taxa_add_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("add_ui"))
  )
}

#' Taxa Add Module - Server
#'
#' Server logic for adding new taxonomic entries
#'
#' @param id Module namespace ID
#' @param pool Reactive returning taxa database connection pool
#' @param has_write_permission Reactive returning TRUE if user can write
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL
#'
#' @keywords internal
#' @export
mod_taxa_add_server <- function(id, pool, has_write_permission, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      tropicos_results = NULL,
      current_step = 1,
      form_data = list(),
      new_taxon_id = NULL,
      existing_check = NULL,
      growth_form_data = NULL,
      order_required = FALSE,
      class_required = FALSE,
      existing_taxon_matches = NULL,  # For multiple matches when setting existing as synonym
      accepted_taxon_matches = NULL   # For multiple matches when setting new as synonym
    )

    # Initialize growth form selector module
    growth_form_module <- mod_growth_form_selector_server(
      "growth_form",
      pool = pool,
      i18n = i18n
    )

    # Main UI
    output$add_ui <- shiny::renderUI({
      if (!has_write_permission()) {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("lock"),
            " ",
            i18n()$t("Write Access:"),
            " ",
            i18n()$t("Only users with INSERT privileges can modify data")
          )
        )
      }

      shiny::tagList(
        shiny::h4(i18n()$t("Add New Taxon")),

        # Step indicator
        shiny::div(
          class = "alert alert-info",
          style = "display: flex; justify-content: space-around;",
          shiny::div(
            style = if (rv$current_step == 1) "font-weight: bold; color: #007bff;" else "",
            "1. ", i18n()$t("Tropicos Search")
          ),
          shiny::div(
            style = if (rv$current_step == 2) "font-weight: bold; color: #007bff;" else "",
            "2. ", i18n()$t("Taxonomic Info")
          ),
          shiny::div(
            style = if (rv$current_step == 3) "font-weight: bold; color: #007bff;" else "",
            "3. ", i18n()$t("Growth Form")
          ),
          shiny::div(
            style = if (rv$current_step == 4) "font-weight: bold; color: #007bff;" else "",
            "4. ", i18n()$t("Review & Submit")
          ),
          if (rv$current_step == 5) {
            shiny::div(
              style = "font-weight: bold; color: #007bff;",
              "5. ", i18n()$t("Set Synonymy (Optional)")
            )
          }
        ),

        shiny::hr(),

        # Step panels
        shiny::uiOutput(ns("step_panel"))
      )
    })

    # Step panels
    output$step_panel <- shiny::renderUI({
      switch(
        as.character(rv$current_step),
        "1" = step1_tropicos_ui(ns, i18n),
        "2" = step2_taxonomy_ui(ns, i18n, rv),
        "3" = step3_growth_ui(ns, i18n, rv),
        "4" = step4_review_ui(ns, i18n, rv),
        "5" = step5_synonymy_ui(ns, i18n, rv)
      )
    })

    # Step 1: Tropicos Search
    step1_tropicos_ui <- function(ns, i18n) {
      shiny::tagList(
        shiny::h5(i18n()$t("Step 1: Search Tropicos Database (Optional)")),
        shiny::p(i18n()$t("Search Tropicos to auto-fill taxonomic information, or skip to manual entry")),

        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(
              8,
              shiny::textInput(
                ns("tropicos_search"),
                i18n()$t("Scientific name to search"),
                placeholder = "e.g., Gilbertiodendron dewevrei"
              )
            ),
            shiny::column(
              4,
              shiny::br(),
              shiny::actionButton(
                ns("btn_search_tropicos"),
                i18n()$t("Search Tropicos"),
                icon = shiny::icon("search"),
                class = "btn-primary btn-block"
              )
            )
          ),
          shiny::uiOutput(ns("tropicos_results_ui"))
        ),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_skip_tropicos"),
              i18n()$t("Skip - Manual Entry"),
              icon = shiny::icon("edit"),
              class = "btn-secondary btn-block"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_next_step1"),
              i18n()$t("Next: Taxonomic Info"),
              icon = shiny::icon("arrow-right"),
              class = "btn-success btn-block"
            )
          )
        )
      )
    }

    # Search Tropicos
    shiny::observeEvent(input$btn_search_tropicos, {
      shiny::req(input$tropicos_search)

      search_name <- trimws(input$tropicos_search)

      if (nchar(search_name) == 0) {
        shiny::showNotification(
          i18n()$t("Please enter a name to search"),
          type = "warning"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          tps_key <- Sys.getenv("TROPICOS_API_KEY", "15ad0b4c-f0d3-46ab-b649-178f2c75724f")
          cli::cli_alert_info("Searching Tropicos for: {search_name}")
          results <- taxize::tp_search(sci = search_name, key = tps_key)

          if (ncol(results) == 1) {
            shiny::showNotification(
              i18n()$t("No results found on Tropicos"),
              type = "warning",
              duration = 5
            )
            rv$tropicos_results <- NULL
          } else {
            rv$tropicos_results <- results
            shiny::showNotification(
              sprintf(i18n()$t("Found %d results"), nrow(results)),
              type = "message",
              duration = 3
            )
          }
        }, error = function(e) {
          cli::cli_alert_danger("Tropicos search failed: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Search error:"), e$message),
            type = "error",
            duration = 10
          )
          rv$tropicos_results <- NULL
        })
      }, message = i18n()$t("Searching Tropicos..."))
    })

    # Display Tropicos results
    output$tropicos_results_ui <- shiny::renderUI({
      if (is.null(rv$tropicos_results)) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("No Tropicos results yet - enter a name and click Search")
          )
        )
      }

      shiny::tagList(
        shiny::h6(i18n()$t("Tropicos Results")),
        DT::DTOutput(ns("tropicos_table")),
        shiny::br(),
        shiny::actionButton(
          ns("btn_use_selected"),
          i18n()$t("Use Selected Result"),
          icon = shiny::icon("check"),
          class = "btn-success"
        )
      )
    })

    # Tropicos results table
    output$tropicos_table <- DT::renderDT({
      shiny::req(rv$tropicos_results)
      DT::datatable(
        rv$tropicos_results,
        selection = list(mode = "single"),
        options = list(pageLength = 5, scrollX = TRUE, dom = "tp"),
        rownames = FALSE
      )
    })

    # Use selected Tropicos result
    shiny::observeEvent(input$btn_use_selected, {
      shiny::req(rv$tropicos_results)
      selected <- input$tropicos_table_rows_selected

      if (length(selected) == 0) {
        shiny::showNotification(
          i18n()$t("Please select a result from the table"),
          type = "warning"
        )
        return()
      }

      result <- rv$tropicos_results[selected, ]
      rv$form_data$tax_tax <- result$scientificnamewithauthors
      rv$form_data$tax_gen <- strsplit(result$scientificname, " ")[[1]][1]
      rv$form_data$tax_esp <- strsplit(result$scientificname, " ")[[1]][2]
      rv$form_data$tax_fam <- result$family
      rv$form_data$author1 <- result$author
      rv$form_data$year_description <- as.numeric(result$displaydate)

      rank <- result$rankabbreviation
      if (!is.na(rank) && rank != "sp.") {
        rv$form_data$tax_rank1 <- rank
        rv$form_data$tax_name1 <- strsplit(result$scientificname, " ")[[1]][4]
      }

      shiny::showNotification(
        i18n()$t("Tropicos data loaded - proceed to next step"),
        type = "message"
      )
    })

    # Skip Tropicos
    shiny::observeEvent(input$btn_skip_tropicos, {
      rv$current_step <- 2
    })

    # Next from Step 1
    shiny::observeEvent(input$btn_next_step1, {
      rv$current_step <- 2
    })

    # Step 2: Taxonomic Information + Existence Check
    step2_taxonomy_ui <- function(ns, i18n, rv) {
      shiny::tagList(
        shiny::h5(i18n()$t("Step 2: Enter Taxonomic Information")),

        # Show existence check results if any
        shiny::uiOutput(ns("existence_check_ui")),

        shiny::wellPanel(
          shiny::h6(i18n()$t("Required Fields")),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_gen"),
                paste(i18n()$t("Genus"), "*"),
                value = if (is.null(rv$form_data$tax_gen)) "" else rv$form_data$tax_gen
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_esp"),
                i18n()$t("Species epithet"),
                value = if (is.null(rv$form_data$tax_esp)) "" else rv$form_data$tax_esp
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_fam"),
                paste(i18n()$t("Family"), "*"),
                value = if (is.null(rv$form_data$tax_fam)) "" else rv$form_data$tax_fam
              )
            )
          ),

          shiny::h6(i18n()$t("Higher Taxonomy (auto-filled if possible)")),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_order"),
                shiny::textOutput(ns("order_label"), inline = TRUE),
                value = if (is.null(rv$form_data$tax_order)) "" else rv$form_data$tax_order
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_famclass"),
                shiny::textOutput(ns("class_label"), inline = TRUE),
                value = if (is.null(rv$form_data$tax_famclass)) "" else rv$form_data$tax_famclass
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_tax"),
                i18n()$t("Full name with authors"),
                value = if (is.null(rv$form_data$tax_tax)) "" else rv$form_data$tax_tax
              )
            )
          ),

          shiny::h6(i18n()$t("Infraspecific (if applicable)")),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::selectInput(
                ns("tax_rank1"),
                i18n()$t("Infraspecific rank"),
                choices = c("None" = "", "var." = "var.", "subsp." = "subsp.", "f." = "f."),
                selected = if (is.null(rv$form_data$tax_rank1)) "" else rv$form_data$tax_rank1
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_name1"),
                i18n()$t("Infraspecific name"),
                value = if (is.null(rv$form_data$tax_name1)) "" else rv$form_data$tax_name1
              )
            )
          ),

          shiny::h6(i18n()$t("Authors & Year")),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::textInput(
                ns("author1"),
                i18n()$t("Author 1"),
                value = if (is.null(rv$form_data$author1)) "" else rv$form_data$author1
              )
            ),
            shiny::column(
              3,
              shiny::textInput(
                ns("author2"),
                i18n()$t("Author 2"),
                value = if (is.null(rv$form_data$author2)) "" else rv$form_data$author2
              )
            ),
            shiny::column(
              3,
              shiny::textInput(
                ns("author3"),
                i18n()$t("Author 3"),
                value = if (is.null(rv$form_data$author3)) "" else rv$form_data$author3
              )
            ),
            shiny::column(
              3,
              shiny::numericInput(
                ns("year_description"),
                i18n()$t("Year"),
                value = rv$form_data$year_description,
                min = 1700,
                max = as.numeric(format(Sys.Date(), "%Y"))
              )
            )
          ),

          shiny::checkboxInput(
            ns("morpho_species"),
            i18n()$t("This is a morphotaxon"),
            value = FALSE
          )
        ),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_back_step2"),
              i18n()$t("Back"),
              icon = shiny::icon("arrow-left"),
              class = "btn-secondary btn-block"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_next_step2"),
              i18n()$t("Next: Growth Form"),
              icon = shiny::icon("arrow-right"),
              class = "btn-success btn-block"
            )
          )
        )
      )
    }

    # Check if taxon exists in database
    output$existence_check_ui <- shiny::renderUI({
      if (is.null(rv$existing_check)) {
        return(NULL)
      }

      check <- rv$existing_check

      if (check$exists) {
        if (check$is_synonym) {
          # Exists as synonym
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            " ",
            shiny::strong(i18n()$t("Taxon already exists as SYNONYM")),
            shiny::br(),
            sprintf(i18n()$t("ID: %s, Synonym of ID: %s"), check$idtax_n, check$idtax_good_n),
            shiny::br(),
            shiny::p(i18n()$t("This taxon is already in the database but marked as a synonym.")),
            shiny::actionButton(
              ns("btn_cancel_existing_synonymy"),
              i18n()$t("Cancel its synonymy to make it an accepted name"),
              icon = shiny::icon("unlink"),
              class = "btn-warning"
            )
          )
        } else {
          # Exists as accepted name
          shiny::div(
            class = "alert alert-danger",
            shiny::icon("times-circle"),
            " ",
            shiny::strong(i18n()$t("Taxon already exists as ACCEPTED name")),
            shiny::br(),
            sprintf(i18n()$t("ID: %s"), check$idtax_n),
            shiny::br(),
            shiny::p(i18n()$t("Cannot add duplicate. Please use the Update or Synonymy tabs to modify this taxon."))
          )
        }
      } else {
        shiny::div(
          class = "alert alert-success",
          shiny::icon("check-circle"),
          " ",
          i18n()$t("Taxon does not exist in database - you can proceed with addition")
        )
      }
    })

    # Cancel existing synonymy
    shiny::observeEvent(input$btn_cancel_existing_synonymy, {
      check <- rv$existing_check

      shiny::withProgress({
        tryCatch({
          cli::cli_alert_info("Canceling synonymy for existing taxon ID {check$idtax_n}...")

          # Get pool connection
          pool_conn <- pool()

          update_dico_name(
            id_searched = check$idtax_n,
            cancel_synonymy = TRUE,
            ask_before_update = FALSE,
            add_backup = TRUE,
            show_results = FALSE,
            con = pool_conn
          )

          shiny::showNotification(
            i18n()$t("Synonymy cancelled! The taxon is now an accepted name."),
            type = "message",
            duration = 5
          )

          # Reset check
          rv$existing_check <- NULL

        }, error = function(e) {
          cli::cli_alert_danger("Failed to cancel synonymy: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Cancelling synonymy..."))
    })

    # Back from Step 2
    # Validate and auto-fill taxonomy based on family
    shiny::observeEvent(input$tax_fam, {
      if (!is.null(input$tax_fam) && nchar(trimws(input$tax_fam)) > 0) {

        tryCatch({
          family_name <- trimws(input$tax_fam)

          # Get connection
          actual_con <- if (inherits(pool(), "Pool")) {
            pool::poolCheckout(pool())
          } else {
            pool()
          }

          on.exit({
            if (inherits(pool(), "Pool") && !is.null(actual_con)) {
              pool::poolReturn(actual_con)
            }
          }, add = TRUE)

          # Direct minimal query for family-level taxon (no traits!)
          # Family-level taxa have genus = NA/NULL and species = NA/NULL
          # Class is stored as FK to table_tax_famclass
          family_entry <- dplyr::tbl(actual_con, "table_taxa") %>%
            dplyr::filter(tax_fam == !!family_name) %>%
            dplyr::filter(is.na(tax_gen)) %>%
            dplyr::filter(is.na(tax_esp)) %>%
            dplyr::select(-tax_famclass) %>%
            dplyr::left_join(
              dplyr::tbl(actual_con, "table_tax_famclass"),
              by = "id_tax_famclass"
            ) %>%
            dplyr::select(tax_fam, tax_order, id_tax_famclass, tax_famclass) %>%
            dplyr::distinct() %>%
            dplyr::collect()

          if (nrow(family_entry) > 0) {
            # Found family in backbone - use its order and class

            # DIAGNOSTIC: Print what we found
            cli::cli_alert_info("Family entry found with {nrow(family_entry)} row(s)")
            cli::cli_alert_info("Columns: {paste(names(family_entry), collapse = ', ')}")
            cli::cli_alert_info("First row data:")
            print(family_entry[1, ])

            # Extract order: remove NAs, select most frequent
            order_values <- family_entry$tax_order[!is.na(family_entry$tax_order)]
            if (length(order_values) > 0) {
              # Get most frequent value (mode)
              order_val <- names(sort(table(order_values), decreasing = TRUE))[1]
            } else {
              order_val <- NA_character_
            }

            # Extract class: remove NAs, select most frequent
            class_values <- family_entry$tax_famclass[!is.na(family_entry$tax_famclass)]
            if (length(class_values) > 0) {
              # Get most frequent value (mode)
              class_val <- names(sort(table(class_values), decreasing = TRUE))[1]
            } else {
              class_val <- NA_character_
            }

            # DIAGNOSTIC: Print extracted values
            cli::cli_alert_info("Extracted order_val: {if(is.na(order_val)) 'NA' else order_val}")
            cli::cli_alert_info("Extracted class_val: {if(is.na(class_val)) 'NA' else class_val}")

            # Auto-fill if fields are empty
            if (!is.na(order_val) && (is.null(input$tax_order) || nchar(trimws(input$tax_order)) == 0)) {
              cli::cli_alert_info("Updating order field with: {order_val}")
              shiny::updateTextInput(session, "tax_order", value = order_val)
              rv$order_required <- FALSE
            }

            if (!is.na(class_val) && (is.null(input$tax_famclass) || nchar(trimws(input$tax_famclass)) == 0)) {
              cli::cli_alert_info("Updating class field with: {class_val}")
              shiny::updateTextInput(session, "tax_famclass", value = class_val)
              rv$class_required <- FALSE
            }

            # If order or class is missing from family entry, require manual input
            if (is.na(order_val)) {
              cli::cli_alert_warning("Order is NA in family entry - requiring manual input")
              rv$order_required <- TRUE
            }
            if (is.na(class_val)) {
              cli::cli_alert_warning("Class is NA in family entry - requiring manual input")
              rv$class_required <- TRUE
            }

            cli::cli_alert_success("Family found in backbone: {family_name}")

          } else {
            # Try fuzzy matching with LIKE pattern (no traits!)
            similar_families <- dplyr::tbl(actual_con, "table_taxa") %>%
              dplyr::filter(is.na(tax_gen)) %>%
              dplyr::filter(is.na(tax_esp)) %>%
              dplyr::left_join(
                dplyr::tbl(actual_con, "table_tax_famclass"),
                by = "id_tax_famclass"
              ) %>%
              dplyr::select(tax_fam, tax_order, tax_famclass) %>%
              dplyr::distinct() %>%
              dplyr::collect() %>%
              dplyr::filter(grepl(family_name, tax_fam, ignore.case = TRUE))

            if (nrow(similar_families) > 0) {
              # Found similar family names
              suggestions <- paste(head(similar_families$tax_fam, 5), collapse = ", ")
              shiny::showNotification(
                paste0(i18n()$t("Family not found. Did you mean:"), " ", suggestions, "?"),
                type = "warning",
                duration = 8
              )
            } else {
              shiny::showNotification(
                i18n()$t("Family not found in taxonomic backbone. Please enter Order and Class manually, or add the family to the backbone first."),
                type = "warning",
                duration = 8
              )
            }

            # Mark fields as required since family not found
            rv$order_required <- TRUE
            rv$class_required <- TRUE
            cli::cli_alert_warning("Family '{family_name}' not found in backbone as family-level taxon")
          }

        }, error = function(e) {
          cli::cli_alert_warning("Could not validate taxonomy: {e$message}")
          # On error, require manual input to be safe
          rv$order_required <- TRUE
          rv$class_required <- TRUE
        })
      }
    }, ignoreInit = TRUE)

    # Dynamic labels for order and class
    output$order_label <- shiny::renderText({
      if (rv$order_required) {
        paste(i18n()$t("Order"), "*")
      } else {
        i18n()$t("Order")
      }
    })

    output$class_label <- shiny::renderText({
      if (rv$class_required) {
        paste(i18n()$t("Class"), "*")
      } else {
        i18n()$t("Class")
      }
    })

    shiny::observeEvent(input$btn_back_step2, {
      rv$current_step <- 1
    })

    # Next from Step 2 (validate and check existence)
    shiny::observeEvent(input$btn_next_step2, {
      # Validate required fields
      if (is.null(input$tax_gen) || nchar(trimws(input$tax_gen)) == 0) {
        shiny::showNotification(
          i18n()$t("Genus is required"),
          type = "error"
        )
        return()
      }

      # Validate order if required
      if (rv$order_required && (is.null(input$tax_order) || nchar(trimws(input$tax_order)) == 0)) {
        shiny::showNotification(
          i18n()$t("Order is required because the family was not found in the taxonomic backbone"),
          type = "error",
          duration = 8
        )
        return()
      }

      # Validate class if required
      if (rv$class_required && (is.null(input$tax_famclass) || nchar(trimws(input$tax_famclass)) == 0)) {
        shiny::showNotification(
          i18n()$t("Class is required because the family was not found in the taxonomic backbone"),
          type = "error",
          duration = 8
        )
        return()
      }

      if (is.null(input$tax_fam) || nchar(trimws(input$tax_fam)) == 0) {
        shiny::showNotification(
          i18n()$t("Family is required"),
          type = "error"
        )
        return()
      }

      # Save form data
      rv$form_data$tax_gen <- trimws(input$tax_gen)
      rv$form_data$tax_esp <- if (nchar(trimws(input$tax_esp)) > 0) trimws(input$tax_esp) else NULL
      rv$form_data$tax_fam <- trimws(input$tax_fam)
      rv$form_data$tax_order <- if (nchar(trimws(input$tax_order)) > 0) trimws(input$tax_order) else NULL
      rv$form_data$tax_famclass <- if (nchar(trimws(input$tax_famclass)) > 0) trimws(input$tax_famclass) else NULL
      rv$form_data$tax_tax <- if (nchar(trimws(input$tax_tax)) > 0) trimws(input$tax_tax) else NULL
      rv$form_data$tax_rank1 <- if (nchar(input$tax_rank1) > 0) input$tax_rank1 else NULL
      rv$form_data$tax_name1 <- if (nchar(trimws(input$tax_name1)) > 0) trimws(input$tax_name1) else NULL
      rv$form_data$author1 <- if (nchar(trimws(input$author1)) > 0) trimws(input$author1) else NULL
      rv$form_data$author2 <- if (nchar(trimws(input$author2)) > 0) trimws(input$author2) else NULL
      rv$form_data$author3 <- if (nchar(trimws(input$author3)) > 0) trimws(input$author3) else NULL
      rv$form_data$year_description <- input$year_description
      rv$form_data$morpho_species <- input$morpho_species

      # Check if taxon already exists in database
      shiny::withProgress({
        tryCatch({
          result <- query_taxa(
            genus = rv$form_data$tax_gen,
            species = rv$form_data$tax_esp,
            exact_match = TRUE,
            con = pool()
          )

          if (nrow(result) > 0) {
            # Taxon exists
            taxon <- result[1, ]
            rv$existing_check <- list(
              exists = TRUE,
              idtax_n = taxon$idtax_n,
              idtax_good_n = taxon$idtax_good_n,
              is_synonym = !is.na(taxon$idtax_good_n)
            )

            if (!rv$existing_check$is_synonym) {
              # Exists as accepted - cannot proceed
              shiny::showNotification(
                i18n()$t("This taxon already exists as an accepted name. Cannot add duplicate."),
                type = "error",
                duration = 10
              )
              return()
            } else {
              # Exists as synonym - show option to cancel synonymy
              shiny::showNotification(
                i18n()$t("This taxon exists as a synonym. See options above."),
                type = "warning",
                duration = 10
              )
              return()
            }
          } else {
            # Does not exist - can proceed
            rv$existing_check <- list(exists = FALSE)
            rv$current_step <- 3
          }

        }, error = function(e) {
          cli::cli_alert_warning("Could not check existence: {e$message}")
          # Proceed anyway
          rv$existing_check <- NULL
          rv$current_step <- 3
        })
      }, message = i18n()$t("Checking if taxon exists..."))
    })

    # Step 3: Growth Form (with integrated selector)
    step3_growth_ui <- function(ns, i18n, rv) {
      shiny::tagList(
        shiny::h5(i18n()$t("Step 3: Growth Form (Optional)")),

        shiny::p(
          class = "text-muted",
          i18n()$t("Select growth form characteristics for this taxon. You can skip this step if you don't have this information.")
        ),

        # Growth form selector module
        mod_growth_form_selector_ui(ns("growth_form")),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_back_step3"),
              i18n()$t("Back"),
              icon = shiny::icon("arrow-left"),
              class = "btn-secondary btn-block"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_next_step3"),
              i18n()$t("Next: Review"),
              icon = shiny::icon("arrow-right"),
              class = "btn-success btn-block"
            )
          )
        )
      )
    }

    # Back from Step 3
    shiny::observeEvent(input$btn_back_step3, {
      rv$current_step <- 2
    })

    # Next from Step 3
    shiny::observeEvent(input$btn_next_step3, {
      rv$current_step <- 4
    })

    # Step 4: Review & Submit
    step4_review_ui <- function(ns, i18n, rv) {
      fd <- rv$form_data

      shiny::tagList(
        shiny::h5(i18n()$t("Step 4: Review & Submit")),

        shiny::wellPanel(
          style = "background-color: #f8f9fa;",
          shiny::h6(i18n()$t("Review New Taxon")),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::strong(i18n()$t("Genus:")), " ", fd$tax_gen, shiny::br(),
              shiny::strong(i18n()$t("Species:")), " ", if (is.null(fd$tax_esp)) "N/A" else fd$tax_esp, shiny::br(),
              shiny::strong(i18n()$t("Family:")), " ", fd$tax_fam, shiny::br(),
              shiny::strong(i18n()$t("Order:")), " ", if (is.null(fd$tax_order)) "N/A" else fd$tax_order, shiny::br(),
              shiny::strong(i18n()$t("Class:")), " ", if (is.null(fd$tax_famclass)) "N/A" else fd$tax_famclass
            ),
            shiny::column(
              6,
              shiny::strong(i18n()$t("Full name:")), " ", if (is.null(fd$tax_tax)) "N/A" else fd$tax_tax, shiny::br(),
              shiny::strong(i18n()$t("Author:")), " ", if (is.null(fd$author1)) "N/A" else fd$author1, shiny::br(),
              shiny::strong(i18n()$t("Year:")), " ", if (is.null(fd$year_description)) "N/A" else fd$year_description, shiny::br(),
              shiny::strong(i18n()$t("Morphotaxon:")), " ", if (fd$morpho_species) i18n()$t("Yes") else i18n()$t("No")
            )
          )
        ),

        shiny::div(
          class = "alert alert-info",
          shiny::icon("info-circle"),
          " ",
          i18n()$t("After adding, you can optionally set this taxon as a synonym of another")
        ),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_back_step4"),
              i18n()$t("Back"),
              icon = shiny::icon("arrow-left"),
              class = "btn-secondary btn-block"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_submit"),
              i18n()$t("Submit - Add Taxon"),
              icon = shiny::icon("plus-circle"),
              class = "btn-success btn-block"
            )
          )
        )
      )
    }

    # Back from Step 4
    shiny::observeEvent(input$btn_back_step4, {
      rv$current_step <- 3
    })

    # Submit new taxon (WITHOUT synonymy)
    shiny::observeEvent(input$btn_submit, {
      shiny::req(rv$form_data)
      fd <- rv$form_data

      shiny::withProgress({
        tryCatch({
          cli::cli_alert_info("Adding new taxon to database...")

          # Call non-interactive function (no growth form prompts)
          new_id <- .add_taxa_noninteractive(
            tax_gen = fd$tax_gen,
            tax_esp = fd$tax_esp,
            tax_fam = fd$tax_fam,
            tax_order = fd$tax_order,
            tax_famclass = fd$tax_famclass,
            tax_rank1 = fd$tax_rank1,
            tax_name1 = fd$tax_name1,
            author1 = fd$author1,
            author2 = fd$author2,
            author3 = fd$author3,
            year_description = fd$year_description,
            morpho_species = fd$morpho_species,
            tax_tax = fd$tax_tax,
            con = pool()
          )

          # Store the new taxon ID
          rv$new_taxon_id <- new_id

          # Add growth forms if selected
          n_selections <- length(growth_form_module$growth_form_selections())
          is_valid <- growth_form_module$is_valid()
          basis_value <- growth_form_module$basisofrecord()
          remarks_value <- growth_form_module$measurementremarks()

          cli::cli_alert_info("Growth form debug:")
          cli::cli_alert_info("  - Number of selections: {n_selections}")
          cli::cli_alert_info("  - Is valid: {is_valid}")
          cli::cli_alert_info("  - Basis of record: '{if(is.null(basis_value)) 'NULL' else if(basis_value == '') 'EMPTY' else basis_value}'")
          cli::cli_alert_info("  - Remarks: '{if(is.null(remarks_value)) 'NULL' else if(remarks_value == '') 'EMPTY' else remarks_value}'")

          if (n_selections > 0) {
            cli::cli_alert_info("Growth form selections found:")
            print(growth_form_module$growth_form_selections())
          }

          if (n_selections > 0 && is_valid) {

            cli::cli_alert_info("Adding growth forms...")

            tryCatch({
              # Add growth forms directly (non-interactive)
              .add_growth_forms_noninteractive(
                idtax = new_id,
                growth_form_selections = growth_form_module$growth_form_selections(),
                basisofrecord = growth_form_module$basisofrecord(),
                measurementremarks = growth_form_module$measurementremarks(),
                pool = pool()
              )

              cli::cli_alert_success("Growth forms added successfully")

              shiny::showNotification(
                i18n()$t("Growth forms added successfully!"),
                type = "message",
                duration = 5
              )

            }, error = function(e) {
              cli::cli_alert_danger("Failed to add growth forms: {e$message}")
              cli::cli_alert_info("Error details:")
              print(e)
              traceback()
              shiny::showNotification(
                paste(i18n()$t("Warning: Growth forms not added:"), e$message),
                type = "warning",
                duration = 10
              )
            })
          } else {
            if (n_selections == 0) {
              cli::cli_alert_warning("No growth forms selected")
            } else if (!is_valid) {
              cli::cli_alert_warning("Growth form validation failed - basis of record missing?")
            }
          }

          shiny::showNotification(
            i18n()$t("Taxon added successfully!"),
            type = "message",
            duration = 5
          )

          # Move to Step 5 (optional synonymy)
          rv$current_step <- 5

        }, error = function(e) {
          cli::cli_alert_danger("Failed to add taxon: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error adding taxon:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Adding taxon to database..."))
    })

    # Step 5: Optional Synonymy
    step5_synonymy_ui <- function(ns, i18n, rv) {
      shiny::tagList(
        shiny::h5(i18n()$t("Step 5: Manage Synonymy (Optional)")),

        shiny::div(
          class = "alert alert-success",
          shiny::icon("check-circle"),
          " ",
          shiny::strong(i18n()$t("Taxon successfully added!")),
          shiny::br(),
          if (!is.null(rv$new_taxon_id)) {
            sprintf(i18n()$t("New taxon ID: %s"), rv$new_taxon_id)
          }
        ),

        # OPTION 1: Set EXISTING taxon as synonym of NEW (most common)
        shiny::wellPanel(
          style = "border-left: 4px solid #28a745;",
          shiny::h6(
            shiny::icon("star"),
            " ",
            i18n()$t("Option 1: Set an existing taxon as synonym of this new entry")
          ),
          shiny::p(
            class = "text-muted",
            i18n()$t("Most common: If an existing taxon should be updated to point to this new entry as the accepted name")
          ),

          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                ns("existing_binomial"),
                i18n()$t("Existing taxon (binomial)"),
                placeholder = "Genus species"
              ),
              shiny::helpText(i18n()$t("Enter genus and species separated by space (e.g., 'Pinus alba')"))
            ),
            shiny::column(
              6,
              shiny::numericInput(
                ns("existing_id"),
                i18n()$t("Or existing taxon ID"),
                value = NA
              )
            )
          ),

          shiny::uiOutput(ns("existing_taxon_selector_ui")),

          shiny::actionButton(
            ns("btn_set_existing_as_synonym"),
            i18n()$t("Set Existing as Synonym of New"),
            icon = shiny::icon("arrow-left"),
            class = "btn-success"
          )
        ),

        shiny::hr(),

        # OPTION 2: Set NEW taxon as synonym of EXISTING
        shiny::wellPanel(
          shiny::h6(i18n()$t("Option 2: Set this new entry as synonym of an existing taxon")),
          shiny::p(
            class = "text-muted",
            i18n()$t("Less common: If this new entry should point to an existing taxon as the accepted name")
          ),

          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                ns("accepted_binomial"),
                i18n()$t("Accepted name (binomial)"),
                placeholder = "Genus species"
              ),
              shiny::helpText(i18n()$t("Enter genus and species separated by space (e.g., 'Pinus alba')"))
            ),
            shiny::column(
              6,
              shiny::numericInput(
                ns("accepted_id"),
                i18n()$t("Or accepted taxon ID"),
                value = NA
              )
            )
          ),

          shiny::uiOutput(ns("accepted_taxon_selector_ui")),

          shiny::actionButton(
            ns("btn_set_new_as_synonym"),
            i18n()$t("Set New as Synonym of Existing"),
            icon = shiny::icon("arrow-right"),
            class = "btn-warning"
          )
        ),

        shiny::hr(),

        shiny::actionButton(
          ns("btn_finish"),
          i18n()$t("Finish - Start New Addition"),
          icon = shiny::icon("check"),
          class = "btn-primary btn-block"
        )
      )
    }

    # Render taxon selector for existing taxon (when multiple matches found)
    output$existing_taxon_selector_ui <- shiny::renderUI({
      if (is.null(rv$existing_taxon_matches) || nrow(rv$existing_taxon_matches) == 0) {
        return(NULL)
      }

      # Create display labels with full taxonomic info
      choices <- setNames(
        rv$existing_taxon_matches$idtax_n,
        paste0(
          rv$existing_taxon_matches$tax_gen, " ",
          rv$existing_taxon_matches$tax_esp,
          ifelse(!is.na(rv$existing_taxon_matches$tax_infrasp_type) & rv$existing_taxon_matches$tax_infrasp_type != "",
                 paste0(" ", rv$existing_taxon_matches$tax_infrasp_type, " ", rv$existing_taxon_matches$tax_infrasp),
                 ""),
          " (ID: ", rv$existing_taxon_matches$idtax_n, ")"
        )
      )

      shiny::div(
        class = "alert alert-info",
        style = "margin-top: 10px;",
        shiny::icon("info-circle"),
        " ",
        shiny::strong(i18n()$t("Multiple taxa found - please select one:")),
        shiny::br(),
        shiny::br(),
        shiny::selectInput(
          ns("selected_existing_taxon"),
          i18n()$t("Select taxon to set as synonym"),
          choices = choices
        )
      )
    })

    # Render taxon selector for accepted taxon (when multiple matches found)
    output$accepted_taxon_selector_ui <- shiny::renderUI({
      if (is.null(rv$accepted_taxon_matches) || nrow(rv$accepted_taxon_matches) == 0) {
        return(NULL)
      }

      # Create display labels with full taxonomic info
      choices <- setNames(
        rv$accepted_taxon_matches$idtax_n,
        paste0(
          rv$accepted_taxon_matches$tax_gen, " ",
          rv$accepted_taxon_matches$tax_esp,
          ifelse(!is.na(rv$accepted_taxon_matches$tax_infrasp_type) & rv$accepted_taxon_matches$tax_infrasp_type != "",
                 paste0(" ", rv$accepted_taxon_matches$tax_infrasp_type, " ", rv$accepted_taxon_matches$tax_infrasp),
                 ""),
          " (ID: ", rv$accepted_taxon_matches$idtax_n, ")"
        )
      )

      shiny::div(
        class = "alert alert-info",
        style = "margin-top: 10px;",
        shiny::icon("info-circle"),
        " ",
        shiny::strong(i18n()$t("Multiple taxa found - please select one:")),
        shiny::br(),
        shiny::br(),
        shiny::selectInput(
          ns("selected_accepted_taxon"),
          i18n()$t("Select accepted taxon"),
          choices = choices
        )
      )
    })

    # OPTION 1: Set existing taxon as synonym of new (REVERSE - most common)
    shiny::observeEvent(input$btn_set_existing_as_synonym, {
      if (is.null(rv$new_taxon_id)) {
        shiny::showNotification(
          i18n()$t("Error: Could not determine new taxon ID"),
          type = "error"
        )
        return()
      }

      # Validate inputs
      has_binomial <- !is.null(input$existing_binomial) && nchar(trimws(input$existing_binomial)) > 0
      has_id <- !is.null(input$existing_id) && !is.na(input$existing_id)

      if (!has_binomial && !has_id) {
        shiny::showNotification(
          i18n()$t("Please provide binomial name or taxon ID"),
          type = "error"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          # Check if user has selected from dropdown (multiple matches scenario)
          if (!is.null(input$selected_existing_taxon)) {
            existing_id <- as.numeric(input$selected_existing_taxon)
            rv$existing_taxon_matches <- NULL  # Clear matches after selection
            cli::cli_alert_info("Using selected taxon ID {existing_id}")
          } else {
            # First, find the existing taxon
            cli::cli_alert_info("Finding existing taxon...")

            # Parse binomial if provided
            search_params <- list()
            if (has_binomial) {
              binomial_parts <- trimws(strsplit(trimws(input$existing_binomial), "\\s+")[[1]])
              if (length(binomial_parts) >= 2) {
                # Full binomial provided - pass as species parameter (query_taxa expects full binomial)
                search_params$species <- trimws(input$existing_binomial)
              } else if (length(binomial_parts) == 1) {
                # Only genus provided
                search_params$genus <- binomial_parts[1]
              }
            }
            if (has_id) search_params$ids <- input$existing_id

            existing_taxon <- do.call(query_taxa, search_params)

            if (nrow(existing_taxon) == 0) {
              shiny::showNotification(
                i18n()$t("Existing taxon not found"),
                type = "error"
              )
              return()
            }

            if (nrow(existing_taxon) > 1) {
              # Store matches and show selector
              rv$existing_taxon_matches <- existing_taxon
              shiny::showNotification(
                i18n()$t("Multiple taxa found. Please select one from the dropdown above."),
                type = "warning",
                duration = 5
              )
              return()
            }

            existing_id <- existing_taxon$idtax_n[1]
            cli::cli_alert_info("Found existing taxon ID {existing_id}")
          }

          # Get pool connection
          pool_conn <- pool()

          # Check if this existing taxon has other synonyms pointing to it
          actual_con <- pool::poolCheckout(pool_conn)

          on.exit({
            pool::poolReturn(actual_con)
          }, add = TRUE)

          synonyms_of_existing <- dplyr::tbl(actual_con, "table_taxa") %>%
            dplyr::filter(idtax_good_n == !!existing_id) %>%
            dplyr::select(idtax_n, tax_gen, tax_esp, tax_fam, idtax_good_n) %>%
            dplyr::collect()

          # Check if there are cascade synonyms to handle
          # If yes, use direct SQL for EVERYTHING to avoid interactive prompts from update_dico_name()
          # If no, use update_dico_name() which handles backups properly
          if (nrow(synonyms_of_existing) > 0) {
            cli::cli_alert_info("Found {nrow(synonyms_of_existing)} existing synonym(s) of taxon {existing_id}")
            cli::cli_alert_info("Using direct SQL for main synonym and cascade synonyms to avoid prompts...")

            # Build list of all IDs to update (main existing + its synonyms)
            all_ids_to_update <- c(existing_id, synonyms_of_existing$idtax_n)

            # Update all at once with single SQL statement
            sql <- sprintf(
              "UPDATE table_taxa SET idtax_good_n = %d WHERE idtax_n IN (%s)",
              rv$new_taxon_id,
              paste(all_ids_to_update, collapse = ", ")
            )

            n_updated <- DBI::dbExecute(actual_con, sql)

            cli::cli_alert_success("Updated {n_updated} taxon/taxa (1 main + {nrow(synonyms_of_existing)} cascade)")

            shiny::showNotification(
              sprintf(
                i18n()$t("Successfully updated %d synonym(s) to point to new taxon"),
                n_updated
              ),
              type = "message",
              duration = 5
            )
          } else {
            # No cascade synonyms - use update_dico_name() which handles backups
            cli::cli_alert_info("No cascade synonyms - using update_dico_name() with backups...")

            update_dico_name(
              id_searched = existing_id,
              synonym_of = list(id = rv$new_taxon_id),
              ask_before_update = FALSE,
              add_backup = TRUE,
              show_results = FALSE,
              con = pool_conn
            )

            cli::cli_alert_success("Main synonym relationship set")

            shiny::showNotification(
              i18n()$t("Existing taxon set as synonym of new entry!"),
              type = "message",
              duration = 5
            )
          }

        }, error = function(e) {
          cli::cli_alert_danger("Failed to set synonym: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error setting synonym:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Setting synonym relationship..."))
    })

    # OPTION 2: Set newly added taxon as synonym of existing
    shiny::observeEvent(input$btn_set_new_as_synonym, {
      if (is.null(rv$new_taxon_id)) {
        shiny::showNotification(
          i18n()$t("Error: Could not determine new taxon ID"),
          type = "error"
        )
        return()
      }

      # Validate inputs
      has_binomial <- !is.null(input$accepted_binomial) && nchar(trimws(input$accepted_binomial)) > 0
      has_id <- !is.null(input$accepted_id) && !is.na(input$accepted_id)

      if (!has_binomial && !has_id) {
        shiny::showNotification(
          i18n()$t("Please provide binomial name or taxon ID"),
          type = "error"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          # Check if user has selected from dropdown (multiple matches scenario)
          if (!is.null(input$selected_accepted_taxon)) {
            accepted_id <- as.numeric(input$selected_accepted_taxon)
            rv$accepted_taxon_matches <- NULL  # Clear matches after selection
            cli::cli_alert_info("Using selected taxon ID {accepted_id}")
          } else {
            # First, find the accepted taxon
            cli::cli_alert_info("Finding accepted taxon...")

            # Parse binomial if provided
            search_params <- list()
            if (has_binomial) {
              binomial_parts <- trimws(strsplit(trimws(input$accepted_binomial), "\\s+")[[1]])
              if (length(binomial_parts) >= 2) {
                # Full binomial provided - pass as species parameter (query_taxa expects full binomial)
                search_params$species <- trimws(input$accepted_binomial)
              } else if (length(binomial_parts) == 1) {
                # Only genus provided
                search_params$genus <- binomial_parts[1]
              }
            }
            if (has_id) search_params$ids <- input$accepted_id

            accepted_taxon <- do.call(query_taxa, search_params)

            if (nrow(accepted_taxon) == 0) {
              shiny::showNotification(
                i18n()$t("Accepted taxon not found"),
                type = "error"
              )
              return()
            }

            if (nrow(accepted_taxon) > 1) {
              # Store matches and show selector
              rv$accepted_taxon_matches <- accepted_taxon
              shiny::showNotification(
                i18n()$t("Multiple taxa found. Please select one from the dropdown above."),
                type = "warning",
                duration = 5
              )
              return()
            }

            accepted_id <- accepted_taxon$idtax_n[1]
            cli::cli_alert_info("Found accepted taxon ID {accepted_id}")
          }

          cli::cli_alert_info("Setting new taxon ID {rv$new_taxon_id} as synonym of {accepted_id}...")

          # Get pool connection
          pool_conn <- pool()

          # Call update_dico_name with the accepted taxon ID
          update_dico_name(
            id_searched = rv$new_taxon_id,
            synonym_of = list(id = accepted_id),
            ask_before_update = FALSE,
            add_backup = TRUE,
            show_results = FALSE,
            con = pool_conn
          )

          shiny::showNotification(
            i18n()$t("Synonym relationship set successfully!"),
            type = "message",
            duration = 5
          )

        }, error = function(e) {
          cli::cli_alert_danger("Failed to set synonym: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error setting synonym:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Setting synonym relationship..."))
    })

    # Finish and reset
    shiny::observeEvent(input$btn_finish, {
      rv$form_data <- list()
      rv$current_step <- 1
      rv$tropicos_results <- NULL
      rv$new_taxon_id <- NULL
      rv$existing_check <- NULL

      shiny::showNotification(
        i18n()$t("Ready to add another taxon"),
        type = "message"
      )
    })

    return(NULL)
  })
}


#' Prepare Growth Form Data for Database Insert
#'
#' Converts hierarchical growth form selections into a data frame
#' suitable for add_sp_traits_measures()
#'
#' @param growth_form_selections List of growth form paths
#' @param idtax Taxon ID
#'
#' @return Data frame with one row per path, traits as columns
#' @keywords internal
.prepare_growth_form_data <- function(growth_form_selections, idtax) {

  if (length(growth_form_selections) == 0) {
    return(NULL)
  }

  # Initialize empty list to collect rows
  all_rows <- list()

  # Process each path (each path represents one complete growth form selection)
  for (path_idx in seq_along(growth_form_selections)) {
    path <- growth_form_selections[[path_idx]]

    # Create a row for this path
    row_data <- list(idtax = idtax)

    # Add each level in the path as a column
    for (level in path) {
      trait_name <- level$trait
      trait_value <- level$value

      # Add trait column to row
      row_data[[trait_name]] <- trait_value
    }

    # Convert to data frame and add to collection
    all_rows[[path_idx]] <- as.data.frame(row_data, stringsAsFactors = FALSE)
  }

  # Combine all rows
  if (length(all_rows) == 1) {
    result <- all_rows[[1]]
  } else {
    # Use rbind with fill for missing columns
    result <- dplyr::bind_rows(all_rows)
  }

  return(result)
}


#' Add Growth Forms to Database (Non-Interactive)
#'
#' Directly inserts growth form measurements without interactive prompts
#'
#' @param idtax Taxon ID
#' @param growth_form_selections List of growth form paths
#' @param basisofrecord Basis of record
#' @param measurementremarks Measurement remarks
#' @param pool Database connection pool
#'
#' @return NULL (silently adds data)
#' @keywords internal
.add_growth_forms_noninteractive <- function(idtax,
                                             growth_form_selections,
                                             basisofrecord,
                                             measurementremarks,
                                             pool) {

  if (length(growth_form_selections) == 0) {
    cli::cli_alert_warning("No growth forms to add")
    return(invisible(NULL))
  }

  # Get connection
  actual_con <- if (inherits(pool, "Pool")) {
    pool::poolCheckout(pool)
  } else {
    pool
  }

  on.exit({
    if (inherits(pool, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Process each growth form path
  for (path_idx in seq_along(growth_form_selections)) {
    path <- growth_form_selections[[path_idx]]

    # Insert each level in the path as a separate measurement
    for (level in path) {
      id_trait <- level$id_trait
      trait_value <- level$value

      # Prepare measurement record
      measurement <- data.frame(
        idtax = idtax,
        fk_id_trait = id_trait,
        traitvalue_char = trait_value,
        traitvalue = NA_real_,
        basisofrecord = basisofrecord,
        measurementremarks = if (is.null(measurementremarks) || measurementremarks == "") NA_character_ else measurementremarks,
        stringsAsFactors = FALSE
      )

      # Insert into table_traits_measures
      tryCatch({
        DBI::dbAppendTable(actual_con, "table_traits_measures", measurement)
        cli::cli_alert_success("Added {level$trait} = {trait_value}")
      }, error = function(e) {
        cli::cli_alert_warning("Failed to add {level$trait}: {e$message}")
        stop(e)
      })
    }
  }

  cli::cli_alert_success("All growth forms added for taxon {idtax}")
  return(invisible(NULL))
}
