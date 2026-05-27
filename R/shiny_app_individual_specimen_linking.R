# Shiny App: Individual-Specimen Linking
#
# Interactive application for linking individual trees to herbarium specimens
# based on herbarium information stored in the individuals dataset.
#
# Workflow:
# 1. Select individuals using plot filters
# 2. Parse herbarium_code_char and herbarium_nbe_type columns
# 3. Match extracted collector names to table_colnam
# 4. Retrieve matching specimens from database
# 5. Validate taxonomic matches between individuals and specimens
# 6. Create confirmed links in data_link_specimens table
#
# Main function: launch_individual_specimen_linking_app()

#' Launch Individual-Specimen Linking App
#'
#' Launches interactive Shiny application for creating links between individual
#' trees and herbarium specimens based on herbarium information in the individuals
#' dataset.
#'
#' **Workflow:**
#'
#' 1. **Select Individuals**: Filter individuals by plot, country, tag, etc.
#' 2. **Parse Herbarium Info**: Extract collector names and specimen numbers from
#'    `herbarium_nbe_char` and `herbarium_nbe_type` columns
#' 3. **Match Collectors**: Interactively match extracted collector names to
#'    entries in `table_colnam`
#' 4. **Retrieve Specimens**: Find matching specimens in database by collector + number
#' 5. **Validate Taxonomy**: Review taxonomic matches between individuals and specimens
#' 6. **Create Links**: Execute link creation for validated matches
#'
#' **Understanding the Two Column Types:**
#'
#' The system uses two columns to track different specimen-individual relationships:
#'
#' - **`herbarium_nbe_type`**: The ACTUAL tree where the specimen was physically collected
#'   - Direct evidence (high confidence)
#'   - Creates `type_individual` link
#'
#' - **`herbarium_nbe_char`**: Trees field-identified as the SAME SPECIES as the specimen tree
#'   - Indirect evidence based on field identification (lower confidence)
#'   - Creates `referenced_individual` link
#'   - Extends specimen utility to more trees without collecting additional specimens
#'
#' **Link Type Logic:**
#' - If `herbarium_nbe_type` is non-empty → `type_individual` (specimen from THIS tree)
#' - If only `herbarium_nbe_char` is non-empty → `referenced_individual` (tree believed to be same species)
#' - If both columns have the SAME value → `type_individual` (confirms this is the specimen tree)
#'
#' @param lang Character, initial language ("en" or "fr"). Default "en".
#'
#' @return Launches Shiny app (does not return until app closes)
#'
#' @examples
#' \dontrun{
#' launch_individual_specimen_linking_app()
#' launch_individual_specimen_linking_app(lang = "fr")
#' }
#'
#' @export
launch_individual_specimen_linking_app <- function(lang = "fr") {
  # Load i18n translations
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file(
      "translations/translation.json",
      package = "CafriplotsR"
    )
  )
  i18n$set_translation_language(lang)

  # Define UI
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(spin = "fading-circle"),

    # Custom CSS
    shiny::tags$style(shiny::HTML("
      .step-indicator {
        display: inline-block;
        padding: 10px 20px;
        margin: 5px;
        border-radius: 25px;
        background: #e9ecef;
        color: #6c757d;
        font-weight: 500;
      }
      .step-indicator.active {
        background: #007bff;
        color: white;
      }
      .step-indicator.completed {
        background: #28a745;
        color: white;
      }
      .section-card {
        background: #f8f9fa;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        border-left: 4px solid #007bff;
      }
    ")),

    # App title and language toggle
    shiny::fluidRow(
      shiny::column(
        width = 8,
        shiny::h2(shiny::textOutput("app_title"))
      ),
      shiny::column(
        width = 4,
        style = "text-align: right; padding-top: 15px;",
        mod_language_toggle_ui("lang_toggle")
      )
    ),

    shiny::hr(),

    # Login panel (shown before authentication)
    shiny::conditionalPanel(
      condition = "!output.authenticated",
      mod_database_login_ui("login")
    ),

    # Main content (shown after authentication)
    shiny::conditionalPanel(
      condition = "output.authenticated",

      # Prerequisites information
      shiny::div(
        class = "section-card",
        style = "background: #fff3cd; border-left: 4px solid #ffc107; margin-bottom: 30px;",
        shiny::div(
          style = "display: flex; align-items: start; gap: 15px;",
          shiny::div(
            shiny::icon("exclamation-triangle", style = "font-size: 24px; color: #856404;")
          ),
          shiny::div(
            shiny::h4(
              shiny::textOutput("prerequisites_title", inline = TRUE),
              style = "margin: 0 0 10px 0; color: #856404;"
            ),
            shiny::uiOutput("prerequisites_content")
          )
        )
      ),

      # Progress indicator
      shiny::div(
        style = "text-align: center; margin-bottom: 30px;",
        shiny::uiOutput("step_progress")
      ),

      # Step content
      shiny::div(
        id = "step_container",

        # Step 1: Select Individuals
        shiny::conditionalPanel(
          condition = "output.current_step == 1",
          shiny::div(
            class = "section-card",
            shiny::h3(
              shiny::icon("filter"),
              " ",
              shiny::textOutput("step1_title", inline = TRUE)
            ),
            shiny::p(shiny::textOutput("step1_desc")),
            shiny::hr(),
            mod_plot_filters_ui("filters"),

            # Results preview
            shiny::hr(),
            shiny::uiOutput("step1_results_summary"),
            DT::dataTableOutput("step1_results_table")
          )
        ),

        # Step 2: Parse Herbarium Info
        shiny::conditionalPanel(
          condition = "output.current_step == 2",
          mod_herbarium_parser_ui("parser", i18n)
        ),

        # Step 3: Match Collectors
        shiny::conditionalPanel(
          condition = "output.current_step == 3",
          shiny::div(
            class = "section-card",
            shiny::h3(
              shiny::icon("link"),
              " ",
              shiny::textOutput("step3_title", inline = TRUE)
            ),
            shiny::p(shiny::textOutput("step3_desc")),
            shiny::hr(),
            mod_lookup_matcher_ui("matcher")
          )
        ),

        # Step 4: Retrieve Specimens
        shiny::conditionalPanel(
          condition = "output.current_step == 4",
          mod_specimen_retriever_ui("retriever", i18n)
        ),

        # Step 5: Validate Taxonomy
        shiny::conditionalPanel(
          condition = "output.current_step == 5",
          mod_taxonomic_validator_ui("validator", i18n)
        ),

        # Step 6: Create Links
        shiny::conditionalPanel(
          condition = "output.current_step == 6",
          mod_link_executor_ui("executor", i18n)
        )
      ),

      # Navigation buttons
      shiny::div(
        style = "margin-top: 30px; padding: 20px; border-top: 1px solid #dee2e6;",
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              "nav_back",
              shiny::tagList(shiny::icon("arrow-left"), " ", i18n$t("Back")),
              class = "btn-secondary"
            )
          ),
          shiny::column(
            6,
            style = "text-align: right;",
            shiny::actionButton(
              "nav_next",
              shiny::tagList(i18n$t("Next"), " ", shiny::icon("arrow-right")),
              class = "btn-primary"
            )
          )
        )
      )
    )
  )

  # Define Server
  server <- function(input, output, session) {

    # ===== LANGUAGE MANAGEMENT =====
    current_lang <- mod_language_toggle_server("lang_toggle", initial = lang)

    # Reactive i18n that updates when language changes
    i18n_reactive <- shiny::reactive({
      selected_lang <- current_lang()
      i18n$set_translation_language(selected_lang)
      i18n
    })

    # ===== DATABASE LOGIN =====
    login_result <- mod_database_login_server("login")
    pool_main <- login_result$pool_main
    pool_taxa <- login_result$pool_taxa
    authenticated <- login_result$authenticated

    # Sync language from login module to app language toggle
    shiny::observe({
      lang <- login_result$language()
      shiny::req(lang)
      shiny::updateRadioButtons(session, "lang_toggle-language", selected = lang)
    })

    # Output for conditional panels
    output$authenticated <- shiny::reactive({
      authenticated()
    })
    shiny::outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

    # ===== STEP TRACKING =====
    current_step <- shiny::reactiveVal(1)

    output$current_step <- shiny::reactive({
      current_step()
    })
    shiny::outputOptions(output, "current_step", suspendWhenHidden = FALSE)

    # Step completion tracking
    step_data <- shiny::reactiveValues(
      step1_complete = FALSE,
      step2_complete = FALSE,
      step3_complete = FALSE,
      step4_complete = FALSE,
      step5_complete = FALSE,
      step6_complete = FALSE,
      individuals_data = NULL,
      parsed_data = NULL,
      collector_matches = NULL,
      retrieved_specimens = NULL,
      validated_links = NULL,
      final_results = NULL
    )

    # ===== TEXT OUTPUTS =====
    output$app_title <- shiny::renderText({
      i18n_reactive()$t("Individual-Specimen Linking")
    })

    output$prerequisites_title <- shiny::renderText({
      i18n_reactive()$t("Prerequisites")
    })

    output$prerequisites_content <- shiny::renderUI({
      shiny::tagList(
        shiny::p(
          i18n_reactive()$t("Before linking individuals to specimens, ensure that:"),
          style = "margin-bottom: 10px; color: #856404;"
        ),
        shiny::tags$ul(
          style = "color: #856404; margin: 0;",
          shiny::tags$li(
            shiny::strong(i18n_reactive()$t("Individuals")),
            " ",
            i18n_reactive()$t("are already entered in the database with herbarium reference information in at least one of the following column"),
            " ",
            shiny::code("herbarium_nbe_char"),
            " ",
            i18n_reactive()$t("or"),
            " ",
            shiny::code("herbarium_nbe_type")
          ),
          shiny::tags$li(
            shiny::strong(i18n_reactive()$t("Specimens")),
            " ",
            i18n_reactive()$t("are already entered in the herbarium database with collector names and specimen numbers")
          )
        ),
        shiny::tags$hr(style = "border-color: #856404; margin: 15px 0;"),
        shiny::p(
          shiny::strong(i18n_reactive()$t("Understanding the two column types:")),
          style = "margin: 10px 0 5px 0; color: #856404;"
        ),
        shiny::tags$ul(
          style = "color: #856404; margin: 0; font-size: 14px;",
          shiny::tags$li(
            shiny::code("herbarium_nbe_type"),
            ": ",
            i18n_reactive()$t("The ACTUAL tree where the specimen was physically collected (high confidence)")
          ),
          shiny::tags$li(
            shiny::code("herbarium_nbe_char"),
            ": ",
            i18n_reactive()$t("Other trees field-identified as the SAME SPECIES as the specimen tree (lower confidence, extends specimen utility)")
          )
        )
      )
    })

    output$step1_title <- shiny::renderText({
      i18n_reactive()$t("Step 1: Select Individuals")
    })

    output$step1_desc <- shiny::renderText({
      i18n_reactive()$t("Filter and select individuals that have herbarium information to link to specimens.")
    })

    output$step3_title <- shiny::renderText({
      i18n_reactive()$t("Step 3: Match Collectors")
    })

    output$step3_desc <- shiny::renderText({
      i18n_reactive()$t("Match extracted collector names to entries in the database.")
    })

    # ===== STEP PROGRESS INDICATOR =====
    output$step_progress <- shiny::renderUI({
      step <- current_step()

      steps <- list(
        list(num = 1, label = i18n_reactive()$t("Select")),
        list(num = 2, label = i18n_reactive()$t("Parse")),
        list(num = 3, label = i18n_reactive()$t("Match")),
        list(num = 4, label = i18n_reactive()$t("Retrieve")),
        list(num = 5, label = i18n_reactive()$t("Validate")),
        list(num = 6, label = i18n_reactive()$t("Create"))
      )

      shiny::div(
        lapply(steps, function(s) {
          class <- "step-indicator"
          if (s$num < step) class <- paste(class, "completed")
          if (s$num == step) class <- paste(class, "active")

          icon_name <- if (s$num < step) "check" else paste0("", s$num)

          shiny::span(
            class = class,
            if (s$num < step) shiny::icon("check") else s$num,
            " ",
            s$label
          )
        })
      )
    })

    # ===== MODULE INITIALIZATION =====
    shiny::observe({
      shiny::req(authenticated() == TRUE)

      # Step 1: Plot Filters
      filter_output <- mod_plot_filters_server("filters", pool = pool_main, i18n = i18n_reactive)

      # When filters execute, get individuals with herbarium info
      shiny::observeEvent(filter_output$execute_trigger(), {
        shiny::req(filter_output$filters())

        filters <- filter_output$filters()

        tryCatch({
          # Query individuals based on filters
          # This uses the PlotFetcher with filters, then extracts individuals
          individuals <- .get_individuals_with_herbarium(
            filters = filters,
            con = pool_main()
          )

          if (nrow(individuals) == 0) {
            shiny::showNotification(
              i18n_reactive()$t("No individuals found with herbarium information matching your filters."),
              type = "warning",
              duration = 5
            )
            step_data$step1_complete <- FALSE
            step_data$individuals_data <- NULL
          } else {
            step_data$individuals_data <- individuals
            step_data$step1_complete <- TRUE

            shiny::showNotification(
              sprintf(i18n_reactive()$t("Found %d individuals with herbarium information."), nrow(individuals)),
              type = "message",
              duration = 4
            )
          }
        }, error = function(e) {
          shiny::showNotification(
            paste(i18n_reactive()$t("Error querying individuals:"), e$message),
            type = "error",
            duration = NULL
          )
          step_data$step1_complete <- FALSE
          step_data$individuals_data <- NULL
        })
      })

      # Step 1: Results summary
      output$step1_results_summary <- shiny::renderUI({
        individuals <- step_data$individuals_data

        if (is.null(individuals)) {
          return(NULL)
        }

        shiny::div(
          class = "alert alert-info",
          shiny::strong(nrow(individuals)),
          " ",
          i18n_reactive()$t("individuals found with herbarium information")
        )
      })

      # Step 1: Results table
      output$step1_results_table <- DT::renderDataTable({
        individuals <- step_data$individuals_data
        shiny::req(individuals)

        # Create display label for individuals
        display_data <- individuals %>%
          dplyr::mutate(
            individual_label = paste0(
              plot_name, " - ",
              ifelse(!is.na(tag), paste0("Tag ", tag), ""),
              ifelse(!is.na(code_individu), paste0(" (", code_individu, ")"), "")
            )
          ) %>%
          dplyr::select(
            dplyr::any_of(c(
              "id_n",
              "individual_label",
              "herbarium_nbe_char",
              "herbarium_nbe_type",
              "already_linked"
            ))
          ) %>%
          dplyr::rename(
            "ID" = "id_n",
            !!i18n_reactive()$t("Individual") := "individual_label",
            !!i18n_reactive()$t("Herb. Reference") := "herbarium_nbe_char",
            !!i18n_reactive()$t("Herb. Type") := "herbarium_nbe_type",
            !!i18n_reactive()$t("Already linked") := "already_linked"
          )

        DT::datatable(
          display_data,
          options = list(
            pageLength = 20,
            scrollX = TRUE,
            dom = 'frtip',
            language = list(
              search = paste0(i18n_reactive()$t("Search:"), " "),
              lengthMenu = paste(i18n_reactive()$t("Show"), "_MENU_", i18n_reactive()$t("entries")),
              info = paste(i18n_reactive()$t("Showing"), "_START_", i18n_reactive()$t("to"), "_END_",
                           i18n_reactive()$t("of"), "_TOTAL_", i18n_reactive()$t("entries"))
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        ) %>%
          DT::formatStyle(
            columns = c(i18n_reactive()$t("Herb. Reference"), i18n_reactive()$t("Herb. Type")),
            backgroundColor = DT::styleEqual(c(NA, ""), c("#f8f9fa", "#f8f9fa"), default = "#d4edda"),
            fontWeight = DT::styleEqual(c(NA, ""), c("normal", "normal"), default = "bold")
          ) %>%
          DT::formatStyle(
            columns = i18n_reactive()$t("Already linked"),
            backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#fff3cd", "#ffffff"))
          )
      })

      # Step 2: Parse Herbarium Info
      parser_output <- mod_herbarium_parser_server(
        "parser",
        individuals_data = shiny::reactive(step_data$individuals_data),
        i18n = i18n_reactive
      )

      shiny::observe({
        parsed <- parser_output$parsed_data()
        if (!is.null(parsed) && nrow(parsed) > 0) {
          step_data$parsed_data <- parsed
          step_data$step2_complete <- TRUE
        }
      })

      # Step 3: Match Collectors (using existing mod_lookup_matcher)
      # Prepare invalid_values for lookup matcher
      invalid_collectors <- shiny::reactive({
        shiny::req(step_data$parsed_data)
        unique_collectors <- unique(step_data$parsed_data$extracted_collector)
        unique_collectors <- unique_collectors[!is.na(unique_collectors) & unique_collectors != ""]
        list(collector = unique_collectors)
      })

      matcher_output <- mod_lookup_matcher_server(
        "matcher",
        invalid_values = invalid_collectors,
        con = pool_main
      )

      shiny::observe({
        if (matcher_output$applied()) {
          matches <- matcher_output$matches()
          step_data$collector_matches <- matches
          step_data$step3_complete <- TRUE
        }
      })

      # Step 4: Retrieve Specimens
      retriever_output <- mod_specimen_retriever_server(
        "retriever",
        parsed_data = shiny::reactive(step_data$parsed_data),
        collector_matches = shiny::reactive(step_data$collector_matches),
        con = pool_main,
        i18n = i18n_reactive
      )

      shiny::observe({
        retrieved <- retriever_output$retrieved_specimens()
        if (!is.null(retrieved)) {
          step_data$retrieved_specimens <- retrieved
          step_data$step4_complete <- retriever_output$is_complete()
        }
      })

      # Step 5: Validate Taxonomy
      validator_output <- mod_taxonomic_validator_server(
        "validator",
        preliminary_links = shiny::reactive(step_data$retrieved_specimens),
        con_taxa = pool_taxa,
        i18n = i18n_reactive
      )

      shiny::observe({
        validated <- validator_output$validated_links()
        if (!is.null(validated)) {
          step_data$validated_links <- validated
          step_data$step5_complete <- validator_output$is_complete()
        }
      })

      # Step 6: Execute Link Creation
      executor_output <- mod_link_executor_server(
        "executor",
        validated_links = shiny::reactive(step_data$validated_links),
        con = pool_main,
        i18n = i18n_reactive
      )

      shiny::observe({
        if (executor_output$is_complete()) {
          step_data$final_results <- executor_output$results()
          step_data$step6_complete <- TRUE
        }
      })
    })

    # ===== NAVIGATION =====
    shiny::observeEvent(input$nav_back, {
      step <- current_step()
      if (step > 1) {
        current_step(step - 1)
      }
    })

    shiny::observeEvent(input$nav_next, {
      step <- current_step()

      # Validate before proceeding
      can_proceed <- FALSE

      if (step == 1) {
        if (step_data$step1_complete) {
          can_proceed <- TRUE
        } else {
          shiny::showNotification(
            i18n_reactive()$t("Please select individuals first by applying filters."),
            type = "warning"
          )
        }
      } else if (step == 2) {
        if (step_data$step2_complete) {
          can_proceed <- TRUE
        } else {
          shiny::showNotification(
            i18n_reactive()$t("Please parse herbarium information first."),
            type = "warning"
          )
        }
      } else if (step == 3) {
        if (step_data$step3_complete) {
          can_proceed <- TRUE
        } else {
          shiny::showNotification(
            i18n_reactive()$t("Please match all collectors before proceeding."),
            type = "warning"
          )
        }
      } else if (step == 4) {
        if (step_data$step4_complete) {
          can_proceed <- TRUE
        } else {
          shiny::showNotification(
            i18n_reactive()$t("Please retrieve specimens first."),
            type = "warning"
          )
        }
      } else if (step == 5) {
        if (step_data$step5_complete) {
          can_proceed <- TRUE
        } else {
          shiny::showNotification(
            i18n_reactive()$t("Please validate taxonomic matches first."),
            type = "warning"
          )
        }
      }

      if (can_proceed && step < 6) {
        current_step(step + 1)
      }
    })

    # Update button states
    shiny::observe({
      step <- current_step()

      # Add defensive checks
      if (is.null(step) || length(step) == 0) {
        return()
      }

      shinyjs::toggleState("nav_back", condition = step > 1)
      shinyjs::toggleState("nav_next", condition = step < 6)

      if (step == 6) {
        shinyjs::hide("nav_next")
      } else {
        shinyjs::show("nav_next")
      }
    })

    # ===== SESSION CLEANUP =====
    session$onSessionEnded(function() {
      tryCatch({
        cleanup_connections()
      }, error = function(e) {
        cli::cli_alert_warning("Failed to cleanup connections: {e$message}")
      })
      shiny::stopApp()
    })
  }

  # Run the app
  shiny::shinyApp(ui = ui, server = server)
}


#' Get Individuals with Herbarium Information (Internal)
#'
#' Queries individuals based on plot filters and filters to only those with
#' herbarium information and no existing links.
#'
#' @param filters Named list of filter values from mod_plot_filters
#' @param con Database connection pool
#'
#' @return Data frame with individuals
#' @keywords internal
.get_individuals_with_herbarium <- function(filters, con) {

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  tryCatch({
    # Start with individuals table
    query <- dplyr::tbl(actual_con, "data_individuals")

  # Apply tag filter if provided
  if (!is.null(filters) && !is.null(filters$tag) && filters$tag != "") {
    query <- query %>%
      dplyr::filter(tag == !!filters$tag)
  }

  # Join with plots for filtering
  query <- query %>%
    dplyr::left_join(
      dplyr::tbl(actual_con, "data_liste_plots"),
      by = c("id_table_liste_plots_n" = "id_liste_plots")
    )

  # Join with lookup tables for country and method filtering
  if (!is.null(filters)) {
    # Resolve country names to id_country, then filter directly
    if (!is.null(filters$country) && length(filters$country) > 0) {
      country_ids <- DBI::dbGetQuery(
        actual_con,
        glue::glue_sql(
          "SELECT id_country FROM table_countries WHERE country IN ({vals*})",
          vals = filters$country,
          .con = actual_con
        )
      )$id_country

      if (length(country_ids) > 0) {
        query <- query %>%
          dplyr::filter(id_country %in% !!country_ids)
      }
    }

    # Join with methods if filtering by method
    if (!is.null(filters$method) && length(filters$method) > 0) {
      query <- query %>%
        dplyr::left_join(
          dplyr::tbl(actual_con, "methodslist"),
          by = "id_method"
        ) %>%
        dplyr::filter(method %in% !!filters$method)
    }

    # Filter by plot name (direct column)
    if (!is.null(filters$plot_name) && length(filters$plot_name) > 0) {
      query <- query %>%
        dplyr::filter(plot_name %in% !!filters$plot_name)
    }

    # Filter by locality name (direct column)
    if (!is.null(filters$locality_name) && length(filters$locality_name) > 0) {
      query <- query %>%
        dplyr::filter(locality_name %in% !!filters$locality_name)
    }
  }

  # Filter to individuals with herbarium info
  query <- query %>%
    dplyr::filter(
      !is.na(herbarium_nbe_char) | !is.na(herbarium_nbe_type)
    )

  # Get existing links to mark (not exclude)
  existing_links <- dplyr::tbl(actual_con, "data_link_specimens") %>%
    dplyr::select(id_n) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  # Select relevant columns and collect
  individuals <- query %>%
    dplyr::select(
      id_n, tag, code_individu, id_table_liste_plots_n, idtax_n,
      herbarium_nbe_char, herbarium_nbe_type, plot_name
    ) %>%
    dplyr::collect()

  # Mark already-linked individuals rather than excluding them
  individuals <- individuals %>%
    dplyr::mutate(already_linked = id_n %in% existing_links$id_n)

  return(individuals)

  }, error = function(e) {
    cli::cli_alert_danger("Error querying individuals: {e$message}")
    stop(e)
  })
}
