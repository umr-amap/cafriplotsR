#' Plot Filters Module - UI
#'
#' UI component for plot query filters
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_plot_filters_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Query Filters"),
    shiny::p("Select criteria to filter forest plots", class = "text-muted"),

    # Basic Filters
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput(
          ns("country"),
          "Country",
          choices = NULL,  # Will be populated in server
          selected = NULL,
          multiple = TRUE
        )
      ),
      shiny::column(
        6,
        shiny::selectInput(
          ns("method"),
          "Method",
          choices = NULL,  # Will be populated in server
          selected = NULL,
          multiple = TRUE
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        6,
        shiny::textInput(
          ns("plot_name"),
          "Plot Name(s)",
          placeholder = "e.g., bouamir001, mbalmayo001 (comma-separated)"
        )
      ),
      shiny::column(
        6,
        shiny::textInput(
          ns("locality"),
          "Locality",
          placeholder = "e.g., Lope"
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        6,
        shiny::textInput(
          ns("tag"),
          "Individual Tag",
          placeholder = "Search by tree tag"
        )
      )
    ),

    # Advanced Filters (collapsible)
    shiny::hr(),
    shiny::h5(
      shiny::icon("chevron-down"),
      "Advanced Filters",
      style = "cursor: pointer;",
      onclick = sprintf("$('#%s').toggle();", ns("advanced_panel"))
    ),

    shiny::div(
      id = ns("advanced_panel"),
      style = "display: none;",
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::numericInput(
            ns("id_plot"),
            "Plot ID",
            value = NA,
            min = 1
          )
        ),
        shiny::column(
          4,
          shiny::numericInput(
            ns("id_individual"),
            "Individual ID",
            value = NA,
            min = 1
          )
        ),
        shiny::column(
          4,
          shiny::numericInput(
            ns("id_tax"),
            "Taxon ID",
            value = NA,
            min = 1
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::numericInput(
            ns("id_specimen"),
            "Specimen ID",
            value = NA,
            min = 1
          )
        ),
        shiny::column(
          4,
          shiny::checkboxInput(
            ns("exact_match"),
            "Exact match for text filters",
            value = FALSE
          )
        )
      )
    ),

    # Execute Button
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::actionButton(
          ns("execute_query"),
          "Execute Query",
          icon = shiny::icon("search"),
          class = "btn-primary btn-lg btn-block"
        )
      )
    ),

    # Query summary
    shiny::uiOutput(ns("query_summary"))
  )
}

#' Plot Filters Module - Server
#'
#' Server logic for plot query filters
#'
#' @param id Module namespace ID
#' @param pool Database connection pool (reactive or static)
#'
#' @return A reactive list containing:
#'   - filters: Named list of filter values
#'   - execute_trigger: Reactive counter that increments on execute
#'
#' @keywords internal
#' @export
mod_plot_filters_server <- function(id, pool) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    execute_counter <- shiny::reactiveVal(0)
    choices_loaded <- shiny::reactiveVal(FALSE)

    # Get pool connection
    con <- shiny::reactive({
      pool_val <- if (shiny::is.reactive(pool)) pool() else pool
      # Return NULL if pool is not available yet
      if (is.null(pool_val)) return(NULL)
      pool_val
    })

    # Populate country choices (only once, using observeEvent with once=TRUE)
    shiny::observeEvent(con(), {
      pool_conn <- con()
      shiny::req(pool_conn)

      cli::cli_alert_info("Loading filter choices from database...")

      tryCatch({
        # Query through data_liste_plots to respect RLS policies
        # Join with lookup tables to get country and method names
        countries <- DBI::dbGetQuery(
          pool_conn,
          "SELECT DISTINCT c.country
           FROM data_liste_plots p
           JOIN table_countries c ON p.id_country = c.id_country
           WHERE c.country IS NOT NULL
           ORDER BY c.country"
        )

        methods <- DBI::dbGetQuery(
          pool_conn,
          "SELECT DISTINCT m.method
           FROM data_liste_plots p
           JOIN methodslist m ON p.id_method = m.id_method
           WHERE m.method IS NOT NULL
           ORDER BY m.method"
        )

        cli::cli_alert_success("Loaded {nrow(countries)} accessible countries and {nrow(methods)} accessible methods")

        # Update both select inputs WITHOUT resetting selection
        shiny::updateSelectInput(
          session,
          "country",
          choices = countries$country
        )

        shiny::updateSelectInput(
          session,
          "method",
          choices = methods$method
        )

        choices_loaded(TRUE)

      }, error = function(e) {
        cli::cli_alert_danger("Failed to load filter choices: {e$message}")
      })
    }, once = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE)

    # Execute button handler
    shiny::observeEvent(input$execute_query, {
      cli::cli_alert_info("Execute button clicked! Counter: {execute_counter()}")
      execute_counter(execute_counter() + 1)
      cli::cli_alert_success("Execute counter incremented to: {execute_counter()}")
    })

    # Build filter list
    filters <- shiny::reactive({
      # Parse comma-separated plot names
      plot_names <- NULL
      if (nzchar(input$plot_name)) {
        plot_names <- trimws(unlist(strsplit(input$plot_name, ",")))
        plot_names <- plot_names[nzchar(plot_names)]  # Remove empty strings
        if (length(plot_names) == 0) plot_names <- NULL
      }

      list(
        # Basic filters
        country = if (length(input$country) > 0 && !all(input$country == "")) input$country else NULL,
        plot_name = plot_names,
        locality_name = if (nzchar(input$locality)) input$locality else NULL,
        method = if (length(input$method) > 0 && !all(input$method == "")) input$method else NULL,
        tag = if (nzchar(input$tag)) input$tag else NULL,

        # Advanced filters
        id_plot = if (!is.na(input$id_plot)) input$id_plot else NULL,
        id_individual = if (!is.na(input$id_individual)) input$id_individual else NULL,
        id_tax = if (!is.na(input$id_tax)) input$id_tax else NULL,
        id_specimen = if (!is.na(input$id_specimen)) input$id_specimen else NULL,
        exact_match = input$exact_match
      )
    })

    # Query summary output
    output$query_summary <- shiny::renderUI({
      active_filters <- Filter(Negate(is.null), filters())

      if (length(active_filters) == 0) {
        return(
          shiny::div(
            class = "alert alert-info",
            style = "margin-top: 15px;",
            shiny::icon("info-circle"),
            " No filters applied - will return all plots"
          )
        )
      }

      filter_text <- paste(
        names(active_filters),
        "=",
        sapply(active_filters, function(x) {
          if (length(x) > 1) paste(x, collapse = ", ") else as.character(x)
        }),
        collapse = " | "
      )

      shiny::div(
        class = "alert alert-success",
        style = "margin-top: 15px;",
        shiny::icon("filter"),
        shiny::strong(" Active filters: "),
        shiny::tags$br(),
        shiny::tags$small(filter_text)
      )
    })

    # Return reactive values
    return(
      list(
        filters = filters,
        execute_trigger = shiny::reactive(execute_counter())
      )
    )
  })
}
