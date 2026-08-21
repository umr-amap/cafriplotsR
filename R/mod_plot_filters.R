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
    shiny::uiOutput(ns("title_ui")),

    # Basic Filters - inputs are static to preserve choices
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput(
          ns("country"),
          label = NULL,  # Label set dynamically
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )
      ),
      shiny::column(
        6,
        shiny::selectInput(
          ns("method"),
          label = NULL,  # Label set dynamically
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput(
          ns("plot_name"),
          label = NULL,  # Label set dynamically
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )
      ),
      shiny::column(
        6,
        shiny::selectInput(
          ns("locality"),
          label = NULL,  # Label set dynamically
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::textInput(
          ns("tag"),
          label = NULL,  # Label set dynamically
          placeholder = ""
        )
      )
    ),

    # Advanced Filters (collapsible)
    shiny::hr(),
    shiny::uiOutput(ns("advanced_filters_ui")),

    # Execute Button
    shiny::hr(),
    shiny::uiOutput(ns("execute_button_ui")),

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
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return A reactive list containing:
#'   - filters: Named list of filter values
#'   - execute_trigger: Reactive counter that increments on execute
#'
#' @keywords internal
#' @export
mod_plot_filters_server <- function(id, pool, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    execute_counter <- shiny::reactiveVal(0)
    choices_loaded <- shiny::reactiveVal(FALSE)

    # Title UI
    output$title_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::h4(i18n()$t("Query Filters")),
        shiny::p(i18n()$t("Select criteria to filter forest plots"), class = "text-muted")
      )
    })

    # Update input labels when language changes
    shiny::observe({
      # Update labels using updateSelectInput/updateTextInput
      shiny::updateSelectInput(
        session, "country",
        label = i18n()$t("Country")
      )
      shiny::updateSelectInput(
        session, "method",
        label = i18n()$t("Method")
      )
      shiny::updateSelectInput(
        session, "plot_name",
        label = i18n()$t("Plot Name(s)")
      )
      shiny::updateSelectInput(
        session, "locality",
        label = i18n()$t("Locality")
      )
      shiny::updateTextInput(
        session, "tag",
        label = i18n()$t("Individual Tag"),
        placeholder = i18n()$t("Search by tree tag")
      )
    })

    # ---- Plot feature filters ------------------------------------------
    #
    # A plot feature is not a column of `data_liste_plots` but a row of
    # `data_liste_sub_plots` typed by `subplotype_list`, so it cannot be a
    # fixed input the way country or method can: the user picks which feature
    # first, and only then which of its values.
    #
    # Rows carry a stable id and their state is kept outside the inputs, so the
    # panel can be rebuilt -- on a language change, or when a row is added --
    # without losing what was already chosen.
    feature_choices   <- shiny::reactiveVal(NULL)
    feature_values    <- shiny::reactiveVal(list())
    feature_row_ids   <- shiny::reactiveVal(character(0))
    feature_row_state <- shiny::reactiveValues()

    next_feature_row_id <- local({
      counter <- 0
      function() {
        counter <<- counter + 1
        paste0("f", counter)
      }
    })

    # One filter row: which feature, which values, and a way to drop it.
    feature_filter_row <- function(rid, choices_vec, state, tr) {
      shiny::fluidRow(
        shiny::column(
          5,
          shiny::selectInput(
            ns(paste0("feature_type_", rid)),
            label = tr$t("Feature"),
            choices = c(
              stats::setNames("", tr$t("Choose a feature...")),
              choices_vec
            ),
            selected = state$feature
          )
        ),
        shiny::column(
          6,
          shiny::selectizeInput(
            ns(paste0("feature_value_", rid)),
            label = tr$t("Value(s)"),
            # Only what is already selected: the observer below fills in the
            # values the database actually holds once a feature is chosen.
            choices = state$values,
            selected = state$values,
            multiple = TRUE,
            options = list(
              create = TRUE,
              placeholder = tr$t("Type or choose a value")
            )
          )
        ),
        shiny::column(
          1,
          shiny::div(
            style = "margin-top: 25px;",
            # A plain button setting one shared input, so a single observer
            # serves every row however many there are.
            shiny::tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-danger",
              title = tr$t("Remove this feature filter"),
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                ns("remove_feature_filter"), rid
              ),
              shiny::icon("times")
            )
          )
        )
      )
    }

    shiny::observeEvent(input$add_feature_filter, {
      rid <- next_feature_row_id()
      feature_row_state[[rid]] <- list(feature = "", values = character(0))
      feature_row_ids(c(feature_row_ids(), rid))
    })

    shiny::observeEvent(input$remove_feature_filter, {
      rid <- input$remove_feature_filter
      feature_row_ids(setdiff(feature_row_ids(), rid))
      feature_row_state[[rid]] <- NULL
    }, ignoreInit = TRUE)

    # Keep the rows' state in step with their inputs. The UI reads this state
    # only under isolate(), so writing it here cannot re-render the panel and
    # reset the very inputs being read.
    shiny::observe({
      for (rid in feature_row_ids()) {
        feat <- input[[paste0("feature_type_", rid)]]
        vals <- input[[paste0("feature_value_", rid)]]
        shiny::isolate({
          feature_row_state[[rid]] <- list(
            feature = if (is.null(feat)) "" else feat,
            values  = if (is.null(vals)) character(0) else vals
          )
        })
      }
    })

    # Offer the values a feature actually holds, fetched once per feature.
    shiny::observe({
      pool_conn <- con()
      shiny::req(pool_conn)

      for (rid in feature_row_ids()) {
        feat <- input[[paste0("feature_type_", rid)]]
        if (is.null(feat) || !nzchar(feat)) next

        cached <- shiny::isolate(feature_values())

        if (is.null(cached[[feat]])) {
          cached[[feat]] <- tryCatch(
            plot_feature_values(feat, con = pool_conn)$value,
            error = function(e) {
              cli::cli_alert_warning(
                "Could not load values for feature {feat}: {e$message}"
              )
              character(0)
            }
          )
          feature_values(cached)
          cli::cli_alert_info(
            "Loaded {length(cached[[feat]])} value(s) for feature {feat}"
          )
        }

        shiny::updateSelectizeInput(
          session,
          paste0("feature_value_", rid),
          choices  = cached[[feat]],
          selected = shiny::isolate(input[[paste0("feature_value_", rid)]])
        )
      }
    })

    # Advanced Filters UI
    output$advanced_filters_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::h5(
          shiny::icon("chevron-down"),
          i18n()$t("Advanced Filters"),
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
                i18n()$t("Plot ID"),
                value = NA,
                min = 1
              )
            ),
            shiny::column(
              4,
              shiny::numericInput(
                ns("id_individual"),
                i18n()$t("Individual ID"),
                value = NA,
                min = 1
              )
            ),
            shiny::column(
              4,
              shiny::numericInput(
                ns("id_tax"),
                i18n()$t("Taxon ID"),
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
                i18n()$t("Specimen ID"),
                value = NA,
                min = 1
              )
            ),
            shiny::column(
              4,
              shiny::checkboxInput(
                ns("exact_match"),
                i18n()$t("Exact match for text filters"),
                value = TRUE
              )
            )
          ),

          shiny::hr(),
          shiny::h6(
            shiny::icon("layer-group"), " ",
            i18n()$t("Plot feature filters")
          ),
          shiny::p(
            i18n()$t("Filter on values stored as plot features rather than as plot columns, such as the data provider or the principal investigator."),
            class = "text-muted",
            style = "font-size: 0.85em;"
          ),
          {
            feats <- feature_choices()

            if (is.null(feats) || nrow(feats) == 0) {
              shiny::div(
                class = "text-muted",
                shiny::em(i18n()$t("No filterable plot features available"))
              )
            } else {
              choices_vec <- stats::setNames(feats$feature, feats$feature)

              shiny::tagList(
                lapply(feature_row_ids(), function(rid) {
                  state <- shiny::isolate(feature_row_state[[rid]])
                  if (is.null(state)) {
                    state <- list(feature = "", values = character(0))
                  }
                  feature_filter_row(rid, choices_vec, state, i18n())
                }),
                shiny::actionButton(
                  ns("add_feature_filter"),
                  i18n()$t("Add feature filter"),
                  icon = shiny::icon("plus"),
                  class = "btn-sm btn-outline-primary"
                )
              )
            }
          }
        )
      )
    })

    # Execute Button UI
    output$execute_button_ui <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::actionButton(
            ns("execute_query"),
            i18n()$t("Execute Query"),
            icon = shiny::icon("search"),
            class = "btn-primary btn-lg btn-block"
          )
        )
      )
    })

    # Get pool connection
    con <- shiny::reactive({
      pool_val <- if (shiny::is.reactive(pool)) pool() else pool
      # Return NULL if pool is not available yet
      if (is.null(pool_val)) return(NULL)
      pool_val
    })

    # Populate filter choices (only once, using observeEvent with once=TRUE)
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

        # Get distinct plot names
        plot_names <- DBI::dbGetQuery(
          pool_conn,
          "SELECT DISTINCT plot_name
           FROM data_liste_plots
           WHERE plot_name IS NOT NULL
           ORDER BY plot_name"
        )

        # Get distinct locality names
        localities <- DBI::dbGetQuery(
          pool_conn,
          "SELECT DISTINCT locality_name
           FROM data_liste_plots
           WHERE locality_name IS NOT NULL
           ORDER BY locality_name"
        )

        cli::cli_alert_success("Loaded {nrow(countries)} accessible countries, {nrow(methods)} accessible methods, {nrow(plot_names)} plot names, and {nrow(localities)} localities")

        # Update all select inputs WITHOUT resetting selection
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

        shiny::updateSelectInput(
          session,
          "plot_name",
          choices = plot_names$plot_name
        )

        shiny::updateSelectInput(
          session,
          "locality",
          choices = localities$locality_name
        )

        # Which plot features can be filtered on. Failing here must not cost
        # the user the rest of the filters, so it is caught on its own.
        feats <- tryCatch(
          plot_feature_filters(con = pool_conn),
          error = function(e) {
            cli::cli_alert_warning(
              "Could not load filterable plot features: {e$message}"
            )
            NULL
          }
        )
        feature_choices(feats)
        if (!is.null(feats)) {
          cli::cli_alert_success("Loaded {nrow(feats)} filterable plot feature(s)")
        }

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
      list(
        # Basic filters
        country = if (length(input$country) > 0 && !all(input$country == "")) input$country else NULL,
        plot_name = if (length(input$plot_name) > 0 && !all(input$plot_name == "")) input$plot_name else NULL,
        locality_name = if (length(input$locality) > 0 && !all(input$locality == "")) input$locality else NULL,
        method = if (length(input$method) > 0 && !all(input$method == "")) input$method else NULL,
        tag = if (!is.null(input$tag) && nzchar(input$tag)) input$tag else NULL,

        # Advanced filters (with NULL checks)
        id_plot = if (!is.null(input$id_plot) && !is.na(input$id_plot)) input$id_plot else NULL,
        id_individual = if (!is.null(input$id_individual) && !is.na(input$id_individual)) input$id_individual else NULL,
        id_tax = if (!is.null(input$id_tax) && !is.na(input$id_tax)) input$id_tax else NULL,
        id_specimen = if (!is.null(input$id_specimen) && !is.na(input$id_specimen)) input$id_specimen else NULL,
        exact_match = if (!is.null(input$exact_match)) input$exact_match else TRUE,

        # Plot features, as the named list query_plots() expects.
        feature_filters = {
          out <- list()

          for (rid in feature_row_ids()) {
            feat <- input[[paste0("feature_type_", rid)]]
            vals <- input[[paste0("feature_value_", rid)]]

            if (is.null(feat) || !nzchar(feat)) next

            vals <- as.character(vals)
            vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
            if (length(vals) == 0) next

            # Two rows naming the same feature are one filter holding both
            # sets of values: query_plots() refuses a repeated name, and the
            # user plainly meant "either of these".
            out[[feat]] <- unique(c(out[[feat]], vals))
          }

          if (length(out) == 0) NULL else out
        }
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
            " ", i18n()$t("No filters applied - will return all plots")
          )
        )
      }

      filter_text <- paste(
        names(active_filters),
        "=",
        vapply(active_filters, function(x) {
          # feature_filters is itself a named list, one entry per feature.
          if (is.list(x)) {
            paste(
              names(x), vapply(x, paste, character(1), collapse = ", "),
              sep = ": ", collapse = " ; "
            )
          } else {
            paste(x, collapse = ", ")
          }
        }, character(1)),
        collapse = " | "
      )

      shiny::div(
        class = "alert alert-success",
        style = "margin-top: 15px;",
        shiny::icon("filter"),
        shiny::strong(" ", i18n()$t("Active filters:"), " "),
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
