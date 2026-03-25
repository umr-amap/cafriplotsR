# Feature Wizard - Step 1: Select Existing Plots
#
# Module for searching and selecting plots to add features to.

#' Feature Wizard Step 1: Select Plots - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step1_select_plots_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("map-marked-alt"),
      i18n$t("Step 1: Select Plots"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Search and select the plots you want to add features to."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Plot name selector (multi-select dropdown — rendered from server with choices)
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::uiOutput(ns("plot_name_ui"))
      ),
      shiny::column(
        4,
        shiny::div(
          style = "margin-top: 25px;",
          shiny::actionButton(
            ns("load_info"),
            shiny::tagList(shiny::icon("info-circle"), " ", i18n$t("Load Plot Info")),
            class = "btn-primary"
          )
        )
      )
    ),

    shiny::hr(),

    # Selected plots summary
    shiny::h4(
      shiny::icon("check-square"),
      i18n$t("Selected Plots"),
      style = "margin-top: 20px; margin-bottom: 15px;"
    ),

    shiny::uiOutput(ns("selected_summary")),
    DT::DTOutput(ns("selected_plots_table"))
  )
}


#' Feature Wizard Step 1: Select Plots - Server
#'
#' @param id Module namespace ID
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive containing data.frame of selected plots
#' @keywords internal
#' @export
mod_feat_step1_select_plots_server <- function(id, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_plots_data <- shiny::reactiveVal(NULL)
    plot_name_choices <- shiny::reactiveVal(character(0))

    # Load plot names once (lightweight query, same pattern as mod_plot_filters)
    shiny::observeEvent(con(), {
      shiny::req(con())

      tryCatch({
        pool_conn <- con()

        plot_names <- DBI::dbGetQuery(
          pool_conn,
          "SELECT DISTINCT plot_name
           FROM data_liste_plots
           WHERE plot_name IS NOT NULL
           ORDER BY plot_name"
        )

        plot_name_choices(plot_names$plot_name)
        cli::cli_alert_success("Loaded {nrow(plot_names)} plot names")
      }, error = function(e) {
        cli::cli_alert_warning("Could not load plot names: {e$message}")
        shiny::showNotification(
          paste("Error loading plot names:", e$message),
          type = "error", duration = 10
        )
      })
    }, once = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE)

    # Render the selectInput with choices — created fresh each time the UI renders
    output$plot_name_ui <- shiny::renderUI({
      choices <- plot_name_choices()
      shiny::selectInput(
        ns("plot_name"),
        label = i18n()$t("Plot Name(s)"),
        choices = choices,
        selected = NULL,
        multiple = TRUE
      )
    })

    # Load info for selected plots
    shiny::observeEvent(input$load_info, {
      selected_names <- input$plot_name
      if (is.null(selected_names) || length(selected_names) == 0) {
        shiny::showNotification(
          i18n()$t("Please select at least one plot."),
          type = "warning"
        )
        return()
      }

      tryCatch({
        pool_conn <- con()

        # Query plot metadata
        plot_info <- DBI::dbGetQuery(pool_conn, sprintf(
          "SELECT p.id_liste_plots, p.plot_name, p.ddlat, p.ddlon,
                  m.method
           FROM data_liste_plots p
           LEFT JOIN methodslist m ON p.id_method = m.id_method
           WHERE p.plot_name IN (%s)",
          paste(sprintf("'%s'", gsub("'", "''", selected_names)), collapse = ", ")
        ))

        # Query census counts from subplot features
        # Census is stored as a feature with type = "census", typevalue = census number
        census_info <- DBI::dbGetQuery(pool_conn, sprintf(
          "SELECT s.id_table_liste_plots,
                  COUNT(DISTINCT s.typevalue) as n_census,
                  MAX(s.typevalue::numeric)   as last_census,
                  MAX(s.year)                 as last_year
           FROM data_liste_sub_plots s
           JOIN subplotype_list spt ON s.id_type_sub_plot = spt.id_subplotype
           WHERE s.id_table_liste_plots IN (%s)
             AND spt.type = 'census'
             AND s.typevalue IS NOT NULL
           GROUP BY s.id_table_liste_plots",
          paste(plot_info$id_liste_plots, collapse = ", ")
        ))

        # Merge
        result <- plot_info %>%
          dplyr::left_join(census_info,
                           by = c("id_liste_plots" = "id_table_liste_plots")) %>%
          dplyr::mutate(
            n_census = ifelse(is.na(n_census), 0L, as.integer(n_census)),
            last_census = ifelse(is.na(last_census), NA_real_, last_census),
            last_year = ifelse(is.na(last_year), NA_real_, last_year)
          )

        selected_plots_data(result)

        shiny::showNotification(
          sprintf(i18n()$t("%d plot(s) selected"), nrow(result)),
          type = "message", duration = 3
        )
      }, error = function(e) {
        cli::cli_alert_warning("Could not load plot info: {e$message}")
        shiny::showNotification(
          paste("Error:", e$message),
          type = "error", duration = 10
        )
      })
    })

    # Selected plots summary
    output$selected_summary <- shiny::renderUI({
      sel <- selected_plots_data()
      if (is.null(sel) || nrow(sel) == 0) {
        return(shiny::div(
          class = "alert alert-secondary",
          shiny::icon("info-circle"), " ",
          i18n()$t("No plots selected yet. Use the search above to find and select plots.")
        ))
      }

      shiny::div(
        class = "alert alert-success",
        shiny::icon("check-circle"), " ",
        shiny::strong(sprintf(i18n()$t("%d plot(s) selected"), nrow(sel)))
      )
    })

    output$selected_plots_table <- DT::renderDT({
      sel <- selected_plots_data()
      if (is.null(sel) || nrow(sel) == 0) return(NULL)

      display <- sel %>%
        dplyr::select(
          plot_name,
          dplyr::any_of(c("method", "ddlat", "ddlon", "n_census", "last_census", "last_year"))
        )

      DT::datatable(
        display,
        options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
        rownames = FALSE,
        class = "display cell-border stripe"
      )
    })

    # Return selected plots
    return(shiny::reactive(selected_plots_data()))
  })
}
