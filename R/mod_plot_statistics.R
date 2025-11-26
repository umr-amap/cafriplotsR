#' Plot Statistics Module - UI
#'
#' @description
#' Displays basic statistics and visualizations for extracted plot data.
#' Adapts to different output styles by detecting available columns.
#'
#' @param id Namespace ID
#'
#' @return Shiny UI
#' @keywords internal
#' @export
mod_plot_statistics_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Header
    shiny::fluidRow(
      shiny::column(12,
        shiny::h3(shiny::icon("chart-bar"), " ", shiny::textOutput(ns("title"), inline = TRUE))
      )
    ),

    shiny::br(),

    # Status/message when no data
    shiny::conditionalPanel(
      condition = sprintf("!output['%s']", ns("has_data")),
      shiny::div(
        class = "alert alert-info",
        shiny::icon("info-circle"),
        " ",
        shiny::textOutput(ns("no_data_message"), inline = TRUE)
      )
    ),

    # Main content when data is available
    shiny::conditionalPanel(
      condition = sprintf("output['%s']", ns("has_data")),

      # Summary Statistics Cards
      shiny::fluidRow(
        shiny::column(3,
          shiny::div(
            class = "well",
            style = "background-color: #e3f2fd; border-left: 4px solid #2196F3;",
            shiny::h4(shiny::textOutput(ns("n_plots"), inline = TRUE)),
            shiny::p(shiny::textOutput(ns("label_plots"), inline = TRUE), style = "margin: 0;")
          )
        ),
        shiny::column(3,
          shiny::div(
            class = "well",
            style = "background-color: #e8f5e9; border-left: 4px solid #4CAF50;",
            shiny::h4(shiny::textOutput(ns("n_individuals"), inline = TRUE)),
            shiny::p(shiny::textOutput(ns("label_individuals"), inline = TRUE), style = "margin: 0;")
          )
        ),
        shiny::column(3,
          shiny::div(
            class = "well",
            style = "background-color: #fff3e0; border-left: 4px solid #FF9800;",
            shiny::h4(shiny::textOutput(ns("n_species"), inline = TRUE)),
            shiny::p(shiny::textOutput(ns("label_species"), inline = TRUE), style = "margin: 0;")
          )
        ),
        shiny::column(3,
          shiny::div(
            class = "well",
            style = "background-color: #f3e5f5; border-left: 4px solid #9C27B0;",
            shiny::h4(shiny::textOutput(ns("n_families"), inline = TRUE)),
            shiny::p(shiny::textOutput(ns("label_families"), inline = TRUE), style = "margin: 0;")
          )
        )
      ),

      shiny::br(),

      # Diameter statistics (if available)
      shiny::conditionalPanel(
        condition = sprintf("output['%s']", ns("has_diameter")),
        shiny::fluidRow(
          shiny::column(12,
            shiny::div(
              class = "well",
              shiny::h4(shiny::icon("ruler"), " ", shiny::textOutput(ns("diameter_stats_title"), inline = TRUE)),
              shiny::fluidRow(
                shiny::column(3,
                  shiny::strong(shiny::textOutput(ns("label_mean"), inline = TRUE)),
                  shiny::textOutput(ns("mean_dbh"), inline = TRUE)
                ),
                shiny::column(3,
                  shiny::strong(shiny::textOutput(ns("label_median"), inline = TRUE)),
                  shiny::textOutput(ns("median_dbh"), inline = TRUE)
                ),
                shiny::column(3,
                  shiny::strong(shiny::textOutput(ns("label_min"), inline = TRUE)),
                  shiny::textOutput(ns("min_dbh"), inline = TRUE)
                ),
                shiny::column(3,
                  shiny::strong(shiny::textOutput(ns("label_max"), inline = TRUE)),
                  shiny::textOutput(ns("max_dbh"), inline = TRUE)
                )
              )
            )
          )
        ),
        shiny::br()
      ),

      shiny::hr(),

      # Visualizations
      shiny::fluidRow(
        # Diameter distribution
        shiny::column(6,
          shiny::conditionalPanel(
            condition = sprintf("output['%s']", ns("has_diameter")),
            shiny::div(
              class = "well",
              shiny::h4(shiny::textOutput(ns("diameter_plot_title"), inline = TRUE)),
              plotly::plotlyOutput(ns("diameter_plot"), height = "350px")
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_diameter")),
            shiny::div(
              class = "alert alert-warning",
              shiny::icon("exclamation-triangle"),
              " ",
              shiny::textOutput(ns("no_diameter_message"), inline = TRUE)
            )
          )
        ),

        # Species composition
        shiny::column(6,
          shiny::conditionalPanel(
            condition = sprintf("output['%s']", ns("has_species")),
            shiny::div(
              class = "well",
              shiny::h4(shiny::textOutput(ns("species_plot_title"), inline = TRUE)),
              plotly::plotlyOutput(ns("species_plot"), height = "350px"),
              shiny::br(),
              shiny::sliderInput(ns("n_species_show"),
                shiny::textOutput(ns("label_n_species"), inline = TRUE),
                min = 5, max = 30, value = 10, step = 5
              )
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_species")),
            shiny::div(
              class = "alert alert-warning",
              shiny::icon("exclamation-triangle"),
              " ",
              shiny::textOutput(ns("no_species_message"), inline = TRUE)
            )
          )
        )
      ),

      shiny::hr(),

      # Download section
      shiny::fluidRow(
        shiny::column(12,
          shiny::h4(shiny::icon("download"), " ", shiny::textOutput(ns("download_title"), inline = TRUE)),
          shiny::downloadButton(ns("download_stats"), shiny::textOutput(ns("download_button_label"), inline = TRUE))
        )
      )
    )
  )
}


#' Plot Statistics Module - Server
#'
#' @description
#' Server logic for plot statistics visualization
#'
#' @param id Namespace ID
#' @param results Reactive containing query results (individuals data)
#' @param i18n Reactive translator object
#'
#' @return None (module handles its own outputs)
#' @keywords internal
#' @export
mod_plot_statistics_server <- function(id, results, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- Reactive: Extract individuals data ----
    individuals_data <- shiny::reactive({
      req(results())
      data <- results()

      # Handle list structure (with $individuals table)
      if (is.list(data) && !is.data.frame(data)) {
        if ("individuals" %in% names(data)) {
          return(data$individuals)
        } else if ("extract" %in% names(data)) {
          return(data$extract)
        }
        return(NULL)
      }

      # Already a data frame
      return(data)
    })

    # ---- Reactive: Column mapping ----
    col_map <- shiny::reactive({
      req(individuals_data())
      .detect_column_names(individuals_data())
    })

    # ---- Reactive: Check data availability ----
    output$has_data <- shiny::reactive({
      !is.null(individuals_data()) && nrow(individuals_data()) > 0
    })
    shiny::outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    output$has_diameter <- shiny::reactive({
      req(col_map())
      !is.null(col_map()$diameter)
    })
    shiny::outputOptions(output, "has_diameter", suspendWhenHidden = FALSE)

    output$has_species <- shiny::reactive({
      req(col_map())
      !is.null(col_map()$species)
    })
    shiny::outputOptions(output, "has_species", suspendWhenHidden = FALSE)

    # ---- Reactive: Compute statistics ----
    stats <- shiny::reactive({
      req(individuals_data(), col_map())
      .compute_basic_statistics(individuals_data(), col_map())
    })

    # ---- Translations ----
    output$title <- shiny::renderText({
      i18n()$t("Plot Statistics & Visualizations")
    })

    output$no_data_message <- shiny::renderText({
      i18n()$t("No data available. Please extract individuals first.")
    })

    output$no_diameter_message <- shiny::renderText({
      i18n()$t("Diameter data not available in current output")
    })

    output$no_species_message <- shiny::renderText({
      i18n()$t("Species data not available in current output")
    })

    output$label_plots <- shiny::renderText({ i18n()$t("Plots") })
    output$label_individuals <- shiny::renderText({ i18n()$t("Individuals") })
    output$label_species <- shiny::renderText({ i18n()$t("Species") })
    output$label_families <- shiny::renderText({ i18n()$t("Families") })
    output$label_mean <- shiny::renderText({ i18n()$t("Mean:") })
    output$label_median <- shiny::renderText({ i18n()$t("Median:") })
    output$label_min <- shiny::renderText({ i18n()$t("Min:") })
    output$label_max <- shiny::renderText({ i18n()$t("Max:") })
    output$label_n_species <- shiny::renderText({ i18n()$t("Number of species to show:") })

    output$diameter_stats_title <- shiny::renderText({
      i18n()$t("Diameter Statistics (cm)")
    })

    output$diameter_plot_title <- shiny::renderText({
      i18n()$t("Diameter Distribution")
    })

    output$species_plot_title <- shiny::renderText({
      i18n()$t("Most Abundant Species")
    })

    output$download_title <- shiny::renderText({
      i18n()$t("Export Statistics")
    })

    output$download_button_label <- shiny::renderText({
      i18n()$t("Download Summary (.csv)")
    })

    # ---- Summary Statistics Outputs ----
    output$n_plots <- shiny::renderText({
      req(stats())
      format(stats()$n_plots, big.mark = ",")
    })

    output$n_individuals <- shiny::renderText({
      req(stats())
      format(stats()$n_individuals, big.mark = ",")
    })

    output$n_species <- shiny::renderText({
      req(stats())
      if (is.na(stats()$n_species)) "N/A" else format(stats()$n_species, big.mark = ",")
    })

    output$n_families <- shiny::renderText({
      req(stats())
      if (is.na(stats()$n_families)) "N/A" else format(stats()$n_families, big.mark = ",")
    })

    # ---- Diameter Statistics ----
    output$mean_dbh <- shiny::renderText({
      req(stats())
      if (is.na(stats()$mean_dbh)) {
        "N/A"
      } else {
        paste(round(stats()$mean_dbh, 2), "cm")
      }
    })

    output$median_dbh <- shiny::renderText({
      req(stats())
      if (is.na(stats()$median_dbh)) {
        "N/A"
      } else {
        paste(round(stats()$median_dbh, 2), "cm")
      }
    })

    output$min_dbh <- shiny::renderText({
      req(stats())
      if (is.na(stats()$min_dbh)) {
        "N/A"
      } else {
        paste(round(stats()$min_dbh, 2), "cm")
      }
    })

    output$max_dbh <- shiny::renderText({
      req(stats())
      if (is.na(stats()$max_dbh)) {
        "N/A"
      } else {
        paste(round(stats()$max_dbh, 2), "cm")
      }
    })

    # ---- Diameter Distribution Plot ----
    output$diameter_plot <- plotly::renderPlotly({
      req(individuals_data(), col_map(), col_map()$diameter)

      data <- individuals_data()
      diam_col <- col_map()$diameter

      # Filter out NA values
      plot_data <- data %>%
        dplyr::filter(!is.na(.data[[diam_col]]))

      # Create histogram
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[diam_col]])) +
        ggplot2::geom_histogram(bins = 30, fill = "#2196F3", color = "white", alpha = 0.7) +
        ggplot2::labs(
          x = i18n()$t("Diameter (cm)"),
          y = i18n()$t("Frequency")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          panel.grid.minor = ggplot2::element_blank()
        )

      plotly::ggplotly(p, tooltip = c("x", "y"))
    })

    # ---- Species Composition Plot ----
    output$species_plot <- plotly::renderPlotly({
      req(individuals_data(), col_map(), col_map()$species)

      data <- individuals_data()
      species_col <- col_map()$species
      n_show <- input$n_species_show

      # Count species
      species_counts <- data %>%
        dplyr::filter(!is.na(.data[[species_col]])) %>%
        dplyr::count(.data[[species_col]], name = "count") %>%
        dplyr::arrange(dplyr::desc(count)) %>%
        dplyr::slice_head(n = n_show) %>%
        dplyr::mutate(species = forcats::fct_reorder(.data[[species_col]], count))

      # Create bar plot
      p <- ggplot2::ggplot(species_counts, ggplot2::aes(x = count, y = species)) +
        ggplot2::geom_col(fill = "#FF9800", alpha = 0.8) +
        ggplot2::labs(
          x = i18n()$t("Number of individuals"),
          y = i18n()$t("Species")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 8)
        )

      plotly::ggplotly(p, tooltip = c("x", "y"))
    })

    # ---- Download Handler ----
    output$download_stats <- shiny::downloadHandler(
      filename = function() {
        paste0("plot_statistics_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(stats())

        # Create summary data frame
        summary_df <- data.frame(
          Metric = c(
            "Number of Plots",
            "Number of Individuals",
            "Number of Species",
            "Number of Families",
            "Mean Diameter (cm)",
            "Median Diameter (cm)",
            "Min Diameter (cm)",
            "Max Diameter (cm)"
          ),
          Value = c(
            stats()$n_plots,
            stats()$n_individuals,
            stats()$n_species,
            stats()$n_families,
            stats()$mean_dbh,
            stats()$median_dbh,
            stats()$min_dbh,
            stats()$max_dbh
          )
        )

        readr::write_csv(summary_df, file)
      }
    )
  })
}


#' Detect column names from data
#'
#' @description
#' Maps standard column concepts (diameter, species, family, etc.) to actual
#' column names in the data, accounting for different output styles.
#'
#' @param data Data frame to analyze
#'
#' @return Named list with detected column names (or NULL if not found)
#' @keywords internal
#' @noRd
.detect_column_names <- function(data) {

  cols <- names(data)

  # Column name mappings (in order of preference)
  mappings <- list(
    diameter = c("stem_diameter", "dbh", "D", "diameter"),
    height = c("tree_height", "height", "H"),
    pom = c("height_of_stem_diameter", "pom", "POM"),
    species = c("tax_sp_level", "species", "tax_sp", "sp"),
    genus = c("tax_gen", "genus", "gen"),
    family = c("tax_fam", "family", "fam"),
    plot_name = c("plot_name", "plot", "plot_id"),
    tag = c("tag", "tree_tag", "tree_id"),
    census_date = c("census_date", "date", "year")
  )

  result <- list()

  for (concept in names(mappings)) {
    candidates <- mappings[[concept]]
    matched <- intersect(candidates, cols)
    result[[concept]] <- if (length(matched) > 0) matched[1] else NULL
  }

  return(result)
}


#' Compute basic statistics from individuals data
#'
#' @description
#' Calculates summary statistics using detected column names
#'
#' @param data Data frame with individuals
#' @param col_map Column mapping from .detect_column_names()
#'
#' @return Named list with statistics
#' @keywords internal
#' @noRd
.compute_basic_statistics <- function(data, col_map) {

  stats <- list(
    n_individuals = nrow(data),
    n_plots = NA,
    n_species = NA,
    n_families = NA,
    mean_dbh = NA,
    median_dbh = NA,
    min_dbh = NA,
    max_dbh = NA
  )

  # Number of plots
  if (!is.null(col_map$plot_name)) {
    stats$n_plots <- data %>%
      dplyr::distinct(.data[[col_map$plot_name]]) %>%
      nrow()
  }

  # Number of species
  if (!is.null(col_map$species)) {
    stats$n_species <- data %>%
      dplyr::filter(!is.na(.data[[col_map$species]])) %>%
      dplyr::distinct(.data[[col_map$species]]) %>%
      nrow()
  }

  # Number of families
  if (!is.null(col_map$family)) {
    stats$n_families <- data %>%
      dplyr::filter(!is.na(.data[[col_map$family]])) %>%
      dplyr::distinct(.data[[col_map$family]]) %>%
      nrow()
  }

  # Diameter statistics
  if (!is.null(col_map$diameter)) {
    diam_data <- data %>%
      dplyr::filter(!is.na(.data[[col_map$diameter]])) %>%
      dplyr::pull(.data[[col_map$diameter]])

    if (length(diam_data) > 0) {
      stats$mean_dbh <- mean(diam_data, na.rm = TRUE)
      stats$median_dbh <- median(diam_data, na.rm = TRUE)
      stats$min_dbh <- min(diam_data, na.rm = TRUE)
      stats$max_dbh <- max(diam_data, na.rm = TRUE)
    }
  }

  return(stats)
}
