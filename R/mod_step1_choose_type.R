# Import Wizard - Step 1: Choose Import Type
#
# Module for selecting whether to import plot metadata or individual tree data

#' Step 1 Module: Choose Import Type - UI
#'
#' @param id Module namespace ID
#' @keywords internal
mod_step1_choose_type_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("clipboard-list"),
      "Step 1: Choose Import Type",
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      "Select the type of data you want to import. Plot metadata must be imported before individual tree data.",
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Import type selection cards
    shiny::fluidRow(
      # Plot Metadata card
      shiny::column(
        6,
        shiny::div(
          id = ns("card_plots"),
          class = "import-card",
          onclick = sprintf("Shiny.setInputValue('%s', 'plots', {priority: 'event'})", ns("import_type_click")),

          shiny::h4(
            shiny::icon("map-marked-alt", style = "color: #007bff; font-size: 32px;"),
            br(),
            "Plot Metadata"
          ),

          shiny::p(
            "Import plot locations, census dates, methods, and associated metadata.",
            style = "color: #6c757d;"
          ),

          shiny::tags$ul(
            shiny::tags$li("Plot names and locations (coordinates)"),
            shiny::tags$li("Census dates and methods"),
            shiny::tags$li("Team members and investigators"),
            shiny::tags$li("Plot characteristics")
          ),

          shiny::div(
            style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;",
            shiny::strong("Required fields:"),
            shiny::tags$code("plot_name, method, country")
          )
        )
      ),

      # Individual Trees card
      shiny::column(
        6,
        shiny::div(
          id = ns("card_individuals"),
          class = "import-card",
          onclick = sprintf("Shiny.setInputValue('%s', 'individuals', {priority: 'event'})", ns("import_type_click")),

          shiny::h4(
            shiny::icon("tree", style = "color: #28a745; font-size: 32px;"),
            br(),
            "Individual Trees"
          ),

          shiny::p(
            "Import tree measurements, species identifications, and trait data.",
            style = "color: #6c757d;"
          ),

          shiny::tags$ul(
            shiny::tags$li("Tree identifiers (tags)"),
            shiny::tags$li("Species names (taxonomic data)"),
            shiny::tags$li("Measurements (DBH, height, etc.)"),
            shiny::tags$li("Trait values")
          ),

          shiny::div(
            style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;",
            shiny::strong("Required fields:"),
            shiny::tags$code("plot_name, idtax_n")
          )
        )
      )
    ),

    # Selected type indicator
    shiny::uiOutput(ns("selection_indicator")),

    # Info box
    shiny::div(
      class = "alert alert-info",
      style = "margin-top: 30px;",
      shiny::icon("info-circle"),
      shiny::strong(" Important: "),
      "You must import plot metadata before importing individual tree data. ",
      "Individual trees are linked to plots via ",
      shiny::tags$code("plot_name"),
      "."
    )
  )
}


#' Step 1 Module: Choose Import Type - Server
#'
#' @param id Module namespace ID
#' @return Reactive value containing selected import type ("plots" or "individuals")
#' @keywords internal
mod_step1_choose_type_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # Selected import type
    selected_type <- shiny::reactiveVal(NULL)

    # Handle card clicks
    shiny::observeEvent(input$import_type_click, {
      selected_type(input$import_type_click)

      # Update card styling via JavaScript
      shinyjs::runjs(sprintf("
        $('.import-card').removeClass('selected');
        $('#%s').addClass('selected');
      ", session$ns(paste0("card_", input$import_type_click))))

      # Show notification
      type_label <- if (input$import_type_click == "plots") {
        "Plot Metadata"
      } else {
        "Individual Trees"
      }

      shiny::showNotification(
        paste("Selected:", type_label),
        type = "message",
        duration = 2
      )
    })

    # Selection indicator
    output$selection_indicator <- shiny::renderUI({
      shiny::req(selected_type())

      type_info <- if (selected_type() == "plots") {
        list(
          icon = "map-marked-alt",
          label = "Plot Metadata",
          color = "#007bff"
        )
      } else {
        list(
          icon = "tree",
          label = "Individual Trees",
          color = "#28a745"
        )
      }

      shiny::div(
        class = "alert",
        style = sprintf(
          "margin-top: 30px; background-color: %s; color: white; border-color: %s;",
          type_info$color,
          type_info$color
        ),
        shiny::h4(
          shiny::icon(type_info$icon),
          sprintf(" Selected: %s", type_info$label),
          style = "margin: 0;"
        )
      )
    })

    # Return selected type
    return(selected_type)
  })
}
