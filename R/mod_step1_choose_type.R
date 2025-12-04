# Import Wizard - Step 1: Choose Import Type
#
# Module for selecting whether to import plot metadata or individual tree data

#' Step 1 Module: Choose Import Type - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
mod_step1_choose_type_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("clipboard-list"),
      i18n$t("Step 1: Choose Import Type"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Select the type of data you want to import."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 20px;"
    ),

    # Important messages with checkboxes at the top
    shiny::div(
      class = "alert alert-warning",
      style = "margin-bottom: 30px; background-color: #fff3cd; border-left: 4px solid #ffc107;",
      shiny::h5(
        shiny::icon("exclamation-triangle"),
        shiny::strong(paste0(" ", i18n$t("Important Requirements"))),
        style = "margin-top: 0; color: #856404;"
      ),
      shiny::p(
        i18n$t("Please read and confirm you understand these requirements before proceeding:"),
        style = "color: #856404; margin-bottom: 15px;"
      ),

      shiny::tags$style(shiny::HTML("
        .custom-checkbox-wrapper {
          display: flex;
          align-items: flex-start;
          margin-bottom: 15px;
          width: 100%;
        }
        .custom-checkbox-wrapper input[type='checkbox'] {
          margin-top: 3px;
          margin-right: 10px;
          flex-shrink: 0;
        }
        .custom-checkbox-wrapper .checkbox-text {
          flex: 1;
          line-height: 1.5;
          color: #856404;
        }
      ")),

      shiny::div(
        style = "margin-left: 10px;",
        shiny::tags$div(
          class = "custom-checkbox-wrapper",
          shiny::tags$input(
            type = "checkbox",
            id = ns("confirm_plot_first"),
            value = FALSE
          ),
          shiny::tags$div(
            class = "checkbox-text",
            shiny::HTML(
              paste0(
                "<strong>", i18n$t("Plot/inventory metadata must be imported before individual trees/stems:"), "</strong> ",
                i18n$t("Always import plot metadata first, before importing individual tree data. Individual trees are linked to plots via"),
                " <code>plot_name</code>. ",
                i18n$t("If unsure how to structure your dataset, see the templates provided in the Plot metadata import section.")
              )
            )
          )
        ),

        shiny::tags$div(
          class = "custom-checkbox-wrapper",
          shiny::tags$input(
            type = "checkbox",
            id = ns("confirm_taxonomy"),
            value = FALSE
          ),
          shiny::tags$div(
            class = "checkbox-text",
            shiny::HTML(
              paste0(
                "<strong>", i18n$t("Taxonomic standardization required for trees/stems data:"), "</strong> ",
                i18n$t("Before importing individual tree data, you must standardize taxonomic information. Use either the automatic standardization function or the interactive Shiny app for semi-automatic taxonomic matching. See the dedicated vignette for detailed instructions on taxonomic standardization.")
              )
            )
          )
        )
      )
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
            i18n$t("Plot Metadata")
          ),

          shiny::p(
            i18n$t("Import plot locations, census dates, methods, and associated metadata."),
            style = "color: #6c757d;"
          ),

          shiny::tags$ul(
            shiny::tags$li(i18n$t("Plot names and locations (coordinates)")),
            shiny::tags$li(i18n$t("Census dates and methods")),
            shiny::tags$li(i18n$t("Team members and investigators")),
            shiny::tags$li(i18n$t("Plot characteristics"))
          ),

          shiny::div(
            style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;",
            shiny::strong(paste0(i18n$t("Required fields"), ":")),
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
            i18n$t("Individual Trees")
          ),

          shiny::p(
            i18n$t("Import tree measurements, species identifications, and trait data."),
            style = "color: #6c757d;"
          ),

          shiny::tags$ul(
            shiny::tags$li(i18n$t("Tree identifiers (tags)")),
            shiny::tags$li(i18n$t("Species names (taxonomic data)")),
            shiny::tags$li(i18n$t("Measurements (DBH, height, etc.)")),
            shiny::tags$li(i18n$t("Trait values"))
          ),

          shiny::div(
            style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;",
            shiny::strong(paste0(i18n$t("Required fields"), ":")),
            shiny::tags$code("plot_name, idtax_n")
          )
        )
      )
    ),

    # Selected type indicator
    shiny::uiOutput(ns("selection_indicator")),

    # Requirements validation message
    shiny::uiOutput(ns("requirements_validation"))
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

    # Check if requirements are confirmed
    requirements_confirmed <- shiny::reactive({
      # For plot metadata, no special requirements needed
      if (is.null(selected_type()) || selected_type() == "plots") {
        return(TRUE)
      }

      # For individual trees, both checkboxes must be checked
      if (selected_type() == "individuals") {
        return(input$confirm_plot_first && input$confirm_taxonomy)
      }

      return(FALSE)
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

    # Requirements validation message
    output$requirements_validation <- shiny::renderUI({
      shiny::req(selected_type())

      # Only show for individual trees
      if (selected_type() == "individuals") {
        if (!requirements_confirmed()) {
          shiny::div(
            class = "alert alert-danger",
            style = "margin-top: 20px;",
            shiny::icon("exclamation-circle"),
            shiny::strong(" Action Required: "),
            "Please confirm that you have read and understood both requirements above by checking the boxes before proceeding."
          )
        } else {
          shiny::div(
            class = "alert alert-success",
            style = "margin-top: 20px;",
            shiny::icon("check-circle"),
            shiny::strong(" Requirements Confirmed: "),
            "You may now proceed to the next step."
          )
        }
      }
    })

    # Return selected type only if requirements are met
    validated_selection <- shiny::reactive({
      shiny::req(selected_type())

      if (requirements_confirmed()) {
        return(selected_type())
      } else {
        return(NULL)
      }
    })

    # Return validated selection
    return(validated_selection)
  })
}
