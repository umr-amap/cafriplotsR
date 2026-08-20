# Shiny App: Data Update
#
# Interactive front end for update_records(), with two sections:
#   - Plot metadata     -> data_liste_plots + its features (data_liste_sub_plots)
#   - Individual data   -> data_individuals + its features (data_traits_measures)
#
# Main function: launch_data_update_app()

#' Launch the Data Update App
#'
#' Launches an interactive Shiny app for correcting plot metadata and individual
#' data one record at a time. It is the user-friendly counterpart to
#' \code{\link{update_records}}, which is powerful but expects the caller to
#' already know which table a value lives in.
#'
#' The app has two sections:
#'
#' \itemize{
#'   \item \strong{Plot metadata} - pick a plot, edit the columns stored
#'         directly in \code{data_liste_plots} (including the \code{method} and
#'         \code{country} lookups, offered as dropdowns), and edit its features.
#'   \item \strong{Individual data} - find an individual by plot and tag or by
#'         \code{id_n}, edit the columns of \code{data_individuals}, change its
#'         identification through an embedded taxonomic search, and edit its
#'         trait measurements.
#' }
#'
#' \strong{Why features need care.} Many columns of an extracted plot or
#' individual table are not columns of that record at all. Plot features are
#' rows of \code{data_liste_sub_plots}; individual features are rows of
#' \code{data_traits_measures}. Worse, one extracted column can be the
#' \emph{aggregate} of several such rows - the mean of a trait measured at
#' three censuses, or the concatenated names of everyone recorded as
#' \code{additional_people}. Writing back to that single value is meaningless,
#' which is why \code{update_records()} refuses it.
#'
#' The app therefore never edits an aggregate. For every feature it shows how
#' many records back it, what the extracted table would display, and how that
#' display was computed; the editable inputs are the underlying records, each
#' labelled with its own id and its census or subplot context.
#'
#' Only existing records can be edited. Adding or deleting measurements is done
#' with the feature wizard (\code{\link{launch_feature_wizard}}) and the
#' \code{safe_delete_*} functions.
#'
#' Every write goes through \code{detect_direct_changes()} and
#' \code{execute_direct_updates()}, so stored values are re-read immediately
#' before writing, only genuine differences are written, and records are backed
#' up to their follow-up table where one exists.
#'
#' @param lang Character. Initial UI language: \code{"en"} or \code{"fr"}.
#'   Default: \code{"fr"}.
#'
#' @return Launches a Shiny app (does not return until the app closes).
#'
#' @examples
#' \dontrun{
#' launch_data_update_app()
#' launch_data_update_app(lang = "en")
#' }
#'
#' @seealso \code{\link{update_records}},
#'   \code{\link{query_plot_features}},
#'   \code{\link{query_individual_features}},
#'   \code{\link{launch_specimen_identification_app}},
#'   \code{\link{launch_feature_wizard}}
#'
#' @export
launch_data_update_app <- function(lang = "fr") {

  lang <- match.arg(lang, c("fr", "en"))

  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file(
      "translations/translation.json",
      package = "CafriplotsR"
    )
  )
  i18n$set_translation_language(lang)

  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(spin = "fading-circle"),

    shiny::tags$style(shiny::HTML("
      .section-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 18px;
        border-radius: 8px;
        margin-bottom: 18px;
      }
      .nav-tabs > li > a { font-weight: 500; }
      .well { background: #fbfbfd; }
      .panel-default .form-group { margin-bottom: 8px; }
    ")),

    shiny::fluidRow(
      shiny::column(8, shiny::h2(shiny::textOutput("app_title"))),
      shiny::column(
        4,
        style = "text-align: right; padding-top: 12px;",
        mod_language_toggle_ui("lang_toggle")
      )
    ),
    shiny::hr(),

    shiny::conditionalPanel(
      condition = "!output.authenticated",
      mod_database_login_ui("login")
    ),

    shiny::conditionalPanel(
      condition = "output.authenticated",

      shiny::div(
        class = "section-header",
        shiny::h3(shiny::icon("pen-to-square"), " ",
                  shiny::textOutput("header_title", inline = TRUE)),
        shiny::p(shiny::textOutput("header_desc"))
      ),

      shiny::tabsetPanel(
        id = "section",
        shiny::tabPanel(
          title = i18n$t("Plot metadata"),
          value = "plots",
          shiny::br(),
          mod_update_record_ui("plots", entity = "plot", i18n = i18n)
        ),
        shiny::tabPanel(
          title = i18n$t("Individual data"),
          value = "individuals",
          shiny::br(),
          mod_update_record_ui("individuals", entity = "individual", i18n = i18n)
        )
      )
    )
  )

  server <- function(input, output, session) {

    current_lang <- mod_language_toggle_server("lang_toggle", initial = lang)
    i18n_reactive <- shiny::reactive({
      sel <- current_lang()
      i18n$set_translation_language(sel)
      i18n
    })

    login_result <- mod_database_login_server("login")
    pool_main <- login_result$pool_main
    pool_taxa <- login_result$pool_taxa
    authenticated <- login_result$authenticated

    shiny::observe({
      l <- login_result$language()
      shiny::req(l)
      shiny::updateRadioButtons(session, "lang_toggle-language", selected = l)
    })

    output$authenticated <- shiny::reactive({ authenticated() })
    shiny::outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

    output$app_title <- shiny::renderText({
      i18n_reactive()$t("Data Update")
    })
    output$header_title <- shiny::renderText({
      i18n_reactive()$t("Update plot metadata and individual data")
    })
    output$header_desc <- shiny::renderText({
      i18n_reactive()$t("Correct stored values one record at a time. Columns that are in fact features are resolved down to the records behind them, so an aggregated value is never written back as if it were a single field.")
    })

    # Both section modules are registered exactly once, the first time the user
    # authenticates - re-registering them would duplicate every observer.
    modules_started <- shiny::reactiveVal(FALSE)
    shiny::observe({
      shiny::req(authenticated() == TRUE, !modules_started())
      modules_started(TRUE)
      mod_update_record_server(
        "plots", entity = "plot",
        pool_main = pool_main, pool_taxa = pool_taxa, i18n = i18n_reactive
      )
      mod_update_record_server(
        "individuals", entity = "individual",
        pool_main = pool_main, pool_taxa = pool_taxa, i18n = i18n_reactive
      )
    })

    session$onSessionEnded(function() {
      tryCatch(cleanup_connections(),
               error = function(e) cli::cli_alert_warning(
                 "Failed to cleanup connections: {e$message}"))
      shiny::stopApp()
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
