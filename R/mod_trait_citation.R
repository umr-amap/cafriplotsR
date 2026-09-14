# =============================================================================
# Taxa Traits Import - Citation Module
#
# Picks the citation (database / dataset source) the imported measurements
# come from, or creates it. A new citation is written to table_citations
# straight away, so it exists before the trait import runs and can be reused
# by later imports.
#
# The choice is optional: measurements can be imported without a citation.
# =============================================================================

#' Trait Citation Module - UI
#' @param id Module namespace ID
#' @keywords internal
#' @export
mod_trait_citation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shiny::uiOutput(ns("citation_selector")),
    shiny::uiOutput(ns("citation_details"))
  )
}


#' Trait Citation Module - Server
#'
#' @param id Module namespace ID
#' @param pool Reactive returning database connection pool
#' @param i18n Reactive returning translator
#'
#' @return Reactive list: `id_citation` (integer, NA when none is selected),
#'   `citation` (the selected row of `table_citations`, or NULL).
#' @keywords internal
#' @export
mod_trait_citation_server <- function(id, pool, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Refresh trigger (incremented after a new citation is created inline)
    citation_refresh <- shiny::reactiveVal(0)

    # Fetch existing citations from DB
    citations_df <- shiny::reactive({
      citation_refresh()
      shiny::req(pool())
      tryCatch({
        actual_con <- if (inherits(pool(), "Pool")) pool::poolCheckout(pool()) else pool()
        on.exit(if (inherits(pool(), "Pool")) pool::poolReturn(actual_con), add = TRUE)
        DBI::dbGetQuery(actual_con,
          "SELECT id_citation, citation_key, authors, year, title, journal,
                  doi, url, dataset_name
           FROM table_citations ORDER BY citation_key")
      }, error = function(e) {
        message("Could not fetch table_citations: ", e$message)
        data.frame(id_citation = integer(), citation_key = character(),
                   authors = character(), year = integer(), title = character(),
                   journal = character(), doi = character(), url = character(),
                   dataset_name = character(), stringsAsFactors = FALSE)
      })
    })

    # Named vector for the dropdown: label -> id_citation
    citation_choices <- shiny::reactive({
      df <- citations_df()
      if (nrow(df) == 0) return(c("-- No citations in database --" = ""))
      c("-- None --" = "",
        setNames(as.character(df$id_citation), .citation_labels(df)))
    })

    # ---- Header ----
    output$header <- shiny::renderUI({
      shiny::tagList(
        shiny::h4(shiny::icon("book", style = "color: #20c997;"),
                  i18n()$t("Citation (database/dataset source)")),
        shiny::p(
          i18n()$t("Select the citation for the database or dataset this import comes from. This is distinct from the 'reference' field which records the original source of each measurement."),
          style = "color: #6c757d; margin-bottom: 10px;"
        ),
        shiny::p(
          i18n()$t("Optional: leave it on '-- None --' to import measurements without a citation."),
          style = "color: #6c757d; font-size: 12px;"
        )
      )
    })

    # ---- Selector ----
    # Reads the input it renders, so the rendered HTML keeps the current
    # choice when the wizard rebuilds this step.
    output$citation_selector <- shiny::renderUI({
      choices <- citation_choices()
      current <- input$selected_citation
      selected <- if (!is.null(current) && current %in% choices) current else ""

      shiny::div(
        style = "padding: 12px; background: #f0fff4; border-left: 4px solid #20c997; border-radius: 4px; margin-bottom: 15px;",
        shiny::fluidRow(
          shiny::column(8,
            shiny::selectizeInput(
              ns("selected_citation"),
              label = NULL,
              choices = choices,
              selected = selected,
              width = "100%",
              options = list(placeholder = i18n()$t("Search a citation..."))
            )
          ),
          shiny::column(4,
            shiny::actionButton(
              ns("btn_add_citation"),
              shiny::tagList(shiny::icon("plus"), i18n()$t("New citation")),
              class = "btn-outline-success btn-sm",
              style = "width: 100%;"
            )
          )
        )
      )
    })

    # Resolved id_citation (integer or NA)
    selected_id_citation <- shiny::reactive({
      val <- input$selected_citation
      if (length(val) != 1 || is.na(val) || !nzchar(val)) return(NA_integer_)
      suppressWarnings(as.integer(val))
    })

    selected_citation <- shiny::reactive({
      id <- selected_id_citation()
      if (is.na(id)) return(NULL)
      df <- citations_df()
      row <- df[df$id_citation == id, , drop = FALSE]
      if (nrow(row) == 0) NULL else row[1, , drop = FALSE]
    })

    # ---- What the selected citation says ----
    output$citation_details <- shiny::renderUI({
      cit <- selected_citation()
      if (is.null(cit)) {
        return(shiny::div(
          style = "padding: 10px; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
          shiny::icon("exclamation-triangle", style = "color: #856404;"),
          shiny::tags$small(
            paste0(" ", i18n()$t("No citation selected: the measurements will be imported without a link to a source.")),
            style = "color: #856404;")
        ))
      }

      field <- function(label, value) {
        if (is.null(value) || is.na(value) || !nzchar(trimws(as.character(value))))
          return(NULL)
        shiny::tags$li(shiny::tags$strong(paste0(label, ": ")), as.character(value))
      }

      shiny::div(
        style = "padding: 12px; background: #f8f9fa; border-left: 4px solid #20c997; border-radius: 4px;",
        shiny::tags$strong(cit$citation_key),
        shiny::tags$ul(
          style = "margin: 8px 0 0 0; color: #495057;",
          field(i18n()$t("Authors"), cit$authors),
          field(i18n()$t("Year"), cit$year),
          field(i18n()$t("Title"), cit$title),
          field(i18n()$t("Journal / Publisher"), cit$journal),
          field(i18n()$t("Dataset name"), cit$dataset_name),
          field("DOI", cit$doi),
          field("URL", cit$url)
        )
      )
    })

    # ---- New citation modal ----
    shiny::observeEvent(input$btn_add_citation, {
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("plus-circle"),
                               paste0(" ", i18n()$t("Create New Citation"))),
        size = "l",
        shiny::p(
          i18n()$t("The citation is saved to the database now, before the import, and can then be reused by other imports."),
          style = "color: #6c757d; margin-bottom: 15px;"
        ),
        shiny::fluidRow(
          shiny::column(6,
            shiny::textInput(ns("new_cit_key"),
              paste0(i18n()$t("Citation key"), " *"),
              placeholder = "e.g. TRY_2020, Dauby2022"),
            shiny::tags$small(
              i18n()$t("Short unique identifier — use only letters, digits, underscores"),
              style = "color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;"
            ),
            shiny::textInput(ns("new_cit_authors"),
              i18n()$t("Authors"),
              placeholder = "Last F., Last2 F2., ..."),
            shiny::numericInput(ns("new_cit_year"),
              i18n()$t("Year"),
              value = as.integer(format(Sys.Date(), "%Y")),
              min = 1800, max = 2100, step = 1),
            shiny::textInput(ns("new_cit_dataset"),
              i18n()$t("Dataset name"),
              placeholder = "e.g. TRY, BIEN, CoForTraits")
          ),
          shiny::column(6,
            shiny::textAreaInput(ns("new_cit_title"),
              paste0(i18n()$t("Title"), " *"),
              placeholder = i18n()$t("Full title of the article or dataset"),
              rows = 3),
            shiny::textInput(ns("new_cit_journal"),
              i18n()$t("Journal / Publisher"),
              placeholder = "e.g. Scientific Data, CIRAD Dataverse"),
            shiny::textInput(ns("new_cit_doi"),
              "DOI",
              placeholder = "10.XXXX/..."),
            shiny::textInput(ns("new_cit_url"),
              "URL",
              placeholder = "https://...")
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton(i18n()$t("Cancel")),
          shiny::actionButton(ns("confirm_add_citation"),
            shiny::tagList(shiny::icon("check"), paste0(" ", i18n()$t("Save citation"))),
            class = "btn-success")
        ),
        easyClose = FALSE
      ))
    })

    shiny::observeEvent(input$confirm_add_citation, {
      key   <- trimws(input$new_cit_key %||% "")
      title <- trimws(input$new_cit_title %||% "")

      if (nchar(key) == 0 || nchar(title) == 0) {
        shiny::showNotification(
          i18n()$t("Citation key and title are required."),
          type = "warning"
        )
        return()
      }

      # add_citation() skips an existing key with a warning, which would leave
      # the user selecting someone else's citation without noticing.
      if (key %in% citations_df()$citation_key) {
        shiny::showNotification(
          sprintf(i18n()$t("Citation key '%s' already exists. Select it in the list or use another key."), key),
          type = "warning", duration = 10
        )
        return()
      }

      tryCatch({
        new_row <- data.frame(
          citation_key = key,
          authors      = trimws(input$new_cit_authors %||% ""),
          year         = as.integer(input$new_cit_year),
          title        = title,
          journal      = trimws(input$new_cit_journal %||% ""),
          doi          = trimws(input$new_cit_doi %||% ""),
          url          = trimws(input$new_cit_url %||% ""),
          dataset_name = trimws(input$new_cit_dataset %||% ""),
          stringsAsFactors = FALSE
        )
        add_citation(new_row, con = pool(), interactive = FALSE)
        shiny::removeModal()
        shiny::showNotification(
          sprintf(i18n()$t("Citation '%s' created"), key),
          type = "message"
        )
        citation_refresh(citation_refresh() + 1)

        # Auto-select the newly created citation
        new_id <- citations_df()$id_citation[citations_df()$citation_key == key]
        if (length(new_id) == 1) {
          shiny::updateSelectizeInput(session, "selected_citation",
            choices = citation_choices(),
            selected = as.character(new_id)
          )
        }
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("Error saving citation:"), e$message),
          type = "error"
        )
      })
    })

    shiny::reactive({
      list(
        id_citation = selected_id_citation(),
        citation = selected_citation()
      )
    })
  })
}


# ---- Helper: dropdown labels ----
#' One-line label for each citation
#'
#' @param df Data frame with `citation_key`, `authors`, `year` and
#'   `dataset_name`.
#' @return Character vector, one label per row.
#' @keywords internal
.citation_labels <- function(df) {
  vapply(seq_len(nrow(df)), function(i) {
    authors <- df$authors[i]
    auth_short <- if (!is.na(authors) && nchar(authors) > 0) {
      paste0(trimws(strsplit(authors, ",")[[1]][1]), " et al.")
    } else ""
    yr <- if (!is.na(df$year[i])) paste0(" (", df$year[i], ")") else ""
    ds <- df$dataset_name[i]
    ds_str <- if (!is.na(ds) && nchar(ds) > 0) paste0(" [", ds, "]") else ""
    paste0(df$citation_key[i], " — ", auth_short, yr, ds_str)
  }, character(1))
}
