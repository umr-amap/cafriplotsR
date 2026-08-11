# Step 3b — review the identifications a census table revises
#
# A census import overwrites nothing about an individual; it adds measurements.
# A revised determination is therefore a different kind of write, and one that
# destroys information rather than adding it. It gets its own step, with the
# herbarium evidence behind each existing identification shown next to the
# proposed one, because that evidence is what makes the decision.


#' UI for the taxon revision step
#'
#' @param id Module id.
#' @param i18n Translator object.
#' @return A `shiny::tagList`.
#' @export
mod_feat_step3b_taxon_revision_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(shiny::icon("dna"), " ", i18n$t("Review Revised Identifications")),
    shiny::p(
      class = "text-muted",
      i18n$t("Stems whose identification in the file differs from the database. Accepting a revision overwrites the determination held for that tree; the original field name is kept either way.")
    ),
    shiny::uiOutput(ns("summary_ui")),
    shiny::uiOutput(ns("bulk_ui")),
    DT::DTOutput(ns("revisions_table")),
    shiny::uiOutput(ns("footer_ui"))
  )
}


#' Server for the taxon revision step
#'
#' @param id Module id.
#' @param split_result Reactive returning the `census_split`.
#' @param con,con_taxa Reactives returning the two connections.
#' @param i18n Reactive translator.
#' @return Reactive returning the accepted revisions, or `NULL`.
#' @export
mod_feat_step3b_taxon_revision_server <- function(id, split_result, con,
                                                  con_taxa, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    revisions <- shiny::reactiveVal(NULL)

    shiny::observe({
      split <- split_result()
      if (is.null(split)) {
        revisions(NULL)
        return()
      }
      res <- tryCatch(
        collect_taxon_revisions(split, con = con(), con_taxa = con_taxa()),
        error = function(e) {
          cli::cli_alert_warning("Could not collect taxon revisions: {e$message}")
          NULL
        }
      )
      revisions(res)
    })

    # Decisions live in their own vector so redrawing the table cannot lose
    # them, and so bulk actions and per-row edits write to the same place
    decisions <- shiny::reactiveVal(character(0))
    shiny::observeEvent(revisions(), {
      rev <- revisions()
      decisions(if (is.null(rev) || nrow(rev) == 0) character(0) else rev$decision)
    })

    output$summary_ui <- shiny::renderUI({
      rev <- revisions()
      if (is.null(rev) || nrow(rev) == 0) {
        return(shiny::div(
          class = "alert alert-success",
          shiny::icon("check"), " ",
          i18n()$t("No identification in this file differs from the database.")
        ))
      }

      card <- function(n, label, colour) {
        shiny::column(3, shiny::div(
          style = sprintf(
            "background: %s; color: white; padding: 12px; border-radius: 8px; text-align: center;",
            colour),
          shiny::h4(n, style = "margin: 0; font-weight: bold;"),
          shiny::div(label, style = "font-size: 12px;")
        ))
      }

      shiny::tagList(
        shiny::fluidRow(
          card(sum(rev$evidence == "voucher"),
               i18n()$t("Specimen of this tree"), "#dc3545"),
          card(sum(rev$evidence == "collected_this_census"),
               i18n()$t("Collected this census"), "#fd7e14"),
          card(sum(rev$evidence == "reference"),
               i18n()$t("Reference specimen"), "#17a2b8"),
          card(sum(rev$evidence == "field_only"),
               i18n()$t("Field determination only"), "#6c757d")
        ),
        shiny::br(),
        if (any(rev$evidence == "voucher")) shiny::div(
          class = "alert alert-danger",
          shiny::icon("triangle-exclamation"), " ",
          i18n()$t("Some of these stems have a specimen collected from them. Revising the tree does not revise the specimen's own determination — do that in the specimen tools.")
        ),
        if (any(rev$evidence == "collected_this_census")) shiny::div(
          class = "alert alert-warning",
          shiny::icon("box-archive"), " ",
          i18n()$t("Some stems carry a herbarium number in this file with no specimen link yet. The voucher is not in the database — link it once it is registered.")
        ),
        if (any(rev$category == "precision_lost")) shiny::div(
          class = "alert alert-warning",
          shiny::icon("arrow-down-short-wide"), " ",
          i18n()$t("Some rows identify a stem less precisely than the database does. That is usually a data entry problem rather than a revision, so they default to keeping the database value.")
        )
      )
    })

    output$bulk_ui <- shiny::renderUI({
      rev <- revisions()
      if (is.null(rev) || nrow(rev) == 0) return(NULL)

      shiny::div(
        style = "margin: 10px 0;",
        shiny::strong(i18n()$t("Apply to all: ")), " ",
        shiny::actionButton(ns("accept_safe"),
                            i18n()$t("Accept those without a specimen"),
                            class = "btn-sm btn-outline-success"), " ",
        shiny::actionButton(ns("accept_all"), i18n()$t("Accept everything"),
                            class = "btn-sm btn-outline-warning"), " ",
        shiny::actionButton(ns("keep_all"), i18n()$t("Keep the database value"),
                            class = "btn-sm btn-outline-secondary")
      )
    })

    shiny::observeEvent(input$accept_safe, {
      rev <- revisions(); shiny::req(rev)
      decisions(ifelse(rev$evidence %in% c("voucher") |
                         rev$category == "precision_lost",
                       "keep_db", "accept_file"))
    })
    shiny::observeEvent(input$accept_all, {
      rev <- revisions(); shiny::req(rev)
      decisions(rep("accept_file", nrow(rev)))
    })
    shiny::observeEvent(input$keep_all, {
      rev <- revisions(); shiny::req(rev)
      decisions(rep("keep_db", nrow(rev)))
    })

    output$revisions_table <- DT::renderDT({
      rev <- revisions()
      if (is.null(rev) || nrow(rev) == 0) return(NULL)
      dec <- decisions()
      if (length(dec) != nrow(rev)) dec <- rev$decision

      DT::datatable(
        .census_revision_display(rev, dec, i18n()),
        rownames = FALSE, selection = "none",
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    output$footer_ui <- shiny::renderUI({
      rev <- revisions()
      if (is.null(rev) || nrow(rev) == 0) return(NULL)
      n <- sum(decisions() == "accept_file")

      shiny::div(
        class = if (n > 0) "alert alert-info" else "alert alert-secondary",
        sprintf(i18n()$t("%d of %d revision(s) will be applied."), n, nrow(rev))
      )
    })

    shiny::reactive({
      rev <- revisions()
      if (is.null(rev) || nrow(rev) == 0) return(NULL)
      dec <- decisions()
      if (length(dec) != nrow(rev)) dec <- rev$decision
      rev$decision <- dec
      out <- rev[dec == "accept_file", , drop = FALSE]
      if (nrow(out) == 0) NULL else out
    })
  })
}


#' Render the revision table for display
#'
#' Kept out of the server so the column set and the wording can be tested
#' without a Shiny session.
#'
#' @param rev Frame from [.classify_taxon_revisions()].
#' @param decisions Character vector, one decision per row.
#' @param i18n Optional translator.
#' @return Data frame ready for `DT::datatable()`.
#' @export
.census_revision_display <- function(rev, decisions = NULL, i18n = NULL) {

  t <- function(x) if (!is.null(i18n)) i18n$t(x) else x

  if (is.null(rev) || nrow(rev) == 0) {
    return(data.frame(
      plot = character(0), tag = character(0), current = character(0),
      proposed = character(0), evidence = character(0),
      herbarium = character(0), decision = character(0),
      stringsAsFactors = FALSE
    ))
  }
  if (is.null(decisions) || length(decisions) != nrow(rev)) {
    decisions <- rev$decision
  }

  labels <- c(
    voucher               = t("Specimen of this tree"),
    collected_this_census = t("Collected this census"),
    reference             = t("Reference specimen"),
    field_only            = t("Field only")
  )
  note <- c(
    identification_gained = t(" (was unidentified)"),
    precision_lost        = t(" (less precise!)"),
    revision              = ""
  )

  data.frame(
    plot      = rev$plot_name,
    tag       = rev$tag,
    current   = rev$name_db,
    proposed  = paste0(rev$name_file, unname(note[rev$category])),
    evidence  = unname(labels[rev$evidence]),
    herbarium = ifelse(is.na(rev$herbarium_nbe_char), "", rev$herbarium_nbe_char),
    decision  = ifelse(decisions == "accept_file", t("accept"), t("keep database")),
    stringsAsFactors = FALSE
  )
}
