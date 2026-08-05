# Specimen Identification App - Manual mode
#
# Workflow:
#   1. User searches a specimen (by id_specimen OR collector+number).
#   2. If multiple matches, user picks one row.
#   3. Module shows current specimen values.
#   4. User searches and selects a new accepted taxon (idtax_n) via an
#      embedded mini taxa search.
#   5. User optionally edits detd/detm/dety/detby/detvalue/colnbr/suffix.
#   6. User optionally edits the other specimen fields (collection date,
#      locality, coordinates, notes). Those inputs are pre-filled with the
#      current values and are absolute: clearing one clears the field.
#   7. Module shows a diff (current vs new) and a Confirm button.
#   8. On confirm, update_ident_specimens() is called with
#      ask_before_update=FALSE for the identification part, and
#      update_specimen_fields() for the remaining fields.

# Non-identification specimen fields editable in the manual pane, mapped to
# the type of shiny input used for them. Names must exist both in the
# `specimens` table and in .specimen_editable_fields().
.SPECID_MANUAL_FIELDS <- c(
  coly        = "numeric",
  colm        = "numeric",
  cold        = "numeric",
  locality    = "text",
  country     = "text",
  ddlat       = "numeric",
  ddlon       = "numeric",
  add_col     = "text",
  description = "text"
)

#' Manual mode UI
#' @param id Module id
#' @param i18n Translator
#' @keywords internal
#' @export
mod_specid_manual_ui <- function(id, i18n) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # --- 1a. Resolve collector ---
    shiny::wellPanel(
      shiny::h4(shiny::icon("user"), " ",
                i18n$t("1a. Resolve collector (skip if using id_specimen)")),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::textInput(ns("collector"), i18n$t("Collector name"),
                           placeholder = "e.g. Dauby")
        ),
        shiny::column(
          3, style = "padding-top: 25px;",
          shiny::actionButton(ns("resolve_collector"),
                              shiny::tagList(shiny::icon("search"), " ",
                                             i18n$t("Find collector")),
                              class = "btn-info")
        )
      ),
      shiny::uiOutput(ns("collector_resolve_info")),
      DT::DTOutput(ns("collector_candidates_tbl")),
      shiny::uiOutput(ns("collector_picked"))
    ),

    # --- 1b. Find specimen ---
    shiny::wellPanel(
      shiny::h4(shiny::icon("search"), " ",
                i18n$t("1b. Find the specimen")),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::numericInput(ns("id_specimen"), i18n$t("id_specimen (direct)"),
                              value = NA, min = 1)
        ),
        shiny::column(
          4,
          shiny::numericInput(ns("number"),
                              i18n$t("Collector number (uses resolved collector)"),
                              value = NA, min = 0)
        )
      ),
      shiny::actionButton(ns("search_speci"),
                          shiny::tagList(shiny::icon("search"), " ", i18n$t("Search specimen")),
                          class = "btn-primary"),
      shiny::br(), shiny::br(),
      shiny::uiOutput(ns("speci_results_info")),
      DT::DTOutput(ns("speci_results_tbl"))
    ),

    # --- 2. Current values ---
    shiny::conditionalPanel(
      condition = sprintf("output['%s']", ns("has_selection")),
      shiny::wellPanel(
        shiny::h4(shiny::icon("info-circle"), " ", i18n$t("2. Current values")),
        shiny::uiOutput(ns("current_card"))
      ),

      # --- 2b. Linked individuals summary ---
      shiny::wellPanel(
        shiny::h4(shiny::icon("link"), " ",
                  i18n$t("2b. Individuals linked to this specimen")),
        shiny::uiOutput(ns("links_summary_info")),
        DT::DTOutput(ns("links_summary_tbl"))
      ),

      # --- 3. Pick new taxon (reuses mod_taxa_search from launch_taxo_backbone_app) ---
      shiny::wellPanel(
        shiny::h4(shiny::icon("seedling"), " ",
                  i18n$t("3. Pick new identification (optional)")),
        mod_taxa_search_ui(ns("taxa_pick"))
      ),

      # --- 4. Determination fields ---
      shiny::wellPanel(
        shiny::h4(shiny::icon("calendar-day"), " ",
                  i18n$t("4. Determination & specimen metadata (optional)")),
        shiny::tags$p(
          class = "text-muted",
          shiny::tags$small(
            i18n$t("Leave a field empty to keep its current value.")
          )
        ),
        shiny::fluidRow(
          shiny::column(2, shiny::numericInput(ns("new_dety"), i18n$t("Det year"),
                                               value = NA, min = 1700, max = 2200)),
          shiny::column(2, shiny::numericInput(ns("new_detm"), i18n$t("Det month"),
                                               value = NA, min = 1, max = 12)),
          shiny::column(2, shiny::numericInput(ns("new_detd"), i18n$t("Det day"),
                                               value = NA, min = 1, max = 31)),
          shiny::column(3, shiny::textInput(ns("new_detby"), i18n$t("Determined by"))),
          shiny::column(3, shiny::textInput(ns("new_detvalue"), i18n$t("Det value (label)")))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::textInput(ns("new_colnbr"), i18n$t("New collector number"))),
          shiny::column(4, shiny::textInput(ns("new_suffix"), i18n$t("New suffix")))
        )
      ),

      # --- 4b. Other specimen fields (collection, locality, notes) ---
      shiny::wellPanel(
        shiny::h4(shiny::icon("map-marker-alt"), " ",
                  i18n$t("4b. Collection, locality & notes (optional)")),
        shiny::tags$p(
          class = "text-muted",
          shiny::tags$small(
            i18n$t("These fields are pre-filled with the current values. Edit them directly - clearing a field will erase its value in the database.")
          )
        ),
        shiny::fluidRow(
          shiny::column(2, shiny::numericInput(ns("new_coly"), i18n$t("Collection year"),
                                               value = NA, min = 1700, max = 2200)),
          shiny::column(2, shiny::numericInput(ns("new_colm"), i18n$t("Collection month"),
                                               value = NA, min = 1, max = 12)),
          shiny::column(2, shiny::numericInput(ns("new_cold"), i18n$t("Collection day"),
                                               value = NA, min = 1, max = 31)),
          shiny::column(6, shiny::textInput(ns("new_add_col"),
                                            i18n$t("Additional collectors")))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(ns("new_locality"), i18n$t("Locality"))),
          shiny::column(6, shiny::textInput(ns("new_country"), i18n$t("Country")))
        ),
        shiny::fluidRow(
          shiny::column(3, shiny::numericInput(ns("new_ddlat"), i18n$t("Latitude (ddlat)"),
                                               value = NA, min = -90, max = 90)),
          shiny::column(3, shiny::numericInput(ns("new_ddlon"), i18n$t("Longitude (ddlon)"),
                                               value = NA, min = -180, max = 180))
        ),
        shiny::fluidRow(
          shiny::column(12, shiny::textAreaInput(ns("new_description"),
                                                 i18n$t("Description / notes"),
                                                 width = "100%", rows = 3))
        ),
        shiny::actionLink(
          ns("reset_fields"),
          label = shiny::tagList(shiny::icon("undo"), " ",
                                 i18n$t("Reset to current values"))
        )
      ),

      # --- 5. Preview + apply ---
      shiny::wellPanel(
        shiny::h4(shiny::icon("eye"), " ", i18n$t("5. Preview & apply")),
        shiny::actionButton(ns("preview_btn"),
                            shiny::tagList(shiny::icon("eye"), " ", i18n$t("Preview changes")),
                            class = "btn-secondary"),
        shiny::actionButton(ns("apply_btn"),
                            shiny::tagList(shiny::icon("save"), " ",
                                           i18n$t("Apply update")),
                            class = "btn-success",
                            style = "margin-left: 10px;"),
        shiny::br(), shiny::br(),
        shiny::uiOutput(ns("diff_ui")),
        shiny::uiOutput(ns("apply_result"))
      )
    )
  )
}

#' Manual mode server
#' @param id Module id
#' @param pool_main Reactive main DB pool
#' @param pool_taxa Reactive taxa DB pool
#' @param i18n Reactive translator
#' @param active Reactive logical, TRUE when this mode is active
#' @keywords internal
#' @export
mod_specid_manual_server <- function(id, pool_main, pool_taxa, i18n,
                                     active = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    speci_hits         <- shiny::reactiveVal(NULL)
    selected_speci     <- shiny::reactiveVal(NULL)
    diff_tbl           <- shiny::reactiveVal(NULL)
    apply_status       <- shiny::reactiveVal(NULL)
    taxa_reset_counter <- shiny::reactiveVal(0)

    # Bump reset counter whenever the user picks a different specimen so that
    # any previously-selected taxon is cleared.
    shiny::observeEvent(selected_speci(), {
      taxa_reset_counter(taxa_reset_counter() + 1)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ----- Non-identification fields: pre-fill with current values -----
    .fill_specimen_fields <- function(s) {
      if (is.null(s)) return(invisible(NULL))
      for (field in names(.SPECID_MANUAL_FIELDS)) {
        value <- s[[field]]
        has_value <- !is.null(value) && length(value) == 1 && !is.na(value)
        if (identical(.SPECID_MANUAL_FIELDS[[field]], "numeric")) {
          shiny::updateNumericInput(
            session, paste0("new_", field),
            value = if (has_value) as.numeric(value) else NA
          )
        } else {
          shiny::updateTextInput(
            session, paste0("new_", field),
            value = if (has_value) as.character(value) else ""
          )
        }
      }
      invisible(NULL)
    }

    shiny::observeEvent(selected_speci(), {
      .fill_specimen_fields(selected_speci())
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$reset_fields, {
      .fill_specimen_fields(selected_speci())
    })

    # Named list of the section 4b inputs, shaped the way
    # update_specimen_fields() expects (NA meaning "clear the field").
    # Fields absent from the queried record are skipped so that a missing
    # column can never be mistaken for a request to erase a value.
    .specimen_field_inputs <- function() {
      s <- selected_speci()
      out <- list()
      for (field in names(.SPECID_MANUAL_FIELDS)) {
        if (is.null(s) || !field %in% names(s)) next
        value <- input[[paste0("new_", field)]]
        if (is.null(value)) {
          out[[field]] <- NA
        } else if (is.character(value) && !nzchar(trimws(value))) {
          out[[field]] <- NA
        } else if (length(value) == 1 && is.na(value)) {
          out[[field]] <- NA
        } else {
          out[[field]] <- value
        }
      }
      out
    }

    # Embed mod_taxa_search (same module used by launch_taxo_backbone_app);
    # returns reactive holding the selected row (1-row tibble) or NULL.
    taxa_pick_sel <- mod_taxa_search_server(
      "taxa_pick",
      pool  = pool_taxa,
      i18n  = i18n,
      reset = taxa_reset_counter
    )

    # Reactive returning the chosen taxon (single-row tibble) or NULL
    selected_taxon <- shiny::reactive({
      sel <- taxa_pick_sel()
      if (is.null(sel)) return(NULL)
      if (!is.data.frame(sel) || nrow(sel) == 0) return(NULL)
      sel[1, , drop = FALSE]
    })
    resolved_colnam <- shiny::reactiveVal(NULL)  # list(id, name) once picked
    coll_candidates <- shiny::reactiveVal(NULL)  # tibble from query_colnam()

    # ===== 1a. Resolve collector via query_colnam(pattern = ...) =====
    shiny::observeEvent(input$resolve_collector, {
      shiny::req(pool_main())
      raw <- input$collector
      if (is.null(raw) || !nzchar(raw)) {
        shiny::showNotification(i18n()$t("Type a collector name first."),
                                type = "warning")
        return()
      }

      tryCatch({
        hits <- query_colnam(pattern = raw)
        coll_candidates(hits)
        resolved_colnam(NULL)
        if (is.null(hits) || nrow(hits) == 0) {
          shiny::showNotification(i18n()$t("No collector matched that pattern."),
                                  type = "warning")
        } else if (nrow(hits) == 1) {
          resolved_colnam(list(
            id   = as.integer(hits$id_table_colnam[1]),
            name = hits$colnam[1]
          ))
        }
      }, error = function(e) {
        shiny::showNotification(paste(i18n()$t("Collector lookup failed:"),
                                      e$message),
                                type = "error", duration = NULL)
      })
    })

    output$collector_resolve_info <- shiny::renderUI({
      q <- coll_candidates()
      if (is.null(q)) return(NULL)
      if (nrow(q) == 0) {
        return(shiny::div(class = "alert alert-warning",
                          i18n()$t("No collectors matched.")))
      }
      shiny::div(class = "alert alert-info",
                 sprintf(i18n()$t("Found %d collector(s). Click a row to select."),
                         nrow(q)))
    })

    output$collector_candidates_tbl <- DT::renderDT({
      q <- coll_candidates()
      shiny::req(q, nrow(q) > 0)
      cols <- intersect(
        c("id_table_colnam", "colnam", "family_name", "surname",
          "nationality", "institute"),
        names(q)
      )
      DT::datatable(
        q[, cols, drop = FALSE],
        selection = "single", rownames = FALSE,
        options = list(pageLength = 10, dom = "tip", scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$collector_candidates_tbl_rows_selected, {
      i <- input$collector_candidates_tbl_rows_selected
      q <- coll_candidates()
      if (length(i) == 1 && !is.null(q)) {
        resolved_colnam(list(
          id   = as.integer(q$id_table_colnam[i]),
          name = q$colnam[i]
        ))
      }
    })

    output$collector_picked <- shiny::renderUI({
      r <- resolved_colnam()
      if (is.null(r)) return(NULL)
      shiny::div(class = "alert alert-success",
                 shiny::icon("check"), " ",
                 sprintf(i18n()$t("Resolved collector: %s (id_colnam = %s)"),
                         r$name, r$id))
    })

    # ===== 1b. Search specimens =====
    shiny::observeEvent(input$search_speci, {
      shiny::req(pool_main())

      id_sp <- if (!is.null(input$id_specimen) && !is.na(input$id_specimen)) {
        as.integer(input$id_specimen)
      } else NULL
      nbr   <- if (!is.null(input$number) && !is.na(input$number)) {
        as.integer(input$number)
      } else NULL
      r_coll <- resolved_colnam()

      if (is.null(id_sp) && (is.null(r_coll) || is.null(nbr))) {
        shiny::showNotification(
          i18n()$t("Provide id_specimen, or resolve a collector and provide a number."),
          type = "warning"
        )
        return()
      }

      tryCatch({
        hits <- query_specimens(
          id_colnam   = if (!is.null(id_sp)) NULL else r_coll$id,
          number      = if (!is.null(id_sp)) NULL else nbr,
          id_specimen = id_sp,
          interactive = FALSE,
          show_html   = FALSE,
          subset_columns = FALSE,
          con         = pool_main(),
          con.taxa    = pool_taxa()
        )
        speci_hits(hits)
        selected_speci(NULL)
        if (is.null(hits) || nrow(hits) == 0) {
          shiny::showNotification(i18n()$t("No specimen matches."), type = "warning")
        }
      }, error = function(e) {
        shiny::showNotification(paste(i18n()$t("Search failed:"), e$message),
                                type = "error", duration = NULL)
      })
    })

    output$speci_results_info <- shiny::renderUI({
      h <- speci_hits()
      if (is.null(h)) return(NULL)
      shiny::div(class = "alert alert-info",
                 sprintf(i18n()$t("Found %d matching specimen(s). Click a row to select."),
                         nrow(h)))
    })

    output$speci_results_tbl <- DT::renderDT({
      h <- speci_hits()
      shiny::req(h)
      cols <- intersect(
        c("id_specimen", "colnam", "colnbr", "suffix", "family_name", "surname",
          "detby", "dety", "detm", "detd", "country", "idtax_n"),
        names(h)
      )
      DT::datatable(
        h[, cols, drop = FALSE],
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, dom = "tip")
      )
    })

    shiny::observeEvent(input$speci_results_tbl_rows_selected, {
      h <- speci_hits()
      i <- input$speci_results_tbl_rows_selected
      if (length(i) == 1 && !is.null(h)) {
        selected_speci(h[i, , drop = FALSE])
      }
    })

    output$has_selection <- shiny::reactive({ !is.null(selected_speci()) })
    shiny::outputOptions(output, "has_selection", suspendWhenHidden = FALSE)

    # ===== 2b. Linked individuals summary =====
    linked_summary <- shiny::reactive({
      s <- selected_speci()
      shiny::req(s, pool_main())
      tryCatch(
        .get_linked_individuals_summary(
          id_specimen = as.integer(s$id_specimen),
          con         = pool_main()
        ),
        error = function(e) {
          message("links summary error: ", e$message)
          NULL
        }
      )
    })

    output$links_summary_info <- shiny::renderUI({
      lnk <- linked_summary()
      if (is.null(lnk) || nrow(lnk) == 0) {
        return(shiny::div(class = "alert alert-secondary",
                          i18n()$t("No individuals are currently linked to this specimen.")))
      }
      n_ind   <- sum(lnk$n_individuals, na.rm = TRUE)
      plots   <- unique(lnk$plot_name)
      n_plots <- length(plots)
      shiny::div(
        class = "alert alert-info",
        sprintf(i18n()$t("%d individual(s) across %d plot(s) are linked to this specimen."),
                n_ind, n_plots),
        shiny::br(),
        shiny::tags$small(paste(i18n()$t("Plot(s):"), paste(plots, collapse = ", ")))
      )
    })

    output$links_summary_tbl <- DT::renderDT({
      lnk <- linked_summary()
      shiny::req(lnk, nrow(lnk) > 0)
      cols <- intersect(
        c("plot_name", "n_individuals", "tax_fam", "tax_gen", "tax_sp_level", "idtax_n"),
        names(lnk)
      )
      DT::datatable(
        lnk[, cols, drop = FALSE],
        selection = "none",
        rownames  = FALSE,
        options   = list(pageLength = 10, dom = "tip", scrollX = TRUE)
      )
    })

    # ===== 2. Current values card =====
    output$current_card <- shiny::renderUI({
      s <- selected_speci()
      shiny::req(s)
      fmt <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "-" else as.character(x)
      shiny::tags$table(
        class = "table table-sm",
        shiny::tags$tbody(
          shiny::tags$tr(shiny::tags$th("id_specimen"), shiny::tags$td(fmt(s$id_specimen))),
          shiny::tags$tr(shiny::tags$th("collector / number / suffix"),
                         shiny::tags$td(paste(fmt(s$colnam), "/",
                                              fmt(s$colnbr), "/", fmt(s$suffix)))),
          shiny::tags$tr(shiny::tags$th("idtax_n"), shiny::tags$td(fmt(s$idtax_n))),
          shiny::tags$tr(shiny::tags$th("family / genus / species"),
                         shiny::tags$td(paste(fmt(s$family_name), "/",
                                              fmt(s$tax_gen), "/", fmt(s$tax_esp)))),
          shiny::tags$tr(shiny::tags$th("detby / dety-m-d / detvalue"),
                         shiny::tags$td(sprintf("%s / %s-%s-%s / %s",
                                                fmt(s$detby), fmt(s$dety),
                                                fmt(s$detm), fmt(s$detd),
                                                fmt(s$detvalue)))),
          shiny::tags$tr(shiny::tags$th("coly-colm-cold / add_col"),
                         shiny::tags$td(sprintf("%s-%s-%s / %s",
                                                fmt(s$coly), fmt(s$colm),
                                                fmt(s$cold), fmt(s$add_col)))),
          shiny::tags$tr(shiny::tags$th("locality / country"),
                         shiny::tags$td(paste(fmt(s$locality), "/",
                                              fmt(s$country)))),
          shiny::tags$tr(shiny::tags$th("ddlat / ddlon"),
                         shiny::tags$td(paste(fmt(s$ddlat), "/",
                                              fmt(s$ddlon)))),
          shiny::tags$tr(shiny::tags$th("description"),
                         shiny::tags$td(fmt(s$description)))
        )
      )
    })

    # ===== 4. Diff preview =====
    .build_diff <- function() {
      s <- selected_speci()
      if (is.null(s)) return(NULL)

      new_idtax <- if (!is.null(selected_taxon())) selected_taxon()$idtax_n else s$idtax_n

      get_new <- function(input_val, current) {
        if (is.null(input_val)) return(current)
        if (is.character(input_val) && !nzchar(input_val)) return(current)
        if (is.numeric(input_val) && is.na(input_val)) return(current)
        input_val
      }

      new_values <- list(
        idtax_n  = new_idtax,
        detd     = get_new(input$new_detd, s$detd),
        detm     = get_new(input$new_detm, s$detm),
        dety     = get_new(input$new_dety, s$dety),
        detby    = get_new(input$new_detby, s$detby),
        detvalue = get_new(input$new_detvalue, s$detvalue),
        colnbr   = get_new(input$new_colnbr, s$colnbr),
        suffix   = get_new(input$new_suffix, s$suffix)
      )

      # Section 4b fields are absolute: whatever is in the input wins, an
      # empty input meaning the value will be cleared.
      new_values <- c(new_values, .specimen_field_inputs())

      data.frame(
        field   = names(new_values),
        current = sapply(names(new_values), function(k) {
          v <- s[[k]]; if (is.null(v) || length(v) == 0 || is.na(v)) "-" else as.character(v)
        }),
        new = sapply(new_values, function(v) {
          if (is.null(v) || length(v) == 0 || is.na(v)) "-" else as.character(v)
        }),
        stringsAsFactors = FALSE
      )
    }

    shiny::observeEvent(input$preview_btn, {
      d <- .build_diff()
      if (is.null(d)) {
        shiny::showNotification(i18n()$t("Select a specimen first."),
                                type = "warning")
        return()
      }
      d$changed <- d$current != d$new
      diff_tbl(d)
    })

    output$diff_ui <- shiny::renderUI({
      d <- diff_tbl()
      if (is.null(d)) return(NULL)

      n_changed <- sum(d$changed)
      header_cls <- if (n_changed > 0) "alert alert-warning" else "alert alert-secondary"
      header_msg <- if (n_changed > 0) {
        sprintf(i18n()$t("%d field(s) will be updated."), n_changed)
      } else {
        i18n()$t("No fields differ from current values.")
      }

      diff_table <- shiny::tags$table(
        class = "table table-sm table-bordered",
        shiny::tags$thead(
          class = "thead-light",
          shiny::tags$tr(
            shiny::tags$th(i18n()$t("Field")),
            shiny::tags$th(i18n()$t("Current")),
            shiny::tags$th(i18n()$t("New"))
          )
        ),
        shiny::tags$tbody(
          lapply(seq_len(nrow(d)), function(i) {
            row_style <- if (d$changed[i]) "background-color: #fff3cd; font-weight: bold;" else ""
            shiny::tags$tr(
              style = row_style,
              shiny::tags$td(d$field[i]),
              shiny::tags$td(d$current[i]),
              shiny::tags$td(if (d$changed[i]) {
                shiny::span(style = "color: #155724;", d$new[i])
              } else {
                d$new[i]
              })
            )
          })
        )
      )

      # When idtax_n changes, show impact on linked individuals (mirrors
      # what update_ident_specimens() prints in the console).
      idtax_row <- d[d$field == "idtax_n", ]
      impact_block <- if (nrow(idtax_row) == 1 && idtax_row$changed) {
        lnk <- linked_summary()
        if (!is.null(lnk) && nrow(lnk) > 0) {
          n_ind   <- sum(lnk$n_individuals, na.rm = TRUE)
          imp_tbl <- lnk[, intersect(c("plot_name", "n_individuals",
                                       "tax_fam", "tax_gen", "tax_sp_level"),
                                     names(lnk)), drop = FALSE]
          names(imp_tbl)[names(imp_tbl) == "tax_sp_level"] <- "current_taxon"
          shiny::tagList(
            shiny::br(),
            shiny::div(
              class = "alert alert-info",
              shiny::icon("users"), " ",
              sprintf(i18n()$t(
                "Changing idtax_n will propagate to %d linked individual(s) in %d plot(s):"),
                n_ind, nrow(lnk))
            ),
            DT::renderDT(
              DT::datatable(imp_tbl, selection = "none", rownames = FALSE,
                            options = list(dom = "t", pageLength = nrow(imp_tbl)))
            )
          )
        } else {
          shiny::div(
            class = "alert alert-secondary",
            shiny::icon("info-circle"), " ",
            i18n()$t("No individuals linked to this specimen.")
          )
        }
      } else NULL

      shiny::tagList(
        shiny::div(class = header_cls, header_msg),
        diff_table,
        impact_block
      )
    })

    # ===== 5. Apply =====
    shiny::observeEvent(input$apply_btn, {
      s <- selected_speci()
      if (is.null(s)) {
        shiny::showNotification(i18n()$t("Select a specimen first."),
                                type = "warning")
        return()
      }

      # Inject the main pool's connection so update_ident_specimens uses our
      # session connection. update_ident_specimens() internally calls
      # call.mydb() which will reuse the cached connection from this session.
      shiny::withProgress(message = i18n()$t("Applying update..."), value = 0, {
        shiny::incProgress(0.3)

        new_idtax <- if (!is.null(selected_taxon())) selected_taxon()$idtax_n else NULL

        opt_val <- function(x) {
          if (is.null(x)) return(NULL)
          if (is.character(x) && !nzchar(x)) return(NULL)
          if (is.numeric(x) && is.na(x)) return(NULL)
          x
        }

        result <- tryCatch({
          # Identification & determination metadata
          update_ident_specimens(
            id_speci      = as.integer(s$id_specimen),
            id_new_taxa   = new_idtax,
            new_detd      = opt_val(input$new_detd),
            new_detm      = opt_val(input$new_detm),
            new_dety      = opt_val(input$new_dety),
            new_detby     = opt_val(input$new_detby),
            new_detvalue  = opt_val(input$new_detvalue),
            new_colnbr    = opt_val(input$new_colnbr),
            new_suffix    = opt_val(input$new_suffix),
            add_backup    = TRUE,
            show_results  = FALSE,
            only_new_ident = FALSE,
            ask_before_update = FALSE
          )

          shiny::incProgress(0.3)

          # Collection / locality / notes fields
          changed <- update_specimen_fields(
            id_speci          = as.integer(s$id_specimen),
            new_values        = .specimen_field_inputs(),
            add_backup        = TRUE,
            ask_before_update = FALSE,
            show_results      = FALSE,
            con               = pool_main()
          )

          list(ok = TRUE, n_fields = nrow(changed))
        }, error = function(e) list(ok = FALSE, err = e$message))

        shiny::incProgress(1)
        apply_status(result)

        # Refresh the selected specimen so the current values, the pre-filled
        # inputs and any further diff reflect what is now stored.
        if (isTRUE(result$ok)) {
          tryCatch({
            refreshed <- query_specimens(
              id_specimen    = as.integer(s$id_specimen),
              interactive    = FALSE,
              show_html      = FALSE,
              subset_columns = FALSE,
              con            = pool_main(),
              con.taxa       = pool_taxa()
            )
            if (!is.null(refreshed) && nrow(refreshed) == 1) {
              selected_speci(refreshed)
              diff_tbl(NULL)
            }
          }, error = function(e) {
            message("could not refresh specimen after update: ", e$message)
          })
        }
      })
    })

    output$apply_result <- shiny::renderUI({
      r <- apply_status()
      if (is.null(r)) return(NULL)
      if (isTRUE(r$ok)) {
        detail <- if (!is.null(r$n_fields) && r$n_fields > 0) {
          shiny::tags$small(
            shiny::br(),
            sprintf(i18n()$t("%d specimen field(s) updated (excluding identification)."),
                    r$n_fields)
          )
        } else NULL
        shiny::div(class = "alert alert-success", shiny::icon("check-circle"),
                   " ", i18n()$t("Update applied successfully."), detail)
      } else {
        shiny::div(class = "alert alert-danger", shiny::icon("times-circle"),
                   " ", paste(i18n()$t("Update failed:"), r$err))
      }
    })

    invisible(NULL)
  })
}
