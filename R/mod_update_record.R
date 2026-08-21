# Data Update App - record editing module
#
# One module serves both sections of launch_data_update_app(): plot metadata
# (`entity = "plot"`) and individual data (`entity = "individual"`). The two
# differ only in how a record is found and in the taxonomic picker, so the
# editing, aggregation-resolution and apply logic is written once.
#
# Workflow:
#   1. Find and load one record.
#   2. Review its current stored values.
#   3. Edit the flat columns of data_liste_plots / data_individuals.
#   4. Edit features. Features live in data_liste_sub_plots /
#      data_traits_measures, and a column of an extracted table may be the
#      aggregate of several such records - so the app never edits the
#      aggregate. It shows it read-only, with a badge saying how many records
#      it came from, and edits those records individually.
#   5. Preview a diff and apply.

#' Record update module - UI
#'
#' @param id Character, module namespace id.
#' @param entity Either `"plot"` or `"individual"`.
#' @param i18n A `shiny.i18n` translator object.
#'
#' @return A Shiny UI element.
#' @export
mod_update_record_ui <- function(id, entity = c("plot", "individual"), i18n) {
  entity <- match.arg(entity)
  ns <- shiny::NS(id)

  search_panel <- if (entity == "plot") {
    shiny::wellPanel(
      shiny::h4(shiny::icon("search"), " ", i18n$t("1. Select the plot")),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectizeInput(
            ns("plot"), i18n$t("Plot"), choices = NULL,
            options = list(placeholder = i18n$t("Select a plot..."), maxOptions = 100)
          )
        ),
        shiny::column(
          3,
          shiny::numericInput(ns("record_id"), i18n$t("or id_liste_plots"),
                              value = NA, min = 1)
        ),
        shiny::column(
          3, style = "padding-top: 25px;",
          shiny::actionButton(
            ns("load"),
            shiny::tagList(shiny::icon("download"), " ", i18n$t("Load record")),
            class = "btn-primary"
          )
        )
      )
    )
  } else {
    shiny::wellPanel(
      shiny::h4(shiny::icon("search"), " ", i18n$t("1. Find the individual")),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectizeInput(
            ns("plot"), i18n$t("Plot"), choices = NULL,
            options = list(placeholder = i18n$t("Select a plot..."), maxOptions = 100)
          )
        ),
        shiny::column(3, shiny::textInput(ns("tag"), i18n$t("Tag"), placeholder = "e.g. 123")),
        shiny::column(3, shiny::numericInput(ns("record_id"), i18n$t("or id_n"),
                                             value = NA, min = 1)),
        shiny::column(
          2, style = "padding-top: 25px;",
          shiny::actionButton(
            ns("search"),
            shiny::tagList(shiny::icon("search"), " ", i18n$t("Search")),
            class = "btn-primary"
          )
        )
      ),
      shiny::uiOutput(ns("search_info")),
      DT::DTOutput(ns("search_tbl"))
    )
  }

  taxon_panel <- if (entity == "individual") {
    shiny::wellPanel(
      shiny::h4(shiny::icon("seedling"), " ",
                i18n$t("3b. Change the identification (optional)")),
      shiny::uiOutput(ns("taxon_current")),
      mod_taxa_search_ui(ns("taxa_pick"))
    )
  } else {
    NULL
  }

  shiny::tagList(
    search_panel,

    shiny::conditionalPanel(
      condition = sprintf("output['%s']", ns("loaded")),

      # --- 2. Current values ---
      shiny::wellPanel(
        shiny::h4(shiny::icon("info-circle"), " ", i18n$t("2. Current stored values")),
        shiny::tags$p(
          class = "text-muted",
          shiny::tags$small(
            i18n$t("The whole record as an extraction returns it (output_style = \"full\"), features included and columns that cannot be edited here as well. It runs a full extraction, so it is fetched only when you ask for it.")
          )
        ),
        shiny::fluidRow(
          shiny::column(
            5,
            shiny::actionButton(
              ns("load_full"),
              shiny::tagList(shiny::icon("list"), " ", i18n$t("Show the full record"))
            )
          ),
          shiny::column(
            7,
            shiny::checkboxInput(ns("current_hide_empty"),
                                 i18n$t("Hide fields with no value"), value = TRUE)
          )
        ),
        shiny::uiOutput(ns("current_note")),
        DT::DTOutput(ns("current_tbl"))
      ),

      # --- 3. Flat columns ---
      shiny::wellPanel(
        shiny::h4(shiny::icon("table"), " ", i18n$t("3. Columns stored directly on the record")),
        shiny::tags$p(
          class = "text-muted",
          shiny::tags$small(
            i18n$t("These inputs are pre-filled with the stored values and are absolute: clearing one will erase that value in the database.")
          )
        ),
        shiny::uiOutput(ns("direct_form")),
        shiny::actionLink(
          ns("reset_direct"),
          label = shiny::tagList(shiny::icon("undo"), " ", i18n$t("Reset to stored values"))
        )
      ),

      taxon_panel,

      # --- 4. Features ---
      shiny::wellPanel(
        shiny::h4(shiny::icon("layer-group"), " ", i18n$t("4. Features")),
        shiny::div(
          class = "alert alert-info",
          shiny::icon("info-circle"), " ",
          i18n$t("Features are not columns of this record: each one is a separate row in another table. When several rows feed the same column, the value you see in an extracted table is an aggregate and cannot be edited as one value - edit the underlying records below.")
        ),
        shiny::uiOutput(ns("feature_overview_info")),
        DT::DTOutput(ns("feature_overview_tbl")),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(ns("feature_pick"), i18n$t("Feature to edit"),
                               choices = NULL)
          ),
          shiny::column(
            6, style = "padding-top: 25px;",
            shiny::uiOutput(ns("feature_badge"))
          )
        ),
        shiny::uiOutput(ns("feature_form"))
      ),

      # --- 5. Preview & apply ---
      shiny::wellPanel(
        shiny::h4(shiny::icon("eye"), " ", i18n$t("5. Preview & apply")),
        shiny::actionButton(
          ns("preview"),
          shiny::tagList(shiny::icon("eye"), " ", i18n$t("Preview changes")),
          class = "btn-secondary"
        ),
        shiny::actionButton(
          ns("apply"),
          shiny::tagList(shiny::icon("save"), " ", i18n$t("Apply update")),
          class = "btn-success", style = "margin-left: 10px;"
        ),
        shiny::br(), shiny::br(),
        shiny::uiOutput(ns("diff_ui")),
        shiny::uiOutput(ns("apply_result"))
      )
    )
  )
}


#' Record update module - server
#'
#' @param id Character, module namespace id.
#' @param entity Either `"plot"` or `"individual"`.
#' @param pool_main Reactive returning the main database pool.
#' @param pool_taxa Reactive returning the taxa database pool.
#' @param i18n Reactive returning a `shiny.i18n` translator.
#'
#' @return Invisibly `NULL`.
#' @export
mod_update_record_server <- function(id, entity = c("plot", "individual"),
                                     pool_main, pool_taxa, i18n) {
  entity <- match.arg(entity)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    spec <- .upd_entity_spec(entity)

    record        <- shiny::reactiveVal(NULL)   # 1-row tibble, current DB state
    fields        <- shiny::reactiveVal(NULL)   # editable flat column descriptors
    lookup_cache  <- shiny::reactiveVal(list()) # lookup table -> named choices
    feat_records  <- shiny::reactiveVal(NULL)   # one row per feature record
    pending_feat  <- shiny::reactiveVal(list()) # record_id -> list(column = value)
    rendered_feat <- shiny::reactiveVal(NULL)   # feature currently shown in the form
    search_hits   <- shiny::reactiveVal(NULL)
    diff_tbl      <- shiny::reactiveVal(NULL)
    apply_status  <- shiny::reactiveVal(NULL)
    taxa_reset    <- shiny::reactiveVal(0)

    with_main <- function(fun) {
      shiny::req(pool_main())
      .upd_with_con(pool_main(), fun)
    }

    # =========================================================================
    # 1. FIND THE RECORD
    # =========================================================================

    shiny::observe({
      shiny::req(pool_main())
      plots <- tryCatch(
        with_main(function(con) {
          DBI::dbGetQuery(
            con, "SELECT id_liste_plots, plot_name FROM data_liste_plots ORDER BY plot_name"
          )
        }),
        error = function(e) {
          cli::cli_alert_warning("Could not load plots: {e$message}")
          NULL
        }
      )
      shiny::req(plots)
      shiny::updateSelectizeInput(
        session, "plot",
        choices = c(stats::setNames("", ""),
                    stats::setNames(plots$id_liste_plots, plots$plot_name)),
        server = TRUE
      )
    })

    # Load one record by id and rebuild every derived piece of state.
    load_record <- function(id_value) {
      id_value <- suppressWarnings(as.integer(id_value))
      if (is.na(id_value)) {
        shiny::showNotification(i18n()$t("No record selected."), type = "warning")
        return(invisible(NULL))
      }

      res <- tryCatch(
        with_main(function(con) {
          list(
            row      = .upd_fetch_record(entity, id_value, con),
            fields   = .upd_direct_fields(entity, con),
            features = if (entity == "plot") {
              .upd_plot_feature_records(id_value, con)
            } else {
              .upd_individual_feature_records(id_value, con)
            }
          )
        }),
        error = function(e) {
          shiny::showNotification(paste(i18n()$t("Could not load record:"), e$message),
                                  type = "error", duration = NULL)
          NULL
        }
      )
      if (is.null(res)) return(invisible(NULL))

      if (is.null(res$row)) {
        shiny::showNotification(i18n()$t("No record with that id."), type = "warning")
        return(invisible(NULL))
      }

      record(res$row)
      fields(res$fields)
      feat_records(res$features)
      pending_feat(list())
      # `rendered_feat` is deliberately left alone. If the new record's first
      # feature has the same name as the one already selected, the select input
      # does not change and its observer never fires - clearing it here would
      # leave nothing tracking the form that is on screen, and the next harvest
      # would silently drop the user's edits. Records of the new feature have
      # different ids, so a stale name cannot harvest the wrong values.
      diff_tbl(NULL)
      apply_status(NULL)
      taxa_reset(taxa_reset() + 1)
      invisible(NULL)
    }

    if (entity == "plot") {
      shiny::observeEvent(input$load, {
        id_value <- if (!is.null(input$record_id) && !is.na(input$record_id)) {
          input$record_id
        } else if (!is.null(input$plot) && nzchar(input$plot)) {
          input$plot
        } else {
          NA
        }
        load_record(id_value)
      })
    } else {
      shiny::observeEvent(input$search, {
        if (!is.null(input$record_id) && !is.na(input$record_id)) {
          search_hits(NULL)
          load_record(input$record_id)
          return()
        }

        hits <- tryCatch(
          with_main(function(con) {
            conditions <- character()
            if (!is.null(input$plot) && nzchar(input$plot)) {
              conditions <- c(conditions, glue::glue_sql(
                "i.id_table_liste_plots_n = {as.integer(input$plot)}", .con = con))
            }
            if (!is.null(input$tag) && nzchar(trimws(input$tag))) {
              conditions <- c(conditions, glue::glue_sql(
                "i.tag::text = {trimws(input$tag)}", .con = con))
            }
            if (length(conditions) == 0) return(NULL)

            sql <- paste(
              "SELECT i.id_n, i.tag, i.idtax_n, i.original_tax_name,
                      i.herbarium_nbe_char, p.plot_name
                 FROM data_individuals i
                 LEFT JOIN data_liste_plots p ON i.id_table_liste_plots_n = p.id_liste_plots
                WHERE", paste(conditions, collapse = " AND "),
              "ORDER BY i.tag LIMIT 500"
            )
            dplyr::as_tibble(DBI::dbGetQuery(con, sql))
          }),
          error = function(e) {
            shiny::showNotification(paste(i18n()$t("Search failed:"), e$message),
                                    type = "error", duration = NULL)
            NULL
          }
        )

        if (is.null(hits)) {
          shiny::showNotification(
            i18n()$t("Give a plot, a tag, or an id to search on."), type = "warning")
          return()
        }
        search_hits(hits)
        if (nrow(hits) == 0) {
          shiny::showNotification(i18n()$t("No individual matches."), type = "warning")
        } else if (nrow(hits) == 1) {
          load_record(hits$id_n[1])
        }
      })

      output$search_info <- shiny::renderUI({
        h <- search_hits()
        if (is.null(h)) return(NULL)
        if (nrow(h) == 0) {
          return(shiny::div(class = "alert alert-warning", i18n()$t("No individual matches.")))
        }
        shiny::div(class = "alert alert-info",
                   sprintf(i18n()$t("Found %d individual(s). Click a row to load it."), nrow(h)))
      })

      output$search_tbl <- DT::renderDT({
        h <- search_hits()
        shiny::req(h, nrow(h) > 0)
        DT::datatable(h, selection = "single", rownames = FALSE,
                      options = list(pageLength = 5, scrollX = TRUE, dom = "tip"))
      })

      shiny::observeEvent(input$search_tbl_rows_selected, {
        i <- input$search_tbl_rows_selected
        h <- search_hits()
        if (length(i) == 1 && !is.null(h)) load_record(h$id_n[i])
      })
    }

    output$loaded <- shiny::reactive({ !is.null(record()) })
    shiny::outputOptions(output, "loaded", suspendWhenHidden = FALSE)

    # =========================================================================
    # 2. CURRENT VALUES
    # =========================================================================

    # The whole record, as query_plots(output_style = "full") returns it: the
    # form below shows only what it can write, which is not what the user needs
    # to review. Fetched apart from the record itself - it is a heavier query,
    # and a failure here must not stop the record from being edited.
    current_full <- shiny::reactiveVal(NULL)
    current_err  <- shiny::reactiveVal(NULL)

    # A new record clears the view; the extraction is only run on demand.
    shiny::observeEvent(record(), {
      current_full(NULL)
      current_err(NULL)
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$load_full, {
      r <- record()
      shiny::req(r, pool_main())
      current_err(NULL)

      taxa_con <- tryCatch(pool_taxa(), error = function(e) NULL)
      res <- tryCatch(
        shiny::withProgress(
          message = i18n()$t("Loading the full record..."), value = 0.4,
          .upd_full_record_view(entity, r[[spec$id_column]][1],
                                pool_main(), taxa_con)
        ),
        error = function(e) {
          current_err(e$message)
          NULL
        }
      )
      current_full(res)
    })

    # Most columns of a full extraction are empty for any one record, so they
    # are hidden by default rather than paged through.
    current_view <- shiny::reactive({
      tb <- current_full()
      if (is.null(tb)) return(NULL)
      if (isTRUE(input$current_hide_empty)) {
        value_cols <- setdiff(names(tb), "field")
        if (length(value_cols) == 0) return(tb)
        filled <- Reduce(`|`, lapply(value_cols, function(cl) nzchar(tb[[cl]])))
        tb <- tb[filled, , drop = FALSE]
      }
      tb
    })

    output$current_note <- shiny::renderUI({
      shiny::req(record())
      if (!is.null(current_err())) {
        return(shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"), " ",
          paste(i18n()$t("Could not load the full record:"), current_err())
        ))
      }
      tb <- current_full()
      if (is.null(tb)) {
        return(shiny::div(
          class = "text-muted",
          i18n()$t("Not loaded. Use the button above to see the whole record.")
        ))
      }
      shiny::tags$p(
        class = "text-muted",
        shiny::tags$small(sprintf(i18n()$t("%d of %d fields shown."),
                                  nrow(current_view()), nrow(tb)))
      )
    })

    output$current_tbl <- DT::renderDT({
      tb <- current_view()
      shiny::req(tb, nrow(tb) > 0)
      names(tb)[names(tb) == "field"] <- i18n()$t("Field")
      if (ncol(tb) == 2) names(tb)[2] <- i18n()$t("Value")
      DT::datatable(
        tb, rownames = FALSE, selection = "none",
        options = list(pageLength = 15, scrollX = TRUE, dom = "ftip",
                       columnDefs = list(list(width = "240px", targets = 0)))
      )
    })

    # =========================================================================
    # 3. FLAT COLUMNS
    # =========================================================================

    # Lookup choices, fetched once per lookup table per session.
    lookup_for <- function(f) {
      cache <- lookup_cache()
      key <- f$lookup_table
      if (!is.null(cache[[key]])) return(cache[[key]])
      choices <- with_main(function(con) {
        .upd_lookup_choices(f$lookup_table, f$lookup_key, f$lookup_value, con)
      })
      cache[[key]] <- choices
      lookup_cache(cache)
      choices
    }

    output$direct_form <- shiny::renderUI({
      fl <- fields()
      r  <- record()
      shiny::req(fl, r, nrow(fl) > 0)

      inputs <- lapply(seq_len(nrow(fl)), function(i) {
        f <- fl[i, ]
        current <- r[[f$field]]
        has <- !is.null(current) && length(current) == 1 && !is.na(current)

        if (f$kind == "taxon") {
          return(shiny::column(
            4,
            shiny::div(
              class = "form-group",
              shiny::tags$label(f$field),
              shiny::div(class = "form-control", style = "background:#eee;",
                         if (has) as.character(current) else "-"),
              shiny::tags$small(class = "text-muted",
                                i18n()$t("Edit in the identification section below."))
            )
          ))
        }

        ctl <- switch(
          f$kind,
          lookup = {
            choices <- lookup_for(f)
            shiny::selectizeInput(
              ns(paste0("dir_", f$field)), f$field,
              choices  = c(stats::setNames("", ""), choices),
              selected = if (has) as.character(current) else ""
            )
          },
          boolean = shiny::selectInput(
            ns(paste0("dir_", f$field)), f$field,
            choices  = c("", "TRUE", "FALSE"),
            selected = if (has) as.character(current) else ""
          ),
          numeric = ,
          integer = shiny::numericInput(
            ns(paste0("dir_", f$field)), f$field,
            value = if (has) as.numeric(current) else NA
          ),
          shiny::textInput(
            ns(paste0("dir_", f$field)), f$field,
            value = if (has) as.character(current) else ""
          )
        )
        shiny::column(4, ctl)
      })

      # Three inputs per row.
      rows <- split(inputs, ceiling(seq_along(inputs) / 3))
      shiny::tagList(lapply(rows, function(r) do.call(shiny::fluidRow, r)))
    })

    shiny::observeEvent(input$reset_direct, {
      fl <- fields(); r <- record()
      shiny::req(fl, r)
      for (i in seq_len(nrow(fl))) {
        f <- fl[i, ]
        if (f$kind == "taxon") next
        current <- r[[f$field]]
        has <- !is.null(current) && length(current) == 1 && !is.na(current)
        input_id <- paste0("dir_", f$field)
        if (f$kind %in% c("numeric", "integer")) {
          shiny::updateNumericInput(session, input_id,
                                    value = if (has) as.numeric(current) else NA)
        } else if (f$kind == "lookup") {
          shiny::updateSelectizeInput(session, input_id,
                                      selected = if (has) as.character(current) else "")
        } else if (f$kind == "boolean") {
          shiny::updateSelectInput(session, input_id,
                                   selected = if (has) as.character(current) else "")
        } else {
          shiny::updateTextInput(session, input_id,
                                 value = if (has) as.character(current) else "")
        }
      }
    })

    # Flat-column values as currently typed, coerced to the column's type.
    #
    # Returns an empty list while the form has not rendered yet: every input
    # would read as NULL, which would otherwise be indistinguishable from the
    # user clearing every field.
    direct_inputs <- function() {
      fl <- fields(); r <- record()
      if (is.null(fl) || is.null(r) || nrow(fl) == 0) return(list())

      editable <- fl[fl$kind != "taxon", , drop = FALSE]
      rendered <- nrow(editable) == 0 || any(vapply(
        editable$field,
        function(f) !is.null(input[[paste0("dir_", f)]]),
        logical(1)
      ))

      out <- list()
      for (i in seq_len(nrow(fl))) {
        f <- fl[i, ]
        if (f$kind == "taxon") {
          sel <- selected_taxon()
          if (!is.null(sel)) out[[f$field]] <- as.integer(sel$idtax_n[1])
          next
        }
        if (!rendered) next
        out[[f$field]] <- .upd_coerce(input[[paste0("dir_", f$field)]], f$pg_type)
      }
      out
    }

    # ----- Taxon picker (individuals only) -----
    selected_taxon <- if (entity == "individual") {
      picked <- mod_taxa_search_server("taxa_pick", pool = pool_taxa,
                                       i18n = i18n, reset = taxa_reset)
      shiny::reactive({
        sel <- picked()
        if (is.null(sel) || !is.data.frame(sel) || nrow(sel) == 0) return(NULL)
        sel[1, , drop = FALSE]
      })
    } else {
      shiny::reactive(NULL)
    }

    if (entity == "individual") {
      output$taxon_current <- shiny::renderUI({
        r <- record()
        shiny::req(r)
        shiny::div(
          class = "alert alert-secondary",
          sprintf(i18n()$t("Current idtax_n: %s (original name: %s)"),
                  .upd_fmt(r$idtax_n), .upd_fmt(r$original_tax_name)),
          shiny::br(),
          shiny::tags$small(
            i18n()$t("Leave the search below untouched to keep the current identification.")
          )
        )
      })
    }

    # =========================================================================
    # 4. FEATURES
    # =========================================================================

    feature_summary <- shiny::reactive({
      fr <- feat_records()
      shiny::req(fr)
      .upd_feature_summary(fr)
    })

    # How the extraction treats a feature. The rule comes from the resolver;
    # the wording lives here because it needs the translator.
    rule_label <- function(rule, n) {
      if (n == 1 && rule %in% c("mean", "concat", "other")) {
        return(i18n()$t("one record, shown as it is"))
      }
      switch(
        rule,
        mean       = sprintf(i18n()$t("mean of %d records"), n),
        concat     = sprintf(i18n()$t("%d records joined into one text"), n),
        per_census = sprintf(i18n()$t("one value per census, from %d records"), n),
        census     = i18n()$t("not a value: n_census, first_census, last_census, date_census_N"),
        not_extracted = i18n()$t("not carried into extracted tables"),
        sprintf(i18n()$t("%d record(s)"), n)
      )
    }

    rule_labels <- function(rules, ns_records) {
      vapply(seq_along(rules), function(i) rule_label(rules[i], ns_records[i]),
             character(1))
    }

    output$feature_overview_info <- shiny::renderUI({
      fr <- feat_records()
      shiny::req(fr)
      if (nrow(fr) == 0) {
        return(shiny::div(class = "alert alert-secondary",
                          i18n()$t("This record has no features recorded.")))
      }
      s <- feature_summary()
      n_agg <- sum(s$is_aggregated)
      if (n_agg == 0) {
        shiny::div(class = "alert alert-success", shiny::icon("check"), " ",
                   sprintf(i18n()$t("%d feature(s), each backed by a single record."),
                           nrow(s)))
      } else {
        shiny::div(class = "alert alert-warning", shiny::icon("exclamation-triangle"), " ",
                   sprintf(i18n()$t("%d of %d feature(s) are backed by several records. An extracted table summarises them - how depends on the feature, and the summary is not editable as one value."),
                           n_agg, nrow(s)))
      }
    })

    output$feature_overview_tbl <- DT::renderDT({
      s <- feature_summary()
      shiny::req(nrow(s) > 0)
      shown <- s
      shown$stored_as <- rule_labels(shown$agg_rule, shown$n_records)
      shown$aggregate_display <- ifelse(is.na(shown$aggregate_display), "-",
                                        shown$aggregate_display)
      shown <- shown[, c("feature", "valuetype", "unit", "n_records",
                         "aggregate_display", "stored_as"), drop = FALSE]
      DT::datatable(
        shown, selection = "none", rownames = FALSE,
        colnames = c(i18n()$t("Feature"), i18n()$t("Value type"), i18n()$t("Unit"),
                     i18n()$t("Records"), i18n()$t("Value in extracted table"),
                     i18n()$t("In an extracted table")),
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
      ) %>%
        DT::formatStyle(
          "n_records", target = "row",
          backgroundColor = DT::styleInterval(1, c("transparent", "#fff3cd"))
        )
    })

    shiny::observeEvent(feature_summary(), {
      s <- feature_summary()
      choices <- if (nrow(s) == 0) character(0) else {
        stats::setNames(
          s$feature,
          sprintf("%s - %s", s$feature, rule_labels(s$agg_rule, s$n_records))
        )
      }
      shiny::updateSelectInput(session, "feature_pick", choices = choices)
    })

    # Harvest whatever is typed in the form before it is replaced, otherwise
    # switching feature would silently discard the edits.
    shiny::observeEvent(input$feature_pick, {
      harvest_feature(rendered_feat())
      rendered_feat(input$feature_pick)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    output$feature_badge <- shiny::renderUI({
      s <- feature_summary()
      pick <- input$feature_pick
      shiny::req(pick, nrow(s) > 0)
      row <- s[s$feature == pick, ]
      shiny::req(nrow(row) == 1)
      shown <- if (is.na(row$aggregate_display)) "-" else row$aggregate_display

      if (identical(row$agg_rule, "census")) {
        shiny::div(
          class = "alert alert-info", style = "margin-bottom:0;",
          shiny::icon("info-circle"), " ",
          sprintf(i18n()$t("These %d records are the plot's censuses. An extracted table does not show their numbers: it shows n_census, first_census, last_census and one date_census_N column per census (%s)."),
                  row$n_records, shown)
        )
      } else if (identical(row$agg_rule, "not_extracted")) {
        shiny::div(
          class = "alert alert-secondary", style = "margin-bottom:0;",
          sprintf(i18n()$t("%d record(s). This feature is not carried into extracted tables; the records below are the whole of it."),
                  row$n_records)
        )
      } else if (!row$is_aggregated) {
        shiny::div(class = "alert alert-secondary", style = "margin-bottom:0;",
                   sprintf(i18n()$t("One record. Extracted value: %s"), shown))
      } else {
        shiny::div(
          class = "alert alert-warning", style = "margin-bottom:0;",
          shiny::icon("exclamation-triangle"), " ",
          sprintf(i18n()$t("%d records below. An extracted table shows %s (%s), which cannot be edited as one value - edit the records individually."),
                  row$n_records, shown, rule_label(row$agg_rule, row$n_records))
        )
      }
    })

    output$feature_form <- shiny::renderUI({
      fr <- feat_records()
      pick <- input$feature_pick
      shiny::req(fr, pick, nrow(fr) > 0)
      rows <- fr[fr$feature == pick, , drop = FALSE]
      shiny::req(nrow(rows) > 0)

      pend <- pending_feat()
      colnam_choices <- if (any(grepl("^table_colnam", rows$valuetype))) {
        with_main(function(con) {
          .upd_lookup_choices("table_colnam", "id_table_colnam", "colnam", con)
        })
      } else {
        character(0)
      }

      cards <- lapply(seq_len(nrow(rows)), function(i) {
        rec <- rows[i, ]
        rid <- as.character(rec$record_id)
        stored <- pend[[rid]]
        pick_stored <- function(column, fallback) {
          if (!is.null(stored) && column %in% names(stored)) stored[[column]] else fallback
        }

        value_col <- .upd_value_column(rec$valuetype, entity)
        is_colnam <- !is.na(rec$valuetype) && grepl("^table_colnam", rec$valuetype)

        value_input <- if (is_colnam) {
          shiny::selectizeInput(
            ns(paste0("f_", rid, "_value")), i18n()$t("Value"),
            choices  = c(stats::setNames("", ""), colnam_choices),
            selected = {
              v <- pick_stored(value_col, rec$lookup_id)
              # An unset reference must select the blank entry, not the literal
              # string "NA", which selectize would show as a phantom choice.
              if (is.null(v) || is.na(v)) "" else as.character(v)
            }
          )
        } else if (!is.na(rec$valuetype) && rec$valuetype %in% c("numeric", "integer")) {
          shiny::numericInput(
            ns(paste0("f_", rid, "_value")),
            paste0(i18n()$t("Value"),
                   if (!is.na(rec$unit) && nzchar(rec$unit)) paste0(" (", rec$unit, ")") else ""),
            value = suppressWarnings(as.numeric(pick_stored(value_col, rec$value_num))),
            min = if (is.na(rec$min_allowed)) NA else rec$min_allowed,
            max = if (is.na(rec$max_allowed)) NA else rec$max_allowed
          )
        } else {
          shiny::textInput(
            ns(paste0("f_", rid, "_value")), i18n()$t("Value"),
            value = {
              v <- pick_stored(value_col, rec$value_char)
              if (is.null(v) || is.na(v)) "" else as.character(v)
            }
          )
        }

        num_or_blank <- function(x) if (is.null(x) || is.na(x)) NA else as.numeric(x)

        shiny::div(
          class = "panel panel-default",
          style = "padding: 12px; margin-bottom: 12px; border: 1px solid #dee2e6; border-radius: 6px;",
          shiny::tags$strong(sprintf("%s = %s", spec$feature_id, rid)),
          if (!is.na(rec$context) && nzchar(rec$context)) {
            shiny::tags$span(class = "label label-info", style = "margin-left:8px;",
                             rec$context)
          },
          shiny::fluidRow(
            shiny::column(4, value_input),
            shiny::column(2, shiny::numericInput(
              ns(paste0("f_", rid, "_year")), i18n()$t("Year"),
              value = num_or_blank(pick_stored("year", rec$year)))),
            shiny::column(2, shiny::numericInput(
              ns(paste0("f_", rid, "_month")), i18n()$t("Month"),
              value = num_or_blank(pick_stored("month", rec$month)), min = 1, max = 12)),
            shiny::column(2, shiny::numericInput(
              ns(paste0("f_", rid, "_day")), i18n()$t("Day"),
              value = num_or_blank(pick_stored("day", rec$day)), min = 1, max = 31)),
            shiny::column(2, shiny::textInput(
              ns(paste0("f_", rid, "_issue")), i18n()$t("Issue"),
              value = {
                v <- pick_stored("issue", rec$issue)
                if (is.null(v) || is.na(v)) "" else as.character(v)
              }))
          )
        )
      })

      shiny::tagList(cards)
    })

    # Read the currently rendered feature form back into `pending_feat`.
    #
    # Only values that differ from what is stored are kept, so an untouched
    # form contributes nothing and a cleared input is recorded as NA (clear).
    harvest_feature <- function(pick = rendered_feat()) {
      fr <- feat_records()
      if (is.null(fr) || is.null(pick) || !nzchar(pick)) return(invisible(NULL))
      rows <- fr[fr$feature == pick, , drop = FALSE]
      if (nrow(rows) == 0) return(invisible(NULL))

      pend <- pending_feat()
      for (i in seq_len(nrow(rows))) {
        rec <- rows[i, ]
        rid <- as.character(rec$record_id)
        value_col <- .upd_value_column(rec$valuetype, entity)
        is_colnam <- !is.na(rec$valuetype) && grepl("^table_colnam", rec$valuetype)
        numeric_value <- is_colnam ||
          (!is.na(rec$valuetype) && rec$valuetype %in% c("numeric", "integer"))

        raw_value <- input[[paste0("f_", rid, "_value")]]
        # The form has not been rendered yet for this record: nothing to harvest.
        if (is.null(raw_value) && is.null(input[[paste0("f_", rid, "_year")]])) next

        new_value <- .upd_coerce(raw_value, if (numeric_value) "numeric" else "text")
        stored_value <- if (is_colnam) rec$lookup_id
                        else if (numeric_value) rec$value_num
                        else rec$value_char

        changes <- list()
        if (!.upd_same(new_value, stored_value)) {
          # A table_colnam feature is a numeric feature holding the
          # table_colnam id, so the id goes to `typevalue` and nowhere else.
          # `typevalue_char` is never used for these, and `id_colnam` is not
          # their store - the few rows carrying one were filled by mistake.
          changes[[value_col]] <- new_value
        }

        for (dcol in c("year", "month", "day")) {
          new_d <- .upd_coerce(input[[paste0("f_", rid, "_", dcol)]], "integer")
          if (!.upd_same(new_d, rec[[dcol]])) changes[[dcol]] <- new_d
        }
        new_issue <- .upd_coerce(input[[paste0("f_", rid, "_issue")]], "text")
        if (!.upd_same(new_issue, rec$issue)) changes[["issue"]] <- new_issue

        if (length(changes) > 0) pend[[rid]] <- changes else pend[[rid]] <- NULL
      }
      pending_feat(pend)
      invisible(NULL)
    }

    # =========================================================================
    # 5. PREVIEW & APPLY
    # =========================================================================

    # Turn a table_colnam id back into the name it stands for, so the diff
    # reads the way the form did. Fetched at most once per preview.
    colnam_names <- NULL
    resolve_reference <- function(id) {
      if (is.null(id) || is.na(id)) return("-")
      if (is.null(colnam_names)) {
        colnam_names <<- tryCatch(
          with_main(function(con) {
            .upd_lookup_choices("table_colnam", "id_table_colnam", "colnam", con)
          }),
          error = function(e) character(0)
        )
      }
      hit <- names(colnam_names)[colnam_names == as.character(id)]
      if (length(hit) == 1) hit else as.character(id)
    }

    build_diff <- function() {
      r <- record()
      if (is.null(r)) return(NULL)
      # The form on screen is the one for input$feature_pick, whether or not the
      # select observer has caught up with it.
      harvest_feature(input$feature_pick)

      fl <- fields()
      values <- direct_inputs()
      rows <- list()

      for (f_name in names(values)) {
        # A field the form did not supply (the taxon picker while untouched) is
        # not a request to clear anything - leave it out of the diff entirely.
        new_value <- values[[f_name]]
        old_value <- r[[f_name]]
        rows[[length(rows) + 1]] <- data.frame(
          scope   = i18n()$t("record column"),
          target  = paste0(spec$table, ".", f_name),
          current = .upd_fmt(old_value),
          new     = .upd_fmt(new_value),
          changed = !.upd_same(new_value, old_value),
          stringsAsFactors = FALSE
        )
      }

      fr <- feat_records()
      for (rid in names(pending_feat())) {
        changes <- pending_feat()[[rid]]
        rec <- fr[as.character(fr$record_id) == rid, , drop = FALSE]
        if (nrow(rec) != 1) next

        value_col <- .upd_value_column(rec$valuetype, entity)
        is_ref <- !is.na(rec$valuetype) && grepl("^table_", rec$valuetype)

        for (column in names(changes)) {
          old_value <- switch(
            column,
            typevalue       = rec$value_num,
            traitvalue      = rec$value_num,
            typevalue_char  = rec$value_char,
            traitvalue_char = rec$value_char,
            rec[[column]]
          )
          # A reference feature stores an id; the diff must still read as names,
          # which is what the user picked and what an extracted table shows.
          show_as_name <- is_ref && identical(column, value_col)
          rows[[length(rows) + 1]] <- data.frame(
            scope   = sprintf("%s [%s = %s]", rec$feature, spec$feature_id, rid),
            target  = paste0(spec$feature_table, ".", column),
            current = if (show_as_name) .upd_fmt(rec$value_display) else .upd_fmt(old_value),
            new     = if (show_as_name) resolve_reference(changes[[column]])
                      else .upd_fmt(changes[[column]]),
            changed = TRUE,
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(rows) == 0) return(NULL)
      do.call(rbind, rows)
    }

    shiny::observeEvent(input$preview, {
      d <- build_diff()
      if (is.null(d)) {
        shiny::showNotification(i18n()$t("Load a record first."), type = "warning")
        return()
      }
      diff_tbl(d)
    })

    output$diff_ui <- shiny::renderUI({
      d <- diff_tbl()
      if (is.null(d)) return(NULL)
      changed <- d[d$changed, , drop = FALSE]

      header <- if (nrow(changed) == 0) {
        shiny::div(class = "alert alert-secondary",
                   i18n()$t("Nothing differs from the stored values."))
      } else {
        shiny::div(class = "alert alert-warning",
                   sprintf(i18n()$t("%d value(s) will be written."), nrow(changed)))
      }
      if (nrow(changed) == 0) return(header)

      shiny::tagList(
        header,
        shiny::tags$table(
          class = "table table-sm table-bordered",
          shiny::tags$thead(
            class = "thead-light",
            shiny::tags$tr(
              shiny::tags$th(i18n()$t("Scope")),
              shiny::tags$th(i18n()$t("Target")),
              shiny::tags$th(i18n()$t("Current")),
              shiny::tags$th(i18n()$t("New"))
            )
          ),
          shiny::tags$tbody(
            lapply(seq_len(nrow(changed)), function(i) {
              shiny::tags$tr(
                style = "background-color: #fff3cd;",
                shiny::tags$td(changed$scope[i]),
                shiny::tags$td(shiny::tags$code(changed$target[i])),
                shiny::tags$td(changed$current[i]),
                shiny::tags$td(shiny::span(style = "color:#155724; font-weight:600;",
                                           changed$new[i]))
              )
            })
          )
        )
      )
    })

    shiny::observeEvent(input$apply, {
      r <- record()
      if (is.null(r)) {
        shiny::showNotification(i18n()$t("Load a record first."), type = "warning")
        return()
      }
      d <- build_diff()
      if (is.null(d) || sum(d$changed) == 0) {
        shiny::showNotification(i18n()$t("Nothing differs from the stored values."),
                                type = "message")
        diff_tbl(d)
        return()
      }
      diff_tbl(d)

      id_value <- as.integer(r[[spec$id_column]])
      values   <- direct_inputs()
      features <- pending_feat()

      shiny::withProgress(message = i18n()$t("Applying update..."), value = 0, {
        result <- tryCatch({
          out <- with_main(function(con) {
            .upd_apply_all(entity, id_value, values, features, con)
          })
          shiny::incProgress(0.5)
          c(list(ok = TRUE), out)
        }, error = function(e) list(ok = FALSE, err = conditionMessage(e)))

        shiny::incProgress(1)
      })

      if (isTRUE(result$ok)) {
        # Re-read from the database so the card, the form and any further diff
        # show what is now stored rather than what we hoped to write.
        # load_record() clears apply_status, so report the outcome after it.
        load_record(id_value)
      }
      apply_status(result)
    })

    output$apply_result <- shiny::renderUI({
      res <- apply_status()
      if (is.null(res)) return(NULL)
      if (isTRUE(res$ok)) {
        shiny::div(
          class = "alert alert-success", shiny::icon("check-circle"), " ",
          sprintf(i18n()$t("Applied: %d record column(s) and %d feature value(s) written."),
                  res$n_direct, res$n_feature)
        )
      } else {
        shiny::div(class = "alert alert-danger", shiny::icon("times-circle"), " ",
                   paste(i18n()$t("Update failed:"), res$err))
      }
    })

    invisible(NULL)
  })
}


# -----------------------------------------------------------------------------
# SMALL HELPERS
# -----------------------------------------------------------------------------

#' Are two scalar values the same, treating NA / "" / NULL as one absent value?
#' @keywords internal
.upd_same <- function(a, b) {
  norm <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (is.na(x)) return(NA_character_)
    x <- trimws(as.character(x))
    if (!nzchar(x)) return(NA_character_)
    # 12.50 and 12.5 are the same stored number, not an edit.
    num <- suppressWarnings(as.numeric(x))
    if (!is.na(num)) return(format(num, trim = TRUE, scientific = FALSE))
    x
  }
  identical(norm(a), norm(b))
}

#' Format a value for the diff table
#' @keywords internal
.upd_fmt <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("-")
  x <- as.character(x)
  if (!nzchar(trimws(x))) "-" else x
}
