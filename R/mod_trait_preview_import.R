# =============================================================================
# Taxa Traits Import - Preview & Import Module
#
# Validates mapped data, shows preview, detects duplicates, and executes
# import via add_sp_traits_measures() with interactive = FALSE.
# =============================================================================

#' Trait Preview & Import Module - UI
#' @param id Module namespace ID
#' @keywords internal
#' @export
mod_trait_preview_import_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("preview_header")),
    shiny::uiOutput(ns("citation_selector")),
    shiny::uiOutput(ns("basisofrecord_selector")),
    shiny::uiOutput(ns("measurementremarks_input")),
    shiny::hr(),

    # Loading spinner — visible immediately, hidden once prepared_data() resolves
    shiny::div(
      id = ns("loading_preview"),
      style = "padding: 60px 20px; text-align: center;",
      shiny::icon("circle-notch", class = "fa-spin",
                  style = "font-size: 48px; color: #007bff;"),
      shiny::h4("Computing preview...",
                style = "color: #495057; margin-top: 20px;"),
      shiny::p("This may take a few seconds for large datasets.",
               style = "color: #6c757d;"),
      shiny::div(
        style = paste0("display: inline-block; margin-top: 12px; padding: 10px 20px;",
                       " background: #d4edda; border-radius: 6px;",
                       " border-left: 4px solid #28a745;"),
        shiny::icon("check-circle", style = "color: #28a745;"),
        shiny::tags$strong(" Validation passed — your data is ready to import.",
                           style = "color: #155724;")
      )
    ),

    shiny::uiOutput(ns("preview_summary")),
    DT::DTOutput(ns("preview_table")),
    shiny::hr(),
    shiny::uiOutput(ns("import_controls")),
    shiny::uiOutput(ns("import_status"))
  )
}


#' Trait Preview & Import Module - Server
#'
#' @param id Module namespace ID
#' @param data Reactive returning uploaded data frame
#' @param mapping Reactive returning mapping result from mod_trait_column_mapping_server
#' @param pool Reactive returning database connection pool
#' @param i18n Reactive returning translator
#'
#' @return Reactive list with import_result
#'
#' @keywords internal
#' @export
mod_trait_preview_import_server <- function(id, data, mapping, pool, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Track import state --
    import_state <- shiny::reactiveValues(
      running = FALSE,
      result = NULL,
      dry_run_result = NULL
    )

    # Valid basisofrecord choices
    basis_choices <- c("LivingSpecimen", "PreservedSpecimen", "FossilSpecimen",
                       "literatureData", "traitDatabase", "expertKnowledge")

    # -- Check if basisofrecord is mapped as a column --
    basisofrecord_col <- shiny::reactive({
      m <- mapping()
      col <- names(m$metadata_cols)[m$metadata_cols == "basisofrecord"]
      if (length(col) == 1) col else NULL
    })

    # -- Unique basisofrecord values in the data (when column is mapped) --
    basis_unique_vals <- shiny::reactive({
      col <- basisofrecord_col()
      shiny::req(!is.null(col), data())
      unique(stats::na.omit(as.character(data()[[col]])))
    })

    # -- Auto-match user basis values to canonical choices (fuzzy) --
    basis_auto_match <- shiny::reactive({
      vals <- basis_unique_vals()
      setNames(sapply(vals, function(v) {
        # Exact (case-insensitive)
        m <- basis_choices[tolower(basis_choices) == tolower(v)]
        if (length(m) == 1) return(m)
        # Fuzzy
        dists <- utils::adist(tolower(v), tolower(basis_choices))[1, ]
        basis_choices[which.min(dists)]
      }), vals)
    })

    # -- Check if measurementremarks is mapped --
    needs_measurementremarks <- shiny::reactive({
      m <- mapping()
      !("measurementremarks" %in% m$metadata_cols)
    })

    # -------------------------------------------------------------------------
    # -- Citation selector --
    # -------------------------------------------------------------------------

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
          "SELECT id_citation, citation_key, authors, year, dataset_name
           FROM table_citations ORDER BY citation_key")
      }, error = function(e) {
        message("Could not fetch table_citations: ", e$message)
        data.frame(id_citation = integer(), citation_key = character(),
                   authors = character(), year = integer(),
                   dataset_name = character(), stringsAsFactors = FALSE)
      })
    })

    # Named vector for selectInput: label -> id_citation
    citation_choices <- shiny::reactive({
      df <- citations_df()
      if (nrow(df) == 0) return(c("-- No citations in database --" = ""))
      labels <- mapply(function(key, authors, year, ds) {
        auth_short <- if (!is.na(authors) && nchar(authors) > 0) {
          paste0(strsplit(authors, ",")[[1]][1], " et al.")
        } else ""
        yr <- if (!is.na(year)) paste0(" (", year, ")") else ""
        ds_str <- if (!is.na(ds) && nchar(ds) > 0) paste0(" [", ds, "]") else ""
        paste0(key, " — ", auth_short, yr, ds_str)
      }, df$citation_key, df$authors, df$year, df$dataset_name)
      c("-- None --" = "", setNames(as.character(df$id_citation), labels))
    })

    # UI panel
    output$citation_selector <- shiny::renderUI({
      shiny::div(
        style = "padding: 12px; background: #f0fff4; border-left: 4px solid #20c997; border-radius: 4px; margin-bottom: 15px;",
        shiny::fluidRow(
          shiny::column(8,
            shiny::tags$strong(
              shiny::icon("book", style = "color: #20c997;"),
              paste0(" ", i18n()$t("Citation (database/dataset source)"))
            ),
            shiny::p(
              i18n()$t("Select the citation for the database or dataset this import comes from. This is distinct from the 'reference' field which records the original source of each measurement."),
              style = "color: #6c757d; margin: 4px 0 8px 0; font-size: 12px;"
            ),
            shiny::selectInput(
              ns("selected_citation"),
              label = NULL,
              choices = citation_choices(),
              selected = "",
              width = "100%"
            )
          ),
          shiny::column(4,
            shiny::br(),
            shiny::actionButton(
              ns("btn_add_citation"),
              shiny::tagList(shiny::icon("plus"), i18n()$t("New citation")),
              class = "btn-outline-success btn-sm",
              style = "margin-top: 28px; width: 100%;"
            )
          )
        )
      )
    })

    # Resolved id_citation (integer or NA)
    selected_id_citation <- shiny::reactive({
      val <- input$selected_citation
      if (is.null(val) || val == "") return(NA_integer_)
      as.integer(val)
    })

    # -- New citation modal --
    shiny::observeEvent(input$btn_add_citation, {
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("plus-circle"),
                               paste0(" ", i18n()$t("Create New Citation"))),
        size = "l",
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
        shiny::updateSelectInput(session, "selected_citation",
          choices = citation_choices(),
          selected = as.character(
            citations_df()$id_citation[citations_df()$citation_key == key]
          )
        )
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("Error saving citation:"), e$message),
          type = "error"
        )
      })
    })

    # -- Header --
    output$preview_header <- shiny::renderUI({
      shiny::tagList(
        shiny::h4(
          shiny::icon("search"),
          i18n()$t("Preview & Import")
        ),
        shiny::p(
          i18n()$t("Review your data before importing. Use dry run to check for issues without writing to the database."),
          style = "color: #6c757d;"
        )
      )
    })

    # -- Basisofrecord UI: global selector or per-value lookup match --
    output$basisofrecord_selector <- shiny::renderUI({
      col <- basisofrecord_col()

      if (is.null(col)) {
        # No column mapped → global selector
        shiny::div(
          style = "padding: 10px; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px; margin-bottom: 15px;",
          shiny::icon("tag", style = "color: #856404;"),
          shiny::tags$strong(
            paste0(" ", i18n()$t("No 'basisofrecord' column mapped")),
            style = "color: #856404;"
          ),
          shiny::br(), shiny::br(),
          shiny::selectInput(
            ns("global_basisofrecord"),
            i18n()$t("Select basis of record for all rows:"),
            choices = basis_choices,
            selected = "LivingSpecimen",
            width = "300px"
          )
        )
      } else {
        # Column mapped → show lookup match per unique value
        vals <- basis_unique_vals()
        auto  <- basis_auto_match()

        rows <- lapply(vals, function(v) {
          shiny::fluidRow(
            style = "padding: 4px 0;",
            shiny::column(5, shiny::code(v)),
            shiny::column(1, shiny::icon("arrow-right", style = "color: #aaa;")),
            shiny::column(6,
              shiny::selectInput(
                ns(paste0("basis_map_", make.names(v))),
                label = NULL,
                choices = basis_choices,
                selected = auto[[v]],
                width = "100%"
              )
            )
          )
        })

        shiny::div(
          style = "padding: 10px; background: #e7f3ff; border-left: 4px solid #007bff; border-radius: 4px; margin-bottom: 15px;",
          shiny::icon("exchange-alt", style = "color: #0056b3;"),
          shiny::tags$strong(
            paste0(" ", i18n()$t("Match basisofrecord values")),
            style = "color: #0056b3;"
          ),
          shiny::p(
            i18n()$t("Map each value found in your data to a valid basis of record:"),
            style = "color: #6c757d; margin: 8px 0 4px 0;"
          ),
          do.call(shiny::tagList, rows)
        )
      }
    })

    # -- Resolve basisofrecord from current inputs --
    # Returns either NULL (will use column as-is after remap), a single string
    # (global for all rows), or a named vector (user_val -> canonical_val).
    resolved_basisofrecord <- shiny::reactive({
      col <- basisofrecord_col()
      if (is.null(col)) {
        # No column mapped: use the global selectInput value
        input$global_basisofrecord %||% "LivingSpecimen"
      } else {
        # Column mapped: read per-value mappings from input
        vals <- basis_unique_vals()
        mapping_vec <- setNames(
          sapply(vals, function(v) {
            val <- input[[paste0("basis_map_", make.names(v))]]
            if (is.null(val)) basis_auto_match()[[v]] else val
          }),
          vals
        )
        mapping_vec
      }
    })

    # -- Measurementremarks input (when not in data) --
    output$measurementremarks_input <- shiny::renderUI({
      shiny::req(needs_measurementremarks())
      shiny::div(
        style = "padding: 10px; background: #f8f9fa; border-radius: 4px; margin-bottom: 15px;",
        shiny::textInput(
          ns("global_measurementremarks"),
          i18n()$t("Measurement remarks (optional, applies to all rows):"),
          value = "",
          width = "100%"
        )
      )
    })

    # -- Build prepared data for preview --
    prepared_data <- shiny::reactive({
      shiny::req(data(), mapping())
      m <- mapping()
      shiny::req(m$valid)

      df <- data()
      idtax_col <- m$idtax_col
      trait_cols <- m$trait_cols   # named: user_col = trait_name
      meta_cols <- m$metadata_cols # named: user_col = db_col

      # Start building the preview
      n <- nrow(df)

      # Collect trait columns info
      traits_info  <- m$available_traits
      feature_cols <- m$feature_cols  # named: user_col = traitlist name

      # Build summary rows for both trait measures and features
      make_summary <- function(cols, role) {
        lapply(names(cols), function(user_col) {
          trait_name <- cols[user_col]
          n_values   <- sum(!is.na(df[[user_col]]))
          trait_info <- traits_info[traits_info$trait == trait_name, ]
          valuetype  <- if (nrow(trait_info) > 0) trait_info$valuetype[1] else "unknown"
          data.frame(role = role, user_column = user_col, trait_name = trait_name,
                     valuetype = valuetype, n_values = n_values, stringsAsFactors = FALSE)
        })
      }

      trait_summary_df <- do.call(rbind, c(
        make_summary(trait_cols, "trait measure"),
        make_summary(if (!is.null(feature_cols)) feature_cols else setNames(character(0), character(0)), "feature")
      ))

      # Build a "long format" preview of what will be inserted
      # Each row = one trait measurement for one taxon
      preview_rows <- list()
      for (user_col in names(trait_cols)) {
        trait_name <- trait_cols[user_col]
        trait_info <- traits_info[traits_info$trait == trait_name, ]
        valuetype <- if (nrow(trait_info) > 0) trait_info$valuetype[1] else "unknown"

        for (row_i in seq_len(n)) {
          val <- df[[user_col]][row_i]
          if (is.na(val)) next

          row_data <- list(
            idtax = df[[idtax_col]][row_i],
            trait = trait_name,
            value = as.character(val),
            valuetype = valuetype
          )

          # Add metadata
          for (meta_user_col in names(meta_cols)) {
            db_col <- meta_cols[meta_user_col]
            row_data[[db_col]] <- as.character(df[[meta_user_col]][row_i])
          }

          preview_rows <- c(preview_rows, list(as.data.frame(row_data, stringsAsFactors = FALSE)))
        }
      }

      if (length(preview_rows) == 0) {
        return(list(
          preview = data.frame(),
          trait_summary = trait_summary_df,
          n_total = 0
        ))
      }

      preview_df <- dplyr::bind_rows(preview_rows)

      list(
        preview = preview_df,
        trait_summary = trait_summary_df,
        n_total = nrow(preview_df)
      )
    })

    # -- Preview summary --
    output$preview_summary <- shiny::renderUI({
      shiny::req(prepared_data())
      shinyjs::hide("loading_preview")
      pd <- prepared_data()

      n_trait_rows   <- sum(pd$trait_summary$role == "trait measure")
      n_feature_rows <- sum(pd$trait_summary$role == "feature")

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(3, shiny::div(
            class = "card text-center p-3",
            style = "border-color: #007bff;",
            shiny::h3(pd$n_total, style = "color: #007bff; margin: 0;"),
            shiny::tags$small(i18n()$t("Total measurements to insert"))
          )),
          shiny::column(3, shiny::div(
            class = "card text-center p-3",
            style = "border-color: #28a745;",
            shiny::h3(n_trait_rows, style = "color: #28a745; margin: 0;"),
            shiny::tags$small(i18n()$t("Trait measures"))
          )),
          shiny::column(3, shiny::div(
            class = "card text-center p-3",
            style = "border-color: #6610f2;",
            shiny::h3(n_feature_rows, style = "color: #6610f2; margin: 0;"),
            shiny::tags$small(i18n()$t("Features of measures"))
          )),
          shiny::column(3, shiny::div(
            class = "card text-center p-3",
            style = "border-color: #6c757d;",
            shiny::h3(
              length(unique(pd$preview$idtax)),
              style = "color: #6c757d; margin: 0;"
            ),
            shiny::tags$small(i18n()$t("Unique taxa"))
          ))
        ),
        shiny::br(),
        # Per-trait summary
        shiny::div(
          style = "margin-bottom: 15px;",
          shiny::h5(i18n()$t("Per-trait breakdown:")),
          shiny::tableOutput(ns("trait_breakdown"))
        )
      )
    })

    # -- Per-trait breakdown table --
    output$trait_breakdown <- shiny::renderTable({
      shiny::req(prepared_data())
      prepared_data()$trait_summary
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # -- Preview table --
    output$preview_table <- DT::renderDT({
      shiny::req(prepared_data())
      pd <- prepared_data()
      shiny::req(nrow(pd$preview) > 0)

      DT::datatable(
        pd$preview,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftip"
        ),
        rownames = FALSE
      )
    })

    # -- Import controls --
    output$import_controls <- shiny::renderUI({
      shiny::req(prepared_data())
      pd <- prepared_data()
      shiny::req(pd$n_total > 0)

      running <- import_state$running

      shiny::div(
        style = "margin-top: 20px;",
        shiny::fluidRow(
          shiny::column(6,
            shiny::actionButton(
              ns("btn_dry_run"),
              shiny::tagList(
                shiny::icon("flask"),
                i18n()$t("Dry Run (Preview Only)")
              ),
              class = "btn-outline-primary btn-lg",
              style = "width: 100%;",
              disabled = running
            )
          ),
          shiny::column(6,
            shiny::actionButton(
              ns("btn_import"),
              shiny::tagList(
                shiny::icon("database"),
                i18n()$t("Import to Database")
              ),
              class = "btn-success btn-lg",
              style = "width: 100%;",
              disabled = running
            )
          )
        )
      )
    })

    # -- Execute dry run --
    shiny::observeEvent(input$btn_dry_run, {
      import_state$running <- TRUE
      import_state$result <- NULL

      tryCatch({
        result <- .execute_trait_import(
          data = data(),
          mapping = mapping(),
          pool = pool(),
          add_data = FALSE,
          basis_col = basisofrecord_col(),
          basis_resolved = resolved_basisofrecord(),
          measurementremarks = if (needs_measurementremarks()) input$global_measurementremarks else NULL,
          id_citation = selected_id_citation()
        )

        import_state$dry_run_result <- result
        import_state$result <- list(
          success = TRUE,
          dry_run = TRUE,
          message = paste0("Dry run complete: ", result$n_prepared, " measurements prepared for ",
                           length(result$traits_processed), " trait(s)")
        )
      }, error = function(e) {
        err_msg <- tryCatch(conditionMessage(e), error = function(x) "")
        if (nchar(trimws(err_msg)) == 0) err_msg <- paste0("(class: ", class(e)[1], ")")
        import_state$result <- list(
          success = FALSE,
          dry_run = TRUE,
          message = paste0("Dry run failed: ", err_msg)
        )
      })

      import_state$running <- FALSE
    })

    # -- Execute real import --
    shiny::observeEvent(input$btn_import, {
      # Confirm
      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("Confirm Import"),
        shiny::p(
          shiny::icon("exclamation-triangle", style = "color: #ffc107;"),
          i18n()$t("This will write data to the database. This action cannot be easily undone.")
        ),
        shiny::p(
          shiny::strong(
            sprintf("%d measurements will be inserted.", prepared_data()$n_total)
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton(i18n()$t("Cancel")),
          shiny::actionButton(
            ns("confirm_import"),
            i18n()$t("Confirm Import"),
            class = "btn-danger"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_import, {
      shiny::removeModal()
      import_state$running <- TRUE
      import_state$result <- NULL

      tryCatch({
        result <- .execute_trait_import(
          data = data(),
          mapping = mapping(),
          pool = pool(),
          add_data = TRUE,
          basis_col = basisofrecord_col(),
          basis_resolved = resolved_basisofrecord(),
          measurementremarks = if (needs_measurementremarks()) input$global_measurementremarks else NULL,
          id_citation = selected_id_citation()
        )

        import_state$result <- list(
          success = TRUE,
          dry_run = FALSE,
          message = paste0("Import complete: ", result$n_inserted, " measurements inserted for ",
                           length(result$traits_processed), " trait(s)")
        )
      }, error = function(e) {
        err_msg <- tryCatch(conditionMessage(e), error = function(x) "")
        if (nchar(trimws(err_msg)) == 0) err_msg <- paste0("(class: ", class(e)[1], ")")
        import_state$result <- list(
          success = FALSE,
          dry_run = FALSE,
          message = paste0("Import failed: ", err_msg)
        )
      })

      import_state$running <- FALSE
    })

    # -- Import status display --
    output$import_status <- shiny::renderUI({
      res <- import_state$result

      if (import_state$running) {
        return(shiny::div(
          style = "margin-top: 20px; padding: 15px; background: #e9ecef; border-radius: 8px; text-align: center;",
          shiny::icon("spinner", class = "fa-spin", style = "font-size: 24px; color: #007bff;"),
          shiny::br(),
          shiny::strong(i18n()$t("Processing..."))
        ))
      }

      if (is.null(res)) return(NULL)

      if (res$success) {
        style <- if (res$dry_run) {
          "margin-top: 20px; padding: 15px; background: #d4edda; border-left: 4px solid #28a745; border-radius: 4px;"
        } else {
          "margin-top: 20px; padding: 15px; background: #d4edda; border-left: 4px solid #28a745; border-radius: 4px;"
        }
        icon_name <- if (res$dry_run) "flask" else "check-circle"

        shiny::div(
          style = style,
          shiny::icon(icon_name, style = "color: #28a745; font-size: 20px;"),
          shiny::strong(res$message, style = "color: #155724; margin-left: 10px;")
        )
      } else {
        shiny::div(
          style = "margin-top: 20px; padding: 15px; background: #f8d7da; border-left: 4px solid #dc3545; border-radius: 4px;",
          shiny::icon("times-circle", style = "color: #dc3545; font-size: 20px;"),
          shiny::strong(res$message, style = "color: #721c24; margin-left: 10px;")
        )
      }
    })

    # -- Return reactive --
    shiny::reactive({
      import_state$result
    })
  })
}


# =============================================================================
# Helper: Execute trait import
#
# Calls add_sp_traits_measures() in non-interactive mode.
# =============================================================================

#' @keywords internal
.execute_trait_import <- function(data, mapping, pool, add_data = FALSE,
                                  basis_col = NULL,
                                  basis_resolved = NULL,
                                  measurementremarks = NULL,
                                  id_citation = NA_integer_) {

  idtax_col    <- mapping$idtax_col
  trait_cols   <- mapping$trait_cols    # named: user_col = traitlist name
  feature_cols <- mapping$feature_cols  # named: user_col = traitlist name (may be NULL)
  meta_cols    <- mapping$metadata_cols # named: user_col = db_col

  # Build new_data with standardized column names
  new_data <- data

  # Rename metadata columns to their DB names
  for (user_col in names(meta_cols)) {
    db_col <- meta_cols[user_col]
    if (user_col != db_col) {
      names(new_data)[names(new_data) == user_col] <- db_col
    }
  }

  # Handle basisofrecord:
  # - basis_col NULL → basis_resolved is a single string, passed as parameter
  # - basis_col set  → basis_resolved is a named vector (user_val -> canonical),
  #                    applied in-place (column was already renamed to "basisofrecord")
  if (is.null(basis_col)) {
    basisofrecord_param <- basis_resolved
  } else {
    if ("basisofrecord" %in% names(new_data) && !is.null(basis_resolved) && length(basis_resolved) > 0) {
      new_data$basisofrecord <- basis_resolved[as.character(new_data$basisofrecord)]
    }
    basisofrecord_param <- NULL
  }

  # Rename user trait columns to their traitlist names
  # (.link_sp_trait matches column names to traitlist)
  for (user_col in names(trait_cols)) {
    trait_name <- trait_cols[user_col]
    if (user_col != trait_name) {
      names(new_data)[names(new_data) == user_col] <- trait_name
    }
  }

  # Rename user feature columns to their traitlist names
  for (user_col in names(feature_cols)) {
    feat_name <- feature_cols[user_col]
    if (user_col != feat_name) {
      names(new_data)[names(new_data) == user_col] <- feat_name
    }
  }

  # Attach id_citation as a column so add_sp_traits_measures() picks it up
  # via .optional_column(). NA means NULL will be stored (no citation linked).
  new_data$id_citation <- if (!is.na(id_citation)) id_citation else NA_integer_

  traits_field  <- unname(trait_cols)
  features_field <- if (length(feature_cols) > 0) unname(feature_cols) else NULL

  result <- add_sp_traits_measures(
    new_data = new_data,
    traits_field = traits_field,
    features_field = features_field,
    idtax = idtax_col,
    add_data = add_data,
    ask_before_update = FALSE,
    basisofrecord = basisofrecord_param,
    measurementremarks = if (!is.null(measurementremarks) && nchar(trimws(measurementremarks)) > 0)
      measurementremarks else NULL,
    interactive = FALSE,
    con = pool
  )

  # Count results
  n_prepared <- sum(sapply(result$list_traits_add, function(x) {
    if (is.null(x)) 0 else nrow(x)
  }))

  list(
    result = result,
    n_prepared = n_prepared,
    n_inserted = if (add_data) n_prepared else 0,
    traits_processed = unname(trait_cols)
  )
}
