# Feature Wizard - Step 3: Define Multi-Stems
#
# Module for grouping individual stems into multi-stem trees.
# Two input methods:
#   1. Upload a pre-filled grouping table (e.g. from process_openforis_census())
#   2. Interactive grouping: select individuals per plot and group them visually

#' Feature Wizard Step 3: Multi-Stems - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step3_multi_stems_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("project-diagram"),
      i18n$t("Step 3: Define Multi-Stem Groups"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Group individual tags that belong to the same multi-stem tree. The first tag in each group becomes the parent stem."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 20px;"
    ),

    # ---- Input method selector ----
    shiny::radioButtons(
      ns("input_method"),
      label = i18n$t("Input method"),
      choices = stats::setNames(
        c("upload", "interactive"),
        c(i18n$t("Upload grouping table"), i18n$t("Interactive grouping"))
      ),
      selected = "upload",
      inline = TRUE
    ),

    shiny::hr(),

    # ---- Upload panel ----
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'upload'", ns("input_method")),
      .multi_stems_upload_ui(ns, i18n)
    ),

    # ---- Interactive panel ----
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'interactive'", ns("input_method")),
      .multi_stems_interactive_ui(ns, i18n)
    ),

    # ---- Current grouping table (shared by both methods) ----
    shiny::hr(),
    shiny::h4(
      shiny::icon("table"),
      i18n$t("Current Stem Groups"),
      style = "margin-top: 20px; margin-bottom: 15px;"
    ),
    shiny::uiOutput(ns("groups_summary")),

    # ---- Edit controls ----
    shiny::fluidRow(
      shiny::column(3,
        shiny::actionButton(
          ns("btn_remove_selected"),
          shiny::tagList(shiny::icon("trash-alt"), " ", i18n$t("Remove Selected Rows")),
          class = "btn-outline-danger btn-sm",
          style = "margin-bottom: 10px;"
        )
      ),
      shiny::column(3,
        shiny::actionButton(
          ns("btn_remove_unchanged"),
          shiny::tagList(shiny::icon("filter"), " ", i18n$t("Remove Unchanged")),
          class = "btn-outline-secondary btn-sm",
          style = "margin-bottom: 10px;"
        )
      ),
      shiny::column(3,
        shiny::actionButton(
          ns("btn_reassign_group"),
          shiny::tagList(shiny::icon("exchange-alt"), " ", i18n$t("Change Group Tag")),
          class = "btn-outline-warning btn-sm",
          style = "margin-bottom: 10px;"
        )
      ),
      shiny::column(3,
        shiny::actionButton(
          ns("btn_reset_upload"),
          shiny::tagList(shiny::icon("undo"), " ", i18n$t("Reset to Upload")),
          class = "btn-outline-info btn-sm",
          style = "margin-bottom: 10px;"
        )
      )
    ),

    # Modal placeholder for reassign
    shiny::uiOutput(ns("reassign_modal_ui")),

    DT::DTOutput(ns("groups_table")),

    shiny::p(
      shiny::icon("info-circle"), " ",
      i18n$t("The table shows all individuals from selected plots. Grouped rows can be edited; 'available' rows (white) can be added to a group via 'Change Group Tag'. 'Remove Selected' moves grouped rows back to available."),
      style = "color: #6c757d; font-size: 13px; margin-top: 8px;"
    )
  )
}


#' Upload sub-UI for multi-stems
#' @keywords internal
.multi_stems_upload_ui <- function(ns, i18n) {
  shiny::tagList(
    shiny::div(
      class = "alert alert-info",
      shiny::icon("info-circle"), " ",
      i18n$t("Upload an xlsx or csv file with at least 3 columns: plot_name, tag, and group_tag (the parent tag for each group). You can generate this table from process_openforis_census().")
    ),

    shiny::fluidRow(
      shiny::column(
        6,
        shiny::fileInput(
          ns("file_upload"),
          label = i18n$t("Upload grouping file"),
          accept = c(".xlsx", ".csv"),
          placeholder = i18n$t("Choose file...")
        )
      )
    ),

    # Column mapping
    shiny::uiOutput(ns("upload_mapping_ui"))
  )
}


#' Interactive sub-UI for multi-stems
#' @keywords internal
.multi_stems_interactive_ui <- function(ns, i18n) {
  shiny::tagList(
    shiny::div(
      class = "alert alert-info",
      shiny::icon("info-circle"), " ",
      i18n$t("Select a plot, then check the boxes next to tags you want to group together. Click 'Create Group' to form a multi-stem group from the selected tags.")
    ),

    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("plot_selector"),
          label = i18n$t("Select plot"),
          choices = NULL
        )
      ),
      shiny::column(
        4,
        shiny::actionButton(
          ns("btn_create_group"),
          shiny::tagList(shiny::icon("object-group"), " ", i18n$t("Create Group from Selected")),
          class = "btn-success",
          style = "margin-top: 25px;"
        )
      ),
      shiny::column(
        4,
        shiny::actionButton(
          ns("btn_remove_group"),
          shiny::tagList(shiny::icon("unlink"), " ", i18n$t("Remove Selected from Groups")),
          class = "btn-warning",
          style = "margin-top: 25px;"
        )
      )
    ),

    # Individuals table for the selected plot
    shiny::h5(i18n$t("Individuals in selected plot"), style = "margin-top: 15px;"),
    DT::DTOutput(ns("individuals_table"))
  )
}


#' Feature Wizard Step 3: Multi-Stems - Server
#'
#' @param id Module namespace ID
#' @param selected_plots Reactive containing selected plots data
#' @param operation_mode Reactive containing operation mode string
#' @param con Reactive returning database connection/pool
#' @param i18n Reactive returning translator object
#' @return Reactive containing list(data, config) for downstream steps
#' @keywords internal
#' @export
mod_feat_step3_multi_stems_server <- function(id, selected_plots, operation_mode,
                                               con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive storing the current grouping table
    # Columns: plot_name, tag, group_tag, stem_order, original_tax_name (optional)
    groups_rv <- shiny::reactiveVal(NULL)

    # All individuals from selected plots (queried from DB after upload)
    all_plot_individuals <- shiny::reactiveVal(NULL)

    # Result reactive (emitted to parent)
    result <- shiny::reactiveVal(NULL)

    # Individuals cache per plot (queried from DB)
    individuals_cache <- shiny::reactiveValues()

    # ====================================================================
    # Upload method
    # ====================================================================

    shiny::observeEvent(input$file_upload, {
      shiny::req(input$file_upload)
      file <- input$file_upload

      ext <- tolower(tools::file_ext(file$name))
      data <- tryCatch({
        if (ext == "csv") {
          utils::read.csv(file$datapath, stringsAsFactors = FALSE)
        } else {
          as.data.frame(readxl::read_excel(file$datapath))
        }
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("Error reading file:"), e$message),
          type = "error"
        )
        return(NULL)
      })

      if (is.null(data) || nrow(data) == 0) return()

      # Show mapping UI
      output$upload_mapping_ui <- shiny::renderUI({
        cols <- names(data)
        shiny::tagList(
          shiny::h5(i18n()$t("Map columns"), style = "margin-top: 15px;"),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::selectInput(
                ns("map_plot_name"), i18n()$t("Plot name column"),
                choices = cols,
                selected = .auto_detect_col(cols, c("plot_name", "plot"))
              )
            ),
            shiny::column(
              3,
              shiny::selectInput(
                ns("map_tag"), i18n()$t("Tag column"),
                choices = cols,
                selected = .auto_detect_col(cols, c("tag", "ind_num", "arbre"))
              )
            ),
            shiny::column(
              3,
              shiny::selectInput(
                ns("map_group_tag"), i18n()$t("Group tag column (parent)"),
                choices = cols,
                # Not stem_grouping: this dropdown wants the parent's tag,
                # while stem_grouping holds the parent's id_n
                selected = .auto_detect_col(cols, c("group_tag", "parent_tag"))
              )
            ),
            shiny::column(
              3,
              shiny::actionButton(
                ns("btn_apply_upload"),
                shiny::tagList(shiny::icon("check"), " ", i18n()$t("Apply Mapping")),
                class = "btn-primary",
                style = "margin-top: 25px;"
              )
            )
          )
        )
      })
    })

    # Store uploaded data temporarily
    uploaded_raw <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$file_upload, {
      shiny::req(input$file_upload)
      file <- input$file_upload
      ext <- tolower(tools::file_ext(file$name))
      data <- tryCatch({
        if (ext == "csv") {
          utils::read.csv(file$datapath, stringsAsFactors = FALSE)
        } else {
          as.data.frame(readxl::read_excel(file$datapath))
        }
      }, error = function(e) NULL)
      uploaded_raw(data)
    })

    shiny::observeEvent(input$btn_apply_upload, {
      shiny::req(uploaded_raw())
      data <- uploaded_raw()

      plot_col <- input$map_plot_name
      tag_col <- input$map_tag
      group_col <- input$map_group_tag

      if (is.null(plot_col) || is.null(tag_col) || is.null(group_col)) {
        shiny::showNotification(i18n()$t("Please map all three columns."), type = "warning")
        return()
      }

      mapped <- data.frame(
        plot_name = as.character(data[[plot_col]]),
        tag = suppressWarnings(as.numeric(data[[tag_col]])),
        group_tag = suppressWarnings(as.numeric(data[[group_col]])),
        stringsAsFactors = FALSE
      )

      # Add optional columns if present
      if ("stem_order" %in% names(data)) {
        mapped$stem_order <- suppressWarnings(as.integer(data$stem_order))
      }
      if ("original_tax_name" %in% names(data)) {
        mapped$original_tax_name <- as.character(data$original_tax_name)
      }
      if ("flag" %in% names(data)) {
        mapped$flag <- as.character(data$flag)
      }

      # Remove rows with NA tags
      mapped <- mapped[!is.na(mapped$tag) & !is.na(mapped$group_tag), , drop = FALSE]

      # Compute stem_order if not present
      if (!"stem_order" %in% names(mapped)) {
        mapped <- .compute_stem_order(mapped)
      }

      # Filter to selected plots only
      sel_plots <- selected_plots()
      if (!is.null(sel_plots) && "plot_name" %in% names(sel_plots)) {
        mapped <- mapped[mapped$plot_name %in% sel_plots$plot_name, , drop = FALSE]
      }

      if (nrow(mapped) == 0) {
        shiny::showNotification(
          i18n()$t("No valid stem groups found for selected plots."),
          type = "warning"
        )
        return()
      }

      # --- Join with database to resolve id_n and enrich with existing info ---
      tryCatch({
        pool <- con()
        shiny::req(pool)
        actual_con <- if (inherits(pool, "Pool")) pool::poolCheckout(pool) else pool
        on.exit({
          if (inherits(pool, "Pool")) pool::poolReturn(actual_con)
        }, add = TRUE)

        plot_names_sql <- paste(sprintf("'%s'", gsub("'", "''", unique(mapped$plot_name))), collapse = ",")

        db_inds <- DBI::dbGetQuery(actual_con, sprintf(
          "SELECT di.id_n, di.tag, di.stem_grouping, di.original_tax_name,
                  dlp.plot_name
           FROM data_individuals di
           JOIN data_liste_plots dlp ON di.id_table_liste_plots_n = dlp.id_liste_plots
           WHERE dlp.plot_name IN (%s)
           ORDER BY dlp.plot_name, di.tag",
          plot_names_sql
        ))

        # Store all plot individuals for the full preview table
        all_plot_individuals(db_inds)

        if (nrow(db_inds) > 0) {
          # Coerce tag types to match for comparison
          db_inds$tag_char <- as.character(db_inds$tag)

          # Resolve each row: id_n, group_id_n, existing grouping
          mapped$id_n <- NA_integer_
          mapped$group_id_n <- NA_integer_
          mapped$db_exists <- FALSE
          mapped$db_current_group <- NA_character_
          if (!"original_tax_name" %in% names(mapped)) mapped$original_tax_name <- NA_character_

          for (i in seq_len(nrow(mapped))) {
            tag_char <- as.character(mapped$tag[i])
            pl <- mapped$plot_name[i]

            # Resolve this tag's id_n
            db_row <- db_inds[db_inds$tag_char == tag_char & db_inds$plot_name == pl, , drop = FALSE]
            if (nrow(db_row) > 0) {
              mapped$id_n[i] <- db_row$id_n[1]
              mapped$db_exists[i] <- TRUE

              # Existing stem_grouping → resolve to parent tag for display
              sg <- db_row$stem_grouping[1]
              if (!is.na(sg)) {
                parent_row <- db_inds[db_inds$id_n == sg, , drop = FALSE]
                if (nrow(parent_row) > 0) {
                  mapped$db_current_group[i] <- as.character(parent_row$tag[1])
                } else {
                  mapped$db_current_group[i] <- paste0("id:", sg)
                }
              }

              # Fill taxon name from DB if missing
              if (is.na(mapped$original_tax_name[i]) || mapped$original_tax_name[i] == "") {
                if (!is.na(db_row$original_tax_name[1])) {
                  mapped$original_tax_name[i] <- db_row$original_tax_name[1]
                }
              }
            }

            # Resolve group_tag's id_n (the parent)
            gt_char <- as.character(mapped$group_tag[i])
            parent_match <- db_inds[db_inds$tag_char == gt_char & db_inds$plot_name == pl, , drop = FALSE]
            if (nrow(parent_match) > 0) {
              mapped$group_id_n[i] <- parent_match$id_n[1]
            }
          }

          # Determine status for each row
          mapped$status <- ifelse(
            !mapped$db_exists,
            "not_in_db",
            ifelse(
              is.na(mapped$db_current_group),
              "new_grouping",
              ifelse(
                as.character(mapped$group_tag) == mapped$db_current_group,
                "unchanged",
                "changed"
              )
            )
          )
          # Parent stems (tag == group_tag) are "parent"
          is_parent <- mapped$tag == mapped$group_tag
          mapped$status[is_parent & mapped$status %in% c("new_grouping", "unchanged")] <- "parent"
        }
      }, error = function(e) {
        cli::cli_alert_warning("Could not enrich with DB data: {e$message}")
      })

      groups_rv(mapped)

      n_groups <- length(unique(paste(mapped$plot_name, mapped$group_tag)))
      shiny::showNotification(
        sprintf(i18n()$t("Loaded %d stems in %d groups"), nrow(mapped), n_groups),
        type = "message"
      )
    })

    # ====================================================================
    # Interactive method
    # ====================================================================

    # Populate plot selector from selected plots
    # Re-trigger when switching to interactive mode so the input exists in DOM
    shiny::observe({
      shiny::req(operation_mode() == "define_multi_stems")
      shiny::req(input$input_method == "interactive")
      sel <- selected_plots()
      shiny::req(sel)
      plots <- sort(unique(sel$plot_name))
      shiny::updateSelectInput(session, "plot_selector", choices = plots)
    })

    # Query individuals for selected plot
    plot_individuals <- shiny::reactive({
      shiny::req(input$plot_selector)
      plot <- input$plot_selector
      pool <- con()
      shiny::req(pool)

      # Check cache
      if (!is.null(individuals_cache[[plot]])) {
        return(individuals_cache[[plot]])
      }

      actual_con <- if (inherits(pool, "Pool")) pool::poolCheckout(pool) else pool
      on.exit({
        if (inherits(pool, "Pool")) pool::poolReturn(actual_con)
      })

      tryCatch({
        query <- sprintf(
          "SELECT di.id_n, di.tag, di.stem_grouping,
                  dlp.plot_name, di.original_tax_name AS taxon_name
           FROM data_individuals di
           JOIN data_liste_plots dlp ON di.id_table_liste_plots_n = dlp.id_liste_plots
           WHERE dlp.plot_name = '%s'
           ORDER BY di.tag",
          gsub("'", "''", plot)
        )
        res <- DBI::dbGetQuery(actual_con, query)
        individuals_cache[[plot]] <- res
        res
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("Error querying individuals:"), e$message),
          type = "error"
        )
        data.frame(id_n = integer(0), tag = numeric(0), stem_grouping = integer(0),
                   plot_name = character(0), taxon_name = character(0))
      })
    })

    # Render individuals table with checkboxes and group info
    output$individuals_table <- DT::renderDT({
      ind <- plot_individuals()
      shiny::req(nrow(ind) > 0)

      # Add current group info from groups_rv
      current_groups <- groups_rv()
      ind$current_group <- NA_real_

      if (!is.null(current_groups)) {
        plot <- input$plot_selector
        plot_groups <- current_groups[current_groups$plot_name == plot, , drop = FALSE]
        match_idx <- match(ind$tag, plot_groups$tag)
        ind$current_group <- plot_groups$group_tag[match_idx]
      }

      # Resolve existing stem_grouping (id_n) to parent tag for display
      parent_tag_db <- rep("", nrow(ind))
      has_grouping <- !is.na(ind$stem_grouping)
      if (any(has_grouping)) {
        parent_idx <- match(ind$stem_grouping[has_grouping], ind$id_n)
        parent_tag_db[has_grouping] <- ifelse(
          !is.na(parent_idx),
          as.character(ind$tag[parent_idx]),
          paste0("id:", ind$stem_grouping[has_grouping])  # fallback if parent not in same plot
        )
      }

      display <- data.frame(
        tag = ind$tag,
        taxon = ind$taxon_name,
        db_parent_tag = parent_tag_db,
        new_group = ifelse(is.na(ind$current_group), "", as.character(ind$current_group)),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        display,
        selection = "multiple",
        options = list(
          pageLength = 12,
          lengthMenu = c(10, 12, 15, 25, 50),
          order = list(list(0, "asc"))
        ),
        rownames = FALSE
      )
    })

    # Create group from selected rows
    shiny::observeEvent(input$btn_create_group, {
      sel_rows <- input$individuals_table_rows_selected
      if (is.null(sel_rows) || length(sel_rows) < 2) {
        shiny::showNotification(
          i18n()$t("Select at least 2 individuals to create a group."),
          type = "warning"
        )
        return()
      }

      ind <- plot_individuals()
      selected_ind <- ind[sel_rows, , drop = FALSE]
      plot <- input$plot_selector

      # First selected tag is the parent (lowest tag number)
      tags <- sort(selected_ind$tag)
      parent_tag <- tags[1]

      new_rows <- data.frame(
        plot_name = rep(plot, length(tags)),
        tag = tags,
        group_tag = rep(parent_tag, length(tags)),
        stem_order = seq_along(tags),
        original_tax_name = selected_ind$taxon_name[match(tags, selected_ind$tag)],
        stringsAsFactors = FALSE
      )

      # Merge with existing groups
      current <- groups_rv()
      if (!is.null(current)) {
        # Remove these tags from any existing group
        current <- current[!(current$plot_name == plot & current$tag %in% tags), , drop = FALSE]
        updated <- rbind(current, new_rows)
      } else {
        updated <- new_rows
      }

      groups_rv(updated)

      shiny::showNotification(
        sprintf(i18n()$t("Created group with parent tag %s (%d stems)"),
                parent_tag, length(tags)),
        type = "message"
      )
    })

    # Remove selected from groups
    shiny::observeEvent(input$btn_remove_group, {
      sel_rows <- input$individuals_table_rows_selected
      if (is.null(sel_rows) || length(sel_rows) == 0) {
        shiny::showNotification(
          i18n()$t("Select individuals to remove from groups."),
          type = "warning"
        )
        return()
      }

      ind <- plot_individuals()
      tags_to_remove <- ind$tag[sel_rows]
      plot <- input$plot_selector

      current <- groups_rv()
      if (is.null(current)) return()

      # Remove selected tags
      updated <- current[!(current$plot_name == plot & current$tag %in% tags_to_remove), , drop = FALSE]

      # Also remove any groups that now have only 1 member
      group_keys <- paste(updated$plot_name, updated$group_tag)
      group_counts <- table(group_keys)
      singleton_keys <- names(group_counts[group_counts < 2])
      updated <- updated[!group_keys %in% singleton_keys, , drop = FALSE]

      if (nrow(updated) == 0) updated <- NULL

      groups_rv(updated)

      shiny::showNotification(
        i18n()$t("Removed selected stems from groups."),
        type = "message"
      )
    })

    # ====================================================================
    # Groups display (shared by both methods)
    # ====================================================================

    output$groups_summary <- shiny::renderUI({
      grp <- groups_rv()
      if (is.null(grp) || nrow(grp) == 0) {
        return(shiny::div(
          class = "alert alert-secondary",
          i18n()$t("No stem groups defined yet.")
        ))
      }

      n_groups <- length(unique(paste(grp$plot_name, grp$group_tag)))
      n_stems <- nrow(grp)
      n_flagged <- if ("flag" %in% names(grp)) sum(!is.na(grp$flag) & grp$flag != "") else 0L

      # Count all individuals from selected plots
      all_inds <- all_plot_individuals()
      n_all <- if (!is.null(all_inds)) nrow(all_inds) else 0L

      # Status counts (if enriched with DB data)
      status_ui <- NULL
      if ("status" %in% names(grp)) {
        n_unchanged <- sum(grp$status == "unchanged", na.rm = TRUE)
        n_changed   <- sum(grp$status == "changed", na.rm = TRUE)
        n_new       <- sum(grp$status == "new_grouping", na.rm = TRUE)
        n_parent    <- sum(grp$status == "parent", na.rm = TRUE)
        n_missing   <- sum(grp$status == "not_in_db", na.rm = TRUE)
        n_available <- n_all - n_stems

        status_parts <- list()
        if (n_new > 0) status_parts <- c(status_parts, list(
          shiny::tags$span(style = "color: #28a745; margin-right: 12px;",
            shiny::icon("plus-circle"), sprintf(" %d new", n_new))
        ))
        if (n_changed > 0) status_parts <- c(status_parts, list(
          shiny::tags$span(style = "color: #ffc107; margin-right: 12px;",
            shiny::icon("exchange-alt"), sprintf(" %d changed", n_changed))
        ))
        if (n_unchanged > 0) status_parts <- c(status_parts, list(
          shiny::tags$span(style = "color: #6c757d; margin-right: 12px;",
            shiny::icon("equals"), sprintf(" %d unchanged", n_unchanged))
        ))
        if (n_missing > 0) status_parts <- c(status_parts, list(
          shiny::tags$span(style = "color: #dc3545; margin-right: 12px;",
            shiny::icon("exclamation-circle"), sprintf(" %d not in DB", n_missing))
        ))
        if (n_available > 0) status_parts <- c(status_parts, list(
          shiny::tags$span(style = "color: #17a2b8; margin-right: 12px;",
            shiny::icon("user"), sprintf(" %d available", n_available))
        ))

        status_ui <- shiny::div(
          style = "margin-top: 8px;",
          shiny::tags$small(status_parts)
        )
      }

      shiny::div(
        class = "alert alert-success",
        shiny::div(
          shiny::icon("check-circle"), " ",
          sprintf(i18n()$t("%d group(s), %d stems in groups, %d total individuals in plots"),
                  n_groups, n_stems, n_all),
          if (n_flagged > 0) {
            shiny::span(
              style = "color: #dc3545; margin-left: 15px;",
              shiny::icon("exclamation-triangle"), " ",
              sprintf(i18n()$t("%d flagged — review before import"), n_flagged)
            )
          }
        ),
        status_ui
      )
    })

    # Build the combined display: uploaded groups + all other individuals from plots
    combined_display <- shiny::reactive({
      grp <- groups_rv()
      all_inds <- all_plot_individuals()

      if (is.null(grp) || nrow(grp) == 0) return(NULL)

      # Start with the grouped rows
      display <- grp

      # If we have all individuals from the DB, add the ones NOT in the upload
      if (!is.null(all_inds) && nrow(all_inds) > 0) {
        # Tags already in the groups
        grouped_keys <- paste(as.character(grp$plot_name), as.character(grp$tag))

        # All DB individuals as keys
        db_keys <- paste(as.character(all_inds$plot_name), as.character(all_inds$tag))
        not_in_upload <- !db_keys %in% grouped_keys

        if (any(not_in_upload)) {
          remaining <- all_inds[not_in_upload, , drop = FALSE]

          # Build data frame with same columns as display
          extra <- data.frame(
            plot_name = as.character(remaining$plot_name),
            tag = suppressWarnings(as.numeric(remaining$tag)),
            group_tag = NA_real_,
            stem_order = NA_integer_,
            stringsAsFactors = FALSE
          )

          # Fill optional columns that exist in display
          if ("original_tax_name" %in% names(display)) {
            extra$original_tax_name <- if ("original_tax_name" %in% names(remaining)) {
              as.character(remaining$original_tax_name)
            } else NA_character_
          }
          if ("id_n" %in% names(display)) {
            extra$id_n <- remaining$id_n
          }
          if ("group_id_n" %in% names(display)) {
            extra$group_id_n <- NA_integer_
          }
          if ("db_exists" %in% names(display)) {
            extra$db_exists <- TRUE
          }
          if ("db_current_group" %in% names(display)) {
            # Resolve existing stem_grouping to parent tag
            extra$db_current_group <- NA_character_
            for (j in seq_len(nrow(remaining))) {
              sg <- remaining$stem_grouping[j]
              if (!is.na(sg)) {
                parent_row <- all_inds[all_inds$id_n == sg, , drop = FALSE]
                if (nrow(parent_row) > 0) {
                  extra$db_current_group[j] <- as.character(parent_row$tag[1])
                } else {
                  extra$db_current_group[j] <- paste0("id:", sg)
                }
              }
            }
          }
          extra$status <- "available"
          if ("flag" %in% names(display)) {
            extra$flag <- NA_character_
          }

          # Align columns
          for (col in names(display)) {
            if (!col %in% names(extra)) extra[[col]] <- NA
          }
          extra <- extra[, names(display), drop = FALSE]

          display <- rbind(display, extra)
        }
      }

      # Sort by plot_name then tag so available rows sit next to their neighbours
      display <- display[order(display$plot_name, display$tag), , drop = FALSE]
      rownames(display) <- NULL

      display
    })

    output$groups_table <- DT::renderDT({
      display <- combined_display()
      shiny::req(display, nrow(display) > 0)

      display_cols <- intersect(
        c("plot_name", "tag", "group_tag", "stem_order", "original_tax_name",
          "db_current_group", "status", "flag"),
        names(display)
      )

      dt <- DT::datatable(
        display[, display_cols, drop = FALSE],
        selection = "multiple",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "400px",
          order = list(list(0, "asc"), list(1, "asc")),
          ordering = FALSE
        ),
        rownames = FALSE
      )

      # Color-code by status
      if ("status" %in% display_cols) {
        dt <- dt |>
          DT::formatStyle(
            "status",
            backgroundColor = DT::styleEqual(
              c("new_grouping", "changed", "unchanged", "parent", "not_in_db", "available"),
              c("#d4edda",       "#fff3cd",  "#f8f9fa",    "#e2e3e5", "#f8d7da", "#ffffff")
            )
          )
      }

      # Highlight flag column
      if ("flag" %in% display_cols) {
        dt <- dt |>
          DT::formatStyle(
            "flag",
            backgroundColor = DT::styleEqual(
              c(NA, ""),
              c("transparent", "transparent"),
              default = "#fff3cd"
            )
          )
      }

      dt
    })

    # ====================================================================
    # Edit controls for the groups table
    # ====================================================================

    # Store the original upload for reset
    upload_snapshot <- shiny::reactiveVal(NULL)
    shiny::observe({
      grp <- groups_rv()
      if (!is.null(grp) && is.null(upload_snapshot())) {
        upload_snapshot(grp)
      }
    })

    # Remove selected rows (from groups — moves them back to "available")
    shiny::observeEvent(input$btn_remove_selected, {
      sel <- input$groups_table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        shiny::showNotification(i18n()$t("Select rows to remove first."), type = "warning")
        return()
      }

      display <- combined_display()
      grp <- groups_rv()
      shiny::req(display, grp)

      # Identify which selected rows are in groups (not "available")
      sel_statuses <- display$status[sel]
      sel_grouped <- sel[!sel_statuses %in% "available"]

      if (length(sel_grouped) == 0) {
        shiny::showNotification(i18n()$t("Selected rows are not in any group."), type = "warning")
        return()
      }

      # Map display rows back to groups_rv rows by tag+plot_name key
      sel_keys <- paste(as.character(display$plot_name[sel_grouped]),
                        as.character(display$tag[sel_grouped]))
      grp_keys <- paste(as.character(grp$plot_name), as.character(grp$tag))
      rows_to_remove <- which(grp_keys %in% sel_keys)

      updated <- grp[-rows_to_remove, , drop = FALSE]

      # Remove singleton groups
      if (nrow(updated) > 0) {
        group_keys <- paste(updated$plot_name, updated$group_tag)
        group_counts <- table(group_keys)
        singleton_keys <- names(group_counts[group_counts < 2])
        if (length(singleton_keys) > 0) {
          n_singletons <- sum(group_keys %in% singleton_keys)
          updated <- updated[!group_keys %in% singleton_keys, , drop = FALSE]
          shiny::showNotification(
            sprintf(i18n()$t("Also removed %d stems that became singleton groups."), n_singletons),
            type = "message"
          )
        }
      }

      if (nrow(updated) == 0) updated <- NULL

      # Recompute stem_order
      if (!is.null(updated)) updated <- .compute_stem_order(updated)

      groups_rv(updated)

      shiny::showNotification(
        sprintf(i18n()$t("Removed %d row(s) from groups."), length(sel_grouped)),
        type = "message"
      )
    })

    # Remove unchanged rows (keep only new/changed groupings)
    shiny::observeEvent(input$btn_remove_unchanged, {
      grp <- groups_rv()
      shiny::req(grp)
      if (!"status" %in% names(grp)) {
        shiny::showNotification(i18n()$t("No status information available."), type = "warning")
        return()
      }

      updated <- grp[!grp$status %in% c("unchanged"), , drop = FALSE]

      # Remove singleton groups
      if (nrow(updated) > 0) {
        group_keys <- paste(updated$plot_name, updated$group_tag)
        group_counts <- table(group_keys)
        singleton_keys <- names(group_counts[group_counts < 2])
        updated <- updated[!group_keys %in% singleton_keys, , drop = FALSE]
      }

      if (nrow(updated) == 0) updated <- NULL
      if (!is.null(updated)) updated <- .compute_stem_order(updated)

      n_removed <- nrow(grp) - (if (!is.null(updated)) nrow(updated) else 0L)
      groups_rv(updated)

      shiny::showNotification(
        sprintf(i18n()$t("Removed %d unchanged row(s)."), n_removed),
        type = "message"
      )
    })

    # Change group_tag for selected rows via modal (also adds "available" rows to groups)
    shiny::observeEvent(input$btn_reassign_group, {
      sel <- input$groups_table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        shiny::showNotification(i18n()$t("Select rows to reassign first."), type = "warning")
        return()
      }

      display <- combined_display()
      grp <- groups_rv()
      shiny::req(display)

      # Show selected info from the display table
      selected_tags <- paste(display$tag[sel], collapse = ", ")

      # Build choices: existing parent tags in the same plots
      sel_plots <- unique(as.character(display$plot_name[sel]))
      existing_parents <- if (!is.null(grp)) {
        sort(unique(grp$group_tag[grp$plot_name %in% sel_plots]))
      } else {
        numeric(0)
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("Change Group Tag"),
        shiny::p(
          sprintf(i18n()$t("Reassigning %d stem(s): tags %s"), length(sel), selected_tags)
        ),
        shiny::numericInput(
          ns("new_group_tag_value"),
          label = i18n()$t("New group tag (parent tag):"),
          value = if (length(existing_parents) > 0) existing_parents[1] else NA,
          min = 0
        ),
        if (length(existing_parents) > 0) {
          shiny::p(
            shiny::tags$small(
              style = "color: #6c757d;",
              i18n()$t("Existing parent tags in these plots:"), " ",
              paste(existing_parents, collapse = ", ")
            )
          )
        },
        footer = shiny::tagList(
          shiny::modalButton(i18n()$t("Cancel")),
          shiny::actionButton(ns("btn_confirm_reassign"),
            i18n()$t("Apply"),
            class = "btn-primary"
          )
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_reassign, {
      shiny::removeModal()

      new_gt <- input$new_group_tag_value
      if (is.null(new_gt) || is.na(new_gt)) {
        shiny::showNotification(i18n()$t("Please enter a valid tag number."), type = "warning")
        return()
      }

      sel <- input$groups_table_rows_selected
      display <- combined_display()
      grp <- groups_rv()
      shiny::req(display, sel)

      # Separate selected rows into already-grouped vs available
      sel_statuses <- display$status[sel]
      sel_in_groups <- sel[!sel_statuses %in% "available"]
      sel_available <- sel[sel_statuses %in% "available"]

      # Update existing grouped rows
      if (length(sel_in_groups) > 0 && !is.null(grp)) {
        sel_keys <- paste(as.character(display$plot_name[sel_in_groups]),
                          as.character(display$tag[sel_in_groups]))
        grp_keys <- paste(as.character(grp$plot_name), as.character(grp$tag))
        match_idx <- which(grp_keys %in% sel_keys)
        grp$group_tag[match_idx] <- new_gt

        # Resolve group_id_n for reassigned rows
        all_inds <- all_plot_individuals()
        if (!is.null(all_inds) && nrow(all_inds) > 0) {
          gt_char <- as.character(new_gt)
          for (mi in match_idx) {
            pl <- as.character(grp$plot_name[mi])
            parent_match <- all_inds[as.character(all_inds$tag) == gt_char &
                                     all_inds$plot_name == pl, , drop = FALSE]
            if (nrow(parent_match) > 0 && "group_id_n" %in% names(grp)) {
              grp$group_id_n[mi] <- parent_match$id_n[1]
            }
          }
        }
      }

      # Add "available" rows as new group members
      if (length(sel_available) > 0) {
        avail_rows <- display[sel_available, , drop = FALSE]
        avail_rows$group_tag <- new_gt
        avail_rows$status <- "new_grouping"

        # Resolve group_id_n for new rows
        all_inds <- all_plot_individuals()
        if (!is.null(all_inds) && nrow(all_inds) > 0) {
          gt_char <- as.character(new_gt)
          if (!"group_id_n" %in% names(avail_rows)) avail_rows$group_id_n <- NA_integer_
          for (j in seq_len(nrow(avail_rows))) {
            pl <- as.character(avail_rows$plot_name[j])
            parent_match <- all_inds[as.character(all_inds$tag) == gt_char &
                                     all_inds$plot_name == pl, , drop = FALSE]
            if (nrow(parent_match) > 0) {
              avail_rows$group_id_n[j] <- parent_match$id_n[1]
            }
          }
        }

        if (!is.null(grp)) {
          # Align columns
          for (col in names(grp)) {
            if (!col %in% names(avail_rows)) avail_rows[[col]] <- NA
          }
          avail_rows <- avail_rows[, names(grp), drop = FALSE]
          grp <- rbind(grp, avail_rows)
        } else {
          grp <- avail_rows
        }
      }

      if (is.null(grp) || nrow(grp) == 0) return()

      # Recompute stem_order and update status
      grp <- .compute_stem_order(grp)
      if ("status" %in% names(grp)) {
        # Recalculate status for all affected rows
        for (i in seq_len(nrow(grp))) {
          if (as.character(grp$group_tag[i]) == as.character(new_gt) ||
              grp$status[i] == "new_grouping") {
            grp$status[i] <- if (!isTRUE(grp$db_exists[i])) {
              "not_in_db"
            } else if (is.na(grp$db_current_group[i])) {
              "new_grouping"
            } else if (as.character(grp$group_tag[i]) == grp$db_current_group[i]) {
              "unchanged"
            } else {
              "changed"
            }
          }
        }
        # Parents
        is_parent <- grp$tag == grp$group_tag
        grp$status[is_parent & grp$status %in% c("new_grouping", "unchanged")] <- "parent"
      }

      groups_rv(grp)

      shiny::showNotification(
        sprintf(i18n()$t("Reassigned %d stem(s) to group tag %s."), length(sel), new_gt),
        type = "message"
      )
    })

    # Reset to original upload
    shiny::observeEvent(input$btn_reset_upload, {
      snap <- upload_snapshot()
      if (is.null(snap)) {
        shiny::showNotification(i18n()$t("No upload to reset to."), type = "warning")
        return()
      }
      groups_rv(snap)
      shiny::showNotification(i18n()$t("Reset to original upload."), type = "message")
    })

    # ====================================================================
    # Emit result for downstream steps
    # ====================================================================

    shiny::observe({
      grp <- groups_rv()
      if (is.null(grp) || nrow(grp) == 0) {
        result(NULL)
        return()
      }

      # Only emit non-parent rows (parent doesn't need stem_grouping update)
      # But include all rows for validation; step 6 will filter
      result(list(
        data = grp,
        config = list(
          mode = "define_multi_stems",
          type = "multi_stems"
        )
      ))
    })

    return(shiny::reactive(result()))
  })
}


# ===========================================================================
# Helpers
# ===========================================================================

#' Auto-detect a column name from a list of candidates
#' @keywords internal
.auto_detect_col <- function(col_names, candidates) {
  lower <- tolower(col_names)
  for (c in candidates) {
    idx <- which(lower == tolower(c))
    if (length(idx) > 0) return(col_names[idx[1]])
  }
  col_names[1]  # fallback to first column
}


#' Compute stem_order within each group
#' @keywords internal
.compute_stem_order <- function(data) {
  data$stem_order <- NA_integer_
  groups <- unique(paste(data$plot_name, data$group_tag))
  for (g in groups) {
    mask <- paste(data$plot_name, data$group_tag) == g
    data$stem_order[mask] <- rank(data$tag[mask], ties.method = "first")
  }
  data
}
