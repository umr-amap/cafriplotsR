#' Taxa Add Module - UI
#'
#' UI component for adding new taxonomic entries
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_taxa_add_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("add_ui"))
  )
}

#' Taxa Add Module - Server
#'
#' Server logic for adding new taxonomic entries
#'
#' @param id Module namespace ID
#' @param pool Reactive returning taxa database connection pool
#' @param has_write_permission Reactive returning TRUE if user can write
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL
#'
#' @keywords internal
#' @export
mod_taxa_add_server <- function(id, pool, pool_main = NULL, has_write_permission, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      tropicos_results = NULL,
      backbone_results = NULL,          # Hits from every backbone, stacked
      backbone_selected = character(0), # external_id per backbone code, to be linked
      backbone_synonymy_candidates = NULL, # Internal taxa sharing the same accepted name
      current_step = 1,
      form_data = list(),
      new_taxon_id = NULL,
      existing_check = NULL,
      growth_form_data = NULL,
      order_required = FALSE,
      class_required = FALSE,
      existing_taxon_matches = NULL,  # For multiple matches when setting existing as synonym
      accepted_taxon_matches = NULL,  # For multiple matches when setting new as synonym
      tropicos_key_set = FALSE        # Bumped when a key is entered, to redraw the panel
    )

    # Initialize growth form selector module
    growth_form_module <- mod_growth_form_selector_server(
      "growth_form",
      pool = pool,
      i18n = i18n
    )

    # Every backbone this database registers, as code -> full name. Backbones
    # not yet offered as a source of names are included: an identifier is worth
    # recording before the backbone starts supplying names.
    backbone_choices <- shiny::reactive({
      bb <- tryCatch(
        list_backbones(con_taxa = pool(), name_sources_only = FALSE),
        error = function(e) {
          cli::cli_alert_warning("Could not list backbones: {e$message}")
          NULL
        }
      )
      if (is.null(bb) || nrow(bb) == 0) return(character(0))
      stats::setNames(bb$code, bb$name)
    })

    # Full name of one backbone, falling back to its code
    backbone_label <- function(code) {
      if (is.null(code) || length(code) != 1L || is.na(code)) {
        return(i18n()$t("Taxonomic backbone"))
      }
      choices <- backbone_choices()
      hit <- names(choices)[choices == code]
      if (length(hit) == 0) as.character(code) else hit[1]
    }

    # Main UI
    output$add_ui <- shiny::renderUI({
      if (!has_write_permission()) {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("lock"),
            " ",
            i18n()$t("Write Access:"),
            " ",
            i18n()$t("Only users with INSERT privileges can modify data")
          )
        )
      }

      shiny::tagList(
        shiny::h4(i18n()$t("Add New Taxon")),

        # Step indicator
        shiny::div(
          class = "alert alert-info",
          style = "display: flex; justify-content: space-around;",
          shiny::div(
            style = if (rv$current_step == 1) "font-weight: bold; color: #007bff;" else "",
            "1. ", i18n()$t("Search in backbones")
          ),
          shiny::div(
            style = if (rv$current_step == 2) "font-weight: bold; color: #007bff;" else "",
            "2. ", i18n()$t("Taxonomic Info")
          ),
          shiny::div(
            style = if (rv$current_step == 3) "font-weight: bold; color: #007bff;" else "",
            "3. ", i18n()$t("Growth Form")
          ),
          shiny::div(
            style = if (rv$current_step == 4) "font-weight: bold; color: #007bff;" else "",
            "4. ", i18n()$t("Review & Submit")
          ),
          if (rv$current_step == 5) {
            shiny::div(
              style = "font-weight: bold; color: #007bff;",
              "5. ", i18n()$t("Set Synonymy (Optional)")
            )
          }
        ),

        shiny::hr(),

        # Step panels
        shiny::uiOutput(ns("step_panel"))
      )
    })

    # Step panels
    output$step_panel <- shiny::renderUI({
      switch(
        as.character(rv$current_step),
        "1" = step1_tropicos_ui(ns, i18n),
        "2" = step2_taxonomy_ui(ns, i18n, rv),
        "3" = step3_growth_ui(ns, i18n, rv),
        "4" = step4_review_ui(ns, i18n, rv),
        "5" = step5_synonymy_ui(ns, i18n, rv)
      )
    })

    # Step 1: search every backbone at once, and Tropicos if a key is available
    step1_tropicos_ui <- function(ns, i18n) {
      shiny::tagList(
        shiny::h5(i18n()$t("Step 1: Search in taxonomic backbones (optional)")),
        shiny::p(i18n()$t("The name is looked up in every backbone this database registers, and in Tropicos when an API key is available. Each backbone that matches contributes its identifier to the new taxon.")),

        # Shared search input
        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(
              8,
              shiny::textInput(
                ns("tropicos_search"),
                i18n()$t("Scientific name to search"),
                placeholder = "e.g., Gilbertiodendron dewevrei",
                width = "100%"
              )
            ),
            shiny::column(
              4,
              shiny::br(),
              shiny::actionButton(
                ns("btn_search_tropicos"),
                i18n()$t("Search all backbones"),
                icon = shiny::icon("search"),
                class = "btn-primary btn-block"
              )
            )
          )
        ),

        # Backbone results, first: they work without any API key
        shiny::wellPanel(
          style = "border-left: 4px solid #28a745;",
          shiny::h6(shiny::icon("globe"), " ", i18n()$t("Taxonomic backbones")),
          shiny::uiOutput(ns("backbone_results_ui"))
        ),

        # Tropicos results
        shiny::wellPanel(
          style = "border-left: 4px solid #007bff;",
          shiny::h6(shiny::icon("leaf"), " ", i18n()$t("Tropicos")),
          shiny::uiOutput(ns("tropicos_results_ui"))
        ),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_skip_tropicos"),
              i18n()$t("Skip - Manual Entry"),
              icon = shiny::icon("edit"),
              class = "btn-secondary btn-block"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_next_step1"),
              i18n()$t("Next: Taxonomic Info"),
              icon = shiny::icon("arrow-right"),
              class = "btn-success btn-block"
            )
          )
        )
      )
    }

    # Search Tropicos + WCVP
    shiny::observeEvent(input$btn_search_tropicos, {
      shiny::req(input$tropicos_search)

      search_name <- trimws(input$tropicos_search)

      if (nchar(search_name) == 0) {
        shiny::showNotification(
          i18n()$t("Please enter a name to search"),
          type = "warning"
        )
        return()
      }

      shiny::withProgress({

        # -- Tropicos --
        tryCatch({
          if (!requireNamespace("taxize", quietly = TRUE)) {
            shiny::showNotification(
              "Package 'taxize' is required for Tropicos search. Please install it.",
              type = "error", duration = 8
            )
          } else {
            # Personal credential: no key ships with the package, and a console
            # prompt would hang the app, so ask for it in the Tropicos panel.
            tps_key <- get_tropicos_key(prompt = FALSE)

            if (is.null(tps_key)) {
              shiny::showNotification(
                i18n()$t("No Tropicos API key - enter one in the Tropicos panel below"),
                type = "warning", duration = 8
              )
              rv$tropicos_results <- NULL
            } else {
              cli::cli_alert_info("Searching Tropicos for: {search_name}")
              results <- taxize::tp_search(sci = search_name, key = tps_key)

              if (ncol(results) == 1) {
                rv$tropicos_results <- NULL
              } else {
                rv$tropicos_results <- results
              }
            }
          }
        }, error = function(e) {
          cli::cli_alert_danger("Tropicos search failed: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Tropicos search error:"), e$message),
            type = "warning",
            duration = 8
          )
          rv$tropicos_results <- NULL
        })

        # -- Every taxonomic backbone (wcvp, apd, ...) --
        # A name usually exists in several backbones and its identifier is
        # worth keeping in each, so all of them are searched and every
        # unambiguous hit is pre-selected for linking.
        tryCatch({
          rv$backbone_results <- search_all_backbones(search_name, pool())
          rv$backbone_selected <- .auto_backbone_selection(
            rv$backbone_results, search_name
          )
          refresh_synonymy_candidates()

          n_auto <- length(rv$backbone_selected)
          if (n_auto > 0) {
            shiny::showNotification(
              sprintf(
                i18n()$t("Exact match in %d backbone(s): %s - identifiers will be linked to the new taxon"),
                n_auto,
                paste(vapply(names(rv$backbone_selected), backbone_label,
                             character(1)), collapse = ", ")
              ),
              type = "message", duration = 8
            )
          }
        }, error = function(e) {
          cli::cli_alert_danger("Backbone search failed: {e$message}")
          rv$backbone_results <- NULL
          rv$backbone_selected <- character(0)
          rv$backbone_synonymy_candidates <- NULL
        })

      }, message = i18n()$t("Searching backbones..."))
    })

    # Ask for the Tropicos API key, and keep it for the session only. Storing
    # it beyond that is the user's decision, taken from the console with
    # setup_tropicos_key().
    shiny::observeEvent(input$btn_save_tropicos_key, {
      entered <- trimws(input$tropicos_key %||% "")

      if (!nzchar(entered)) {
        shiny::showNotification(
          i18n()$t("Please enter a Tropicos API key"),
          type = "warning"
        )
        return()
      }

      get_tropicos_key(entered)
      rv$tropicos_key_set <- TRUE

      shiny::showNotification(
        i18n()$t("Tropicos API key saved for this session"),
        type = "message"
      )
    })

    # Display Tropicos results
    output$tropicos_results_ui <- shiny::renderUI({
      # Re-render once a key has been entered
      rv$tropicos_key_set

      if (is.null(get_tropicos_key(prompt = FALSE))) {
        return(
          shiny::div(
            shiny::div(
              class = "alert alert-warning",
              shiny::icon("key"), " ",
              i18n()$t("Searching Tropicos requires a personal API key, free on request at"),
              " ",
              shiny::a(
                href = "https://services.tropicos.org/help?requestkey",
                target = "_blank",
                "services.tropicos.org"
              ),
              ". ",
              i18n()$t("The key entered here is kept for this session only; store it permanently with setup_tropicos_key() in the R console.")
            ),
            shiny::fluidRow(
              shiny::column(
                8,
                shiny::passwordInput(
                  ns("tropicos_key"),
                  i18n()$t("Tropicos API key"),
                  width = "100%"
                )
              ),
              shiny::column(
                4,
                shiny::br(),
                shiny::actionButton(
                  ns("btn_save_tropicos_key"),
                  i18n()$t("Use this key"),
                  icon = shiny::icon("check"),
                  class = "btn-primary btn-block"
                )
              )
            )
          )
        )
      }

      if (is.null(rv$tropicos_results)) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("No Tropicos results yet - enter a name and click Search")
          )
        )
      }

      shiny::tagList(
        shiny::h6(i18n()$t("Tropicos Results")),
        DT::DTOutput(ns("tropicos_table")),
        shiny::br(),
        shiny::actionButton(
          ns("btn_use_selected"),
          i18n()$t("Use Tropicos Result"),
          icon = shiny::icon("check"),
          class = "btn-primary"
        )
      )
    })

    # Tropicos results table
    output$tropicos_table <- DT::renderDT({
      shiny::req(rv$tropicos_results)
      DT::datatable(
        rv$tropicos_results,
        selection = list(mode = "single"),
        options = list(pageLength = 5, scrollX = TRUE, dom = "tp"),
        rownames = FALSE
      )
    })

    # Use selected Tropicos result
    shiny::observeEvent(input$btn_use_selected, {
      shiny::req(rv$tropicos_results)
      selected <- input$tropicos_table_rows_selected

      if (length(selected) == 0) {
        shiny::showNotification(
          i18n()$t("Please select a result from the table"),
          type = "warning"
        )
        return()
      }

      result <- rv$tropicos_results[selected, ]
      rv$form_data$tax_tax <- result$scientificnamewithauthors
      rv$form_data$tax_gen <- strsplit(result$scientificname, " ")[[1]][1]
      rv$form_data$tax_esp <- strsplit(result$scientificname, " ")[[1]][2]
      rv$form_data$tax_fam <- result$family
      rv$form_data$author1 <- result$author
      rv$form_data$year_description <- as.numeric(result$displaydate)

      rank <- result$rankabbreviation
      if (!is.na(rank) && rank != "sp.") {
        rv$form_data$tax_rank1 <- rank
        rv$form_data$tax_name1 <- strsplit(result$scientificname, " ")[[1]][4]
      }

      # Backbone identifiers are kept: they identify the same name, whichever
      # source filled the form. Step 4 lists every link before it is created.

      shiny::showNotification(
        i18n()$t("Tropicos data loaded - proceed to next step"),
        type = "message"
      )
    })

    # ---- Backbone results UI ----
    output$backbone_results_ui <- shiny::renderUI({
      if (length(backbone_choices()) == 0) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"), " ",
            i18n()$t("No external taxonomic backbone in this database")
          )
        )
      }

      if (is.null(rv$backbone_results)) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("No backbone results yet - enter a name and click Search")
          )
        )
      }

      if (nrow(rv$backbone_results) == 0) {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            " ",
            i18n()$t("No match found in any backbone")
          )
        )
      }

      # One badge per backbone that answered, saying how it answered
      hit_codes <- unique(rv$backbone_results$backbone)
      badges <- lapply(hit_codes, function(code) {
        rows <- rv$backbone_results[rv$backbone_results$backbone == code, , drop = FALSE]
        is_exact <- "exact" %in% rows$match_type
        shiny::span(
          style = "margin-right: 10px;",
          shiny::span(
            class = if (is_exact) "badge badge-success" else "badge badge-warning",
            paste0(backbone_label(code), ": ",
                   if (is_exact) i18n()$t("Exact match") else i18n()$t("Fuzzy match"))
          ),
          shiny::span(
            class = "text-muted",
            style = "margin-left: 4px; font-size: 0.9em;",
            sprintf(i18n()$t("%d result(s)"), nrow(rows))
          )
        )
      })

      # Backbones that were searched and said nothing
      silent <- setdiff(unname(backbone_choices()), hit_codes)

      shiny::tagList(
        shiny::div(badges),
        if (length(silent) > 0) {
          shiny::div(
            class = "text-muted",
            style = "font-size: 0.85em; margin-top: 4px;",
            sprintf(
              i18n()$t("No match in: %s"),
              paste(vapply(silent, backbone_label, character(1)), collapse = ", ")
            )
          )
        },
        shiny::br(),
        DT::DTOutput(ns("backbone_table")),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_use_backbone"),
              i18n()$t("Use backbone result"),
              icon = shiny::icon("check"),
              class = "btn-success",
              title = i18n()$t("Fill form fields from this row and validate the backbone link")
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_validate_backbone"),
              i18n()$t("Validate backbone match only"),
              icon = shiny::icon("link"),
              class = "btn-outline-success",
              title = i18n()$t("Keep existing form fields but record this identifier as the backbone link")
            )
          )
        ),
        shiny::uiOutput(ns("backbone_selected_badge_ui")),
        shiny::uiOutput(ns("backbone_synonymy_preview_ui"))
      )
    })

    # Backbone results table. `backbone` comes first: rows from several
    # backbones share the table, and an external_id means nothing without it.
    output$backbone_table <- DT::renderDT({
      shiny::req(rv$backbone_results)
      display_cols <- c(
        "backbone", "external_id", "taxon_name", "authors", "rank", "status_raw",
        "family", "genus", "species", "infra_rank", "infra_epithet",
        "accepted_external_id", "match_type"
      )
      cols_present <- intersect(display_cols, names(rv$backbone_results))
      DT::datatable(
        rv$backbone_results[, cols_present, drop = FALSE],
        selection = list(mode = "single"),
        options = list(pageLength = 8, scrollX = TRUE, dom = "tp"),
        rownames = FALSE
      )
    })

    # Warning shown in Step 1 when synonymy candidates exist in internal backbone
    output$backbone_synonymy_preview_ui <- shiny::renderUI({
      cands <- rv$backbone_synonymy_candidates
      if (is.null(cands) || nrow(cands) == 0) return(NULL)

      names_txt <- paste(
        paste0(cands$tax_gen, " ", cands$tax_esp,
               ifelse(!is.na(cands$tax_nam01) & cands$tax_nam01 != "",
                      paste0(" ", cands$tax_rank01, " ", cands$tax_nam01), ""),
               " (ID: ", cands$idtax_n, ")"),
        collapse = ", "
      )

      shiny::div(
        class = "alert alert-warning",
        style = "margin-top: 8px;",
        shiny::icon("exclamation-triangle"),
        " ",
        shiny::strong(
          sprintf(i18n()$t("%d existing taxon/taxa share the same accepted name in the backbones:"),
                  nrow(cands))
        ),
        shiny::br(),
        shiny::span(class = "text-muted", names_txt),
        shiny::br(),
        shiny::tags$small(i18n()$t("You will be able to set synonymy relationships in Step 5 after adding the new taxon."))
      )
    })

    # What will be linked, one line per backbone
    output$backbone_selected_badge_ui <- shiny::renderUI({
      sel <- rv$backbone_selected
      if (length(sel) == 0) {
        return(
          shiny::div(
            class = "alert alert-secondary",
            style = "margin-top: 6px; padding: 6px 12px;",
            shiny::icon("unlink"), " ",
            i18n()$t("No backbone identifier selected - the taxon will be created without any backbone link")
          )
        )
      }

      shiny::div(
        class = "alert alert-success",
        style = "margin-top: 6px; padding: 6px 12px;",
        shiny::icon("link"), " ",
        shiny::strong(i18n()$t("Will be linked after taxon creation:")),
        shiny::tags$ul(
          style = "margin-bottom: 4px;",
          lapply(names(sel), function(code) {
            shiny::tags$li(backbone_label(code), ": ", shiny::code(sel[[code]]))
          })
        ),
        shiny::actionLink(
          ns("btn_clear_backbone_links"),
          i18n()$t("Clear all backbone links")
        )
      )
    })

    # The row the user is acting on, defaulting to the first
    selected_backbone_row <- function() {
      selected <- input$backbone_table_rows_selected
      if (length(selected) == 0) selected <- 1L
      rv$backbone_results[selected, ]
    }

    # Record one backbone's identifier, replacing whatever was selected for
    # that same backbone. The other backbones keep their own.
    select_backbone_link <- function(code, external_id) {
      sel <- rv$backbone_selected
      sel[[as.character(code)]] <- as.character(external_id)
      rv$backbone_selected <- sel
    }

    # Internal taxa that every selected backbone considers the same accepted
    # name. Each backbone is asked about its own identifier, and the answers
    # are merged so a taxon suggested by two backbones is offered once.
    refresh_synonymy_candidates <- function() {
      sel <- rv$backbone_selected
      if (length(sel) == 0) {
        rv$backbone_synonymy_candidates <- NULL
        return(invisible(NULL))
      }

      frames <- lapply(names(sel), function(code) {
        cands <- tryCatch(
          .backbone_synonymy_candidates(sel[[code]], code, pool()),
          error = function(e) {
            cli::cli_alert_warning("Could not check {code} synonymy: {e$message}")
            NULL
          }
        )
        if (is.null(cands) || nrow(cands) == 0) return(NULL)
        cands$backbone <- code
        cands$backbone_label <- backbone_label(code)
        cands
      })

      rv$backbone_synonymy_candidates <- .merge_synonymy_candidates(frames)
      invisible(NULL)
    }

    # Use selected backbone result
    shiny::observeEvent(input$btn_use_backbone, {
      shiny::req(rv$backbone_results)
      result <- selected_backbone_row()

      # Populate form fields from the backbone
      rv$form_data$tax_gen <- result$genus
      rv$form_data$tax_esp <- result$species
      rv$form_data$tax_fam <- result$family
      rv$form_data$author1 <- result$authors
      rv$form_data$tax_tax <- trimws(paste(result$taxon_name, result$authors %||% ""))

      if (!is.na(result$infra_rank) && nchar(trimws(result$infra_rank)) > 0) {
        rv$form_data$tax_rank1 <- result$infra_rank
        rv$form_data$tax_name1 <- result$infra_epithet
      } else {
        rv$form_data$tax_rank1 <- NULL
        rv$form_data$tax_name1 <- NULL
      }

      select_backbone_link(result$backbone, result$external_id)
      refresh_synonymy_candidates()

      shiny::showNotification(
        sprintf(
          i18n()$t("%s data loaded (ID: %s) - proceed to next step"),
          backbone_label(result$backbone), result$external_id
        ),
        type = "message"
      )
    })

    # Validate the backbone match (link only - does not fill form fields)
    shiny::observeEvent(input$btn_validate_backbone, {
      shiny::req(rv$backbone_results)
      result <- selected_backbone_row()

      select_backbone_link(result$backbone, result$external_id)
      refresh_synonymy_candidates()

      shiny::showNotification(
        sprintf(
          i18n()$t("%s match validated (ID: %s) - link will be saved after taxon creation"),
          backbone_label(result$backbone), result$external_id
        ),
        type = "message"
      )
    })

    # Drop every pre-selected identifier - the taxon is then created alone
    shiny::observeEvent(input$btn_clear_backbone_links, {
      rv$backbone_selected <- character(0)
      rv$backbone_synonymy_candidates <- NULL
      shiny::showNotification(
        i18n()$t("Backbone links cleared"),
        type = "message"
      )
    })

    # Skip Tropicos
    shiny::observeEvent(input$btn_skip_tropicos, {
      rv$current_step <- 2
    })

    # Next from Step 1
    shiny::observeEvent(input$btn_next_step1, {
      rv$current_step <- 2
    })

    # Step 2: Taxonomic Information + Existence Check
    step2_taxonomy_ui <- function(ns, i18n, rv) {
      shiny::tagList(
        shiny::h5(i18n()$t("Step 2: Enter Taxonomic Information")),

        # Show existence check results if any
        shiny::uiOutput(ns("existence_check_ui")),

        shiny::wellPanel(
          shiny::h6(i18n()$t("Required Fields")),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_gen"),
                paste(i18n()$t("Genus"), "*"),
                value = if (is.null(rv$form_data$tax_gen)) "" else rv$form_data$tax_gen
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_esp"),
                i18n()$t("Species epithet"),
                value = if (is.null(rv$form_data$tax_esp)) "" else rv$form_data$tax_esp
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_fam"),
                paste(i18n()$t("Family"), "*"),
                value = if (is.null(rv$form_data$tax_fam)) "" else rv$form_data$tax_fam
              )
            )
          ),

          shiny::h6(i18n()$t("Higher Taxonomy (auto-filled if possible)")),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_order"),
                shiny::textOutput(ns("order_label"), inline = TRUE),
                value = if (is.null(rv$form_data$tax_order)) "" else rv$form_data$tax_order
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_famclass"),
                shiny::textOutput(ns("class_label"), inline = TRUE),
                value = if (is.null(rv$form_data$tax_famclass)) "" else rv$form_data$tax_famclass
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_tax"),
                i18n()$t("Full name with authors"),
                value = if (is.null(rv$form_data$tax_tax)) "" else rv$form_data$tax_tax
              )
            )
          ),

          shiny::h6(i18n()$t("Infraspecific (if applicable)")),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::selectInput(
                ns("tax_rank1"),
                i18n()$t("Infraspecific rank"),
                choices = c("None" = "", "var." = "var.", "subsp." = "subsp.", "f." = "f."),
                selected = if (is.null(rv$form_data$tax_rank1)) "" else rv$form_data$tax_rank1
              )
            ),
            shiny::column(
              4,
              shiny::textInput(
                ns("tax_name1"),
                i18n()$t("Infraspecific name"),
                value = if (is.null(rv$form_data$tax_name1)) "" else rv$form_data$tax_name1
              )
            )
          ),

          shiny::h6(i18n()$t("Authors & Year")),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::textInput(
                ns("author1"),
                i18n()$t("Author 1"),
                value = if (is.null(rv$form_data$author1)) "" else rv$form_data$author1
              )
            ),
            shiny::column(
              3,
              shiny::textInput(
                ns("author2"),
                i18n()$t("Author 2"),
                value = if (is.null(rv$form_data$author2)) "" else rv$form_data$author2
              )
            ),
            shiny::column(
              3,
              shiny::textInput(
                ns("author3"),
                i18n()$t("Author 3"),
                value = if (is.null(rv$form_data$author3)) "" else rv$form_data$author3
              )
            ),
            shiny::column(
              3,
              shiny::numericInput(
                ns("year_description"),
                i18n()$t("Year"),
                value = rv$form_data$year_description,
                min = 1700,
                max = as.numeric(format(Sys.Date(), "%Y"))
              )
            )
          ),

          shiny::checkboxInput(
            ns("morpho_species"),
            i18n()$t("This is a morphotaxon"),
            value = FALSE
          )
        ),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_back_step2"),
              i18n()$t("Back"),
              icon = shiny::icon("arrow-left"),
              class = "btn-secondary btn-block"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_next_step2"),
              i18n()$t("Next: Growth Form"),
              icon = shiny::icon("arrow-right"),
              class = "btn-success btn-block"
            )
          )
        )
      )
    }

    # Check if taxon exists in database
    output$existence_check_ui <- shiny::renderUI({
      if (is.null(rv$existing_check)) {
        return(NULL)
      }

      check <- rv$existing_check

      if (check$exists) {
        if (check$is_synonym) {
          # Exists as synonym
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            " ",
            shiny::strong(i18n()$t("Taxon already exists as SYNONYM")),
            shiny::br(),
            sprintf(i18n()$t("ID: %s, Synonym of ID: %s"), check$idtax_n, check$idtax_good_n),
            shiny::br(),
            shiny::p(i18n()$t("This taxon is already in the database but marked as a synonym.")),
            shiny::actionButton(
              ns("btn_cancel_existing_synonymy"),
              i18n()$t("Cancel its synonymy to make it an accepted name"),
              icon = shiny::icon("unlink"),
              class = "btn-warning"
            )
          )
        } else {
          # Exists as accepted name
          shiny::div(
            class = "alert alert-danger",
            shiny::icon("times-circle"),
            " ",
            shiny::strong(i18n()$t("Taxon already exists as ACCEPTED name")),
            shiny::br(),
            sprintf(i18n()$t("ID: %s"), check$idtax_n),
            shiny::br(),
            shiny::p(i18n()$t("Cannot add duplicate. Please use the Update or Synonymy tabs to modify this taxon."))
          )
        }
      } else {
        shiny::div(
          class = "alert alert-success",
          shiny::icon("check-circle"),
          " ",
          i18n()$t("Taxon does not exist in database - you can proceed with addition")
        )
      }
    })

    # Cancel existing synonymy
    shiny::observeEvent(input$btn_cancel_existing_synonymy, {
      check <- rv$existing_check

      shiny::withProgress({
        tryCatch({
          cli::cli_alert_info("Canceling synonymy for existing taxon ID {check$idtax_n}...")

          # Get pool connection
          pool_conn <- pool()

          update_dico_name(
            id_searched = check$idtax_n,
            cancel_synonymy = TRUE,
            ask_before_update = FALSE,
            add_backup = TRUE,
            show_results = FALSE,
            con = pool_conn
          )

          shiny::showNotification(
            i18n()$t("Synonymy cancelled! The taxon is now an accepted name."),
            type = "message",
            duration = 5
          )

          # Reset check
          rv$existing_check <- NULL

        }, error = function(e) {
          cli::cli_alert_danger("Failed to cancel synonymy: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Cancelling synonymy..."))
    })

    # Back from Step 2
    # Validate and auto-fill taxonomy based on family
    shiny::observeEvent(input$tax_fam, {
      if (!is.null(input$tax_fam) && nchar(trimws(input$tax_fam)) > 0) {

        tryCatch({
          family_name <- trimws(input$tax_fam)

          # Get connection
          actual_con <- if (inherits(pool(), "Pool")) {
            pool::poolCheckout(pool())
          } else {
            pool()
          }

          on.exit({
            if (inherits(pool(), "Pool") && !is.null(actual_con)) {
              pool::poolReturn(actual_con)
            }
          }, add = TRUE)

          # Navigate the hierarchy using id_parent to get canonical order and class.
          # Steps:
          #   1. Find the accepted family entry (tax_level = 'family', idtax_good_n IS NULL)
          #   2. Follow id_parent to the order-level entry → get tax_order name
          #   3. Get class via id_tax_famclass → table_tax_famclass join on the family entry
          family_row <- DBI::dbGetQuery(actual_con,
            "SELECT f.idtax_n, f.id_parent, f.id_tax_famclass,
                    COALESCE(p.tax_order, f.tax_order) AS tax_order,
                    tc.tax_famclass
             FROM table_taxa f
             LEFT JOIN table_taxa p  ON p.idtax_n = f.id_parent
             LEFT JOIN table_tax_famclass tc ON tc.id_tax_famclass = f.id_tax_famclass
             WHERE LOWER(f.tax_fam) = LOWER($1)
               AND f.tax_level = 'family'
               AND f.idtax_good_n IS NULL
             LIMIT 1",
            params = list(family_name)
          )

          if (nrow(family_row) > 0) {
            order_val <- if (!is.na(family_row$tax_order[1]) && nchar(family_row$tax_order[1]) > 0)
              family_row$tax_order[1] else NA_character_
            class_val <- if (!is.na(family_row$tax_famclass[1]) && nchar(family_row$tax_famclass[1]) > 0)
              family_row$tax_famclass[1] else NA_character_

            # Always update order and class when family changes —
            # previous taxon's values may still be in the fields
            if (!is.na(order_val)) {
              shiny::updateTextInput(session, "tax_order", value = order_val)
              rv$order_required <- FALSE
            }
            if (!is.na(class_val)) {
              shiny::updateTextInput(session, "tax_famclass", value = class_val)
              rv$class_required <- FALSE
            }

            rv$order_required <- is.na(order_val)
            rv$class_required <- is.na(class_val)

            cli::cli_alert_success("Family found: {family_name} → order={if(is.na(order_val)) 'NA' else order_val}, class={if(is.na(class_val)) 'NA' else class_val}")

          } else {
            # Try fuzzy match to suggest correct spelling
            similar_families <- DBI::dbGetQuery(actual_con,
              "SELECT DISTINCT tax_fam FROM table_taxa
               WHERE tax_level = 'family' AND tax_fam ILIKE $1
               LIMIT 5",
              params = list(paste0("%", family_name, "%"))
            )

            if (nrow(similar_families) > 0) {
              suggestions <- paste(similar_families$tax_fam, collapse = ", ")
              shiny::showNotification(
                paste0(i18n()$t("Family not found. Did you mean:"), " ", suggestions, "?"),
                type = "warning",
                duration = 8
              )
            } else {
              shiny::showNotification(
                i18n()$t("Family not found in taxonomic backbone. Please enter Order and Class manually, or add the family to the backbone first."),
                type = "warning",
                duration = 8
              )
            }

            rv$order_required <- TRUE
            rv$class_required <- TRUE
            cli::cli_alert_warning("Family '{family_name}' not found in backbone")
          }

        }, error = function(e) {
          cli::cli_alert_warning("Could not validate taxonomy: {e$message}")
          # On error, require manual input to be safe
          rv$order_required <- TRUE
          rv$class_required <- TRUE
        })
      }
    }, ignoreInit = TRUE)

    # Dynamic labels for order and class
    output$order_label <- shiny::renderText({
      if (rv$order_required) {
        paste(i18n()$t("Order"), "*")
      } else {
        i18n()$t("Order")
      }
    })

    output$class_label <- shiny::renderText({
      if (rv$class_required) {
        paste(i18n()$t("Class"), "*")
      } else {
        i18n()$t("Class")
      }
    })

    shiny::observeEvent(input$btn_back_step2, {
      rv$current_step <- 1
    })

    # Next from Step 2 (validate and check existence)
    shiny::observeEvent(input$btn_next_step2, {
      # Validate required fields
      if (is.null(input$tax_gen) || nchar(trimws(input$tax_gen)) == 0) {
        shiny::showNotification(
          i18n()$t("Genus is required"),
          type = "error"
        )
        return()
      }

      # Validate order if required
      if (rv$order_required && (is.null(input$tax_order) || nchar(trimws(input$tax_order)) == 0)) {
        shiny::showNotification(
          i18n()$t("Order is required because the family was not found in the taxonomic backbone"),
          type = "error",
          duration = 8
        )
        return()
      }

      # Validate class if required
      if (rv$class_required && (is.null(input$tax_famclass) || nchar(trimws(input$tax_famclass)) == 0)) {
        shiny::showNotification(
          i18n()$t("Class is required because the family was not found in the taxonomic backbone"),
          type = "error",
          duration = 8
        )
        return()
      }

      if (is.null(input$tax_fam) || nchar(trimws(input$tax_fam)) == 0) {
        shiny::showNotification(
          i18n()$t("Family is required"),
          type = "error"
        )
        return()
      }

      # Save form data
      rv$form_data$tax_gen <- trimws(input$tax_gen)
      rv$form_data$tax_esp <- if (nchar(trimws(input$tax_esp)) > 0) trimws(input$tax_esp) else NULL
      rv$form_data$tax_fam <- trimws(input$tax_fam)
      rv$form_data$tax_order <- if (nchar(trimws(input$tax_order)) > 0) trimws(input$tax_order) else NULL
      rv$form_data$tax_famclass <- if (nchar(trimws(input$tax_famclass)) > 0) trimws(input$tax_famclass) else NULL
      rv$form_data$tax_tax <- if (nchar(trimws(input$tax_tax)) > 0) trimws(input$tax_tax) else NULL
      rv$form_data$tax_rank1 <- if (nchar(input$tax_rank1) > 0) input$tax_rank1 else NULL
      rv$form_data$tax_name1 <- if (nchar(trimws(input$tax_name1)) > 0) trimws(input$tax_name1) else NULL
      rv$form_data$author1 <- if (nchar(trimws(input$author1)) > 0) trimws(input$author1) else NULL
      rv$form_data$author2 <- if (nchar(trimws(input$author2)) > 0) trimws(input$author2) else NULL
      rv$form_data$author3 <- if (nchar(trimws(input$author3)) > 0) trimws(input$author3) else NULL
      rv$form_data$year_description <- input$year_description
      rv$form_data$morpho_species <- input$morpho_species

      # Check if taxon already exists in database
      shiny::withProgress({
        tryCatch({
          result <- query_taxa(
            genus = rv$form_data$tax_gen,
            species = rv$form_data$tax_esp,
            exact_match = TRUE
          )

          if (nrow(result) > 0) {
            # Taxon exists
            taxon <- result[1, ]
            rv$existing_check <- list(
              exists = TRUE,
              idtax_n = taxon$idtax_n,
              idtax_good_n = taxon$idtax_good_n,
              is_synonym = !is.na(taxon$idtax_good_n)
            )

            if (!rv$existing_check$is_synonym) {
              # Exists as accepted - cannot proceed
              shiny::showNotification(
                i18n()$t("This taxon already exists as an accepted name. Cannot add duplicate."),
                type = "error",
                duration = 10
              )
              return()
            } else {
              # Exists as synonym - show option to cancel synonymy
              shiny::showNotification(
                i18n()$t("This taxon exists as a synonym. See options above."),
                type = "warning",
                duration = 10
              )
              return()
            }
          } else {
            # Does not exist - can proceed
            rv$existing_check <- list(exists = FALSE)
            rv$current_step <- 3
          }

        }, error = function(e) {
          cli::cli_alert_warning("Could not check existence: {e$message}")
          # Proceed anyway
          rv$existing_check <- NULL
          rv$current_step <- 3
        })
      }, message = i18n()$t("Checking if taxon exists..."))
    })

    # Step 3: Growth Form (with integrated selector)
    step3_growth_ui <- function(ns, i18n, rv) {
      shiny::tagList(
        shiny::h5(i18n()$t("Step 3: Growth Form (Optional)")),

        shiny::p(
          class = "text-muted",
          i18n()$t("Select growth form characteristics for this taxon. You can skip this step if you don't have this information.")
        ),

        # Growth form selector module
        mod_growth_form_selector_ui(ns("growth_form")),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_back_step3"),
              i18n()$t("Back"),
              icon = shiny::icon("arrow-left"),
              class = "btn-secondary btn-block"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_next_step3"),
              i18n()$t("Next: Review"),
              icon = shiny::icon("arrow-right"),
              class = "btn-success btn-block"
            )
          )
        )
      )
    }

    # Back from Step 3
    shiny::observeEvent(input$btn_back_step3, {
      rv$current_step <- 2
    })

    # Next from Step 3
    shiny::observeEvent(input$btn_next_step3, {
      rv$current_step <- 4
    })

    # Step 4: Review & Submit
    step4_review_ui <- function(ns, i18n, rv) {
      fd <- rv$form_data

      shiny::tagList(
        shiny::h5(i18n()$t("Step 4: Review & Submit")),

        shiny::wellPanel(
          style = "background-color: #f8f9fa;",
          shiny::h6(i18n()$t("Review New Taxon")),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::strong(i18n()$t("Genus:")), " ", fd$tax_gen, shiny::br(),
              shiny::strong(i18n()$t("Species:")), " ", if (is.null(fd$tax_esp)) "N/A" else fd$tax_esp, shiny::br(),
              shiny::strong(i18n()$t("Family:")), " ", fd$tax_fam, shiny::br(),
              shiny::strong(i18n()$t("Order:")), " ", if (is.null(fd$tax_order)) "N/A" else fd$tax_order, shiny::br(),
              shiny::strong(i18n()$t("Class:")), " ", if (is.null(fd$tax_famclass)) "N/A" else fd$tax_famclass
            ),
            shiny::column(
              6,
              shiny::strong(i18n()$t("Full name:")), " ", if (is.null(fd$tax_tax)) "N/A" else fd$tax_tax, shiny::br(),
              shiny::strong(i18n()$t("Author:")), " ", if (is.null(fd$author1)) "N/A" else fd$author1, shiny::br(),
              shiny::strong(i18n()$t("Year:")), " ", if (is.null(fd$year_description)) "N/A" else fd$year_description, shiny::br(),
              shiny::strong(i18n()$t("Morphotaxon:")), " ", if (fd$morpho_species) i18n()$t("Yes") else i18n()$t("No")
            )
          )
        ),

        # Every backbone identifier that will be linked to the new taxon
        if (length(rv$backbone_selected) > 0) {
          shiny::div(
            class = "alert alert-success",
            shiny::icon("link"),
            " ",
            shiny::strong(i18n()$t("Backbone links that will be created:")),
            shiny::tags$ul(
              style = "margin-bottom: 0;",
              lapply(names(rv$backbone_selected), function(code) {
                shiny::tags$li(
                  backbone_label(code), ": ",
                  shiny::code(rv$backbone_selected[[code]])
                )
              })
            )
          )
        },

        shiny::div(
          class = "alert alert-info",
          shiny::icon("info-circle"),
          " ",
          i18n()$t("After adding, you can optionally set this taxon as a synonym of another")
        ),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_back_step4"),
              i18n()$t("Back"),
              icon = shiny::icon("arrow-left"),
              class = "btn-secondary btn-block"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("btn_submit"),
              i18n()$t("Submit - Add Taxon"),
              icon = shiny::icon("plus-circle"),
              class = "btn-success btn-block"
            )
          )
        )
      )
    }

    # Back from Step 4
    shiny::observeEvent(input$btn_back_step4, {
      rv$current_step <- 3
    })

    # Submit new taxon (WITHOUT synonymy)
    shiny::observeEvent(input$btn_submit, {
      shiny::req(rv$form_data)
      fd <- rv$form_data

      shiny::withProgress({
        tryCatch({
          cli::cli_alert_info("Adding new taxon to database...")

          # Call non-interactive function (no growth form prompts)
          new_id <- .add_taxa_noninteractive(
            tax_gen = fd$tax_gen,
            tax_esp = fd$tax_esp,
            tax_fam = fd$tax_fam,
            tax_order = fd$tax_order,
            tax_famclass = fd$tax_famclass,
            tax_rank1 = fd$tax_rank1,
            tax_name1 = fd$tax_name1,
            author1 = fd$author1,
            author2 = fd$author2,
            author3 = fd$author3,
            year_description = fd$year_description,
            morpho_species = fd$morpho_species,
            tax_tax = fd$tax_tax,
            con = pool()
          )

          # Store the new taxon ID
          rv$new_taxon_id <- new_id

          # Add growth forms if selected
          n_selections <- length(growth_form_module$growth_form_selections())
          is_valid <- growth_form_module$is_valid()
          basis_value <- growth_form_module$basisofrecord()
          remarks_value <- growth_form_module$measurementremarks()

          cli::cli_alert_info("Growth form debug:")
          cli::cli_alert_info("  - Number of selections: {n_selections}")
          cli::cli_alert_info("  - Is valid: {is_valid}")
          cli::cli_alert_info("  - Basis of record: '{if(is.null(basis_value)) 'NULL' else if(basis_value == '') 'EMPTY' else basis_value}'")
          cli::cli_alert_info("  - Remarks: '{if(is.null(remarks_value)) 'NULL' else if(remarks_value == '') 'EMPTY' else remarks_value}'")

          if (n_selections > 0) {
            cli::cli_alert_info("Growth form selections found:")
            print(growth_form_module$growth_form_selections())
          }

          if (n_selections > 0 && is_valid) {

            cli::cli_alert_info("Adding growth forms...")

            # taxa_traits_measures lives in the main DB; fall back to taxa pool if needed
            growth_pool <- if (!is.null(pool_main) && !is.null(pool_main())) pool_main() else pool()

            tryCatch({
              # Add growth forms directly (non-interactive)
              .add_growth_forms_noninteractive(
                idtax = new_id,
                growth_form_selections = growth_form_module$growth_form_selections(),
                basisofrecord = growth_form_module$basisofrecord(),
                measurementremarks = growth_form_module$measurementremarks(),
                pool = growth_pool
              )

              cli::cli_alert_success("Growth forms added successfully")

              shiny::showNotification(
                i18n()$t("Growth forms added successfully!"),
                type = "message",
                duration = 5
              )

            }, error = function(e) {
              cli::cli_alert_danger("Failed to add growth forms: {e$message}")
              cli::cli_alert_info("Error details:")
              print(e)
              traceback()
              shiny::showNotification(
                paste(i18n()$t("Warning: Growth forms not added:"), e$message),
                type = "warning",
                duration = 10
              )
            })
          } else {
            if (n_selections == 0) {
              cli::cli_alert_warning("No growth forms selected")
            } else if (!is_valid) {
              cli::cli_alert_warning("Growth form validation failed - basis of record missing?")
            }
          }

          shiny::showNotification(
            i18n()$t("Taxon added successfully!"),
            type = "message",
            duration = 5
          )

          # Move to Step 5 (optional synonymy)
          rv$current_step <- 5

        }, error = function(e) {
          cli::cli_alert_danger("Failed to add taxon: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error adding taxon:"), e$message),
            type = "error",
            duration = 10
          )
        })

        # Save one link per backbone — runs AFTER the outer tryCatch so that a
        # growth form failure does not prevent the links from being created.
        # Uses rv$new_taxon_id (set inside the tryCatch before growth forms run).
        saved_taxon_id <- rv$new_taxon_id
        cli::cli_alert_info(
          "Backbone link check: {length(rv$backbone_selected)} selected, new_taxon_id={if (is.null(saved_taxon_id)) 'NULL' else saved_taxon_id}"
        )

        if (length(rv$backbone_selected) > 0 && !is.null(saved_taxon_id)) {
          for (backbone_code in names(rv$backbone_selected)) {
            id_to_link <- as.character(rv$backbone_selected[[backbone_code]])
            bb_label   <- backbone_label(backbone_code)

            # Derive match_type from the search results of that same backbone
            # (manual when the row is no longer among them)
            link_match_type <- "manual"
            if (!is.null(rv$backbone_results) && nrow(rv$backbone_results) > 0) {
              hit <- rv$backbone_results[
                rv$backbone_results$backbone == backbone_code &
                  as.character(rv$backbone_results$external_id) == id_to_link, ]
              if (nrow(hit) > 0) link_match_type <- hit$match_type[1L]
            }

            tryCatch({
              cli::cli_alert_info(
                "Saving {backbone_code} link ({link_match_type}): idtax_n={saved_taxon_id} -> external_id={id_to_link}"
              )
              match_row <- data.frame(
                idtax_n     = as.integer(saved_taxon_id),
                external_id = id_to_link,
                match_type  = link_match_type,
                match_score = if (link_match_type == "exact") 1.0 else NA_real_,
                # chosen by the person adding the taxon, so it supplies names
                # even when the search hit was fuzzy
                verified    = TRUE,
                stringsAsFactors = FALSE
              )
              save_backbone_links(match_row, backbone_code, con_taxa = pool(),
                                  replace = FALSE, verbose = FALSE)
              shiny::showNotification(
                sprintf(
                  i18n()$t("Link to %s saved (ID: %s)"),
                  bb_label, id_to_link
                ),
                type = "message",
                duration = 5
              )
            }, error = function(e) {
              cli::cli_alert_warning("Could not save {backbone_code} link: {e$message}")
              shiny::showNotification(
                paste(i18n()$t("Warning: backbone link not saved:"),
                      bb_label, "-", e$message),
                type = "warning",
                duration = 10
              )
            })
          }
        }

      }, message = i18n()$t("Adding taxon to database..."))
    })

    # Step 5: Optional Synonymy
    step5_synonymy_ui <- function(ns, i18n, rv) {
      shiny::tagList(
        shiny::h5(i18n()$t("Step 5: Manage Synonymy (Optional)")),

        shiny::div(
          class = "alert alert-success",
          shiny::icon("check-circle"),
          " ",
          shiny::strong(i18n()$t("Taxon successfully added!")),
          shiny::br(),
          if (!is.null(rv$new_taxon_id)) {
            sprintf(i18n()$t("New taxon ID: %s"), rv$new_taxon_id)
          }
        ),

        # BACKBONE-SUGGESTED SYNONYMIES (shown only when candidates exist)
        if (!is.null(rv$backbone_synonymy_candidates) && nrow(rv$backbone_synonymy_candidates) > 0) {
          cands <- rv$backbone_synonymy_candidates
          checkbox_choices <- setNames(
            as.character(cands$idtax_n),
            paste0(
              cands$tax_gen, " ", cands$tax_esp,
              ifelse(!is.na(cands$tax_nam01) & cands$tax_nam01 != "",
                     paste0(" ", cands$tax_rank01, " ", cands$tax_nam01), ""),
              " (ID: ", cands$idtax_n,
              " | ", cands$sources,
              ")"
            )
          )
          shiny::tagList(
            shiny::wellPanel(
              style = "border-left: 4px solid #fd7e14; background-color: #fff8f0;",
              shiny::h6(
                shiny::icon("sitemap"), " ",
                i18n()$t("Synonymies suggested by the backbones")
              ),
              shiny::p(
                class = "text-muted",
                i18n()$t("These taxa share the accepted name of the taxon you just added, in at least one backbone. Select those you want to set as synonyms of this new entry.")
              ),
              shiny::checkboxGroupInput(
                ns("backbone_synonym_ids"),
                label = NULL,
                choices = checkbox_choices
              ),
              shiny::actionButton(
                ns("btn_confirm_backbone_synonymies"),
                i18n()$t("Set selected as synonyms of new taxon"),
                icon = shiny::icon("check-double"),
                class = "btn-warning"
              )
            ),
            shiny::hr()
          )
        },

        # OPTION 1: Set EXISTING taxon as synonym of NEW (most common)
        shiny::wellPanel(
          style = "border-left: 4px solid #28a745;",
          shiny::h6(
            shiny::icon("star"),
            " ",
            i18n()$t("Option 1: Set an existing taxon as synonym of this new entry")
          ),
          shiny::p(
            class = "text-muted",
            i18n()$t("Most common: If an existing taxon should be updated to point to this new entry as the accepted name")
          ),

          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                ns("existing_binomial"),
                i18n()$t("Existing taxon (binomial)"),
                placeholder = "Genus species"
              ),
              shiny::helpText(i18n()$t("Enter genus and species separated by space (e.g., 'Pinus alba')"))
            ),
            shiny::column(
              6,
              shiny::numericInput(
                ns("existing_id"),
                i18n()$t("Or existing taxon ID"),
                value = NA
              )
            )
          ),

          shiny::uiOutput(ns("existing_taxon_selector_ui")),

          shiny::actionButton(
            ns("btn_set_existing_as_synonym"),
            i18n()$t("Set Existing as Synonym of New"),
            icon = shiny::icon("arrow-left"),
            class = "btn-success"
          )
        ),

        shiny::hr(),

        # OPTION 2: Set NEW taxon as synonym of EXISTING
        shiny::wellPanel(
          shiny::h6(i18n()$t("Option 2: Set this new entry as synonym of an existing taxon")),
          shiny::p(
            class = "text-muted",
            i18n()$t("Less common: If this new entry should point to an existing taxon as the accepted name")
          ),

          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                ns("accepted_binomial"),
                i18n()$t("Accepted name (binomial)"),
                placeholder = "Genus species"
              ),
              shiny::helpText(i18n()$t("Enter genus and species separated by space (e.g., 'Pinus alba')"))
            ),
            shiny::column(
              6,
              shiny::numericInput(
                ns("accepted_id"),
                i18n()$t("Or accepted taxon ID"),
                value = NA
              )
            )
          ),

          shiny::uiOutput(ns("accepted_taxon_selector_ui")),

          shiny::actionButton(
            ns("btn_set_new_as_synonym"),
            i18n()$t("Set New as Synonym of Existing"),
            icon = shiny::icon("arrow-right"),
            class = "btn-warning"
          )
        ),

        shiny::hr(),

        shiny::actionButton(
          ns("btn_finish"),
          i18n()$t("Finish - Start New Addition"),
          icon = shiny::icon("check"),
          class = "btn-primary btn-block"
        )
      )
    }

    # Render taxon selector for existing taxon (when multiple matches found)
    output$existing_taxon_selector_ui <- shiny::renderUI({
      if (is.null(rv$existing_taxon_matches) || nrow(rv$existing_taxon_matches) == 0) {
        return(NULL)
      }

      # Create display labels with full taxonomic info
      choices <- setNames(
        rv$existing_taxon_matches$idtax_n,
        paste0(
          rv$existing_taxon_matches$tax_gen, " ",
          rv$existing_taxon_matches$tax_esp,
          ifelse(!is.na(rv$existing_taxon_matches$tax_rank01) & rv$existing_taxon_matches$tax_rank01 != "",
                 paste0(" ", rv$existing_taxon_matches$tax_rank01, " ", rv$existing_taxon_matches$tax_nam01),
                 ""),
          " (ID: ", rv$existing_taxon_matches$idtax_n, ")"
        )
      )

      shiny::div(
        class = "alert alert-info",
        style = "margin-top: 10px;",
        shiny::icon("info-circle"),
        " ",
        shiny::strong(i18n()$t("Multiple taxa found - please select one:")),
        shiny::br(),
        shiny::br(),
        shiny::selectInput(
          ns("selected_existing_taxon"),
          i18n()$t("Select taxon to set as synonym"),
          choices = choices
        )
      )
    })

    # Render taxon selector for accepted taxon (when multiple matches found)
    output$accepted_taxon_selector_ui <- shiny::renderUI({
      if (is.null(rv$accepted_taxon_matches) || nrow(rv$accepted_taxon_matches) == 0) {
        return(NULL)
      }

      # Create display labels with full taxonomic info
      choices <- setNames(
        rv$accepted_taxon_matches$idtax_n,
        paste0(
          rv$accepted_taxon_matches$tax_gen, " ",
          rv$accepted_taxon_matches$tax_esp,
          ifelse(!is.na(rv$accepted_taxon_matches$tax_rank01) & rv$accepted_taxon_matches$tax_rank01 != "",
                 paste0(" ", rv$accepted_taxon_matches$tax_rank01, " ", rv$accepted_taxon_matches$tax_nam01),
                 ""),
          " (ID: ", rv$accepted_taxon_matches$idtax_n, ")"
        )
      )

      shiny::div(
        class = "alert alert-info",
        style = "margin-top: 10px;",
        shiny::icon("info-circle"),
        " ",
        shiny::strong(i18n()$t("Multiple taxa found - please select one:")),
        shiny::br(),
        shiny::br(),
        shiny::selectInput(
          ns("selected_accepted_taxon"),
          i18n()$t("Select accepted taxon"),
          choices = choices
        )
      )
    })

    # OPTION 1: Set existing taxon as synonym of new (REVERSE - most common)
    shiny::observeEvent(input$btn_set_existing_as_synonym, {
      if (is.null(rv$new_taxon_id)) {
        shiny::showNotification(
          i18n()$t("Error: Could not determine new taxon ID"),
          type = "error"
        )
        return()
      }

      # Validate inputs
      has_binomial <- !is.null(input$existing_binomial) && nchar(trimws(input$existing_binomial)) > 0
      has_id <- !is.null(input$existing_id) && !is.na(input$existing_id)

      if (!has_binomial && !has_id) {
        shiny::showNotification(
          i18n()$t("Please provide binomial name or taxon ID"),
          type = "error"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          # Check if user has selected from dropdown (multiple matches scenario)
          if (!is.null(input$selected_existing_taxon)) {
            existing_id <- as.numeric(input$selected_existing_taxon)
            rv$existing_taxon_matches <- NULL  # Clear matches after selection
            cli::cli_alert_info("Using selected taxon ID {existing_id}")
          } else {
            # First, find the existing taxon
            cli::cli_alert_info("Finding existing taxon...")

            # Parse binomial if provided
            search_params <- list()
            if (has_binomial) {
              binomial_parts <- trimws(strsplit(trimws(input$existing_binomial), "\\s+")[[1]])
              if (length(binomial_parts) >= 2) {
                # Full binomial provided - pass as species parameter (query_taxa expects full binomial)
                search_params$species <- trimws(input$existing_binomial)
              } else if (length(binomial_parts) == 1) {
                # Only genus provided
                search_params$genus <- binomial_parts[1]
              }
            }
            if (has_id) search_params$ids <- input$existing_id

            existing_taxon <- do.call(query_taxa, search_params)

            if (nrow(existing_taxon) == 0) {
              shiny::showNotification(
                i18n()$t("Existing taxon not found"),
                type = "error"
              )
              return()
            }

            if (nrow(existing_taxon) > 1) {
              # Store matches and show selector
              rv$existing_taxon_matches <- existing_taxon
              shiny::showNotification(
                i18n()$t("Multiple taxa found. Please select one from the dropdown above."),
                type = "warning",
                duration = 5
              )
              return()
            }

            existing_id <- existing_taxon$idtax_n[1]
            cli::cli_alert_info("Found existing taxon ID {existing_id}")
          }

          # Get pool connection
          pool_conn <- pool()

          # Check if this existing taxon has other synonyms pointing to it
          actual_con <- pool::poolCheckout(pool_conn)

          on.exit({
            pool::poolReturn(actual_con)
          }, add = TRUE)

          synonyms_of_existing <- dplyr::tbl(actual_con, "table_taxa") %>%
            dplyr::filter(idtax_good_n == !!existing_id) %>%
            dplyr::select(idtax_n, tax_gen, tax_esp, tax_fam, idtax_good_n) %>%
            dplyr::collect()

          # Check if there are cascade synonyms to handle
          # If yes, use direct SQL for EVERYTHING to avoid interactive prompts from update_dico_name()
          # If no, use update_dico_name() which handles backups properly
          if (nrow(synonyms_of_existing) > 0) {
            cli::cli_alert_info("Found {nrow(synonyms_of_existing)} existing synonym(s) of taxon {existing_id}")
            cli::cli_alert_info("Using direct SQL for main synonym and cascade synonyms to avoid prompts...")

            # Build list of all IDs to update (main existing + its synonyms)
            all_ids_to_update <- c(existing_id, synonyms_of_existing$idtax_n)

            # Update all at once with single SQL statement
            sql <- sprintf(
              "UPDATE table_taxa SET idtax_good_n = %d WHERE idtax_n IN (%s)",
              rv$new_taxon_id,
              paste(all_ids_to_update, collapse = ", ")
            )

            n_updated <- DBI::dbExecute(actual_con, sql)

            cli::cli_alert_success("Updated {n_updated} taxon/taxa (1 main + {nrow(synonyms_of_existing)} cascade)")

            shiny::showNotification(
              sprintf(
                i18n()$t("Successfully updated %d synonym(s) to point to new taxon"),
                n_updated
              ),
              type = "message",
              duration = 5
            )
          } else {
            # No cascade synonyms - use update_dico_name() which handles backups
            cli::cli_alert_info("No cascade synonyms - using update_dico_name() with backups...")

            update_dico_name(
              id_searched = existing_id,
              synonym_of = list(id = rv$new_taxon_id),
              ask_before_update = FALSE,
              add_backup = TRUE,
              show_results = FALSE,
              con = pool_conn
            )

            cli::cli_alert_success("Main synonym relationship set")

            shiny::showNotification(
              i18n()$t("Existing taxon set as synonym of new entry!"),
              type = "message",
              duration = 5
            )
          }

        }, error = function(e) {
          cli::cli_alert_danger("Failed to set synonym: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error setting synonym:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Setting synonym relationship..."))
    })

    # OPTION 2: Set newly added taxon as synonym of existing
    shiny::observeEvent(input$btn_set_new_as_synonym, {
      if (is.null(rv$new_taxon_id)) {
        shiny::showNotification(
          i18n()$t("Error: Could not determine new taxon ID"),
          type = "error"
        )
        return()
      }

      # Validate inputs
      has_binomial <- !is.null(input$accepted_binomial) && nchar(trimws(input$accepted_binomial)) > 0
      has_id <- !is.null(input$accepted_id) && !is.na(input$accepted_id)

      if (!has_binomial && !has_id) {
        shiny::showNotification(
          i18n()$t("Please provide binomial name or taxon ID"),
          type = "error"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          # Check if user has selected from dropdown (multiple matches scenario)
          if (!is.null(input$selected_accepted_taxon)) {
            accepted_id <- as.numeric(input$selected_accepted_taxon)
            rv$accepted_taxon_matches <- NULL  # Clear matches after selection
            cli::cli_alert_info("Using selected taxon ID {accepted_id}")
          } else {
            # First, find the accepted taxon
            cli::cli_alert_info("Finding accepted taxon...")

            # Parse binomial if provided
            search_params <- list()
            if (has_binomial) {
              binomial_parts <- trimws(strsplit(trimws(input$accepted_binomial), "\\s+")[[1]])
              if (length(binomial_parts) >= 2) {
                # Full binomial provided - pass as species parameter (query_taxa expects full binomial)
                search_params$species <- trimws(input$accepted_binomial)
              } else if (length(binomial_parts) == 1) {
                # Only genus provided
                search_params$genus <- binomial_parts[1]
              }
            }
            if (has_id) search_params$ids <- input$accepted_id

            accepted_taxon <- do.call(query_taxa, search_params)

            if (nrow(accepted_taxon) == 0) {
              shiny::showNotification(
                i18n()$t("Accepted taxon not found"),
                type = "error"
              )
              return()
            }

            if (nrow(accepted_taxon) > 1) {
              # Store matches and show selector
              rv$accepted_taxon_matches <- accepted_taxon
              shiny::showNotification(
                i18n()$t("Multiple taxa found. Please select one from the dropdown above."),
                type = "warning",
                duration = 5
              )
              return()
            }

            accepted_id <- accepted_taxon$idtax_n[1]
            cli::cli_alert_info("Found accepted taxon ID {accepted_id}")
          }

          cli::cli_alert_info("Setting new taxon ID {rv$new_taxon_id} as synonym of {accepted_id}...")

          # Get pool connection
          pool_conn <- pool()

          # Call update_dico_name with the accepted taxon ID
          update_dico_name(
            id_searched = rv$new_taxon_id,
            synonym_of = list(id = accepted_id),
            ask_before_update = FALSE,
            add_backup = TRUE,
            show_results = FALSE,
            con = pool_conn
          )

          shiny::showNotification(
            i18n()$t("Synonym relationship set successfully!"),
            type = "message",
            duration = 5
          )

        }, error = function(e) {
          cli::cli_alert_danger("Failed to set synonym: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error setting synonym:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Setting synonym relationship..."))
    })

    # Backbone-suggested: set all selected as synonyms of the new taxon
    shiny::observeEvent(input$btn_confirm_backbone_synonymies, {
      shiny::req(rv$new_taxon_id)
      selected_ids <- as.integer(input$backbone_synonym_ids)

      if (length(selected_ids) == 0) {
        shiny::showNotification(
          i18n()$t("No taxa selected"),
          type = "warning"
        )
        return()
      }

      shiny::withProgress({
        tryCatch({
          pool_conn <- pool()
          actual_con <- pool::poolCheckout(pool_conn)
          on.exit(pool::poolReturn(actual_con), add = TRUE)

          n_updated <- 0L
          for (existing_id in selected_ids) {
            # Check for cascade synonyms already pointing to this existing taxon
            cascade <- DBI::dbGetQuery(
              actual_con,
              sprintf(
                "SELECT idtax_n FROM table_taxa WHERE idtax_good_n = %d AND idtax_n != %d",
                existing_id, existing_id
              )
            )
            all_ids <- c(existing_id, cascade$idtax_n)
            sql <- sprintf(
              "UPDATE table_taxa SET idtax_good_n = %d WHERE idtax_n IN (%s)",
              rv$new_taxon_id,
              paste(all_ids, collapse = ", ")
            )
            n_updated <- n_updated + DBI::dbExecute(actual_con, sql)
          }

          shiny::showNotification(
            sprintf(i18n()$t("Set %d taxon/taxa as synonym(s) of new entry"), n_updated),
            type = "message",
            duration = 5
          )

          # Clear candidates so the panel disappears
          rv$backbone_synonymy_candidates <- NULL

        }, error = function(e) {
          cli::cli_alert_danger("Failed to set backbone synonymies: {e$message}")
          shiny::showNotification(
            paste(i18n()$t("Error setting synonymies:"), e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = i18n()$t("Setting backbone-suggested synonymies..."))
    })

    # Finish and reset
    shiny::observeEvent(input$btn_finish, {
      rv$form_data <- list()
      rv$current_step <- 1
      rv$tropicos_results <- NULL
      rv$backbone_results <- NULL
      rv$backbone_selected <- character(0)
      rv$backbone_synonymy_candidates <- NULL
      rv$new_taxon_id <- NULL
      rv$existing_check <- NULL
      rv$growth_form_data <- NULL
      rv$order_required <- FALSE
      rv$class_required <- FALSE
      rv$existing_taxon_matches <- NULL
      rv$accepted_taxon_matches <- NULL

      # The growth form selector keeps its own state, which survives the step
      # panel being destroyed. Without this the next taxon inherits the growth
      # forms, basis of record and remarks of the one just added - and saves
      # them against it.
      growth_form_module$reset()

      shiny::showNotification(
        i18n()$t("Ready to add another taxon"),
        type = "message"
      )
    })

    return(NULL)
  })
}


#' Prepare Growth Form Data for Database Insert
#'
#' Converts hierarchical growth form selections into a data frame
#' suitable for add_sp_traits_measures()
#'
#' @param growth_form_selections List of growth form paths
#' @param idtax Taxon ID
#'
#' @return Data frame with one row per path, traits as columns
#' @keywords internal
.prepare_growth_form_data <- function(growth_form_selections, idtax) {

  if (length(growth_form_selections) == 0) {
    return(NULL)
  }

  # Initialize empty list to collect rows
  all_rows <- list()

  # Process each path (each path represents one complete growth form selection)
  for (path_idx in seq_along(growth_form_selections)) {
    path <- growth_form_selections[[path_idx]]

    # Create a row for this path
    row_data <- list(idtax = idtax)

    # Add each level in the path as a column
    for (level in path) {
      trait_name <- level$trait
      trait_value <- level$value

      # Add trait column to row
      row_data[[trait_name]] <- trait_value
    }

    # Convert to data frame and add to collection
    all_rows[[path_idx]] <- as.data.frame(row_data, stringsAsFactors = FALSE)
  }

  # Combine all rows
  if (length(all_rows) == 1) {
    result <- all_rows[[1]]
  } else {
    # Use rbind with fill for missing columns
    result <- dplyr::bind_rows(all_rows)
  }

  return(result)
}


#' Add Growth Forms to Database (Non-Interactive)
#'
#' Directly inserts growth form measurements without interactive prompts
#'
#' @param idtax Taxon ID
#' @param growth_form_selections List of growth form paths
#' @param basisofrecord Basis of record
#' @param measurementremarks Measurement remarks
#' @param pool Database connection pool
#'
#' @return NULL (silently adds data)
#' @keywords internal
.add_growth_forms_noninteractive <- function(idtax,
                                             growth_form_selections,
                                             basisofrecord,
                                             measurementremarks,
                                             pool) {

  if (length(growth_form_selections) == 0) {
    cli::cli_alert_warning("No growth forms to add")
    return(invisible(NULL))
  }

  # Get connection
  actual_con <- if (inherits(pool, "Pool")) {
    pool::poolCheckout(pool)
  } else {
    pool
  }

  on.exit({
    if (inherits(pool, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Process each growth form path
  for (path_idx in seq_along(growth_form_selections)) {
    path <- growth_form_selections[[path_idx]]

    # Insert each level in the path as a separate measurement
    for (level in path) {
      id_trait <- level$id_trait
      trait_value <- level$value

      # Prepare measurement record
      measurement <- data.frame(
        idtax = idtax,
        fk_id_trait = id_trait,
        traitvalue_char = trait_value,
        traitvalue = NA_real_,
        basisofrecord = basisofrecord,
        measurementremarks = if (is.null(measurementremarks) || measurementremarks == "") NA_character_ else measurementremarks,
        stringsAsFactors = FALSE
      )

      # Insert into taxa_traits_measures
      tryCatch({
        DBI::dbAppendTable(actual_con, "taxa_traits_measures", measurement)
        cli::cli_alert_success("Added {level$trait} = {trait_value}")
      }, error = function(e) {
        cli::cli_alert_warning("Failed to add {level$trait}: {e$message}")
        stop(e)
      })
    }
  }

  cli::cli_alert_success("All growth forms added for taxon {idtax}")
  return(invisible(NULL))
}


#' Merge the synonymy candidates suggested by several backbones
#'
#' @description
#' Each backbone is asked separately which internal taxa share the accepted
#' name of the identifier selected in it, so the same taxon can come back from
#' two backbones. This keeps one row per internal taxon and collapses what each
#' backbone said about it into a single `sources` string, so the user is
#' offered each candidate once, with the evidence for it.
#'
#' @param frames List of data frames from [.backbone_synonymy_candidates()],
#'   each with the extra columns `backbone` and `backbone_label`. `NULL`
#'   entries and empty frames are ignored.
#'
#' @return A data frame with one row per `idtax_n` and an added `sources`
#'   column, or `NULL` when no backbone suggested anything.
#'
#' @keywords internal
.merge_synonymy_candidates <- function(frames) {

  frames <- Filter(
    function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0,
    frames
  )
  if (length(frames) == 0) return(NULL)

  all_rows <- dplyr::bind_rows(frames)
  if (nrow(all_rows) == 0) return(NULL)

  evidence <- paste0(
    all_rows$backbone_label, ": ", all_rows$backbone_name,
    ifelse(is.na(all_rows$status_raw) | all_rows$status_raw == "",
           "", paste0(" [", all_rows$status_raw, "]"))
  )

  collapsed <- tapply(
    evidence, as.character(all_rows$idtax_n),
    function(x) paste(unique(x), collapse = " | ")
  )

  out <- all_rows[!duplicated(all_rows$idtax_n), , drop = FALSE]
  out$sources <- as.character(collapsed[as.character(out$idtax_n)])
  out
}
