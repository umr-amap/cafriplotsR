# Feature Wizard - Step 3: Import a full census
#
# Takes the single flat table a field team actually produces — recruits and
# existing stems interleaved, traits in columns — and does the work the user
# would otherwise do by hand: classify every row against the database, then
# route the pieces to the right tables.

#' Feature Wizard Step 3: Full Census Import - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step3_census_import_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("file-import"),
      i18n$t("Step 3: Import a Full Census"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::div(
      class = "alert alert-info",
      shiny::icon("info-circle"), " ",
      i18n$t("Upload one flat table containing every stem measured during the campaign — both stems already in the database and new recruits. The wizard works out which is which by comparing tags against the database, so you do not have to split the file yourself.")
    ),

    # ---- Census identity --------------------------------------------------
    shiny::h4(shiny::icon("calendar"), " ", i18n$t("Census")),
    shiny::uiOutput(ns("census_identity_ui")),

    shiny::hr(),

    # ---- File upload ------------------------------------------------------
    shiny::h4(shiny::icon("upload"), " ", i18n$t("Census Table")),
    shiny::fileInput(
      ns("xlsx_file"),
      i18n$t("Upload xlsx file"),
      accept = c(".xlsx", ".xls"),
      width = "100%"
    ),

    # ---- Column mapping ---------------------------------------------------
    shiny::uiOutput(ns("column_mapping_ui")),

    # ---- Split review -----------------------------------------------------
    shiny::uiOutput(ns("split_summary_ui")),
    shiny::uiOutput(ns("split_details_ui")),

    # ---- Prepared data ----------------------------------------------------
    shiny::uiOutput(ns("prepared_message"))
  )
}


#' Feature Wizard Step 3: Full Census Import - Server
#'
#' @param id Module namespace ID
#' @param selected_plots Reactive containing data.frame of selected plots
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive containing list(data, config)
#' @keywords internal
#' @export
mod_feat_step3_census_import_server <- function(id, selected_plots, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    uploaded_raw     <- shiny::reactiveVal(NULL)
    available_traits <- shiny::reactiveVal(NULL)
    census_choices   <- shiny::reactiveVal(NULL)
    split_result     <- shiny::reactiveVal(NULL)
    prepared         <- shiny::reactiveVal(NULL)
    prepared_config  <- shiny::reactiveVal(NULL)

    # ---- reference data ---------------------------------------------------

    shiny::observe({
      shiny::req(con())
      tryCatch({
        actual_con <- if (inherits(con(), "Pool")) pool::poolCheckout(con()) else con()
        on.exit(if (inherits(con(), "Pool")) pool::poolReturn(actual_con), add = TRUE)
        traits <- DBI::dbGetQuery(actual_con,
          "SELECT id_trait, trait, valuetype, traitdescription, category,
                  expectedunit, minallowedvalue, maxallowedvalue, factorlevels
           FROM traitlist ORDER BY trait")
        available_traits(traits)
      }, error = function(e) {
        cli::cli_alert_warning("Could not load trait list: {e$message}")
      })
    })

    shiny::observe({
      shiny::req(con(), selected_plots())
      tryCatch({
        actual_con <- if (inherits(con(), "Pool")) pool::poolCheckout(con()) else con()
        on.exit(if (inherits(con(), "Pool")) pool::poolReturn(actual_con), add = TRUE)
        census_choices(.fetch_census_subplots(selected_plots()$id_liste_plots, actual_con))
      }, error = function(e) {
        cli::cli_alert_warning("Could not load censuses: {e$message}")
        census_choices(NULL)
      })
    })

    # ---- census identity --------------------------------------------------

    output$census_identity_ui <- shiny::renderUI({
      plots <- selected_plots()
      shiny::req(plots)
      censuses <- census_choices()

      next_census <- 1L
      if (!is.null(censuses) && nrow(censuses) > 0) {
        mx <- suppressWarnings(max(as.numeric(censuses$census_num), na.rm = TRUE))
        if (is.finite(mx)) next_census <- as.integer(mx + 1)
      }

      existing_choices <- if (!is.null(censuses) && nrow(censuses) > 0) {
        stats::setNames(
          censuses$id_sub_plots,
          sprintf("%s — census %s (%s)",
                  plots$plot_name[match(censuses$id_table_liste_plots, plots$id_liste_plots)],
                  censuses$census_num,
                  ifelse(is.na(censuses$year), "?", censuses$year))
        )
      } else {
        NULL
      }

      shiny::tagList(
        shiny::radioButtons(
          ns("census_mode"), NULL,
          choices = stats::setNames(
            c("create", "existing"),
            c(i18n()$t("Create a new census for the selected plots"),
              i18n()$t("Use a census that already exists"))
          ),
          selected = if (is.null(existing_choices)) "create" else "create"
        ),

        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'create'", ns("census_mode")),
          shiny::fluidRow(
            shiny::column(3, shiny::numericInput(
              ns("census_number"), i18n()$t("Census Number"),
              value = next_census, min = 1, step = 1
            )),
            shiny::column(3, shiny::numericInput(
              ns("census_year"), i18n()$t("Year *"),
              value = as.integer(format(Sys.Date(), "%Y")),
              min = 1900, max = 2100, step = 1
            )),
            shiny::column(3, shiny::numericInput(
              ns("census_month"), i18n()$t("Month"), value = NA,
              min = 1, max = 12, step = 1
            )),
            shiny::column(3, shiny::numericInput(
              ns("census_day"), i18n()$t("Day"), value = NA,
              min = 1, max = 31, step = 1
            ))
          ),
          shiny::div(
            class = "alert alert-secondary", style = "font-size: 13px;",
            shiny::icon("users"), " ",
            i18n()$t("Team members are not recorded here. To attach a team leader, principal investigator or data manager to this campaign, create the census with the New Census mode first and then select it above.")
          )
        ),

        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'existing'", ns("census_mode")),
          if (is.null(existing_choices)) {
            shiny::div(
              class = "alert alert-warning",
              shiny::icon("exclamation-triangle"), " ",
              i18n()$t("No census exists yet for the selected plots.")
            )
          } else {
            shiny::selectInput(
              ns("census_ids"), i18n()$t("Census (one per plot)"),
              choices = existing_choices, multiple = TRUE
            )
          }
        )
      )
    })

    # ---- upload -----------------------------------------------------------

    shiny::observeEvent(input$xlsx_file, {
      shiny::req(input$xlsx_file)
      uploaded_raw(NULL); split_result(NULL); prepared(NULL); prepared_config(NULL)

      tryCatch({
        raw <- as.data.frame(readxl::read_excel(input$xlsx_file$datapath, guess_max = 5000))
        if (nrow(raw) == 0) {
          shiny::showNotification(i18n()$t("Uploaded file is empty."), type = "error")
          return()
        }
        uploaded_raw(raw)
        shiny::showNotification(
          sprintf(i18n()$t("Loaded %d rows, %d columns. Please map columns below."),
                  nrow(raw), ncol(raw)),
          type = "message", duration = 5
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error reading file:", e$message),
                                type = "error", duration = 10)
      })
    })

    # ---- column mapping ---------------------------------------------------

    output$column_mapping_ui <- shiny::renderUI({
      raw <- uploaded_raw()
      # Deliberately not reading available_traits() here: the trait dropdowns
      # live in their own output, and taking the dependency would re-render
      # this panel when the trait list arrives, wiping the user's choices
      if (is.null(raw)) return(NULL)

      user_cols <- names(raw)
      none <- c("-- not mapped --" = "")

      synonyms <- tryCatch(.get_individual_column_synonyms(), error = function(e) list())
      synonyms$plot_name <- unique(c(synonyms$plot_name, "plotname", "plot",
                                     "parcelle", "site"))
      synonyms$tag <- unique(c(synonyms$tag, "tag_number", "individual",
                               "tree_number", "numero", "num", "tree_id",
                               "ind", "arbre", "no_arbre"))
      auto <- .auto_match_columns(user_cols, synonyms)

      key_pick <- function(field, label) {
        shiny::column(6, shiny::selectInput(
          ns(paste0("map_", field)), label,
          choices = c(none, stats::setNames(user_cols, user_cols)),
          selected = .null_default(auto[[field]], "")
        ))
      }

      # Columns that describe the individual rather than a measurement. They
      # are only written for rows that turn out to be recruits.
      individual_fields <- c("idtax_n", "original_tax_name", "sous_plot_name",
                             "multi_tiges_id", "herbarium_nbe_char",
                             "herbarium_nbe_type")

      shiny::tagList(
        shiny::hr(),
        shiny::h4(shiny::icon("exchange-alt"), " ", i18n()$t("Map Key Columns")),
        shiny::fluidRow(
          key_pick("plot_name", i18n()$t("Plot name column *")),
          key_pick("tag", i18n()$t("Tag column *"))
        ),

        shiny::h4(shiny::icon("tree"), " ", i18n()$t("Individual Columns")),
        shiny::p(
          i18n()$t("These describe the tree itself. They are written only for rows identified as recruits; for stems already in the database they are ignored."),
          style = "color: #6c757d;"
        ),
        shiny::fluidRow(
          lapply(individual_fields, function(f) {
            shiny::column(4, shiny::selectInput(
              ns(paste0("map_", f)), f,
              choices = c(none, stats::setNames(user_cols, user_cols)),
              selected = .null_default(auto[[f]], "")
            ))
          })
        ),

        shiny::uiOutput(ns("trait_mapping_ui")),

        shiny::div(
          style = "text-align: center; margin-top: 20px;",
          shiny::actionButton(
            ns("apply_mapping"),
            shiny::tagList(shiny::icon("wand-magic-sparkles"), " ",
                           i18n()$t("Classify Rows & Preview")),
            class = "btn-success btn-lg"
          )
        )
      )
    })

    # Remaining columns are offered as traits
    output$trait_mapping_ui <- shiny::renderUI({
      raw <- uploaded_raw()
      traits <- available_traits()
      if (is.null(raw) || is.null(traits)) return(NULL)

      used <- .census_mapped_columns(input)
      free <- setdiff(names(raw), used)
      if (length(free) == 0) return(NULL)

      choices <- .build_grouped_trait_choices(traits)
      trait_names <- traits$trait

      shiny::tagList(
        shiny::h4(shiny::icon("tags"), " ", i18n()$t("Map Columns to Traits")),
        shiny::p(
          i18n()$t("Every remaining column can become a measurement. Leave a column on '-- skip --' to ignore it."),
          style = "color: #6c757d;"
        ),
        shiny::fluidRow(
          lapply(free, function(col) {
            safe <- gsub("[^a-zA-Z0-9]", "_", col)
            guess <- if (col %in% trait_names) col else ""
            shiny::column(4, shiny::selectInput(
              ns(paste0("trait_map_", safe)), col,
              choices = choices, selected = guess
            ))
          })
        )
      )
    })

    # ---- classify ---------------------------------------------------------

    shiny::observeEvent(input$apply_mapping, {
      shiny::req(uploaded_raw(), selected_plots(), con())

      raw   <- uploaded_raw()
      plots <- selected_plots()

      plot_col <- input$map_plot_name
      tag_col  <- input$map_tag
      if (is.null(plot_col) || plot_col == "" || is.null(tag_col) || tag_col == "") {
        shiny::showNotification(
          i18n()$t("Please map both the plot name and the tag column."), type = "error")
        return()
      }

      shiny::withProgress(message = i18n()$t("Classifying rows..."), {
        tryCatch({
          df <- raw
          names(df)[names(df) == plot_col] <- "plot_name"
          if (tag_col != "tag") names(df)[names(df) == tag_col] <- "tag"

          # Individual columns are renamed to their database names up front so
          # the split and the insert see the same thing
          for (f in c("idtax_n", "original_tax_name", "sous_plot_name",
                      "multi_tiges_id", "herbarium_nbe_char", "herbarium_nbe_type")) {
            src <- input[[paste0("map_", f)]]
            if (!is.null(src) && nzchar(src) && src %in% names(df) && src != f) {
              names(df)[names(df) == src] <- f
            }
          }

          shiny::setProgress(0.5)

          actual_con <- if (inherits(con(), "Pool")) pool::poolCheckout(con()) else con()
          on.exit(if (inherits(con(), "Pool")) pool::poolReturn(actual_con), add = TRUE)

          split <- split_census_table(
            data       = df,
            plot_names = plots$plot_name,
            con        = actual_con
          )
          split_result(split)

          shiny::showNotification(
            sprintf(i18n()$t("%d remeasure(s), %d recruit(s), %d row(s) to review."),
                    nrow(split$remeasures), nrow(split$recruits), nrow(split$review)),
            type = "message", duration = 6
          )
        }, error = function(e) {
          shiny::showNotification(paste("Error:", e$message), type = "error", duration = 10)
          split_result(NULL)
        })
      })
    })

    # ---- split review -----------------------------------------------------

    output$split_summary_ui <- shiny::renderUI({
      split <- split_result()
      if (is.null(split)) return(NULL)

      card <- function(n, label, colour) {
        shiny::column(3, shiny::div(
          style = sprintf(
            "background: %s; color: white; padding: 15px; border-radius: 8px; text-align: center;",
            colour),
          shiny::h3(n, style = "margin: 0; font-weight: bold;"),
          shiny::div(label, style = "font-size: 13px;")
        ))
      }

      shiny::tagList(
        shiny::hr(),
        shiny::h4(shiny::icon("code-branch"), " ", i18n()$t("Row Classification")),
        shiny::fluidRow(
          card(nrow(split$remeasures), i18n()$t("Remeasures"), "#17a2b8"),
          card(nrow(split$recruits),   i18n()$t("Recruits"),   "#fd7e14"),
          card(nrow(split$review),     i18n()$t("To review"),  "#dc3545"),
          card(nrow(split$missing_stems), i18n()$t("Not seen"), "#6c757d")
        ),
        shiny::br(),
        DT::renderDT(
          DT::datatable(split$summary, rownames = FALSE,
                        options = list(dom = "t", scrollX = TRUE))
        )
      )
    })

    output$split_details_ui <- shiny::renderUI({
      split <- split_result()
      if (is.null(split)) return(NULL)

      blocks <- list()

      # The review pile holds two different problems with two different
      # remedies, so they get two panels. Only the typo one can be confirmed
      # away — an ambiguous row has no tag to correct.
      n_ambiguous <- nrow(split$ambiguous)
      n_typo <- nrow(split$review) - n_ambiguous

      if (n_ambiguous > 0) {
        blocks <- c(blocks, list(shiny::div(
          class = "alert alert-danger",
          shiny::icon("code-branch"), " ",
          shiny::strong(sprintf(
            i18n()$t("%d row(s) match more than one recorded stem."),
            n_ambiguous)),
          shiny::br(),
          i18n()$t("These plots numbered tags per quadrat, so several recorded stems carry the same tag and the tag alone cannot say which tree was measured. These rows are excluded from the import — they have to be resolved against the individual ids listed below."),
          shiny::br(), shiny::br(),
          DT::renderDT(DT::datatable(
            split$ambiguous, rownames = FALSE,
            options = list(pageLength = 5, scrollX = TRUE, dom = "tp")
          ))
        )))
      }

      if (n_typo > 0) {
        blocks <- c(blocks, list(shiny::div(
          class = "alert alert-danger",
          shiny::icon("triangle-exclamation"), " ",
          shiny::strong(sprintf(
            i18n()$t("%d row(s) have a tag that does not exist but closely resembles one that does."),
            n_typo)),
          shiny::br(),
          i18n()$t("These are usually mistyped tags. Importing them would create a duplicate tree. Correct them in your file and upload it again, or confirm below that they really are new stems."),
          shiny::br(), shiny::br(),
          DT::renderDT(DT::datatable(
            split$possible_typos, rownames = FALSE,
            options = list(pageLength = 5, scrollX = TRUE, dom = "tp")
          )),
          shiny::checkboxInput(
            ns("confirm_review"),
            i18n()$t("I checked these tags — treat them as new recruits"),
            value = FALSE
          )
        )))
      }

      if (nrow(split$invalid) > 0) {
        blocks <- c(blocks, list(shiny::div(
          class = "alert alert-danger",
          shiny::icon("ban"), " ",
          sprintf(i18n()$t("%d row(s) have no usable plot name or tag and will be skipped."),
                  nrow(split$invalid))
        )))
      }

      if (nrow(split$duplicates) > 0) {
        blocks <- c(blocks, list(shiny::div(
          class = "alert alert-warning",
          shiny::icon("clone"), " ",
          sprintf(i18n()$t("%d plot + tag combination(s) appear more than once in the file."),
                  nrow(split$duplicates)),
          DT::renderDT(DT::datatable(
            split$duplicates, rownames = FALSE,
            options = list(pageLength = 5, dom = "tp")
          ))
        )))
      }

      if (nrow(split$taxon_drift) > 0) {
        blocks <- c(blocks, list(shiny::div(
          class = "alert alert-warning",
          shiny::icon("dna"), " ",
          sprintf(i18n()$t("%d remeasured stem(s) carry a different taxon than the database. The database value is kept."),
                  nrow(split$taxon_drift)),
          DT::renderDT(DT::datatable(
            split$taxon_drift, rownames = FALSE,
            options = list(pageLength = 5, scrollX = TRUE, dom = "tp")
          ))
        )))
      }

      if (nrow(split$missing_stems) > 0) {
        blocks <- c(blocks, list(shiny::div(
          class = "alert alert-info",
          shiny::icon("magnifying-glass"), " ",
          sprintf(i18n()$t("%d stem(s) recorded in these plots have no row in this file. Run Compute Stem Status after the import to mark them."),
                  nrow(split$missing_stems)),
          DT::renderDT(DT::datatable(
            split$missing_stems, rownames = FALSE,
            options = list(pageLength = 5, scrollX = TRUE, dom = "tp")
          ))
        )))
      }

      if (length(blocks) == 0) return(NULL)
      shiny::tagList(blocks)
    })

    # ---- build the prepared payload ---------------------------------------

    shiny::observe({
      split <- split_result()
      traits <- available_traits()
      plots <- selected_plots()
      if (is.null(split) || is.null(traits) || is.null(plots)) {
        prepared(NULL); prepared_config(NULL)
        return()
      }

      # Review rows join the recruits only once the user has confirmed them
      include_review <- isTRUE(input$confirm_review)

      res <- tryCatch(
        .build_census_payload(
          split           = split,
          plots           = plots,
          traits          = traits,
          trait_mapping   = .census_trait_mapping(input, split$data, traits),
          include_review  = include_review,
          census_mode     = .null_default(input$census_mode, "create"),
          census_number   = input$census_number,
          census_year     = input$census_year,
          census_month    = input$census_month,
          census_day      = input$census_day,
          census_map      = .census_selected_map(input$census_ids, census_choices())
        ),
        error = function(e) {
          cli::cli_alert_warning("Could not build census payload: {e$message}")
          NULL
        }
      )

      if (is.null(res)) { prepared(NULL); prepared_config(NULL); return() }
      prepared(res$data)
      prepared_config(res$config)
    })

    output$prepared_message <- shiny::renderUI({
      d <- prepared()
      cfg <- prepared_config()
      split <- split_result()

      if (is.null(split)) {
        return(shiny::div(
          class = "alert alert-secondary",
          shiny::icon("info-circle"), " ",
          i18n()$t("Upload a file and map the columns, then classify the rows.")
        ))
      }
      if (is.null(d) || nrow(d) == 0) {
        return(shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"), " ",
          i18n()$t("No measurement could be prepared. Map at least one column to a trait.")
        ))
      }

      shiny::div(
        class = "alert alert-success",
        shiny::icon("check-circle"), " ",
        sprintf(
          i18n()$t("Ready: %d measurement(s) for %d stem(s), including %d new individual(s)."),
          nrow(d), length(unique(paste(d$plot_name, d$tag))), nrow(cfg$recruits)
        )
      )
    })

    return(shiny::reactive({
      d <- prepared(); cfg <- prepared_config()
      if (is.null(d) || is.null(cfg)) return(NULL)
      list(data = d, config = cfg)
    }))
  })
}


# ============================================================
# Internal helpers (no Shiny reactivity — testable directly)
# ============================================================

#' Columns already claimed as keys or individual attributes
#'
#' @param input Shiny input object.
#' @return Character vector of column names.
#' @keywords internal
#' @export
.census_mapped_columns <- function(input) {
  fields <- c("plot_name", "tag", "idtax_n", "original_tax_name",
              "sous_plot_name", "multi_tiges_id", "herbarium_nbe_char",
              "herbarium_nbe_type")
  vals <- vapply(fields, function(f) {
    v <- input[[paste0("map_", f)]]
    if (is.null(v)) "" else as.character(v)
  }, character(1))
  unname(vals[nzchar(vals)])
}


#' Collect the column-to-trait mapping chosen in the UI
#'
#' @param input Shiny input object.
#' @param data Data frame whose columns were offered.
#' @param traits Trait table from `traitlist`.
#' @return Named character vector, column name to trait name.
#' @keywords internal
#' @export
.census_trait_mapping <- function(input, data, traits) {
  used <- c(.census_mapped_columns(input),
            "plot_name", "tag", "idtax_n", "original_tax_name",
            "sous_plot_name", "multi_tiges_id", "herbarium_nbe_char",
            "herbarium_nbe_type", "row_id", "row_role", "id_n", "split_note")
  free <- setdiff(names(data), used)

  mapping <- character(0)
  for (col in free) {
    safe <- gsub("[^a-zA-Z0-9]", "_", col)
    sel <- input[[paste0("trait_map_", safe)]]
    if (!is.null(sel) && nzchar(sel) && sel %in% traits$trait) {
      mapping[[col]] <- sel
    }
  }
  mapping
}


#' Census subplot map for the ids picked in the UI
#'
#' @param census_ids Selected `id_sub_plots` values.
#' @param censuses Table from [.fetch_census_subplots()].
#' @return Data frame with `id_table_liste_plots` and `id_sub_plots`.
#' @keywords internal
#' @export
.census_selected_map <- function(census_ids, censuses) {
  if (is.null(census_ids) || length(census_ids) == 0 || is.null(censuses)) return(NULL)
  keep <- censuses[censuses$id_sub_plots %in% as.integer(census_ids), , drop = FALSE]
  if (nrow(keep) == 0) return(NULL)
  keep[, c("id_table_liste_plots", "id_sub_plots"), drop = FALSE]
}


#' Turn a classified census table into measurements plus recruits
#'
#' The wide table becomes one measurement row per stem per trait, and the
#' recruit rows become a frame shaped for `data_individuals`. Rows held for
#' review are left out unless `include_review` says otherwise.
#'
#' @param split A `census_split` from [split_census_table()].
#' @param plots Selected plots, with `plot_name` and `id_liste_plots`.
#' @param traits Trait table from `traitlist`.
#' @param trait_mapping Named character vector, column to trait name.
#' @param include_review Treat reviewed rows as recruits?
#' @param census_mode `"create"` or `"existing"`.
#' @param census_number,census_year,census_month,census_day Census identity
#'   when creating one.
#' @param census_map Census map when reusing one.
#' @return List with `data` (long measurements) and `config`.
#' @keywords internal
#' @export
.build_census_payload <- function(split, plots, traits, trait_mapping,
                                  include_review = FALSE,
                                  census_mode = "create",
                                  census_number = NA, census_year = NA,
                                  census_month = NA, census_day = NA,
                                  census_map = NULL) {

  data <- split$data
  roles <- if (include_review) c("remeasure", "recruit", "review")
           else c("remeasure", "recruit")

  # A row matching several recorded stems is held for review too, but it is
  # the opposite of a recruit — promoting it would add a second copy of a tree
  # that is already there. include_review does not reach it.
  ambiguous_ids <- if (is.null(split$ambiguous)) integer(0) else split$ambiguous$row_id
  keep <- data[data$row_role %in% roles &
                 !(data$row_id %in% ambiguous_ids), , drop = FALSE]

  if (nrow(keep) == 0) {
    return(list(data = keep[0, , drop = FALSE], config = NULL))
  }

  keep$id_liste_plots <- plots$id_liste_plots[match(keep$plot_name, plots$plot_name)]

  # ---- recruits ---------------------------------------------------------
  recruit_roles <- if (include_review) c("recruit", "review") else "recruit"
  rec <- keep[keep$row_role %in% recruit_roles, , drop = FALSE]

  recruits <- data.frame(
    plot_name              = as.character(rec$plot_name),
    id_table_liste_plots_n = as.integer(rec$id_liste_plots),
    tag                    = .normalize_tag(rec$tag),
    stringsAsFactors       = FALSE
  )
  for (f in c("idtax_n", "original_tax_name", "herbarium_nbe_char",
              "herbarium_nbe_type", "multi_tiges_id")) {
    if (f %in% names(rec)) recruits[[f]] <- rec[[f]]
  }

  # Same convention as the Import Wizard: a stem with no taxon is recorded as
  # unidentified rather than rejected. Step 5 warns about the count.
  # rep() rather than a scalar: a census where every stem is already known has
  # no recruits at all, and assigning a length-1 value to a 0-row frame errors
  if (!"idtax_n" %in% names(recruits)) {
    recruits$idtax_n <- rep(NA_integer_, nrow(recruits))
  }
  recruits$idtax_n <- suppressWarnings(as.integer(recruits$idtax_n))
  n_unidentified <- sum(is.na(recruits$idtax_n))
  recruits$idtax_n[is.na(recruits$idtax_n)] <- 351190L

  # ---- measurements -----------------------------------------------------
  trait_ids <- stats::setNames(traits$id_trait, traits$trait)
  trait_types <- stats::setNames(traits$valuetype, traits$trait)

  long <- list()
  for (col in names(trait_mapping)) {
    trait_name <- trait_mapping[[col]]
    if (!col %in% names(keep)) next

    vals <- keep[[col]]
    has <- !is.na(vals) & nzchar(trimws(as.character(vals)))
    if (!any(has)) next

    is_num <- isTRUE(trait_types[[trait_name]] %in%
                       c("numeric", "integer", "table_colnam"))

    long[[col]] <- data.frame(
      plot_name       = as.character(keep$plot_name[has]),
      tag             = .normalize_tag(keep$tag[has]),
      row_role        = as.character(keep$row_role[has]),
      id_liste_plots  = as.integer(keep$id_liste_plots[has]),
      trait_name      = trait_name,
      traitid         = as.integer(trait_ids[[trait_name]]),
      traitvalue      = if (is_num) suppressWarnings(as.numeric(vals[has])) else NA_real_,
      traitvalue_char = if (!is_num) as.character(vals[has]) else NA_character_,
      stringsAsFactors = FALSE
    )
  }

  measurements <- if (length(long) == 0) {
    data.frame(plot_name = character(0), tag = character(0),
               row_role = character(0), id_liste_plots = integer(0),
               trait_name = character(0), traitid = integer(0),
               traitvalue = numeric(0), traitvalue_char = character(0),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, long)
  }
  rownames(measurements) <- NULL

  list(
    data = measurements,
    config = list(
      mode          = "import_census",
      recruits      = recruits,
      split         = split,
      census_mode   = census_mode,
      census_map    = census_map,
      census_number = census_number,
      census_year   = census_year,
      census_month  = census_month,
      census_day    = census_day,
      trait_mapping = trait_mapping,
      n_unidentified_recruits = n_unidentified,
      # Ambiguous rows are never included, so they do not count here
      n_review_included = if (include_review) {
        nrow(split$review) - length(ambiguous_ids)
      } else 0L
    )
  )
}
