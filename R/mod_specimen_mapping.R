# Specimen Import Wizard - Step 2: Column Mapping
#
# Module for mapping user columns to database specimen columns.
#
# The automatic column guessing reuses the same engine as the plot/individual
# import wizard (`map_user_columns()` in import_column_mapping.R): exact match,
# then a domain-specific synonym dictionary, then fuzzy string matching, with
# category-aware scoring and de-duplication. This replaces the much weaker
# first-pattern-wins `.find_column_match()` approach used previously.

#' Specimen Column Synonym Dictionary (Internal)
#'
#' Maps each specimen database field to a vector of common column-name
#' variations (including French equivalents). Used to feed the shared
#' \code{\link{map_user_columns}} engine so specimen auto-detection matches the
#' quality of the plot/individual import wizard.
#'
#' @return Named list: names are specimen database fields, values are character
#'   vectors of synonyms.
#' @keywords internal
.get_specimen_column_synonyms <- function() {
  list(
    # ----- Required fields -----
    collector = c(
      "collector", "collecteur", "recolteur", "coll", "collname", "coll_name",
      "collector_name", "nom_collecteur", "colnam", "leg", "legit"
    ),
    colnbr = c(
      "colnbr", "col_nbr", "colno", "collno", "collection_number",
      "collector_number", "numero_collecteur", "numero_recolte", "num_recolte",
      "field_number", "fieldno", "numero", "num", "nbr", "number"
    ),
    idtax_n = c(
      "idtax_n", "idtax", "id_tax", "id_taxon", "taxon_id", "taxonid",
      "tax_id", "idtaxon"
    ),

    # ----- Determination -----
    suffix = c("suffix", "suffixe", "suf"),
    det_by = c(
      "det_by", "detby", "determined_by", "determinateur", "determiner",
      "determinavit", "identified_by", "det_name", "det"
    ),
    original_tax_name = c(
      "original_tax_name", "original_name", "orig_tax", "tax_original",
      "original_taxon", "original_determination", "nom_original",
      "taxon_name", "scientific_name", "nom_scientifique", "species_name",
      "species", "espece", "field_det", "field_name"
    ),
    det_year = c(
      "det_year", "dety", "det_y", "determination_year", "year_det",
      "annee_det", "year", "annee"
    ),
    det_month = c(
      "det_month", "detm", "det_m", "determination_month", "month", "mois"
    ),
    det_day = c(
      "det_day", "detd", "det_d", "determination_day", "day", "jour"
    ),

    # ----- Collection -----
    col_year = c(
      "coly", "col_year", "collection_year", "year_coll", "recolte_year",
      "annee_recolte"
    ),
    col_month = c(
      "colm", "col_month", "collection_month", "month_coll", "mois_recolte"
    ),
    col_day = c(
      "cold", "colday", "col_day", "collection_day", "day_coll", "jour_recolte"
    ),
    add_col = c(
      "add_col", "additional_collector", "additional_collectors", "addcol",
      "coll_add", "co_collectors", "other_collectors", "autres_collecteurs"
    ),

    # ----- Location -----
    locality = c(
      "locality", "localite", "loc", "location", "lieu", "locality_name",
      "place"
    ),
    country = c("country", "pays", "pais", "ctry", "nation"),
    ddlat = c(
      "ddlat", "latitude", "lat", "decimal_latitude", "declat", "y"
    ),
    ddlon = c(
      "ddlon", "longitude", "lon", "long", "lng", "decimal_longitude",
      "declon", "x"
    ),

    # ----- Other -----
    description = c(
      "description", "desc", "notes", "note", "comment", "comments",
      "commentaire", "remarks", "remarques", "observations"
    )
  )
}


#' Specimen Import Configuration for Column Mapping (Internal)
#'
#' Builds a minimal \code{config} object compatible with
#' \code{\link{map_user_columns}} for the fixed set of specimen database fields.
#' Unlike the plot/individual import wizard, specimens have a fixed target
#' schema (no dynamic features), so no database connection is required.
#'
#' @return List with \code{direct_columns} and \code{import_config}
#'   (\code{column_synonyms}, \code{required_columns}).
#' @keywords internal
.get_specimen_import_config <- function() {
  direct_columns <- c(
    # Required
    "collector", "colnbr", "idtax_n",
    # Determination
    "suffix", "det_by", "original_tax_name", "det_year", "det_month", "det_day",
    # Collection
    "col_year", "col_month", "col_day", "add_col",
    # Location
    "locality", "country", "ddlat", "ddlon",
    # Other
    "description"
  )

  list(
    direct_columns = direct_columns,
    subplot_features = NULL,
    feature_columns = NULL,
    import_config = list(
      column_synonyms = .get_specimen_column_synonyms(),
      required_columns = c("collector", "colnbr", "idtax_n")
    )
  )
}


#' Invert a map_user_columns() Result for Specimens (Internal)
#'
#' \code{\link{map_user_columns}} returns a \code{user_column -> database_field}
#' mapping. The specimen wizard is oriented \code{database_field -> user_column},
#' so this inverts the result. Because \code{map_user_columns()} de-duplicates
#' targets, each database field maps to at most one user column.
#'
#' @param res Result list from \code{map_user_columns()} (or NULL).
#' @return Named list keyed by database field; each element is a list with
#'   \code{user_col}, \code{method}, and \code{confidence}.
#' @keywords internal
.invert_specimen_mapping <- function(res) {
  out <- list()
  if (is.null(res) || is.null(res$mappings)) {
    return(out)
  }

  m <- res$mappings
  for (user_col in names(m)) {
    field <- m[[user_col]]
    if (is.null(field) || is.na(field)) {
      next
    }
    out[[field]] <- list(
      user_col = user_col,
      method = if (!is.null(res$methods)) res$methods[[user_col]] else "synonym",
      confidence = if (!is.null(res$confidence)) res$confidence[[user_col]] else NA_real_
    )
  }
  out
}


# Specimen database field -> Shiny input id. Used throughout the server to
# iterate over all mapping dropdowns. Order matches the UI layout.
.SPECIMEN_FIELD_INPUTS <- c(
  collector         = "col_collector",
  colnbr            = "col_colnbr",
  idtax_n           = "col_idtax_n",
  suffix            = "col_suffix",
  det_by            = "col_det_by",
  original_tax_name = "col_original_tax_name",
  det_year          = "col_det_year",
  det_month         = "col_det_month",
  det_day           = "col_det_day",
  col_year          = "col_col_year",
  col_month         = "col_col_month",
  col_day           = "col_col_day",
  add_col           = "col_add_col",
  locality          = "col_locality",
  country           = "col_country",
  ddlat             = "col_ddlat",
  ddlon             = "col_ddlon",
  description       = "col_description"
)

.SPECIMEN_REQUIRED_FIELDS <- c("collector", "colnbr", "idtax_n")


#' Specimen Mapping Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_specimen_mapping_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  # Helper: a labelled mapping dropdown + reactive status badge underneath.
  # selectize = FALSE uses a native <select>, which (unlike selectize.js)
  # reliably allows re-selecting the empty "Not mapped" option, so a wrongly
  # mapped column can always be changed or removed.
  map_select <- function(field, label) {
    input_id <- .SPECIMEN_FIELD_INPUTS[[field]]
    shiny::tagList(
      shiny::selectInput(
        ns(input_id),
        label,
        choices = NULL,
        width = "100%",
        selectize = FALSE
      ),
      shiny::uiOutput(ns(paste0("badge_", field)))
    )
  }

  shiny::tagList(
    shiny::h3(
      shiny::icon("columns"),
      i18n$t("Step 2: Map Your Columns"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Match your data columns to the database fields. Required fields are marked with *"),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Auto-detect button (auto-detection also runs automatically on upload)
    shiny::actionButton(
      ns("auto_detect"),
      shiny::tagList(shiny::icon("magic"), paste0(" ", i18n$t("Auto-detect Columns"))),
      class = "btn-info",
      style = "margin-bottom: 20px;"
    ),

    # Summary of the automatic detection
    shiny::uiOutput(ns("mapping_summary")),

    # Mapping interface
    shiny::div(
      class = "mapping-container",
      style = "background: #f8f9fa; padding: 20px; border-radius: 8px;",

      # Required fields section
      shiny::h4(
        shiny::icon("asterisk", style = "color: #dc3545;"),
        paste0(" ", i18n$t("Required Fields")),
        style = "margin-bottom: 15px;"
      ),

      shiny::fluidRow(
        shiny::column(6, map_select("collector", shiny::tagList(i18n$t("Collector"), " *"))),
        shiny::column(6, map_select("colnbr", shiny::tagList(i18n$t("Specimen Number"), " *")))
      ),

      shiny::fluidRow(
        shiny::column(6, map_select("idtax_n", shiny::tagList(i18n$t("Taxonomic ID (idtax_n)"), " *"))),
        shiny::column(6, map_select("suffix", i18n$t("Suffix (optional)")))
      ),

      shiny::hr(),

      # Optional fields section
      shiny::h4(
        shiny::icon("plus-circle", style = "color: #6c757d;"),
        paste0(" ", i18n$t("Optional Fields")),
        style = "margin-bottom: 15px;"
      ),

      # Determination information
      shiny::h5(i18n$t("Determination Information"), style = "color: #6c757d; margin-top: 15px;"),
      shiny::fluidRow(
        shiny::column(6, map_select("det_by", i18n$t("Determined by (text)"))),
        shiny::column(6, map_select("original_tax_name", i18n$t("Original Taxon Name")))
      ),

      shiny::fluidRow(
        shiny::column(4, map_select("det_year", i18n$t("Determination Year"))),
        shiny::column(4, map_select("det_month", i18n$t("Determination Month"))),
        shiny::column(4, map_select("det_day", i18n$t("Determination Day")))
      ),

      # Collection information
      shiny::h5(i18n$t("Collection Information"), style = "color: #6c757d; margin-top: 15px;"),
      shiny::fluidRow(
        shiny::column(4, map_select("col_year", i18n$t("Collection Year"))),
        shiny::column(4, map_select("col_month", i18n$t("Collection Month"))),
        shiny::column(4, map_select("col_day", i18n$t("Collection Day")))
      ),

      shiny::fluidRow(
        shiny::column(6, map_select("add_col", i18n$t("Additional Collector(s)")))
      ),

      # Location information
      shiny::h5(i18n$t("Location Information"), style = "color: #6c757d; margin-top: 15px;"),
      shiny::fluidRow(
        shiny::column(6, map_select("locality", i18n$t("Locality"))),
        shiny::column(6, map_select("country", i18n$t("Country")))
      ),

      shiny::fluidRow(
        shiny::column(6, map_select("ddlat", i18n$t("Latitude (ddlat)"))),
        shiny::column(6, map_select("ddlon", i18n$t("Longitude (ddlon)")))
      ),

      # Other information
      shiny::h5(i18n$t("Other Information"), style = "color: #6c757d; margin-top: 15px;"),
      shiny::fluidRow(
        shiny::column(12, map_select("description", i18n$t("Description/Notes")))
      )
    ),

    # Mapping validation
    shiny::uiOutput(ns("mapping_validation"))
  )
}


#' Specimen Mapping Module - Server
#'
#' @param id Module namespace ID
#' @param data Reactive containing uploaded user data
#' @param con Reactive database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive list containing column mappings
#' @keywords internal
#' @export
mod_specimen_mapping_server <- function(id, data, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    field_inputs <- .SPECIMEN_FIELD_INPUTS
    required_fields <- .SPECIMEN_REQUIRED_FIELDS

    # Store mappings (DB field -> user column) and validity
    mappings <- shiny::reactiveVal(NULL)
    mapping_valid <- shiny::reactiveVal(FALSE)

    # Last applied automatic guess (DB field -> list(user_col, method, confidence)).
    # Drives the per-field status badges and the detection summary.
    guess_applied <- shiny::reactiveVal(list())

    # Dropdown choices: "Not mapped" sentinel ("") plus the user's columns.
    # Depends on i18n() for the translated "Not mapped" label.
    mapping_choices <- shiny::reactive({
      shiny::req(data())
      user_cols <- names(data())
      user_cols <- user_cols[!user_cols %in% c("_row_id")]
      c(
        stats::setNames("", paste0("-- ", i18n()$t("Not mapped"), " --")),
        stats::setNames(user_cols, user_cols)
      )
    })

    # Automatic guess using the shared import-wizard engine.
    auto_guess <- shiny::reactive({
      shiny::req(data())

      user_data <- data()
      user_data <- user_data[, !names(user_data) %in% "_row_id", drop = FALSE]

      cli::cli_alert_info("Running automatic specimen column mapping...")

      res <- tryCatch(
        map_user_columns(
          user_data = user_data,
          config = .get_specimen_import_config(),
          similarity_threshold = 0.6,
          interactive = FALSE
        ),
        error = function(e) {
          message("Specimen auto-mapping failed: ", e$message)
          NULL
        }
      )

      .invert_specimen_mapping(res)
    })

    # Apply the automatic guess to every dropdown (used on upload and on the
    # manual "Auto-detect" button).
    apply_guess <- function() {
      guess <- auto_guess()
      choices <- mapping_choices()

      for (fld in names(field_inputs)) {
        g <- guess[[fld]]
        sel <- if (!is.null(g) && !is.null(g$user_col) && !is.na(g$user_col)) {
          g$user_col
        } else {
          ""
        }
        shiny::updateSelectInput(session, field_inputs[[fld]], choices = choices, selected = sel)
      }

      guess_applied(guess)
    }

    # Auto-run detection whenever new data is uploaded
    shiny::observeEvent(data(), {
      shiny::req(data())
      apply_guess()
    }, ignoreNULL = TRUE)

    # Manual re-detection
    shiny::observeEvent(input$auto_detect, {
      shiny::req(data())
      apply_guess()
      shiny::showNotification(
        i18n()$t("Auto-detection complete. Please verify the mappings."),
        type = "message",
        duration = 3
      )
    })

    # Language change: refresh the "Not mapped" label while preserving the
    # current selection (previously this reset every dropdown).
    shiny::observeEvent(i18n(), {
      shiny::req(data())
      choices <- mapping_choices()
      for (fld in names(field_inputs)) {
        id_fld <- field_inputs[[fld]]
        shiny::updateSelectInput(
          session, id_fld,
          choices = choices,
          selected = shiny::isolate(input[[id_fld]])
        )
      }
    }, ignoreInit = TRUE)

    # Per-field status badges (exact / synonym / fuzzy / manual / not mapped)
    for (fld in names(field_inputs)) {
      local({
        f <- fld
        input_id <- field_inputs[[f]]
        is_required <- f %in% required_fields

        output[[paste0("badge_", f)]] <- shiny::renderUI({
          cur <- input[[input_id]]
          g <- guess_applied()[[f]]

          if (is.null(cur) || cur == "") {
            color <- if (is_required) "#dc3545" else "#adb5bd"
            icon_name <- if (is_required) "times-circle" else "minus-circle"
            label <- i18n()$t("Not mapped")
          } else if (!is.null(g) && identical(cur, g$user_col)) {
            method <- g$method
            if (identical(method, "exact")) {
              color <- "#28a745"; icon_name <- "check-circle"; label <- i18n()$t("Exact match")
            } else if (identical(method, "synonym")) {
              color <- "#28a745"; icon_name <- "check-circle"; label <- i18n()$t("Synonym")
            } else if (identical(method, "fuzzy")) {
              pct <- if (!is.null(g$confidence) && !is.na(g$confidence)) {
                sprintf(" (%.0f%%)", min(g$confidence, 1) * 100)
              } else {
                ""
              }
              color <- "#ffc107"; icon_name <- "question-circle"
              label <- paste0(i18n()$t("Fuzzy"), pct)
            } else {
              color <- "#007bff"; icon_name <- "edit"; label <- i18n()$t("Manual")
            }
          } else {
            # Current selection differs from the auto guess -> user set it
            color <- "#007bff"; icon_name <- "edit"; label <- i18n()$t("Manual")
          }

          shiny::tags$small(
            shiny::icon(icon_name, style = sprintf("color: %s;", color)),
            " ",
            label,
            style = sprintf("color: %s; display: block; margin-top: -10px; margin-bottom: 10px;", color)
          )
        })
      })
    }

    # Detection summary (counts by match method)
    output$mapping_summary <- shiny::renderUI({
      shiny::req(data())
      guess <- guess_applied()
      if (length(guess) == 0) {
        return(NULL)
      }

      methods <- vapply(guess, function(g) {
        if (!is.null(g$method)) g$method else "none"
      }, character(1))

      n_exact <- sum(methods == "exact")
      n_syn <- sum(methods == "synonym")
      n_fuzzy <- sum(methods == "fuzzy")
      n_auto <- length(guess)

      shiny::div(
        class = "alert alert-info",
        style = "background-color: #e7f3ff; border-left: 4px solid #007bff;",
        shiny::icon("magic"),
        " ",
        sprintf(i18n()$t("Auto-detected %d field(s):"), n_auto),
        " ",
        shiny::tags$strong(sprintf("%d %s", n_exact, i18n()$t("Exact match"))), ", ",
        shiny::tags$strong(sprintf("%d %s", n_syn, i18n()$t("Synonym"))), ", ",
        shiny::tags$strong(sprintf("%d %s", n_fuzzy, i18n()$t("Fuzzy"))), ". ",
        i18n()$t("Please verify the mappings below and adjust any that are wrong.")
      )
    })

    # Validate mappings and build the field -> user column list.
    # Optional fields are NULL when unmapped (contract used by the lookup and
    # import steps).
    shiny::observe({
      collector_ok <- !is.null(input$col_collector) && input$col_collector != ""
      colnbr_ok <- !is.null(input$col_colnbr) && input$col_colnbr != ""
      idtax_ok <- !is.null(input$col_idtax_n) && input$col_idtax_n != ""

      is_valid <- collector_ok && colnbr_ok && idtax_ok
      mapping_valid(is_valid)

      if (is_valid) {
        opt <- function(x) if (!is.null(x) && x != "") x else NULL

        current_mappings <- list(
          # Required fields
          collector = input$col_collector,
          colnbr = input$col_colnbr,
          idtax_n = input$col_idtax_n,

          # Optional fields
          suffix = opt(input$col_suffix),

          # Determination
          det_by = opt(input$col_det_by),
          original_tax_name = opt(input$col_original_tax_name),
          det_year = opt(input$col_det_year),
          det_month = opt(input$col_det_month),
          det_day = opt(input$col_det_day),

          # Collection
          col_year = opt(input$col_col_year),
          col_month = opt(input$col_col_month),
          col_day = opt(input$col_col_day),
          add_col = opt(input$col_add_col),

          # Location
          locality = opt(input$col_locality),
          country = opt(input$col_country),
          ddlat = opt(input$col_ddlat),
          ddlon = opt(input$col_ddlon),

          # Other
          description = opt(input$col_description)
        )
        mappings(current_mappings)
      } else {
        mappings(NULL)
      }
    })

    # Mapping validation display
    output$mapping_validation <- shiny::renderUI({
      collector_ok <- !is.null(input$col_collector) && input$col_collector != ""
      colnbr_ok <- !is.null(input$col_colnbr) && input$col_colnbr != ""
      idtax_ok <- !is.null(input$col_idtax_n) && input$col_idtax_n != ""

      all_ok <- collector_ok && colnbr_ok && idtax_ok

      shiny::div(
        style = "margin-top: 20px;",

        if (all_ok) {
          shiny::div(
            class = "alert alert-success",
            shiny::icon("check-circle"),
            " ",
            i18n()$t("All required fields are mapped. You can proceed to the next step.")
          )
        } else {
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            " ",
            i18n()$t("Please map all required fields:"),
            shiny::tags$ul(
              if (!collector_ok) shiny::tags$li(i18n()$t("Collector column is required")),
              if (!colnbr_ok) shiny::tags$li(i18n()$t("Specimen number column is required")),
              if (!idtax_ok) shiny::tags$li(i18n()$t("Taxonomic ID (idtax_n) column is required"))
            )
          )
        }
      )
    })

    # Return mappings and validity
    return(list(
      mappings = mappings,
      is_valid = mapping_valid
    ))
  })
}
