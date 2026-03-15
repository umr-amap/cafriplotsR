# =============================================================================
# Taxa Traits Import - Metadata Column Mapping Module
#
# Maps user columns to metadata fields: taxon ID (required), flat metadata
# columns stored directly in taxa_traits_measures, and supplementary trait
# features stored in taxa_traits_measures_feat via traitlist.
# =============================================================================

#' Trait Metadata Mapping Module - UI
#' @param id Module namespace ID
#' @keywords internal
#' @export
mod_trait_metadata_mapping_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shiny::uiOutput(ns("add_feature_button")),
    shiny::uiOutput(ns("idtax_selector")),
    shiny::hr(),
    shiny::uiOutput(ns("summary_cards")),
    shiny::uiOutput(ns("mapping_interface")),
    shiny::uiOutput(ns("validation"))
  )
}


#' Trait Metadata Mapping Module - Server
#'
#' @param id Module namespace ID
#' @param data Reactive: uploaded data frame
#' @param trait_mapping Reactive: result from mod_trait_column_mapping_server
#' @param pool Reactive: database connection pool
#' @param i18n Reactive: shiny.i18n translator
#'
#' @return Reactive list: valid, idtax_col, metadata_cols, feature_cols, available_traits
#' @keywords internal
#' @export
mod_trait_metadata_mapping_server <- function(id, data, trait_mapping, pool, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Refresh counter for add-new-feature ----
    refresh_counter <- shiny::reactiveVal(0)

    # ---- Available traits (fetched from DB directly so refresh works) ----
    available_traits <- shiny::reactive({
      refresh_counter()
      shiny::req(pool())
      tryCatch({
        actual_con <- if (inherits(pool(), "Pool")) pool::poolCheckout(pool()) else pool()
        on.exit(if (inherits(pool(), "Pool")) pool::poolReturn(actual_con), add = TRUE)
        DBI::dbGetQuery(actual_con,
          "SELECT id_trait, trait, valuetype, traitdescription, category,
                  expectedunit, minallowedvalue, maxallowedvalue, factorlevels
           FROM traitlist ORDER BY trait")
      }, error = function(e) {
        message("Could not fetch traitlist: ", e$message)
        # Fallback to trait_mapping module's copy
        tm <- trait_mapping()
        if (!is.null(tm)) tm$available_traits else
          data.frame(id_trait = integer(), trait = character(), valuetype = character(),
                     traitdescription = character(), category = character(),
                     expectedunit = character(), minallowedvalue = numeric(),
                     maxallowedvalue = numeric(), factorlevels = character(),
                     stringsAsFactors = FALSE)
      })
    })

    # ---- Flat metadata columns in taxa_traits_measures ----
    flat_meta_cols <- c(
      "basisofrecord", "decimallatitude", "decimallongitude", "elevation",
      "verbatimlocality", "reference", "year", "month", "day",
      "measurementremarks", "measurementmethod", "original_tax_name"
    )

    # ---- Columns already taken by trait mapping ----
    trait_mapped_cols <- shiny::reactive({
      tm <- trait_mapping()
      if (is.null(tm)) return(character(0))
      names(tm$trait_cols)
    })

    # ---- Columns available for metadata mapping (exclude trait-mapped ones) ----
    available_cols <- shiny::reactive({
      shiny::req(data())
      all_cols <- colnames(data())
      mapped <- trait_mapped_cols()
      setdiff(all_cols, mapped)
    })

    # ---- Dropdown choices (features grouped by category) ----
    meta_choices <- shiny::reactive({
      tr <- available_traits()
      shiny::req(tr)

      # Flat metadata choices
      flat_ch <- setNames(
        paste0("meta:", flat_meta_cols),
        flat_meta_cols
      )

      # Feature choices from traitlist, grouped by category
      cats <- if ("category" %in% names(tr) && any(!is.na(tr$category) & nchar(trimws(tr$category)) > 0)) {
        tr$category
      } else {
        rep("Other", nrow(tr))
      }
      cats[is.na(cats) | trimws(cats) == ""] <- "Other"

      unique_cats <- unique(cats)
      feat_grouped <- lapply(setNames(unique_cats, paste0("-- ", unique_cats, " --")), function(cat) {
        idx <- which(cats == cat)
        setNames(paste0("feature:", tr$trait[idx]),
                 paste0(tr$trait[idx], " [", tr$valuetype[idx], "]"))
      })

      c(
        list("--- Flat metadata ---" = flat_ch),
        feat_grouped,
        list("--- Other ---" = c("Skip" = "skip"))
      )
    })

    # ---- Auto-mapping ----
    auto_map <- shiny::reactive({
      shiny::req(data(), available_traits())
      cols <- available_cols()
      tr <- available_traits()
      result <- setNames(rep("skip", length(cols)), cols)

      for (col in cols) {
        cl <- tolower(trimws(col))

        # Check flat metadata (exact match)
        flat_match <- which(tolower(flat_meta_cols) == cl)
        if (length(flat_match) == 1) {
          result[col] <- paste0("meta:", flat_meta_cols[flat_match])
          next
        }

        # Check traitlist (exact match)
        exact_trait <- which(tolower(tr$trait) == cl)
        if (length(exact_trait) == 1) {
          result[col] <- paste0("feature:", tr$trait[exact_trait])
          next
        }

        # Fuzzy match against flat metadata
        if (requireNamespace("stringdist", quietly = TRUE)) {
          sims_flat <- stringdist::stringsim(cl, tolower(flat_meta_cols), method = "jw")
          best_flat <- which.max(sims_flat)
          if (length(best_flat) == 1 && sims_flat[best_flat] > 0.80) {
            result[col] <- paste0("meta:", flat_meta_cols[best_flat])
            next
          }

          # Fuzzy match against traitlist
          if (nrow(tr) > 0) {
            sims_trait <- stringdist::stringsim(cl, tolower(tr$trait), method = "jw")
            best_trait <- which.max(sims_trait)
            if (length(best_trait) == 1 && sims_trait[best_trait] > 0.72) {
              result[col] <- paste0("feature:", tr$trait[best_trait])
            }
          }
        }
      }
      result
    })

    # ---- Header ----
    output$header <- shiny::renderUI({
      shiny::tagList(
        shiny::h4(shiny::icon("columns", style = "color: #007bff;"),
                  i18n()$t("Map Metadata Columns")),
        shiny::p(
          i18n()$t("Map columns that document the measurements: flat metadata fields (basisofrecord, coordinates, reference...) stored in taxa_traits_measures, and supplementary trait features stored in taxa_traits_measures_feat."),
          style = "color: #6c757d; margin-bottom: 10px;"
        )
      )
    })

    # ---- Add new feature button + modal ----
    output$add_feature_button <- shiny::renderUI({
      shiny::actionButton(ns("btn_add_feature"),
        shiny::tagList(shiny::icon("plus"), i18n()$t("Add New Feature")),
        class = "btn-outline-primary btn-sm", style = "margin-bottom: 15px;")
    })

    shiny::observeEvent(input$btn_add_feature, {
      # Derive category choices from existing traitlist categories
      tr <- available_traits()
      existing_cats <- if ("category" %in% names(tr)) {
        sort(unique(tr$category[!is.na(tr$category) & nchar(trimws(tr$category)) > 0]))
      } else {
        character(0)
      }
      default_cats <- c("Leaf trait", "Wood trait", "Stem-level trait", "Stem status",
                        "Phenology", "Vitality", "Root trait", "Bark trait",
                        "Reproductive trait", "Morphological trait",
                        "Physiological trait", "Ecological trait", "Other trait", "Other")
      category_choices <- unique(c(existing_cats, default_cats))

      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("plus-circle"),
                               paste0(" ", i18n()$t("Create New Feature"))),
        size = "l",

        shiny::p(
          i18n()$t("Create a new feature entry in the traitlist. It will immediately become available in the mapping above."),
          style = "color: #6c757d; margin-bottom: 20px;"
        ),

        shiny::fluidRow(
          shiny::column(6,
            shiny::textInput(ns("new_feature_name"),
              i18n()$t("Feature Name *"),
              placeholder = i18n()$t("e.g., bark_thickness, leaf_area")),
            shiny::tags$small(
              shiny::icon("info-circle", style = "color: #007bff;"),
              paste0(" ", i18n()$t("Use lowercase, underscores (not spaces), no special characters")),
              style = "color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;"
            ),
            shiny::selectInput(ns("new_feature_valuetype"),
              i18n()$t("Value Type *"),
              choices = setNames(
                c("numeric", "integer", "categorical", "character", "logical", "ordinal"),
                c(i18n()$t("Numeric (measurements)"),
                  i18n()$t("Integer (counts)"),
                  i18n()$t("Categorical (categories)"),
                  i18n()$t("Character (text)"),
                  i18n()$t("Logical (yes/no)"),
                  i18n()$t("Ordinal (ordered categories)"))
              ),
              selected = "numeric"
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'numeric' || input['%s'] == 'integer'",
                                  ns("new_feature_valuetype"), ns("new_feature_valuetype")),
              shiny::textInput(ns("new_feature_unit"),
                i18n()$t("Expected Unit (optional)"),
                placeholder = i18n()$t("e.g., cm, m, kg, g/cm3, %"))
            )
          ),
          shiny::column(6,
            shiny::textAreaInput(ns("new_feature_description"),
              i18n()$t("Description *"),
              placeholder = i18n()$t("Describe what this feature measures or represents"),
              rows = 3),
            shiny::selectInput(ns("new_feature_category"),
              i18n()$t("Category"),
              choices = category_choices,
              selected = category_choices[1]),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'numeric' || input['%s'] == 'integer'",
                                  ns("new_feature_valuetype"), ns("new_feature_valuetype")),
              shiny::textInput(ns("new_feature_min"),
                i18n()$t("Minimum Allowed Value (optional)"),
                placeholder = i18n()$t("e.g., 0")),
              shiny::textInput(ns("new_feature_max"),
                i18n()$t("Maximum Allowed Value (optional)"),
                placeholder = i18n()$t("e.g., 100"))
            )
          )
        ),

        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'categorical' || input['%s'] == 'ordinal'",
                              ns("new_feature_valuetype"), ns("new_feature_valuetype")),
          shiny::textInput(ns("new_feature_factors"),
            i18n()$t("Factor Levels (comma-separated)"),
            placeholder = i18n()$t("e.g., small, medium, large"))
        ),

        footer = shiny::tagList(
          shiny::modalButton(i18n()$t("Cancel")),
          shiny::actionButton(ns("confirm_add_feature"),
            shiny::tagList(shiny::icon("check"), paste0(" ", i18n()$t("Create Feature"))),
            class = "btn-primary")
        ),
        easyClose = FALSE
      ))
    })

    shiny::observeEvent(input$confirm_add_feature, {
      shiny::req(input$new_feature_name, input$new_feature_valuetype, input$new_feature_description)

      feat_name <- tolower(trimws(input$new_feature_name))
      description <- trimws(input$new_feature_description)

      if (nchar(feat_name) == 0 || nchar(description) == 0) {
        shiny::showNotification(
          i18n()$t("Feature name and description are required."),
          type = "warning"
        )
        return()
      }

      tryCatch({
        new_min <- if (!is.null(input$new_feature_min) && nchar(trimws(input$new_feature_min)) > 0)
          as.numeric(input$new_feature_min) else NULL
        new_max <- if (!is.null(input$new_feature_max) && nchar(trimws(input$new_feature_max)) > 0)
          as.numeric(input$new_feature_max) else NULL
        new_unit <- if (!is.null(input$new_feature_unit) && nchar(trimws(input$new_feature_unit)) > 0)
          trimws(input$new_feature_unit) else NULL
        new_levels <- if (!is.null(input$new_feature_factors) && nchar(trimws(input$new_feature_factors)) > 0)
          trimws(input$new_feature_factors) else NULL

        add_trait(
          new_trait = feat_name,
          new_valuetype = input$new_feature_valuetype,
          new_traitdescription = description,
          new_expectedunit = new_unit,
          new_minallowedvalue = new_min,
          new_maxallowedvalue = new_max,
          new_factorlevels = new_levels,
          new_category = input$new_feature_category,
          con = pool(), interactive = FALSE)
        shiny::removeModal()
        shiny::showNotification(
          sprintf(i18n()$t("Feature '%s' created successfully"), feat_name),
          type = "message"
        )
        refresh_counter(refresh_counter() + 1)
      }, error = function(e) {
        shiny::showNotification(paste(i18n()$t("Error adding feature:"), e$message), type = "error")
      })
    })

    # ---- Taxon ID selector ----
    output$idtax_selector <- shiny::renderUI({
      shiny::req(data())
      all_cols <- colnames(data())

      # Auto-detect idtax column
      idtax_candidates <- c("idtax_n", "idtax", "id_tax", "taxon_id")
      auto_selected <- NULL
      for (cand in idtax_candidates) {
        match <- all_cols[tolower(all_cols) == tolower(cand)]
        if (length(match) == 1) { auto_selected <- match; break }
      }

      shiny::div(
        style = "padding: 15px; background: #e7f3ff; border-left: 4px solid #007bff; border-radius: 4px; margin-bottom: 15px;",
        shiny::fluidRow(
          shiny::column(6,
            shiny::tags$strong(
              shiny::icon("fingerprint", style = "color: #007bff;"),
              " ", i18n()$t("Taxon ID column (required)"),
              style = "font-size: 14px;"
            ),
            shiny::p(
              i18n()$t("Select which column contains the taxon ID (idtax_n)."),
              style = "color: #6c757d; margin: 4px 0;"
            )
          ),
          shiny::column(6,
            shiny::selectInput(
              ns("idtax_col"),
              label = NULL,
              choices = all_cols,
              selected = auto_selected,
              width = "100%"
            )
          )
        )
      )
    })

    # ---- Mapping interface ----
    output$mapping_interface <- shiny::renderUI({
      shiny::req(data(), meta_choices(), auto_map())
      cols    <- available_cols()
      choices <- meta_choices()
      auto    <- auto_map()

      if (length(cols) == 0) {
        return(shiny::div(
          style = "padding: 15px; background: #f8f9fa; border-radius: 4px;",
          shiny::icon("info-circle", style = "color: #6c757d;"),
          i18n()$t("All columns are already mapped as traits.")
        ))
      }

      rows <- lapply(cols, function(col) {
        safe <- make.names(col)
        # Preserve user selection if it exists, otherwise use auto-map
        current_val <- input[[paste0("meta_", safe)]]
        selected <- if (!is.null(current_val)) current_val else auto[col]
        border <- if (selected != "skip") "#007bff" else "#dee2e6"

        vals <- utils::head(stats::na.omit(data()[[col]]), 3)
        sample_str <- paste(vals, collapse = ", ")
        if (nchar(sample_str) > 60) sample_str <- paste0(substr(sample_str, 1, 57), "...")

        shiny::div(
          style = sprintf("padding: 10px 12px; margin: 5px 0; border-left: 4px solid %s;
                           background: #fafafa; border-radius: 4px;", border),
          shiny::fluidRow(
            shiny::column(4,
              shiny::tags$strong(col, style = "font-size: 13px;"), shiny::br(),
              shiny::tags$small(shiny::icon("eye"), " ",
                shiny::tags$code(sample_str, style = "font-size: 11px;"),
                style = "color: #999;")),
            shiny::column(1, shiny::div(shiny::icon("arrow-right", style = "color: #aaa;"),
                          style = "text-align:center; padding-top:12px;")),
            shiny::column(7,
              shiny::selectInput(ns(paste0("meta_", safe)), label = NULL,
                choices = choices, selected = selected, width = "100%"),
              shiny::uiOutput(ns(paste0("metadesc_", safe))))
          )
        )
      })
      do.call(shiny::tagList, rows)
    })

    # ---- Description outputs ----
    shiny::observe({
      shiny::req(data(), available_traits())
      tr <- available_traits()
      cols <- available_cols()
      lapply(cols, function(col) {
        safe <- make.names(col)
        output[[paste0("metadesc_", safe)]] <- shiny::renderUI({
          val <- input[[paste0("meta_", safe)]]
          .meta_desc_ui(val, tr, flat_meta_cols)
        })
      })
    })

    # ---- Current mappings ----
    current_mappings <- shiny::reactive({
      shiny::req(data())
      cols <- available_cols()
      if (length(cols) == 0) return(setNames(character(0), character(0)))
      setNames(
        sapply(cols, function(col) input[[paste0("meta_", make.names(col))]] %||% "skip"),
        cols)
    })

    # ---- Summary cards ----
    output$summary_cards <- shiny::renderUI({
      m <- current_mappings()
      n_meta <- sum(grepl("^meta:", m))
      n_feat <- sum(grepl("^feature:", m))
      n_skip <- sum(m == "skip")
      idtax_ok <- !is.null(input$idtax_col) && nchar(input$idtax_col) > 0

      shiny::fluidRow(
        shiny::column(3, shiny::div(
          class = "card text-center p-2",
          style = paste0("border-color:", if (idtax_ok) "#28a745" else "#dc3545", ";"),
          shiny::tags$strong(
            if (idtax_ok) shiny::icon("check") else shiny::icon("times"),
            style = paste0("color:", if (idtax_ok) "#28a745" else "#dc3545", ";")),
          shiny::br(), shiny::tags$small(i18n()$t("Taxon ID"))
        )),
        shiny::column(3, shiny::div(
          class = "card text-center p-2", style = "border-color: #007bff;",
          shiny::tags$strong(n_meta, style = "color: #007bff;"),
          shiny::br(), shiny::tags$small(i18n()$t("Flat metadata"))
        )),
        shiny::column(3, shiny::div(
          class = "card text-center p-2", style = "border-color: #6610f2;",
          shiny::tags$strong(n_feat, style = "color: #6610f2;"),
          shiny::br(), shiny::tags$small(i18n()$t("Features of measures"))
        )),
        shiny::column(3, shiny::div(
          class = "card text-center p-2", style = "border-color: #6c757d;",
          shiny::tags$strong(n_skip, style = "color: #6c757d;"),
          shiny::br(), shiny::tags$small(i18n()$t("Skipped"))
        ))
      )
    })

    # ---- Validation ----
    output$validation <- shiny::renderUI({
      m <- current_mappings()
      errors <- character()

      # Check idtax
      if (is.null(input$idtax_col) || nchar(input$idtax_col) == 0)
        errors <- c(errors, i18n()$t("You must select the Taxon ID column"))

      # Check idtax not also mapped as trait
      if (!is.null(input$idtax_col) && input$idtax_col %in% trait_mapped_cols())
        errors <- c(errors, i18n()$t("The Taxon ID column is also mapped in a section - it will be used only as taxon ID"))

      # Check no duplicate feature mappings
      f_vals <- m[grepl("^feature:", m)]
      if (length(f_vals) != length(unique(f_vals)))
        errors <- c(errors, i18n()$t("Each feature can only be mapped once"))

      # Check no duplicate flat metadata mappings
      meta_vals <- m[grepl("^meta:", m)]
      if (length(meta_vals) != length(unique(meta_vals)))
        errors <- c(errors, i18n()$t("Each metadata field can only be mapped once"))

      if (length(errors) == 0) return(NULL)
      shiny::div(
        style = "margin-top: 15px; padding: 10px; background: #f8d7da; border-radius: 4px;",
        shiny::icon("exclamation-triangle", style = "color: #dc3545;"),
        lapply(errors, function(e) shiny::p(e, style = "margin: 2px 0; color: #721c24;")))
    })

    # ---- Return ----
    shiny::reactive({
      m <- current_mappings()
      idtax_col <- input$idtax_col

      # Parse metadata columns (meta:xxx -> user_col = xxx)
      meta_vals <- m[grepl("^meta:", m)]
      metadata_cols <- setNames(sub("^meta:", "", meta_vals), names(meta_vals))

      # Parse feature columns (feature:xxx -> user_col = xxx)
      feat_vals <- m[grepl("^feature:", m)]
      feature_cols <- setNames(sub("^feature:", "", feat_vals), names(feat_vals))

      # Validation
      idtax_ok <- !is.null(idtax_col) && nchar(idtax_col) > 0
      no_dup_feat <- length(feat_vals) == length(unique(feat_vals))
      no_dup_meta <- length(meta_vals) == length(unique(meta_vals))
      valid <- idtax_ok && no_dup_feat && no_dup_meta

      list(
        valid = valid,
        idtax_col = idtax_col,
        metadata_cols = metadata_cols,
        feature_cols = feature_cols,
        available_traits = available_traits()
      )
    })
  })
}


# ---- Helper: metadata description UI ----
#' @keywords internal
.meta_desc_ui <- function(val, traits_df, flat_meta_cols) {
  if (is.null(val) || val == "skip") return(NULL)

  if (grepl("^meta:", val)) {
    meta_name <- sub("^meta:", "", val)
    desc <- .flat_meta_description(meta_name)
    if (is.null(desc)) return(NULL)
    return(shiny::div(
      style = "margin-top:6px; padding:6px 10px; background:#e7f3ff;
               border-radius:4px; border-left:3px solid #007bff;",
      shiny::tags$small(
        shiny::icon("info-circle", style = "color:#007bff;"), " ",
        desc, style = "color:#495057;"
      )
    ))
  }

  if (grepl("^feature:", val)) {
    trait_name <- sub("^feature:", "", val)
    .meta_trait_desc_ui(trait_name, traits_df)
  }
}


# ---- Helper: flat metadata field descriptions ----
#' @keywords internal
.flat_meta_description <- function(field) {
  descs <- list(
    basisofrecord = "Basis of record: LivingSpecimen, PreservedSpecimen, literatureData, etc.",
    decimallatitude = "Decimal latitude of the measurement location (WGS84)",
    decimallongitude = "Decimal longitude of the measurement location (WGS84)",
    elevation = "Elevation in meters above sea level",
    verbatimlocality = "Free text locality description",
    reference = "Bibliographic reference or data source",
    year = "Year of measurement",
    month = "Month of measurement (1-12)",
    day = "Day of measurement (1-31)",
    measurementremarks = "Free text remarks about the measurement",
    measurementmethod = "Description of the measurement method used",
    original_tax_name = "Original taxon name as provided in the source data"
  )
  descs[[field]]
}


# Reuse .trait_desc_ui from mod_trait_column_mapping for feature descriptions
# (accepts trait_name string + traits_df)
.meta_trait_desc_ui <- function(trait_name, traits_df) {
  info <- traits_df[traits_df$trait == trait_name, ]
  if (nrow(info) == 0) return(NULL)

  parts <- list()
  desc <- info$traitdescription[1]
  if (!is.na(desc) && nchar(trimws(desc)) > 0)
    parts <- c(parts, list(shiny::tags$small(
      shiny::icon("info-circle", style = "color:#6610f2;"), " ", desc, style = "color:#6c757d;")))
  vt <- info$valuetype[1]
  if (!is.na(vt))
    parts <- c(parts, list(shiny::br(), shiny::tags$small(
      shiny::icon("tag"), " ", shiny::tags$strong("Type: "), vt, style = "color:#495057;")))
  unit <- info$expectedunit[1]
  if (!is.na(unit) && nchar(trimws(unit)) > 0)
    parts <- c(parts, list(shiny::br(), shiny::tags$small(
      shiny::icon("ruler"), " ", shiny::tags$strong("Unit: "), unit, style = "color:#28a745;")))
  mn <- info$minallowedvalue[1]; mx <- info$maxallowedvalue[1]
  if (!is.na(mn) || !is.na(mx)) {
    rng <- paste0(if (!is.na(mn)) mn else "-\u221e", " \u2013 ",
                  if (!is.na(mx)) mx else "+\u221e")
    parts <- c(parts, list(shiny::br(), shiny::tags$small(
      shiny::icon("arrows-alt-h"), " ", shiny::tags$strong("Range: "),
      shiny::tags$code(rng, style = "font-size:10px;"), style = "color:#6610f2;")))
  }
  fl <- info$factorlevels[1]
  if (!is.na(fl) && nchar(trimws(fl)) > 0)
    parts <- c(parts, list(shiny::br(), shiny::tags$small(
      shiny::icon("list"), " ", shiny::tags$strong("Levels: "),
      shiny::tags$code(fl, style = "font-size:10px;"), style = "color:#856404;")))
  if (length(parts) == 0) return(NULL)
  shiny::div(
    style = "margin-top:6px; padding:6px 10px; background:#f5f0ff;
             border-radius:4px; border-left:3px solid #6610f2;", parts)
}
