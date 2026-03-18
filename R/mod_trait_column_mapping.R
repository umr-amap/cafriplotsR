# =============================================================================
# Taxa Traits Import - Trait Column Mapping Module
#
# Selects which user columns contain primary trait observations.
# Each column is mapped to a traitlist entry or skipped.
# At least one trait column is required.
# =============================================================================

#' Trait Column Mapping Module - UI
#' @param id Module namespace ID
#' @keywords internal
#' @export
mod_trait_column_mapping_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shiny::uiOutput(ns("add_trait_button")),
    shiny::uiOutput(ns("summary_cards")),
    shiny::hr(),
    shiny::uiOutput(ns("mapping_interface")),
    shiny::uiOutput(ns("validation"))
  )
}


#' Trait Column Mapping Module - Server
#'
#' @param id Module namespace ID
#' @param data Reactive: uploaded data frame
#' @param pool Reactive: database connection pool
#' @param i18n Reactive: shiny.i18n translator
#'
#' @return Reactive list: valid, trait_cols (user_col → trait_name), available_traits
#' @keywords internal
#' @export
mod_trait_column_mapping_server <- function(id, data, pool, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Refresh counter for add-new-trait ----
    refresh_counter <- shiny::reactiveVal(0)

    # ---- Available traits ----
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
        data.frame(id_trait = integer(), trait = character(), valuetype = character(),
                   traitdescription = character(), expectedunit = character(),
                   minallowedvalue = numeric(), maxallowedvalue = numeric(),
                   factorlevels = character(), stringsAsFactors = FALSE)
      })
    })

    # ---- Dropdown choices (grouped by category) ----
    trait_choices <- shiny::reactive({
      tr <- available_traits()
      cats <- if ("category" %in% names(tr) && any(!is.na(tr$category) & nchar(trimws(tr$category)) > 0)) {
        tr$category
      } else {
        rep("Other", nrow(tr))
      }
      cats[is.na(cats) | trimws(cats) == ""] <- "Other"

      # Build one named list entry per category
      unique_cats <- unique(cats)
      grouped <- lapply(setNames(unique_cats, unique_cats), function(cat) {
        idx <- which(cats == cat)
        setNames(paste0("trait:", tr$trait[idx]),
                 paste0(tr$trait[idx], " [", tr$valuetype[idx], "]"))
      })
      c(grouped, list("--- Other ---" = c("Skip" = "skip")))
    })

    # ---- Auto-mapping ----
    auto_map <- shiny::reactive({
      shiny::req(data())
      tr <- available_traits()
      user_cols <- colnames(data())
      result <- setNames(rep("skip", length(user_cols)), user_cols)

      for (col in user_cols) {
        cl <- tolower(trimws(col))
        exact <- which(tolower(tr$trait) == cl)
        if (length(exact) == 1) { result[col] <- paste0("trait:", tr$trait[exact]); next }
        if (nrow(tr) > 0 && requireNamespace("stringdist", quietly = TRUE)) {
          sims <- stringdist::stringsim(cl, tolower(tr$trait), method = "jw")
          best <- which.max(sims)
          if (length(best) == 1 && sims[best] > 0.72)
            result[col] <- paste0("trait:", tr$trait[best])
        }
      }
      result
    })

    # ---- Header ----
    output$header <- shiny::renderUI({
      shiny::tagList(
        shiny::h4(shiny::icon("star", style = "color: #28a745;"),
                  i18n()$t("Map Trait Columns")),
        shiny::p(
          i18n()$t("Select which columns contain primary trait observations (e.g. wood density, max height). At least one trait column is required."),
          style = "color: #6c757d; margin-bottom: 10px;"
        )
      )
    })

    # ---- Add new trait button + modal ----
    output$add_trait_button <- shiny::renderUI({
      shiny::actionButton(ns("btn_add_trait"),
        shiny::tagList(shiny::icon("plus"), i18n()$t("Add New Trait")),
        class = "btn-outline-primary btn-sm", style = "margin-bottom: 15px;")
    })

    shiny::observeEvent(input$btn_add_trait, {
      # Derive category choices from existing traitlist categories
      tr <- available_traits()
      existing_cats <- if ("category" %in% names(tr)) {
        sort(unique(tr$category[!is.na(tr$category) & nchar(trimws(tr$category)) > 0]))
      } else {
        character(0)
      }
      # Merge with default taxa-trait categories; preserve existing + add defaults
      default_cats <- c("Leaf trait", "Wood trait", "Stem-level trait", "Stem status",
                        "Phenology", "Vitality", "Root trait", "Bark trait",
                        "Reproductive trait", "Morphological trait",
                        "Physiological trait", "Ecological trait", "Other trait", "Other")
      category_choices <- unique(c(existing_cats, default_cats))

      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("plus-circle"),
                               paste0(" ", i18n()$t("Create New Trait"))),
        size = "l",

        shiny::p(
          i18n()$t("Create a new trait entry in the traitlist. It will immediately become available in the mapping above."),
          style = "color: #6c757d; margin-bottom: 20px;"
        ),

        shiny::fluidRow(
          shiny::column(6,
            shiny::textInput(ns("new_trait_name"),
              i18n()$t("Trait Name *"),
              placeholder = i18n()$t("e.g., bark_thickness, leaf_area")),
            shiny::tags$small(
              shiny::icon("info-circle", style = "color: #007bff;"),
              paste0(" ", i18n()$t("Use lowercase, underscores (not spaces), no special characters")),
              style = "color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;"
            ),
            shiny::selectInput(ns("new_trait_valuetype"),
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
                                  ns("new_trait_valuetype"), ns("new_trait_valuetype")),
              shiny::textInput(ns("new_trait_unit"),
                i18n()$t("Expected Unit (optional)"),
                placeholder = i18n()$t("e.g., cm, m, kg, g/cm3, %"))
            )
          ),
          shiny::column(6,
            shiny::textAreaInput(ns("new_trait_description"),
              i18n()$t("Description *"),
              placeholder = i18n()$t("Describe what this trait measures or represents"),
              rows = 3),
            shiny::selectInput(ns("new_trait_category"),
              i18n()$t("Category"),
              choices = category_choices,
              selected = category_choices[1]),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'numeric' || input['%s'] == 'integer'",
                                  ns("new_trait_valuetype"), ns("new_trait_valuetype")),
              shiny::textInput(ns("new_trait_min"),
                i18n()$t("Minimum Allowed Value (optional)"),
                placeholder = i18n()$t("e.g., 0")),
              shiny::textInput(ns("new_trait_max"),
                i18n()$t("Maximum Allowed Value (optional)"),
                placeholder = i18n()$t("e.g., 100"))
            )
          )
        ),

        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'categorical' || input['%s'] == 'ordinal'",
                              ns("new_trait_valuetype"), ns("new_trait_valuetype")),
          shiny::textInput(ns("new_trait_factors"),
            i18n()$t("Factor Levels (comma-separated)"),
            placeholder = i18n()$t("e.g., small, medium, large"))
        ),

        footer = shiny::tagList(
          shiny::modalButton(i18n()$t("Cancel")),
          shiny::actionButton(ns("confirm_add_trait"),
            shiny::tagList(shiny::icon("check"), paste0(" ", i18n()$t("Create Trait"))),
            class = "btn-primary")
        ),
        easyClose = FALSE
      ))
    })

    shiny::observeEvent(input$confirm_add_trait, {
      shiny::req(input$new_trait_name, input$new_trait_valuetype, input$new_trait_description)

      trait_name <- tolower(trimws(input$new_trait_name))
      description <- trimws(input$new_trait_description)

      if (nchar(trait_name) == 0 || nchar(description) == 0) {
        shiny::showNotification(
          i18n()$t("Trait name and description are required."),
          type = "warning"
        )
        return()
      }

      tryCatch({
        new_min <- if (!is.null(input$new_trait_min) && nchar(trimws(input$new_trait_min)) > 0)
          as.numeric(input$new_trait_min) else NULL
        new_max <- if (!is.null(input$new_trait_max) && nchar(trimws(input$new_trait_max)) > 0)
          as.numeric(input$new_trait_max) else NULL
        new_unit <- if (!is.null(input$new_trait_unit) && nchar(trimws(input$new_trait_unit)) > 0)
          trimws(input$new_trait_unit) else NULL
        new_levels <- if (!is.null(input$new_trait_factors) && nchar(trimws(input$new_trait_factors)) > 0)
          trimws(input$new_trait_factors) else NULL

        add_trait(
          new_trait = trait_name,
          new_valuetype = input$new_trait_valuetype,
          new_traitdescription = description,
          new_expectedunit = new_unit,
          new_minallowedvalue = new_min,
          new_maxallowedvalue = new_max,
          new_factorlevels = new_levels,
          new_category = input$new_trait_category,
          con = pool(), interactive = FALSE)
        shiny::removeModal()
        shiny::showNotification(
          sprintf(i18n()$t("Trait '%s' created successfully"), trait_name),
          type = "message"
        )
        refresh_counter(refresh_counter() + 1)
      }, error = function(e) {
        shiny::showNotification(paste(i18n()$t("Error adding trait:"), e$message), type = "error")
      })
    })

    # ---- Mapping interface ----
    output$mapping_interface <- shiny::renderUI({
      shiny::req(data(), trait_choices(), auto_map())
      user_cols <- colnames(data())
      choices   <- trait_choices()
      auto      <- auto_map()

      rows <- lapply(user_cols, function(col) {
        safe <- make.names(col)
        # Preserve user selection if it exists, otherwise use auto-map
        current_val <- input[[paste0("map_", safe)]]
        selected <- if (!is.null(current_val)) current_val else auto[col]
        border <- if (selected != "skip") "#28a745" else "#dee2e6"

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
              shiny::selectizeInput(ns(paste0("map_", safe)), label = NULL,
                choices = choices, selected = selected, width = "100%",
                options = list(
                  onBlur = I("function() { if (!this.getValue()) { this.setValue('skip'); } }")
                )),
              shiny::uiOutput(ns(paste0("desc_", safe))))
          )
        )
      })
      do.call(shiny::tagList, rows)
    })

    # ---- Description outputs ----
    shiny::observe({
      shiny::req(data(), available_traits())
      tr <- available_traits()
      lapply(colnames(data()), function(col) {
        safe <- make.names(col)
        output[[paste0("desc_", safe)]] <- shiny::renderUI({
          val <- input[[paste0("map_", safe)]]
          .trait_desc_ui(val, tr)
        })
      })
    })

    # ---- Current mappings ----
    current_mappings <- shiny::reactive({
      shiny::req(data())
      cols <- colnames(data())
      setNames(
        sapply(cols, function(col) input[[paste0("map_", make.names(col))]] %||% "skip"),
        cols)
    })

    # ---- Summary cards ----
    output$summary_cards <- shiny::renderUI({
      m <- current_mappings()
      n_traits <- sum(grepl("^trait:", m))
      n_skip   <- sum(m == "skip")
      shiny::fluidRow(
        shiny::column(6, shiny::div(
          class = "card text-center p-2",
          style = paste0("border-color:", if (n_traits > 0) "#28a745" else "#ffc107", ";"),
          shiny::tags$strong(n_traits, style = paste0("color:", if (n_traits > 0) "#28a745" else "#ffc107", ";")),
          shiny::br(), shiny::tags$small(i18n()$t("Trait measures"))
        )),
        shiny::column(6, shiny::div(
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
      t_vals <- m[grepl("^trait:", m)]
      if (length(t_vals) == 0)
        errors <- c(errors, i18n()$t("You must map at least one column to a trait measure"))
      if (length(t_vals) != length(unique(t_vals)))
        errors <- c(errors, i18n()$t("Each trait can only be mapped once as a trait measure"))

      if (length(errors) == 0) return(NULL)
      shiny::div(
        style = "margin-top: 15px; padding: 10px; background: #f8d7da; border-radius: 4px;",
        shiny::icon("exclamation-triangle", style = "color: #dc3545;"),
        lapply(errors, function(e) shiny::p(e, style = "margin: 2px 0; color: #721c24;")))
    })

    # ---- Return ----
    shiny::reactive({
      m <- current_mappings()
      t_vals <- m[grepl("^trait:", m)]
      valid <- (length(t_vals) > 0) && (length(t_vals) == length(unique(t_vals)))
      list(
        valid = valid,
        trait_cols = setNames(sub("^trait:", "", t_vals), names(t_vals)),
        available_traits = available_traits()
      )
    })
  })
}

# ---- Helper: trait description UI ----
#' @keywords internal
.trait_desc_ui <- function(val, traits_df) {
  if (is.null(val) || !grepl("^trait:", val)) return(NULL)
  trait_name <- sub("^trait:", "", val)
  info <- traits_df[traits_df$trait == trait_name, ]
  if (nrow(info) == 0) return(NULL)

  parts <- list()
  desc <- info$traitdescription[1]
  if (!is.na(desc) && nchar(trimws(desc)) > 0)
    parts <- c(parts, list(shiny::tags$small(
      shiny::icon("info-circle", style = "color:#007bff;"), " ", desc, style = "color:#6c757d;")))
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
    style = "margin-top:6px; padding:6px 10px; background:#f0fff4;
             border-radius:4px; border-left:3px solid #28a745;", parts)
}
