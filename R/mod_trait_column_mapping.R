# =============================================================================
# Taxa Traits Import - Trait Column Mapping Module
#
# Selects which user columns contain primary trait observations.
# Each column is mapped to a traitlist entry or skipped.
# At least one trait column is required.
#
# Two layouts are accepted:
#   - wide: one column per trait, each column mapped to a trait;
#   - long: one row per measurement, a column naming the trait and one or two
#     value columns. Each distinct trait name is mapped to a trait, and the
#     table is then spread into one column per trait (see
#     .long_traits_to_wide()), so the later steps see the same shape as wide
#     data.
# =============================================================================

#' Trait Column Mapping Module - UI
#' @param id Module namespace ID
#' @keywords internal
#' @export
mod_trait_column_mapping_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shiny::uiOutput(ns("format_ui")),
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
#' @return Reactive list: valid, trait_cols (user_col → trait_name),
#'   available_traits, format ("wide" or "long") and data. `data` is NULL for
#'   wide input; for long input it holds the table spread into one column per
#'   trait, which the later steps must use instead of the uploaded table.
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
      c(list("---" = c("(Skip this column)" = "")), grouped)
    })

    # ---- Data format (wide / long) ----
    long_guess <- shiny::reactive({
      shiny::req(data())
      .guess_long_trait_columns(colnames(data()))
    })

    # Until the selector is rendered, fall back to long only when the file
    # has recognisable trait-name and value columns.
    data_format <- shiny::reactive({
      fmt <- input$data_format
      if (!is.null(fmt) && fmt %in% c("wide", "long")) return(fmt)
      g <- long_guess()
      if (nzchar(g$name) && (nzchar(g$value_num) || nzchar(g$value_char))) "long" else "wide"
    })

    # Long-format key columns: the user's choice if it still belongs to the
    # table, otherwise the guess (a new upload with other column names).
    long_cols <- shiny::reactive({
      shiny::req(data())
      cols <- colnames(data())
      g <- long_guess()
      pick <- function(id, guess) {
        v <- input[[id]]
        if (!is.null(v) && v %in% c("", cols)) v else guess
      }
      list(
        name       = pick("long_name_col", g$name),
        value_num  = pick("long_value_num_col", g$value_num),
        value_char = pick("long_value_char_col", g$value_char)
      )
    })

    # Reads the inputs it renders so the rendered HTML always carries the
    # current choice: the wizard rebuilds this step from that HTML when the
    # user comes back to it.
    output$format_ui <- shiny::renderUI({
      shiny::req(data())
      fmt  <- data_format()
      cols <- colnames(data())
      lc   <- long_cols()
      none <- c("-- not mapped --" = "")

      shiny::div(
        style = "margin-bottom: 15px;",
        shiny::radioButtons(
          ns("data_format"),
          i18n()$t("Data Format"),
          choices = setNames(
            c("wide", "long"),
            c(i18n()$t("Wide format (one column per trait)"),
              i18n()$t("Long format (trait name + value columns)"))
          ),
          selected = fmt,
          inline = TRUE
        ),
        if (fmt == "long") {
          shiny::tagList(
            shiny::p(
              i18n()$t("Long format: one row per measurement, with a column holding the trait name and a column holding the value. Give a numeric and a character value column if numeric and categorical traits are stored separately. The other columns (idtax, reference, coordinates...) are kept and mapped in the next step."),
              style = "color: #6c757d;"
            ),
            shiny::fluidRow(
              shiny::column(4, shiny::selectInput(
                ns("long_name_col"), i18n()$t("Trait name column *"),
                choices = c(none, cols), selected = lc$name, width = "100%")),
              shiny::column(4, shiny::selectInput(
                ns("long_value_num_col"), i18n()$t("Numeric value column"),
                choices = c(none, cols), selected = lc$value_num, width = "100%")),
              shiny::column(4, shiny::selectInput(
                ns("long_value_char_col"), i18n()$t("Character value column"),
                choices = c(none, cols), selected = lc$value_char, width = "100%"))
            )
          )
        } else {
          shiny::p(
            i18n()$t("Wide format: one column per trait. Map each trait column to a trait below."),
            style = "color: #6c757d;"
          )
        }
      )
    })

    # Distinct trait names found in the long-format name column
    long_names <- shiny::reactive({
      shiny::req(data())
      nc <- long_cols()$name
      if (data_format() != "long" || !nzchar(nc)) return(character(0))
      v <- trimws(as.character(data()[[nc]]))
      sort(unique(v[!is.na(v) & nzchar(v)]))
    })

    # ---- Auto-mapping ----
    auto_map <- shiny::reactive({
      shiny::req(data())
      .auto_map_trait_names(colnames(data()), available_traits())
    })

    # ---- Rows of the mapping interface ----
    # One row per column (wide) or per distinct trait name (long).
    mapping_rows <- shiny::reactive({
      shiny::req(data())
      df <- data()

      if (data_format() == "long") {
        nms <- long_names()
        if (length(nms) == 0) return(NULL)
        lc <- long_cols()
        row_names <- trimws(as.character(df[[lc$name]]))
        values <- .coalesce_values(
          if (nzchar(lc$value_num)) df[[lc$value_num]],
          if (nzchar(lc$value_char)) df[[lc$value_char]]
        )
        samples <- vapply(nms, function(nm) {
          in_group <- which(row_names == nm)
          vals <- if (is.null(values)) character(0) else
            utils::head(stats::na.omit(values[in_group]), 3)
          paste0(sprintf("%d rows", length(in_group)),
                 if (length(vals) > 0) paste0(": ", paste(vals, collapse = ", ")))
        }, character(1))
        auto <- .auto_map_trait_names(nms, available_traits())
        return(data.frame(
          key = nms, input_id = paste0("lmap_", make.unique(make.names(nms))),
          sample = unname(samples), auto = unname(auto), stringsAsFactors = FALSE
        ))
      }

      cols <- colnames(df)
      samples <- vapply(cols, function(col) {
        paste(utils::head(stats::na.omit(df[[col]]), 3), collapse = ", ")
      }, character(1))
      data.frame(
        key = cols, input_id = paste0("map_", make.unique(make.names(cols))),
        sample = unname(samples), auto = unname(auto_map()[cols]),
        stringsAsFactors = FALSE
      )
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
      shiny::req(data(), trait_choices())
      mr      <- mapping_rows()
      choices <- trait_choices()

      if (is.null(mr)) {
        return(shiny::div(
          class = "alert alert-secondary",
          i18n()$t("Select the column holding the trait names to list them here.")
        ))
      }

      rows <- lapply(seq_len(nrow(mr)), function(i) {
        col      <- mr$key[i]
        input_id <- mr$input_id[i]
        # Preserve user selection if it exists, otherwise use auto-map
        current_val <- input[[input_id]]
        selected <- if (!is.null(current_val)) current_val else mr$auto[i]
        border <- if (selected != "") "#28a745" else "#dee2e6"

        sample_str <- mr$sample[i]
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
              shiny::selectizeInput(ns(input_id), label = NULL,
                choices = choices, selected = selected, width = "100%",
                options = list(
                  placeholder = i18n()$t("(Skip this column)"),
                  allowEmptyOption = TRUE
                )),
              shiny::uiOutput(ns(paste0("desc_", input_id))))
          )
        )
      })
      do.call(shiny::tagList, rows)
    })

    # ---- Description outputs ----
    shiny::observe({
      mr <- mapping_rows()
      shiny::req(mr, available_traits())
      tr <- available_traits()
      lapply(mr$input_id, function(input_id) {
        output[[paste0("desc_", input_id)]] <- shiny::renderUI({
          .trait_desc_ui(input[[input_id]], tr)
        })
      })
    })

    # ---- Current mappings ----
    # Named by column (wide) or by trait name (long)
    current_mappings <- shiny::reactive({
      mr <- mapping_rows()
      if (is.null(mr)) return(setNames(character(0), character(0)))
      setNames(
        vapply(mr$input_id, function(id) input[[id]] %||% "", character(1)),
        mr$key)
    })

    # ---- Mapping state: errors and, when valid, the result ----
    mapping_state <- shiny::reactive({
      shiny::req(data())
      m <- current_mappings()
      t_vals <- m[grepl("^trait:", m)]
      errors <- character()

      if (data_format() == "long") {
        lc <- long_cols()
        if (!nzchar(lc$name)) {
          errors <- c(errors, i18n()$t("Select the column holding the trait names"))
        }
        if (!nzchar(lc$value_num) && !nzchar(lc$value_char)) {
          errors <- c(errors, i18n()$t("Select at least one value column"))
        }
        if (nzchar(lc$name) && length(t_vals) == 0) {
          errors <- c(errors, i18n()$t("You must map at least one trait name to a trait"))
        }

        # Each mapped trait becomes a column; it must not overwrite one the
        # user keeps for metadata.
        traits <- unique(sub("^trait:", "", t_vals))
        kept_cols <- setdiff(colnames(data()),
                             c(lc$name, lc$value_num, lc$value_char))
        clash <- intersect(traits, kept_cols)
        if (length(clash) > 0) {
          errors <- c(errors, sprintf(
            i18n()$t("Columns with the same name as a mapped trait: %s. Rename them in your file."),
            paste(clash, collapse = ", ")))
        }

        if (length(errors) > 0) return(list(errors = errors, result = NULL))

        name_map <- sub("^trait:", "", m)
        tr <- available_traits()
        wide <- .long_traits_to_wide(
          data(),
          name_col = lc$name,
          value_num_col = lc$value_num,
          value_char_col = lc$value_char,
          name_map = name_map,
          valuetypes = setNames(tr$valuetype, tr$trait)
        )
        return(list(errors = character(), result = list(
          valid = TRUE,
          trait_cols = setNames(traits, traits),
          available_traits = tr,
          format = "long",
          data = wide
        )))
      }

      if (length(t_vals) == 0)
        errors <- c(errors, i18n()$t("You must map at least one column to a trait measure"))
      if (length(t_vals) != length(unique(t_vals)))
        errors <- c(errors, i18n()$t("Each trait can only be mapped once as a trait measure"))

      list(errors = errors, result = list(
        valid = length(errors) == 0,
        trait_cols = setNames(sub("^trait:", "", t_vals), names(t_vals)),
        available_traits = available_traits(),
        format = "wide",
        data = NULL
      ))
    })

    # ---- Summary cards ----
    output$summary_cards <- shiny::renderUI({
      m <- current_mappings()
      n_traits <- sum(grepl("^trait:", m))
      n_skip   <- sum(m == "")
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
      errors <- mapping_state()$errors
      if (length(errors) == 0) return(NULL)
      shiny::div(
        style = "margin-top: 15px; padding: 10px; background: #f8d7da; border-radius: 4px;",
        shiny::icon("exclamation-triangle", style = "color: #dc3545;"),
        lapply(errors, function(e) shiny::p(e, style = "margin: 2px 0; color: #721c24;")))
    })

    # ---- Return ----
    shiny::reactive({
      st <- mapping_state()
      if (!is.null(st$result)) return(st$result)
      list(valid = FALSE, trait_cols = setNames(character(0), character(0)),
           available_traits = available_traits(),
           format = data_format(), data = NULL)
    })
  })
}


# ---- Helper: auto-map names to traits ----
#' Guess the trait matching each name
#'
#' Exact (case-insensitive) match first, then the closest trait by
#' Jaro-Winkler similarity above 0.72.
#'
#' @param user_names Column names (wide) or trait names (long).
#' @param traits Data frame with a `trait` column.
#' @return Named character vector, `"trait:<name>"` or `""`, named by
#'   `user_names`.
#' @keywords internal
.auto_map_trait_names <- function(user_names, traits) {
  result <- setNames(rep("", length(user_names)), user_names)
  if (is.null(traits) || nrow(traits) == 0) return(result)

  trait_lower <- tolower(traits$trait)
  has_stringdist <- requireNamespace("stringdist", quietly = TRUE)

  for (i in seq_along(user_names)) {
    cl <- tolower(trimws(user_names[i]))
    exact <- which(trait_lower == cl)
    if (length(exact) == 1) {
      result[i] <- paste0("trait:", traits$trait[exact])
      next
    }
    if (has_stringdist) {
      sims <- stringdist::stringsim(cl, trait_lower, method = "jw")
      best <- which.max(sims)
      if (length(best) == 1 && sims[best] > 0.72)
        result[i] <- paste0("trait:", traits$trait[best])
    }
  }
  result
}


# ---- Helper: guess long-format key columns ----
#' Guess the trait-name and value columns of a long-format table
#'
#' @param cols Column names of the uploaded table.
#' @return List with `name`, `value_num` and `value_char`, each a column name
#'   or `""`.
#' @keywords internal
.guess_long_trait_columns <- function(cols) {
  lower <- tolower(trimws(cols))
  first_match <- function(patterns) {
    for (pat in patterns) {
      idx <- which(lower == pat)
      if (length(idx) > 0) return(cols[idx[1]])
    }
    ""
  }
  list(
    name = first_match(c("trait", "trait_name", "traitname", "trait_type",
                         "variable", "measure", "measurement", "caractere",
                         "trait_measured", "mesure")),
    value_num = first_match(c("value", "traitvalue", "trait_value",
                              "value_num", "numeric_value", "valeur",
                              "mesure_num")),
    value_char = first_match(c("value_char", "traitvalue_char", "char_value",
                               "text_value", "valeur_char", "categorie"))
  )
}


# ---- Helper: fill the gaps of one vector with another ----
#' Take `a`, and `b` where `a` is missing
#'
#' Blank strings count as missing. When the two vectors have different
#' classes and both contribute, the result is character.
#'
#' @param a,b Vectors of equal length, or NULL.
#' @return A vector, or NULL when both are NULL.
#' @keywords internal
.coalesce_values <- function(a, b) {
  blank_to_na <- function(x) {
    if (is.character(x)) x[!is.na(x) & !nzchar(trimws(x))] <- NA
    x
  }
  a <- blank_to_na(a)
  b <- blank_to_na(b)
  if (is.null(b)) return(a)
  if (is.null(a)) return(b)

  miss <- is.na(a) & !is.na(b)
  if (!any(miss)) return(a)
  if (all(is.na(a))) return(b)
  if (!identical(class(a), class(b))) {
    a <- as.character(a)
    b <- as.character(b)
  }
  a[miss] <- b[miss]
  a
}


# ---- Helper: long format to one column per trait ----
#' Spread long-format trait data into one column per trait
#'
#' Each row keeps its other columns (idtax, metadata) and gets its value in
#' the column of its trait, with NA in the other trait columns. This is the
#' shape `add_sp_traits_measures()` expects, and it drops the NA cells trait
#' by trait. Rows whose trait name is missing or not mapped are dropped.
#'
#' Numeric and integer traits read the numeric value column first and fall
#' back on the character one; other traits do the reverse.
#'
#' @param df Uploaded data frame.
#' @param name_col Column holding the trait names.
#' @param value_num_col,value_char_col Value columns; `""` or NULL when
#'   absent. At least one is required.
#' @param name_map Named character vector: trait name in the file -> trait in
#'   `traitlist`, `""` for skipped names.
#' @param valuetypes Named character vector: trait -> valuetype.
#' @return A data.frame without the name and value columns, plus one column
#'   per mapped trait.
#' @keywords internal
.long_traits_to_wide <- function(df, name_col, value_num_col = NULL,
                                 value_char_col = NULL, name_map,
                                 valuetypes = NULL) {
  given <- function(col) !is.null(col) && length(col) == 1 && nzchar(col)
  if (!given(value_num_col) && !given(value_char_col))
    stop("Provide at least one value column")

  num <- if (given(value_num_col)) df[[value_num_col]]
  chr <- if (given(value_char_col)) df[[value_char_col]]

  row_names <- trimws(as.character(df[[name_col]]))
  row_trait <- unname(name_map[row_names])
  keep <- !is.na(row_trait) & nzchar(row_trait)

  drop_cols <- c(name_col,
                 if (given(value_num_col)) value_num_col,
                 if (given(value_char_col)) value_char_col)
  out <- df[keep, setdiff(names(df), drop_cols), drop = FALSE]
  src_rows <- which(keep)
  row_trait <- row_trait[keep]

  for (trait in unique(row_trait)) {
    vt <- if (!is.null(valuetypes)) unname(valuetypes[trait]) else NA_character_
    is_num <- !is.na(vt) && vt %in% c("numeric", "integer")
    in_trait <- row_trait == trait
    src <- src_rows[in_trait]
    # Coalesce over this trait's rows only, so a text value of another trait
    # cannot turn a numeric column into character.
    vals <- if (is_num) .coalesce_values(num[src], chr[src])
            else .coalesce_values(chr[src], num[src])
    col <- vals[rep(NA_integer_, length(in_trait))]  # NA of the same type
    col[in_trait] <- vals
    out[[trait]] <- col
  }

  rownames(out) <- NULL
  out
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
