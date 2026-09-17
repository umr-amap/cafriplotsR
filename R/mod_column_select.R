# Column Selection Module
#
# Allows user to select which column contains taxonomic names
# Supports both single column and multi-column (genus/species/family) modes

#' Column Select Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_column_select_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4(shiny::textOutput(ns("title"))),
    shiny::uiOutput(ns("column_controls"))
  )
}


#' Column Select Module - Server
#'
#' @param id Character, module ID
#' @param data Reactive data.frame from data input module
#' @param initial_column Character, optional pre-selected column name
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return Reactive list with $column (selected column name), $include_authors
#'   (logical), $data (potentially modified data), $mode ("single" or
#'   "multiple") and, in multiple mode, $genus_column / $species_column /
#'   $family_column
#'
#' @keywords internal
mod_column_select_server <- function(id, data, initial_column = NULL, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Module title
    output$title <- shiny::renderText({
      i18n()$t("Column Selection")
    })

    # Column selection controls
    output$column_controls <- shiny::renderUI({
      req(data())

      ns <- session$ns
      df <- data()

      # Get character columns
      char_cols <- names(df)[sapply(df, is.character)]

      if (length(char_cols) == 0) {
        return(
          shiny::div(
            style = "color: red;",
            shiny::p(i18n()$t("Error:"), i18n()$t("No character columns found in data"))
          )
        )
      }

      # Determine selected column
      selected_col <- if (!is.null(initial_column) && initial_column %in% char_cols) {
        initial_column
      } else {
        char_cols[1]
      }

      # Build choices vector for column mode
      mode_choices <- c("single", "multiple")
      names(mode_choices) <- c(
        i18n()$t("Single column (all taxonomic info in one column)"),
        i18n()$t("Multiple columns (genus, species, family separated)")
      )

      # Build choices with translated "(none)" option
      none_choice <- c("")
      names(none_choice) <- i18n()$t("(none)")

      shiny::tagList(
        shiny::radioButtons(
          inputId = ns("column_mode"),
          label = shiny::strong(i18n()$t("Column structure:")),
          choices = mode_choices,
          selected = "single"
        ),

        # Single column mode
        shiny::conditionalPanel(
          condition = "input.column_mode == 'single'",
          ns = ns,
          shiny::selectInput(
            inputId = ns("column_name"),
            label = i18n()$t("Select name column:"),
            choices = char_cols,
            selected = selected_col
          )
        ),

        # Multiple columns mode
        shiny::conditionalPanel(
          condition = "input.column_mode == 'multiple'",
          ns = ns,
          shiny::div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
            shiny::p(
              shiny::icon("info-circle"),
              shiny::strong(i18n()$t("Select columns for each taxonomic component:")),
              style = "margin-top: 0;"
            ),
            shiny::helpText(i18n()$t("The app will create a combined column using available information (genus + species epithet, or genus only, or family only).")),

            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = ns("genus_column"),
                  label = i18n()$t("Genus column:"),
                  choices = c(none_choice, char_cols),
                  selected = ""
                ),
                shiny::helpText(i18n()$t("Genus name only, e.g. Garcinia"))
              ),
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = ns("species_column"),
                  label = i18n()$t("Species epithet column:"),
                  choices = c(none_choice, char_cols),
                  selected = ""
                ),
                shiny::helpText(i18n()$t("Second word of the name only, e.g. kola - not Garcinia kola. If a column holds the full name (genus + epithet), use Single column mode instead."))
              ),
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = ns("family_column"),
                  label = i18n()$t("Family column:"),
                  choices = c(none_choice, char_cols),
                  selected = ""
                ),
                shiny::helpText(i18n()$t("Family name, e.g. Clusiaceae. Used only when genus is empty."))
              )
            ),
            shiny::uiOutput(ns("epithet_warning"))
          )
        ),

        shiny::checkboxInput(
          inputId = ns("include_authors"),
          label = i18n()$t("Match with author names"),
          value = FALSE
        ),
        shiny::helpText(i18n()$t("Include author names in matching (slower but more precise)"))
      )
    })

    # Reactive to create combined column if in multiple mode
    processed_data <- shiny::reactive({
      req(data())
      req(input$column_mode)

      df <- data()

      if (input$column_mode == "multiple") {
        # `req()` treats "" as missing, so requiring the three inputs
        # outright would block as soon as one of them is left at "(none)"
        # - which is the ordinary case (genus + species, no family). Only
        # require that the inputs exist; emptiness is handled just below.
        req(!is.null(input$genus_column),
            !is.null(input$species_column),
            !is.null(input$family_column))

        # Check that at least one column is selected
        if (input$genus_column == "" && input$species_column == "" && input$family_column == "") {
          return(NULL)
        }

        # Create combined taxonomic column
        df$taxonomic_name_combined <- apply(df, 1, function(row) {
          genus <- if (input$genus_column != "") as.character(row[input$genus_column]) else ""
          species <- if (input$species_column != "") as.character(row[input$species_column]) else ""
          family <- if (input$family_column != "") as.character(row[input$family_column]) else ""

          # Replace NA with empty string
          genus <- ifelse(is.na(genus), "", genus)
          species <- ifelse(is.na(species), "", species)
          family <- ifelse(is.na(family), "", family)

          # Trim whitespace
          genus <- trimws(genus)
          species <- trimws(species)
          family <- trimws(family)

          # Build taxonomic name according to hierarchy
          if (genus != "" && species != "") {
            paste(genus, .strip_repeated_genus(species, genus))
          } else if (genus != "") {
            genus
          } else if (family != "") {
            family
          } else {
            NA_character_
          }
        })

        return(df)
      } else {
        return(df)
      }
    })

    # Warn when the "epithet" column actually holds full names - a user picked
    # a "Genus species" column there, which combined into "Genus Genus species".
    output$epithet_warning <- shiny::renderUI({
      req(data(), input$column_mode == "multiple")
      sp_col <- input$species_column
      req(!is.null(sp_col), sp_col != "", sp_col %in% names(data()))

      gen_col <- input$genus_column
      genus <- if (!is.null(gen_col) && gen_col %in% names(data())) data()[[gen_col]] else NULL

      share <- .share_binomial_epithets(data()[[sp_col]], genus)
      if (is.na(share) || share < 0.5) return(NULL)

      shiny::div(
        class = "alert alert-warning",
        style = "margin: 10px 0 0 0; padding: 8px 12px;",
        shiny::icon("exclamation-triangle"), " ",
        sprintf(
          i18n()$t("%d%% of the values in '%s' look like full names (genus + epithet), e.g. '%s'. This column should hold the epithet only. If it contains the full name, choose Single column mode and select it there."),
          round(share * 100),
          sp_col,
          attr(share, "example")
        )
      )
    })

    # Warn once per upload when the file already carries columns the matching
    # pipeline produces (an `idtax_n` from an earlier run is the common case).
    shiny::observeEvent(data(), {
      renamed <- .rename_conflicting_columns(data())$renamed

      if (length(renamed) > 0) {
        shiny::showNotification(
          paste0(
            i18n()$t("Existing columns renamed to avoid conflicts with the matching results:"),
            " ",
            paste(names(renamed), "\u2192", renamed, collapse = ", ")
          ),
          duration = 10,
          type = "warning"
        )
      }
    })

    # Return reactive list
    return(
      shiny::reactive({
        req(input$column_mode)

        if (input$column_mode == "single") {
          req(data())
          sanitised <- .rename_conflicting_columns(data())
          selected  <- input$column_name
        } else {
          # Multiple column mode
          req(processed_data())
          sanitised <- .rename_conflicting_columns(processed_data())
          selected  <- "taxonomic_name_combined"
        }

        # The selected column itself may have been renamed away from a
        # reserved name — follow it so the two stay in sync.
        if (selected %in% names(sanitised$renamed)) {
          selected <- unname(sanitised$renamed[[selected]])
        }

        list(
          column = selected,
          include_authors = input$include_authors %||% FALSE,
          data = sanitised$data,
          # Kept so the R-code preview can reproduce the multi-column
          # concatenation rather than only the resulting column.
          mode = input$column_mode,
          genus_column = if (identical(input$column_mode, "multiple")) input$genus_column %||% "" else "",
          species_column = if (identical(input$column_mode, "multiple")) input$species_column %||% "" else "",
          family_column = if (identical(input$column_mode, "multiple")) input$family_column %||% "" else ""
        )
      })
    )
  })
}


#' Share of "species epithet" values that are really full names
#'
#' An epithet is a single lower-case word ("kola"). A value is counted as a
#' full name when its first word repeats that row's genus, or, with no genus
#' column, when it starts with a capitalised word followed by another word
#' ("Garcinia kola").
#'
#' @param species Vector, the column chosen as species epithet.
#' @param genus Vector of the same length, the genus column, or NULL.
#' @return Numeric share in 0-1 (NA when no values), with attribute
#'   `example` holding the first offending value.
#' @keywords internal
.share_binomial_epithets <- function(species, genus = NULL) {
  species <- trimws(as.character(species))
  keep <- !is.na(species) & species != ""
  if (!any(keep)) return(NA_real_)

  first_word <- sub("[[:space:]].*$", "", species)
  n_words    <- lengths(strsplit(species, "[[:space:]]+"))

  looks_full <- grepl("^[A-Z][a-z-]+$", first_word) & n_words >= 2
  if (!is.null(genus)) {
    genus <- trimws(as.character(genus))
    looks_full <- looks_full |
      (!is.na(genus) & genus != "" & tolower(first_word) == tolower(genus))
  }

  looks_full <- looks_full[keep]
  share <- mean(looks_full)
  attr(share, "example") <- if (any(looks_full)) species[keep][which(looks_full)[1]] else NA_character_
  share
}

#' Drop a genus repeated at the start of an epithet
#'
#' `"Garcinia"` + `"Garcinia kola"` would otherwise combine into
#' `"Garcinia Garcinia kola"`, which matches nothing.
#'
#' @param species,genus Character scalars.
#' @return `species` without a leading copy of `genus`.
#' @keywords internal
.strip_repeated_genus <- function(species, genus) {
  if (is.na(genus) || genus == "") return(species)
  # Compared as text, not as a regex, so a genus is never read as a pattern
  words <- strsplit(species, "[[:space:]]+")[[1]]
  if (length(words) >= 2 && tolower(words[1]) == tolower(genus)) {
    paste(words[-1], collapse = " ")
  } else {
    species
  }
}
