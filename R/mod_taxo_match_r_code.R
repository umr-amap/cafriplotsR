# Taxonomic Matching R Code Preview Module
#
# Shows the R code equivalent to the automatic matching performed in the
# taxonomic standardization app, so the same standardization can be re-run
# programmatically (in a script, on a bigger file, or as part of a pipeline).
#
# The manual review step is interactive by nature and has no programmatic
# equivalent - the generated script says so rather than pretending otherwise.

#' Taxonomic Matching R Code Preview Module - UI
#'
#' UI component for displaying the equivalent R code of the automatic
#' matching. Shown/hidden by a toggle button.
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
mod_taxo_match_r_code_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    .rclipboard_setup(),
    shiny::uiOutput(ns("toggle_btn")),
    shiny::uiOutput(ns("code_panel"))
  )
}


#' Taxonomic Matching R Code Preview Module - Server
#'
#' Generates R code reproducing the automatic matching run in the app with
#' `match_taxonomic_names()`.
#'
#' @param id Module namespace ID
#' @param match_results Reactive returning the auto-matching result list
#'   (as returned by `mod_auto_matching_server()`); its `$params` element
#'   carries the settings actually used for the last run.
#' @param column_info Reactive returning the column-selection list (as
#'   returned by `mod_column_select_server()`).
#' @param use_wcvp Reactive returning TRUE when WCVP names were requested.
#' @param is_offline Reactive returning TRUE when the app runs on the cached
#'   backbone without a database connection.
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL (invisible)
#' @keywords internal
mod_taxo_match_r_code_server <- function(id, match_results, column_info,
                                          use_wcvp = NULL, is_offline = NULL,
                                          i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_code <- shiny::reactiveVal(FALSE)

    # Toggle button - visible only once a matching run has produced results
    output$toggle_btn <- shiny::renderUI({
      shiny::req(match_results())

      label <- if (show_code()) {
        shiny::tagList(shiny::icon("code"), " ", i18n()$t("Hide R Code"))
      } else {
        shiny::tagList(shiny::icon("code"), " ", i18n()$t("Show Equivalent R Code"))
      }

      shiny::div(
        style = "margin-top: 20px;",
        shiny::actionButton(
          ns("btn_toggle"),
          label = label,
          class = "btn-outline-secondary btn-sm"
        )
      )
    })

    shiny::observeEvent(input$btn_toggle, {
      show_code(!show_code())
    })

    # Code panel
    output$code_panel <- shiny::renderUI({
      shiny::req(show_code())
      shiny::req(match_results())

      params <- match_results()$params %||% list()
      cols   <- column_info()

      prep_code <- .taxo_match_prep_code(cols)
      match_code <- .taxo_match_matching_code(params)
      wcvp_code <- if (isTRUE(params$use_wcvp %||% (!is.null(use_wcvp) && isTRUE(use_wcvp())))) {
        .taxo_match_wcvp_code()
      } else {
        NULL
      }

      combined_code <- .taxo_match_combined_code(
        prep_code, match_code, wcvp_code,
        is_offline = isTRUE(params$is_offline %||% (!is.null(is_offline) && isTRUE(is_offline())))
      )

      shiny::wellPanel(
        style = "background-color: #f5f5f5; border: 1px solid #e0e0e0; margin-top: 8px;",

        shiny::h5(shiny::icon("code"), " ", i18n()$t("Equivalent R Code")),
        shiny::p(
          class = "text-muted",
          style = "font-size: 0.9em;",
          i18n()$t("Use this code to reproduce the same automatic matching programmatically with standardize_taxonomic_batch().")
        ),

        # Data preparation (only shown when several columns were combined)
        if (!is.null(prep_code)) {
          shiny::tagList(
            shiny::h6(shiny::icon("table"), " ", i18n()$t("Data Preparation")),
            .dark_code_block(ns, "code_prep", prep_code),
            .copy_btn(ns, "copy_prep", prep_code, i18n),
            shiny::br(), shiny::br()
          )
        },

        # Automatic matching
        shiny::h6(shiny::icon("magic"), " ", i18n()$t("Automatic Matching")),
        .dark_code_block(ns, "code_match", match_code),
        .copy_btn(ns, "copy_match", match_code, i18n),
        shiny::br(), shiny::br(),

        # WCVP enrichment (only when the option was enabled)
        if (!is.null(wcvp_code)) {
          shiny::tagList(
            shiny::h6(shiny::icon("globe"), " ", i18n()$t("WCVP Names")),
            .dark_code_block(ns, "code_wcvp", wcvp_code),
            .copy_btn(ns, "copy_wcvp", wcvp_code, i18n),
            shiny::br(), shiny::br()
          )
        },

        # The one app step with no programmatic equivalent
        shiny::div(
          class = "alert alert-info",
          style = "font-size: 0.85em; padding: 10px;",
          shiny::icon("info-circle"), " ",
          i18n()$t(paste0(
            "The manual review step has no programmatic equivalent: it is interactive by nature. ",
            "Names the automatic matching could not resolve stay unmatched (idtax_n is NA) in the ",
            "code below - review them in the app, or inspect the ranked suggestions returned by ",
            "standardize_taxonomic_batch(keep_all_matches = TRUE)."
          ))
        ),

        # Combined script
        shiny::hr(),
        shiny::h6(shiny::icon("file-code"), " ", i18n()$t("Complete Workflow Script")),
        .dark_code_block(ns, "code_combined", combined_code),
        .copy_btn(ns, "copy_combined", combined_code, i18n,
                  label = i18n()$t("Copy complete script"))
      )
    })

    return(invisible(NULL))
  })
}


# ---- Internal helpers -------------------------------------------------------

#' Quote a column name for use inside generated `[[ ]]` indexing
#' @keywords internal
.taxo_match_col <- function(x) {
  paste0('[["', x, '"]]')
}


#' Build the data-preparation code
#'
#' Returns NULL in single-column mode: there is nothing to prepare, the
#' column is used as it stands. In multi-column mode it reproduces the
#' genus / epithet / family concatenation the app performs.
#'
#' @param cols The column-selection list returned by the column module
#' @keywords internal
.taxo_match_prep_code <- function(cols) {
  if (!identical(cols$mode %||% "single", "multiple")) {
    return(NULL)
  }

  gen <- cols$genus_column %||% ""
  sp  <- cols$species_column %||% ""
  fam <- cols$family_column %||% ""

  if (!nzchar(gen) && !nzchar(sp) && !nzchar(fam)) {
    return(NULL)
  }

  lines <- c(
    "# Combine the taxonomic columns into a single name column,",
    "# exactly as the app does in multi-column mode."
  )

  parts <- c()
  if (nzchar(gen)) {
    parts <- c(parts, sprintf('gen <- trimws(as.character(my_data%s))', .taxo_match_col(gen)))
  }
  if (nzchar(sp)) {
    parts <- c(parts, sprintf('sp  <- trimws(as.character(my_data%s))', .taxo_match_col(sp)))
  }
  if (nzchar(fam)) {
    parts <- c(parts, sprintf('fam <- trimws(as.character(my_data%s))', .taxo_match_col(fam)))
  }
  parts <- c(parts, "")

  # case_when clauses follow the same hierarchy as the app:
  # genus + epithet, then genus alone, then family alone.
  clauses <- c()
  if (nzchar(gen) && nzchar(sp)) {
    clauses <- c(clauses, '  !is.na(gen) & gen != "" & !is.na(sp) & sp != "" ~ paste(gen, sp),')
  }
  if (nzchar(gen)) {
    clauses <- c(clauses, '  !is.na(gen) & gen != ""                        ~ gen,')
  }
  if (nzchar(fam)) {
    clauses <- c(clauses, '  !is.na(fam) & fam != ""                        ~ fam,')
  }
  clauses <- c(clauses, "  TRUE                                           ~ NA_character_")

  paste(
    c(
      lines,
      parts,
      "my_data$taxonomic_name_combined <- dplyr::case_when(",
      clauses,
      ")"
    ),
    collapse = "\n"
  )
}


#' Build the `standardize_taxonomic_batch()` code
#'
#' @param params Settings captured at the last matching run
#' @keywords internal
.taxo_match_matching_code <- function(params) {
  col <- params$column %||% "species"
  min_sim <- params$min_similarity %||% 0.6
  incl_auth <- isTRUE(params$include_authors)

  paste0(
    "# The app matches against a local copy of the taxonomic backbone.\n",
    "# Reuse the cache it created: matching then runs entirely in R.\n",
    "backbone <- load_backbone_cache()\n",
    "# If this returns NULL (no cache yet), drop the `backbone` argument below:\n",
    "# the function then queries the taxa database directly.\n\n",
    "# Same cascade as the app, applied to every unique name:\n",
    "#   exact match -> fuzzy match restricted to the recognised genus -> full fuzzy match\n",
    "standardized <- standardize_taxonomic_batch(\n",
    "  data             = my_data,\n",
    "  name_column      = \"", col, "\",\n",
    "  method           = \"auto\",\n",
    "  min_similarity   = ", format(min_sim), ",\n",
    "  include_synonyms = TRUE,\n",
    "  include_authors  = ", if (incl_auth) "TRUE" else "FALSE", ",\n",
    "  backbone         = backbone,\n",
    "  verbose          = TRUE\n",
    ")\n\n",
    "# `standardized$corrected_name` is the standardized name:\n",
    "# the accepted name when the match is a synonym, the matched name otherwise."
  )
}


#' Build the optional WCVP enrichment code
#' @keywords internal
.taxo_match_wcvp_code <- function() {
  paste0(
    "# Replace the standardized name by the WCVP accepted name where available\n",
    "# (requires a database connection - WCVP links live in the taxa database).\n",
    "wcvp <- get_wcvp_names(unique(stats::na.omit(standardized$idtax_n)))\n\n",
    "standardized <- standardized %>%\n",
    "  dplyr::left_join(\n",
    "    dplyr::select(\n",
    "      wcvp, idtax_n, wcvp_taxon_name, wcvp_family,\n",
    "      wcvp_taxon_authors, wcvp_taxon_status, name_source\n",
    "    ),\n",
    "    by = \"idtax_n\"\n",
    "  ) %>%\n",
    "  dplyr::mutate(\n",
    "    corrected_name = dplyr::if_else(\n",
    "      !is.na(wcvp_taxon_name), wcvp_taxon_name, corrected_name\n",
    "    ),\n",
    "    name_source = dplyr::coalesce(name_source, \"internal\")\n",
    "  )"
  )
}


#' Assemble the full workflow script
#' @keywords internal
.taxo_match_combined_code <- function(prep_code, match_code, wcvp_code,
                                       is_offline = FALSE) {
  connection_code <- c(
    "# Step 1 - Connect to the taxa database",
    "# (credentials are requested interactively; not needed when a backbone",
    "#  cache is already available - see step 3)",
    "# call.mydb.taxa()\n"
  )
  if (is_offline) {
    connection_code <- c(
      "# You ran the app in offline mode, on the cached backbone. The script",
      "# below works the same way and needs no database account, except for",
      "# the optional WCVP step.",
      connection_code
    )
  }

  parts <- c(
    "# Complete workflow: standardize a list of taxonomic names",
    "library(CafriplotsR)\n",
    connection_code,
    "# Step 2 - Load your list of names",
    "my_data <- readxl::read_xlsx(\"my_names.xlsx\")",
    "# my_data <- readr::read_csv(\"my_names.csv\")   # or a CSV file\n"
  )

  if (!is.null(prep_code)) {
    parts <- c(parts, prep_code, "")
  }

  parts <- c(
    parts,
    "# Step 3 - Automatic matching against the taxonomic backbone",
    match_code
  )

  if (!is.null(wcvp_code)) {
    parts <- c(parts, "\n# Step 4 - WCVP names", wcvp_code)
  }

  parts <- c(
    parts,
    "\n# Names left unmatched (idtax_n is NA) are the ones the app asks you to",
    "# review manually - that step is interactive and has no equivalent here.",
    "unmatched <- standardized[is.na(standardized$idtax_n), ]\n",
    "# Save the standardized table",
    "writexl::write_xlsx(standardized, \"standardized_taxonomy.xlsx\")\n",
    "# To then retrieve traits for the matched taxa (the app's Traits tab):",
    "# traits <- query_taxa_traits(",
    "#   idtax            = unique(stats::na.omit(standardized$idtax_n)),",
    "#   format           = \"wide\",",
    "#   include_synonyms = TRUE,",
    "#   categorical_mode = \"mode\",",
    "#   include_citation = TRUE",
    "# )"
  )

  paste(parts, collapse = "\n")
}
