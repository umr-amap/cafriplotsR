#' Taxa R Code Preview Module - UI
#'
#' UI component for displaying equivalent R code for taxa search and trait
#' extraction. Shown/hidden by a toggle button.
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
mod_taxa_r_code_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    .rclipboard_setup(),
    shiny::uiOutput(ns("toggle_btn")),
    shiny::uiOutput(ns("code_panel"))
  )
}


#' Taxa R Code Preview Module - Server
#'
#' Generates equivalent R code reproducing the taxa search and (optionally)
#' trait extraction performed in the Shiny app.
#'
#' @param id Module namespace ID
#' @param search_params Reactive returning a named list of search parameters
#'   captured at last search execution.
#' @param selected_taxon Reactive returning the selected taxon data frame (one
#'   or more rows, each with `idtax_n`).
#' @param traits_fetched Reactive returning TRUE once trait extraction has been
#'   triggered in the trait table module.
#' @param is_public Reactive returning TRUE if user connected with public
#'   credentials.
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL (invisible)
#' @keywords internal
mod_taxa_r_code_server <- function(id, search_params, selected_taxon,
                                    traits_fetched, is_public, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_code <- shiny::reactiveVal(FALSE)

    # Toggle button — visible only after a search has been run
    output$toggle_btn <- shiny::renderUI({
      shiny::req(search_params())

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
      shiny::req(search_params())

      params        <- search_params()
      has_traits    <- isTRUE(traits_fetched())
      sel           <- selected_taxon()
      has_selection <- !is.null(sel) && nrow(sel) > 0

      # ---- Build query_taxa() call ----------------------------------------
      query_taxa_code <- .build_query_taxa_code(params)

      # ---- Notes for Shiny-only features ------------------------------------
      notes <- .build_shiny_notes(params, i18n)

      # ---- Build query_taxa_traits() call ----------------------------------
      traits_code <- if (has_traits && has_selection) {
        .build_query_taxa_traits_code(sel$idtax_n)
      } else {
        NULL
      }

      # ---- Combined workflow -----------------------------------------------
      combined_code <- .build_combined_taxa_code(
        query_taxa_code, traits_code, has_selection,
        is_public = isTRUE(is_public())
      )

      # ---- Render -----------------------------------------------------------
      shiny::wellPanel(
        style = "background-color: #f5f5f5; border: 1px solid #e0e0e0; margin-top: 8px;",

        shiny::h5(
          shiny::icon("code"),
          " ",
          i18n()$t("Equivalent R Code")
        ),
        shiny::p(
          class = "text-muted",
          style = "font-size: 0.9em;",
          i18n()$t("Use this code to reproduce the same query programmatically.")
        ),

        # query_taxa() section
        shiny::h6(shiny::icon("search"), " ", i18n()$t("Taxonomic Search")),
        .dark_code_block(ns, "code_search", query_taxa_code),
        .copy_btn(ns, "copy_search", query_taxa_code, i18n),
        shiny::br(), shiny::br(),

        # Notes for unsupported parameters
        if (!is.null(notes)) notes,

        # query_taxa_traits() section (only when triggered)
        if (has_traits && has_selection) {
          shiny::tagList(
            shiny::h6(shiny::icon("leaf"), " ", i18n()$t("Trait Extraction")),
            .dark_code_block(ns, "code_traits", traits_code),
            .copy_btn(ns, "copy_traits", traits_code, i18n),
            shiny::br(), shiny::br()
          )
        } else if (!has_traits) {
          shiny::p(
            class = "text-muted",
            style = "font-size: 0.85em; font-style: italic;",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("Click 'Extract as Table' above to also generate the query_taxa_traits() code.")
          )
        },

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

#' Build query_taxa() code string from search params
#' @keywords internal
.build_query_taxa_code <- function(params) {
  args <- c()

  if (params$search_mode == "binomial") {
    # Binomial mode: whole input passed as species
    val <- params$binomial %||% ""
    if (nzchar(val)) {
      args <- c(args, sprintf('  species = "%s"', val))
    }
  } else {
    # Structured mode
    if (!is.null(params$genus) && nzchar(params$genus))
      args <- c(args, sprintf('  genus = "%s"', params$genus))
    if (!is.null(params$species) && nzchar(params$species))
      args <- c(args, sprintf('  species = "%s"', params$species))
    if (!is.null(params$family) && nzchar(params$family))
      args <- c(args, sprintf('  family = "%s"', params$family))
    if (!is.null(params$order) && nzchar(params$order))
      args <- c(args, sprintf('  order = "%s"', params$order))
  }

  # ID filter
  if (!is.null(params$id_filter) && !is.na(params$id_filter)) {
    args <- c(args, sprintf('  ids = %s', params$id_filter))
  }

  # exact_match (function default is TRUE)
  if (isFALSE(params$exact_match)) {
    args <- c(args, '  exact_match = FALSE')
  }

  # include_children (function default is FALSE)
  if (isTRUE(params$include_children)) {
    args <- c(args, '  include_children = TRUE')
  }

  # extract_traits FALSE to get raw taxa first
  args <- c(args, '  extract_traits = FALSE')

  if (length(args) == 0) {
    # No filters — browse all
    return("# Browse all taxa (no filters)\ntaxa <- query_taxa(extract_traits = FALSE)")
  }

  paste0(
    "# Search taxa in the backbone\n",
    "taxa <- query_taxa(\n",
    paste(args, collapse = ",\n"),
    "\n)"
  )
}


#' Build informational notes for Shiny-only features not in query_taxa()
#' @keywords internal
.build_shiny_notes <- function(params, i18n) {
  notes <- list()

  if (isTRUE(params$include_synonyms)) {
    notes[[length(notes) + 1]] <- shiny::div(
      class = "alert alert-info",
      style = "font-size: 0.85em; padding: 10px;",
      shiny::icon("info-circle"),
      shiny::tags$strong(
        " include_synonyms: "
      ),
      i18n()$t("Not a parameter of query_taxa(). To add synonyms manually:"),
      shiny::tags$pre(
        style = "background-color: #282c34; color: #abb2bf; padding: 10px; border-radius: 4px; margin-top: 8px; font-size: 0.85em;",
        shiny::tags$code(
'# Find synonyms of the accepted taxa in results
accepted_ids <- taxa$idtax_n[is.na(taxa$idtax_good_n)]
synonyms     <- query_taxa(ids = accepted_ids, extract_traits = FALSE)
taxa_with_synonyms <- dplyr::bind_rows(taxa, synonyms)'
        )
      )
    )
  }

  if (!is.null(params$synonymy_filter) && params$synonymy_filter != "all") {
    filter_code <- if (params$synonymy_filter == "accepted") {
      'taxa_filtered <- dplyr::filter(taxa, is.na(idtax_good_n))  # accepted names only'
    } else {
      'taxa_filtered <- dplyr::filter(taxa, !is.na(idtax_good_n)) # synonyms only'
    }
    notes[[length(notes) + 1]] <- shiny::div(
      class = "alert alert-info",
      style = "font-size: 0.85em; padding: 10px;",
      shiny::icon("info-circle"),
      shiny::tags$strong(" synonymy_filter: "),
      i18n()$t("Not a parameter of query_taxa(). Apply as a post-processing filter:"),
      shiny::tags$pre(
        style = "background-color: #282c34; color: #abb2bf; padding: 10px; border-radius: 4px; margin-top: 8px; font-size: 0.85em;",
        shiny::tags$code(filter_code)
      )
    )
  }

  if (length(notes) == 0) return(NULL)
  do.call(shiny::tagList, notes)
}


#' Build query_taxa_traits() code string from selected taxon IDs
#' @keywords internal
.build_query_taxa_traits_code <- function(idtax_ids) {
  ids_str <- if (length(idtax_ids) == 1) {
    as.character(idtax_ids)
  } else {
    paste0("c(", paste(idtax_ids, collapse = ", "), ")")
  }

  paste0(
    "# Extract taxa-level traits (wide format — aggregated per taxon)\n",
    "traits_wide <- query_taxa_traits(\n",
    "  idtax            = ", ids_str, ",\n",
    "  format           = \"wide\",\n",
    "  include_synonyms = TRUE,\n",
    "  categorical_mode = \"mode\",\n",
    "  include_citation = TRUE\n",
    ")\n\n",
    "# Extract taxa-level traits (long format — one row per measurement)\n",
    "traits_long <- query_taxa_traits(\n",
    "  idtax                      = ", ids_str, ",\n",
    "  format                     = \"long\",\n",
    "  include_synonyms           = TRUE,\n",
    "  include_remarks            = TRUE,\n",
    "  include_measurement_features = TRUE,\n",
    "  include_citation           = TRUE\n",
    ")"
  )
}


#' Build the combined workflow script
#' @keywords internal
.build_combined_taxa_code <- function(query_taxa_code, traits_code,
                                       has_selection, is_public = FALSE) {
  # Connection code.
  #
  # The public branch used to print the public account's own credentials here,
  # which handed them to every visitor of the hosted app — and to anyone they
  # pasted the snippet to. The snippet is a starting point for work in R, and
  # work in R is done under one's own account, so both branches now prompt.
  connection_code <- c(
    "# Connect to databases (credentials will be requested interactively)",
    "# call.mydb()       # main database",
    "# call.mydb.taxa()  # taxa database\n"
  )
  if (is_public) {
    connection_code <- c(
      "# You are browsing through the read-only public account. To run this",
      "# script you need a database account of your own.",
      connection_code
    )
  }

  parts <- c(
    "# Complete workflow: Search taxa and extract trait data",
    "library(CafriplotsR)\n",
    connection_code,
    query_taxa_code
  )

  if (!is.null(traits_code)) {
    parts <- c(
      parts,
      "\n# Extract traits for the taxa found above",
      traits_code,
      "\n# Access results:",
      "# traits_wide$traits_numeric    — numeric traits (mean / sd / n per trait)",
      "# traits_wide$traits_categorical — categorical traits (most frequent value)",
      "# traits_long$traits_raw        — all individual measurements with citations"
    )
  } else if (has_selection) {
    parts <- c(
      parts,
      "\n# To also extract taxa-level traits, call:",
      "# traits <- query_taxa_traits(idtax = taxa$idtax_n, format = \"wide\", include_citation = TRUE)"
    )
  }

  paste(parts, collapse = "\n")
}


#' Render a dark-background code block
#' @keywords internal
.dark_code_block <- function(ns, output_id, code_str) {
  shiny::tags$pre(
    style = "background-color: #282c34; color: #abb2bf; padding: 15px; border-radius: 5px; overflow-x: auto; font-family: 'Fira Code', 'Consolas', monospace; font-size: 0.85em; max-height: 350px;",
    shiny::tags$code(id = ns(output_id), code_str)
  )
}


#' Render a rclipboard copy button
#' @keywords internal
.copy_btn <- function(ns, btn_id, code_str, i18n,
                      label = NULL) {
  if (is.null(label)) label <- i18n()$t("Copy to clipboard")
  .rclip_button(
    ns(btn_id),
    label,
    code_str,
    icon  = shiny::icon("copy"),
    class = "btn-sm btn-outline-secondary"
  )
}
