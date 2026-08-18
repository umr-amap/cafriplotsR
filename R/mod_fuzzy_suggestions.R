# Fuzzy Suggestions Module
#
# Displays fuzzy match suggestions for a given taxonomic name

#' Fuzzy Suggestions Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_fuzzy_suggestions_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("suggestions_header")),
    shiny::uiOutput(ns("suggestions_controls")),
    shiny::hr(),
    shiny::uiOutput(ns("suggestions_list"))
  )
}


#' Fuzzy Suggestions Module - Server
#'
#' @param id Character, module ID
#' @param input_name Reactive character, the name to find suggestions for
#' @param max_suggestions Reactive or numeric, maximum suggestions to show
#' @param min_similarity Reactive or numeric, minimum similarity threshold
#' @param include_authors Reactive or logical, whether to include author names
#' @param i18n Reactive returning shiny.i18n translator
#' @param backbone Reactive returning the cached backbone tibble (or NULL).
#'   When non-NULL, all per-level searches run R-side without DB access —
#'   required for offline mode.
#'
#' @return Reactive integer, idtax_n of selected suggestion (or NULL)
#'
#' @keywords internal
mod_fuzzy_suggestions_server <- function(id, input_name, max_suggestions = shiny::reactive(10),
                                         min_similarity = shiny::reactive(0.3),
                                         include_authors = shiny::reactive(FALSE),
                                         i18n,
                                         backbone = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    suggestions <- shiny::reactiveVal(NULL)
    selected_id <- shiny::reactiveVal(NULL)

    # Fetch suggestions when input name OR filters change
    # Make reactive to: input_name, num_suggestions, min_similarity_slider, filter_level
    shiny::observe({
      req(input_name())

      name <- input_name()

      # Use slider value if available, otherwise use parameter default
      min_sim <- input$min_similarity_slider %||% (if (shiny::is.reactive(min_similarity)) min_similarity() else min_similarity)

      # Use input value if available, otherwise use parameter default
      max_sug <- input$num_suggestions %||% (if (shiny::is.reactive(max_suggestions)) max_suggestions() else max_suggestions)

      incl_auth <- if (shiny::is.reactive(include_authors)) include_authors() else include_authors

      # Get the selected level filter
      level_filter <- input$filter_level %||% "all"

      bb <- backbone()  # NULL when online without cache routing, tibble when offline

      # Based on level filter, do targeted searches or use hierarchical matching
      if (level_filter == "all") {
        # Use standard hierarchical matching for "all levels"
        matches <- match_taxonomic_names(
          names = name,
          method = "hierarchical",
          max_matches = 50,
          min_similarity = min_sim,
          include_synonyms = TRUE,
          return_scores = TRUE,
          include_authors = incl_auth,
          con = NULL,
          backbone = bb,
          verbose = FALSE
        )
      } else if (!is.null(bb)) {
        # Offline / cached-backbone path: R-side per-level fuzzy search
        matches <- .level_fuzzy_search_r(bb, name, level_filter, min_sim, max_results = 50L)
      } else {
        # Online path: direct database fuzzy search
        mydb_taxa <- call.mydb.taxa()

        if (level_filter == "family") {
          # Search in tax_fam column for family-level taxa
          sql <- glue::glue_sql("
            SELECT DISTINCT ON (tax_fam)
              idtax_n, idtax_good_n, tax_gen, tax_esp, tax_fam, tax_level,
              tax_fam AS matched_name,
              SIMILARITY(lower(tax_fam), lower({search_name})) AS match_score
            FROM table_taxa
            WHERE tax_fam IS NOT NULL
              AND tax_level = 'family'
              AND SIMILARITY(lower(tax_fam), lower({search_name})) >= {min_sim}
            ORDER BY tax_fam, match_score DESC
            LIMIT 50
          ", search_name = name, min_sim = min_sim, .con = mydb_taxa)

        } else if (level_filter == "genus") {
          # Search in tax_gen column for genus-level taxa
          sql <- glue::glue_sql("
            SELECT DISTINCT ON (tax_gen)
              idtax_n, idtax_good_n, tax_gen, tax_esp, tax_fam, tax_level,
              tax_gen AS matched_name,
              SIMILARITY(lower(tax_gen), lower({search_name})) AS match_score
            FROM table_taxa
            WHERE tax_gen IS NOT NULL
              AND tax_level = 'genus'
              AND SIMILARITY(lower(tax_gen), lower({search_name})) >= {min_sim}
            ORDER BY tax_gen, match_score DESC
            LIMIT 50
          ", search_name = name, min_sim = min_sim, .con = mydb_taxa)

        } else if (level_filter == "higher") {
          # Search in tax_famclass column for class-level taxa
          sql <- glue::glue_sql("
            SELECT DISTINCT ON (tax_famclass)
              idtax_n, idtax_good_n, tax_gen, tax_esp, tax_fam, tax_level,
              tax_famclass AS matched_name,
              SIMILARITY(lower(tax_famclass), lower({search_name})) AS match_score
            FROM table_taxa
            WHERE tax_famclass IS NOT NULL
              AND tax_level = 'higher'
              AND SIMILARITY(lower(tax_famclass), lower({search_name})) >= {min_sim}
            ORDER BY tax_famclass, match_score DESC
            LIMIT 50
          ", search_name = name, min_sim = min_sim, .con = mydb_taxa)

        } else if (level_filter == "order") {
          # Search in tax_order column for order-level taxa (if column exists)
          sql <- glue::glue_sql("
            SELECT DISTINCT ON (tax_order)
              idtax_n, idtax_good_n, tax_gen, tax_esp, tax_fam, tax_level,
              tax_order AS matched_name,
              SIMILARITY(lower(tax_order), lower({search_name})) AS match_score
            FROM table_taxa
            WHERE tax_order IS NOT NULL
              AND SIMILARITY(lower(tax_order), lower({search_name})) >= {min_sim}
            ORDER BY tax_order, match_score DESC
            LIMIT 50
          ", search_name = name, min_sim = min_sim, .con = mydb_taxa)

        } else {
          # For species/infraspecific, use hierarchical matching and filter
          matches <- match_taxonomic_names(
            names = name,
            method = "hierarchical",
            max_matches = 50,
            min_similarity = min_sim,
            include_synonyms = TRUE,
            return_scores = TRUE,
            include_authors = incl_auth,
            con = NULL,
            verbose = FALSE
          ) %>%
            dplyr::filter(tax_level == level_filter)

          suggestions(matches)
          return()
        }

        # Execute the SQL query for family/genus/higher/order
        matches <- tryCatch({
          result <- func_try_fetch(con = mydb_taxa, sql = sql)
          if (nrow(result) > 0) {
            result %>%
              dplyr::mutate(
                input_name = name,
                match_method = "fuzzy",
                match_rank = 1,
                is_synonym = idtax_n != idtax_good_n,
                accepted_name = NA_character_
              )
          } else {
            tibble()
          }
        }, error = function(e) {
          tibble()
        })
      }

      suggestions(matches)
    })

    # Suggestions header
    output$suggestions_header <- shiny::renderUI({
      req(input_name())

      shiny::div(
        style = "padding: 10px; background-color: #e7f3ff; border-radius: 5px; margin-bottom: 10px;",
        shiny::h4(
          paste(i18n()$t("Suggestions for:"), '"', input_name(), '"'),
          style = "margin: 0; color: #0056b3;"
        )
      )
    })

    # Suggestions controls
    output$suggestions_controls <- shiny::renderUI({
      ns <- session$ns

      # Build choices vectors with translations
      level_choices <- c("all", "species", "genus", "family", "order", "higher", "infraspecific")
      names(level_choices) <- c(
        i18n()$t("All levels"),
        i18n()$t("Species"),
        i18n()$t("Genus"),
        i18n()$t("Family"),
        i18n()$t("Order"),
        i18n()$t("Class (Higher)"),
        i18n()$t("Infraspecific")
      )

      sort_choices <- c("similarity", "alphabetical")
      names(sort_choices) <- c(
        i18n()$t("Similarity"),
        i18n()$t("Alphabetical")
      )

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::numericInput(
              inputId = ns("num_suggestions"),
              label = i18n()$t("Number of suggestions:"),
              value = if (shiny::is.reactive(max_suggestions)) max_suggestions() else max_suggestions,
              min = 5,
              max = 30,
              step = 5
            )
          ),
          shiny::column(
            width = 3,
            shiny::sliderInput(
              inputId = ns("min_similarity_slider"),
              label = i18n()$t("Min. similarity"),
              value = if (shiny::is.reactive(min_similarity)) min_similarity() else min_similarity,
              min = 0.3,
              max = 1.0,
              step = 0.05
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = ns("filter_level"),
              label = i18n()$t("Filter by level"),
              choices = level_choices,
              selected = "all"
            )
          ),
          shiny::column(
            width = 3,
            shiny::radioButtons(
              inputId = ns("sort_by"),
              label = i18n()$t("Sort by:"),
              choices = sort_choices,
              selected = "similarity",
              inline = TRUE
            )
          )
        )
      )
    })

    # Suggestions list
    output$suggestions_list <- shiny::renderUI({
      req(suggestions())

      sug <- suggestions()

      if (nrow(sug) == 0 || all(is.na(sug$idtax_n))) {
        return(
          shiny::div(
            style = "padding: 20px; background-color: #fff3cd; border-radius: 5px;",
            shiny::p(
              shiny::icon("exclamation-triangle"),
              i18n()$t("No suggestions found"),
              style = "color: #856404; margin: 0;"
            )
          )
        )
      }

      # Filter out NA matches
      sug <- sug %>% dplyr::filter(!is.na(idtax_n))

      # No need for post-fetch filtering - level filtering is now done at query time

      # Check if any results remain after filtering
      if (nrow(sug) == 0) {
        return(
          shiny::div(
            style = "padding: 20px; background-color: #fff3cd; border-radius: 5px;",
            shiny::p(
              shiny::icon("info-circle"),
              paste0("No matches found at the '", input$filter_level, "' level. Try selecting 'All levels' or a different taxonomic level."),
              style = "color: #856404; margin: 0;"
            )
          )
        )
      }

      # Sort suggestions
      if (!is.null(input$sort_by) && input$sort_by == "alphabetical") {
        sug <- sug %>% dplyr::arrange(matched_name)
      } else {
        sug <- sug %>% dplyr::arrange(desc(match_score))
      }

      # Limit number shown
      num_show <- input$num_suggestions %||% (if (shiny::is.reactive(max_suggestions)) max_suggestions() else max_suggestions)
      sug <- head(sug, num_show)

      ns <- session$ns

      # Create suggestion cards
      suggestion_cards <- lapply(1:nrow(sug), function(i) {
        row <- sug[i, ]

        score_pct <- round(row$match_score * 100)
        color_class <- if (score_pct >= 90) {
          "success"
        } else if (score_pct >= 70) {
          "info"
        } else if (score_pct >= 50) {
          "warning"
        } else {
          "secondary"
        }

        shiny::div(
          class = "card mb-2",
          style = "border-left: 4px solid #007bff;",

          shiny::div(
            class = "card-body p-3",

            shiny::fluidRow(
              shiny::column(
                width = 8,
                shiny::h5(
                  row$matched_name,
                  shiny::tags$small(
                    class = paste0("badge badge-", color_class, " ml-2"),
                    paste0(score_pct, "%")
                  ),
                  style = "margin: 0;"
                ),
                shiny::p(
                  class = "text-muted mb-1",
                  style = "font-size: 0.9em;",
                  paste0(
                    if (!is.na(row$tax_level)) paste(i18n()$t("Level:"), row$tax_level, " | ") else "",
                    if (!is.na(row$tax_fam)) paste(i18n()$t("Family:"), row$tax_fam, " | ") else "",
                    if (!is.na(row$tax_gen)) paste(i18n()$t("Genus:"), row$tax_gen) else ""
                  )
                ),
                shiny::p(
                  class = "text-muted mb-0",
                  style = "font-size: 0.85em;",
                  paste(i18n()$t("Match method:"), row$match_method)
                ),
                if (row$is_synonym && !is.na(row$accepted_name)) {
                  shiny::p(
                    class = "text-warning mb-0",
                    style = "font-size: 0.85em;",
                    shiny::icon("info-circle"),
                    paste(i18n()$t("Synonym"), "→", row$accepted_name)
                  )
                }
              ),
              shiny::column(
                width = 4,
                class = "text-right",
                shiny::actionButton(
                  inputId = ns(paste0("select_", i)),
                  label = i18n()$t("Select"),
                  class = "btn-sm btn-primary",
                  onclick = paste0("Shiny.setInputValue('", ns("selected_row"), "', ", i, ", {priority: 'event'});")
                )
              )
            )
          )
        )
      })

      shiny::div(suggestion_cards)
    })

    # Handle selection
    # Use ignoreInit = FALSE and ignoreNULL = FALSE to catch all clicks
    shiny::observeEvent(input$selected_row, ignoreInit = FALSE, ignoreNULL = FALSE, {
      req(suggestions())
      req(input$selected_row)

      sug <- suggestions()

      # Filter out NA matches
      sug <- sug %>% dplyr::filter(!is.na(idtax_n))

      # No need for level filtering here anymore - already done at query time
      # Just apply the same sorting as display

      # Sort same way as display
      if (!is.null(input$sort_by) && input$sort_by == "alphabetical") {
        sug <- sug %>% dplyr::arrange(matched_name)
      } else {
        sug <- sug %>% dplyr::arrange(desc(match_score))
      }

      # Limit to num_suggestions (same as display)
      num_show <- input$num_suggestions %||% (if (shiny::is.reactive(max_suggestions)) max_suggestions() else max_suggestions)
      sug <- head(sug, num_show)

      selected_row_idx <- input$selected_row

      if (selected_row_idx > 0 && selected_row_idx <= nrow(sug)) {
        selected_id(sug$idtax_n[selected_row_idx])
      }
    })

    # Return selected ID
    return(selected_id)
  })
}


#' R-side per-level fuzzy search on cached backbone
#'
#' Mirrors the per-level SQL queries in `mod_fuzzy_suggestions_server` for
#' the offline/cached-backbone code path. Returns a tibble with the same
#' columns the SQL path produces so downstream rendering doesn't change.
#'
#' @keywords internal
.level_fuzzy_search_r <- function(backbone, name, level_filter, min_sim,
                                  max_results = 50L) {
  if (is.null(backbone) || nrow(backbone) == 0L) return(dplyr::tibble())

  if (level_filter == "family") {
    cand <- backbone %>%
      dplyr::filter(!is.na(.data$tax_fam), .data$tax_level == "family") %>%
      dplyr::distinct(.data$tax_fam, .keep_all = TRUE)
    if (nrow(cand) == 0L) return(dplyr::tibble())
    sc <- .trigram_sim(cand$tax_fam, name)
    keep <- which(sc >= min_sim)
    if (length(keep) == 0L) return(dplyr::tibble())
    ord <- order(sc[keep], decreasing = TRUE)
    keep <- keep[ord[seq_len(min(max_results, length(ord)))]]
    out <- cand[keep, , drop = FALSE]
    out$matched_name <- out$tax_fam
    out$match_score <- sc[keep]

  } else if (level_filter == "genus") {
    cand <- backbone %>%
      dplyr::filter(!is.na(.data$tax_gen), .data$tax_level == "genus") %>%
      dplyr::distinct(.data$tax_gen, .keep_all = TRUE)
    if (nrow(cand) == 0L) return(dplyr::tibble())
    sc <- .trigram_sim(cand$tax_gen, name)
    keep <- which(sc >= min_sim)
    if (length(keep) == 0L) return(dplyr::tibble())
    ord <- order(sc[keep], decreasing = TRUE)
    keep <- keep[ord[seq_len(min(max_results, length(ord)))]]
    out <- cand[keep, , drop = FALSE]
    out$matched_name <- out$tax_gen
    out$match_score <- sc[keep]

  } else if (level_filter == "higher") {
    cand <- backbone %>%
      dplyr::filter(!is.na(.data$tax_famclass), .data$tax_level == "higher") %>%
      dplyr::distinct(.data$tax_famclass, .keep_all = TRUE)
    if (nrow(cand) == 0L) return(dplyr::tibble())
    sc <- .trigram_sim(cand$tax_famclass, name)
    keep <- which(sc >= min_sim)
    if (length(keep) == 0L) return(dplyr::tibble())
    ord <- order(sc[keep], decreasing = TRUE)
    keep <- keep[ord[seq_len(min(max_results, length(ord)))]]
    out <- cand[keep, , drop = FALSE]
    out$matched_name <- out$tax_famclass
    out$match_score <- sc[keep]

  } else if (level_filter == "order") {
    # Cache does not include tax_order — order-level fuzzy not supported offline
    return(dplyr::tibble())

  } else {
    # species / infraspecific: filter the hierarchical match by level
    matches <- match_taxonomic_names(
      names           = name,
      method          = "hierarchical",
      max_matches     = max_results,
      min_similarity  = min_sim,
      include_synonyms = TRUE,
      return_scores   = TRUE,
      include_authors = FALSE,
      con             = NULL,
      backbone        = backbone,
      verbose         = FALSE
    )
    if (nrow(matches) == 0L) return(matches)
    return(matches %>% dplyr::filter(.data$tax_level == level_filter))
  }

  # Shape to the columns the consumer expects (mirrors SQL path)
  dplyr::tibble(
    idtax_n      = out$idtax_n,
    idtax_good_n = out$idtax_good_n,
    tax_gen      = out$tax_gen,
    tax_esp      = if ("tax_esp" %in% names(out)) out$tax_esp else NA_character_,
    tax_fam      = out$tax_fam,
    tax_level    = out$tax_level,
    matched_name = out$matched_name,
    match_score  = out$match_score,
    input_name   = name,
    match_method = "fuzzy",
    match_rank   = 1L,
    is_synonym   = !is.na(out$idtax_good_n) & out$idtax_n != out$idtax_good_n,
    accepted_name = NA_character_
  )
}
