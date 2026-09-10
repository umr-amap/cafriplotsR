# Name Preview Module
#
# Sits between column selection and automatic matching: shows what the app
# will actually look up, so a wrong column is caught before a long run
# rather than after it.

# ---------------------------------------------------------------------------
# Summary helper (pure — no shiny, so it can be tested directly)
# ---------------------------------------------------------------------------

#' Summarise the names a matching run would look up
#'
#' @description
#' Describes the column the user selected the way the matching pipeline will
#' read it: distinct values, how often each occurs, the normalised form that
#' is actually searched, and the rank the parser detects. This is what lets a
#' user notice, before starting, that they pointed the app at the wrong
#' column.
#'
#' Counting follows the pipeline: distinct *raw* values, because that is what
#' `mod_auto_matching_server()` iterates over. Two spellings that normalise to
#' the same string are two lookups, and are reported as two names.
#'
#' @param values Character vector, the selected column's contents.
#' @param max_parse Integer, above this many distinct names the per-name rank
#'   detection is skipped (it is only a display aid, and the parse is the one
#'   part of this that grows with the list). Default 5000.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{n_rows}: number of rows in the column
#'     \item \code{n_missing}: rows with no usable name (NA or blank)
#'     \item \code{n_unique}: distinct names that would be looked up
#'     \item \code{rank_counts}: named integer vector, distinct names per
#'       detected rank (empty when the parse was skipped)
#'     \item \code{parsed}: logical, whether rank detection ran
#'     \item \code{names}: data.frame of \code{name}, \code{searched},
#'       \code{n} and \code{rank}, most frequent first
#'   }
#'
#' @keywords internal
.summarise_names_to_match <- function(values, max_parse = 5000L) {
  values <- as.character(values)
  n_rows <- length(values)

  trimmed <- trimws(values)
  missing <- is.na(trimmed) | trimmed == ""
  n_missing <- sum(missing)

  empty_result <- data.frame(
    name = character(0), searched = character(0),
    n = integer(0), rank = character(0),
    stringsAsFactors = FALSE
  )

  usable <- trimmed[!missing]

  if (length(usable) == 0) {
    return(list(
      n_rows = n_rows, n_missing = n_missing, n_unique = 0L,
      rank_counts = integer(0), parsed = TRUE, names = empty_result
    ))
  }

  counts <- table(usable)
  uniq   <- names(counts)
  n      <- as.integer(counts)

  # Most frequent first: a mis-selected column usually shows up as one value
  # repeated hundreds of times, which then sits at the top of the table.
  ord  <- order(-n, uniq)
  uniq <- uniq[ord]
  n    <- n[ord]

  searched <- clean_taxonomic_name(uniq)

  parsed <- length(uniq) <= max_parse

  if (parsed) {
    rank <- vapply(
      searched,
      function(x) parse_taxonomic_name(x)$rank,
      character(1),
      USE.NAMES = FALSE
    )
    rank_counts <- table(rank)
    rank_counts <- stats::setNames(as.integer(rank_counts), names(rank_counts))
  } else {
    rank <- rep(NA_character_, length(uniq))
    rank_counts <- integer(0)
  }

  list(
    n_rows      = n_rows,
    n_missing   = n_missing,
    n_unique    = length(uniq),
    rank_counts = rank_counts,
    parsed      = parsed,
    names       = data.frame(
      name = uniq, searched = searched, n = n, rank = rank,
      stringsAsFactors = FALSE
    )
  )
}


#' Name Preview Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_name_preview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(ns("panel"))
}


#' Name Preview Module - Server
#'
#' @param id Character, module ID
#' @param data Reactive data.frame, post column-selection data
#' @param column_name Reactive character, the column that will be matched
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return Reactive returning the summary list from
#'   \code{.summarise_names_to_match()}, or NULL when no column is selected.
#'
#' @keywords internal
mod_name_preview_server <- function(id, data, column_name, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    summary_data <- shiny::reactive({
      df  <- data()
      col <- column_name()

      shiny::req(df, col)
      # The combined column is built by the selection module, so it can lag a
      # frame behind a change of mode. Nothing to preview until it catches up.
      shiny::req(col %in% names(df))

      .summarise_names_to_match(df[[col]])
    })

    # `counts[["missing"]]` errors and `counts["missing"]` gives NA — neither
    # is what a chip wants, so normalise to a plain count here.
    .rank_n <- function(counts, r) {
      v <- counts[r]
      if (length(v) == 0L || is.na(v)) 0L else as.integer(v)
    }

    # Small labelled figure, repeated across the header row.
    .stat <- function(value, label, colour = "#2c3e50") {
      shiny::div(
        style = "display:inline-block; margin-right:28px; vertical-align:top;",
        shiny::div(
          style = paste0("font-size:22px; font-weight:bold; color:", colour, ";"),
          format(value, big.mark = " ")
        ),
        shiny::div(
          style = "font-size:12px; color:#7f8c8d; text-transform:uppercase;",
          label
        )
      )
    }

    output$panel <- shiny::renderUI({
      s <- summary_data()

      # Rank breakdown, in a fixed order so the row does not reshuffle
      # between datasets. Ranks the parser never returned are left out.
      rank_labels <- list(
        species = i18n()$t("species"),
        genus   = i18n()$t("genus only"),
        family  = i18n()$t("family"),
        order   = i18n()$t("order"),
        class   = i18n()$t("class"),
        unknown = i18n()$t("unrecognised")
      )

      rank_chips <- lapply(names(rank_labels), function(r) {
        n <- .rank_n(s$rank_counts, r)
        if (n == 0L) return(NULL)
        colour <- if (r == "unknown") "#c0392b" else "#34495e"
        .stat(n, rank_labels[[r]], colour)
      })

      warnings <- shiny::tagList()

      if (s$n_missing > 0) {
        warnings <- shiny::tagList(
          warnings,
          shiny::div(
            style = "color:#8a6d3b; background:#fcf8e3; border:1px solid #faebcc; padding:8px 12px; border-radius:4px; margin-top:10px;",
            shiny::icon("exclamation-triangle"), " ",
            sprintf(
              "%s %s",
              format(s$n_missing, big.mark = " "),
              i18n()$t("rows have no name in the selected column and will not be matched.")
            )
          )
        )
      }

      # Every name reading as a bare genus is legitimate for a genus list, but
      # it is also exactly what a mis-selected column looks like. Say what was
      # detected and let the user judge.
      only_genus <- s$n_unique > 1 &&
        .rank_n(s$rank_counts, "genus") == s$n_unique

      if (isTRUE(only_genus)) {
        warnings <- shiny::tagList(
          warnings,
          shiny::div(
            style = "color:#31708f; background:#d9edf7; border:1px solid #bce8f1; padding:8px 12px; border-radius:4px; margin-top:10px;",
            shiny::icon("info-circle"), " ",
            i18n()$t("Every entry is read as a genus name, with no specific epithet. If that is not intended, check the column selection.")
          )
        )
      }

      shiny::div(
        style = "border:1px solid #d6e4f0; border-radius:5px; padding:14px 16px; margin-bottom:18px; background:#ffffff;",

        shiny::h4(
          shiny::icon("list"), " ",
          i18n()$t("Names to be matched"),
          style = "margin-top:0; color:#2c3e50;"
        ),
        shiny::helpText(
          i18n()$t("Check below that the selected column contains what you expect before starting the matching."),
          style = "margin-top:-6px;"
        ),

        shiny::div(
          style = "margin:12px 0 4px 0;",
          .stat(s$n_unique, i18n()$t("unique names"), "#2980b9"),
          .stat(s$n_rows, i18n()$t("rows")),
          rank_chips
        ),

        warnings,

        shiny::hr(style = "margin:12px 0;"),

        DT::DTOutput(session$ns("preview_table"))
      )
    })

    output$preview_table <- DT::renderDT({
      s <- summary_data()
      shiny::req(nrow(s$names) > 0)

      tbl <- s$names

      # The normalised form is only worth a column when it actually differs
      # somewhere ("Genus sp." -> "Genus"); otherwise it is a duplicate of
      # the name column and costs the reader attention for nothing.
      show_searched <- any(tbl$searched != tbl$name)

      out <- data.frame(
        name = tbl$name,
        stringsAsFactors = FALSE
      )
      names(out) <- i18n()$t("Name")

      if (show_searched) {
        out[[i18n()$t("Searched as")]] <- tbl$searched
      }

      out[[i18n()$t("Occurrences")]] <- tbl$n

      if (s$parsed) {
        out[[i18n()$t("Detected rank")]] <- tbl$rank
      }

      DT::datatable(
        out,
        rownames = FALSE,
        selection = "none",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          language = list(
            search = i18n()$t("Search:"),
            emptyTable = i18n()$t("No names found in the selected column")
          )
        )
      )
    })

    summary_data
  })
}
