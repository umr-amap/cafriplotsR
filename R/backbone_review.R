# =============================================================================
# Taxonomic backbones - reviewing uncertain matches
#
# review_backbone_matches() opens a table of the matches a person must judge
# (fuzzy names, identical names whose authors differ, taxa with several
# candidates), with the differing words highlighted. Accepted rows come back
# verified, rejected rows are dropped when saved.
# =============================================================================


#' Review kinds, in display order
#' @noRd
.review_kind_labels <- c(
  fuzzy           = "Fuzzy name",
  author_mismatch = "Authors differ",
  several         = "Several candidates"
)


#' Matches with the review columns added
#'
#' Adds `.key` (`idtax_n|external_id`), `review_kind` (`NA` for rows that need
#' no review) and `decision` (kept if already present).
#' @noRd
.review_rows <- function(matches) {
  missing_cols <- setdiff(c("idtax_n", "external_id", "match_type"), names(matches))
  if (length(missing_cols) > 0) {
    cli::cli_abort("{.arg matches} lacks column{?s} {.field {missing_cols}}.")
  }
  matches <- as.data.frame(matches, stringsAsFactors = FALSE)
  n <- nrow(matches)

  n_candidates <- if (n > 0) stats::ave(rep(1L, n), matches$idtax_n, FUN = length) else integer(0)
  matches$.key <- paste(matches$idtax_n, matches$external_id, sep = "|")
  matches$review_kind <- ifelse(
    matches$match_type == "fuzzy", "fuzzy",
    ifelse(matches$match_type == "author_mismatch", "author_mismatch",
           ifelse(n_candidates > 1, "several", NA_character_))
  )
  if (!"decision" %in% names(matches)) matches$decision <- rep(NA_character_, n)
  for (col in c("taxon_name_internal", "authors_internal", "backbone_taxon_name",
                "backbone_authors", "backbone_status")) {
    if (!col %in% names(matches)) matches[[col]] <- rep(NA_character_, n)
  }
  for (col in c("match_score", "author_score")) {
    if (!col %in% names(matches)) matches[[col]] <- rep(NA_real_, n)
  }
  matches
}


#' Escape text for HTML
#' @noRd
.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}


#' Highlight the words of one string absent from the other
#'
#' @param x,y Character vectors of the same length.
#' @return A list of two character vectors of HTML, `x` and `y`, with the
#'   differing words wrapped in `<mark>`.
#' @noRd
.mark_word_diff <- function(x, y) {
  mark <- function(a, b) {
    if (is.na(a) || !nzchar(a)) return("")
    wa <- strsplit(a, " ", fixed = TRUE)[[1]]
    wb <- if (is.na(b)) character(0) else strsplit(b, " ", fixed = TRUE)[[1]]
    out <- .html_escape(wa)
    differs <- !wa %in% wb
    out[differs] <- paste0("<mark>", out[differs], "</mark>")
    paste(out, collapse = " ")
  }
  x <- as.character(x)
  y <- as.character(y)
  list(
    x = as.character(mapply(mark, x, y, USE.NAMES = FALSE)),
    y = as.character(mapply(mark, y, x, USE.NAMES = FALSE))
  )
}


#' Record decisions
#'
#' Accepting a candidate rejects the taxon's other undecided candidates: only
#' one link can supply its names.
#'
#' @param data Data frame from `.review_rows()`.
#' @param keys `.key` values.
#' @param decision `"accepted"`, `"rejected"` or `NA` (undo).
#' @noRd
.set_decision <- function(data, keys, decision) {
  idx <- which(data$.key %in% keys)
  if (length(idx) == 0) return(data)
  data$decision[idx] <- decision
  if (identical(decision, "accepted")) {
    siblings <- data$idtax_n %in% data$idtax_n[idx] & !data$.key %in% keys &
      is.na(data$decision)
    data$decision[siblings] <- "rejected"
  }
  data
}


#' Decisions saved so far, applied to matches
#' @noRd
.load_review_file <- function(data, file) {
  if (is.null(file) || !file.exists(file)) return(data)
  saved <- readRDS(file)
  idx <- match(saved$.key, data$.key)
  found <- !is.na(idx)
  data$decision[idx[found]] <- saved$decision[found]
  data
}


#' Save the decisions made so far
#' @noRd
.save_review_file <- function(data, file) {
  if (is.null(file)) return(invisible(NULL))
  decided <- data[!is.na(data$decision), c(".key", "idtax_n", "external_id", "decision"), drop = FALSE]
  saveRDS(decided, file)
  invisible(NULL)
}


#' Matches as returned to the caller
#' @noRd
.review_result <- function(data) {
  data$verified <- data$decision %in% "accepted"
  data$.key <- NULL
  dplyr::as_tibble(data)
}


#' Rows shown in the review table
#' @noRd
.review_display <- function(d) {
  names_diff <- .mark_word_diff(d$taxon_name_internal, d$backbone_taxon_name)
  authors_diff <- .mark_word_diff(d$authors_internal, d$backbone_authors)
  decision <- ifelse(is.na(d$decision), "undecided", d$decision)
  data.frame(
    Decision           = sprintf("<span class='rv-badge rv-%s'>%s</span>", decision, decision),
    Kind               = unname(.review_kind_labels[d$review_kind]),
    `Internal name`    = names_diff$x,
    `Internal authors` = authors_diff$x,
    `Backbone name`    = names_diff$y,
    `Backbone authors` = authors_diff$y,
    Status             = .html_escape(ifelse(is.na(d$backbone_status), "", d$backbone_status)),
    `Name score`       = round(d$match_score, 3),
    `Author score`     = round(d$author_score, 2),
    idtax_n            = d$idtax_n,
    `Backbone ID`      = .html_escape(d$external_id),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


#' The review app
#'
#' @param matches Matches, with or without the review columns.
#' @param review_file Path of the decisions file, or `NULL`.
#' @return A `shiny.appobj`; stopping it returns the reviewed matches.
#' @noRd
.review_app <- function(matches, review_file = NULL) {

  data <- if (all(c(".key", "review_kind", "decision") %in% names(matches))) {
    matches
  } else {
    .load_review_file(.review_rows(matches), review_file)
  }
  kind_counts <- table(factor(data$review_kind, levels = names(.review_kind_labels)))
  kinds_present <- names(kind_counts)[kind_counts > 0]

  css <- "
    body { padding: 12px 18px; }
    .rv-header { display: flex; align-items: baseline; gap: 18px; margin-bottom: 8px; }
    .rv-header h3 { margin: 0; }
    .rv-progress { color: #555; }
    .rv-side { background: #f7f7f7; border-radius: 6px; padding: 12px; }
    .rv-side .btn { margin-bottom: 6px; }
    .rv-keys { color: #666; font-size: 12px; }
    mark { background: #ffe08a; padding: 0 2px; border-radius: 2px; }
    .rv-badge { display: inline-block; padding: 1px 7px; border-radius: 9px; font-size: 11px; }
    .rv-undecided { background: #e9ecef; color: #495057; }
    .rv-accepted { background: #d4edda; color: #155724; }
    .rv-rejected { background: #f8d7da; color: #721c24; }
    table.dataTable tbody td { font-size: 13px; vertical-align: middle; }
  "

  js <- "
    document.addEventListener('keydown', function(e) {
      var tag = e.target.tagName;
      if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
      if (e.ctrlKey || e.metaKey || e.altKey) return;
      var actions = {a: 'accept', r: 'reject', u: 'undo'};
      var action = actions[e.key.toLowerCase()];
      if (action) {
        e.preventDefault();
        Shiny.setInputValue('key_action', {action: action, nonce: Math.random()});
      }
    });
  "

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(css)),
                     shiny::tags$script(shiny::HTML(js))),
    shiny::div(
      class = "rv-header",
      shiny::h3("Review backbone matches"),
      shiny::span(class = "rv-progress", shiny::textOutput("progress", inline = TRUE))
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::div(
          class = "rv-side",
          shiny::checkboxGroupInput(
            "kinds", "Show kinds",
            choices = stats::setNames(names(.review_kind_labels),
                                      paste0(.review_kind_labels, " (", as.integer(kind_counts), ")")),
            selected = kinds_present
          ),
          shiny::radioButtons("show", "Decision",
                              choices = c(Undecided = "undecided", Accepted = "accepted",
                                          Rejected = "rejected", All = "all"),
                              inline = TRUE),
          shiny::sliderInput("score", "Name score", min = 0, max = 1,
                             value = c(0, 1), step = 0.01),
          shiny::hr(),
          shiny::actionButton("accept", "Accept selected", class = "btn-success btn-block"),
          shiny::actionButton("reject", "Reject selected", class = "btn-danger btn-block"),
          shiny::actionButton("undo", "Undo decision", class = "btn-default btn-block"),
          shiny::p(class = "rv-keys",
                   "Keys: A accept, R reject, U undo. Click rows to select several. ",
                   "Accepting a candidate rejects the taxon's other undecided candidates."),
          shiny::hr(),
          shiny::strong("Accept in bulk"),
          shiny::p(class = "rv-keys", "Undecided fuzzy and author rows shown, at or above:"),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput("bulk_name", "Name score", 0.97, 0, 1, 0.01)),
            shiny::column(6, shiny::numericInput("bulk_author", "Author score", 0.8, 0, 1, 0.05))
          ),
          shiny::checkboxInput("bulk_author_na", "Also rows without an author score", FALSE),
          shiny::actionButton("bulk", "Accept these rows", class = "btn-block"),
          shiny::hr(),
          shiny::actionButton("done", "Done: return the decisions", class = "btn-primary btn-block"),
          if (!is.null(review_file)) {
            shiny::p(class = "rv-keys", "Decisions are saved to ", shiny::code(review_file),
                     " after each change.")
          }
        )
      ),
      shiny::column(9, DT::DTOutput("table"))
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(data = data)
    stopped <- FALSE
    finish <- function(value) {
      if (!stopped) {
        stopped <<- TRUE
        shiny::stopApp(value)
      }
    }

    shown <- shiny::reactive({
      d <- rv$data
      kinds <- if (is.null(input$kinds)) character(0) else input$kinds
      keep <- d$review_kind %in% kinds
      show <- if (is.null(input$show)) "undecided" else input$show
      keep <- keep & switch(show,
                            undecided = is.na(d$decision),
                            accepted  = d$decision %in% "accepted",
                            rejected  = d$decision %in% "rejected",
                            all       = TRUE)
      range <- if (is.null(input$score)) c(0, 1) else input$score
      s <- d$match_score
      keep <- keep & (is.na(s) | (s >= range[1] - 1e-9 & s <= range[2] + 1e-9))
      idx <- which(keep)
      idx[order(match(d$review_kind[idx], names(.review_kind_labels)),
                -d$match_score[idx], d$taxon_name_internal[idx], d$idtax_n[idx])]
    })

    output$progress <- shiny::renderText({
      d <- rv$data[!is.na(rv$data$review_kind), , drop = FALSE]
      sprintf("%d of %d decided: %d accepted, %d rejected",
              sum(!is.na(d$decision)), nrow(d),
              sum(d$decision %in% "accepted"), sum(d$decision %in% "rejected"))
    })

    output$table <- DT::renderDT({
      idx <- shiny::isolate(shown())
      DT::datatable(
        .review_display(shiny::isolate(rv$data)[idx, , drop = FALSE]),
        escape = FALSE, rownames = FALSE, selection = "multiple",
        class = "compact stripe hover",
        options = list(pageLength = 25, lengthMenu = c(25, 50, 100, 500),
                       ordering = FALSE, autoWidth = FALSE,
                       language = list(search = "Filter:"))
      )
    }, server = TRUE)

    proxy <- DT::dataTableProxy("table")
    refresh <- function(select_first) {
      idx <- shown()
      DT::replaceData(proxy, .review_display(rv$data[idx, , drop = FALSE]),
                      rownames = FALSE, resetPaging = FALSE, clearSelection = "all")
      if (select_first && length(idx) > 0) DT::selectRows(proxy, 1)
    }

    shiny::observeEvent(list(input$kinds, input$show, input$score), refresh(FALSE),
                        ignoreInit = TRUE, ignoreNULL = FALSE)

    decide <- function(decision) {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        shiny::showNotification("Select rows first.", type = "warning", duration = 2)
        return(invisible(NULL))
      }
      idx <- shown()[selected]
      rv$data <- .set_decision(rv$data, rv$data$.key[idx], decision)
      .save_review_file(rv$data, review_file)
      refresh(select_first = TRUE)
    }

    shiny::observeEvent(input$accept, decide("accepted"))
    shiny::observeEvent(input$reject, decide("rejected"))
    shiny::observeEvent(input$undo, decide(NA_character_))
    shiny::observeEvent(input$key_action, {
      switch(input$key_action$action,
             accept = decide("accepted"),
             reject = decide("rejected"),
             undo   = decide(NA_character_))
    })

    shiny::observeEvent(input$bulk, {
      idx <- shown()
      d <- rv$data[idx, , drop = FALSE]
      author_ok <- (!is.na(d$author_score) & d$author_score >= input$bulk_author) |
        (is.na(d$author_score) & isTRUE(input$bulk_author_na))
      ok <- d$review_kind %in% c("fuzzy", "author_mismatch") & is.na(d$decision) &
        !is.na(d$match_score) & d$match_score >= input$bulk_name & author_ok
      if (!any(ok)) {
        shiny::showNotification("No undecided row shown meets these scores.", type = "warning")
        return(invisible(NULL))
      }
      rv$data <- .set_decision(rv$data, d$.key[ok], "accepted")
      .save_review_file(rv$data, review_file)
      shiny::showNotification(sprintf("Accepted %d rows.", sum(ok)), type = "message")
      refresh(select_first = FALSE)
    })

    shiny::observeEvent(input$done, finish(.review_result(rv$data)))
    session$onSessionEnded(function() finish(.review_result(shiny::isolate(rv$data))))
  }

  shiny::shinyApp(ui, server)
}


#' Review uncertain backbone matches
#'
#' @description
#' Opens a table of the matches from [match_taxa_to_backbone()] that need a
#' person's judgement, to accept or reject them quickly:
#' \itemize{
#'   \item \strong{Fuzzy name}: a close backbone name;
#'   \item \strong{Authors differ}: an identical name whose authors disagree;
#'   \item \strong{Several candidates}: a taxon matched to several identical
#'     names (homonyms).
#' }
#' Words that differ between the internal and the backbone name, and between
#' their authors, are highlighted. Select rows and press \kbd{A} to accept,
#' \kbd{R} to reject, \kbd{U} to undo; the first remaining row is then
#' selected, so a list can be worked through from the keyboard. Accepting a
#' candidate rejects the taxon's other undecided candidates. Rows can also be
#' accepted in bulk above a name and author score.
#'
#' With \code{review_file}, decisions are written to that file after each
#' change and loaded when the review is opened again with the same file, so a
#' long review can be spread over several sessions, even with a new matching
#' run (decisions are kept by taxon and backbone ID).
#'
#' No database connection is used.
#'
#' @param matches Data frame from [match_taxa_to_backbone()].
#' @param review_file Optional path of an \code{.rds} file holding the
#'   decisions.
#' @param open Logical. Open the review page. With \code{FALSE}, the decisions
#'   of \code{review_file} are applied to \code{matches} and returned at once,
#'   which is what a script needs after a new matching run.
#' @param launch.browser Logical. Open in the web browser (default) rather than
#'   the RStudio viewer.
#'
#' @return The matches, with \code{review_kind}, \code{decision}
#'   (\code{"accepted"}, \code{"rejected"} or \code{NA}) and \code{verified}
#'   (\code{TRUE} for accepted rows). Pass them to [save_backbone_links()] or
#'   [replace_backbone_links()]: rejected rows are not saved, accepted rows are
#'   saved verified, undecided fuzzy and author-mismatch rows are saved but
#'   supply no names.
#'
#' @examples
#' \dontrun{
#' matches <- match_taxa_to_backbone("wcvp", con_taxa, author_match = "fuzzy")
#' matches <- review_backbone_matches(matches, review_file = "wcvp_review.rds")
#' table(matches$review_kind, matches$decision, useNA = "ifany")
#' replace_backbone_links(matches, "wcvp", con_taxa, taxa = "all")
#' }
#'
#' @export
review_backbone_matches <- function(matches, review_file = NULL, open = TRUE,
                                    launch.browser = TRUE) {
  data <- .load_review_file(.review_rows(matches), review_file)

  if (!open) {
    decided <- !is.na(data$decision)
    cli::cli_alert_info(
      "{sum(data$decision %in% 'accepted')} accepted, {sum(data$decision %in% 'rejected')} rejected, {sum(!is.na(data$review_kind) & !decided)} left to review."
    )
    return(.review_result(data))
  }

  n_review <- sum(!is.na(data$review_kind))
  if (n_review == 0) {
    cli::cli_alert_info("Nothing to review: no fuzzy, author-mismatch or several-candidate rows.")
    return(.review_result(data))
  }
  n_decided <- sum(!is.na(data$review_kind) & !is.na(data$decision))
  cli::cli_alert_info("{n_review} row{?s} to review, {n_decided} already decided.")

  viewer <- if (launch.browser) shiny::browserViewer() else shiny::paneViewer()
  result <- shiny::runGadget(.review_app(data, review_file), viewer = viewer,
                             stopOnCancel = FALSE)

  reviewed <- result[!is.na(result$review_kind), , drop = FALSE]
  cli::cli_alert_success(
    "{sum(reviewed$decision %in% 'accepted')} accepted, {sum(reviewed$decision %in% 'rejected')} rejected, {sum(is.na(reviewed$decision))} undecided."
  )
  result
}
