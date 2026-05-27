# Specimen Identification App - Batch Step 3: Collector lookup
#
# If id_specimen is mapped, no lookup needed (matched_data = uploaded data).
# Otherwise: resolve user collector strings against table_colnam, with
# exact/fuzzy/unmatched buckets, allowing the user to confirm fuzzy matches.

#' Batch lookup UI
#' @param id Module id
#' @param i18n Translator
#' @keywords internal
#' @export
mod_specid_batch_lookup_ui <- function(id, i18n) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("user"), " ", i18n$t("Step 3: Match collectors")),
    shiny::p(i18n$t("If id_specimen is mapped, this step is skipped. Otherwise, collector names are matched to the database."),
             style = "color: #6c757d;"),
    shiny::actionButton(ns("run_analysis"),
                        shiny::tagList(shiny::icon("search"), " ", i18n$t("Analyze")),
                        class = "btn-primary"),
    shiny::br(), shiny::br(),
    shiny::uiOutput(ns("summary_ui")),
    shiny::uiOutput(ns("matching_ui"))
  )
}

#' Batch lookup server
#' @param id Module id
#' @param data Reactive uploaded data
#' @param mappings Reactive mappings list
#' @param pool_main Reactive main DB pool
#' @param i18n Reactive translator
#' @return list(matched_data, is_complete)
#' @keywords internal
#' @export
mod_specid_batch_lookup_server <- function(id, data, mappings, pool_main, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    coll_analysis <- shiny::reactiveVal(NULL)
    matched_rv    <- shiny::reactiveVal(NULL)
    complete_rv   <- shiny::reactiveVal(FALSE)

    # If id_specimen is mapped, skip lookup automatically.
    shiny::observe({
      d <- data(); m <- mappings()
      if (is.null(d) || is.null(m)) return()
      if (!is.null(m$id_specimen)) {
        out <- d
        out$id_specimen <- as.integer(d[[m$id_specimen]])
        matched_rv(out)
        complete_rv(TRUE)
      } else {
        complete_rv(FALSE)
      }
    })

    shiny::observeEvent(input$run_analysis, {
      d <- data(); m <- mappings()
      shiny::req(d, m, pool_main())

      if (!is.null(m$id_specimen)) {
        # already auto-resolved
        shiny::showNotification(
          i18n()$t("id_specimen is mapped; collector lookup not needed."),
          type = "message"
        )
        return()
      }
      if (is.null(m$collector)) {
        shiny::showNotification(
          i18n()$t("No collector column mapped."),
          type = "warning"
        )
        return()
      }

      coll_vals <- unique(d[[m$collector]])
      coll_vals <- coll_vals[!is.na(coll_vals) & nzchar(as.character(coll_vals))]

      con <- pool_main()
      actual <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
      on.exit({
        if (inherits(con, "Pool") && !is.null(actual)) pool::poolReturn(actual)
      }, add = TRUE)

      db_coll <- DBI::dbGetQuery(
        actual,
        "SELECT id_table_colnam, colnam FROM table_colnam ORDER BY colnam"
      )

      res <- .match_collector_names(
        user_values = as.character(coll_vals),
        db_values   = db_coll$colnam,
        db_ids      = db_coll$id_table_colnam
      )
      coll_analysis(res)
      .rebuild_matched()
    })

    .rebuild_matched <- function() {
      d <- data(); m <- mappings(); a <- coll_analysis()
      if (is.null(d) || is.null(m) || is.null(a)) return()

      out <- d
      out$id_colnam <- sapply(as.character(d[[m$collector]]), function(v) {
        if (is.na(v) || !nzchar(v)) return(NA_integer_)
        if (v %in% names(a$exact)) return(as.integer(a$exact[[v]]))
        NA_integer_
      })
      matched_rv(out)
      complete_rv(length(a$unmatched) == 0 && length(a$fuzzy) == 0)
    }

    output$summary_ui <- shiny::renderUI({
      m <- mappings()
      if (!is.null(m) && !is.null(m$id_specimen)) {
        return(shiny::div(class = "alert alert-success",
                          shiny::icon("check-circle"), " ",
                          i18n()$t("id_specimen is mapped - lookup not needed. You can continue.")))
      }
      a <- coll_analysis()
      if (is.null(a)) return(NULL)
      shiny::fluidRow(
        shiny::column(4, shiny::div(class = "alert alert-success",
                                    shiny::h4(length(a$exact)),
                                    i18n()$t("Exact matches"))),
        shiny::column(4, shiny::div(class = "alert alert-warning",
                                    shiny::h4(length(a$fuzzy)),
                                    i18n()$t("Fuzzy (need confirmation)"))),
        shiny::column(4, shiny::div(
          class = if (length(a$unmatched) == 0) "alert alert-success" else "alert alert-danger",
          shiny::h4(length(a$unmatched)),
          i18n()$t("Unmatched")
        ))
      )
    })

    output$matching_ui <- shiny::renderUI({
      a <- coll_analysis()
      if (is.null(a)) return(NULL)

      shiny::tagList(
        if (length(a$fuzzy) > 0) {
          shiny::div(
            class = "alert alert-warning",
            shiny::h5(shiny::icon("question-circle"), " ",
                      i18n()$t("Fuzzy matches - confirm or skip")),
            lapply(seq_along(a$fuzzy), function(i) {
              user_val <- names(a$fuzzy)[i]
              sugg     <- a$fuzzy[[i]]
              labels <- sprintf("%s  (%.0f%%)", sugg$name,
                                100 * sugg$similarity)
              shiny::div(
                style = "margin: 8px 0; padding: 8px; background: white;",
                shiny::strong(user_val), " ", shiny::icon("arrow-right"), " ",
                shiny::selectizeInput(
                  ns(paste0("fz_", i)), label = NULL,
                  choices = c(setNames("", i18n()$t("-- Select --")),
                              setNames(sugg$id, labels)),
                  width = "420px",
                  options = list(placeholder = i18n()$t("Type to search..."))
                )
              )
            }),
            shiny::actionButton(ns("apply_fuzzy"),
                                shiny::tagList(shiny::icon("check"), " ",
                                               i18n()$t("Apply selected matches")),
                                class = "btn-warning")
          )
        },
        if (length(a$unmatched) > 0) {
          shiny::div(class = "alert alert-danger",
                     shiny::h5(shiny::icon("times-circle"), " ",
                               i18n()$t("Unmatched collectors")),
                     shiny::tags$ul(lapply(a$unmatched, function(v)
                       shiny::tags$li(shiny::tags$code(v)))),
                     shiny::p(i18n()$t("These must be added to table_colnam before continuing, or fix typos in the source file.")))
        }
      )
    })

    shiny::observeEvent(input$apply_fuzzy, {
      a <- coll_analysis()
      shiny::req(a, length(a$fuzzy) > 0)
      fzn <- names(a$fuzzy)
      still <- list()
      for (i in seq_along(fzn)) {
        sel <- input[[paste0("fz_", i)]]
        v <- fzn[i]
        if (!is.null(sel) && nzchar(sel)) {
          a$exact[[v]] <- as.integer(sel)
        } else {
          still[[v]] <- a$fuzzy[[v]]
        }
      }
      a$fuzzy <- still
      coll_analysis(a)
      .rebuild_matched()
    })

    list(
      matched_data = matched_rv,
      is_complete  = complete_rv
    )
  })
}


#' Match collector names against table_colnam
#'
#' Token-based Jaro-Winkler matching (same algorithm as
#' `.get_fuzzy_matches()` in `mod_lookup_matcher`, which is what
#' `launch_import_wizard()` uses). Handles multi-word collector names
#' like "Transect Cameroun" gracefully.
#'
#' @param user_values Character vector of user-provided collector strings.
#' @param db_values Character vector of `colnam` from `table_colnam`.
#' @param db_ids Integer vector of matching `id_table_colnam`.
#' @return list(exact = named list user_value -> id,
#'              fuzzy = named list user_value -> list(name, id, similarity)
#'                with ALL candidates ranked by similarity, descending),
#'              unmatched = character vector (only values that failed token-split))
#' @keywords internal
.match_collector_names <- function(user_values, db_values, db_ids) {
  exact <- list()
  fuzzy <- list()
  unmatched <- character(0)

  db_lower <- tolower(trimws(db_values))
  db_tokens_list <- strsplit(db_lower, "\\s+")

  for (val in user_values) {
    if (is.na(val) || !nzchar(val)) next
    val_lower <- tolower(trimws(val))

    # Exact (case/whitespace-insensitive)
    hit <- which(db_lower == val_lower)
    if (length(hit) > 0) {
      exact[[val]] <- db_ids[hit[1]]
      next
    }

    # Token-based Jaro-Winkler over ALL db entries, sorted descending.
    user_tokens <- unlist(strsplit(val_lower, "\\s+"))
    if (length(user_tokens) == 0) {
      unmatched <- c(unmatched, val)
      next
    }
    sims <- vapply(db_tokens_list, function(dbtok) {
      if (length(dbtok) == 0) return(0)
      m <- outer(user_tokens, dbtok, function(x, y) {
        1 - stringdist::stringdist(x, y, method = "jw")
      })
      mean(apply(m, 1, max))
    }, numeric(1))

    ord <- order(sims, decreasing = TRUE)
    fuzzy[[val]] <- list(
      name = db_values[ord],
      id   = db_ids[ord],
      similarity = sims[ord]
    )
  }

  list(exact = exact, fuzzy = fuzzy, unmatched = unmatched)
}
