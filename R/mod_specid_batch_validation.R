# Specimen Identification App - Batch Step 4: Validation
#
# Verifies each row before updates are applied:
#   - id_specimen (if mapped) exists in `specimens`
#   - (id_colnam, colnbr [, suffix]) resolves to exactly one specimen
#   - idtax_n (if mapped) exists in `table_taxa`
#   - detd/detm/dety form a real calendar date (if provided)
#
# Output: validated_data adds columns:
#   .resolved_id_specimen, .row_valid, .row_issues

#' Batch validation UI
#' @param id Module id
#' @param i18n Translator
#' @keywords internal
#' @export
mod_specid_batch_validation_ui <- function(id, i18n) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("check-double"), " ", i18n$t("Step 4: Validate rows")),
    shiny::p(i18n$t("Each row is checked: target specimen exists, idtax_n is valid, dates parse, etc."),
             style = "color: #6c757d;"),
    shiny::actionButton(ns("run"),
                        shiny::tagList(shiny::icon("play"), " ", i18n$t("Run validation")),
                        class = "btn-primary"),
    shiny::br(), shiny::br(),
    shiny::uiOutput(ns("summary")),
    shiny::uiOutput(ns("skip_controls")),
    DT::DTOutput(ns("issues_tbl"))
  )
}

#' Batch validation server
#' @param id Module id
#' @param matched_data Reactive matched data (with id_colnam if applicable)
#' @param mappings Reactive mapping list
#' @param pool_main Reactive main DB pool
#' @param pool_taxa Reactive taxa DB pool
#' @param i18n Reactive translator
#' @return list(validated_data, is_valid)
#' @keywords internal
#' @export
mod_specid_batch_validation_server <- function(id, matched_data, mappings,
                                               pool_main, pool_taxa, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    validated_rv <- shiny::reactiveVal(NULL)
    all_valid_rv <- shiny::reactiveVal(FALSE)   # TRUE only when no issues

    shiny::observeEvent(input$run, {
      d <- matched_data(); m <- mappings()
      shiny::req(d, m, pool_main())

      n <- nrow(d)
      issues   <- rep("", n)
      resolved <- rep(NA_integer_, n)

      # --- Resolve specimens ---
      if (!is.null(m$id_specimen)) {
        resolved <- as.integer(d[[m$id_specimen]])
        # check existence
        con  <- pool_main()
        actual <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
        on.exit({
          if (inherits(con, "Pool") && !is.null(actual)) pool::poolReturn(actual)
        }, add = TRUE)
        ids_to_check <- unique(resolved[!is.na(resolved)])
        if (length(ids_to_check) > 0) {
          q <- DBI::dbGetQuery(
            actual,
            sprintf("SELECT id_specimen FROM specimens WHERE id_specimen IN (%s)",
                    paste(ids_to_check, collapse = ","))
          )
          found <- q$id_specimen
          missing_rows <- which(!is.na(resolved) & !(resolved %in% found))
          if (length(missing_rows) > 0) {
            issues[missing_rows] <- paste(issues[missing_rows],
                                          i18n()$t("id_specimen not found in specimens table;"))
          }
        }
        missing_rows2 <- which(is.na(resolved))
        if (length(missing_rows2) > 0) {
          issues[missing_rows2] <- paste(issues[missing_rows2],
                                         i18n()$t("id_specimen is missing/NA;"))
        }
      } else {
        # Resolve by id_colnam + colnbr (+ optional suffix)
        if (is.null(d$id_colnam) || is.null(m$colnbr)) {
          shiny::showNotification(
            i18n()$t("Missing id_colnam or colnbr to resolve specimens."),
            type = "error"
          )
          return()
        }
        con  <- pool_main()
        actual <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
        on.exit({
          if (inherits(con, "Pool") && !is.null(actual)) pool::poolReturn(actual)
        }, add = TRUE)

        # Pull all relevant specimens at once
        ids_coll <- unique(d$id_colnam[!is.na(d$id_colnam)])
        if (length(ids_coll) > 0) {
          q <- DBI::dbGetQuery(
            actual,
            sprintf(
              "SELECT id_specimen, id_colnam, colnbr, suffix FROM specimens WHERE id_colnam IN (%s)",
              paste(ids_coll, collapse = ",")
            )
          )
        } else {
          q <- data.frame(id_specimen = integer(0), id_colnam = integer(0),
                          colnbr = character(0), suffix = character(0))
        }

        use_suffix <- !is.null(m$suffix)
        for (i in seq_len(n)) {
          ic <- d$id_colnam[i]
          raw_cn <- d[[m$colnbr]][i]
          if (is.na(ic)) {
            issues[i] <- paste(issues[i], i18n()$t("collector unmatched;"))
            next
          }
          if (is.na(raw_cn) || !nzchar(as.character(raw_cn))) {
            issues[i] <- paste(issues[i], i18n()$t("colnbr missing;"))
            next
          }

          # Parse "1234A" / "B567" / "1234bis" into digits + leftover chars.
          cn_str  <- as.character(raw_cn)
          cn_num  <- gsub("[^0-9]", "", cn_str)        # digits only
          cn_rest <- trimws(gsub("[0-9]+", "", cn_str)) # non-digits
          if (!nzchar(cn_num)) {
            issues[i] <- paste(issues[i],
                               i18n()$t("colnbr has no numeric part;"))
            next
          }

          # Effective suffix: user-mapped suffix takes precedence; otherwise
          # use leftover non-digit chars from colnbr.
          if (use_suffix) {
            sf <- d[[m$suffix]][i]
            sf_str <- if (is.na(sf)) NA_character_ else as.character(sf)
          } else if (nzchar(cn_rest)) {
            sf_str <- cn_rest
          } else {
            sf_str <- NA_character_
          }

          # Match colnbr by integer value (DB stores numeric).
          db_colnbr_num <- suppressWarnings(as.integer(q$colnbr))
          target_num    <- suppressWarnings(as.integer(cn_num))
          rows <- q[!is.na(db_colnbr_num) &
                      q$id_colnam == ic &
                      db_colnbr_num == target_num, , drop = FALSE]

          if (use_suffix || nzchar(cn_rest)) {
            rows <- rows[
              (is.na(rows$suffix) & is.na(sf_str)) |
                (!is.na(rows$suffix) & !is.na(sf_str) &
                   tolower(trimws(rows$suffix)) == tolower(trimws(sf_str))),
              ,
              drop = FALSE
            ]
          }
          if (nrow(rows) == 0) {
            issues[i] <- paste(issues[i], i18n()$t("specimen not found;"))
          } else if (nrow(rows) > 1) {
            issues[i] <- paste(issues[i],
                               sprintf(i18n()$t("%d specimens match (ambiguous);"),
                                       nrow(rows)))
          } else {
            resolved[i] <- as.integer(rows$id_specimen[1])
          }
        }
      }

      # --- Check idtax_n ---
      if (!is.null(m$idtax_n)) {
        idtax_vals <- suppressWarnings(as.integer(d[[m$idtax_n]]))
        bad_num <- is.na(idtax_vals) & !is.na(d[[m$idtax_n]]) &
                   nzchar(as.character(d[[m$idtax_n]]))
        if (any(bad_num)) {
          issues[bad_num] <- paste(issues[bad_num],
                                   i18n()$t("idtax_n not numeric;"))
        }
        # Check existence in taxa DB
        to_check <- unique(idtax_vals[!is.na(idtax_vals)])
        if (length(to_check) > 0 && !is.null(pool_taxa()) &&
            inherits(try(pool_taxa(), silent = TRUE), "Pool")) {
          ctaxa <- pool_taxa()
          actaxa <- if (inherits(ctaxa, "Pool")) pool::poolCheckout(ctaxa) else ctaxa
          on.exit({
            if (inherits(ctaxa, "Pool") && !is.null(actaxa))
              pool::poolReturn(actaxa)
          }, add = TRUE)
          tryCatch({
            q <- DBI::dbGetQuery(
              actaxa,
              sprintf("SELECT idtax_n FROM table_taxa WHERE idtax_n IN (%s)",
                      paste(to_check, collapse = ","))
            )
            found <- q$idtax_n
            missing <- which(!is.na(idtax_vals) & !(idtax_vals %in% found))
            if (length(missing) > 0) {
              issues[missing] <- paste(issues[missing],
                                       i18n()$t("idtax_n not found in table_taxa;"))
            }
          }, error = function(e) {
            shiny::showNotification(paste(i18n()$t("Could not verify idtax_n:"),
                                          e$message), type = "warning")
          })
        }
      }

      # --- Check determination date ---
      get_int <- function(col) {
        if (is.null(col)) return(rep(NA_integer_, n))
        suppressWarnings(as.integer(d[[col]]))
      }
      dy <- get_int(m$dety); dm <- get_int(m$detm); dd <- get_int(m$detd)
      for (i in seq_len(n)) {
        # only check if any of them is non-NA
        if (any(!is.na(c(dy[i], dm[i], dd[i])))) {
          y <- dy[i]; mo <- dm[i]; da <- dd[i]
          if (!is.na(mo) && (mo < 1 || mo > 12)) {
            issues[i] <- paste(issues[i], i18n()$t("detm out of 1-12;"))
          }
          if (!is.na(da) && (da < 1 || da > 31)) {
            issues[i] <- paste(issues[i], i18n()$t("detd out of 1-31;"))
          }
          if (!is.na(y) && (y < 1700 || y > 2200)) {
            issues[i] <- paste(issues[i], i18n()$t("dety unrealistic;"))
          }
          if (!is.na(y) && !is.na(mo) && !is.na(da)) {
            chk <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, mo, da)))
            if (is.na(chk)) {
              issues[i] <- paste(issues[i], i18n()$t("invalid det date;"))
            }
          }
        }
      }

      out <- d
      out$.resolved_id_specimen <- resolved
      out$.row_issues <- trimws(issues)
      out$.row_valid  <- nchar(out$.row_issues) == 0

      validated_rv(out)
      all_valid_rv(all(out$.row_valid))
    })

    # Skip-unmatched controls + download (only shown after validation ran
    # and there are unmatched rows).
    output$skip_controls <- shiny::renderUI({
      v <- validated_rv()
      if (is.null(v)) return(NULL)
      n_bad <- sum(!v$.row_valid)
      if (n_bad == 0) return(NULL)
      shiny::div(
        style = "margin-bottom: 12px; padding: 12px; background: #fff3cd; border-radius: 6px;",
        shiny::checkboxInput(
          ns("skip_unmatched"),
          label = sprintf(
            i18n()$t("Skip %d row(s) with issues and continue with valid rows only"),
            n_bad),
          value = FALSE
        ),
        shiny::downloadButton(
          ns("dl_unmatched"),
          label = i18n()$t("Download unmatched rows (CSV)"),
          class = "btn-secondary"
        )
      )
    })

    # is_valid: TRUE if everything valid OR user opted to skip
    is_valid_reactive <- shiny::reactive({
      isTRUE(all_valid_rv()) || isTRUE(input$skip_unmatched)
    })

    output$dl_unmatched <- shiny::downloadHandler(
      filename = function() {
        sprintf("specid_unmatched_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        v <- validated_rv()
        if (is.null(v)) {
          utils::write.csv(data.frame(), file, row.names = FALSE)
          return()
        }
        bad <- v[!v$.row_valid, , drop = FALSE]
        utils::write.csv(bad, file, row.names = FALSE, na = "")
      }
    )

    output$summary <- shiny::renderUI({
      v <- validated_rv()
      if (is.null(v)) return(NULL)
      n_ok  <- sum(v$.row_valid)
      n_bad <- nrow(v) - n_ok
      shiny::fluidRow(
        shiny::column(6, shiny::div(class = "alert alert-success",
                                    shiny::h4(n_ok),
                                    i18n()$t("Valid rows"))),
        shiny::column(6, shiny::div(
          class = if (n_bad == 0) "alert alert-success" else "alert alert-danger",
          shiny::h4(n_bad),
          i18n()$t("Rows with issues")
        ))
      )
    })

    output$issues_tbl <- DT::renderDT({
      v <- validated_rv()
      shiny::req(v)
      bad <- v[!v$.row_valid, , drop = FALSE]
      if (nrow(bad) == 0) {
        return(DT::datatable(data.frame(message = i18n()$t("No issues - all rows valid.")),
                             options = list(dom = "t"), rownames = FALSE))
      }
      keep <- intersect(c(".resolved_id_specimen", ".row_issues",
                          mappings()$id_specimen, mappings()$collector,
                          mappings()$colnbr, mappings()$idtax_n),
                        names(bad))
      DT::datatable(bad[, keep, drop = FALSE],
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    list(
      validated_data = validated_rv,
      is_valid       = is_valid_reactive
    )
  })
}
