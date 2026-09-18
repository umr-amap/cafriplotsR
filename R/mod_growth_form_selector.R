#' Growth Form Selector Module - UI
#'
#' UI component for selecting growth forms hierarchically
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_growth_form_selector_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("growth_form_ui"))
  )
}


#' Growth Form Selector Module - Server
#'
#' Server logic for hierarchical growth form selection
#'
#' @param id Module namespace ID
#' @param pool Reactive returning taxa database connection pool
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return List with:
#'   - growth_form_selections: Reactive list of selected growth form paths
#'   - reset: Function clearing every selection, for the next taxon. The
#'     selections survive their UI being destroyed on purpose, so the caller
#'     has to say when a taxon is finished.
#'   - basisofrecord: Reactive character
#'   - measurementremarks: Reactive character
#'   - is_valid: Reactive logical indicating if selections are complete
#'
#' @keywords internal
#' @export
mod_growth_form_selector_server <- function(id, pool, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      growth_form_cat = NULL,
      current_path = list(),
      all_paths = list(),
      level_1_selected = NULL,
      level_2_selected = NULL,
      level_3_selected = NULL,
      saved_basisofrecord = "",
      saved_measurementremarks = ""
    )

    cli::cli_alert_warning("MODULE INIT: Growth form selector module initialized")

    # traitlist row of one growth form level
    level_trait <- function(level) {
      growth_form_cat <- rv$growth_form_cat
      if (is.null(growth_form_cat)) return(NULL)
      growth_form_cat[growth_form_cat$trait == paste0("growth_form_level_", level), ,
                      drop = FALSE]
    }

    # Values offered at a level, under the choice made one level up
    level_choices <- function(level, parent) {
      trait_row <- level_trait(level)
      if (is.null(trait_row) || nrow(trait_row) == 0) return(character(0))
      .growth_form_children(parent, level, trait_row$factorlevels_list[[1]])
    }

    # A selectInput removed from the page keeps its last value on the server,
    # so a level 2 choice outlives a change of level 1. Only trust a value that
    # is still a child of the choice above it.
    selected_level_2 <- shiny::reactive({
      value <- input$level_2
      if (is.null(value) || !value %in% level_choices(2, input$level_1)) {
        return(NULL)
      }
      value
    })

    selected_level_3 <- shiny::reactive({
      value <- input$level_3
      if (is.null(value) || !value %in% level_choices(3, selected_level_2())) {
        return(NULL)
      }
      value
    })

    # Load growth form categories from database
    shiny::observe({
      shiny::req(pool())

      tryCatch({
        # traitlist lives in the main database (plots_transects), not the taxa
        # database (rainbio). Use pool_main from .db_env when available;
        # fall back to the supplied pool so the module still works in other
        # contexts where pool IS the main database.
        active_pool <- if (!is.null(.db_env$pool_main)) {
          .db_env$pool_main
        } else {
          pool()
        }

        # Get connection
        actual_con <- if (inherits(active_pool, "Pool")) {
          pool::poolCheckout(active_pool)
        } else {
          active_pool
        }

        on.exit({
          if (inherits(active_pool, "Pool") && !is.null(actual_con)) {
            pool::poolReturn(actual_con)
          }
        }, add = TRUE)

        # Query growth form traits
        growth_form_cat <- get_traitlist(actual_con)
        keep_cols <- intersect(c("id_trait", "trait", "traitdescription", "factorlevels"),
                               names(growth_form_cat))
        growth_form_cat <- growth_form_cat[, keep_cols, drop = FALSE]
        growth_form_cat <- growth_form_cat[grepl("growth_form", growth_form_cat$trait), , drop = FALSE]

        # Parse factor levels
        growth_form_cat$factorlevels_list <- lapply(growth_form_cat$factorlevels,
                                                    .parse_factorlevels)

        # Which value sits under which is no longer in traitlist; it comes from
        # .growth_form_hierarchy(). An accepted value that table cannot place is
        # never offered, so say so rather than hide it.
        for (level in 2:3) {
          accepted <- unlist(growth_form_cat$factorlevels_list[
            growth_form_cat$trait == paste0("growth_form_level_", level)])
          unplaced <- .growth_form_unplaced(level, accepted)
          if (length(unplaced) > 0) {
            message("Note: growth_form_level_", level, " values with no parent ",
                    "in .growth_form_hierarchy() are not offered: ",
                    paste(unplaced, collapse = ", "))
          }
        }

        rv$growth_form_cat <- growth_form_cat

      }, error = function(e) {
        cli::cli_alert_danger("Failed to load growth form categories: {e$message}")
      })
    })

    # Main UI
    output$growth_form_ui <- shiny::renderUI({
      shiny::req(rv$growth_form_cat)

      shiny::tagList(
        shiny::h5(i18n()$t("Growth Form Selection")),
        shiny::p(
          class = "text-muted",
          i18n()$t("Select growth forms hierarchically. You can add multiple growth forms.")
        ),

        # Level 1 selection
        shiny::wellPanel(
          shiny::h6(i18n()$t("Level 1: Primary Growth Form")),
          shiny::selectInput(
            ns("level_1"),
            i18n()$t("Select primary growth form"),
            choices = c(
              "Select..." = "",
              setNames(
                rv$growth_form_cat %>%
                  dplyr::filter(trait == "growth_form_level_1") %>%
                  dplyr::pull(factorlevels_list) %>%
                  .[[1]],
                rv$growth_form_cat %>%
                  dplyr::filter(trait == "growth_form_level_1") %>%
                  dplyr::pull(factorlevels_list) %>%
                  .[[1]]
              )
            )
          ),
          shiny::div(
            class = "text-muted small",
            shiny::HTML(rv$growth_form_cat %>%
              dplyr::filter(trait == "growth_form_level_1") %>%
              dplyr::pull(traitdescription))
          )
        ),

        # Level 2 selection (conditional)
        shiny::uiOutput(ns("level_2_ui")),

        # Level 3 selection (conditional)
        shiny::uiOutput(ns("level_3_ui")),

        # Add path button
        shiny::div(
          style = "margin-top: 20px;",
          shiny::actionButton(
            ns("btn_add_path"),
            i18n()$t("Add This Growth Form"),
            icon = shiny::icon("plus"),
            class = "btn-primary"
          )
        ),

        shiny::br(),

        # Display selected paths
        shiny::uiOutput(ns("selected_paths_ui")),

        shiny::hr(),

        # Basis of record
        shiny::h6(i18n()$t("Source Information")),
        shiny::wellPanel(
          shiny::selectInput(
            ns("basisofrecord"),
            i18n()$t("Basis of Record"),
            choices = c(
              "Select..." = "",
              "Living Specimen" = "LivingSpecimen",
              "Preserved Specimen" = "PreservedSpecimen",
              "Fossil Specimen" = "FossilSpecimen",
              "Literature Data" = "literatureData",
              "Trait Database" = "traitDatabase",
              "Expert Knowledge" = "expertKnowledge"
            )
          ),
          shiny::textInput(
            ns("measurementremarks"),
            i18n()$t("Measurement Remarks (optional)"),
            placeholder = i18n()$t("Additional notes or references")
          )
        )
      )
    })

    # Level 2 UI (depends on Level 1)
    output$level_2_ui <- shiny::renderUI({
      shiny::req(input$level_1)

      choices <- level_choices(2, input$level_1)
      if (length(choices) == 0) return(NULL)

      shiny::wellPanel(
        shiny::h6(i18n()$t("Level 2: Refine Growth Form")),
        shiny::selectInput(
          ns("level_2"),
          i18n()$t("Select specific form"),
          choices = c("Select..." = "", stats::setNames(choices, choices))
        ),
        shiny::div(
          class = "text-muted small",
          shiny::HTML(level_trait(2)$traitdescription[1])
        )
      )
    })

    # Level 3 UI (depends on Level 2)
    output$level_3_ui <- shiny::renderUI({
      shiny::req(selected_level_2())

      # e.g. epiphyte has no finer form
      choices <- level_choices(3, selected_level_2())
      if (length(choices) == 0) return(NULL)

      shiny::wellPanel(
        shiny::h6(i18n()$t("Level 3: Final Specification")),
        shiny::selectInput(
          ns("level_3"),
          i18n()$t("Select final form"),
          choices = c("Select..." = "", stats::setNames(choices, choices))
        ),
        shiny::div(
          class = "text-muted small",
          shiny::HTML(level_trait(3)$traitdescription[1])
        )
      )
    })

    # Add path to list
    shiny::observeEvent(input$btn_add_path, {
      shiny::req(input$level_1)

      # Build current path
      current_path <- list()

      # Level 1 (required)
      current_path[[1]] <- list(
        id_trait = level_trait(1)$id_trait[1],
        trait = "growth_form_level_1",
        value = input$level_1
      )

      # Level 2 (if selected, and still under level 1)
      if (!is.null(selected_level_2())) {
        current_path[[2]] <- list(
          id_trait = level_trait(2)$id_trait[1],
          trait = "growth_form_level_2",
          value = selected_level_2()
        )
      }

      # Level 3 (if selected, and still under level 2)
      if (!is.null(selected_level_3())) {
        current_path[[3]] <- list(
          id_trait = level_trait(3)$id_trait[1],
          trait = "growth_form_level_3",
          value = selected_level_3()
        )
      }

      # Add to all paths
      rv$all_paths[[length(rv$all_paths) + 1]] <- current_path

      # Reset selections
      shiny::updateSelectInput(session, "level_1", selected = "")
      shiny::updateSelectInput(session, "level_2", selected = "")
      shiny::updateSelectInput(session, "level_3", selected = "")

      shiny::showNotification(
        i18n()$t("Growth form added!"),
        type = "message",
        duration = 2
      )
    })

    # Display selected paths
    output$selected_paths_ui <- shiny::renderUI({
      if (length(rv$all_paths) == 0) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("No growth forms selected yet")
          )
        )
      }

      shiny::div(
        shiny::h6(i18n()$t("Selected Growth Forms:")),
        shiny::tags$ul(
          lapply(seq_along(rv$all_paths), function(i) {
            path <- rv$all_paths[[i]]
            path_str <- paste(sapply(path, function(x) x$value), collapse = " → ")

            shiny::tags$li(
              path_str,
              " ",
              shiny::actionLink(
                ns(paste0("remove_path_", i)),
                label = NULL,
                icon = shiny::icon("times"),
                style = "color: red;"
              )
            )
          })
        )
      )
    })

    # Handle path removal.
    #
    # One observer per position, created once. The list of positions grows as
    # growth forms are added, so this runs again on every change - but an
    # index that already has its observer must not get a second one: each
    # copy would fire on the same click and remove a further growth form.
    removers_registered <- character(0)

    shiny::observe({
      for (i in seq_along(rv$all_paths)) {
        key <- paste0("remove_path_", i)
        if (key %in% removers_registered) next
        removers_registered <<- c(removers_registered, key)

        local({
          idx <- i
          shiny::observeEvent(input[[paste0("remove_path_", idx)]], {
            # The list shrinks, and reset() empties it; a stale click must not
            # reach past its end.
            if (idx > length(rv$all_paths)) return()
            rv$all_paths[[idx]] <- NULL
            rv$all_paths <- rv$all_paths[!sapply(rv$all_paths, is.null)]
          }, ignoreInit = TRUE)
        })
      }
    })

    # Save basis of record to reactive value when it changes (only non-empty values)
    shiny::observeEvent(input$basisofrecord, {
      cli::cli_alert_info("Basis of record changed to: '{input$basisofrecord}'")
      # Only save non-empty values to prevent overwriting when UI is destroyed
      if (!is.null(input$basisofrecord) && input$basisofrecord != "") {
        rv$saved_basisofrecord <- input$basisofrecord
        cli::cli_alert_success("Saved basis of record: '{rv$saved_basisofrecord}'")
      } else {
        cli::cli_alert_info("Not saving empty value. Current saved value: '{rv$saved_basisofrecord}'")
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Save measurement remarks to reactive value when it changes
    shiny::observeEvent(input$measurementremarks, {
      cli::cli_alert_info("Measurement remarks changed to: '{input$measurementremarks}'")
      # Always save remarks (can be empty)
      rv$saved_measurementremarks <- input$measurementremarks
      cli::cli_alert_success("Saved measurement remarks: '{rv$saved_measurementremarks}'")
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Forget everything the user chose, for the next taxon.
    #
    # The saved values deliberately survive the UI being destroyed, so that
    # walking back and forth between steps does not lose them. That is also
    # why they outlive the taxon they were entered for unless something says
    # otherwise: this is what the caller says it with, once a taxon is done.
    reset <- function() {
      rv$all_paths <- list()
      rv$current_path <- list()
      rv$level_1_selected <- NULL
      rv$level_2_selected <- NULL
      rv$level_3_selected <- NULL
      rv$saved_basisofrecord <- ""
      rv$saved_measurementremarks <- ""

      # Blank the inputs too, or the next taxon opens on the previous
      # selection. Writing "" here is ignored by the observer above, which
      # only saves non-empty values, so the cleared state holds.
      shiny::updateSelectInput(session, "level_1", selected = "")
      shiny::updateSelectInput(session, "level_2", selected = "")
      shiny::updateSelectInput(session, "level_3", selected = "")
      shiny::updateSelectInput(session, "basisofrecord", selected = "")
      shiny::updateTextInput(session, "measurementremarks", value = "")

      cli::cli_alert_info("Growth form selector reset")
      invisible(NULL)
    }

    # Return reactive values (using saved values that persist across UI changes)
    return(list(
      growth_form_selections = shiny::reactive(rv$all_paths),
      reset = reset,
      basisofrecord = shiny::reactive({
        cli::cli_alert_info("MODULE RETURN: Returning basis of record: '{rv$saved_basisofrecord}'")
        rv$saved_basisofrecord
      }),
      measurementremarks = shiny::reactive({
        cli::cli_alert_info("MODULE RETURN: Returning measurement remarks: '{rv$saved_measurementremarks}'")
        rv$saved_measurementremarks
      }),
      is_valid = shiny::reactive({
        valid <- length(rv$all_paths) > 0 &&
          !is.null(rv$saved_basisofrecord) &&
          rv$saved_basisofrecord != ""
        cli::cli_alert_info("MODULE RETURN: Is valid = {valid} (paths: {length(rv$all_paths)}, basis: '{rv$saved_basisofrecord}')")
        valid
      })
    ))
  })
}
