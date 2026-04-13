#' Taxa Update Module - UI
#'
#' UI component for updating existing taxonomic records
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_taxa_update_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("update_ui"))
  )
}

#' Taxa Update Module - Server
#'
#' Server logic for updating taxonomic records
#'
#' @param id Module namespace ID
#' @param pool Reactive returning taxa database connection pool
#' @param selected_taxon Reactive returning selected taxon data from search module
#' @param has_write_permission Reactive returning TRUE if user can write
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL
#'
#' @keywords internal
#' @export
mod_taxa_update_server <- function(id, pool, selected_taxon, has_write_permission, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      modified_fields = list(),
      show_form = FALSE,
      cascade_descendants = NULL,
      show_cascade_warning = FALSE
    )

    # Main UI
    output$update_ui <- shiny::renderUI({
      if (!has_write_permission()) {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("lock"),
            " ",
            i18n()$t("Write Access:"),
            " ",
            i18n()$t("Only users with INSERT privileges can modify data")
          )
        )
      }

      if (is.null(selected_taxon()) || nrow(selected_taxon()) == 0) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("Please select a taxon from the Browse & Search tab first")
          )
        )
      }

      shiny::tagList(
        shiny::h4(i18n()$t("Update Existing Taxon")),

        # Show selected taxon
        shiny::wellPanel(
          style = "background-color: #e7f3ff;",
          shiny::h5(i18n()$t("Selected Taxon")),
          shiny::fluidRow(
            shiny::column(
              6,
              {
                taxon <- selected_taxon()
                na_display <- function(x) if (is.null(x) || is.na(x)) "N/A" else as.character(x)
                shiny::tagList(
                  shiny::strong("ID:"), " ", taxon$idtax_n, shiny::br(),
                  shiny::strong(i18n()$t("Family:")), " ", na_display(taxon$tax_fam), shiny::br(),
                  shiny::strong(i18n()$t("Genus:")), " ", na_display(taxon$tax_gen), shiny::br(),
                  shiny::strong(i18n()$t("Species:")), " ", na_display(taxon$tax_esp)
                )
              }
            ),
            shiny::column(
              6,
              {
                taxon <- selected_taxon()
                na_display <- function(x) if (is.null(x) || is.na(x)) "N/A" else as.character(x)
                shiny::tagList(
                  shiny::strong(i18n()$t("Order:")), " ", na_display(taxon$tax_order), shiny::br(),
                  shiny::strong(i18n()$t("Class:")), " ", na_display(taxon$tax_famclass), shiny::br(),
                  shiny::strong(i18n()$t("Morphotaxon:")), " ",
                  if (isTRUE(taxon$morpho_species)) i18n()$t("Yes") else i18n()$t("No"), shiny::br()
                )
              },
              if (!is.null(selected_taxon()$idtax_good_n) && !is.na(selected_taxon()$idtax_good_n)) {
                shiny::tagList(
                  shiny::strong(i18n()$t("Status:")),
                  shiny::span(
                    class = "synonym-indicator",
                    " ",
                    i18n()$t("Synonym"),
                    " (→ ID: ", selected_taxon()$idtax_good_n, ")"
                  )
                )
              } else {
                shiny::tagList(
                  shiny::strong(i18n()$t("Status:")),
                  " ",
                  i18n()$t("Accepted")
                )
              }
            )
          ),
          shiny::hr(),
          shiny::actionButton(
            ns("btn_start_edit"),
            i18n()$t("Edit This Taxon"),
            icon = shiny::icon("edit"),
            class = "btn-primary"
          )
        ),

        # Edit form (conditionally shown)
        shiny::conditionalPanel(
          condition = "output.show_form == true",
          ns = ns,
          shiny::wellPanel(
            shiny::h5(i18n()$t("Edit Taxonomic Fields")),
            shiny::p(
              class = "text-muted",
              i18n()$t("Modify the fields you want to update. Leave others unchanged.")
            ),

            shiny::fluidRow(
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_gen"),
                  i18n()$t("Genus"),
                  value = ""
                )
              ),
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_esp"),
                  i18n()$t("Species epithet"),
                  value = ""
                )
              ),
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_fam"),
                  i18n()$t("Family"),
                  value = ""
                )
              )
            ),

            shiny::fluidRow(
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_order"),
                  i18n()$t("Order"),
                  value = ""
                )
              ),
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_famclass"),
                  i18n()$t("Class"),
                  value = ""
                )
              )
            ),

            shiny::h6(i18n()$t("Infraspecific (if applicable)")),
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::selectInput(
                  ns("new_tax_rank1"),
                  i18n()$t("Infraspecific rank"),
                  choices = c("None" = "", "var." = "var.", "subsp." = "subsp.", "f." = "f."),
                  selected = ""
                )
              ),
              shiny::column(
                4,
                shiny::textInput(
                  ns("new_tax_name1"),
                  i18n()$t("Infraspecific name"),
                  value = ""
                )
              )
            ),

            shiny::h6(i18n()$t("Other attributes")),
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::checkboxInput(
                  ns("new_morpho_species"),
                  i18n()$t("Morphotaxon"),
                  value = FALSE
                )
              )
            ),

            shiny::div(
              id = ns("modified_fields_display"),
              shiny::uiOutput(ns("modified_fields_ui"))
            ),

            shiny::hr(),

            shiny::fluidRow(
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_cancel"),
                  i18n()$t("Cancel"),
                  icon = shiny::icon("times"),
                  class = "btn-secondary btn-block"
                )
              ),
              shiny::column(
                6,
                shiny::actionButton(
                  ns("btn_submit_update"),
                  i18n()$t("Submit Changes"),
                  icon = shiny::icon("save"),
                  class = "btn-success btn-block"
                )
              )
            )
          )
        )
      )
    })

    # Output for conditional panel
    output$show_form <- shiny::reactive({
      rv$show_form
    })
    shiny::outputOptions(output, "show_form", suspendWhenHidden = FALSE)

    # Start edit
    shiny::observeEvent(input$btn_start_edit, {
      taxon <- selected_taxon()

      # Helper to handle both NULL and NA
      na_to_empty <- function(x) {
        if (is.null(x) || is.na(x)) "" else as.character(x)
      }

      # Prefill form with current values
      shiny::updateTextInput(session, "new_tax_gen", value = na_to_empty(taxon$tax_gen))
      shiny::updateTextInput(session, "new_tax_esp", value = na_to_empty(taxon$tax_esp))
      shiny::updateTextInput(session, "new_tax_fam", value = na_to_empty(taxon$tax_fam))
      shiny::updateTextInput(session, "new_tax_order", value = na_to_empty(taxon$tax_order))
      shiny::updateTextInput(session, "new_tax_famclass", value = na_to_empty(taxon$tax_famclass))
      shiny::updateSelectInput(session, "new_tax_rank1", selected = na_to_empty(taxon$tax_rank1))
      shiny::updateTextInput(session, "new_tax_name1", value = na_to_empty(taxon$tax_name1))
      shiny::updateCheckboxInput(
        session, "new_morpho_species",
        value = isTRUE(taxon$morpho_species)
      )

      rv$show_form <- TRUE
    })

    # Cancel edit
    shiny::observeEvent(input$btn_cancel, {
      rv$show_form <- FALSE
      rv$modified_fields <- list()
    })

    # Track field changes
    shiny::observe({
      shiny::req(rv$show_form)
      taxon <- selected_taxon()

      # Helper to handle both NULL and NA
      na_to_empty <- function(x) {
        if (is.null(x) || is.na(x)) "" else as.character(x)
      }

      modified <- list()

      # Check each field for changes
      if (!is.null(input$new_tax_gen) && trimws(input$new_tax_gen) != na_to_empty(taxon$tax_gen)) {
        modified$tax_gen <- list(old = taxon$tax_gen, new = trimws(input$new_tax_gen))
      }
      if (!is.null(input$new_tax_esp) && trimws(input$new_tax_esp) != na_to_empty(taxon$tax_esp)) {
        modified$tax_esp <- list(old = taxon$tax_esp, new = trimws(input$new_tax_esp))
      }
      if (!is.null(input$new_tax_fam) && trimws(input$new_tax_fam) != na_to_empty(taxon$tax_fam)) {
        modified$tax_fam <- list(old = taxon$tax_fam, new = trimws(input$new_tax_fam))
      }
      if (!is.null(input$new_tax_order) && trimws(input$new_tax_order) != na_to_empty(taxon$tax_order)) {
        modified$tax_order <- list(old = taxon$tax_order, new = trimws(input$new_tax_order))
      }
      if (!is.null(input$new_tax_famclass) && trimws(input$new_tax_famclass) != na_to_empty(taxon$tax_famclass)) {
        modified$tax_famclass <- list(old = taxon$tax_famclass, new = trimws(input$new_tax_famclass))
      }
      if (!is.null(input$new_tax_rank1) && input$new_tax_rank1 != na_to_empty(taxon$tax_rank1)) {
        modified$tax_rank1 <- list(old = taxon$tax_rank1, new = input$new_tax_rank1)
      }
      if (!is.null(input$new_tax_name1) && trimws(input$new_tax_name1) != na_to_empty(taxon$tax_name1)) {
        modified$tax_name1 <- list(old = taxon$tax_name1, new = trimws(input$new_tax_name1))
      }
      if (!is.null(input$new_morpho_species) && !identical(input$new_morpho_species, isTRUE(taxon$morpho_species))) {
        modified$morpho_species <- list(old = isTRUE(taxon$morpho_species), new = input$new_morpho_species)
      }

      rv$modified_fields <- modified
    })

    # Display modified fields
    output$modified_fields_ui <- shiny::renderUI({
      if (length(rv$modified_fields) == 0) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("No changes yet - modify fields above to see changes")
          )
        )
      }

      shiny::div(
        class = "alert alert-warning",
        shiny::h6(i18n()$t("Modified Fields:")),
        shiny::tags$ul(
          lapply(names(rv$modified_fields), function(field) {
            change <- rv$modified_fields[[field]]
            shiny::tags$li(
              shiny::strong(field, ":"),
              " ",
              shiny::code(if (is.null(change$old)) "NULL" else as.character(change$old)),
              " → ",
              shiny::code(if (is.character(change$new) && change$new == "") "NULL" else as.character(change$new))
            )
          })
        )
      )
    })

    # Submit update - check for cascade first
    shiny::observeEvent(input$btn_submit_update, {
      if (length(rv$modified_fields) == 0) {
        shiny::showNotification(
          i18n()$t("No changes to submit"),
          type = "warning"
        )
        return()
      }

      taxon <- selected_taxon()

      # Check if any upper taxonomic fields are being changed
      upper_fields_changed <- c("tax_gen", "tax_fam", "tax_order", "tax_famclass")
      has_upper_change <- any(upper_fields_changed %in% names(rv$modified_fields))

      if (has_upper_change) {
        # Find descendants via id_parent
        cli::cli_alert_info("Checking for descendant taxa...")

        pool_conn <- pool()
        actual_con <- pool::poolCheckout(pool_conn)
        on.exit(pool::poolReturn(actual_con), add = TRUE)

        # Recursively find all descendants
        descendants <- data.frame()
        current_parents <- taxon$idtax_n
        iteration <- 0
        max_iterations <- 10

        while (length(current_parents) > 0 && iteration < max_iterations) {
          iteration <- iteration + 1

          children <- DBI::dbGetQuery(actual_con, sprintf(
            "SELECT idtax_n, tax_gen, tax_esp, tax_fam, tax_order, tax_famclass, tax_level, id_parent
             FROM table_taxa
             WHERE id_parent IN (%s)",
            paste(current_parents, collapse = ", ")
          ))

          if (nrow(children) == 0) break

          if (nrow(descendants) == 0) {
            descendants <- children
          } else {
            descendants <- rbind(descendants, children)
          }

          current_parents <- children$idtax_n
        }

        if (nrow(descendants) > 0) {
          cli::cli_alert_warning("Found {nrow(descendants)} descendant taxa that will be updated")
          rv$cascade_descendants <- descendants
          rv$show_cascade_warning <- TRUE
          return()  # Wait for user confirmation
        }
      }

      # No descendants or no upper field changes - proceed directly
      .perform_update(rv, taxon, pool(), i18n)
    })

    # Show cascade warning modal
    shiny::observeEvent(rv$show_cascade_warning, {
      if (rv$show_cascade_warning) {
        shiny::showModal(
          shiny::modalDialog(
            title = i18n()$t("Warning: Cascade Update"),
            size = "l",

            shiny::div(
              class = "alert alert-warning",
              shiny::h5(
                shiny::icon("exclamation-triangle"),
                " ",
                i18n()$t("This change will affect descendant taxa")
              ),
              shiny::hr(),
              shiny::p(
                sprintf(
                  i18n()$t("Modifying upper taxonomic fields will cascade to %d descendant taxon/taxa."),
                  nrow(rv$cascade_descendants)
                )
              ),
              shiny::p(i18n()$t("The following taxa will have their flat taxonomic columns updated to maintain consistency:"))
            ),

            shiny::div(
              style = "max-height: 300px; overflow-y: auto;",
              shiny::tags$table(
                class = "table table-sm table-striped",
                shiny::tags$thead(
                  shiny::tags$tr(
                    shiny::tags$th("ID"),
                    shiny::tags$th(i18n()$t("Level")),
                    shiny::tags$th(i18n()$t("Genus")),
                    shiny::tags$th(i18n()$t("Species")),
                    shiny::tags$th(i18n()$t("Family"))
                  )
                ),
                shiny::tags$tbody(
                  lapply(1:min(nrow(rv$cascade_descendants), 50), function(i) {
                    desc <- rv$cascade_descendants[i, ]
                    shiny::tags$tr(
                      shiny::tags$td(desc$idtax_n),
                      shiny::tags$td(desc$tax_level),
                      shiny::tags$td(if (is.na(desc$tax_gen)) "—" else desc$tax_gen),
                      shiny::tags$td(if (is.na(desc$tax_esp)) "—" else desc$tax_esp),
                      shiny::tags$td(if (is.na(desc$tax_fam)) "—" else desc$tax_fam)
                    )
                  })
                )
              ),
              if (nrow(rv$cascade_descendants) > 50) {
                shiny::p(
                  class = "text-muted",
                  sprintf(i18n()$t("... and %d more taxa"), nrow(rv$cascade_descendants) - 50)
                )
              }
            ),

            footer = shiny::tagList(
              shiny::actionButton(
                ns("btn_cancel_cascade"),
                i18n()$t("Cancel"),
                icon = shiny::icon("times"),
                class = "btn-secondary"
              ),
              shiny::actionButton(
                ns("btn_confirm_cascade"),
                sprintf(i18n()$t("Confirm - Update %d Taxa"), nrow(rv$cascade_descendants) + 1),
                icon = shiny::icon("check"),
                class = "btn-warning"
              )
            )
          )
        )
        rv$show_cascade_warning <- FALSE
      }
    })

    # Cancel cascade
    shiny::observeEvent(input$btn_cancel_cascade, {
      shiny::removeModal()
      rv$cascade_descendants <- NULL
    })

    # Confirm cascade update
    shiny::observeEvent(input$btn_confirm_cascade, {
      shiny::removeModal()
      taxon <- selected_taxon()
      .perform_cascade_update(rv, taxon, pool(), i18n)
    })

    return(NULL)
  })
}

# Helper function to perform simple update (no cascade)
.perform_update <- function(rv, taxon, pool_conn, i18n) {
  shiny::withProgress({
    tryCatch({
      cli::cli_alert_info("Updating taxon ID {taxon$idtax_n}...")

      # Prepare parameters for update_dico_name
      update_params <- list(
        id_searched = taxon$idtax_n,
        ask_before_update = FALSE,
        add_backup = TRUE,
        show_results = FALSE,
        con = pool_conn
      )

      # Add only modified fields
      if (!is.null(rv$modified_fields$tax_gen)) {
        update_params$new_tax_gen <- rv$modified_fields$tax_gen$new
      }
      if (!is.null(rv$modified_fields$tax_esp)) {
        update_params$new_tax_esp <- if (rv$modified_fields$tax_esp$new == "") NA else rv$modified_fields$tax_esp$new
      }
      if (!is.null(rv$modified_fields$tax_fam)) {
        update_params$new_tax_fam <- rv$modified_fields$tax_fam$new
      }
      if (!is.null(rv$modified_fields$tax_order)) {
        update_params$new_tax_order <- if (rv$modified_fields$tax_order$new == "") NA else rv$modified_fields$tax_order$new
      }
      if (!is.null(rv$modified_fields$tax_famclass)) {
        update_params$new_tax_famclass <- if (rv$modified_fields$tax_famclass$new == "") NA else rv$modified_fields$tax_famclass$new
      }
      if (!is.null(rv$modified_fields$tax_rank1)) {
        update_params$new_tax_rank1 <- if (rv$modified_fields$tax_rank1$new == "") NA else rv$modified_fields$tax_rank1$new
      }
      if (!is.null(rv$modified_fields$tax_name1)) {
        update_params$new_tax_name1 <- if (rv$modified_fields$tax_name1$new == "") NA else rv$modified_fields$tax_name1$new
      }

      # Call update_dico_name only if there are fields it handles (it errors if nothing to update)
      dico_fields <- c("tax_gen", "tax_esp", "tax_fam", "tax_order", "tax_famclass", "tax_rank1", "tax_name1")
      has_dico_changes <- any(dico_fields %in% names(rv$modified_fields))
      if (has_dico_changes) {
        do.call(update_dico_name, update_params)
      }

      # Handle morpho_species separately via direct SQL (not supported by update_dico_name)
      if (!is.null(rv$modified_fields$morpho_species)) {
        actual_con <- if (inherits(pool_conn, "Pool")) pool::poolCheckout(pool_conn) else pool_conn
        on.exit({
          if (inherits(pool_conn, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
        }, add = TRUE)
        DBI::dbExecute(
          actual_con,
          "UPDATE table_taxa SET morpho_species = $1 WHERE idtax_n = $2",
          params = list(rv$modified_fields$morpho_species$new, taxon$idtax_n)
        )
        cli::cli_alert_success("Updated morpho_species to {rv$modified_fields$morpho_species$new}")
      }

      shiny::showNotification(
        i18n()$t("Taxon updated successfully!"),
        type = "message",
        duration = 5
      )

      # Reset form
      rv$show_form <- FALSE
      rv$modified_fields <- list()

    }, error = function(e) {
      cli::cli_alert_danger("Failed to update taxon: {e$message}")
      shiny::showNotification(
        paste(i18n()$t("Error updating taxon:"), e$message),
        type = "error",
        duration = 10
      )
    })
  }, message = i18n()$t("Updating taxon..."))
}

# Helper function to perform cascade update
.perform_cascade_update <- function(rv, taxon, pool_conn, i18n) {
  shiny::withProgress({
    tryCatch({
      cli::cli_alert_info("Performing cascade update for taxon ID {taxon$idtax_n} and {nrow(rv$cascade_descendants)} descendants...")

      actual_con <- pool::poolCheckout(pool_conn)
      on.exit(pool::poolReturn(actual_con), add = TRUE)

      # Start transaction
      DBI::dbExecute(actual_con, "BEGIN;")

      # STEP 1: Find or create upper taxon entries and update id_parent
      new_parent_id <- .ensure_parent_taxon_exists(rv, taxon, actual_con)

      # STEP 2: Update the parent taxon
      update_params <- list(
        id_searched = taxon$idtax_n,
        ask_before_update = FALSE,
        add_backup = TRUE,
        show_results = FALSE,
        con = pool_conn
      )

      # Add modified fields
      if (!is.null(rv$modified_fields$tax_gen)) update_params$new_tax_gen <- rv$modified_fields$tax_gen$new
      if (!is.null(rv$modified_fields$tax_esp)) update_params$new_tax_esp <- if (rv$modified_fields$tax_esp$new == "") NA else rv$modified_fields$tax_esp$new
      if (!is.null(rv$modified_fields$tax_fam)) update_params$new_tax_fam <- rv$modified_fields$tax_fam$new
      if (!is.null(rv$modified_fields$tax_order)) update_params$new_tax_order <- if (rv$modified_fields$tax_order$new == "") NA else rv$modified_fields$tax_order$new
      if (!is.null(rv$modified_fields$tax_famclass)) update_params$new_tax_famclass <- if (rv$modified_fields$tax_famclass$new == "") NA else rv$modified_fields$tax_famclass$new
      if (!is.null(rv$modified_fields$tax_rank1)) update_params$new_tax_rank1 <- if (rv$modified_fields$tax_rank1$new == "") NA else rv$modified_fields$tax_rank1$new
      if (!is.null(rv$modified_fields$tax_name1)) update_params$new_tax_name1 <- if (rv$modified_fields$tax_name1$new == "") NA else rv$modified_fields$tax_name1$new

      # Call update_dico_name only if there are fields it handles
      dico_fields <- c("tax_gen", "tax_esp", "tax_fam", "tax_order", "tax_famclass", "tax_rank1", "tax_name1")
      has_dico_changes <- any(dico_fields %in% names(rv$modified_fields))
      if (has_dico_changes) {
        do.call(update_dico_name, update_params)
      }

      # Handle morpho_species separately via direct SQL (not supported by update_dico_name)
      if (!is.null(rv$modified_fields$morpho_species)) {
        DBI::dbExecute(
          actual_con,
          "UPDATE table_taxa SET morpho_species = $1 WHERE idtax_n = $2",
          params = list(rv$modified_fields$morpho_species$new, taxon$idtax_n)
        )
        cli::cli_alert_success("Updated morpho_species to {rv$modified_fields$morpho_species$new}")
      }

      # STEP 3: Update id_parent if we found/created a new parent
      if (!is.null(new_parent_id)) {
        sql_update_parent <- sprintf(
          "UPDATE table_taxa SET id_parent = %d WHERE idtax_n = %d",
          new_parent_id,
          taxon$idtax_n
        )
        DBI::dbExecute(actual_con, sql_update_parent)
        cli::cli_alert_success("Updated id_parent to {new_parent_id}")
      }

      cli::cli_alert_success("Parent taxon updated")

      # STEP 4: Cascade update to all descendants (both flat columns and id_parent)
      descendants <- rv$cascade_descendants

      for (i in 1:nrow(descendants)) {
        desc <- descendants[i, ]
        updates <- list()
        new_desc_parent_id <- NULL

        # Cascade the appropriate field(s) based on descendant level
        if (!is.null(rv$modified_fields$tax_gen) && desc$tax_level %in% c("species", "infraspecific")) {
          updates$tax_gen <- rv$modified_fields$tax_gen$new
        }
        if (!is.null(rv$modified_fields$tax_fam) && desc$tax_level %in% c("genus", "species", "infraspecific")) {
          updates$tax_fam <- rv$modified_fields$tax_fam$new
        }
        if (!is.null(rv$modified_fields$tax_order) && desc$tax_level %in% c("family", "genus", "species", "infraspecific")) {
          updates$tax_order <- rv$modified_fields$tax_order$new
        }
        if (!is.null(rv$modified_fields$tax_famclass) && desc$tax_level %in% c("order", "family", "genus", "species", "infraspecific")) {
          updates$tax_famclass <- rv$modified_fields$tax_famclass$new
        }

        # Determine the new parent for this descendant
        # IMPORTANT: If this descendant is a DIRECT child of the taxon we're modifying,
        # its id_parent should remain pointing to the modified taxon - no need to find/create
        is_direct_child <- (!is.na(desc$id_parent) && desc$id_parent == taxon$idtax_n)

        if (is_direct_child) {
          # Direct child - keep pointing to the same parent (the taxon we're modifying)
          # No need to update id_parent, it's already correct
          cli::cli_alert_info("Descendant {desc$idtax_n} is direct child - keeping id_parent = {taxon$idtax_n}")
        } else {
          # Not a direct child - find/create the appropriate parent
          # Species → genus, Genus → family, Family → order, Order → class
          if (desc$tax_level == "species") {
          genus_name <- if (!is.null(updates$tax_gen)) updates$tax_gen else desc$tax_gen
          family_name <- if (!is.null(updates$tax_fam)) updates$tax_fam else desc$tax_fam
          if (!is.na(genus_name)) {
            new_desc_parent_id <- .find_or_create_taxon(
              actual_con,
              tax_level = "genus",
              tax_gen = genus_name,
              tax_fam = family_name,
              tax_order = if (!is.null(updates$tax_order)) updates$tax_order else desc$tax_order,
              tax_famclass = if (!is.null(updates$tax_famclass)) updates$tax_famclass else desc$tax_famclass
            )
          }
        } else if (desc$tax_level == "genus") {
          family_name <- if (!is.null(updates$tax_fam)) updates$tax_fam else desc$tax_fam
          if (!is.na(family_name)) {
            new_desc_parent_id <- .find_or_create_taxon(
              actual_con,
              tax_level = "family",
              tax_fam = family_name,
              tax_order = if (!is.null(updates$tax_order)) updates$tax_order else desc$tax_order,
              tax_famclass = if (!is.null(updates$tax_famclass)) updates$tax_famclass else desc$tax_famclass
            )
          }
        } else if (desc$tax_level == "family") {
          order_name <- if (!is.null(updates$tax_order)) updates$tax_order else desc$tax_order
          if (!is.na(order_name)) {
            new_desc_parent_id <- .find_or_create_taxon(
              actual_con,
              tax_level = "order",
              tax_order = order_name,
              tax_famclass = if (!is.null(updates$tax_famclass)) updates$tax_famclass else desc$tax_famclass
            )
          }
        } else if (desc$tax_level == "order") {
          class_name <- if (!is.null(updates$tax_famclass)) updates$tax_famclass else desc$tax_famclass
          if (!is.na(class_name)) {
            new_desc_parent_id <- .find_or_create_taxon(
              actual_con,
              tax_level = "class",
              tax_famclass = class_name
            )
          }
        }
        }  # Close the else block for is_direct_child

        # Update flat columns if needed
        if (length(updates) > 0) {
          set_clause <- paste(
            sapply(names(updates), function(field) {
              value <- updates[[field]]
              if (is.na(value) || value == "") {
                sprintf("%s = NULL", field)
              } else {
                sprintf("%s = '%s'", field, gsub("'", "''", value))
              }
            }),
            collapse = ", "
          )

          sql <- sprintf("UPDATE table_taxa SET %s WHERE idtax_n = %d", set_clause, desc$idtax_n)
          DBI::dbExecute(actual_con, sql)
        }

        # Update id_parent if we determined a new parent
        if (!is.null(new_desc_parent_id)) {
          sql_update_parent <- sprintf(
            "UPDATE table_taxa SET id_parent = %d WHERE idtax_n = %d",
            new_desc_parent_id,
            desc$idtax_n
          )
          DBI::dbExecute(actual_con, sql_update_parent)
        }
      }

      # Commit transaction
      DBI::dbExecute(actual_con, "COMMIT;")

      cli::cli_alert_success("Cascade update completed: 1 parent + {nrow(descendants)} descendants")

      shiny::showNotification(
        sprintf(
          i18n()$t("Cascade update successful! Updated %d taxa (1 parent + %d descendants)"),
          nrow(descendants) + 1,
          nrow(descendants)
        ),
        type = "message",
        duration = 10
      )

      # Reset form
      rv$show_form <- FALSE
      rv$modified_fields <- list()
      rv$cascade_descendants <- NULL

    }, error = function(e) {
      # Rollback on error
      tryCatch({
        DBI::dbExecute(actual_con, "ROLLBACK;")
      }, error = function(e2) {})

      cli::cli_alert_danger("Failed to perform cascade update: {e$message}")
      shiny::showNotification(
        paste(i18n()$t("Error during cascade update:"), e$message),
        type = "error",
        duration = 10
      )
    })
  }, message = i18n()$t("Performing cascade update..."))
}

# Helper function to find or create parent taxon and return its ID
.ensure_parent_taxon_exists <- function(rv, taxon, actual_con) {
  # Determine which upper field is being changed and at what level
  # We need to find/create the parent taxon entry

  new_parent_id <- NULL

  # Determine the taxonomic level of the current taxon
  current_level <- taxon$tax_level

  # Check which upper field is being modified and needs a parent entry
  if (!is.null(rv$modified_fields$tax_gen) && current_level == "species") {
    # Changing genus on a species - find/create genus entry
    genus_name <- rv$modified_fields$tax_gen$new
    family_name <- if (!is.null(rv$modified_fields$tax_fam)) rv$modified_fields$tax_fam$new else taxon$tax_fam

    new_parent_id <- .find_or_create_taxon(
      actual_con,
      tax_level = "genus",
      tax_gen = genus_name,
      tax_fam = family_name,
      tax_order = if (!is.null(rv$modified_fields$tax_order)) rv$modified_fields$tax_order$new else taxon$tax_order,
      tax_famclass = if (!is.null(rv$modified_fields$tax_famclass)) rv$modified_fields$tax_famclass$new else taxon$tax_famclass
    )

  } else if (!is.null(rv$modified_fields$tax_fam) && current_level %in% c("genus", "species")) {
    # Changing family on genus or species - find/create family entry
    family_name <- rv$modified_fields$tax_fam$new

    new_parent_id <- .find_or_create_taxon(
      actual_con,
      tax_level = "family",
      tax_fam = family_name,
      tax_order = if (!is.null(rv$modified_fields$tax_order)) rv$modified_fields$tax_order$new else taxon$tax_order,
      tax_famclass = if (!is.null(rv$modified_fields$tax_famclass)) rv$modified_fields$tax_famclass$new else taxon$tax_famclass
    )

  } else if (!is.null(rv$modified_fields$tax_order) && current_level %in% c("family", "genus", "species")) {
    # Changing order - find/create order entry
    order_name <- rv$modified_fields$tax_order$new

    new_parent_id <- .find_or_create_taxon(
      actual_con,
      tax_level = "order",
      tax_order = order_name,
      tax_famclass = if (!is.null(rv$modified_fields$tax_famclass)) rv$modified_fields$tax_famclass$new else taxon$tax_famclass
    )

  } else if (!is.null(rv$modified_fields$tax_famclass) && current_level %in% c("order", "family", "genus", "species")) {
    # Changing class - find/create class entry
    class_name <- rv$modified_fields$tax_famclass$new

    new_parent_id <- .find_or_create_taxon(
      actual_con,
      tax_level = "class",
      tax_famclass = class_name
    )
  }

  return(new_parent_id)
}

# Helper function to find existing taxon or create new one
.find_or_create_taxon <- function(actual_con, tax_level, tax_gen = NA, tax_fam = NA, tax_order = NA, tax_famclass = NA) {
  # Build search query based on level
  search_conditions <- sprintf("tax_level = '%s'", tax_level)

  if (!is.na(tax_gen)) {
    search_conditions <- paste0(search_conditions, sprintf(" AND tax_gen = '%s'", gsub("'", "''", tax_gen)))
  }
  if (!is.na(tax_fam)) {
    search_conditions <- paste0(search_conditions, sprintf(" AND tax_fam = '%s'", gsub("'", "''", tax_fam)))
  }
  if (!is.na(tax_order)) {
    search_conditions <- paste0(search_conditions, sprintf(" AND tax_order = '%s'", gsub("'", "''", tax_order)))
  }
  if (!is.na(tax_famclass)) {
    search_conditions <- paste0(search_conditions, sprintf(" AND tax_famclass = '%s'", gsub("'", "''", tax_famclass)))
  }

  # Search for existing taxon
  sql_search <- sprintf("SELECT idtax_n FROM table_taxa WHERE %s LIMIT 1", search_conditions)
  existing <- DBI::dbGetQuery(actual_con, sql_search)

  if (nrow(existing) > 0) {
    cli::cli_alert_info("Found existing {tax_level} entry (ID: {existing$idtax_n})")
    return(existing$idtax_n)
  }

  # Not found - create new entry
  cli::cli_alert_warning("Creating new {tax_level} entry")

  # Build INSERT statement
  insert_fields <- c("tax_level")
  insert_values <- c(sprintf("'%s'", tax_level))

  if (!is.na(tax_gen)) {
    insert_fields <- c(insert_fields, "tax_gen")
    insert_values <- c(insert_values, sprintf("'%s'", gsub("'", "''", tax_gen)))
  }
  if (!is.na(tax_fam)) {
    insert_fields <- c(insert_fields, "tax_fam")
    insert_values <- c(insert_values, sprintf("'%s'", gsub("'", "''", tax_fam)))
  }
  if (!is.na(tax_order)) {
    insert_fields <- c(insert_fields, "tax_order")
    insert_values <- c(insert_values, sprintf("'%s'", gsub("'", "''", tax_order)))
  }
  if (!is.na(tax_famclass)) {
    insert_fields <- c(insert_fields, "tax_famclass")
    insert_values <- c(insert_values, sprintf("'%s'", gsub("'", "''", tax_famclass)))
  }

  sql_insert <- sprintf(
    "INSERT INTO table_taxa (%s) VALUES (%s) RETURNING idtax_n",
    paste(insert_fields, collapse = ", "),
    paste(insert_values, collapse = ", ")
  )

  new_id <- DBI::dbGetQuery(actual_con, sql_insert)$idtax_n
  cli::cli_alert_success("Created new {tax_level} entry (ID: {new_id})")

  return(new_id)
}
