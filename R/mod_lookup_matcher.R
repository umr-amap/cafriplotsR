# Interactive Lookup Table Matcher Module
#
# Shiny module for matching user values to lookup tables (method, country, people)

#' Lookup Matcher Module - UI
#'
#' @param id Module namespace ID
#' @keywords internal
mod_lookup_matcher_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4(
      shiny::icon("link"),
      " Interactive Lookup Matching",
      style = "color: #007bff; margin-bottom: 20px;"
    ),

    shiny::p(
      "Some values in your data don't exactly match the database. ",
      "Suggestions are sorted by similarity (best matches first). ",
      "Select the correct match from the dropdown, or create a new entry if needed.",
      style = "color: #6c757d; margin-bottom: 20px;"
    ),

    # Matching interface
    shiny::uiOutput(ns("matching_interface")),

    # Action buttons
    shiny::div(
      style = "margin-top: 30px; padding-top: 20px; border-top: 2px solid #dee2e6;",
      shiny::actionButton(
        ns("apply_matches"),
        shiny::tagList(shiny::icon("check"), " Apply Matches"),
        class = "btn-success btn-lg"
      ),
      shiny::actionButton(
        ns("skip_matching"),
        shiny::tagList(shiny::icon("forward"), " Skip (Keep as Errors)"),
        class = "btn-secondary btn-lg",
        style = "margin-left: 10px;"
      )
    ),

    # Modal for adding new method
    shiny::uiOutput(ns("add_method_modal")),

    # Modal for adding new person
    shiny::uiOutput(ns("add_person_modal"))
  )
}


#' Lookup Matcher Module - Server
#'
#' @param id Module namespace ID
#' @param invalid_values Reactive list of invalid values by column (e.g., list(method = c("val1", "val2")))
#' @param con Reactive database connection pool
#' @return Reactive list of resolved matches
#' @keywords internal
mod_lookup_matcher_server <- function(id, invalid_values, con) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for user selections
    user_matches <- shiny::reactiveVal(list())
    matches_applied <- shiny::reactiveVal(FALSE)

    # Storage for lookup data (suggestions) to enable reactive descriptions
    lookup_data_cache <- shiny::reactiveVal(list())

    # Render matching interface
    output$matching_interface <- shiny::renderUI({
      shiny::req(invalid_values(), con())

      inv_vals <- invalid_values()
      if (length(inv_vals) == 0) {
        return(
          shiny::div(
            class = "alert alert-success",
            shiny::icon("check-circle"),
            " All values match! No manual matching needed."
          )
        )
      }

      # Cache for storing lookup data
      cache <- list()

      # Create matching UI for each column
      cli::cli_alert_info("Processing {length(inv_vals)} column(s): {paste(names(inv_vals), collapse=', ')}")

      matching_sections <- lapply(names(inv_vals), function(col_name) {
        values_to_match <- inv_vals[[col_name]]
        cli::cli_alert_info("Column {col_name}: {length(values_to_match)} value(s) to match")

        if (length(values_to_match) == 0) {
          cli::cli_alert_warning("Skipping {col_name} (no values)")
          return(NULL)
        }

        # Get lookup table info
        lookup_info <- .get_lookup_info(col_name, con())

        # Store suggestions for each value in cache
        cli::cli_alert_info("Building cache for {col_name}...")
        for (user_value in values_to_match) {
          input_id <- .make_input_id(user_value, col_name)
          cli::cli_alert_info("  Caching {col_name} value '{user_value}' as '{input_id}'")
          suggestions <- .get_fuzzy_matches(user_value, lookup_info)
          cache[[input_id]] <<- list(
            suggestions = suggestions,
            column_name = col_name,
            lookup_info = lookup_info
          )
          cli::cli_alert_success("  Cached! Cache now has {length(cache)} entry/entries")
        }

        # Create matching rows for each unique value (use cached suggestions)
        matching_rows <- lapply(values_to_match, function(user_value) {
          input_id <- .make_input_id(user_value, col_name)
          cached_suggestions <- cache[[input_id]]$suggestions

          .create_matching_row(
            user_value = user_value,
            column_name = col_name,
            lookup_info = lookup_info,
            session = session,
            suggestions = cached_suggestions
          )
        })

        # Section for this column
        shiny::tagList(
          shiny::h5(
            shiny::icon("table"),
            sprintf(" Column: %s", col_name),
            style = "color: #495057; margin-top: 30px; margin-bottom: 15px;"
          ),
          shiny::p(
            sprintf("Found %d unique value(s) that need matching:", length(values_to_match)),
            style = "color: #6c757d;"
          ),
          shiny::div(
            class = "alert alert-info",
            style = "font-size: 14px; margin-bottom: 20px;",
            shiny::icon("info-circle"),
            shiny::strong(" How matching works: "),
            "Suggestions are sorted by similarity (most likely matches first). ",
            "If none of the suggestions match, scroll to the bottom of each dropdown and select ",
            shiny::tags$strong("\u2795 Create New Entry"),
            " to add a new person/method."
          ),
          matching_rows,
          shiny::hr()
        )
      })

      # Store cache for reactive access
      cli::cli_alert_info("Storing cache with {length(cache)} entries")
      cli::cli_alert_info("Cache keys: {paste(names(cache), collapse=', ')}")
      lookup_data_cache(cache)

      shiny::tagList(matching_sections)
    })

    # Create reactive description outputs (must be outside renderUI)
    shiny::observe({
      cli::cli_alert_info("Observe block triggered")
      cache <- lookup_data_cache()
      cli::cli_alert_info("Cache has {length(cache) %||% 0} entries")
      shiny::req(length(cache) > 0)

      cli::cli_alert_info("Creating description outputs for {length(cache)} input(s)")

      for (input_id in names(cache)) {
        local({
          id <- input_id
          cached_data <- cache[[id]]

          output[[paste0(id, "_desc")]] <- shiny::renderUI({
            selected_id <- input[[id]]

            cli::cli_alert_info("Description render for {id}: selected_id = {selected_id %||% 'NULL'}")

            # Return NULL if no selection or default selection
            if (is.null(selected_id) || selected_id == "" || selected_id == "ADD_NEW") {
              cli::cli_alert_info("  No selection or ADD_NEW")
              return(NULL)
            }

            # Only show descriptions for columns that have them (e.g., method)
            if (is.null(cached_data$lookup_info$desc_col)) {
              cli::cli_alert_info("  No desc_col in lookup_info")
              return(NULL)
            }

            # Find the selected suggestion
            suggestions <- cached_data$suggestions
            cli::cli_alert_info("  Suggestions columns: {paste(names(suggestions), collapse=', ')}")

            # Check if desc column exists
            if (!"desc" %in% names(suggestions)) {
              cli::cli_alert_warning("  'desc' column not in suggestions!")
              return(NULL)
            }

            # Match by ID (handle both numeric and character)
            selected_row <- suggestions[suggestions$id == selected_id, ]
            cli::cli_alert_info("  Found {nrow(selected_row)} matching row(s)")

            if (nrow(selected_row) == 0) {
              return(NULL)
            }

            description <- selected_row$desc[1]
            cli::cli_alert_info("  Description: {substr(description, 1, 50)}...")

            if (is.na(description) || description == "") {
              cli::cli_alert_warning("  Description is NA or empty")
              return(NULL)
            }

            # Display description
            cli::cli_alert_success("  Rendering description UI")
            shiny::div(
              style = "margin-top: 10px; padding: 10px; background-color: #e7f3ff; border-left: 3px solid #007bff; border-radius: 4px;",
              shiny::tags$small(
                shiny::icon("info-circle", style = "color: #007bff;"),
                " ",
                shiny::strong("Description: "),
                description,
                style = "color: #495057;"
              )
            )
          })
        })
      }
    })

    # Storage for new values being added
    add_new_context <- shiny::reactiveVal(NULL)

    # Observe all dropdown selections for "ADD_NEW"
    shiny::observe({
      shiny::req(invalid_values())

      for (col_name in names(invalid_values())) {
        for (user_value in invalid_values()[[col_name]]) {
          input_id <- .make_input_id(user_value, col_name)

          local({
            col <- col_name
            val <- user_value
            id <- input_id

            shiny::observeEvent(input[[id]], {
              if (!is.null(input[[id]]) && input[[id]] == "ADD_NEW") {
                # Store context for modal
                add_new_context(list(
                  column = col,
                  user_value = val,
                  input_id = id
                ))
              }
            }, ignoreInit = TRUE)
          })
        }
      }
    })

    # Add New Method Modal
    output$add_method_modal <- shiny::renderUI({
      shiny::req(add_new_context())
      ctx <- add_new_context()

      if (ctx$column != "method") {
        return(NULL)
      }

      shiny::modalDialog(
        title = shiny::tagList(
          shiny::icon("plus-circle"),
          " Add New Method"
        ),
        size = "m",

        shiny::p(
          sprintf("Add a new method for: '%s'", ctx$user_value),
          style = "color: #6c757d;"
        ),

        shiny::textInput(
          session$ns("new_method_name"),
          "Method Name:",
          value = ctx$user_value
        ),

        shiny::textAreaInput(
          session$ns("new_method_description"),
          "Protocol Description:",
          rows = 4,
          placeholder = "Describe the survey protocol/method..."
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            session$ns("confirm_add_method"),
            "Add Method",
            class = "btn-primary"
          )
        )
      )
    })

    # Add New Person Modal
    output$add_person_modal <- shiny::renderUI({
      shiny::req(add_new_context(), con())
      ctx <- add_new_context()

      # Check if this is a people column (dynamically detect from database)
      is_people_column <- tryCatch({
        subplot_info <- subplot_list(con())
        if (!is.null(subplot_info) && "type" %in% names(subplot_info) && "valuetype" %in% names(subplot_info)) {
          people_cols <- subplot_info$type[!is.na(subplot_info$valuetype) & subplot_info$valuetype == "table_colnam"]
          people_cols <- people_cols[!is.na(people_cols)]
          ctx$column %in% people_cols
        } else {
          FALSE
        }
      }, error = function(e) {
        FALSE
      })

      if (!is_people_column) {
        return(NULL)
      }

      shiny::modalDialog(
        title = shiny::tagList(
          shiny::icon("user-plus"),
          " Add New Person"
        ),
        size = "m",

        shiny::p(
          sprintf("Add a new person for: '%s'", ctx$user_value),
          style = "color: #6c757d;"
        ),

        shiny::textInput(
          session$ns("new_person_first_name"),
          "First Name: *",
          placeholder = "e.g., John"
        ),

        shiny::textInput(
          session$ns("new_person_last_name"),
          "Last Name: *",
          placeholder = "e.g., Smith"
        ),

        shiny::textInput(
          session$ns("new_person_nationality"),
          "Nationality:",
          placeholder = "e.g., France, USA (optional)"
        ),

        shiny::textInput(
          session$ns("new_person_institute"),
          "Institute:",
          placeholder = "e.g., University of Example (optional)"
        ),

        shiny::textInput(
          session$ns("new_person_contact"),
          "Contact:",
          placeholder = "e.g., email, phone (optional)"
        ),

        shiny::p(
          shiny::tags$small(
            shiny::icon("info-circle"),
            " Fields marked with * are required",
            style = "color: #6c757d;"
          )
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            session$ns("confirm_add_person"),
            "Add Person",
            class = "btn-primary"
          )
        )
      )
    })

    # Show appropriate modal when ADD_NEW is selected
    shiny::observe({
      shiny::req(add_new_context(), con())
      ctx <- add_new_context()

      # Dynamically detect if this is a people column
      is_people_column <- tryCatch({
        subplot_info <- subplot_list(con())
        if (!is.null(subplot_info) && "type" %in% names(subplot_info) && "valuetype" %in% names(subplot_info)) {
          people_cols <- subplot_info$type[!is.na(subplot_info$valuetype) & subplot_info$valuetype == "table_colnam"]
          people_cols <- people_cols[!is.na(people_cols)]
          ctx$column %in% people_cols
        } else {
          FALSE
        }
      }, error = function(e) {
        FALSE
      })

      if (ctx$column == "method") {
        shiny::showModal(shiny::uiOutput(session$ns("add_method_modal")))
      } else if (is_people_column) {
        shiny::showModal(shiny::uiOutput(session$ns("add_person_modal")))
      }
    })

    # Confirm add new method
    shiny::observeEvent(input$confirm_add_method, {
      shiny::req(add_new_context(), con())

      method_name <- input$new_method_name
      description <- input$new_method_description

      if (is.null(method_name) || trimws(method_name) == "") {
        shiny::showNotification(
          "Method name is required",
          type = "error"
        )
        return()
      }

      # Insert into database
      tryCatch({
        new_id <- DBI::dbGetQuery(con(), sprintf("
          INSERT INTO methodslist (method, description_method)
          VALUES ('%s', '%s')
          RETURNING id_method
        ", DBI::dbQuoteLiteral(con(), method_name), DBI::dbQuoteLiteral(con(), description)))$id_method

        cli::cli_alert_success("Added new method: {method_name} (ID: {new_id})")

        # Get context before resetting it
        ctx <- add_new_context()

        # Get cached data before closing modal
        cached <- lookup_data_cache()[[ctx$input_id]]

        # Close modal and reset context immediately
        shiny::removeModal()
        add_new_context(NULL)

        # Build new choices
        new_choices <- c(
          "(Select a match)" = "",
          structure(new_id, names = method_name),
          structure(
            cached$suggestions$id,
            names = cached$suggestions$label
          )
        )

        # Simple synchronous delay - let Shiny process modal closure
        Sys.sleep(0.2)

        # Update dropdown directly
        shiny::updateSelectInput(
          session = session,
          inputId = ctx$input_id,
          choices = new_choices,
          selected = new_id
        )

        # Show success notification
        shiny::showNotification(
          sprintf("Successfully added and selected: %s", method_name),
          type = "message",
          duration = 5
        )

        cli::cli_alert_success("Method added and dropdown updated!")

      }, error = function(e) {
        cli::cli_alert_danger("Failed to add method: {e$message}")
        shiny::showNotification(
          paste("Error adding method:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Confirm add new person
    shiny::observeEvent(input$confirm_add_person, {
      shiny::req(add_new_context(), con())

      first_name <- input$new_person_first_name
      last_name <- input$new_person_last_name
      nationality <- input$new_person_nationality
      institute <- input$new_person_institute
      contact <- input$new_person_contact

      if (is.null(first_name) || trimws(first_name) == "" ||
          is.null(last_name) || trimws(last_name) == "") {
        shiny::showNotification(
          "Both first and last names are required",
          type = "error"
        )
        return()
      }

      # Insert into database
      tryCatch({
        # Construct full name (colnam = surname + " " + family_name)
        full_name <- paste(first_name, last_name)

        # Prepare optional fields (NULL if empty)
        nationality_val <- if (!is.null(nationality) && trimws(nationality) != "") {
          DBI::dbQuoteLiteral(con(), trimws(nationality))
        } else {
          "NULL"
        }

        institute_val <- if (!is.null(institute) && trimws(institute) != "") {
          DBI::dbQuoteLiteral(con(), trimws(institute))
        } else {
          "NULL"
        }

        contact_val <- if (!is.null(contact) && trimws(contact) != "") {
          DBI::dbQuoteLiteral(con(), trimws(contact))
        } else {
          "NULL"
        }

        # Build SQL query with all fields
        query <- sprintf("
          INSERT INTO table_colnam (surname, family_name, colnam, nationality, institute, contact)
          VALUES (%s, %s, %s, %s, %s, %s)
          RETURNING id_table_colnam
        ",
        DBI::dbQuoteLiteral(con(), first_name),
        DBI::dbQuoteLiteral(con(), last_name),
        DBI::dbQuoteLiteral(con(), full_name),
        nationality_val,
        institute_val,
        contact_val)

        new_id <- DBI::dbGetQuery(con(), query)$id_table_colnam
        cli::cli_alert_success("Added new person: {first_name} {last_name} (ID: {new_id})")

        # Get context before resetting it
        ctx <- add_new_context()

        # Get cached data before closing modal
        cached <- lookup_data_cache()[[ctx$input_id]]

        # Close modal and reset context immediately
        shiny::removeModal()
        add_new_context(NULL)

        # Build new choices
        new_choices <- c(
          "(Select a match)" = "",
          structure(new_id, names = full_name),
          structure(
            cached$suggestions$id,
            names = cached$suggestions$label
          )
        )

        # Simple synchronous delay - let Shiny process modal closure
        Sys.sleep(0.2)

        # Update dropdown directly
        shiny::updateSelectInput(
          session = session,
          inputId = ctx$input_id,
          choices = new_choices,
          selected = new_id
        )

        # Show success notification
        shiny::showNotification(
          paste0(
            "Successfully added and selected: ", first_name, " ", last_name
          ),
          type = "message",
          duration = 5
        )

        cli::cli_alert_success("Person added and dropdown updated!")

      }, error = function(e) {
        cli::cli_alert_danger("Failed to add person: {e$message}")
        shiny::showNotification(
          paste("Error adding person:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Apply matches
    shiny::observeEvent(input$apply_matches, {
      shiny::req(invalid_values())

      # Collect all selections
      matches <- list()

      for (col_name in names(invalid_values())) {
        for (user_value in invalid_values()[[col_name]]) {
          input_id <- .make_input_id(user_value, col_name)
          selected <- input[[input_id]]

          if (!is.null(selected) && selected != "") {
            if (!col_name %in% names(matches)) {
              matches[[col_name]] <- list()
            }
            matches[[col_name]][[user_value]] <- selected
          }
        }
      }

      user_matches(matches)
      matches_applied(TRUE)

      shiny::showNotification(
        sprintf("Applied %d match(es)", length(unlist(matches))),
        type = "message",
        duration = 3
      )
    })

    # Skip matching
    shiny::observeEvent(input$skip_matching, {
      matches_applied(TRUE)
    })

    # Return results
    return(
      list(
        matches = shiny::reactive(user_matches()),
        applied = shiny::reactive(matches_applied())
      )
    )
  })
}


#' Get Lookup Table Info
#' @keywords internal
.get_lookup_info <- function(column_name, con) {

  # Check if this is a people column (feature with valuetype == table_colnam)
  is_people_column <- FALSE
  tryCatch({
    subplot_info <- subplot_list(con)

    # Ensure subplot_info has the required columns
    if (!is.null(subplot_info) && "type" %in% names(subplot_info) && "valuetype" %in% names(subplot_info)) {
      # Filter for table_colnam types and remove NAs
      people_cols <- subplot_info$type[!is.na(subplot_info$valuetype) & subplot_info$valuetype == "table_colnam"]
      people_cols <- people_cols[!is.na(people_cols)]
      is_people_column <- column_name %in% people_cols
    } else {
      # Fallback if columns don't exist
      is_people_column <- column_name %in% c("principal_investigator", "data_manager",
                                             "additional_people", "team_leader")
    }
  }, error = function(e) {
    cli::cli_alert_warning("Could not check people columns: {e$message}")
    # If can't fetch, check against known people columns
    is_people_column <- column_name %in% c("principal_investigator", "data_manager",
                                           "additional_people", "team_leader")
  })

  # Return configuration based on column type
  if (column_name == "method") {
    lookup_info <- list(
      table = "methodslist",
      value_col = "method",
      id_col = "id_method",
      desc_col = "description_method",
      function_name = "method_list",
      allow_add_new = TRUE,
      add_fields = c("method", "description_method")
    )
  } else if (column_name == "country") {
    lookup_info <- list(
      table = "table_countries",
      value_col = "country",
      id_col = "id_country",
      desc_col = NULL,
      function_name = "country_list",
      allow_add_new = FALSE,
      add_fields = NULL
    )
  } else if (is_people_column) {
    # ANY feature with valuetype == table_colnam (current and future)
    lookup_info <- list(
      table = "table_colnam",
      value_col = "colnam",
      id_col = "id_table_colnam",
      desc_col = NULL,
      function_name = NULL,
      allow_add_new = TRUE,
      add_fields = c("first_name", "last_name")
    )
  } else {
    # Unknown column type
    lookup_info <- list(
      table = NULL,
      value_col = NULL,
      id_col = NULL,
      desc_col = NULL,
      function_name = NULL,
      allow_add_new = FALSE,
      add_fields = NULL
    )
  }

  if (!is.null(lookup_info$function_name)) {
    # Fetch lookup table using function
    lookup_info$data <- tryCatch({
      do.call(lookup_info$function_name, list())
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch {column_name} lookup table: {e$message}")
      NULL
    })
  } else if (!is.null(lookup_info$table) && lookup_info$table == "table_colnam") {
    # Fetch people data directly from database
    lookup_info$data <- tryCatch({
      DBI::dbGetQuery(con, "
        SELECT id_table_colnam, colnam
        FROM table_colnam
        ORDER BY colnam
      ")
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch {column_name} lookup table: {e$message}")
      NULL
    })
  }

  lookup_info
}


#' Create Matching Row for One Value
#' @keywords internal
.create_matching_row <- function(user_value, column_name, lookup_info, session, suggestions = NULL) {

  ns <- session$ns

  # Get fuzzy matches if not provided
  if (is.null(suggestions)) {
    suggestions <- .get_fuzzy_matches(user_value, lookup_info)
  }

  # Create choices (conditionally include "Add New Value")
  choices <- c(
    "(Select a match)" = "",
    structure(
      suggestions$id,
      names = suggestions$label
    )
  )

  # Add "Add New Value" option if allowed
  if (!is.null(lookup_info$allow_add_new) && lookup_info$allow_add_new) {
    choices <- c(
      choices,
      "\u2795 Create New Entry (if not found above)" = "ADD_NEW"
    )
  }

  # Create input ID
  input_id <- .make_input_id(user_value, column_name)

  # Build UI
  shiny::div(
    class = "card",
    style = "padding: 15px; margin-bottom: 15px; background-color: #f8f9fa; border-left: 4px solid #ffc107;",

    shiny::fluidRow(
      # User value
      shiny::column(
        3,
        shiny::strong("Your value:", style = "font-size: 13px; color: #6c757d;"),
        shiny::br(),
        shiny::tags$code(user_value, style = "font-size: 14px; color: #dc3545;")
      ),

      # Arrow
      shiny::column(
        1,
        shiny::div(
          shiny::icon("arrow-right", style = "font-size: 20px; color: #007bff;"),
          style = "text-align: center; padding-top: 15px;"
        )
      ),

      # Match selector
      shiny::column(
        8,
        shiny::selectInput(
          ns(input_id),
          label = shiny::strong("Select correct match:", style = "font-size: 13px;"),
          choices = choices,
          width = "100%"
        ),

        # Show description for selected value (dynamic)
        shiny::uiOutput(ns(paste0(input_id, "_desc")))
      )
    )
  )
}


#' Get Fuzzy Matches for a Value
#' @keywords internal
.get_fuzzy_matches <- function(user_value, lookup_info, n = NULL) {

  if (is.null(lookup_info$data)) {
    return(data.frame(
      id = character(),
      value = character(),
      label = character(),
      similarity = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  lookup_data <- lookup_info$data

  # Debug: Check source data for duplicates and remove them
  if (!is.null(lookup_info$value_col) && lookup_info$value_col %in% names(lookup_data)) {
    dup_values <- duplicated(lookup_data[[lookup_info$value_col]])
    if (any(dup_values)) {
      cli::cli_alert_warning("Source data has {sum(dup_values)} duplicate value(s) in column '{lookup_info$value_col}'")
      cli::cli_alert_warning("  Duplicates: {paste(unique(lookup_data[[lookup_info$value_col]][dup_values]), collapse=', ')}")
      cli::cli_alert_info("  Removing duplicates (keeping first occurrence)...")
      # Keep only first occurrence of each value
      lookup_data <- lookup_data[!dup_values, ]
    }
    dup_ids <- duplicated(lookup_data[[lookup_info$id_col]])
    if (any(dup_ids)) {
      cli::cli_alert_warning("Source data has {sum(dup_ids)} duplicate ID(s) in column '{lookup_info$id_col}'")
      cli::cli_alert_info("  Removing duplicate IDs (keeping first occurrence)...")
      lookup_data <- lookup_data[!dup_ids, ]
    }
  }

  # Check if this is a people column (from table_colnam)
  is_people_column <- !is.null(lookup_info$table) && lookup_info$table == "table_colnam"

  if (is_people_column) {
    # For people names, use token-based similarity to handle word order
    # This improves matching for "First Last" vs "Last First"
    similarities <- sapply(lookup_data[[lookup_info$value_col]], function(db_value) {
      # Tokenize both strings
      user_tokens <- tolower(trimws(unlist(strsplit(user_value, "\\s+"))))
      db_tokens <- tolower(trimws(unlist(strsplit(db_value, "\\s+"))))

      # Calculate Jaro-Winkler for each token pair
      if (length(user_tokens) == 0 || length(db_tokens) == 0) {
        return(0)
      }

      # Create similarity matrix for all token pairs
      token_sims <- outer(user_tokens, db_tokens, function(x, y) {
        1 - stringdist::stringdist(x, y, method = "jw")
      })

      # For each user token, find best matching db token
      best_matches <- apply(token_sims, 1, max)

      # Average similarity across all user tokens
      mean(best_matches)
    })
  } else {
    # For non-people columns, use standard Jaro-Winkler
    distances <- stringdist::stringdist(
      tolower(trimws(user_value)),
      tolower(trimws(lookup_data[[lookup_info$value_col]])),
      method = "jw"  # Jaro-Winkler distance
    )

    # Get similarities (1 - distance)
    similarities <- 1 - distances
  }

  # Get all matches sorted by similarity (or top N if specified)
  ordered_indices <- order(similarities, decreasing = TRUE)
  if (!is.null(n)) {
    ordered_indices <- head(ordered_indices, n)
  }

  # Build suggestions
  suggestions <- data.frame(
    id = lookup_data[[lookup_info$id_col]][ordered_indices],
    value = lookup_data[[lookup_info$value_col]][ordered_indices],
    similarity = similarities[ordered_indices],
    stringsAsFactors = FALSE
  )

  # Add descriptions if available (store full description for reactive display)
  if (!is.null(lookup_info$desc_col) && lookup_info$desc_col %in% names(lookup_data)) {
    suggestions$desc <- lookup_data[[lookup_info$desc_col]][ordered_indices]
    # For dropdown labels, show similarity percentage only (not truncated desc)
    suggestions$label <- sprintf(
      "%s (%.0f%% match)",
      suggestions$value,
      suggestions$similarity * 100
    )
  } else {
    suggestions$label <- sprintf(
      "%s (%.0f%% match)",
      suggestions$value,
      suggestions$similarity * 100
    )
  }

  # Debug: Check for duplicates
  if (nrow(suggestions) > 0) {
    dup_ids <- duplicated(suggestions$id)
    if (any(dup_ids)) {
      cli::cli_alert_warning("Found {sum(dup_ids)} duplicate ID(s) in suggestions!")
      cli::cli_alert_warning("  Duplicate IDs: {paste(suggestions$id[dup_ids], collapse=', ')}")
    }
  }

  suggestions
}


#' Make Input ID from Value and Column
#' @keywords internal
.make_input_id <- function(user_value, column_name) {
  # Create safe input ID
  safe_value <- gsub("[^a-zA-Z0-9]", "_", user_value)
  paste0("match_", column_name, "_", safe_value)
}
