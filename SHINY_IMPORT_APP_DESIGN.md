# Shiny Import App Design Document

**Date:** 2025-11-27
**Purpose:** Design an interactive Shiny app for plot data import using existing CafriplotsR functions

---

## Executive Summary

The current CafriplotsR package has **excellent import infrastructure**:
- ✅ Transaction-based imports with rollback
- ✅ Smart column mapping with synonyms
- ✅ Comprehensive validation
- ✅ Template generation
- ✅ Interactive console-based workflows

**Goal:** Build a Shiny app that wraps these functions in a visual, step-by-step wizard interface.

**Strategy:** **Wrapper approach** - Reuse all existing functions, add GUI layer only.

---

## Current Import Workflow (Programmatic)

```r
# Step 1: Get template (optional)
template <- get_plot_metadata_template(template_type = "permanent_plot")
export_plot_template("my_template.xlsx")

# Step 2: Load user data
my_data <- readxl::read_excel("my_plots.xlsx")

# Step 3: Configure import
config <- get_import_column_routing("plots")

# Step 4: Map columns
mapping <- map_user_columns(my_data, config, interactive = TRUE)

# Step 5: Validate
validation <- validate_plot_metadata(my_data, mapping$mappings, config)

# Step 6: Dry run
preview <- import_plot_metadata(..., dry_run = TRUE)

# Step 7: Actual import
result <- import_plot_metadata(..., dry_run = FALSE)

# Step 8: Send admin code
writeLines(result$admin_code, "admin_request.R")
```

**Pain Points for Non-Technical Users:**
1. ⚠️ Requires R programming knowledge
2. ⚠️ Multiple manual steps
3. ⚠️ Console-based interactive prompts
4. ⚠️ No visual data preview
5. ⚠️ Error messages in text format

---

## Shiny App Design: Import Wizard

### App Structure: Step-by-Step Wizard

```
┌─────────────────────────────────────────────────────────────┐
│  CafriPlots Data Import Wizard                              │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Progress: [●][●][○][○][○][○]                               │
│            1  2  3  4  5  6                                  │
│                                                              │
│  Step 1: Choose Import Type                                 │
│  Step 2: Upload Data or Download Template                   │
│  Step 3: Map Columns                                        │
│  Step 4: Validate Data                                      │
│  Step 5: Preview Import                                     │
│  Step 6: Execute Import                                     │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Detailed Step Designs

### Step 1: Choose Import Type

**UI:**
```
┌─────────────────────────────────────────────────────────────┐
│ What would you like to import?                              │
│                                                              │
│ ┌─────────────────────────┐  ┌─────────────────────────┐   │
│ │  📊 Plot Metadata       │  │  🌳 Individual Trees    │   │
│ │                         │  │                         │   │
│ │  Import plot locations, │  │  Import tree            │   │
│ │  census dates, and      │  │  measurements and       │   │
│ │  metadata               │  │  traits                 │   │
│ │                         │  │                         │   │
│ │  [ Select ]             │  │  [ Select ]             │   │
│ └─────────────────────────┘  └─────────────────────────┘   │
│                                                              │
│ ℹ️ Note: You must import plot metadata before importing     │
│   individual tree data.                                     │
└─────────────────────────────────────────────────────────────┘
```

**R Code:**
```r
# UI
radioButtons(
  "import_type",
  "Choose import type:",
  choices = c(
    "Plot Metadata" = "plots",
    "Individual Trees" = "individuals"
  ),
  selected = character(0)
)

# Server
observeEvent(input$import_type, {
  rv$config <- get_import_column_routing(input$import_type)
  rv$step <- 2
})
```

---

### Step 2: Upload Data or Download Template

**UI:**
```
┌─────────────────────────────────────────────────────────────┐
│ Upload Your Data                                            │
│                                                              │
│ Option 1: Start with a template                            │
│ ┌───────────────────────────────────────────────────────┐   │
│ │ Template Type:                                        │   │
│ │ ○ Minimal (Required fields only)                      │   │
│ │ ● Permanent Plot (Recommended)                        │   │
│ │ ○ Transect Survey                                     │   │
│ │ ○ Full (All optional fields)                          │   │
│ │                                                        │   │
│ │ ☑ Include example data                                │   │
│ │                                                        │   │
│ │ [ 📥 Download Template ]                               │   │
│ └───────────────────────────────────────────────────────┘   │
│                                                              │
│ Option 2: Upload your existing data                        │
│ ┌───────────────────────────────────────────────────────┐   │
│ │ [ 📁 Choose File... ]  No file selected               │   │
│ │                                                        │   │
│ │ Supported formats: Excel (.xlsx), CSV (.csv)          │   │
│ │ Maximum size: 100 MB                                  │   │
│ └───────────────────────────────────────────────────────┘   │
│                                                              │
│ Data Preview (first 10 rows):                               │
│ ┌───────────────────────────────────────────────────────┐   │
│ │ [Interactive table will appear here]                  │   │
│ └───────────────────────────────────────────────────────┘   │
│                                                              │
│                               [ Cancel ] [ Next → ]         │
└─────────────────────────────────────────────────────────────┘
```

**R Code:**
```r
# UI
tagList(
  h3("Option 1: Download Template"),
  radioButtons("template_type", "Template Type:",
    choices = c(
      "Minimal" = "minimal",
      "Permanent Plot" = "permanent_plot",
      "Transect" = "transect",
      "Full" = "full"
    )
  ),
  checkboxInput("with_examples", "Include example data", TRUE),
  downloadButton("download_template", "Download Template"),

  hr(),
  h3("Option 2: Upload Data"),
  fileInput("file_upload", "Choose File",
    accept = c(".xlsx", ".csv")
  ),

  DT::dataTableOutput("data_preview")
)

# Server
output$download_template <- downloadHandler(
  filename = function() {
    paste0("cafriplot_template_", input$template_type, ".xlsx")
  },
  content = function(file) {
    template <- get_plot_metadata_template(
      template_type = input$template_type,
      with_examples = input$with_examples
    )
    writexl::write_xlsx(template, file)
  }
)

observeEvent(input$file_upload, {
  ext <- tools::file_ext(input$file_upload$name)

  rv$data <- if (ext == "xlsx") {
    readxl::read_excel(input$file_upload$datapath)
  } else {
    read.csv(input$file_upload$datapath)
  }
})

output$data_preview <- DT::renderDataTable({
  req(rv$data)
  head(rv$data, 10)
}, options = list(scrollX = TRUE))
```

---

### Step 3: Map Columns (CRITICAL STEP)

**Current:** Console-based interactive prompts
**Proposed:** Visual drag-and-drop or dropdown mapping interface

**UI Option A: Dropdown Mapping**
```
┌─────────────────────────────────────────────────────────────┐
│ Map Your Columns to Database Schema                         │
│                                                              │
│ ✓ 5 columns auto-mapped | ⚠ 3 need your input | ⊗ 2 skipped│
│                                                              │
│ Your Column         →  Database Column         Status       │
│ ──────────────────────────────────────────────────────────  │
│ PlotID              →  plot_name                ✓ Exact     │
│ Country             →  country                  ✓ Exact     │
│ PI                  →  [Dropdown ▼]             ⚠ Unclear   │
│                        • principal_investigator (Suggested) │
│                        • data_provider                       │
│                        • team_leader                         │
│                        • Skip column                         │
│ Latitude            →  ddlat                    ✓ Synonym   │
│ DBH                 →  [Skip - not plot field]  ⊗ Skipped   │
│ TeamLead            →  team_leader              ✓ Fuzzy     │
│ CensusDate          →  [Dropdown ▼]             ⚠ Unclear   │
│                        • date_begin (Suggested)              │
│                        • date_y                              │
│                        • Skip column                         │
│                                                              │
│ Sample values shown on hover                                │
│                                                              │
│                          [ ← Back ] [ Next → ]              │
└─────────────────────────────────────────────────────────────┘
```

**UI Option B: Visual Drag-and-Drop** (More advanced)
```
┌─────────────────────────────────────────────────────────────┐
│ Drag your columns to match database schema                  │
│                                                              │
│ Your Columns          │  Database Schema                    │
│ ─────────────────────────────────────────────────────────── │
│ ┌─────────────────┐  │  ┌─────────────────┐                │
│ │ PlotID          │──┼─→│ plot_name ✓     │                │
│ └─────────────────┘  │  └─────────────────┘                │
│ ┌─────────────────┐  │  ┌─────────────────┐                │
│ │ Country         │──┼─→│ country ✓       │                │
│ └─────────────────┘  │  └─────────────────┘                │
│ ┌─────────────────┐  │  ┌─────────────────┐                │
│ │ PI              │  │  │ principal_      │  [Connect]     │
│ └─────────────────┘  │  │ investigator    │                │
│                       │  └─────────────────┘                │
│ ┌─────────────────┐  │  ┌─────────────────┐                │
│ │ DBH             │  │  │ method          │                │
│ │ (Will be        │  │  └─────────────────┘                │
│ │  skipped)       │  │                                      │
│ └─────────────────┘  │  Unmapped required fields:          │
│                       │  ⚠ method                           │
│                       │  ⚠ ddlat                            │
└─────────────────────────────────────────────────────────────┘
```

**R Code (Dropdown Approach - Simpler):**
```r
# UI
output$column_mapping_ui <- renderUI({
  req(rv$data, rv$config)

  # Get auto-mapping suggestions
  auto_mapping <- map_user_columns(
    rv$data,
    rv$config,
    interactive = FALSE
  )

  user_cols <- names(rv$data)

  # Build UI for each column
  lapply(user_cols, function(col) {
    # Get suggestion
    suggested <- auto_mapping$mappings[[col]]
    confidence <- auto_mapping$confidence[[col]]  # Would need to add this

    # Build choices
    all_choices <- c(
      "Skip this column" = "",
      rv$config$all_columns
    )

    # Color-code by confidence
    status_icon <- if (confidence > 0.95) {
      "✓"
    } else if (confidence > 0.5) {
      "⚠"
    } else {
      "⊗"
    }

    div(
      class = paste0("mapping-row confidence-", round(confidence * 10)),
      fluidRow(
        column(4,
          strong(col),
          br(),
          small(paste("Sample:", paste(head(rv$data[[col]], 2), collapse = ", ")))
        ),
        column(1, "→"),
        column(6,
          selectInput(
            paste0("map_", col),
            label = NULL,
            choices = all_choices,
            selected = suggested
          )
        ),
        column(1, status_icon)
      )
    )
  })
})

# Server - collect mappings
mapping_inputs <- reactive({
  req(rv$data)

  user_cols <- names(rv$data)
  mappings <- list()

  for (col in user_cols) {
    map_to <- input[[paste0("map_", col)]]
    if (!is.null(map_to) && map_to != "") {
      mappings[[col]] <- map_to
    }
  }

  mappings
})

observeEvent(input$next_from_mapping, {
  rv$mappings <- mapping_inputs()
  rv$step <- 4
})
```

---

### Step 4: Validate Data

**UI:**
```
┌─────────────────────────────────────────────────────────────┐
│ Data Validation Results                                     │
│                                                              │
│ Overall Status: ⚠ WARNINGS FOUND (Can proceed)              │
│                                                              │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ ✓ PASSED (245 checks)                                       │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│   • All required fields present                             │
│   • Column types valid (numeric, character, dates)          │
│   • Unique constraints satisfied (no duplicate plot names)  │
│   • Lookup values valid (methods, countries)                │
│                                                              │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ ⚠ WARNINGS (3) - Can be ignored                             │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│                                                              │
│ 1. Missing optional column: herbarium_nbe_char              │
│    Severity: Low                                            │
│    Impact: Herbarium specimens cannot be linked             │
│    [Ignore] [Add Empty Column]                              │
│                                                              │
│ 2. Unusual elevation value: 4250m (Row 3: Plot-C)          │
│    Severity: Medium                                         │
│    Expected range: -500 to 6000m                            │
│    Current value: 4250                                      │
│    [Ignore] [Edit Value: ____] [View Row]                   │
│                                                              │
│ 3. 2 individuals missing tree_height                        │
│    Severity: Low                                            │
│    Rows affected: 5, 12                                     │
│    [View Details] [Fill with NA] [Ignore]                   │
│                                                              │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ ✗ ERRORS (0)                                                 │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│                                                              │
│ ✓ Data is ready to import!                                  │
│                                                              │
│                          [ ← Back ] [ Next → ]              │
└─────────────────────────────────────────────────────────────┘
```

**R Code:**
```r
# Server
validation_result <- reactive({
  req(rv$data, rv$mappings, rv$config)

  validate_plot_metadata(
    data = rv$data,
    column_mappings = rv$mappings,
    config = rv$config,
    interactive = FALSE,  # No console prompts
    fix_on_fly = TRUE
  )
})

# UI - Summary boxes
output$validation_summary <- renderUI({
  req(validation_result())

  v <- validation_result()

  tagList(
    # Passed checks
    div(class = "alert alert-success",
      icon("check-circle"),
      strong(sprintf(" PASSED (%d checks)", nrow(v$passed)))
    ),

    # Warnings
    if (nrow(v$warnings) > 0) {
      div(class = "alert alert-warning",
        icon("exclamation-triangle"),
        strong(sprintf(" WARNINGS (%d)", nrow(v$warnings))),
        br(),
        lapply(1:nrow(v$warnings), function(i) {
          w <- v$warnings[i, ]
          div(
            class = "warning-item",
            h5(sprintf("%d. %s", i, w$message)),
            p("Severity:", w$severity),
            p("Row:", w$row, "| Column:", w$column),
            actionButton(
              paste0("fix_warning_", i),
              "Fix",
              class = "btn-sm"
            ),
            actionButton(
              paste0("ignore_warning_", i),
              "Ignore",
              class = "btn-sm"
            )
          )
        })
      )
    },

    # Errors
    if (nrow(v$errors) > 0) {
      div(class = "alert alert-danger",
        icon("times-circle"),
        strong(sprintf(" ERRORS (%d)", nrow(v$errors))),
        p("You must fix errors before proceeding"),
        DT::dataTableOutput("error_table")
      )
    } else {
      div(class = "alert alert-success",
        icon("check"),
        strong(" No errors - ready to import!")
      )
    }
  )
})

# Error table
output$error_table <- DT::renderDataTable({
  req(validation_result()$errors)
  validation_result()$errors
})
```

---

### Step 5: Preview Import (Dry Run)

**UI:**
```
┌─────────────────────────────────────────────────────────────┐
│ Import Preview (Dry Run)                                    │
│                                                              │
│ Summary                                                      │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ • 3 plots will be imported                                  │
│ • 5 subplot features will be created                        │
│ • 3 team leaders will be linked                             │
│ • 2 principal investigators will be linked                  │
│                                                              │
│ Plot Names                                                   │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ • Plot-A (Lopé NP, Gabon)                                   │
│ • Plot-B (Lopé NP, Gabon)                                   │
│ • Plot-C (Dja Reserve, Cameroon)                            │
│                                                              │
│ Data Preview                                                 │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ [Tabs: Plots | People | Locations | Dates]                  │
│                                                              │
│ Plots Tab:                                                   │
│ ┌───────────────────────────────────────────────────────┐   │
│ │ plot_name | method  | country  | plot_area | ddlat   │   │
│ │ Plot-A    | 1ha-IRD | Gabon    | 1.0       | -0.50   │   │
│ │ Plot-B    | 1ha-IRD | Gabon    | 1.0       | -0.52   │   │
│ │ Plot-C    | 0.5ha   | Cameroon | 0.5       |  3.20   │   │
│ └───────────────────────────────────────────────────────┘   │
│                                                              │
│ ⚠ Important: This is a preview only. No data has been       │
│   written to the database yet.                              │
│                                                              │
│                [ ← Back ] [ Run Dry Run ] [ Import → ]      │
└─────────────────────────────────────────────────────────────┘
```

**R Code:**
```r
# Server - Run dry run
dry_run_result <- eventReactive(input$run_dry_run, {
  req(validation_result()$valid)

  withProgress(message = 'Running dry run...', {
    import_plot_metadata(
      data = rv$data,
      column_mappings = rv$mappings,
      validation = validation_result(),
      config = rv$config,
      dry_run = TRUE,
      progress = FALSE  # We'll handle progress in Shiny
    )
  })
})

# UI - Display dry run results
output$dry_run_summary <- renderUI({
  req(dry_run_result())

  result <- dry_run_result()

  tagList(
    h4("Summary"),
    tags$ul(
      tags$li(sprintf("%d plots will be imported", result$n_plots)),
      tags$li(sprintf("Plot names: %s", paste(result$plot_names, collapse = ", ")))
    ),

    h4("Data Preview"),
    tabsetPanel(
      tabPanel("Plots",
        DT::dataTableOutput("preview_plots")
      ),
      tabPanel("Subplot Features",
        DT::dataTableOutput("preview_features")
      )
    )
  )
})
```

---

### Step 6: Execute Import

**UI:**
```
┌─────────────────────────────────────────────────────────────┐
│ Importing Data...                                           │
│                                                              │
│ Progress                                                     │
│ ████████████████████░░░░ 80% Complete                       │
│                                                              │
│ ✓ Transaction started                                       │
│ ✓ Methods linked (2 unique methods)                         │
│ ✓ Countries linked (2 unique countries)                     │
│ ✓ People linked (5 individuals)                             │
│ ⏳ Inserting plot data...                                    │
│ ⏸ Pending: Insert subplot features                          │
│ ⏸ Pending: Commit transaction                               │
│                                                              │
│ Elapsed time: 00:00:15                                      │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

**Success Screen:**
```
┌─────────────────────────────────────────────────────────────┐
│ ✅ Import Successful!                                        │
│                                                              │
│ Summary                                                      │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ • 3 plots imported                                          │
│ • 5 subplot features created                                │
│ • Transaction committed at 2025-11-27 14:32:17              │
│ • Total time: 00:00:23                                      │
│                                                              │
│ Imported Plots                                               │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ • Plot-A                                                    │
│ • Plot-B                                                    │
│ • Plot-C                                                    │
│                                                              │
│ ⚠ Important: Admin Access Required                          │
│ ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│ Due to row-level security, you cannot access these plots    │
│ until an administrator grants permission.                   │
│                                                              │
│ Admin Email: admin@institution.org                          │
│                                                              │
│ [📋 Copy Admin Code] [📧 Email Admin] [💾 Download Report]  │
│                                                              │
│ Admin Code Preview:                                          │
│ ┌───────────────────────────────────────────────────────┐   │
│ │ library(CafriplotsR)                                  │   │
│ │ con <- call.mydb()                                    │   │
│ │ define_user_policy(                                   │   │
│ │   con = con,                                          │   │
│ │   user = "john_doe",                                  │   │
│ │   ids = c(145, 146, 147),                             │   │
│ │   ...                                                 │   │
│ └───────────────────────────────────────────────────────┘   │
│                                                              │
│                     [ Start New Import ] [ Close ]          │
└─────────────────────────────────────────────────────────────┘
```

**R Code:**
```r
# Server - Execute import
import_result <- eventReactive(input$execute_import, {
  req(validation_result()$valid)

  # Progress tracking
  progress <- Progress$new()
  progress$set(message = "Importing data...", value = 0)
  on.exit(progress$close())

  # Custom progress handler
  progress_callback <- function(step, message) {
    progress$set(detail = message, value = step / 7)
  }

  result <- tryCatch({
    import_plot_metadata(
      data = rv$data,
      column_mappings = rv$mappings,
      validation = validation_result(),
      config = rv$config,
      dry_run = FALSE,
      progress = TRUE
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = e$message
    )
  })

  result
})

# UI - Success screen
output$import_success <- renderUI({
  req(import_result()$success)

  result <- import_result()

  tagList(
    div(class = "alert alert-success",
      h3(icon("check-circle"), " Import Successful!")
    ),

    h4("Summary"),
    tags$ul(
      tags$li(sprintf("%d plots imported", result$n_plots)),
      tags$li(sprintf("User: %s", result$username)),
      tags$li(sprintf("Plots: %s", paste(result$plot_names, collapse = ", ")))
    ),

    hr(),

    div(class = "alert alert-warning",
      h4(icon("exclamation-triangle"), " Admin Access Required"),
      p("You need administrator approval to access these plots."),
      p(strong("Send this code to:"), " admin@institution.org")
    ),

    verbatimTextOutput("admin_code"),

    actionButton("copy_admin_code", "Copy to Clipboard", icon = icon("clipboard")),
    downloadButton("download_admin_code", "Download Script"),
    actionButton("email_admin", "Email Admin", icon = icon("envelope"))
  )
})

output$admin_code <- renderText({
  import_result()$admin_code
})

# Copy to clipboard using shinyjs
observeEvent(input$copy_admin_code, {
  shinyjs::runjs(sprintf("
    navigator.clipboard.writeText('%s');
    alert('Admin code copied to clipboard!');
  ", import_result()$admin_code))
})
```

---

## Complete Shiny App Structure

### File: `R/shiny_app_import_wizard.R`

```r
#' Launch Import Wizard Shiny App
#'
#' Interactive wizard for importing plot metadata and individual tree data
#'
#' @export
launch_import_wizard <- function() {

  # Check if required packages are available
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install with: install.packages('shiny')")
  }

  shiny::shinyApp(ui = import_wizard_ui(), server = import_wizard_server)
}

#' UI for Import Wizard
#' @keywords internal
import_wizard_ui <- function() {

  shiny::fluidPage(
    # Use shinyjs for JavaScript interactions
    shinyjs::useShinyjs(),

    # Custom CSS
    tags$head(
      tags$style(HTML("
        .step-indicator {
          display: flex;
          justify-content: space-between;
          margin-bottom: 30px;
        }
        .step {
          flex: 1;
          text-align: center;
          padding: 10px;
          background: #f0f0f0;
          border-radius: 5px;
          margin: 0 5px;
        }
        .step.active {
          background: #007bff;
          color: white;
        }
        .step.completed {
          background: #28a745;
          color: white;
        }
        .mapping-row {
          padding: 10px;
          margin: 5px 0;
          border-left: 3px solid #ccc;
        }
        .confidence-10, .confidence-9 {
          border-left-color: #28a745;
        }
        .confidence-8, .confidence-7 {
          border-left-color: #ffc107;
        }
        .confidence-6, .confidence-5, .confidence-4 {
          border-left-color: #dc3545;
        }
      "))
    ),

    # Title
    titlePanel(
      div(
        img(src = "logo.png", height = "50px", style = "margin-right: 10px;"),
        "CafriPlots Import Wizard"
      )
    ),

    # Step indicator
    uiOutput("step_indicator"),

    hr(),

    # Main content (changes based on step)
    uiOutput("step_content"),

    hr(),

    # Navigation buttons
    fluidRow(
      column(6, uiOutput("back_button")),
      column(6, uiOutput("next_button"), align = "right")
    )
  )
}

#' Server for Import Wizard
#' @keywords internal
import_wizard_server <- function(input, output, session) {

  # Reactive values to store state
  rv <- reactiveValues(
    step = 1,
    import_type = NULL,
    config = NULL,
    data = NULL,
    mappings = NULL,
    validation = NULL,
    import_result = NULL
  )

  # Step indicator
  output$step_indicator <- renderUI({
    steps <- c(
      "1. Choose Type",
      "2. Upload Data",
      "3. Map Columns",
      "4. Validate",
      "5. Preview",
      "6. Import"
    )

    div(class = "step-indicator",
      lapply(1:6, function(i) {
        class_name <- if (i < rv$step) {
          "step completed"
        } else if (i == rv$step) {
          "step active"
        } else {
          "step"
        }

        div(class = class_name, steps[i])
      })
    )
  })

  # Step content
  output$step_content <- renderUI({
    switch(rv$step,
      "1" = step1_choose_type_ui(),
      "2" = step2_upload_ui(),
      "3" = step3_mapping_ui(),
      "4" = step4_validation_ui(),
      "5" = step5_preview_ui(),
      "6" = step6_import_ui()
    )
  })

  # Navigation buttons
  output$back_button <- renderUI({
    if (rv$step > 1) {
      actionButton("btn_back", "← Back", class = "btn-secondary")
    }
  })

  output$next_button <- renderUI({
    label <- if (rv$step == 6) "Import" else "Next →"
    actionButton("btn_next", label, class = "btn-primary")
  })

  # Back button handler
  observeEvent(input$btn_back, {
    rv$step <- max(1, rv$step - 1)
  })

  # Next button handler (different logic per step)
  observeEvent(input$btn_next, {
    # Validation before moving to next step
    valid <- switch(rv$step,
      "1" = !is.null(rv$import_type),
      "2" = !is.null(rv$data),
      "3" = !is.null(rv$mappings),
      "4" = !is.null(rv$validation) && rv$validation$valid,
      "5" = TRUE,
      "6" = FALSE  # Import step
    )

    if (valid) {
      if (rv$step < 6) {
        rv$step <- rv$step + 1
      } else {
        # Execute import on step 6
        execute_import()
      }
    } else {
      showNotification("Please complete this step before proceeding", type = "warning")
    }
  })

  # Individual step UI functions (simplified - see detailed designs above)
  step1_choose_type_ui <- function() {
    # Implementation from Step 1 design
  }

  step2_upload_ui <- function() {
    # Implementation from Step 2 design
  }

  step3_mapping_ui <- function() {
    # Implementation from Step 3 design
  }

  step4_validation_ui <- function() {
    # Implementation from Step 4 design
  }

  step5_preview_ui <- function() {
    # Implementation from Step 5 design
  }

  step6_import_ui <- function() {
    # Implementation from Step 6 design
  }

  # Import execution
  execute_import <- function() {
    # Implementation from import execution code
  }
}
```

---

## Modular Approach: Shiny Modules

For better code organization, split into modules:

### File Structure
```
R/
├── shiny_app_import_wizard.R         # Main app launcher
├── mod_import_step1_type.R           # Step 1 module
├── mod_import_step2_upload.R         # Step 2 module
├── mod_import_step3_mapping.R        # Step 3 module (MOST COMPLEX)
├── mod_import_step4_validation.R     # Step 4 module
├── mod_import_step5_preview.R        # Step 5 module
├── mod_import_step6_execute.R        # Step 6 module
└── mod_database_login.R              # Login module (reused)
```

### Example Module: Column Mapping

```r
# File: R/mod_import_step3_mapping.R

#' Column Mapping Module - UI
#' @keywords internal
mod_column_mapping_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Map Your Columns to Database Schema"),

    # Summary stats
    uiOutput(ns("mapping_summary")),

    hr(),

    # Column mapping interface
    uiOutput(ns("mapping_interface")),

    hr(),

    # Validation button
    actionButton(ns("validate_mapping"), "Validate Mapping", class = "btn-info")
  )
}

#' Column Mapping Module - Server
#' @keywords internal
mod_column_mapping_server <- function(id, data, config) {
  moduleServer(id, function(input, output, session) {

    # Auto-generate initial mappings
    auto_mappings <- reactive({
      req(data(), config())

      map_user_columns(
        data = data(),
        config = config(),
        interactive = FALSE  # No console prompts
      )
    })

    # Summary
    output$mapping_summary <- renderUI({
      req(auto_mappings())

      n_exact <- sum(auto_mappings()$method == "exact")
      n_synonym <- sum(auto_mappings()$method == "synonym")
      n_fuzzy <- sum(auto_mappings()$method == "fuzzy")
      n_manual <- sum(auto_mappings()$method == "manual")

      div(
        class = "alert alert-info",
        sprintf("✓ %d auto-mapped | ⚠ %d need review | ⊗ %d skipped",
                n_exact + n_synonym + n_fuzzy,
                n_manual,
                ncol(data()) - length(auto_mappings()$mappings))
      )
    })

    # Mapping interface
    output$mapping_interface <- renderUI({
      req(data(), config(), auto_mappings())

      user_cols <- names(data())
      all_schema_cols <- c("Skip" = "", config()$all_columns)

      lapply(user_cols, function(col) {
        suggested <- auto_mappings()$mappings[[col]]
        method <- auto_mappings()$method[[col]]

        status_icon <- switch(method,
          "exact" = "✓",
          "synonym" = "✓",
          "fuzzy" = "⚠",
          "manual" = "⚠",
          "⊗"
        )

        div(
          class = paste0("mapping-row confidence-",
                        round(auto_mappings()$confidence[[col]] * 10)),
          fluidRow(
            column(4,
              strong(col),
              br(),
              small(paste("Sample:", paste(head(data()[[col]], 2), collapse = ", ")))
            ),
            column(1, "→"),
            column(6,
              selectInput(
                session$ns(paste0("map_", col)),
                label = NULL,
                choices = all_schema_cols,
                selected = suggested
              )
            ),
            column(1, h4(status_icon))
          )
        )
      })
    })

    # Collect final mappings
    final_mappings <- reactive({
      req(data())

      user_cols <- names(data())
      mappings <- list()

      for (col in user_cols) {
        map_to <- input[[paste0("map_", col)]]
        if (!is.null(map_to) && map_to != "") {
          mappings[[col]] <- map_to
        }
      }

      mappings
    })

    # Return mappings
    return(final_mappings)
  })
}
```

---

## Implementation Priorities

### Phase 1: MVP (Week 1-2)
- ✅ Basic app structure with 6 steps
- ✅ File upload (Excel/CSV)
- ✅ Simple column mapping (dropdowns)
- ✅ Validation display
- ✅ Import execution
- ✅ Success/error handling

**Deliverable:** Working wizard that can complete full import workflow

### Phase 2: Enhanced UX (Week 3)
- ✅ Better styling (bslib theming)
- ✅ Real-time data preview at each step
- ✅ Progress bars for import
- ✅ Admin code download/copy
- ✅ Better error messages

**Deliverable:** Polished, professional-looking app

### Phase 3: Advanced Features (Week 4)
- ✅ Drag-and-drop column mapping
- ✅ Interactive validation fixing
- ✅ Email admin integration
- ✅ Import history tracking
- ✅ Template download from app

**Deliverable:** Feature-complete wizard with all conveniences

---

## Testing Checklist

- [ ] Upload Excel file
- [ ] Upload CSV file
- [ ] Auto-mapping accuracy
- [ ] Manual column selection
- [ ] Validation error display
- [ ] Validation warning handling
- [ ] Dry run preview
- [ ] Actual import execution
- [ ] Transaction rollback on error
- [ ] Admin code generation
- [ ] Large file handling (>1000 rows)
- [ ] Special characters in data
- [ ] Missing values handling
- [ ] Database connection errors
- [ ] Session timeout handling

---

## Deployment

### Option 1: Within CafriplotsR Package
```r
# User launches app
library(CafriplotsR)
launch_import_wizard()
```

### Option 2: Standalone Shiny App
```r
# deploy to shinyapps.io or RStudio Connect
rsconnect::deployApp("path/to/app")
```

---

## Next Steps

1. **Create basic app skeleton** (Step 1 + 2)
2. **Test with real data**
3. **Get user feedback**
4. **Iterate on UX**
5. **Add advanced features**
6. **Document and deploy**

---

**Would you like me to start implementing the basic app structure?**
