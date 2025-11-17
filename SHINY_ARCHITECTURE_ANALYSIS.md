# Shiny App Architecture & query_plots() Function Analysis

**Analysis Date**: November 14, 2025  
**Current Branch**: feature/add-query-plots-shiny-app  
**Purpose**: Planning a new Shiny app for querying forest plot data

---

## EXECUTIVE SUMMARY

This analysis provides a complete guide for developing a Shiny app around the `query_plots()` function by studying the existing modular taxonomic matching app.

**Key Findings**:
1. Package uses excellent **modular Shiny architecture** with 9 specialized modules
2. `query_plots()` is highly flexible with **33 parameters** organized into 7 groups
3. **7 output styles** provide data formatting for different analysis types
4. **Built-in mapping** support via mapview and sf packages
5. **R6 classes** (PlotFilterBuilder, PlotFetcher) simplify query construction

---

## PART 1: SHINY APP ARCHITECTURE

### Architecture Type: MODULAR DESIGN

**Main Files**:
- `R/launch_taxonomic_match_app.R` - Public launcher
- `R/shiny_app_taxonomic_match.R` - Core app function
- `R/mod_*.R` (9 modules) - Functional components
- `R/output_styles_config.R` - Shared style definitions

**Module Pattern**:
```r
mod_[name]_ui(id)                           # UI function
mod_[name]_server(id, ..., language)        # Server function
```

**The 9 Modules**: data_input, column_select, auto_matching, name_review, results_export, traits_enrichment, language_toggle, progress_tracker, fuzzy_suggestions

### Database Connection Management
- Uses connection pooling to prevent exhaustion
- Pools created at initialization: `.db_env$pool_main`, `.db_env$pool_taxa`
- Cleaned up on session end

### Data Flow
Input Data → Column Select → Auto Match → Manual Review → Export → (Optional) Traits

---

## PART 2: query_plots() FUNCTION

### Location: R/functions_manip_db.R (lines 118-532)

### 33 Parameters Organized in 7 Groups

#### Group 1: Query Filters (9 parameters)
`plot_name`, `country`, `locality_name`, `method`, `tag`, `id_plot`, `id_individual`, `id_tax`, `id_specimen`

#### Group 2: Data Extraction (4 parameters)
`extract_individuals`, `extract_traits`, `extract_individual_features`, `extract_subplot_features`

#### Group 3: Trait Processing (3 parameters)
`traits_to_genera`, `wd_fam_level`, `include_liana`

#### Group 4: Census & Time Series (2 parameters)
`show_multiple_census`, `census_strategy` ("last", "first", "mean")

#### Group 5: Data Organization (8 parameters)
`remove_ids`, `collapse_multiple_val`, `concatenate_stem`, `remove_obs_with_issue`, `include_issue`, `include_measurement_ids`, `show_all_coordinates`, `exact_match`

#### Group 6: Interaction & Mapping (2 parameters)
`interactive`, `map`

#### Group 7: Output Formatting (1 parameter)
`output_style` - "auto", "minimal", "standard", "permanent_plot", "permanent_plot_multi_census", "transect", "full"

### Return Value Patterns

**Single component**: Returns data.frame directly

**Multiple components**: Returns named list with:
- `$extract` - Main data
- `$meta_data` - Plot metadata
- `$census_features` - Per-census records (if multiple censuses)
- `$coordinates` - Subplot coordinates (if requested)
- `$coordinates_sf` - Subplot spatial objects (if requested)

**With output style**: Returns class "plot_query_list" for named table access

---

## PART 3: OUTPUT STYLES

**Location**: R/output_styles_config.R

### 7 Styles Available

| Style | Use Case | Key Feature |
|-------|----------|------------|
| **auto** | Default | Auto-detects from method field |
| **minimal** | Essential data | Bare minimum columns |
| **standard** | General analysis | Most common choice |
| **permanent_plot** | Single census | Structured for monitoring |
| **permanent_plot_multi_census** | Time series | Preserves all _census_N columns |
| **transect** | Walk surveys | Simplified for transect data |
| **full** | Complete export | All columns, no restructuring |

### Column Renaming Examples
- ddlat → latitude
- ddlon → longitude
- stem_diameter → dbh
- tree_height → height

### Auto-Detection Logic
Maps plot method to style:
- "1 ha plot" → permanent_plot
- "Transect" → transect
- Default → standard

---

## PART 4: MAPPING & VISUALIZATION

### Built-in Mapping (query_plots parameter: map=TRUE)

**Code Pattern**:
```r
data_sf <- sf::st_as_sf(data, coords = c("ddlon", "ddlat"), crs = 4326)
mapview::mapview(data_sf, map.types = c("OpenStreetMap.DE", "Esri.WorldImagery", "Esri.WorldPhysical"))
```

**Features**:
- Multiple basemaps
- Interactive popups
- Optional subplot coordinate overlay
- Zoom and pan controls

**Technologies**: sf, mapview, leaflet.js

---

## PART 5: RECOMMENDED APP STRUCTURE

### 6 Suggested Modules

1. **mod_plot_filters** - All filter inputs (country, plot_name, method, etc.)
2. **mod_query_builder** - Build and preview SQL query
3. **mod_plot_map** - Interactive map display
4. **mod_plot_results** - Tabbed results viewer (metadata, individuals, features)
5. **mod_style_selector** - Output style selection with preview
6. **mod_export_options** - Download in Excel/CSV/RDS formats

### Suggested Data Flow
```
Filters → Query Builder → Execute query_plots()
                ↓
          Style Selector
                ↓
    ┌─────────┬─────────┬─────────┐
    ↓         ↓         ↓         ↓
  Map      Table     Features  Download
```

### Key Patterns to Follow
1. Use shiny::moduleServer() for module logic
2. Pass language = reactive() for i18n
3. Use shiny::reactiveVal() for state
4. Implement progress tracking for long operations
5. Store pools in .db_env for connection management
6. Use CLI messages for user feedback
7. Support interactive and exact matching
8. Use PlotFilterBuilder R6 class for queries
9. Leverage PlotFetcher for data retrieval
10. Implement error handling with user-friendly messages

---

## PART 6: R6 CLASSES AVAILABLE

### PlotFilterBuilder (lines 1107-1390)
```r
query_builder <- PlotFilterBuilder$new(mydb)
query <- query_builder$
  filter_country("Gabon")$
  filter_method("1 ha plot")$
  filter_locality("Lope")$
  build()
```

Methods: filter_country, filter_plot_name, filter_method, filter_locality, build(), build_with_or(), add_custom_condition()

### PlotFetcher (lines 1398-1489)
Methods: fetch_by_ids(), fetch_with_filter()

---

## PART 7: FILE LOCATIONS

### Shiny Examples
- R/launch_taxonomic_match_app.R - Launcher pattern
- R/shiny_app_taxonomic_match.R - App orchestration
- R/mod_data_input.R - Basic module
- R/mod_auto_matching.R - Complex module
- R/mod_results_export.R - Export pattern

### query_plots() Related
- R/functions_manip_db.R - query_plots() and R6 classes
- R/output_styles_config.R - Style definitions
- R/output_styles_helpers.R - Style application

---

## PART 8: QUICK REFERENCE

### Most Used Parameters
- `country` - Common filter
- `plot_name` - Direct lookup
- `extract_individuals` - Include trees? (default: FALSE)
- `extract_traits` - Include traits? (default: TRUE)
- `output_style` - Result format (default: "auto")

### Return Value Access
```r
result <- query_plots(...)

if (is.data.frame(result)) {
  # Single component - use directly
} else if (is.list(result)) {
  # Multiple components
  result$extract
  result$meta_data
  result$census_features
} else if (inherits(result, "plot_query_list")) {
  # Styled output
  names(result)  # See available tables
}
```

---

**Document prepared for feature/add-query-plots-shiny-app branch development**
