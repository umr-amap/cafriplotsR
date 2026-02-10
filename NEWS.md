# CafriplotsR 1.9.2 (Development)

### Bug Fixes

* **Fixed empty specimen handling in `merge_individuals_taxa()`**
  - When no specimens are linked to individuals, function was creating empty tibble without proper column structure
  - Caused "Column 'id_specimen' doesn't exist" error in `dplyr::select()`
  - Now creates empty tibble with correct column structure (id_specimen, idtax_specimen_f, colnam_specimen, colnbr, suffix)
  - Fixes error in `query_plots()` with `extract_individuals = TRUE` when plots have no linked specimens

* **Fixed connection retry logic for trait measurement features queries**
  - Replaced direct `DBI::dbGetQuery()` calls with `func_try_fetch()` in measurement features functions
  - Automatic retry (up to 10 attempts) when database connections are lost or timeout
  - Affected functions: `count_measurement_features()`, `fetch_measurement_features_raw()`, `fetch_taxa_trait_measurements()`, `pivot_table_references()`
  - Prevents "server closed the connection unexpectedly" errors in long-running Shiny sessions
  - Users no longer need to manually rerun queries after connection failures

* **Fixed duplicate rows when including measurement features**
  - `query_taxa_traits()` and `query_individual_features()` with `include_measurement_features = TRUE` were creating duplicate rows for same `id_trait_measures`
  - Root cause: Multiple feature records per measurement (e.g., separate records for `try_dataset_id` and `try_observation_id`) were being kept as separate rows
  - Modified `pivot_features_by_type()` and `pivot_table_references()` to aggregate features by `id_trait_measures` only
  - Multiple feature values are now properly concatenated with `|` separator in single row
  - Added safety check in `pivot_measurement_features()` to ensure one row per measurement

* **Fixed missing id_col in pivoted measurement features**
  - `id_ind_meas_feat` (individuals) and `id_taxa_trait_feat` (taxa) columns were lost during pivot operations
  - Modified `pivot_measurement_features()` to pre-aggregate feature IDs separately and join back after pivoting
  - Feature IDs are now properly preserved and concatenated when multiple features exist per measurement
  - Prevents "objet 'id_ind_meas_feat' introuvable" errors in downstream code

* **Fixed census-linked measurements exclusion in `query_individual_features()`**
  - When `include_multi_census = FALSE`, measurements linked to subplots/censuses (having `id_sub_plots`) were incorrectly excluded from results
  - Now properly aggregates census-linked measurements by individual when `include_multi_census = FALSE`
  - When `include_multi_census = TRUE`, still keeps separate rows for each subplot/census
  - Affects both numeric and character trait pivoting

* **Fixed R code generation bug in Shiny app for individual features**
  - `mod_code_preview` was always generating `individuals$extract$id_n` regardless of `output_style`
  - Now correctly generates `individuals$individuals$id_n` for standard output styles
  - Only uses `individuals$extract$id_n` when `output_style = "full"`
  - Prevents "Column 'id_n' not found" errors when copying generated code

* **Optimized `merge_individuals_taxa()` performance**
  - Was loading entire `table_idtax` synonym table (could be millions of rows) causing long delays
  - Now fetches individuals first, then loads only the synonyms needed for those specific taxa
  - Also optimized specimen and specimen-link queries to filter early
  - Added detailed progress indicators at each step
  - Dramatically improves query speed for large databases

* **Fixed timeout errors for large individual features queries**
  - Count query with huge IN clauses (>10,000 measurements) was causing "SSL SYSCALL error: EOF detected"
  - Now skips count query for very large datasets to avoid timeout
  - Reduced chunking threshold from 15,000 to 5,000 for more aggressive chunking
  - Added better progress indicators throughout the query process

### New Features

* **Comprehensive permission management for import wizard**
  - New `setup_import_wizard_permissions()` - one-command setup for all import permissions
  - New `grant_all_table_permissions()` - grant on ALL existing tables and sequences
  - New `grant_plot_insert_permissions()` - grant specific table permissions
  - New `diagnose_plot_permissions()` - diagnose permission issues
  - New `diagnose_add_person_setup()` - check if secure functions are available
  - Automatic sequence discovery - finds and grants permissions on all sequences for tables
  - Handles missing tables gracefully - skips non-existent tables without failing
  - Comprehensive error messages guide users through permission setup
  - Supports granting to specific users or PUBLIC (all users)
  - RLS policies remain intact - table permissions don't affect row-level security

* **Safe plot deletion with transaction support**
  - New `safe_delete_plot()` function for safely deleting plots and all related data
  - **Dry-run mode by default** - preview what will be deleted before actually deleting
  - Shows detailed counts: individuals, trait measurements, measurement features, subplots
  - Requires explicit confirmation (can be bypassed with `force = TRUE`)
  - Uses database transactions - rolls back all changes if any step fails
  - Correct cascade deletion order respects foreign key constraints:
    1. Measurement features → 2. Trait measurements → 3. Individuals → 4. Subplots → 5. Plot
  - Detailed progress logging at each step
  - Options to keep individuals or subplots if needed
  - Returns deletion summary for verification
  - See documentation: `?safe_delete_plot`

* **Secure function for adding people without INSERT permissions**
  - New `setup_add_person_function()` creates a PostgreSQL SECURITY DEFINER function
  - Database administrators run this once to enable all users to add people to `table_colnam`
  - Users without INSERT permission can now add people through the import wizard
  - Automatic fallback: tries secure function first, then direct INSERT if available
  - Improved error messages guide users to contact admin if permissions lacking
  - New functions: `add_person_to_db()`, `grant_lookup_table_permissions()`
  - See documentation: `?setup_add_person_function`

* **Improved user experience for individual features display**
  - Removed internal `id_ind_meas_feat` column from results when metadata is included
  - Added informative note in Shiny app explaining `id_data_individuals` corresponds to `id_n` in individuals table
  - Helps users understand how to join individual features with individuals data
  - Bilingual support (English/French)

* **Long format traits table in taxonomic match Shiny app**
  - Added tabbed interface to traits enrichment module in `launch_taxonomic_match_app()`
  - Two views now available after fetching traits:
    - **Wide Format (Aggregated)**: One row per taxon with trait columns (existing functionality preserved)
    - **Long Format (Detailed)**: One row per measurement with all metadata
  - Long format automatically includes:
    - Measurement remarks (`include_remarks = TRUE`)
    - Measurement features (`include_measurement_features = TRUE`)
    - Original input names, matched names, and corrected names for traceability
    - All measurement metadata (basisofrecord, traitdescription, expectedunit, etc.)
  - Separate download buttons for each format (`.xlsx` export)
  - Full bilingual support (English/French) with 9 new translations
  - Equivalent to calling `query_taxa_traits(format = "long", include_remarks = TRUE, include_measurement_features = TRUE)`

* **Individual Features Query in Plot Query App**
  - Added optional individual-level features extraction to `launch_query_plots_app()`
  - New collapsible configuration panel for querying individual features separately using `query_individual_features()`
  - Extracts features from already-loaded individuals data (uses `id_n` column)
  - Configurable parameters:
    - Trait selection (all traits or specific trait IDs)
    - Output format (wide with aggregation or long without aggregation)
    - Multi-census data inclusion
    - Measurement metadata inclusion
    - Census strategy
  - Results displayed in separate "Individual Features" tab
  - Included in all export formats (Excel, CSV, RDS)
  - Dynamic R code generation shows equivalent `query_individual_features()` call
  - Format explanations:
    - Wide format: one row per individual, measurements as columns (aggregated if multiple observations)
    - Long format: one row per measurement (complete representation, no aggregation)
  - Full bilingual support with 30+ new translations
  - Positioned logically after individual extraction button for intuitive workflow

# CafriplotsR 1.9.1 (2026-01-18)

### New Features

* **Census Information Module for Import Wizard**
  - New Step 8 in plot metadata import wizard for adding first census information
  - Automatically detects and displays people features (team_leader, principal_investigator, etc.) from imported plot metadata
  - Auto-prefills census date fields from plot database
  - Non-interactive census creation suitable for Shiny environment
  - Directly inserts people features into `data_subplot_feat` bypassing string-to-ID conversion issues
  - Shows read-only summary of people information that will be copied to census
  - Full bilingual support (English/French) with 42 new translations

* **Taxonomic Backbone Management App**
  - New interactive Shiny app `launch_taxo_backbone_app()` for managing the taxonomic reference database
  - Browse and search taxonomic entries at all hierarchical levels (family, genus, species, infraspecific)
  - Visualize full taxonomic hierarchy using the hybrid system (flat columns + id_parent tree structure)
  - Add new taxa with automatic parent linking and hierarchy validation
  - Modify existing taxa with automatic cascade updates to maintain consistency
  - Comprehensive synonymy management:
    - Set new synonymy relationships
    - Reverse synonymy (swap synonym with accepted name)
    - Cancel synonymy (make taxon independent)
  - Hierarchy consistency checking with automatic fixing of missing parent links
  - Full bilingual support (English/French) with shiny.i18n integration

* **Cascade Update System for Taxonomic Hierarchy**
  - When modifying upper taxonomic fields (tax_fam, tax_order, tax_famclass), changes automatically cascade to all descendants
  - Detects all affected taxa via id_parent relationships (recursive traversal)
  - Shows warning modal with list of affected descendants before execution
  - Automatically finds or creates upper taxon entries (e.g., creates "Asterales" order when referenced)
  - Updates both flat taxonomic columns AND id_parent relationships atomically
  - Maintains consistency between denormalized columns and hierarchical structure
  - Transaction-safe with automatic rollback on errors

* **Enhanced Hierarchy Consistency Checking**
  - `check_hierarchy_consistency()` now detects taxa with missing parent links
  - Six types of consistency checks:
    1. Species → genus mismatch
    2. Genus → family mismatch
    3. Family → order mismatch
    4. Order → class mismatch
    5. Infraspecific → species mismatch
    6. **NEW**: Missing parent links (taxa with upper fields but no id_parent)
  - `fix = TRUE` parameter automatically resolves all detected issues
  - Finds and links appropriate parent entries for orphaned taxa

* **Search with Child Taxa Inclusion**
  - New "Include child taxa" option in taxonomic search
  - Recursively retrieves all descendants via id_parent relationships
  - Example: Search "Fabaceae" → returns family + all genera + all species in that family
  - Supports up to 10 levels of recursion for safety
  - Automatically adds traits to all retrieved child taxa

* **Comprehensive Vignettes for Backbone Management**
  - Bilingual vignettes explaining the taxonomic backbone app:
    - English: `vignettes/taxonomic-backbone-app.Rmd`
    - French: `vignettes/taxonomic-backbone-app-fr.Rmd`
  - Detailed documentation of:
    - Hybrid taxonomic system architecture
    - All app features with step-by-step workflows
    - Cascade update examples with real scenarios
    - Best practices for taxonomy management
    - Troubleshooting common issues
  - Added to pkgdown site under "Tools" section

### Bug Fixes

* **Fixed Import Wizard Mapping Reset on New File Upload**
  - Column mappings now properly reset when user uploads a new file mid-workflow
  - Detects data changes by tracking column names between uploads
  - Resets user_modified_mappings and recreates observers when data structure changes
  - Prevents "indice hors limites" errors when old mappings reference non-existent columns

* **Fixed Census Module Input Initialization Crash**
  - Added proper NULL/length checks before comparing input values
  - Uses `shiny::req()` to wait for UI inputs to initialize before accessing them
  - Prevents "l'argument est de longueur nulle" (argument is of zero length) errors on module load

* **Fixed Exact Match Search Behavior**
  - Removed automatic fuzzy fallback when `exact_match = TRUE`
  - Now returns NULL when no exact match found instead of falling back to fuzzy matching
  - Changed default to fuzzy matching (exact match checkbox unchecked by default)

* **Fixed Synonym Priority in Search Results**
  - When duplicates exist (e.g., multiple "Fabaceae" entries), accepted taxa now appear first
  - SQL queries prioritize entries where `idtax_good_n IS NULL` (accepted names)
  - Fixed PostgreSQL `SELECT DISTINCT` + `ORDER BY` compatibility issue using subqueries
  - `check_synonymy = FALSE` now properly excludes synonyms instead of just skipping resolution

* **Fixed Pool Connection Errors**
  - Fixed "Not supported for pool objects" errors in multiple modules:
    - Cancel synonymy operation
    - Modify taxon operation
    - Search with include children option
  - Added proper `con` parameter passing to `update_dico_name()` calls
  - Added `tryCatch` wrappers for `poolReturn()` to handle "already returned" errors gracefully

* **Fixed NA Value Handling in Taxonomic Operations**
  - Created `na_to_empty()` helper function for safe NA/NULL handling
  - Fixed "valeur manquante là où TRUE / FALSE est requis" errors in:
    - Taxon modification form prefill
    - Taxon field change tracking
    - Taxon display rendering
  - Fixed hierarchy visualization crashes when viewing upper taxa (family, order, class)
  - Added safe NA checks in hierarchy tree building and breadcrumb path generation
  - Created `has_value()` helper for safe field validation with NA/NULL handling

### Code Refactoring

* **Enhanced `query_subplots()` Pool Connection Support**
  - Added `con` parameter to support pool connections in Shiny environments
  - Passes connection through to `query_plot_features()` for consistent connection handling
  - Prevents unnecessary connection creation when using connection pools

* **Non-Interactive Census Creation**
  - Refactored census addition to bypass `add_subplot_observations_feat()` string-to-ID conversion
  - Two-step process: creates census records first, then directly inserts people features into `data_subplot_feat`
  - Properly structures dataframe to match `data_subplot_feat` schema with all required columns
  - Uses `typevalue` column for census number storage (not a dedicated `census` column)

* **Improved Taxonomic Hierarchy Functions**
  - `get_taxon_hierarchy()` now uses safe NA handling throughout
  - `build_hierarchy_tree_html()` safely compares IDs with explicit NA checks
  - `build_breadcrumb_path()` validates current level before comparisons

* **Enhanced Synonym Management**
  - Reverse synonym feature with automatic cascade to other synonyms
  - Direct SQL updates for atomic synonym operations
  - Warns about affected taxa before executing changes

### Documentation

* **Updated Search UI Labels**
  - Changed "Binomial search" to "Name search (any taxonomic level)"
  - Updated placeholders to show family/genus/species examples
  - Clarified that search works for all taxonomic levels, not just binomials

* **Enhanced CLAUDE.md Guidelines**
  - Documented plot data storage architecture (flat columns vs lookup columns vs features)
  - Clarified `subplotype_list` feature type categories
  - Added examples for dynamic lookup feature identification

# CafriplotsR 1.9.0 (2026-01-09)

### New Features

* **Herbarium Specimen Linking System**
  - New interactive Shiny app `launch_individual_specimen_linking_app()` for linking herbarium specimens to individual trees
  - Creates formal database-level connections between inventory individuals and herbarium specimens
  - Enables automatic taxonomic updates: when specimens are revised by taxonomists, linked individuals inherit updated taxonomy
  - Supports two link types:
    - `type_individual`: Direct evidence (specimen collected FROM this specific tree)
    - `referenced_individual`: Indirect evidence (tree field-identified as same species as specimen tree)
  - Extends specimen utility: one specimen can provide taxonomic updates to multiple field-identified trees
  - Six-step workflow: Select individuals → Parse herbarium info → Match collectors → Retrieve specimens → Validate taxonomy → Create links
  - Comprehensive taxonomic validation with family/genus/species comparison and visual indicators
  - Full bilingual support (English/French) with shiny.i18n integration

* **Specimen Linking Documentation**
  - Comprehensive bilingual vignettes explaining specimen linking workflow and scientific rationale
  - English: `vignettes/specimen_linking_workflow.Rmd`
  - French: `vignettes/specimen_linking_workflow-fr.Rmd`
  - README.md section highlighting specimen linking as key feature for long-term data quality
  - French README vignette (`vignettes/readme-fr.Rmd`) updated with specimen linking section
  - Clear explanation of the two-column system (`herbarium_nbe_type` vs `herbarium_nbe_char`)
  - Rationale for extending specimen utility while acknowledging confidence trade-offs

* **Modular Specimen Linking Architecture**
  - New R6 classes for efficient batch querying:
    - `SpecimenFilterBuilder`: Build complex specimen queries with multiple filters
    - `SpecimenFetcher`: Execute batch specimen retrieval with connection pooling
  - New Shiny modules for specimen linking workflow:
    - `mod_herbarium_parser`: Parse herbarium references from text (collector names, specimen numbers)
    - `mod_specimen_retriever`: Batch-retrieve specimens by collector and number ranges
    - `mod_taxonomic_validator`: Validate taxonomic concordance with visual indicators
    - `mod_individual_search`: Search individuals with herbarium information
    - `mod_specimen_search`: Search specimens database
    - `mod_link_preview`: Preview and review proposed links before creation
    - `mod_link_executor`: Execute batch link creation with validation
  - Reusable components support both individual-specimen and specimen-only workflows

### Performance Improvements

* **Optimized Batch Validation**
  - Link validation now uses batch queries instead of row-by-row checks
  - Performance improvement: ~200x faster (3 queries vs 639 queries for 213 links)
  - Validates all specimen IDs, individual IDs, and link type IDs in parallel
  - Eliminates app hanging during validation step

* **Optimized Specimen Retrieval**
  - Batch retrieval by collector with min/max specimen number ranges
  - Instead of N individual queries (one per specimen), makes 1 query per collector
  - Example: 222 specimens from 3 collectors = 3 queries instead of 222

### Bug Fixes

* **Fixed Taxonomic Match Classification**
  - "Same Genus" and "Same Family" categories now correctly count links
  - Previously, links where genus AND species matched were incorrectly classified as "same_genus"
  - Now properly handles synonym cases: if genus+species both match even when idtax_n differs → classified as "exact"
  - Categories are now mutually exclusive: exact → same_genus (species differs) → same_family (genus differs) → different_family

* **Fixed Taxonomic Validation for Specimen Links**
  - Dynamically constructs full taxonomic names (`full_name_no_auth`) from base columns (`tax_gen`, `tax_esp`, `tax_nam01`)
  - Handles all taxonomic levels: infraspecific, species, genus
  - Fixes "Missing columns in taxa_info" error in validation step

### User Experience Improvements

* **Opt-Out Selection System for Link Validation**
  - All links now pre-selected by default (opt-out instead of opt-in)
  - Users can uncheck links to reject rather than checking 200+ links individually
  - New selection controls:
    - "Select All" - Re-select all links
    - "Deselect All" - Clear all selections
    - "Reject Different Family" - Auto-reject links with taxonomic family mismatches
  - Visual selection status column with ✓/✗ indicators and color coding
  - Interactive table: click rows to toggle selection

* **Prerequisites Information in Linking App**
  - Prominent yellow warning box explaining prerequisites before starting
  - Clear explanation of two column types (`herbarium_nbe_type` vs `herbarium_nbe_char`)
  - Distinguishes direct evidence (high confidence) from indirect evidence (lower confidence)
  - Explains rationale for extending specimen utility across multiple trees

### Database Schema

* **New Tables and Migrations**
  - `link_individual_specimen`: Stores specimen-individual links with audit trails
  - `linktypelist`: Lookup table for link types (type_individual, referenced_individual)
  - Migration functions for adding audit columns and link type tracking
  - Functions: `run_specimen_links_migration()`, `verify_specimen_links_migration()`

---

# CafriplotsR 1.8.2 (2026-01-05)

### Bug Fixes

* **Fixed multiple Import Wizard issues for individuals import**
  - Automatic column matching now includes trait/feature columns (stem_diameter, tree_height, etc.)
  - Fixed "objet de type 'closure' non indiçable" i18n error in lookup matching step when no lookups needed
  - Fixed "Not supported for pool objects" error in taxonomy validation by using dplyr instead of DBI::dbReadTable
  - Fixed "nombre de dimensions incorrect" error in dry run by detecting import type and calling correct import function
  - Fixed preview showing unmapped columns (like multi_tiges_id) - now only shows columns actually mapped by user
  - Preview module now correctly handles individuals import list structure (individuals + features data frames)
  - Import Step 7 now properly detects and handles both plots and individuals imports

* **Fixed validation issues for flexible data import**
  - Duplicate tags within plots now trigger warnings instead of errors (allows intentional duplicates)
  - Fixed taxa table name from incorrect "taxonomic_table" to correct "table_taxa"
  - Validation now only adds truly required columns, not all possible optional columns

* **Improved database connection resilience for laptop sleep/wake cycles**
  - Added connection validation every 60 seconds to detect stale connections
  - Added onActivate callback to validate connections before use
  - Prevents "SSL SYSCALL error: EOF detected" after laptop wakes from sleep
  - Applied to both main and taxa database connection pools

### New Features

* **Added French translations for Import Wizard validation section**
  - All validation messages, progress indicators, and UI labels now translated
  - Includes error messages, summary cards, and alert messages
  - Follows i18n best practices with i18n()$t() pattern in server functions

### Dependencies

* Added `plotly` to package Imports (required by plot statistics module in shiny_app_query_plots)

---

# CafriplotsR 1.8.1 (2025-12-12)

### New Features

* **Taxonomic backbone caching system for improved performance**
  - Local caching for taxonomic backbone dramatically improves performance with slow internet
  - After first download, subsequent app launches load backbone from cache (~1 second vs 5-30 seconds)
  - Performance improvement: 10-70x faster on subsequent runs
  - Interactive modal allows choosing between cached or fresh backbone
  - Cache displays age and file size for informed decision-making
  - Cache location: platform-appropriate user cache directory via `rappdirs`
  - New exported function `delete_backbone_cache()` for manual cache clearing
  - Added `rappdirs` package dependency

* **Row-level security improvements with `created_by` migration**
  - New `created_by` column tracks which user created each plot
  - Migration function `migrate_add_created_by()` safely adds column and updates policies
  - Simplified import function: automatic privilege management
  - Users automatically get access to plots they create
  - Function `check_created_by_migration()` checks migration status

* **Enhanced `define_user_policy()` with automatic privilege grants**
  - Automatically grants necessary table privileges (SELECT, INSERT, UPDATE) when creating policies
  - Eliminates manual privilege management for administrators
  - Covers all relevant tables: plots, subplots, individuals, features, traits

* **Improved plot ID query with multi-column matching**
  - Plot retrieval now matches on multiple identifying columns beyond just `id_table_liste_plots`
  - Handles `admin_code`, `plot_code`, and `plot_name` for flexible querying
  - Reduces need to know exact internal IDs

### Bug Fixes

* **Fixed `query_plots()` ignoring provided database connections**
  - Function now properly respects `con` parameter when provided
  - Prevents unnecessary connection creation when using connection pools
  - Critical for Shiny apps using reactive database connections

* **Fixed `.link_subplotype()` missing connection parameter**
  - Function now accepts and uses provided database connection
  - Ensures transaction consistency during imports
  - Prevents connection errors in multi-step workflows

* **Fixed multiple RLS policy issues preventing imports**
  - INSERT operations now work correctly for non-admin users
  - RETURNING clause properly returns inserted plot IDs despite RLS restrictions
  - SELECT policy adjusted to allow RETURNING without exposing other users' data
  - All users can now import plots regardless of RLS configuration

* **Fixed Import Wizard lookup matcher performance issues**
  - Eliminated UI freeze when matching large lookup tables
  - Enhanced name matching with better fuzzy algorithms
  - Improved responsiveness during interactive matching

* **Fixed Import Wizard validation and conversion issues**
  - Exact-matched lookup values now properly converted to IDs before validation
  - Prevents validation errors for values that were successfully matched
  - Duplicate column mapping now detected and prevented in Step 3

* **Fixed multiple i18n reactive call errors in Import Wizard**
  - Corrected reactive i18n calls in Step 1, Step 2, and other modules
  - Fixed missing i18n parameters in module calls
  - Removed duplicate translations from translation.json

* **Fixed graphics parameter error in mapview map creation**
  - Added error handling for graphics device issues
  - Required BIOMASS >= 2.2.4 to prevent `par()` parameter errors
  - Graceful fallback when map creation fails

### Documentation

* **Added critical security warnings for database credentials**
  - CLAUDE.md now includes prominent warnings about credential management
  - Clear guidelines for using placeholder credentials in examples
  - Instructions for credential leak response

* **Updated Import Wizard documentation**
  - Vignettes restructured to feature Shiny Import Wizard prominently
  - Added i18n and translation management guidelines to CLAUDE.md

* **Added package citation information**
  - Citation section added to README files (EN and FR)
  - Updated DESCRIPTION with proper author roles and ORCID
  - Package now citable via `citation("CafriplotsR")`

### Infrastructure

* **Cleaned up version control**
  - Removed xlsx, csv, gpkg data files from tracking
  - Removed geospatial temporary and KML files
  - Removed R Markdown cache and generated files
  - Removed RStudio Connect deployment files
  - Added working .Rmd files to .gitignore to prevent credential leaks
  - Added .Rprofile to .gitignore

* **GitHub Actions workflows**
  - Added Claude Code Review workflow for automated code review
  - Added Claude PR Assistant workflow for pull request automation

### Code Refactoring

* **Improved Step 7 messaging in Import Wizard**
  - Clearer explanations of row-level security and access models
  - Better guidance on admin contact for access grants
  - Updated UI based on user feedback

---

# CafriplotsR 1.8.0 (2025-01-15)

### New Features

* **Interactive Import Wizard Shiny App - Complete 7-Step Workflow**
  - New `launch_import_wizard()` function provides comprehensive plot metadata import interface
  - Full internationalization support (English/French)
  - Reuses existing validation and import functions for consistency
  - See version 1.7.2 entries below for detailed step-by-step feature descriptions

* **Plot Statistics & Visualizations module**
  - New "Statistics" tab in `launch_query_plots_app()` with comprehensive summaries
  - Summary statistics: number of plots, individuals, species, families
  - Diameter statistics: mean, median, min, max
  - Interactive visualizations with ggplot2 + plotly:
    - Diameter distribution histogram with hover tooltips
    - Top N species composition bar chart (adjustable slider: 5-30 species)
  - Download summary statistics as CSV
  - Fully bilingual (EN/FR) with i18n support
  - Smart column mapping adapts to different output styles

* **Code preview and export features**
  - Query plots app now includes code preview for reproducibility
  - Generated R code can be copied or downloaded
  - Helps users transition from GUI to programmatic workflows

### Bug Fixes

* **Fixed census date parsing error with missing/invalid data**
  - Gracefully handles missing year/month values in census dates
  - Prevents errors when date components are NA or invalid
  - Returns NA for unparseable dates instead of crashing

### Infrastructure

* **Added stringdist package dependency**
  - Required for fuzzy string matching in import wizard
  - Fixes import wizard initialization errors

* **Updated pkgdown website**
  - Documentation website updated with latest function references
  - Added logo to package site
  - All vignettes updated with new features

---

# CafriplotsR 1.7.2 (2024-12-15)

### New Features

* **Import Wizard: Duplicate plot detection during validation**
  - New validation step checks for potential duplicate plots in database
  - Matches on method, country, and coordinates (rounded to 3 decimals ≈ 111m precision)
  - Prevents re-importing existing plots with different names (e.g., "FND32" vs "Releve32")
  - Returns warnings (not errors) with existing plot names for user awareness
  - Respects row-level security (only checks plots user can access)

* **Import Wizard: UTM coordinate detection and conversion**
  - Two-stage detection system for UTM coordinates:
    - **Stage 1 (Validation)**: Detects coordinates > 1000 with specific error message
    - **Stage 2 (Preview)**: Interactive converter with zone and hemisphere inputs
  - Uses sf package for coordinate transformation (EPSG 326XX/327XX → 4326)
  - Converts in-place and updates preview map automatically
  - Guides users to convert rather than rejecting data

* **Import Wizard: Interactive map preview in Step 6**
  - Leaflet map displays plot locations with marker clustering
  - Auto-zoom to plot extent
  - Clickable popups show plot details (name, coordinates, method, country)
  - Warnings for invalid coordinates and unusual ranges
  - Info box guides users to fix reversed lat/lon by switching column mappings
  - Helps catch common errors: reversed coordinates, wrong coordinate system

* **Interactive Import Wizard Shiny App for Plot Metadata**
  - New `launch_import_wizard()` function provides comprehensive 7-step workflow for importing plot metadata
  - **Step 1: Choose Type** - Select import type (plots or individuals)
  - **Step 2: Upload Data** - Upload Excel/CSV files or download template
  - **Step 3: Map Columns** - Intelligent fuzzy column mapping with confidence scores and descriptions
  - **Step 4: Match Lookups** - Proactive lookup matching before validation
    - Analyzes all lookup columns (method, country, people fields)
    - Identifies exact matches vs. values needing matching
    - Interactive fuzzy matching with similarity scores for all possibilities
    - Displays method descriptions when selecting matches
    - Supports comma-separated people values (e.g., "Gilles Dauby, Hugo Leblanc")
    - Each person matched individually and aggregated back
  - **Step 5: Validate** - Comprehensive data validation using matched values
    - Validates both IDs (after matching) and names (before matching)
    - Clear error reporting with actionable messages
  - **Step 6: Preview** - Preview cleaned data with readable names
    - Displays lookup names instead of IDs for user-friendly preview
    - Handles comma-separated people fields (shows aggregated names)
    - Download enriched data as Excel or CSV with human-readable values
  - **Step 7: Execute Import** - Live import with transaction support
    - Dry run mode for testing without database changes
    - Uses existing `import_plot_metadata()` function
    - Displays admin code for row-level security access
    - Copy to clipboard and download as .R file options
  - New module files: `R/mod_step1_choose_type.R`, `R/mod_step2_upload.R`, `R/mod_step3_mapping.R`, `R/mod_step4_lookup_matching.R`, `R/mod_step5_validation.R`, `R/mod_step6_preview.R`, `R/mod_step7_import.R`, `R/mod_lookup_matcher.R`
  - Reuses existing validation and import functions for consistency
  - Full connection pool support with proper cleanup

* **Plot Statistics & Visualizations module for Query Plots Shiny App**
  - New "Statistics" tab in `launch_query_plots_app()` displays comprehensive plot statistics
  - Smart column mapping system automatically adapts to different output styles (`minimal`, `standard`, `permanent_plot`, etc.)
  - Detects column names regardless of renaming (e.g., `stem_diameter`/`dbh`/`D`, `tax_sp_level`/`species`)
  - Summary statistics cards: number of plots, individuals, species (richness), families
  - Diameter statistics: mean, median, min, max (when diameter data available)
  - Interactive visualizations using `ggplot2` + `plotly`:
    - Diameter distribution histogram with hover tooltips
    - Top N species composition bar chart (adjustable slider: 5-30 species)
  - Graceful degradation: sections automatically hide when data unavailable
  - Download summary statistics as CSV
  - Fully bilingual (EN/FR) with integrated i18n support
  - New module files: `R/mod_plot_statistics.R` (UI/server functions)
  - Added 24 translation strings to `inst/translations/translation.json`
  - Designed for easy expansion (guild analysis, height-diameter plots, basal area, etc.)

* **Bilingual support for Taxonomic Matching Shiny App**
  - Implemented full internationalization using shiny.i18n package
  - French and English interfaces with instant language switching
  - French is now the default language
  - Migrated all 7 modules to use centralized i18n translation system:
    - `mod_auto_matching` - Auto matching tab
    - `mod_column_select` - Column selection
    - `mod_data_input` - Data upload/input
    - `mod_fuzzy_suggestions` - Fuzzy match suggestions
    - `mod_name_review` - Manual review interface
    - `mod_results_export` - Results export
    - `mod_traits_enrichment` - Traits enrichment
  - Added `utils_i18n.R` with `init_translator()` and translation utilities
  - Created `inst/translations/translation.json` with comprehensive translations
  - Language toggle located in top-right corner of app
  - Set via `launch_taxonomic_match_app(language = "fr")` or `language = "en"`

* **Taxonomic Matching: Backbone caching system for improved performance**
  - Added local caching for taxonomic backbone to dramatically improve performance with slow internet connections
  - After first download, subsequent app uses load backbone from local cache (~1 second) instead of downloading from database (5-30 seconds)
  - Performance improvement: 10-70x faster on subsequent runs
  - Users can choose between cached or fresh backbone via modal dialog
  - Cache displays age (e.g., "3 days ago") and file size for informed decision-making
  - Cache location: `rappdirs::user_cache_dir('CafriplotsR')` (platform-appropriate)
  - New exported function `delete_backbone_cache()` allows manual cache clearing
  - Added `rappdirs` package dependency

* **Taxonomic Matching: Improved similarity threshold UI**
  - Similarity threshold now displays as percentage (0-100%) instead of decimal (0-1)
  - Default value: 60% (previously 0.6)
  - More explicit help text: "Minimum similarity percentage for fuzzy matching. Names with similarity below this threshold will not be matched. Higher values = more strict matching (fewer but more accurate matches)."
  - Label updated to "Minimum similarity (%)" for clarity
  - Internal calculations still use decimal format (0-1) for compatibility with matching algorithms

### Bug Fixes

* **Taxonomic Matching: Fixed progress bar not updating after manual review**
  - Progress bar calculation now correctly includes manually reviewed names in completion percentage
  - Previously only counted automatically matched names (exact/genus/fuzzy), causing progress to appear stuck at ~60% even after reviewing all remaining names
  - Connected progress tracker to review module results instead of auto-matching results
  - Added "Manually reviewed" line (displayed in green) to progress breakdown when manual reviews are performed
  - Progress now correctly shows 100% when all names are either auto-matched or manually reviewed

* **Import Wizard: Fixed mixed IDs and names in people column preview**
  - Preview was showing mix of numeric IDs (169, 85) and text names (Théophile Ayol)
  - Now processes each value individually in comma-separated lists
  - Converts numeric IDs to names, keeps text names as-is
  - Applies to all lookup columns (method, country, people fields)

* **Import Wizard: Fixed skipped columns still appearing in preview**
  - Columns set to "skip" in column mapping were still appearing in data preview
  - Now properly filters out NA mappings in both validation and preview steps
  - Removed skipped columns from data before processing

* **Import Wizard: Fixed mapping state not persisting on navigation**
  - Manual changes (e.g., skipping columns) were lost when navigating back
  - Added persistent `user_modified_mappings` ReactiveVal to track changes
  - Observers now track dropdown changes without cascade triggering
  - Returns both `mappings` (no NAs) and `mappings_with_skips` (includes NAs)

* **Fixed import failing with unmapped columns**
  - Import now filters out columns that were skipped during column mapping
  - Prevents "ERROR: column 'X' of relation 'data_liste_plots' does not exist"
  - Only mapped columns are included in database insertion

* **Fixed type mismatch error when joining subplot features**
  - Added explicit character type conversion for `plot_name` before joins
  - Prevents "Can't join due to incompatible types" errors
  - Applies to both people features and other subplot features

* **Fixed `poolWithTransaction()` warning in Shiny app imports**
  - Import now properly handles connection pools by checking out dedicated connections
  - Uses `pool::poolCheckout()` and `pool::poolReturn()` for proper pool management
  - Eliminates "Please use `poolWithTransaction()` instead" warnings

* **Fixed validation errors after lookup matching**
  - Validation now correctly handles both IDs (after Step 4 matching) and names (before matching)
  - Detects if values are numeric (IDs) or character (names) and validates accordingly
  - Applies to method, country, and people column validation

* **Fixed preview and downloads showing IDs instead of names**
  - Preview now enriches lookup columns by replacing IDs with readable names
  - Method IDs → Method names (e.g., 18 → "Plot_40x40")
  - Country IDs → Country names (e.g., 5 → "Cameroon")
  - People IDs → People names with comma-separated aggregation
  - Excel and CSV downloads export enriched data with names

* **Fixed `query_individual_features()` ignoring `trait_ids` parameter with large datasets**
  - When querying more than 1000 individuals, the chunking mechanism was not passing `trait_ids` filter
  - This caused all traits to be returned instead of only the requested ones
  - Fixed by passing `trait_ids` parameter through `fetch_with_chunking()` to `build_trait_query()`
  - Affects `individual_features_function.R:1319` and `individual_features_function.R:1270`

### Documentation

* **Updated Taxonomic App vignettes for bilingual support**
  - English vignette (`taxonomic-app.Rmd`) now documents language switching feature
  - French vignette (`taxonomic-app-fr.Rmd`) includes parallel documentation
  - Documented EN/FR toggle button usage
  - Documented programmatic language selection
  - Noted French as default language

### Code Refactoring

* **Removed obsolete `table_taxa_tb` dataset**
  - Deleted `data/table_taxa_tb.RData` and associated documentation
  - Data is no longer needed as taxonomy queries now use direct database connections
  - Reduced package size and maintenance burden

---

# CafriplotsR 1.7.1 (2025-11-18)

### New Features

* **New `get_user_accessible_plots()` function**
  - Extracts plot IDs that a user can access based on row-level security policies
  - Parses policy expressions to return clean vector of accessible plot IDs
  - Useful for checking user permissions and debugging access issues

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`


* **Fixed `list_user_policies()` returning empty results**
  - Added `::name` type cast for proper comparison with PostgreSQL `name[]` array
  - Function now correctly filters policies by username

### Documentation

* **New vignette: "Database Connections Guide"**
  - Complete guide to database connection workflows
  - Explains both `call.mydb()` and `call.mydb.taxa()` functions
  - Credential management options (interactive, .Renviron, direct)
  - Connection cleanup best practices with `cleanup_connections()`
  - Row-level security and checking plot accessibility
  - Troubleshooting common connection issues
  - Best practices summary for users

---

# CafriplotsR 1.7 (2025-01-13)

### New Features

* **Interactive Shiny app for plot querying and data extraction**
  - New `launch_query_plots_app()` function provides user-friendly interface for `query_plots()`
  - Two-stage workflow: (1) filter and discover plots, (2) select and extract individual data
  - Filter interface: query by country, plot name, locality, method, tags, and IDs (including comma-separated values)
  - Interactive leaflet map with multiple basemaps showing plot locations
  - Metadata table viewer with sortable/searchable columns
  - Plot selection: all plots selected by default, users can deselect specific plots
  - Configurable extraction options: output styles, census strategies, data organization, trait extraction
  - Results viewer with dynamic tabs for each data table
  - Multi-format download: Excel (.xlsx), CSV (zipped), R object (.rds), and shapefile (.zip) formats
  - Row-level security aware: filter options respect user's database access permissions
  - Modular architecture with dedicated UI/server modules for extensibility
  - Database login integration with support for saved credentials from .Renviron

* **Complete individual tree data import workflow**
  - New `import_individual_data()` function with transaction-based imports and automatic rollback on errors
  - Interactive column mapping with `map_individual_columns()` - automatically matches user columns to database schema
  - Comprehensive validation with `validate_individual_data()` - checks plots, taxonomy, tags, traits before import
  - Template generation with `get_individual_template()` - creates Excel templates with guidance
  - Dry-run mode to preview imports without committing changes
  - Support for both flat table and two-table (individuals + features) data structures
  - Auto-generates sequential tags when missing
  - Imports into `data_individuals` and `data_traits_measures` tables
  - See new vignette "Importing Plot Data into the Database" for complete workflow

* **Intelligent column mapping system**
  - Fuzzy matching of user column names to database columns and traits
  - Interactive classification: feature/trait vs individual identification columns
  - Manual selection with ranked suggestions based on similarity scores
  - Synonym support for common column name variations
  - Automatic detection of linking columns (plot_name, tag)
  - Mapping audit trail preserved for reproducibility

* **Comprehensive data validation before import**
  - Required columns validation (plot_name, idtax_n)
  - Plot existence verification with exact name matching
  - Taxonomy ID validation against database
  - Tag uniqueness within plots
  - Tag conflict detection with existing database records
  - Trait value validation (numeric vs categorical)
  - Feature-to-individual linkage verification
  - Method-specific requirements validation
  - Detailed error reporting with actionable messages

* **`query_plots()` exact name matching**
  - New `exact_match` parameter (default FALSE) for precise plot name filtering
  - Prevents unintended pattern matching (e.g., "41" matching "Plot-41", "4100")
  - Uses SQL IN clause for exact matching vs LIKE for pattern matching
  - Applied throughout PlotFilterBuilder pipeline

* **Taxonomic matching app: Class-level taxonomic support**
  - Now recognizes and matches class-level taxa (e.g., names ending in -opsida, -psida)
  - Searches in `tax_famclass` column for class names
  - Both exact and fuzzy matching supported for classes
  - Expands hierarchical matching beyond family/order/genus/species

* **Taxonomic matching app: Improved large dataset handling**
  - Excel file reading now uses `guess_max = 30000` for better column type detection
  - Prevents type mismatches when taxonomic names appear late in large datasets
  - Ensures consistent data type inference across entire dataset

### Documentation

* **New vignette: "Importing Plot Data into the Database"**
  - Complete workflow from plot metadata to individual tree data
  - Step-by-step examples with expected output
  - Interactive and programmatic workflows
  - Common issues and troubleshooting guide
  - Best practices for data import
  - Advanced topics: custom column synonyms

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`


* **Fixed `query_plots()` with `output_style` throwing errors on missing columns**
  - Changed column selection from `all_of()` to `any_of()` in output style transformations
  - Functions now gracefully handle missing columns instead of throwing errors
  - Applies to `.extract_metadata_table()`, `.extract_individuals_table()`, and `.extract_height_diameter_pairs()`
  - Dynamic column selection for `height_of_stem_diameter` (POM) when creating height-diameter pairs
  - Output styles (`permanent_plot`, `standard`, etc.) now work reliably with varying data structures

* **Fixed `.find_cat()` return value handling in column mapping**
  - Interactive column selection was returning wrong columns due to table reordering
  - Now correctly extracts selected value from `result$sorted_matches` instead of original table
  - Applies to both individual column and trait column selection

* **Fixed traits_list() column name**
  - Changed `description` to `traitdescription` to match actual column name
  - Prevents errors during trait column display

* **Fixed tag propagation from individuals to features**
  - Auto-generated tags now correctly synced to features sheet during validation
  - Ensures features can link to individuals via tag column

### Infrastructure

* **`query_plots()` improvements for Shiny integration**
  - New `con` parameter accepts optional database connection (defaults to `call.mydb()` if NULL)
  - Enables Shiny apps to pass reactive connection pools without triggering reactive context errors
  - Consistent `metadata` naming in return list regardless of `output_style` (previously `meta_data` for "full" style, `metadata` for others)
  - Ensures predictable list structure for programmatic access

* **Improved package dependency management**
  - Moved `getPass` and `dm` from Imports to Suggests
  - Reduces installation requirements - only needed for specific optional features
  - `getPass`: Used only for secure password prompts (has fallbacks to rstudioapi and readline)
  - `dm`: Used only for database structure visualization with `get_database_fk()`
  - Both packages now checked with `requireNamespace()` before use with helpful error messages
  - Fixes installation errors for users without these packages: "ERROR: dependencies 'getPass', 'dm' are not available"

### Breaking Changes

* **Taxonomic matching app: Stricter default similarity threshold**
  - Default `min_similarity` increased from 0.3 to 0.7 in `launch_taxonomic_match_app()`
  - Reduces false positive matches by requiring higher similarity scores
  - Previous behavior available by setting `min_similarity = 0.3` explicitly
  - **Action required**: Users relying on low-quality fuzzy matches may need to adjust threshold or improve input data quality
  - Rationale: Quality over quantity - fewer but more reliable matches improve data integrity

# CafriplotsR 1.5

### New Features

* **Interactive validation with fuzzy matching for plot metadata import**
  - `validate_plot_metadata()` now has `interactive = TRUE` and `fix_on_fly = TRUE` parameters (both default to TRUE)
  - Integrates with existing `resolve_multiple_values()` for on-the-fly fixing of lookup mismatches (Country, Method)
  - Returns enhanced structure with three data versions:
    - `original_data`: Unchanged user input
    - `cleaned_data`: Data with interactive fixes applied
    - `changes_made`: Complete audit trail of all corrections (column, row, original, corrected, method)
  - Eliminates tedious manual Excel editing - users interactively match mismatches (e.g., "Cameroun" → "CAMEROON") with fuzzy suggestions
  - Pattern search ("G" option) available for large lookup tables
  - Non-breaking: Old code works but gets enhanced behavior automatically

* **Complete subplot features import system**
  - Plot import now handles ALL subplot feature types, not just people features
  - New `.extract_and_process_subplot_features()` dynamically queries `subplot_list()` to identify all subplot features
  - Automatically separates into two categories:
    - People features (`valuetype == "table_colnam"`): Linked to `table_colnam` via `.link_colnam()`
    - Other features (numeric, character, etc.): Direct value insertion
  - No hardcoded feature lists - fully dynamic based on database schema
  - Identifies subplot features by excluding flat table columns (plot_name, ddlat, ddlon, elevation, etc.)
  - Both types inserted as subplot features in Step 6 of import workflow

* **Row-Level Security (RLS) safe plot import for non-admin users**
  - Uses PostgreSQL `INSERT ... RETURNING` clause to retrieve plot IDs during insertion
  - Bypasses RLS SELECT restrictions that would prevent non-admin users from reading their own inserted plots
  - Enables subplot features to be linked even when user doesn't have SELECT permission yet
  - More secure than alternative approaches (no exposure of other users' plot IDs)
  - Critical fix: Previously, non-admin imports would fail at Step 6 (subplot features) with empty plot_id_data

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`


* **Restored missing helper functions** accidentally commented out
  - `.rename_data()` (R/helpers.R:307) - Renames columns in datasets
  - `.add_modif_field()` (R/helpers.R:283) - Adds modification date fields (date_modif_d/m/y)
  - Both functions now properly exported and available
  - Fixes errors: "impossible de trouver la fonction .rename_data" and ".add_modif_field"

* **Fixed transaction connection management throughout import workflow**
  - `try_open_postgres_table()` now properly handles errors and maintains connection scope
  - `.link_table()` now uses passed `db_connection` parameter instead of creating new connection
  - `.link_colnam()` now uses passed `db_connection` parameter instead of creating new connection
  - `add_subplot_features()` added `con` parameter to accept transaction connection
  - All functions now respect transaction boundaries (no more "Invalid connection" errors)
  - Prevents connection invalidation during multi-step import process

* **Fixed invalid cli package parameter**
  - Removed unsupported `line = 2` parameter from `cli::cli_rule()` calls in import success messages
  - Fixes error: "argument inutilisé (line = 2)"

### Code Refactoring

* **Renamed and expanded subplot features processing**
  - `.extract_and_link_people()` → `.extract_and_process_subplot_features()`
  - Function now handles all subplot feature types, not just people features
  - Enhanced documentation reflects expanded scope and hierarchical processing logic

### Breaking Changes

* **`query_plots()` now returns a list by default instead of a flat data frame**
  - Output is automatically structured based on inventory method using the new output styles system
  - Different styles organize data into separate tables: metadata, individuals, censuses, height-diameter, etc.
  - **Action required**: To preserve old behavior (flat data frame), use `output_style = "full"`
  - Rationale: Structured output makes it easier to work with complex plot data without overwhelming column counts
  - See documentation for `?query_plots` for details on available output styles

### New Features

* **Census selection strategy for multi-census plots**
  - New `census_strategy` parameter in `query_plots()` with three options:
    - `"last"` (default): Extract data from most recent census only
    - `"first"`: Extract data from earliest census only
    - `"mean"`: Average across all censuses (previous default behavior)
  - When using "first" or "last" strategy:
    - Individuals recruited after first census show NA values (biologically correct)
    - Individuals dead before last census show NA values (biologically correct)
    - Single `census_date` column shows the date of the selected census (instead of `date_census_1`, `date_census_2`, etc.)
  - Census selection based on actual census dates using proper date computation
  - Applies to individual-level features (stem diameter, tree height, etc.)
  - When `show_multiple_census = TRUE`, all census data shown regardless of strategy

* **Configurable output styles system for `query_plots()`**
  - 6 predefined output styles: `minimal`, `standard`, `permanent_plot`, `permanent_plot_multi_census`, `transect`, `full`
  - Auto-detection of appropriate style based on `method` field (e.g., "1 ha plot" → `permanent_plot`)
  - Manual style selection via `output_style` parameter
  - Each style returns a structured list with relevant tables (e.g., `$metadata`, `$individuals`, `$censuses`)
  - Column renaming from database names to user-friendly names (e.g., `ddlat` → `latitude`, `tax_sp_level` → `species`)
  - New configuration files: `R/output_styles_config.R`, `R/output_styles_helpers.R`

* **Specialized output tables for permanent plots**
  - `$censuses` table: plot_name, census_number, census_date, team_leader, principal_investigator
  - `$height_diameter` table: Paired height-diameter measurements (id_n, D, H, POM) with issue filtering
  - Handles multiple censuses with automatic pivoting from wide to long format
  - Census-specific column renaming (e.g., `stem_diameter_census_1` → `dbh_census_1`)

* **Custom print method for query results**
  - New S3 class `plot_query_list` with informative print method
  - Shows table dimensions, column names, and geometry type for sf objects
  - Makes it easy to understand query result structure

* **Preservation of spatial data**
  - `coordinates_sf` table automatically included when `show_all_coordinates = TRUE`
  - Print method detects and displays sf geometry information

### Code Refactoring

* **Modular output style configuration**
  - Centralized style definitions in `.plot_output_styles` list
  - Method-to-style mapping in `.method_to_style_map`
  - Style auto-detection function `.detect_style_from_method()`
  - Easy to add new output styles by extending configuration

* **Improved metadata extraction**
  - Uses `res_meta_data` table (created before individual extraction) for metadata source
  - Ensures all plot-level columns available even when `extract_individuals = TRUE`
  - Consistent variable naming and error handling

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`


* **Fixed commented `@export` tag causing roxygen2 errors**
  - Removed `@export` from commented-out `subplot_list()` function in `R/subsplots_features_function.R`
  - Prevents documentation build failures

# CafriplotsR 1.4 (development version)

### New Features

* **Traits enrichment module in taxonomic matching Shiny app**
  - New tab "Enrich with Traits" allows enriching matched taxonomic names with trait data from the taxa database
  - Aggregates multiple input names that match to the same taxon into a single row
  - Concatenates all input names (e.g., "cola edulis | coula edrulis" → "Coula edulis")
  - Configurable options for categorical trait aggregation (mode vs concatenation)
  - User can select which columns to include (original names, corrected names, IDs, metadata)
  - Downloads enriched data as Excel file
  - Filters out `id_trait_measures` columns for cleaner output
  - Module: `mod_traits_enrichment_ui()` and `mod_traits_enrichment_server()`

* **Enhanced file upload in taxonomic matching Shiny app**
  - CSV file support added (in addition to Excel .xlsx and .xls)
  - Excel sheet selector allows choosing which sheet to import from multi-sheet workbooks
  - Sheet selector appears dynamically after Excel file upload
  - Default sheet selection is the first sheet
  - CSV files are loaded directly without sheet selection

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`


* **Fixed NA input names appearing in trait enrichment**
  - Enrichment module now filters out rows where the input taxonomic name is NA or empty
  - Prevents invalid NA entries from being matched to taxa or included in enriched output
  - Applied in both trait fetching and result aggregation steps

* **Fixed incorrect input names in enrichment output**
  - Enrichment now correctly uses the user-selected taxonomic name column (not first column of dataset)
  - `column_name` parameter now passed from main app to enrichment module
  - Ensures `input_names` column shows actual taxonomic names from the selected column

### Code Refactoring

* **Optimized taxonomic name cleaning for faster matching**
  - Name cleaning (removing "sp.", "cf.", "aff.", etc.) now happens **before** batch exact matching
  - Previously, cleaning only occurred during slow fuzzy matching phase
  - Names like "Coula edulis sp." now match exactly to "Coula edulis" in fast batch step
  - Significantly reduces number of names sent to slower fuzzy matching
  - Cleaning happens once at beginning, benefiting all matching steps (species, genus, family)
  - Both original and cleaned names preserved in matching pipeline
  - Added underscore replacement in `clean_taxonomic_name()` (e.g., "Coula_edulis" → "Coula edulis")

### Breaking Changes

* **`query_taxa()` default behavior changed**: `exact_match` parameter now defaults to `TRUE` (was `FALSE`)
  - Exact matching is now the default for family/genus/order queries to prevent unexpected fuzzy matching results
  - For species queries, if exact match fails, the function automatically falls back to intelligent fuzzy matching
  - **Action required**: Code relying on fuzzy matching by default should explicitly set `exact_match = FALSE`
  - Rationale: Higher taxonomic ranks are standardized names where fuzzy matching rarely helps and can introduce errors

### New Features

* **Intelligent taxonomic name matching** with genus-constrained fuzzy search
  - New `match_taxonomic_names()` function implements hierarchical matching strategy:
    1. Exact matching (fastest)
    2. Genus-constrained fuzzy matching (searches species only within matched genus)
    3. Full fuzzy matching (last resort)
  - Dramatically improves match quality by restricting fuzzy search space
  - Includes synonym detection and resolution
  - Supports scoring and ranking of multiple matches
  - New helper functions: `parse_taxonomic_name()`, `.match_exact_sql()`, `.match_genus_constrained_sql()`, `.match_fuzzy_sql()`

* **Auto fuzzy fallback for species queries**
  - `query_taxa()` automatically retries with fuzzy matching when exact species match fails
  - Transparent user feedback shows match quality (similarity score)
  - Handles typos and spelling variations automatically
  - Only applies to species queries; family/genus/order use exact matching only

* **Database enhancement: `tax_level` field added to `table_taxa`**
  - New column explicitly indicates taxonomic level: "species", "genus", "family", "order", "infraspecific", "higher"
  - Indexed for query performance
  - Eliminates ambiguity between missing data and genus/family-level taxa
  - Script provided: `add_tax_level_field.R` for database migration
  - All query functions updated to use new field for cleaner, more reliable filtering

### Code Refactoring

* **Complete rewrite of `query_taxa()`** to use new intelligent matching functions
  - Eliminated redundancy with `helpers.R` functions
  - 8 new modular helper functions replace complex inline logic
  - Cleaner separation of concerns: matching, filtering, synonym resolution, formatting, trait addition
  - ~160 lines of code removed through better abstraction
  - Better maintainability and extensibility
  - Deprecated `query_fuzzy_match()` and `query_exact_match()` in favor of `match_taxonomic_names()`

* **Simplified taxonomic level filtering** using `tax_level` field
  - Replaced complex multi-column checks (e.g., `is.na(tax_esp) & is.na(tax_gen)`) with simple `tax_level == "family"`
  - Applied in `query_taxa()` for clearer intent and better performance via index usage

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`


* **Fixed `query_taxa()` empty results with `only_family = TRUE`**
  - Previously, fuzzy matching by default caused empty results when filtering for family-level taxa
  - Now uses exact matching by default for higher taxonomic ranks

### Dependencies

* Added new package dependencies to DESCRIPTION:
  - `cli` - User-friendly command line interfaces (moved from Suggests to Imports)
  - `lifecycle` - Manage function lifecycle (deprecation warnings)
  - `data.table` - High-performance data manipulation
  - `glue` - String interpolation for SQL queries
  - `RecordLinkage` - String similarity calculations

# CafriplotsR 1.0

### Breaking Changes
* **Database schema change**: Renamed column `ind_num_sous_plot` to `tag` in `data_individuals` and `followup_updates_individuals` tables
  - All R package functions updated to use new column name
  - **Action required**: External scripts accessing `ind_num_sous_plot` must be updated to use `tag`
  - Updated files: `R/functions_manip_db.R`, `R/individual_features_function.R`, `R/functions_divid_plot.R`, `R/generate_plot_summary.Rmd`, `structure.yml`
  - Default parameter in `approximate_isolated_xy()` changed from `tag = "ind_num_sous_plot"` to `tag = "tag"`

### New Features
* Initial release of package structure with comprehensive database query functions
* **Enhanced `update_ident_specimens()`**: Now shows summary of linked individuals before updating specimen identification
  - Displays which plots and how many individuals will inherit the new identification
  - Shows current taxonomic identification of linked individuals
  - Provides better context for informed decision-making before confirmation
  - New helper function `.get_linked_individuals_summary()` queries and summarizes impact

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`

* **Connection error with complex home paths**: Fixed `create_db_config()` function that failed when home directory path contained spaces or special characters (e.g., OneDrive paths like `C:/Users/NOBUS CAPITAL/OneDrive/Documents/`)
  - Added proper error handling with `tryCatch()` for file creation
  - Creates parent directories if they don't exist
  - Falls back to in-memory configuration if file cannot be written
  - Users now get informative warnings instead of connection failures

### Documentation
* Added comprehensive README.md with package overview, quick start guide, and function reference
* README includes prominent link to NEWS.md for tracking updates

### Infrastructure
* Added NEWS.md to track package changes and updates
* Established git branching workflow for all code modifications

### Code Refactoring
* **Major refactoring**: Reorganized `R/functions_manip_db.R` (previously 10,528 lines) into modular, domain-specific files
  - Created `R/growth_census_functions.R` (556 lines) - Growth computation and census analysis functions
  - Created `R/specimen_linking_functions.R` (406 lines) - Herbarium specimen linking and querying functions
  - Created `R/taxonomic_query_functions.R` (944 lines) - Taxonomic query functions with synonym resolution
  - Created `R/taxonomic_update_functions.R` (838 lines) - Taxonomic data update and entry functions
  - Expanded `R/connections_db.R` with database query utilities (`func_try_fetch`, `try_open_postgres_table`)
  - Removed ~6,800 lines from `R/functions_manip_db.R` through extraction to specialized modules
  - All functions verified as moved (not duplicated) to new locations
  - Improved code maintainability and discoverability

---

