# plotsdatabase 1.7 (2025-01-13)

### New Features

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

# plotsdatabase 1.5

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

* **Fixed commented `@export` tag causing roxygen2 errors**
  - Removed `@export` from commented-out `subplot_list()` function in `R/subsplots_features_function.R`
  - Prevents documentation build failures

# plotsdatabase 1.4 (development version)

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

# plotsdatabase 1.0

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

