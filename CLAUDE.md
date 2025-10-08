# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Workflow

**IMPORTANT: Always use feature branches for code changes**

When making any code modifications, follow this workflow:

1. **Create a feature branch** before making changes:
   - Use descriptive branch names (e.g., `feature/add-new-query`, `fix/connection-bug`)

2. **Make updates** on the feature branch

3. **Update NEWS.md** with changes:
   - Add entry under the current development version section
   - Categorize changes appropriately:
     - **Breaking Changes**: Changes that break backward compatibility
     - **New Features**: New functionality added
     - **Bug Fixes**: Fixes for existing issues
     - **Documentation**: Documentation improvements
     - **Infrastructure**: Development/build process changes
   - Use bullet points with clear, concise descriptions
   - Reference issue numbers or PR numbers if applicable

4. **Commit changes** with clear, descriptive commit messages

5. **Create a pull request** (optional, based on user request) back to master

6. **After merging to master**: Ensure NEWS.md reflects all changes from the merged branch

**Never commit directly to master unless explicitly instructed by the user.**

### NEWS.md Format

The NEWS.md file follows standard R package changelog format:

```markdown
# plotsdatabase NEWS

## plotsdatabase X.Y (Development/Release Date)

### Breaking Changes
* Description of breaking change

### New Features
* Description of new feature

### Bug Fixes
* Description of bug fix

### Documentation
* Documentation improvements

### Infrastructure
* Development process changes
```

## Repository Overview

This is `plotsdatabase`, an R package for exploring and updating a PostgreSQL database containing Central African forest plot and transect data. The package provides functions for querying plot data, individual tree measurements, taxonomic traits, and various ecological features.

**What this project does:**
- Query and analyze forest plot data from Central Africa
- Access individual tree measurements and ecological features
- Retrieve and aggregate species-level taxonomic traits
- Update database tables with new measurements and observations
- Resolve taxonomic synonyms and link trait data across hierarchy levels

**Access is restricted** - requires database credentials.

## Technology Stack

**Core Technologies:**
- **R** (≥4.0) - Primary programming language
- **PostgreSQL** - Two databases: `plots_transects` (main) and `rainbio` (taxa)

**Key R Package Dependencies:**
- **DBI** - Database interface abstraction
- **RPostgres** - PostgreSQL connector
- **data.table** - High-performance data manipulation

**Development Tools:**
- **roxygen2** (v7.3.2) - Automatic documentation generation from code comments
- **devtools** - Package development workflow
- **RStudio** - Recommended IDE

**Documentation & Reporting:**
- **RMarkdown** - Interactive tutorials and reports
- **knitr** - Document rendering

**Data Formats:**
- Excel (`.xlsx`) - Data imports/exports
- GeoPackage (`.gpkg`) - Spatial/geographic data
- CSV - Tabular data exchange
- KML - Geographic visualization

**Architecture Patterns:**
- Connection pooling for database efficiency
- Row-level security policies for access control
- Session-based credential caching
- Layered query architecture (connection → fetch → enrich → aggregate → query)

## Core Architecture

### Database Connections

The package manages connections to **two separate PostgreSQL databases**:

1. **Main database** (`plots_transects`): Contains plot, subplot, and individual tree data
2. **Taxa database** (`rainbio`): Contains taxonomic information and species-level traits (READ-ONLY for most users)

**Connection management** (`R/connections_db.R`):
- Connections are stored in internal environment `.db_env`
- Use `call.mydb()` for main database, `call.mydb.taxa()` for taxa database
- Credentials cached in `credentials` environment during session
- Connection pooling: functions check for existing valid connections before creating new ones
- Use `cleanup_connections()` to close all connections and clear credentials

**Credential setup options**:
1. Interactive prompts (default)
2. Store in `~/.Renviron` via `setup_db_credentials()` (WARNING: plaintext storage)
3. Pass directly to connection functions

### Data Query Architecture

The package follows a **layered query architecture**:

1. **Connection layer**: `call.mydb()`, `call.mydb.taxa()`
2. **Fetching layer**: Low-level SQL queries (e.g., `fetch_taxa_trait_measurements()`, `fetch_subplot_features()`)
3. **Enrichment layer**: Add related data (e.g., `enrich_with_taxa_info()`, `enrich_individual_traits()`)
4. **Aggregation layer**: Pivot/aggregate features (e.g., `aggregate_plot_features()`, `pivot_numeric_traits_generic()`)
5. **Query layer**: High-level user-facing functions (e.g., `query_taxa_traits()`, `query_plot_features()`)

### Key Data Structures

**Plot hierarchy**:
- `data_liste_plots` → `data_liste_sub_plots` → `data_ind_measures` (individual trees)
- Plots contain subplots (census points, soil samples, etc.)
- Subplots contain observations with features

**Traits hierarchy**:
- Taxa-level: `table_traits_measures` (species traits in taxa DB)
- Individual-level: `data_ind_measures_feat` (tree-level measurements in main DB)
- Both link to `table_traits` / `traitlist` for trait definitions

**Synonym resolution**:
- Taxa database contains `idtax_n` (taxon ID) and `idtax_good_n` (accepted taxon ID)
- Use `resolve_taxon_synonyms()` to consolidate traits from synonyms

**Database schema visualization**:
- Use `get_database_fk()` (in `R/database_structure.R`) to visualize database structure
- Generates diagram showing primary keys, foreign keys, and relationships between main tables
- Covers key tables: plots, subplots, individuals, traits, and lookup tables
- Requires `dm` package for relationship discovery and visualization

## Development Commands

### Building and Checking
```r
# Load package for development
devtools::load_all()

# Document functions (generate .Rd files from roxygen)
devtools::document()

# Check package
devtools::check()

# Install locally
devtools::install()
```

### Database Connection Testing
```r
# Test connections
library(plotsdatabase)
db_diagnostic()  # Full diagnostic
print_connection_status()  # Quick status check

# Connect to databases
con_main <- call.mydb()
con_taxa <- call.mydb.taxa()

# Visualize database structure
get_database_fk(con_main)  # Shows primary/foreign key relationships
```

### Common Query Examples
```r
# Query plots
plots <- query_plots(plot_ids = c(1, 2, 3))

# Query taxa-level traits
traits <- query_taxa_traits(
  idtax = c(12345, 67890),
  format = "wide",
  add_taxa_info = TRUE
)

# Query individual tree features
indiv <- query_individual_features(
  plot_ids = 1,
  trait_ids = c(1, 2),  # Specific traits
  format = "wide"
)
```

## Repository Structure

### Folder Organization

**`R/`** - Source code (all R function definitions)
- `connections_db.R` - Database connection management, credentials, diagnostics
- `individual_features_function.R` - Individual tree queries and measurements
- `subsplots_features_function.R` - Plot/subplot features and aggregation
- `taxa_traits_function.R` - Species-level trait queries from taxa database
- `query_plots_v2_base.R` - Plot querying with advanced filtering
- `updates_tables_functions.R` - Database update/insert operations
- `link_table_functions.R` - Interactive data matching and linking
- `helpers_traits_common.R` - Generic aggregation functions
- `functions_manip_db.R` - Data processing helpers (335KB+, extensive utilities)

**`man/`** - Documentation files (`.Rd` format)
- Auto-generated from roxygen2 comments via `devtools::document()`
- One `.Rd` file per exported function
- Do not edit manually - modify roxygen comments in `R/` files instead

**Root directory:**
- `DESCRIPTION` - Package metadata (version, authors, dependencies)
- `NAMESPACE` - Exported functions list (auto-generated)
- `.Rproj` - RStudio project configuration
- `.Rmd` files - Tutorials and usage examples (e.g., `tuto_database.Rmd`, `tuto_db.Rmd`)
- `CLAUDE.md` - This file (instructions for Claude Code)
- `NEWS.md` - Version history and changelog
- Data files (`.xlsx`, `.gpkg`, `.csv`) - Example/test datasets and analysis outputs

### Primary Function Categories

**Connection & Utilities**:
- `R/connections_db.R`: Database connection management, credential handling, diagnostics
- `R/database_structure.R`: Database schema visualization (`get_database_fk()`)

**Querying Functions**:
- `R/individual_features_function.R`: Individual tree measurements and traits
- `R/subsplots_features_function.R`: Plot-level features and subplot data
- `R/taxa_traits_function.R`: Species-level trait queries from taxa database
- `R/query_plots_v2_base.R`: Plot querying with filtering

**Update Functions** (`R/updates_tables_functions.R`):
- Adding new data to database tables
- Functions like `add_sp_traits_measures()`, `add_trait_taxa()`

**Linking & Matching** (`R/link_table_functions.R`):
- `.link_table()`: Interactive matching of values to lookup tables
- `.find_cat()`: Fuzzy matching for categorical values

**Manipulation & Helpers**:
- `R/functions_manip_db.R`: Data processing utilities
- `R/helpers_traits_common.R`: Generic aggregation functions

### Coding Conventions

**Helper Functions Pattern:**
- Internal helpers use `.` prefix (e.g., `.link_table()`, `.add_modif_field()`, `.rename_data()`)
- These are exported but intended for internal use or advanced users
- Public-facing functions have descriptive names without prefix

**Generic Aggregation Functions:**
The package has refactored generic aggregation helpers in `R/helpers_traits_common.R`:
- `pivot_numeric_traits_generic()`: Aggregate numeric traits with mean/sd/n
- `pivot_categorical_traits_generic()`: Aggregate categorical traits (mode or concat)
- `aggregate_plot_features()`: Aggregate multiple feature types for plots

**Standard R Package Structure:**
- Documentation lives in `man/` (generated, not manually edited)
- Source code lives in `R/`
- No `tests/` directory present (consider adding for production robustness)
- No `vignettes/` directory (tutorials are in root as `.Rmd` files)

## Important Notes

### Database Write Operations

- **Taxa database is READ-ONLY** for most users (verified on connect)
- Write operations on main database require appropriate user permissions
- Use `define_user_policy()` for row-level security setup
- Updates typically require `add_data = TRUE` parameter and user confirmation

### Trait Value Types

Traits have `valuetype` field:
- `"numeric"`: Stored in `traitvalue` column
- `"categorical"`: Stored in `traitvalue_char` column
- `"ordinal"`, `"character"`: Also use `traitvalue_char`
- `"integer"`: Uses `traitvalue` (numeric)
- `"table_colnam"`: References lookup table (treated as numeric ID)

### Categorical Aggregation Modes

When pivoting categorical traits to wide format:
- `"mode"`: Most frequent value per taxon/individual
- `"concat"`: All unique values concatenated

### Legacy Function Wrappers

Some functions have legacy wrappers for backward compatibility:
- `query_traits_measures()` → wraps `query_taxa_traits()`
- These emit warnings suggesting migration to new functions

## RMarkdown Tutorials

The repository contains several `.Rmd` tutorial files (see `tuto_database.Rmd`, `tuto_db.Rmd`) which demonstrate package usage.

## Package Metadata

- **Version**: 1
- **Authors**: Gilles Dauby, Hugo Leblanc
- **License**: GPL-2
- **Minimum R version**: 4.0
- **roxygen2 version**: 7.3.2
