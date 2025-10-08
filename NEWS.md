# plotsdatabase NEWS

## plotsdatabase 1.0 (Development)

### Breaking Changes
* **Database schema change**: Renamed column `ind_num_sous_plot` to `tag` in `data_individuals` and `followup_updates_individuals` tables
  - All R package functions updated to use new column name
  - **Action required**: External scripts accessing `ind_num_sous_plot` must be updated to use `tag`
  - Updated files: `R/functions_manip_db.R`, `R/individual_features_function.R`, `R/functions_divid_plot.R`, `R/generate_plot_summary.Rmd`, `structure.yml`
  - Default parameter in `approximate_isolated_xy()` changed from `tag = "ind_num_sous_plot"` to `tag = "tag"`

### New Features
* Initial release of package structure with comprehensive database query functions

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

