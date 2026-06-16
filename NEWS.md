# CafriplotsR 1.9.8 (Development)

### New Features

* **`mod_citation_panel`** — new shared Shiny module (`R/mod_citation_panel.R`) rendering a "Data Sources" panel with acknowledgement banner, summary stats cards, and per-citation cards (blue for cited, yellow for unassigned)
  - Extracted from the duplicated inline rendering code in `mod_traits_enrichment` and `mod_taxa_traits_table`; both modules now delegate to `mod_citation_panel_server()`
  - `launch_query_plots_app()` now shows a **Data Sources** tab in the results panel when individuals are extracted with taxa-level traits (`extract_traits = TRUE`); citation data is fetched via a follow-up `query_taxa_traits(include_citation = TRUE, format = "long")` call on the unique taxa found in the extraction

### Bug Fixes

* **`output_styles_helpers.R` / `.extract_individuals_table()`** — `idtax_individual_f` is now always preserved in the individuals output table alongside `id_n`, regardless of output style. Previously this linking column was silently dropped during style processing, preventing downstream citation lookup in the Shiny app.

* **`mod_taxonomic_validator`** — clicking "Confirm Selection" no longer resets the Action column or misclassifies previously accepted rows as rejected. The fix separates the display data (`validated_data`) from the confirmed output (`final_validated_links`): confirming no longer overwrites the table's data, so row indices used by `user_decisions` remain stable.

* **`mod_link_executor` / `.add_link_specimens()`** — duplicate links (same `id_n` + `id_specimen` + `id_linktype`) are now prevented at two levels: internal duplicates within a new batch are removed before insertion, and the existing-DB check now matches on all three key columns instead of only `id_n` + `id_specimen`.

### Documentation

* **`mod_step1_choose_type_ui()`** — reworded the taxonomic-standardization requirement checkbox to explain the `launch_taxonomic_match_app()` workflow more precisely (standardize the taxa list, keep original names, obtain `idtax_n`) and to correct the column-mapping guidance, which previously referred to a non-existent `idtax` column instead of `idtax_n`. French translation synced.

* **Newsletter vignettes** (`newsletter.Rmd`, `newsletter-fr.Rmd`) — copyedited the WCVP and aggregated-traits sections: dropped marketing language tied to the Barcelona presentation, fixed example calls, added a `get_wcvp_status()` snippet, and clarified the public-access/data-sovereignty note.

### New Features

* **`connect_cafri()` — single entry point for both databases** (`R/connections_db.R`)
  - Opens connections to the main (`plots_transects`) and taxa (`rainbio`) databases from one credential prompt instead of requiring separate `call.mydb()` and `call.mydb.taxa()` calls
  - Returns a list with `$main` and `$taxa` connections; both are also stashed in `.db_env` so existing package functions calling `call.mydb()` / `call.mydb.taxa()` later in the session reuse them transparently
  - Supports `taxa = FALSE` for sessions that only need the main database (taxa is opened lazily when a query needs it, still with cached credentials)
  - `call.mydb()` and `call.mydb.taxa()` remain available unchanged for backward compatibility

* **`.Renviron` credentials are now used by default** (`R/connections_db.R`)
  - `use_env_credentials = TRUE` is now the default on `connect_cafri()`, `connect_database()`, `call.mydb()`, `call.mydb.taxa()`, `create_pool_main()`, and `create_pool_taxa()`. Users who previously ran `setup_db_credentials()` no longer need to remember the flag — `MYDB_USER` / `MYDB_PASS` are picked up automatically when neither explicit nor cached credentials are available
  - The "Using stored credentials from environment" message is now emitted only once per session (cleared by `cleanup_connections()` and `remove_db_credentials()`)
  - Resolution priority is now: explicit `user`/`pass` arguments → cached session credentials → `.Renviron` → interactive prompt
  - Set `use_env_credentials = FALSE` to opt out and always prompt
  - New internal helpers `.get_env_credentials()` and `.resolve_credentials()` share the lookup logic between the connect and pool builders

* **Standardize free-text observations into mortality and dawkins traits** (`R/observations_standardization.R`, `R/mod_feat_step_observations.R`)
  - New `standardize_observations()` parses the free-text `observations` trait (id 13) and decodes `flag1_rainfor` (id 19) into two derived traits: `mortality_risk_flag` (multi-token; one DB row per matched token) and `dawkins_index` (id 15; never overwrites existing values)
  - Editable regex ontology shipped in `inst/ontology/observations_ontology.csv` (22 mortality tokens + 5 dawkins classes); `.gitignore` updated with `!inst/ontology/*.csv` exception so the file is tracked
  - PCRE word-boundary preprocessor `.fix_word_boundaries()` converts `\b` to unicode-safe `(?<![[:alpha:]])` / `(?![[:alpha:]])` so accented French tokens (cassé, déraciné) match correctly
  - `flag1_rainfor` letter codes are decoded via `.flag1_to_mortality_map()` and merged with text-derived tokens; source provenance is preserved in the comment (`"text: ..."` vs `"flag1_rainfor: b"`)
  - `bootstrap_mortality_risk_flag_trait()` inserts the new categorical trait into `traitlist` with 22 factor levels including the distinct `uprooted` category
  - Wizard integration: new "Standardize Observations" card in `launch_feature_wizard()` step 2 under a new **Derived / Computed Traits** section alongside Compute Stem Status; step 3 module (`mod_feat_step3_standardize_obs_*`) provides compute → review derived rows → review unresolved phrases → confirm flow; step 4 (lookup) is skipped; step 6 dispatches to `.execute_standardize_observations_import()`

* **Inspectable and customisable `output_style` for `query_plots()`** — built-in styles are now transparent and users can define their own
  - `list_output_styles()` returns a tibble summarising every built-in style (description, additional tables, column/pattern counts)
  - `get_output_style(name)` returns the configuration of a built-in style as a `plot_output_style` object with a dedicated print method that groups fields by purpose (column selection, pattern filters, renames, additional tables, flags); use `unclass()` to see the raw list
  - `output_style(...)` builds a custom style; pass `based_on = "<built-in>"` to inherit and override only the fields you care about (replace, not append, semantics — pass `character()` to clear a vector field while keeping the rest)
  - `validate_output_style()` checks the shape of a custom config; called automatically by the constructor and by `query_plots()` when a raw list is passed
  - `query_plots(output_style = ...)` accepts a built-in name string, a `plot_output_style` object, or a raw list. The `"permanent_plot" → "permanent_plot_multi_census"` auto-upgrade and the `census_pairs` override only apply to character input; custom objects are respected as-is
  - Custom styles live only in the current R session (no registry, no disk cache); to reuse, assign to a variable, `saveRDS()`, or put the constructor call in `.Rprofile`
  - Tests: 42 new unit tests in `tests/testthat/test-output-styles-custom.R`

* **Offline mode for `launch_taxonomic_match_app()`** — auto-matching and manual review now work without a live database connection
  - New "Use offline (cached backbone)" button on the login screen, shown automatically when a backbone cache is present on disk (`mod_database_login.R`)
  - `match_taxonomic_names()` gains a `backbone` parameter; when supplied (or when a cache exists and no `con` is given) all matching runs in R via `stringdist`'s trigram-Jaccard, mirroring PostgreSQL's `pg_trgm` `SIMILARITY()` — no network round-trips
  - Auto-matching, fuzzy suggestions, and the Review tab's custom search are all routed through the cached backbone in offline mode (`mod_auto_matching.R`, `mod_fuzzy_suggestions.R`, `mod_name_review.R`)
  - Traits enrichment and WCVP option are hidden when offline (require live DB)
  - Performance: R-side path is typically faster than the SQL-side per-name loop on slow connections (no network round-trips). For online users with a cache, matching also uses the R-side path by default
  - R-side trigram-Jaccard agrees with PostgreSQL `pg_trgm` `SIMILARITY()` at correlation ~0.99 (mean delta ~0.03) on representative names, so existing `min_similarity` thresholds carry over without retuning
  - Tests: 37 new unit tests in `tests/testthat/test-r-side-matching.R` against a synthetic backbone (no DB required)

* **Aggregated taxa traits from individual measurements** (`R/aggregate_individual_traits.R`)
  - New `rebuild_aggregated_taxa_traits()` aggregates individual-level measurements (`data_traits_measures`) into taxa-level rows in `taxa_traits_measures`, driven by declarative rules in a new `trait_aggregation_config` table
  - Config CRUD helpers: `add_trait_aggregation()`, `remove_trait_aggregation()`, `list_trait_aggregations()`
  - Aggregation kernel `.compute_aggregate()` supports `mean`, `median`, `min`, `max`, `sum`, `sd`, `percentile` (with `method_param`), `mode`, `concat`, `count`
  - Auto-derived target traits preserve the transformation method in the trait identity (e.g. `stem_diameter_p95`); pass `target_trait_id = source_trait_id` to keep the original trait name, or an explicit integer to write into a chosen trait
  - Aggregation is restricted to taxa identified at species or lower (configurable via `allowed_tax_levels`)
  - RLS-safe insert path via parametrised `INSERT` (replaces `dbWriteTable`/`COPY`, which PostgreSQL refuses on RLS-protected tables)
  - Citation `CafriplotsR_aggregated` (auto-managed, `is_public = FALSE`) tags all aggregated rows; `RESTRICTIVE` RLS policy hides them from the public role
  - Migration / rollback helpers in `inst/scripts/migrate_add_aggregated_traits.R`


* **`mod_extraction_config` — UI redesign with CSS-only tooltips and section cards**
  - Replaced dynamically-rendered UI with a static layout featuring coloured section cards (`.cfg-card`) and collapsible advanced options via native `<details>`
  - Added CSS-only question-mark tooltips (`.tip`) requiring no JavaScript

* **`mod_taxa_add` — WCVP backbone search integrated into Step 1**
  - `mod_taxa_add_server()` gains a `pool_main` parameter and new reactive values (`wcvp_results`, `wcvp_selected_id`, `wcvp_synonymy_candidates`)
  - Step 1 now searches both Tropicos and the WCVP backbone simultaneously; results are displayed in separate panels with distinct visual styling

* **WCVP backbone integration** — all major query functions now accept `backbone = "wcvp"` to use the World Checklist of Vascular Plants as an alternative to the internal taxonomy
  - New file `R/wcvp_integration.R` with schema setup (`setup_wcvp_schema()`), data import (`import_wcvp_names()`), taxon matching (`match_taxa_to_wcvp()`, `save_wcvp_links()`), lookup (`get_wcvp_names()`), and status utilities (`get_wcvp_status()`, `check_wcvp_update()`)
  - When `backbone = "wcvp"`, standard taxonomy columns (`tax_fam`, `tax_gen`, `tax_esp`, `tax_sp_level`, `tax_infra_level`, `tax_infra_level_auth`) are replaced in-place with WCVP values; two extra columns are added: `name_source` (`"wcvp"` or `"internal"`) and `alt_taxon_name` (the internal name preserved for reference)
  - WCVP ID columns `wcvp_plant_name_id` and `wcvp_accepted_plant_name_id` are added as analogs of `idtax_n` / `idtax_good_n`; internal IDs are never replaced
  - Taxa with no WCVP match fall back silently to the internal backbone with `name_source = "internal"`
  - `backbone` parameter propagated through: `query_taxa()`, `add_taxa_table_taxa()`, `resolve_taxon_synonyms()`, `merge_individuals_taxa()`, `query_plots()` / `process_individuals()`, `query_individual_features()`, `query_taxa_traits()`
  - `launch_taxonomic_match_app()`: WCVP option now appears in the backbone-selection modal when WCVP data is present in the taxa database; fixed availability check that was always returning `FALSE` due to a missing `is_current` field in `get_wcvp_status()` return value

* **`describe_columns()` — new function for documenting query result columns**
  - Reverse-maps every output column back to its database origin, accounting for output style renames (e.g. `ddlat` → `latitude`), census column renames (e.g. `stem_diameter_census_1` → `dbh_census_1`), and pivot suffixes (`_mean`, `_sd`, `char_`, `issue_agg_`, `_census_N`, `_0`/`_1` pairs)
  - Accepts a `plot_query_list` (from `query_plots()`) or a plain `data.frame`; returns a named list of documentation tables (one per result table) or a single table
  - Each documentation table has columns: `column_name`, `original_name`, `description`, `category`, `unit`, `notes`
  - `con` defaults to `NULL` and uses the active connection via `call.mydb()`, matching `query_plots()` behaviour
  - A dedicated **Column Documentation** tab is shown alongside the results tables in the `launch_query_plots()` Shiny app (replaces per-table collapsible panel); combines all tables' column docs into one filterable table with a leading *Table* column
  - Column documentation can be included in Excel, CSV (zip), and RDS exports via the *Select tables to include* checkbox (selected by default)

* **`query_plots()` new `census_pairs` output format for individual features**
  - New `individual_features_format = "census_pairs"` option produces one row per consecutive census pair per individual
  - Columns include `dbh_0`, `dbh_1`, `date_census_0`, `date_census_1`, `time` (days between censuses), and `stem_status` at the second census
  - Available in both the R function and the Query Plots Shiny app extraction config

* **`safe_delete_specimen_links()` — new function for removing individual–specimen links**
  - Dry-run mode (default) previews what would be deleted before any change is made
  - Selection by individual IDs, specimen ID, or direct link ID
  - Wrapped in a database transaction with rollback on error
  - Replaces the old internal `.delete_link_individual_specimen()`

* **`add_traits_measures()` — redesigned API for inserting individual-level trait measurements**
  - Clearer parameter names: `plot_name_col`, `tag_col`, `id_individual_col`
    (removed ambiguous `id_plot_name`, `id_tag_plot`, `individual_plot_field`)
  - Bulk insert via temp table + COPY protocol — tested on 112 000+ rows
  - `census_col` and `id_sub_plots_col` for flexible census linking

* **Feature Wizard Shiny app** (`launch_feature_wizard()`)
  - New 6-step guided wizard for adding features and census data to existing plots
  - Step 1: Login and multi-select plot selector with summary (reuses `mod_database_login`)
  - Step 2: Choose operation mode — New Census, Add Plot Features, Add Individual Measurements, Define Multi-Stems, or Add Recruits (redirects to Import Wizard)
  - Step 3 (plot features): Form or xlsx upload with column mapping for census metadata and arbitrary subplot features
  - Step 3 (measurements): xlsx upload with trait mapping grouped by category, showing description, unit, factor levels, and column content preview; supports wide and long formats
  - Step 3 (multi-stems): Upload or interactively define stem groups; enriches data by joining with DB to resolve `id_n`, `group_id_n`, and existing `stem_grouping`; shows all plot individuals alongside grouped ones for manual editing (remove, reassign, reset)
  - Step 4: Lookup matching for people columns (skipped for measurements and multi-stems)
  - Step 5: Validation with context-aware checks — duplicate detection (numeric traits only), previous census value comparison with `issue` column, issue summary table by trait; multi-stems uses pre-resolved IDs with "tag not found" as warning
  - Step 6: Import execution with dry-run support — bulk insert via single `dbAppendTable()` in explicit transaction for measurements; `update_records()` for multi-stem `stem_grouping` updates; context-aware labels (import vs update)
  - Full EN/FR internationalization

* **Taxonomic Matching app (`launch_taxonomic_match_app()`) — auto-matching checkpoint/resume**
  - Matching progress is saved to a temp file after each name; closing the browser mid-session no longer loses work
  - On next launch with the same dataset and column, the app detects the saved checkpoint and offers to resume from where it stopped or start fresh
  - Fuzzy matching loop exits immediately when the browser is closed (previously kept running in R until completion)

* **Taxonomic Matching app (`launch_taxonomic_match_app()`) WCVP integration refactored**
  - WCVP is no longer offered as a backbone selection option during auto-matching (removed from modal)
  - Only internal backbone is used for the matching process (automated via `mod_backbone_cache_selection`)
  - New sidebar checkbox "Use WCVP names in output" allows users to enrich matched results with WCVP taxonomic names when available
  - WCVP enrichment happens immediately after matching completes, so corrected names display WCVP values in Review and Export tabs
  - Gracefully falls back to internal names when WCVP data is unavailable
  - Checkbox appears conditionally only when WCVP tables exist in the taxa database

* **Taxonomic Backbone app (`launch_taxo_backbone_app()`) enhancements**
  - Browse & Search tab now displays **morphotaxon status** (Yes/No) in the selected taxon info panel
  - WCVP link information displayed when available: **WCVP ID**, **WCVP Status** (Accepted/Synonym), and **WCVP Name** from the WCVP database
  - Update Taxa tab now allows modifying the **morphotaxon status** via a checkbox in the "Other attributes" section
  - When toggling only morphotaxon status with no other field changes, the update now correctly bypasses the `update_dico_name()` call and applies the morpho_species change via direct SQL

* **`add_subplot_features()` people resolution**
  - For features with `valuetype == "table_colnam"` (team_leader, additional_people, etc.), comma-delimited person names are now split and matched to `table_colnam` IDs before insertion

* **`safe_delete_individuals()` specimen link cascade**
  - Now counts and deletes specimen links (`data_link_specimens`) in cascade when deleting individuals

* **`safe_delete_plot()` `delete_plot` parameter**
  - New `delete_plot = TRUE` parameter; set to `FALSE` to remove only individuals and their features while preserving all plot metadata and subplot features

### Bug Fixes

* **Authentication failures no longer cache a bad password** (`R/connections_db.R`)
  - Previously, a wrong password was cached in memory and every subsequent `call.mydb()` reused it, forcing users to discover the `reset = TRUE` flag
  - `connect_database()` now distinguishes authentication errors (`password authentication failed`, `role does not exist`, etc.) from network errors via the internal `.is_auth_error()` helper; on auth failure, cached credentials are cleared and the user is re-prompted automatically in interactive sessions
  - Network/transient errors retain the existing retry-with-backoff behaviour

* **Username is now prompted before password** in `connect_database()`, matching standard login-form ordering (previously password was asked first)

* **Import Wizard Step 3 — column auto-mapping with category-aware scoring**
  - Fixed "plot" column mapping to "plot_name" (direct) instead of feature "plot"
  - Fixed "subplot" column mapping to "quadrat" (individual feature) instead of no match
  - Replaced first-match-wins sequential strategy with category-aware scoring: all possible matches across exact/synonym/fuzzy are scored; direct/required columns get multiplier bonuses (2.0x for required direct, 1.5x for other direct, 1.0x for features)
  - Added word-boundary aware pattern matching in synonym resolution to prevent "plot" from matching within "subplot"
  - New function `.score_candidates()` evaluates all alternatives for each user column; alternatives stored in result for potential future UI enhancements
  - All schema columns remain available in Step 3 dropdown for user override (no columns hidden from choices)

* **Import Wizard synonym dictionary merging for individuals**
  - Fixed synonym dictionary merging to use `modifyList()` instead of `c()` when combining base column synonyms with trait-specific and feature-specific synonyms
  - This ensures trait-specific definitions (e.g., `stem_diameter` with "dbh" synonym) properly override base definitions
  - Fixes "dbh" column now correctly mapping to "stem_diameter" via synonym match

* **`query_plots()` dead/presumed_dead individual filtering at `census_strategy = "first"/"last"`**
  - When `show_multiple_census = FALSE` and `census_strategy` is `"first"` or `"last"`,
    individuals with `stem_status` of `"dead"` or `"presumed_dead"` at the selected census
    are now automatically removed from the result
  - A warning is emitted when no `stem_status` data is found (requires the stem_status
    workflow to have been run for the plot)
  - Applies to both wide and long `individual_features_format` output paths

* **`query_plots()` `stem_diameter = NA` with `show_multiple_census = FALSE`**
  - Older measurements stored without `id_table_liste_plots` were silently
    dropped during census filtering; fixed by coalescing the plot ID from the
    subplot table

* **`query_plots()` / `query_individual_features()` — consolidated issue-handling parameter**
  - Replaced the confusing pair `remove_obs_with_issue` + `include_issue` with a
    single `issues = c("remove", "include", "ignore")` parameter throughout the
    call stack (including Shiny apps)

* **`update_records()` — setting values to `NA`/`NULL` now detected and applied**
  - Change detection previously ignored rows where the new value is `NA`; now
    both "fill" and "clear" directions are handled
  - Single and batch execution paths both emit `SET col = NULL` correctly

* **`output_styles_config` — `census_date` added to permanent plot styles**
  - `census_date` now included in `individuals_columns` and `keep_patterns`
    for `permanent_plot` and `permanent_plot_multi_census` output styles

* **Import wizard `.row_idx` column leak**
  - Internal `.row_idx` column excluded from trait validation, data preview display, and xlsx/csv exports

* **`mod_taxa_add` unused `con` parameter**
  - Removed unused `con = pool()` argument from `query_taxa()` call that could cause errors

* **`register_user()` validation simplification**
  - Fixed registry table permissions (GRANT ALL to creator); removed redundant role existence check that could fail for users without `pg_roles` access

* **`output_styles_config` additional keep patterns**
  - Added `position_`, `strate`, `transect_part` to default keep_patterns for transect output

* **Import wizard subplot feature insertion type mismatches**
  - Fixed `add_subplot_features()` using bare `NA` in `ifelse()` statements, causing logical-to-numeric/character type errors
  - Now uses `NA_real_` and `NA_character_` for proper type matching with database schema

* **Import wizard map preview crash on non-numeric coordinates**
  - Fixed `mod_step6_preview.R` calling `abs()` on potentially non-numeric coordinate columns
  - Added coercion to numeric with proper fallback handling for invalid coordinates

* **Import transaction error messages unclear**
  - Improved error handling in import transactions to capture and display actual error messages instead of empty strings

* **`.link_table()` dynamic column selection failure**
  - Fixed `pull()` and `select()` using incorrect `{{}}` embrace syntax with string variable column names
  - Changed to `rlang::sym()` for proper symbol conversion in `pull()` and `select()` calls

* **Import wizard silent failure during subplot feature insertion**
  - Fixed `add_subplot_features()` missing `id_colnam` column in `data_to_add` tibble (required by `data_liste_sub_plots` schema)
  - Fixed `.link_colnam()` call using incorrect `id_field = "subplotype"` parameter (should be `id_field = "id_colnam"`)

* **Feature Wizard compute_stem_status mode column mismatches**
  - Fixed `compute_stem_vital_status()` using incorrect column name `p.id_table_liste_plots` in LEFT JOIN (correct: `p.id_liste_plots`)
  - Fixed step 1 plot selection module passing `id_liste_plots` but step 3 expecting `id_table_liste_plots`
  - Fixed step 3 result missing `tag` column in final output; now propagated through joins for display in review table
  - Fixed step 5 validation auto-validation for `compute_stem_status` mode missing `$summary` sub-list structure, causing "argument is of length zero" error
  - Added fallback: non-lookup features now get `id_colnam = NA_integer_` for proper database constraint handling

* **Import wizard `table_colnam` feature insertion with pre-matched IDs**
  - Fixed `add_subplot_features()` silent failure when Shiny import wizard Step 4 pre-matches person names to `id_table_colnam` IDs
  - When feature values are already numeric IDs (e.g., `"123, 456"` from wizard matching), bypass `.link_colnam()` to prevent it from trying to match ID strings as names and falling into an interactive `readline()` loop that hangs in Shiny
  - Added validation of pre-matched IDs against `table_colnam` before assignment
  - Non-numeric values still flow through `.link_colnam()` for interactive name-to-ID resolution

### Documentation

* Newsletter text refined (EN/FR): concise TWDD description, clarified citation tracking panel wording, added function names for interactive apps

* **Structured citation tracking for taxa-level trait measurements**
  - New `table_citations` table in the main database (`plots_transects`) with fields for authors, year, title, journal, DOI, URL, and dataset name
  - `migrate_add_citations_table()`: migration function to create the table and add `id_citation` FK column to `taxa_traits_measures`
  - `query_citations()`, `add_citation()`, `update_citation()`: CRUD functions for managing citation records
  - `export_taxa_traits_for_citation_backfill()` / `apply_citation_backfill()`: workflow for bulk-assigning citations to existing measurements via an exported Excel file
  - `grant_lookup_table_permissions()` now includes `table_citations` by default
  - `add_sp_traits_measures()` accepts `id_citation` to tag new measurements at import
  - `fetch_taxa_trait_measurements()` and `query_taxa_traits()` gain `include_citation = FALSE` parameter to join full citation metadata

* **Citation source panel in taxa traits enrichment module** (`shiny_app_taxo_match`)
  - New "Data Sources" tab in the trait enrichment results showing per-citation measurement/taxa/trait counts
  - Acknowledgement banner emphasising the importance of citing data sources
  - Excel downloads (wide and long format) include a `citations` sheet alongside trait data

* **Citation selector in taxa traits import app** (`shiny_app_taxa_traits_import`)
  - Dropdown to pick the citation for all rows being imported, with a "New citation" modal to create one on the fly

* **Multi-row selection in taxo backbone app** (`shiny_app_taxo_backbone`)
  - Search results table now supports multi-row selection (Ctrl/Cmd click)
  - Selected taxon panel lists all selected taxa when multiple are chosen; Update/Synonymy tabs act on the first selected
  - Tree view shows an explicit message instead of silently showing the first taxon when multiple are selected
  - Trait text panel redirects to "Extract as Table" when multiple taxa are selected

* **New module: taxa trait table extraction** (`mod_taxa_traits_table`)
  - "Extract as Table" button in the taxo backbone search panel fetches wide and long trait tables for selected taxa
  - Results include Wide Format, Long Format, and Data Sources tabs with per-citation cards
  - Excel downloads include a `citations` sheet

* **New module: equivalent R code preview** (`mod_taxa_r_code`)
  - Collapsible "Show Equivalent R Code" panel in the taxo backbone search section
  - Generates `query_taxa()` call matching current search filters (including `include_children`)
  - Generates `query_taxa_traits()` calls once "Extract as Table" is triggered
  - Produces a complete workflow script; when user is connected as public, includes the public connection credentials automatically
  - Notes for Shiny-only options (`include_synonyms`, `synonymy_filter`) that require post-processing in R

* **`include_children` parameter in `query_taxa()`**
  - New `include_children = FALSE` parameter recursively fetches all descendant taxa via the `id_parent` foreign key (up to 10 iterations)
  - New internal helper `.include_children()` used by both the name-based and ID-based query paths
  - The taxo backbone Shiny app now delegates child fetching to this parameter instead of a manual loop

### Bug Fixes

* **`pivot_numeric_traits()` namespace fix**
  - `str_remove()` qualified as `stringr::str_remove()` to avoid ambiguity when package is not attached

### Code Refactoring

* **`safe_delete_plot()` batch transactions**
  - Replaced single large transaction covering all deletions with per-batch transactions (batch size 2000)
  - Pre-resolves individual and trait-measure IDs before deletion to avoid expensive nested subqueries
  - Prevents lock timeouts on plots with large numbers of individuals

* **Output styles config**
  - Added `phenology` and `succession_guild` to default individual output columns

* **Public access login option in all Shiny apps**
  - `mod_database_login` now offers a "Connect as public user" button alongside the personal credentials form
  - Public connection uses a dedicated read-only database user (`CafriP_public`) with access restricted to taxonomy and taxa-level trait tables only — no plot data exposed
  - A yellow warning notice on the login panel informs users that public access is read-only and does not allow adding or modifying data
  - `mod_database_login_server()` returns a new `is_public` reactive so parent apps can adapt their UI accordingly
  - `shiny_app_taxo_backbone`: the existing write-permission check (`has_table_privilege`) automatically detects the public user and displays the amber "Read-Only Mode" badge — no additional changes required
  - Applies to both `launch_taxonomic_match_app()` and `launch_taxo_backbone_app()`

* **Taxa traits import Shiny app** (`shiny_app_taxa_traits_import()`)
  - New interactive app for importing taxa-level trait measurements into the database
  - Modules: column mapping (`mod_trait_column_mapping`), metadata mapping (`mod_trait_metadata_mapping`), validation (`mod_trait_validation`), preview & import (`mod_trait_preview_import`)
  - Supports dry run preview before committing data
  - Duplicate detection against existing database records
  - Transactional import: trait measures and features inserted atomically (single transaction)

* **Internationalization (i18n) on login module**
  - `mod_database_login` now includes an EN/FR language toggle at the login step
  - Language choice is synced to the main app language selector across all 7 Shiny apps
  - Checkbox "Use saved credentials" now correctly hidden when no saved credentials are detected

### Bug Fixes

* **`.traits_to_genera_aggreg()` incorrect `source` assignment**
  - Fixed hardcoded `source = "species"` for all non-NA trait values; traits assigned via a genus-level `idtax` were incorrectly labelled as species-level
  - `tax_level` (already present in `individuals` via `add_taxa_table_taxa`) is now carried through `dataset_subset` and used to set `source` correctly (`"species"` for species/infraspecific, the actual level otherwise)
  - Applies to both categorical and numeric trait paths
  - `tax_level` is dropped before `pivot_wider` to avoid column duplication on join-back

* **`register_user()` NULL parameter crash**
  - Fixed "Expected string vector of length 1" error when `institution` or `notes` are NULL; `glue_sql` requires length-1 values, so NULLs are now converted to `NA_character_` before SQL construction

* **`add_sp_traits_measures()` robustness improvements**
  - Added `con` parameter to accept an existing connection/pool instead of always calling `call.mydb.taxa()`
  - Fixed `else { new_data_renamed <- new_data }` branch that silently discarded the `idtax` column rename
  - Replaced deprecated `dplyr::filter_at()` / `dplyr::any_vars()` with `dplyr::if_any()`
  - Fixed `ifelse()` type coercion bug: numeric trait values were silently converted to character
  - Transaction now wraps both trait measures and features inserts (was committing before features)
  - `dbRollback()` errors no longer mask the original insertion error
  - Fixed `apply()` converting tibble rows to character vectors; replaced with `lapply()`

* **`add_sp_traits_measures_features()` fixes**
  - Added `in_transaction` parameter to prevent nested `dbBegin()` errors when called within an outer transaction
  - Added `interactive` parameter passthrough (was defaulting to `TRUE`, showing console prompts in Shiny)
  - Fixed `valuetype` variable name collision with `dplyr::select()`
  - Added `is.numeric()` guard on zero-value check to prevent NA crash with character trait columns

* **`.link_sp_trait()` range validation fix**
  - Added `!is.na()` guards on `minallowedvalue` / `maxallowedvalue` checks to prevent NA propagation crash when optional range limits are NULL

* **Trait table references migrated to main database**
  - `table_traits` → `traitlist`, `table_traits_measures` → `taxa_traits_measures` across all query, add, update, delete, and link functions
  - `mydb_taxa` → `mydb` for trait operations (traits now in `plots_transects` database)

### Code Refactoring

* Switched all trait operations from taxa database (`rainbio`) to main database (`plots_transects`)
* Updated `R/taxa_traits_function.R`, `R/delete_functions.R`, `R/updates_tables_functions.R`, `R/link_table_functions.R`, `R/individual_features_function.R`, `R/mod_taxa_add.R`, `R/mod_growth_form_selector.R`

---

# CafriplotsR 1.9.4 (Development)

### New Features

* **User management system** (`R/user_management.R`)
  - `create_user_registry()`: Create a `user_registry` table in the main database for tracking user metadata (email, institution, etc.)
  - `register_user()`: Add or update user metadata in the registry
  - `setup_user_permissions()`: Grant/configure user permissions on main and taxa databases
  - `get_registered_users()`: List registered users with their metadata
  - `get_user_emails()`: Retrieve user email addresses for communications
  - `deactivate_user()` / `reactivate_user()`: Manage user active status

* **Feature and trait categories**
  - New `category` parameter in `add_trait()` and `add_subplottype()` for grouping features in the UI
  - Import wizard (step 3) now shows a category selector when adding new traits or subplot features

* **Census summary columns in `query_plots()` metadata output**
  - New columns `n_census`, `first_census`, `last_census` in plot-level metadata
  - Exposed in all output styles

* **Grouped schema column dropdowns in import wizard**
  - `get_schema_choices_grouped()`: Builds optgroup-organized choices for column mapping dropdowns
  - Columns grouped by category, with most-similar matches sorted to the top per group

* **Apply fuzzy matches button in specimen lookup module**
  - Users can now review and confirm fuzzy collector/specimen matches before applying them

* **Long format output for individual features in `query_plots()`**
  - New `individual_features_format = c("wide", "long")` parameter (default: `"wide"`)
  - Wide format (existing behaviour): one row per individual with trait columns pivoted wide
  - Long format: one row per individual × measurement (`trait`, `traitvalue`, `traitvalue_char`, `valuetype`, `census_date`)
  - Census filtering (`census_strategy`, `show_multiple_census`) applies to both formats
  - `concatenate_stem = TRUE` is incompatible with long format and raises an informative error
  - Option exposed in the **Census Handling** section of the interactive query-plots Shiny app

* **Consistent `plot_id` column across all output styles**
  - `id_liste_plots` is now always renamed to `plot_id` in metadata output, regardless of output style
  - Enables reliable chaining: `query_plots(id_plot = metadata$metadata$plot_id, ...)`
  - `remove_patterns` regex updated to also preserve `id_liste_plots` (like `id_n`)

### Bug Fixes

* **Shiny app query-plots — results reset when query parameters change**
  - Going back to the query builder and changing filters, plot selection, or extraction options now clears the results section
  - Prevents stale extraction results from being displayed alongside a new metadata query

* **Shiny app query-plots — generated R code now reflects actual extraction**
  - Code preview captures plot IDs at extraction time (not live selection state)
  - Individuals code always uses `metadata$metadata$plot_id` when a metadata query preceded extraction, regardless of how many plots were selected
  - Fixed metadata viewer failing to find the `plot_id` column (was only checking legacy names `id_liste_plots` / `id_plot`)

### Documentation

* Updated vignettes (`using-query-plots.Rmd`, `using-query-plots-fr.Rmd`) with `individual_features_format` parameter in the **Census Handling** section

---

# CafriplotsR 1.9.3 (Development)

### New Features

* **Improved parameter naming in `query_plots()`**
  - New `extract_coordinates` parameter replaces `show_all_coordinates` for better clarity
  - More intuitive name better describes the action: extracting coordinate data from subplots
  - When TRUE, returns `coordinates` (raw data) and `coordinates_sf` (spatial features) in output list
  - Old parameter `show_all_coordinates` still works but shows deprecation warning
  - Will be removed in a future version (2.0.0)

* **Enhanced column mapping with pattern/substring synonym matching**
  - Column mapping now recognizes synonyms embedded in larger strings (e.g., "DBH [cm]" matches "dbh" → `stem_diameter`)
  - Normalizes both user columns and synonyms by removing special characters, brackets, spaces
  - Minimum 3-character synonym length to avoid false positives
  - Composite scoring: prioritizes longest synonym match (×100), then column similarity (×10) as tiebreaker
  - Dynamic confidence scoring (0.80-0.90) based on similarity for pattern matches
  - Works across all import types: plots, individuals, and traits
  - Dramatically improves auto-mapping success rate for datasets with unit annotations

* **Smart deduplication for duplicate column mappings**
  - Automatically detects when multiple user columns map to the same database column
  - Keeps only the best mapping based on priority: exact match > exact synonym > pattern synonym > fuzzy
  - Unmaps lower-quality duplicates (sets to skip) to prevent data conflicts
  - Console/log output shows which columns were kept and which were unmarked
  - Example: "original_tax_name", "Espece", "Espece N" all mapping to `original_tax_name` → keeps exact match, skips others
  - Prevents import errors from ambiguous column data sources

* **Similarity-based dropdown sorting in column mapping UI**
  - Import wizard dropdowns now show database columns sorted by similarity to user column name
  - Each user column gets its own relevance-ranked dropdown (not global alphabetical)
  - Most similar options appear first, making manual mapping intuitive
  - Uses same string similarity algorithm as fuzzy matching
  - Significantly improves UX for columns that weren't auto-mapped

* **Auto-fill missing taxonomy with Magnoliopsida**
  - Missing `idtax_n` values are automatically filled with 351190 (Magnoliopsida class)
  - Converts taxonomy validation errors to warnings for missing `idtax_n` and `original_tax_name`
  - Clear messaging: "Missing idtax_n are considered to be unidentified stems"
  - Allows import to proceed for unidentified individuals while providing placeholder taxonomy
  - Users can update taxonomy later when identification becomes available

* **Enhanced validation error messages with expected units**
  - Min/max range validation errors now include expected unit information
  - Example: "Trait 'height_of_stem_diameter' has 5930 value(s) above maximum allowed (30) (expected unit: m)"
  - Helps users quickly identify unit mismatches (cm vs m, mm vs cm, etc.)
  - Only appends unit info when trait has `expectedunit` defined in database
  - Reduces debugging time and prevents data import errors

* **Database backup and restore functions**
  - New `backup_database()` function creates timestamped PostgreSQL backups using pg_dump
  - Supports both main (`plots_transects`) and taxa (`rainbio`) databases
  - Backup files use format: `database_backup_YYYY-MM-DD_HH-MM-SS.dump`
  - Optional compression (enabled by default) for smaller file sizes
  - New `list_backups()` function shows all available backups with timestamps and sizes
  - New `restore_database()` function restores from backup with safety confirmations
  - New `cleanup_old_backups()` function removes backups older than specified days (with dry-run mode)
  - Proper Windows path handling using short path names (8.3 format) to avoid space issues
  - Secure password handling via PGPASSWORD environment variable
  - Requires PostgreSQL client tools (pg_dump/pg_restore) installed and in PATH

### Bug Fixes

* **Fixed missing `stringr::` namespace prefix in coordinate extraction**
  - Added `stringr::` prefix to `str_split()` calls in coordinate extraction code
  - Fixes "impossible de trouver la fonction 'str_split'" error in `query_plots()` with `show_all_coordinates = TRUE`
  - Affects `functions_manip_db.R` lines 351-354 in coordinate processing
  - `stringr` was already in package dependencies, just needed proper namespace usage

* **Fixed missing `purrr` dependency for coordinate extraction**
  - Added `purrr` to package Imports (required by `query_plots()` with `show_all_coordinates = TRUE`)
  - Fixed unnamespaced `map_chr()` calls to use `purrr::map_chr()` in coordinate processing
  - Resolves "dépendance 'tidytable' pas chargée (necessaire pour coordinates)" error message
  - Affects `functions_manip_db.R` coordinate extraction when querying subplot coordinates

### Code Refactoring

* **Refactored `generate_rmd_export_plot.R` script**
  - Better structure with clear configuration, validation, and processing sections
  - Comprehensive error handling for each quadrat and plot
  - Improved user feedback with `cli` package progress messages
  - Validates output directory and template existence before processing
  - Tracks results and errors for each operation
  - Final summary with counts of generated files and any errors
  - Optional cleanup of individual PDFs after merging
  - Remains as internal/non-exported script for user convenience

### Documentation

* **Added FUTURE_IMPROVEMENTS.md**
  - Documents enhancement idea for database-backed synonym system for traits
  - Currently trait synonyms are hardcoded in R; proposal to store in `table_traits` table
  - Includes implementation roadmap and related files for future development

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

