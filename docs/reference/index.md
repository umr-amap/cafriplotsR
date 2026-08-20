# Package index

## Database Connections

Connect to and manage PostgreSQL database connections

- [`call.mydb()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md)
  : Get primary database connection (wrapper)
- [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md)
  : Get taxa database connection (wrapper)
- [`connect_cafri()`](https://umr-amap.github.io/cafriplotsR/reference/connect_cafri.md)
  : Connect to both CafriplotsR databases in one step
- [`connect_database()`](https://umr-amap.github.io/cafriplotsR/reference/connect_database.md)
  : Connect to database
- [`create_pool_main()`](https://umr-amap.github.io/cafriplotsR/reference/create_pool_main.md)
  : Create a connection pool for Shiny apps (main database)
- [`create_pool_taxa()`](https://umr-amap.github.io/cafriplotsR/reference/create_pool_taxa.md)
  : Create a connection pool for Shiny apps (taxa database)
- [`cleanup_connections()`](https://umr-amap.github.io/cafriplotsR/reference/cleanup_connections.md)
  : Cleanup all database connections
- [`db_diagnostic()`](https://umr-amap.github.io/cafriplotsR/reference/db_diagnostic.md)
  : Complete database diagnostic
- [`print_connection_status()`](https://umr-amap.github.io/cafriplotsR/reference/print_connection_status.md)
  : Print connection status
- [`get_connection_info()`](https://umr-amap.github.io/cafriplotsR/reference/get_connection_info.md)
  : Get connection information
- [`test_connection()`](https://umr-amap.github.io/cafriplotsR/reference/test_connection.md)
  : Test database connection
- [`setup_db_credentials()`](https://umr-amap.github.io/cafriplotsR/reference/setup_db_credentials.md)
  : Setup credentials storage in environment variables
- [`remove_db_credentials()`](https://umr-amap.github.io/cafriplotsR/reference/remove_db_credentials.md)
  : Remove stored credentials
- [`create_db_config()`](https://umr-amap.github.io/cafriplotsR/reference/create_db_config.md)
  : Create local DB config file
- [`func_try_fetch()`](https://umr-amap.github.io/cafriplotsR/reference/func_try_fetch.md)
  : Safely execute a SQL query with automatic retry
- [`try_open_postgres_table()`](https://umr-amap.github.io/cafriplotsR/reference/try_open_postgres_table.md)
  : Try to open PostgreSQL table
- [`get_database_fk()`](https://umr-amap.github.io/cafriplotsR/reference/get_database_fk.md)
  : Get database foreign keys
- [`check_db_network()`](https://umr-amap.github.io/cafriplotsR/reference/check_db_network.md)
  : Diagnose why the database cannot be reached

## Data Queries

Query plots, individuals, taxa, traits, and specimens from the database

- [`query_plots()`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md)
  : Query plots from database
- [`query_subplots()`](https://umr-amap.github.io/cafriplotsR/reference/query_subplots.md)
  : Legacy function - wrapper for backward compatibility
- [`query_plot_features()`](https://umr-amap.github.io/cafriplotsR/reference/query_plot_features.md)
  : Query subplot features with improved architecture
- [`query_individual_features()`](https://umr-amap.github.io/cafriplotsR/reference/query_individual_features.md)
  : Query individual features with improved architecture
- [`get_individual_aggregated_features()`](https://umr-amap.github.io/cafriplotsR/reference/get_individual_aggregated_features.md)
  : Aggregate individual features to individual level
- [`query_taxa()`](https://umr-amap.github.io/cafriplotsR/reference/query_taxa.md)
  : List, extract taxa
- [`query_taxa_traits()`](https://umr-amap.github.io/cafriplotsR/reference/query_taxa_traits.md)
  : Query traits at the taxonomic level
- [`query_trait()`](https://umr-amap.github.io/cafriplotsR/reference/query_trait.md)
  : Query in taxa trait table
- [`query_traits_measures()`](https://umr-amap.github.io/cafriplotsR/reference/query_traits_measures.md)
  : Legacy function - wrapper for backward compatibility
- [`query_traits_measures_features()`](https://umr-amap.github.io/cafriplotsR/reference/query_traits_measures_features.md)
  : Query features associated with trait measurements
- [`query_colnam()`](https://umr-amap.github.io/cafriplotsR/reference/query_colnam.md)
  : Query in colnam table
- [`query_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/query_specimens.md)
  : Query Specimens
- [`query_all_specimen_links()`](https://umr-amap.github.io/cafriplotsR/reference/query_all_specimen_links.md)
  : Query All Specimen Links for Individuals
- [`query_citations()`](https://umr-amap.github.io/cafriplotsR/reference/query_citations.md)
  : Query citations from table_citations
- [`species_plot_matrix()`](https://umr-amap.github.io/cafriplotsR/reference/species_plot_matrix.md)
  : Get species-plot data frame
- [`explore_allometric_taxa()`](https://umr-amap.github.io/cafriplotsR/reference/explore_allometric_taxa.md)
  : Explore allometric relation
- [`list_individual_features()`](https://umr-amap.github.io/cafriplotsR/reference/list_individual_features.md)
  : List all available individual features
- [`get_linktypes()`](https://umr-amap.github.io/cafriplotsR/reference/get_linktypes.md)
  : Get Link Types from Lookup Table
- [`method_list()`](https://umr-amap.github.io/cafriplotsR/reference/method_list.md)
  : List of method
- [`country_list()`](https://umr-amap.github.io/cafriplotsR/reference/country_list.md)
  : List of countries
- [`subplot_list()`](https://umr-amap.github.io/cafriplotsR/reference/subplot_list.md)
  : List all available subplot types
- [`traits_list()`](https://umr-amap.github.io/cafriplotsR/reference/traits_list.md)
  : List of trait and features potentially liked to individual
- [`traits_taxa_list()`](https://umr-amap.github.io/cafriplotsR/reference/traits_taxa_list.md)
  : List of trait
- [`get_traitlist()`](https://umr-amap.github.io/cafriplotsR/reference/get_traitlist.md)
  : Return the \`traitlist\` lookup table, cached per session
- [`PlotFetcher`](https://umr-amap.github.io/cafriplotsR/reference/PlotFetcher.md)
  : Fetch plot data
- [`PlotFilterBuilder`](https://umr-amap.github.io/cafriplotsR/reference/PlotFilterBuilder.md)
  : Query builder for plot
- [`SpecimenFetcher`](https://umr-amap.github.io/cafriplotsR/reference/SpecimenFetcher.md)
  : Specimen Fetcher
- [`SpecimenFilterBuilder`](https://umr-amap.github.io/cafriplotsR/reference/SpecimenFilterBuilder.md)
  : Specimen Filter Builder
- [`get_user_accessible_plots()`](https://umr-amap.github.io/cafriplotsR/reference/get_user_accessible_plots.md)
  : Get plot IDs accessible to a user
- [`match_tax()`](https://umr-amap.github.io/cafriplotsR/reference/match_tax.md)
  : Query and standardize taxonomy
- [`match_taxonomic_names()`](https://umr-amap.github.io/cafriplotsR/reference/match_taxonomic_names.md)
  : Match taxonomic names to backbone with intelligent SQL-side strategy
- [`resolve_taxon_synonyms()`](https://umr-amap.github.io/cafriplotsR/reference/resolve_taxon_synonyms.md)
  : Resolve taxonomic synonyms
- [`get_taxon_hierarchy()`](https://umr-amap.github.io/cafriplotsR/reference/get_taxon_hierarchy.md)
  : Get Full Taxonomy Hierarchy for a Taxon
- [`get_taxon_ancestors()`](https://umr-amap.github.io/cafriplotsR/reference/get_taxon_ancestors.md)
  : Get All Ancestors of a Taxon
- [`get_taxon_children()`](https://umr-amap.github.io/cafriplotsR/reference/get_taxon_children.md)
  : Get All Children of a Taxon
- [`count_taxon_children()`](https://umr-amap.github.io/cafriplotsR/reference/count_taxon_children.md)
  : Count Children at Each Level
- [`get_updates_diconame()`](https://umr-amap.github.io/cafriplotsR/reference/get_updates_diconame.md)
  : Get backups of modified taxonomic data
- [`get_primary_specimen_link()`](https://umr-amap.github.io/cafriplotsR/reference/get_primary_specimen_link.md)
  : Get Primary Specimen for Individuals
- [`get_ref_specimen_ind()`](https://umr-amap.github.io/cafriplotsR/reference/get_ref_specimen_ind.md)
  : Find Unlinked Individuals with Herbarium Information

## Output Styling

Configure custom output styles for query results

- [`output_style()`](https://umr-amap.github.io/cafriplotsR/reference/output_style.md)
  : Build a custom output style
- [`get_output_style()`](https://umr-amap.github.io/cafriplotsR/reference/get_output_style.md)
  : Retrieve the configuration of an output style
- [`list_output_styles()`](https://umr-amap.github.io/cafriplotsR/reference/list_output_styles.md)
  : List available output styles
- [`print(`*`<plot_output_style>`*`)`](https://umr-amap.github.io/cafriplotsR/reference/print.plot_output_style.md)
  : Print an output style configuration

## WCVP Integration

Functions for integrating with the World Checklist of Vascular Plants
(WCVP)

- [`check_wcvp_update()`](https://umr-amap.github.io/cafriplotsR/reference/check_wcvp_update.md)
  : Check if WCVP Update is Available
- [`get_wcvp_names()`](https://umr-amap.github.io/cafriplotsR/reference/get_wcvp_names.md)
  : Get WCVP Names for Internal Taxa
- [`get_wcvp_status()`](https://umr-amap.github.io/cafriplotsR/reference/get_wcvp_status.md)
  : Get WCVP Import Status
- [`import_wcvp_names()`](https://umr-amap.github.io/cafriplotsR/reference/import_wcvp_names.md)
  : Import WCVP Names into Database
- [`match_taxa_to_wcvp()`](https://umr-amap.github.io/cafriplotsR/reference/match_taxa_to_wcvp.md)
  : Match Internal Taxa to WCVP Names
- [`save_wcvp_links()`](https://umr-amap.github.io/cafriplotsR/reference/save_wcvp_links.md)
  : Save WCVP Links to Database
- [`setup_wcvp_schema()`](https://umr-amap.github.io/cafriplotsR/reference/setup_wcvp_schema.md)
  : Setup WCVP Database Schema

## Data Adding

Add new plots, individuals, traits, specimens, and taxa to the database

- [`add_plots()`](https://umr-amap.github.io/cafriplotsR/reference/add_plots.md)
  : Add new plot metadata
- [`add_individuals()`](https://umr-amap.github.io/cafriplotsR/reference/add_individuals.md)
  : Add new individuals data
- [`add_plot_features()`](https://umr-amap.github.io/cafriplotsR/reference/add_plot_features.md)
  : Add Plot Features to Existing Plots
- [`add_subplot_features()`](https://umr-amap.github.io/cafriplotsR/reference/add_subplot_features.md)
  : Add an observation in subplot_features table
- [`add_subplot_observations_feat()`](https://umr-amap.github.io/cafriplotsR/reference/add_subplot_observations_feat.md)
  : Add subplot observations features
- [`add_plot_coordinates()`](https://umr-amap.github.io/cafriplotsR/reference/add_plot_coordinates.md)
  : Add 1ha IRd plot coordinates
- [`add_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/add_specimens.md)
  : Add new specimens data
- [`add_citation()`](https://umr-amap.github.io/cafriplotsR/reference/add_citation.md)
  : Add one or more citations to table_citations
- [`add_entry_taxa()`](https://umr-amap.github.io/cafriplotsR/reference/add_entry_taxa.md)
  : Add new entry to taxonomic table
- [`add_growth_form_taxa()`](https://umr-amap.github.io/cafriplotsR/reference/add_growth_form_taxa.md)
  : Add growth forms to a single taxa
- [`add_person_to_db()`](https://umr-amap.github.io/cafriplotsR/reference/add_person_to_db.md)
  : Add a person to table_colnam using secure function
- [`add_method()`](https://umr-amap.github.io/cafriplotsR/reference/add_method.md)
  : Add a method in method list
- [`add_sp_traits_measures()`](https://umr-amap.github.io/cafriplotsR/reference/add_sp_traits_measures.md)
  : Add an observation in trait measurement table at species level
- [`add_trait()`](https://umr-amap.github.io/cafriplotsR/reference/add_trait.md)
  : Add trait
- [`add_trait_taxa()`](https://umr-amap.github.io/cafriplotsR/reference/add_trait_taxa.md)
  : Add a trait in species trait list
- [`add_traits_measures()`](https://umr-amap.github.io/cafriplotsR/reference/add_traits_measures.md)
  : Add an observation in trait measurement table
- [`add_taxa_table_taxa()`](https://umr-amap.github.io/cafriplotsR/reference/add_taxa_table_taxa.md)
  : Add formatted taxa information
- [`add_subplottype()`](https://umr-amap.github.io/cafriplotsR/reference/add_subplottype.md)
  : Add a type in subplot table
- [`import_plot_metadata()`](https://umr-amap.github.io/cafriplotsR/reference/import_plot_metadata.md)
  : Import Plot Metadata with Transaction Support
- [`import_individual_data()`](https://umr-amap.github.io/cafriplotsR/reference/import_individual_data.md)
  : Import Individual Data with Transaction Support
- [`build_specimens_from_tropicos()`](https://umr-amap.github.io/cafriplotsR/reference/build_specimens_from_tropicos.md)
  : Build a specimens-shaped table from a Tropicos specimen export
- [`build_tropicos_upload_table()`](https://umr-amap.github.io/cafriplotsR/reference/build_tropicos_upload_table.md)
  : Build a Tropicos bulk-upload table from query_specimens() output

## Update and Delete

Update existing records and safely delete data with cascade handling

- [`update_records()`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md)
  : Update records with optional single-record comparison display
- [`update_dico_name()`](https://umr-amap.github.io/cafriplotsR/reference/update_dico_name.md)
  : Update taxonomic data
- [`update_dico_name_batch()`](https://umr-amap.github.io/cafriplotsR/reference/update_dico_name_batch.md)
  : Update diconame data based on id of taxa
- [`update_ident_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/update_ident_specimens.md)
  : Update specimens table
- [`update_link_specimens_batch()`](https://umr-amap.github.io/cafriplotsR/reference/update_link_specimens_batch.md)
  : Update plot data data
- [`update_specimens_batch()`](https://umr-amap.github.io/cafriplotsR/reference/update_specimens_batch.md)
  : Update specimens data data
- [`update_taxa_link_table()`](https://umr-amap.github.io/cafriplotsR/reference/update_taxa_link_table.md)
  : Update table_idtax (Materialized View Version)
- [`update_taxon_parent()`](https://umr-amap.github.io/cafriplotsR/reference/update_taxon_parent.md)
  : Update Taxon Parent (with consistency check)
- [`update_citation()`](https://umr-amap.github.io/cafriplotsR/reference/update_citation.md)
  : Update fields of an existing citation
- [`apply_citation_backfill()`](https://umr-amap.github.io/cafriplotsR/reference/apply_citation_backfill.md)
  : Apply citation backfill from a manually filled data frame
- [`update_specimen_fields()`](https://umr-amap.github.io/cafriplotsR/reference/update_specimen_fields.md)
  : Update non-identification fields of a single specimen
- [`safe_delete_individuals()`](https://umr-amap.github.io/cafriplotsR/reference/safe_delete_individuals.md)
  : Safely delete individual(s) with all related data
- [`safe_delete_plot()`](https://umr-amap.github.io/cafriplotsR/reference/safe_delete_plot.md)
  : Safely delete plot(s) with all related data
- [`safe_delete_taxa_traits()`](https://umr-amap.github.io/cafriplotsR/reference/safe_delete_taxa_traits.md)
  : Safely delete taxa trait measurements with all related data
- [`safe_delete_individual_features()`](https://umr-amap.github.io/cafriplotsR/reference/safe_delete_individual_features.md)
  : Safely delete individual feature measurements with all related data
- [`safe_delete_specimen_links()`](https://umr-amap.github.io/cafriplotsR/reference/safe_delete_specimen_links.md)
  : Safely delete individual-specimen links
- [`.delete_country()`](https://umr-amap.github.io/cafriplotsR/reference/dot-delete_country.md)
  : Delete an entry in country table
- [`.delete_individual_feature_type()`](https://umr-amap.github.io/cafriplotsR/reference/dot-delete_individual_feature_type.md)
  : Delete an entry in individual feature table
- [`.delete_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/dot-delete_specimens.md)
  : Delete an entry in specimen table
- [`detect_direct_changes()`](https://umr-amap.github.io/cafriplotsR/reference/detect_direct_changes.md)
  : Detect changes in direct columns with visual display

## Internal Linking & Matching Utilities

Advanced internal functions for data matching and linking

- [`.add_link_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/dot-add_link_specimens.md)
  : Add Link Between Specimen and Individual
- [`.add_modif_field()`](https://umr-amap.github.io/cafriplotsR/reference/dot-add_modif_field.md)
  : Query fuzzy match
- [`.add_taxa_noninteractive()`](https://umr-amap.github.io/cafriplotsR/reference/dot-add_taxa_noninteractive.md)
  : Add taxonomic entry non-interactively (for Shiny apps)
- [`.comp_print_vec()`](https://umr-amap.github.io/cafriplotsR/reference/dot-comp_print_vec.md)
  : Compare two row-tibbles and generate HTML with differences
- [`.delete_colnam()`](https://umr-amap.github.io/cafriplotsR/reference/dot-delete_colnam.md)
  : Delete an entry in colnam table
- [`.delete_trait_list()`](https://umr-amap.github.io/cafriplotsR/reference/dot-delete_trait_list.md)
  : Delete an entry in trait list
- [`.find_cat()`](https://umr-amap.github.io/cafriplotsR/reference/dot-find_cat.md)
  : Internal function
- [`.find_ids()`](https://umr-amap.github.io/cafriplotsR/reference/dot-find_ids.md)
  : Internal function
- [`.find_similar_string()`](https://umr-amap.github.io/cafriplotsR/reference/dot-find_similar_string.md)
  : Internal function
- [`.get_trait_individuals_values()`](https://umr-amap.github.io/cafriplotsR/reference/dot-get_trait_individuals_values.md)
  : Legacy function - wrapper for backward compatibility
- [`.link_sp_trait()`](https://umr-amap.github.io/cafriplotsR/reference/dot-link_sp_trait.md)
  : Internal function
- [`.link_subplotype()`](https://umr-amap.github.io/cafriplotsR/reference/dot-link_subplotype.md)
  : Internal function
- [`.link_table()`](https://umr-amap.github.io/cafriplotsR/reference/dot-link_table.md)
  : Internal function
- [`.link_trait()`](https://umr-amap.github.io/cafriplotsR/reference/dot-link_trait.md)
  : Internal function
- [`.query_unmatched_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/dot-query_unmatched_specimens.md)
  : Query Unmatched Specimens (Internal)
- [`.rename_data()`](https://umr-amap.github.io/cafriplotsR/reference/dot-rename_data.md)
  : Internal function

## Data Processing

Process, validate, transform, and aggregate inventory and trait data

- [`compute_growth()`](https://umr-amap.github.io/cafriplotsR/reference/compute_growth.md)
  : Compute growth rates for permanent plots
- [`compute_mortality()`](https://umr-amap.github.io/cafriplotsR/reference/compute_mortality.md)
  : Compute mortality and recruitment rates
- [`compute_stem_vital_status()`](https://umr-amap.github.io/cafriplotsR/reference/compute_stem_vital_status.md)
  : Compute stem vital status for specified individuals
- [`standardize_observations()`](https://umr-amap.github.io/cafriplotsR/reference/standardize_observations.md)
  : Standardize free-text observations into mortality and dawkins flags
- [`process_individuals()`](https://umr-amap.github.io/cafriplotsR/reference/process_individuals.md)
  : Process individuals for query_plots
- [`process_stems()`](https://umr-amap.github.io/cafriplotsR/reference/process_stems.md)
  : Process multiple stems
- [`divid_plot()`](https://umr-amap.github.io/cafriplotsR/reference/divid_plot.md)
  : Divid into quadrats a 1ha plot
- [`extract_corners()`](https://umr-amap.github.io/cafriplotsR/reference/extract_corners.md)
  : Extract all corners of 1ha plot
- [`approximate_isolated_xy()`](https://umr-amap.github.io/cafriplotsR/reference/approximate_isolated_xy.md)
  : Interpolate x y position based on neighnour
- [`proj_rel_xy()`](https://umr-amap.github.io/cafriplotsR/reference/proj_rel_xy.md)
  : Project stems in geographical space
- [`latlong2UTM()`](https://umr-amap.github.io/cafriplotsR/reference/latlong2UTM.md)
  : Get UTM from geographical coordinates
- [`get_plot_rel_xy()`](https://umr-amap.github.io/cafriplotsR/reference/get_plot_rel_xy.md)
  : Project stems in geographical space
- [`pivot_categorical_traits_generic()`](https://umr-amap.github.io/cafriplotsR/reference/pivot_categorical_traits_generic.md)
  : Pivot categorical trait data to wide format
- [`pivot_numeric_traits_generic()`](https://umr-amap.github.io/cafriplotsR/reference/pivot_numeric_traits_generic.md)
  : Pivot numeric trait data to wide format with statistics
- [`summarize_feature()`](https://umr-amap.github.io/cafriplotsR/reference/summarize_feature.md)
  : Get summary statistics for a specific feature
- [`merge_individuals_taxa()`](https://umr-amap.github.io/cafriplotsR/reference/merge_individuals_taxa.md)
  : Merge individual records with taxonomic information
- [`replace_NA()`](https://umr-amap.github.io/cafriplotsR/reference/replace_NA.md)
  : Replace or restore missing values in a data frame
- [`clean_taxonomic_name()`](https://umr-amap.github.io/cafriplotsR/reference/clean_taxonomic_name.md)
  : Clean and normalize taxonomic name
- [`parse_taxonomic_name()`](https://umr-amap.github.io/cafriplotsR/reference/parse_taxonomic_name.md)
  : Parse taxonomic name into components
- [`standardize_taxonomic_batch()`](https://umr-amap.github.io/cafriplotsR/reference/standardize_taxonomic_batch.md)
  : Standardize taxonomic names in a data frame
- [`validate_plot_metadata()`](https://umr-amap.github.io/cafriplotsR/reference/validate_plot_metadata.md)
  : Validate Plot Metadata Before Import
- [`validate_individual_data()`](https://umr-amap.github.io/cafriplotsR/reference/validate_individual_data.md)
  : Validate Individual Data Before Import
- [`map_individual_columns()`](https://umr-amap.github.io/cafriplotsR/reference/map_individual_columns.md)
  : Map Individual Data Columns
- [`map_user_columns()`](https://umr-amap.github.io/cafriplotsR/reference/map_user_columns.md)
  : Map User Columns to Database Schema
- [`get_import_column_routing()`](https://umr-amap.github.io/cafriplotsR/reference/get_import_column_routing.md)
  : Get Import Column Routing Configuration
- [`enrich_traits_with_measurement_features()`](https://umr-amap.github.io/cafriplotsR/reference/enrich_traits_with_measurement_features.md)
  : Enrich trait data with measurement features (generic)
- [`enrich_with_traits()`](https://umr-amap.github.io/cafriplotsR/reference/enrich_with_traits.md)
  : Enrich individuals with all traits
- [`build_data_sources_table()`](https://umr-amap.github.io/cafriplotsR/reference/build_data_sources_table.md)
  : Build a data sources summary table (citations × traits pivot)
- [`export_census_split()`](https://umr-amap.github.io/cafriplotsR/reference/export_census_split.md)
  : Write a census split out for the import wizards
- [`split_census_table()`](https://umr-amap.github.io/cafriplotsR/reference/split_census_table.md)
  [`print(`*`<census_split>`*`)`](https://umr-amap.github.io/cafriplotsR/reference/split_census_table.md)
  : Split a flat census table into remeasures and recruits
- [`collect_taxon_revisions()`](https://umr-amap.github.io/cafriplotsR/reference/collect_taxon_revisions.md)
  : Collect the identification revisions a census import should ask
  about
- [`print_validation_results()`](https://umr-amap.github.io/cafriplotsR/reference/print_validation_results.md)
  : Print Validation Results
- [`print_individual_validation_results()`](https://umr-amap.github.io/cafriplotsR/reference/print_individual_validation_results.md)
  : Print Individual Validation Results
- [`print_import_result()`](https://umr-amap.github.io/cafriplotsR/reference/print_import_result.md)
  : Print Import Result
- [`print_template_info()`](https://umr-amap.github.io/cafriplotsR/reference/print_template_info.md)
  : Print Template Column Information
- [`print_individual_template_info()`](https://umr-amap.github.io/cafriplotsR/reference/print_individual_template_info.md)
  : Export Individual Template Info
- [`print_mapping_summary()`](https://umr-amap.github.io/cafriplotsR/reference/print_mapping_summary.md)
  : Print Mapping Summary
- [`get_individual_template()`](https://umr-amap.github.io/cafriplotsR/reference/get_individual_template.md)
  : Generate Individual Data Import Template
- [`get_plot_metadata_template()`](https://umr-amap.github.io/cafriplotsR/reference/get_plot_metadata_template.md)
  : Get Plot Metadata Template
- [`export_plot_template()`](https://umr-amap.github.io/cafriplotsR/reference/export_plot_template.md)
  : Export Plot Metadata Template to Excel
- [`describe_columns()`](https://umr-amap.github.io/cafriplotsR/reference/describe_columns.md)
  : Describe columns in query results
- [`choose_growth_form()`](https://umr-amap.github.io/cafriplotsR/reference/choose_growth_form.md)
  : Choose growth forms
- [`choose_prompt()`](https://umr-amap.github.io/cafriplotsR/reference/choose_prompt.md)
  : Choose from prompt
- [`test.order.subplot()`](https://umr-amap.github.io/cafriplotsR/reference/test.order.subplot.md)
  : Check the order of subplots in a given data frame
- [`export_taxa_traits_for_citation_backfill()`](https://umr-amap.github.io/cafriplotsR/reference/export_taxa_traits_for_citation_backfill.md)
  : Export taxa trait measurements for citation backfill
- [`print(`*`<column_documentation>`*`)`](https://umr-amap.github.io/cafriplotsR/reference/print.column_documentation.md)
  : Print method for column_documentation objects
- [`print(`*`<plot_features_result>`*`)`](https://umr-amap.github.io/cafriplotsR/reference/print.plot_features_result.md)
  : Print method for add_plot_features result
- [`print(`*`<plot_query_list>`*`)`](https://umr-amap.github.io/cafriplotsR/reference/print.plot_query_list.md)
  : Print method for plot_query_list
- [`print(`*`<plot_validation_result>`*`)`](https://umr-amap.github.io/cafriplotsR/reference/print.plot_validation_result.md)
  : Print method for validation results
- [`print_table()`](https://umr-amap.github.io/cafriplotsR/reference/print_table.md)
  : print table as html in viewer

## Shiny Apps

Launch interactive Shiny applications for data management and
exploration

- [`launch_import_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/launch_import_wizard.md)
  : Launch Import Wizard Shiny App
- [`launch_feature_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/launch_feature_wizard.md)
  : Launch Feature Wizard Shiny App
- [`launch_query_plots_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_query_plots_app.md)
  : Launch Query Plots Interactive App
- [`launch_individual_specimen_linking_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_individual_specimen_linking_app.md)
  : Launch Individual-Specimen Linking App
- [`launch_specimen_import_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/launch_specimen_import_wizard.md)
  : Launch Specimen Import Wizard
- [`launch_taxa_traits_import()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxa_traits_import.md)
  : Launch Taxa Traits Import App
- [`launch_taxo_backbone_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxo_backbone_app.md)
  : Launch Taxonomic Backbone Management App
- [`launch_taxonomic_match_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxonomic_match_app.md)
  : Launch Taxonomic Name Standardization App
- [`launch_specimen_identification_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_specimen_identification_app.md)
  : Launch Specimen Identification Update App
- [`launch_data_update_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_data_update_app.md)
  : Launch the Data Update App
- [`shiny_app_query_plots()`](https://umr-amap.github.io/cafriplotsR/reference/shiny_app_query_plots.md)
  : Query Plots Shiny App
- [`shiny_app_taxo_backbone()`](https://umr-amap.github.io/cafriplotsR/reference/shiny_app_taxo_backbone.md)
  : Taxonomic Backbone Management Shiny App
- [`mod_database_login_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_database_login_ui.md)
  : Database Login Module - UI
- [`mod_database_login_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_database_login_server.md)
  : Database Login Module - Server

## Shiny Module Components

Internal Shiny UI and server module components

- [`mod_census_information_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_census_information_server.md)
  : Census Information Module - Server
- [`mod_census_information_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_census_information_ui.md)
  : Census Information Module - UI
- [`mod_code_preview_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_code_preview_server.md)
  : Code Preview Module - Server
- [`mod_code_preview_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_code_preview_ui.md)
  : Code Preview Module - UI
- [`mod_extraction_config_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_extraction_config_server.md)
  : Extraction Configuration Module - Server
- [`mod_extraction_config_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_extraction_config_ui.md)
  : Extraction Configuration Module - UI
- [`mod_feat_step1_select_plots_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step1_select_plots_server.md)
  : Feature Wizard Step 1: Select Plots - Server
- [`mod_feat_step1_select_plots_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step1_select_plots_ui.md)
  : Feature Wizard Step 1: Select Plots - UI
- [`mod_feat_step2_choose_mode_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step2_choose_mode_server.md)
  : Feature Wizard Step 2: Choose Mode - Server
- [`mod_feat_step2_choose_mode_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step2_choose_mode_ui.md)
  : Feature Wizard Step 2: Choose Mode - UI
- [`mod_feat_step3_measurements_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step3_measurements_server.md)
  : Feature Wizard Step 3: Individual Measurements - Server
- [`mod_feat_step3_measurements_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step3_measurements_ui.md)
  : Feature Wizard Step 3: Individual Measurements - UI
- [`mod_feat_step3_multi_stems_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step3_multi_stems_server.md)
  : Feature Wizard Step 3: Multi-Stems - Server
- [`mod_feat_step3_multi_stems_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step3_multi_stems_ui.md)
  : Feature Wizard Step 3: Multi-Stems - UI
- [`mod_feat_step3_plot_features_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step3_plot_features_server.md)
  : Feature Wizard Step 3: Plot Features - Server
- [`mod_feat_step3_plot_features_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step3_plot_features_ui.md)
  : Feature Wizard Step 3: Plot Features - UI
- [`mod_feat_step4_lookup_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step4_lookup_server.md)
  : Feature Wizard Step 4: Lookup Matching - Server
- [`mod_feat_step4_lookup_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step4_lookup_ui.md)
  : Feature Wizard Step 4: Lookup Matching - UI
- [`mod_feat_step5_validation_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step5_validation_server.md)
  : Feature Wizard Step 5: Validation - Server
- [`mod_feat_step5_validation_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step5_validation_ui.md)
  : Feature Wizard Step 5: Validation - UI
- [`mod_feat_step6_import_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step6_import_server.md)
  : Feature Wizard Step 6: Import - Server
- [`mod_feat_step6_import_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_feat_step6_import_ui.md)
  : Feature Wizard Step 6: Import - UI
- [`mod_growth_form_selector_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_growth_form_selector_server.md)
  : Growth Form Selector Module - Server
- [`mod_growth_form_selector_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_growth_form_selector_ui.md)
  : Growth Form Selector Module - UI
- [`mod_herbarium_parser_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_herbarium_parser_server.md)
  : Herbarium Parser Module - Server
- [`mod_herbarium_parser_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_herbarium_parser_ui.md)
  : Herbarium Parser Module - UI
- [`mod_individual_search_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_individual_search_server.md)
  : Individual Search Module - Server
- [`mod_individual_search_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_individual_search_ui.md)
  : Individual Search Module - UI
- [`mod_link_executor_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_link_executor_server.md)
  : Link Executor Module - Server
- [`mod_link_executor_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_link_executor_ui.md)
  : Link Executor Module - UI
- [`mod_link_preview_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_link_preview_server.md)
  : Link Preview Module - Server
- [`mod_link_preview_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_link_preview_ui.md)
  : Link Preview Module - UI
- [`mod_plot_filters_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_plot_filters_server.md)
  : Plot Filters Module - Server
- [`mod_plot_filters_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_plot_filters_ui.md)
  : Plot Filters Module - UI
- [`mod_plot_metadata_viewer_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_plot_metadata_viewer_server.md)
  : Plot Metadata Viewer Module - Server
- [`mod_plot_metadata_viewer_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_plot_metadata_viewer_ui.md)
  : Plot Metadata Viewer Module - UI
- [`mod_plot_statistics_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_plot_statistics_server.md)
  : Plot Statistics Module - Server
- [`mod_plot_statistics_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_plot_statistics_ui.md)
  : Plot Statistics Module - UI
- [`mod_results_display_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_results_display_server.md)
  : Results Display Module - Server
- [`mod_results_display_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_results_display_ui.md)
  : Results Display Module - UI
- [`mod_specimen_add_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_add_server.md)
  : Specimen Add Module - Server
- [`mod_specimen_add_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_add_ui.md)
  : Specimen Add Module - UI
- [`mod_specimen_import_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_import_server.md)
  : Specimen Import Module - Server
- [`mod_specimen_import_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_import_ui.md)
  : Specimen Import Module - UI
- [`mod_specimen_lookup_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_lookup_server.md)
  : Specimen Lookup Module - Server
- [`mod_specimen_lookup_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_lookup_ui.md)
  : Specimen Lookup Module - UI
- [`mod_specimen_mapping_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_mapping_server.md)
  : Specimen Mapping Module - Server
- [`mod_specimen_mapping_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_mapping_ui.md)
  : Specimen Mapping Module - UI
- [`mod_specimen_retriever_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_retriever_server.md)
  : Specimen Retriever Module - Server
- [`mod_specimen_retriever_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_retriever_ui.md)
  : Specimen Retriever Module - UI
- [`mod_specimen_search_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_search_server.md)
  : Specimen Search Module - Server
- [`mod_specimen_search_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_search_ui.md)
  : Specimen Search Module - UI
- [`mod_specimen_upload_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_upload_server.md)
  : Specimen Upload Module - Server
- [`mod_specimen_upload_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_specimen_upload_ui.md)
  : Specimen Upload Module - UI
- [`mod_taxa_add_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_add_server.md)
  : Taxa Add Module - Server
- [`mod_taxa_add_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_add_ui.md)
  : Taxa Add Module - UI
- [`mod_taxa_search_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_search_server.md)
  : Taxa Search & Browser Module - Server
- [`mod_taxa_search_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_search_ui.md)
  : Taxa Search & Browser Module - UI
- [`mod_taxa_synonymy_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_synonymy_server.md)
  : Taxa Synonymy Module - Server
- [`mod_taxa_synonymy_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_synonymy_ui.md)
  : Taxa Synonymy Module - UI
- [`mod_taxa_tree_view_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_tree_view_server.md)
  : Taxa Tree View Module - Server
- [`mod_taxa_tree_view_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_tree_view_ui.md)
  : Taxa Tree View Module - UI
- [`mod_taxa_update_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_update_server.md)
  : Taxa Update Module - Server
- [`mod_taxa_update_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxa_update_ui.md)
  : Taxa Update Module - UI
- [`mod_taxonomic_validator_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxonomic_validator_server.md)
  : Taxonomic Validator Module - Server
- [`mod_taxonomic_validator_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_taxonomic_validator_ui.md)
  : Taxonomic Validator Module - UI
- [`mod_trait_column_mapping_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_trait_column_mapping_server.md)
  : Trait Column Mapping Module - Server
- [`mod_trait_column_mapping_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_trait_column_mapping_ui.md)
  : Trait Column Mapping Module - UI
- [`mod_trait_metadata_mapping_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_trait_metadata_mapping_server.md)
  : Trait Metadata Mapping Module - Server
- [`mod_trait_metadata_mapping_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_trait_metadata_mapping_ui.md)
  : Trait Metadata Mapping Module - UI
- [`mod_trait_preview_import_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_trait_preview_import_server.md)
  : Trait Preview & Import Module - Server
- [`mod_trait_preview_import_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_trait_preview_import_ui.md)
  : Trait Preview & Import Module - UI
- [`mod_trait_validation_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_trait_validation_server.md)
  : Trait Validation Module - Server
- [`mod_trait_validation_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_trait_validation_ui.md)
  : Trait Validation Module - UI
- [`mod_update_record_server()`](https://umr-amap.github.io/cafriplotsR/reference/mod_update_record_server.md)
  : Record update module - server
- [`mod_update_record_ui()`](https://umr-amap.github.io/cafriplotsR/reference/mod_update_record_ui.md)
  : Record update module - UI
- [`model_wd_2`](https://umr-amap.github.io/cafriplotsR/reference/model_wd_1.md)
  : model_wd_1
- [`model_wd_2`](https://umr-amap.github.io/cafriplotsR/reference/model_wd_2.md)
  : model_wd_2
- [`phylo_tree`](https://umr-amap.github.io/cafriplotsR/reference/phylo_tree.md)
  : phylo_tree

## Internationalization

Translation and multi-language support utilities for Shiny apps

- [`create_reactive_translator()`](https://umr-amap.github.io/cafriplotsR/reference/create_reactive_translator.md)
  : Create Reactive Translator
- [`get_available_languages()`](https://umr-amap.github.io/cafriplotsR/reference/get_available_languages.md)
  : Get Available Translation Languages
- [`init_translator()`](https://umr-amap.github.io/cafriplotsR/reference/init_translator.md)
  : Initialize Translator for Shiny App

## Backbone Cache

Taxonomic backbone cache management for offline use

- [`cache_exists()`](https://umr-amap.github.io/cafriplotsR/reference/cache_exists.md)
  : Check if valid backbone cache exists
- [`delete_backbone_cache()`](https://umr-amap.github.io/cafriplotsR/reference/delete_backbone_cache.md)
  : Clear backbone cache
- [`get_backbone_cache_path()`](https://umr-amap.github.io/cafriplotsR/reference/get_backbone_cache_path.md)
  : Get backbone cache directory path
- [`get_cache_metadata()`](https://umr-amap.github.io/cafriplotsR/reference/get_cache_metadata.md)
  : Get cache metadata with formatted displays
- [`load_backbone_cache()`](https://umr-amap.github.io/cafriplotsR/reference/load_backbone_cache.md)
  : Load backbone from cache with validation
- [`save_backbone_cache()`](https://umr-amap.github.io/cafriplotsR/reference/save_backbone_cache.md)
  : Save backbone data to cache

## Database Monitoring & Activity

Monitor database activity, backup row-level security tables, and analyze
database usage

- [`get_db_activity()`](https://umr-amap.github.io/cafriplotsR/reference/get_db_activity.md)
  : Collect database activity metrics
- [`init_activity_log()`](https://umr-amap.github.io/cafriplotsR/reference/init_activity_log.md)
  : Initialise the activity log directory and CSV files
- [`log_db_snapshot()`](https://umr-amap.github.io/cafriplotsR/reference/log_db_snapshot.md)
  : Append a 30-minute activity snapshot to the log files
- [`summarize_activity_log()`](https://umr-amap.github.io/cafriplotsR/reference/summarize_activity_log.md)
  : Summarise logged connection activity
- [`read_activity_log()`](https://umr-amap.github.io/cafriplotsR/reference/read_activity_log.md)
  : Read the activity log files into R
- [`plot_activity_log()`](https://umr-amap.github.io/cafriplotsR/reference/plot_activity_log.md)
  : Plot logged database activity
- [`print(`*`<db_activity>`*`)`](https://umr-amap.github.io/cafriplotsR/reference/print.db_activity.md)
  : Print a database activity summary to the console
- [`monitor_db()`](https://umr-amap.github.io/cafriplotsR/reference/monitor_db.md)
  : Collect, print, and save a database activity report in one call
- [`save_db_activity_report()`](https://umr-amap.github.io/cafriplotsR/reference/save_db_activity_report.md)
  : Save a database activity report as an HTML file
- [`setup_db_activity_scheduler()`](https://umr-amap.github.io/cafriplotsR/reference/setup_db_activity_scheduler.md)
  : Register the 30-minute recording script in Windows Task Scheduler
- [`backup_rls_tables()`](https://umr-amap.github.io/cafriplotsR/reference/backup_rls_tables.md)
  : Export tables blocked by FORCE ROW LEVEL SECURITY via DBI
- [`census_link_evidence()`](https://umr-amap.github.io/cafriplotsR/reference/census_link_evidence.md)
  : What the recorded data says about census links, feature by feature
- [`generate_specimen_labels()`](https://umr-amap.github.io/cafriplotsR/reference/generate_specimen_labels.md)
  : Generate printable specimen labels (A4 sticker sheets)

## Administration & Permissions

User management, database permissions, backup, and migration utilities

- [`define_user_policy()`](https://umr-amap.github.io/cafriplotsR/reference/define_user_policy.md)
  : Define user policy for row-level security
- [`define_read_only_policy()`](https://umr-amap.github.io/cafriplotsR/reference/define_read_only_policy.md)
  : Define read-only policy for a user
- [`define_read_write_policy()`](https://umr-amap.github.io/cafriplotsR/reference/define_read_write_policy.md)
  : Define read-write policy for a user
- [`define_full_access_policy()`](https://umr-amap.github.io/cafriplotsR/reference/define_full_access_policy.md)
  : Define full access policy for a user
- [`setup_user_permissions()`](https://umr-amap.github.io/cafriplotsR/reference/setup_user_permissions.md)
  : Setup user permissions on both databases
- [`setup_import_wizard_permissions()`](https://umr-amap.github.io/cafriplotsR/reference/setup_import_wizard_permissions.md)
  : Setup complete import wizard permissions
- [`list_user_policies()`](https://umr-amap.github.io/cafriplotsR/reference/list_user_policies.md)
  : List user policies
- [`register_user()`](https://umr-amap.github.io/cafriplotsR/reference/register_user.md)
  : Register a user in the registry
- [`deactivate_user()`](https://umr-amap.github.io/cafriplotsR/reference/deactivate_user.md)
  : Deactivate a user
- [`reactivate_user()`](https://umr-amap.github.io/cafriplotsR/reference/reactivate_user.md)
  : Reactivate a user
- [`list_database_users()`](https://umr-amap.github.io/cafriplotsR/reference/list_database_users.md)
  : List all database users and their permissions
- [`get_registered_users()`](https://umr-amap.github.io/cafriplotsR/reference/get_registered_users.md)
  : Get all registered users
- [`get_user_emails()`](https://umr-amap.github.io/cafriplotsR/reference/get_user_emails.md)
  : Get email addresses of all registered users
- [`create_user_registry()`](https://umr-amap.github.io/cafriplotsR/reference/create_user_registry.md)
  : Create user registry table
- [`grant_all_table_permissions()`](https://umr-amap.github.io/cafriplotsR/reference/grant_all_table_permissions.md)
  : Grant permissions on ALL tables and sequences
- [`grant_lookup_table_permissions()`](https://umr-amap.github.io/cafriplotsR/reference/grant_lookup_table_permissions.md)
  : Grant table-level permissions for lookup tables
- [`grant_plot_insert_permissions()`](https://umr-amap.github.io/cafriplotsR/reference/grant_plot_insert_permissions.md)
  : Setup permissions for users to insert plots
- [`backup_database()`](https://umr-amap.github.io/cafriplotsR/reference/backup_database.md)
  : Backup PostgreSQL database with timestamp
- [`restore_database()`](https://umr-amap.github.io/cafriplotsR/reference/restore_database.md)
  : Restore database from backup
- [`list_backups()`](https://umr-amap.github.io/cafriplotsR/reference/list_backups.md)
  : List available database backups
- [`cleanup_old_backups()`](https://umr-amap.github.io/cafriplotsR/reference/cleanup_old_backups.md)
  : Delete old database backups
- [`diagnose_plot_permissions()`](https://umr-amap.github.io/cafriplotsR/reference/diagnose_plot_permissions.md)
  : Diagnose plot insertion permissions
- [`diagnose_add_person_setup()`](https://umr-amap.github.io/cafriplotsR/reference/diagnose_add_person_setup.md)
  : Diagnose add_person function and permissions
- [`check_taxa_permissions()`](https://umr-amap.github.io/cafriplotsR/reference/check_taxa_permissions.md)
  : Check taxa database permissions
- [`check_hierarchy_consistency()`](https://umr-amap.github.io/cafriplotsR/reference/check_hierarchy_consistency.md)
  : Check Hierarchy Consistency
- [`check_table_idtax_staleness()`](https://umr-amap.github.io/cafriplotsR/reference/check_table_idtax_staleness.md)
  : Check table_idtax Staleness
- [`get_table_idtax_metadata()`](https://umr-amap.github.io/cafriplotsR/reference/get_table_idtax_metadata.md)
  : Get table_idtax Metadata
- [`print_user_access_summary()`](https://umr-amap.github.io/cafriplotsR/reference/print_user_access_summary.md)
  : Print user access summary
- [`setup_add_person_function()`](https://umr-amap.github.io/cafriplotsR/reference/setup_add_person_function.md)
  : Grant permissions for adding people to table_colnam
