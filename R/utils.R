#' @importFrom magrittr %>%
#' @importFrom stats as.formula na.omit setNames
#' @importFrom utils capture.output head object.size read.csv setTxtProgressBar txtProgressBar write.csv
NULL

utils::globalVariables(c(
  "%m-%", ".", ".BY", ".N", ".SD", ".build_plot_query", ".census_typevalue",
  ".original_value", ".src", ".subplot_plot_id", ":=",
  "Crown_spre", "DBH", "DBH_height", "Date", "Family", "Genus",
  "ID", "Identif_co", "Latitude", "Longitude", "N", "New_quadra",
  "Observatio", "PI", "Rainfor_Tr", "Species", "Total_heig",
  "Var1", "Var2", "X", "XAbs", "XRel", "X_theo", "Xrel",
  "Y", "YAbs", "YRel", "Y_theo", "Yrel",
  "ab", "accepted_name", "additional_people", "author1", "author2", "author3",
  "basisofrecord", "category", "census_date", "census_day", "census_month",
  "census_name", "census_name_0", "census_name_1", "census_name_selected",
  "census_number", "census_rank", "census_typevalue", "census_year",
  "check", "citation_authors", "citation_dataset_name", "citation_doi",
  "citation_journal", "citation_key", "citation_title", "citation_year",
  "cleaned_name", "codeUTM", "code_individu", "col_name", "cold",
  "coll", "collector_name", "colm", "colnam", "colnam_specimen",
  "colnbr", "coly", "comparison", "confidence", "confirm_new_ident",
  "consec_missing", "coord1", "coord2", "coord3", "coord4",
  "corrected_name", "country", "crown_width", "current_value_char",
  "current_value_num", "date_census0", "date_census1", "date_char",
  "date_d", "date_julian", "date_m", "date_modif_d", "date_modif_m",
  "date_modif_y", "date_modified", "date_y", "day", "day_clean",
  "db_host", "db_name", "db_name_taxa", "db_port",
  "dbh", "dbh_display", "dbh_height", "ddlat", "ddlon",
  "decimallatitude", "decimallongitude", "depth", "det_date",
  "detby", "detd", "detm", "dety", "diam_status", "dirs",
  "eligible", "evidence_source", "expand_grid", "expectedunit",
  "extracted_collector", "extracted_number",
  "factorlevels", "factorlevels_list", "family_name", "fct_recode",
  "first_measured_rank", "first_name", "fk_id_trait",
  "flag1_alive", "flag1_val", "flag1_value", "flag_dead", "flag_k",
  "flag_val", "flag_val_norm", "flag_value",
  "full_name_no_auth", "full_name_no_auth_linked",
  "geometry", "gps", "has_diam",
  "herbarium_code_char", "herbarium_nbe_char", "herbarium_nbe_type",
  "id", "idDB", "id_brlu", "id_citation", "id_colnam", "id_country",
  "id_data_individuals", "id_diconame", "id_diconame_n",
  "id_fol_up_diconame", "id_linktype", "id_liste_plots", "id_measure",
  "id_method", "id_n", "id_new_data", "id_old", "id_parent",
  "id_senterre_db", "id_specimen", "id_sub_plots", "id_subplottype",
  "id_subplotype", "id_table_colnam", "id_table_liste_plots",
  "id_table_liste_plots_n", "id_tax_famclass", "id_temp", "id_trait",
  "id_trait_measures", "id_tropicos", "id_type_sub_plot",
  "ids_agg", "idtax", "idtax_f", "idtax_good", "idtax_good_n",
  "idtax_individual_f", "idtax_n", "idtax_n.x", "idtax_resolved",
  "idtax_specimen_f",
  "individual_genus", "individual_idtax_n", "individual_idx",
  "individual_label", "individual_taxon",
  "input_name", "input_names", "is_synonym", "issue", "issue_agg",
  "issue_new", "issue_tax", "issues_dup",
  "key", "last_census", "last_name", "last_year",
  "lat", "level", "liana", "link_type", "linktype", "list_factors",
  "locality_name", "long",
  "mapped_to", "match_method", "match_rank", "match_score",
  "match_status", "matched_name", "maxallowedvalue", "mean_id",
  "measurementremarks", "method", "minallowedvalue", "modif_type",
  "month", "month_clean", "mydb", "mydb_taxa",
  "n_census", "n_censuses_plot", "n_individuals", "n_measurements",
  "n_measures", "n_subplots", "n_type_links", "name", "nbrs",
  "new_value", "number", "number_of_stem",
  "obs_alive", "obs_dead", "obs_missing", "obs_text", "observation",
  "old_value", "original_colnam", "original_tax_name", "original_value",
  "plot_name", "priority", "quadrat", "reduce", "relatedterm",
  "remeasured_later", "required", "rn", "row_num", "rrr",
  "similarity", "similarity_score", "sort_key", "source_column",
  "source_taxa_mean_wood_density", "source_taxa_sd_wood_density",
  "sousplot", "species", "specimen_genus", "specimen_idtax_n",
  "specimen_idx", "specimen_label", "specimen_taxon",
  "stem_diameter", "stem_grouping", "stem_status", "stem_vital_status",
  "str_split", "subplot", "subplottype", "subplotype", "suffix",
  "surname", "synonym_status",
  "tax_class_level", "tax_esp", "tax_fam", "tax_fam_level",
  "tax_fam_linked", "tax_famclass", "tax_gen", "tax_gen_good",
  "tax_gen_level", "tax_gen_linked", "tax_infra_level",
  "tax_infra_level_auth", "tax_level", "tax_nam01", "tax_nam02",
  "tax_order", "tax_rank01", "tax_rank02", "tax_rankinf",
  "tax_sp_level",
  "taxa_mean_wood_density", "taxa_mean_wood_density_plot_level",
  "taxa_sd_wood_density", "taxa_sd_wood_density_plot_level",
  "taxo_display", "taxo_status", "taxonomic_match",
  "team_leader", "trait", "traitdescription", "traitid",
  "traitvalue", "traitvalue_char", "traitvalue_exist",
  "traitvalue_num", "tree_height", "true_value",
  "type", "typevalue", "typevalue_char", "typevalue_ddlat",
  "typevalue_ddlon", "typevalue_num", "typevalue_old",
  "unlinked_individuals", "validation_status", "value", "valuetype",
  "wd_fam_level", "wd_ind_level",
  "x", "x_100", "x_filled", "x_quadrat",
  "y", "y_100", "y_filled", "y_quadrat",
  "year", "year_clean", "year_description"
))

#' Conditional rclipboard button
#'
#' Returns an rclipboard copy button when the rclipboard package is available,
#' or NULL otherwise.  Drop-in replacement for rclipboard::rclipButton().
#'
#' @param ... Arguments passed to rclipboard::rclipButton()
#' @return A Shiny tag or NULL
#' @keywords internal
#' @noRd
.rclip_button <- function(...) {
  if (!requireNamespace("rclipboard", quietly = TRUE)) return(NULL)
  rclipboard::rclipButton(...)
}

#' Conditional rclipboard setup
#'
#' Returns rclipboard JS/CSS dependencies when available, or NULL.
#'
#' @return A Shiny dependency tag or NULL
#' @keywords internal
#' @noRd
.rclipboard_setup <- function() {
  if (!requireNamespace("rclipboard", quietly = TRUE)) return(NULL)
  rclipboard::rclipboardSetup()
}
