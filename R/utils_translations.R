# Translation System for Shiny App
#
# Provides bilingual support (English/French) for the taxonomic matching Shiny app.
#
# Functions:
# - get_translations(): Get translation dictionary for specified language
# - t_(): Translate a single key

#' Get translation dictionary
#'
#' @param language Character, either "en" or "fr"
#'
#' @return Named list of translations
#'
#' @keywords internal
get_translations <- function(language = "en") {

  language <- match.arg(language, c("en", "fr"))

  translations <- list(

    # English translations
    en = list(
      # App title and main sections
      app_title = "Taxonomic Name Standardization for Tropical African Plants",
      app_subtitle = "Standardize species names against the taxonomic backbone",

      # Tabs
      tab_auto_match = "Auto Match",
      tab_review = "Review",
      tab_export = "Export",

      # Data Input Module
      data_input_title = "Data Input",
      data_upload_file = "Upload Excel file",
      data_upload_help = "Select an Excel file (.xlsx) containing taxonomic names",
      data_choose_file = "Choose file...",
      data_using_r_data = "Using R data from environment",
      data_rows = "rows",
      data_columns = "columns",

      # Column Selection Module
      column_select_title = "Column Selection",
      column_select_name = "Select name column:",
      column_select_help = "Choose the column containing taxonomic names to standardize",
      column_match_authors = "Match with author names",
      column_match_authors_help = "Include author names in matching (slower but more precise)",

      # Auto Matching Module
      auto_match_title = "Automatic Matching",
      auto_match_start = "Start Matching",
      auto_match_running = "Matching in progress...",
      auto_match_complete = "Matching complete!",
      auto_match_summary = "Matching Summary",
      auto_match_total = "Total unique names:",
      auto_match_exact = "Exact matches:",
      auto_match_genus = "Genus-level matches:",
      auto_match_fuzzy = "Fuzzy matches:",
      auto_match_unmatched = "Requiring review:",
      auto_match_min_sim = "Minimum similarity:",
      auto_match_min_sim_help = "Threshold for fuzzy matching (0-1, higher = stricter)",

      # Progress Tracker Module
      progress_title = "Progress Summary",
      progress_total = "Total unique names:",
      progress_matched = "Successfully matched:",
      progress_review = "Requiring review:",
      progress_remaining = "Remaining:",
      progress_percent = "Progress:",

      # Name Review Module
      review_title = "Review Unmatched Names",
      review_remaining = "remaining",
      review_input_name = "Input name:",
      review_suggestions = "Suggestions (ranked by similarity):",
      review_no_suggestions = "No suggestions found",
      review_custom_name = "Enter custom name:",
      review_mark_unresolved = "Mark as unresolved",
      review_select_match = "Select",
      review_score = "score:",
      review_family = "Family:",
      review_genus = "Genus:",
      review_method = "Match method:",
      review_prev = "Previous",
      review_next = "Next",
      review_skip = "Skip",
      review_accept = "Accept",
      review_num_suggestions = "Number of suggestions:",
      review_sort = "Sort by:",
      review_sort_similarity = "Similarity",
      review_sort_alphabetical = "Alphabetical",
      review_mode = "Review mode:",
      review_mode_interactive = "Interactive (one-by-one)",
      review_mode_batch = "Batch (table view)",

      # Fuzzy Suggestions Module
      suggestions_title = "Suggestions for:",
      suggestions_no_match = "No matches found",
      suggestions_show_more = "Show more...",
      suggestions_show_authors = "Show authors",

      # Results Export Module
      export_title = "Export Results",
      export_format = "Export format:",
      export_format_xlsx = "Excel (.xlsx) - recommended",
      export_format_csv = "CSV (.csv)",
      export_format_rds = "R data (.rds)",
      export_include = "Include columns:",
      export_include_original = "Original data",
      export_include_ids = "Matched IDs (idtax_n, idtax_good_n)",
      export_include_corrected = "Corrected names",
      export_include_metadata = "Match metadata (method, score)",
      export_include_hierarchy = "Taxonomic hierarchy (family, genus)",
      export_download = "Download",
      export_filename = "standardized_taxonomy",
      export_ready = "Results ready for export",
      export_not_ready = "Complete matching and review before exporting",

      # Language Toggle Module
      language_label = "Language:",
      language_en = "English",
      language_fr = "Français",

      # Messages and alerts
      msg_loading = "Loading...",
      msg_processing = "Processing...",
      msg_error = "Error:",
      msg_warning = "Warning:",
      msg_success = "Success:",
      msg_no_data = "No data loaded",
      msg_select_column = "Please select a column to standardize",
      msg_no_unmatched = "All names successfully matched!",
      msg_file_uploaded = "File uploaded successfully",
      msg_matching_complete = "Matching process completed",

      # Buttons and actions
      btn_start = "Start",
      btn_stop = "Stop",
      btn_continue = "Continue",
      btn_reset = "Reset",
      btn_cancel = "Cancel",
      btn_close = "Close",
      btn_help = "Help",

      # Units and formatting
      unit_percent = "%",
      unit_of = "of"
    ),

    # French translations
    fr = list(
      # App title and main sections
      app_title = "Standardisation de noms taxonomiques pour les plantes d'Afrique tropicale",
      app_subtitle = "Standardiser les noms d'espèces selon le référentiel taxonomique",

      # Tabs
      tab_auto_match = "Appariement auto",
      tab_review = "Révision",
      tab_export = "Exporter",

      # Data Input Module
      data_input_title = "Données d'entrée",
      data_upload_file = "Télécharger un fichier Excel",
      data_upload_help = "Sélectionner un fichier Excel (.xlsx) contenant les noms taxonomiques",
      data_choose_file = "Choisir un fichier...",
      data_using_r_data = "Utilisation des données R de l'environnement",
      data_rows = "lignes",
      data_columns = "colonnes",

      # Column Selection Module
      column_select_title = "Sélection de colonne",
      column_select_name = "Sélectionner la colonne de noms :",
      column_select_help = "Choisir la colonne contenant les noms taxonomiques à standardiser",
      column_match_authors = "Apparier avec les noms d'auteurs",
      column_match_authors_help = "Inclure les noms d'auteurs dans l'appariement (plus lent mais plus précis)",

      # Auto Matching Module
      auto_match_title = "Appariement automatique",
      auto_match_start = "Démarrer l'appariement",
      auto_match_running = "Appariement en cours...",
      auto_match_complete = "Appariement terminé !",
      auto_match_summary = "Résumé de l'appariement",
      auto_match_total = "Total de noms uniques :",
      auto_match_exact = "Correspondances exactes :",
      auto_match_genus = "Correspondances au niveau du genre :",
      auto_match_fuzzy = "Correspondances approximatives :",
      auto_match_unmatched = "Nécessitant une révision :",
      auto_match_min_sim = "Similarité minimale :",
      auto_match_min_sim_help = "Seuil pour l'appariement approximatif (0-1, plus élevé = plus strict)",

      # Progress Tracker Module
      progress_title = "Résumé de progression",
      progress_total = "Total de noms uniques :",
      progress_matched = "Appariés avec succès :",
      progress_review = "Nécessitant une révision :",
      progress_remaining = "Restant :",
      progress_percent = "Progression :",

      # Name Review Module
      review_title = "Réviser les noms non appariés",
      review_remaining = "restant(s)",
      review_input_name = "Nom saisi :",
      review_suggestions = "Suggestions (classées par similarité) :",
      review_no_suggestions = "Aucune suggestion trouvée",
      review_custom_name = "Saisir un nom personnalisé :",
      review_mark_unresolved = "Marquer comme non résolu",
      review_select_match = "Sélectionner",
      review_score = "score :",
      review_family = "Famille :",
      review_genus = "Genre :",
      review_method = "Méthode d'appariement :",
      review_prev = "Précédent",
      review_next = "Suivant",
      review_skip = "Passer",
      review_accept = "Accepter",
      review_num_suggestions = "Nombre de suggestions :",
      review_sort = "Trier par :",
      review_sort_similarity = "Similarité",
      review_sort_alphabetical = "Alphabétique",
      review_mode = "Mode de révision :",
      review_mode_interactive = "Interactif (un par un)",
      review_mode_batch = "Par lot (vue tableau)",

      # Fuzzy Suggestions Module
      suggestions_title = "Suggestions pour :",
      suggestions_no_match = "Aucune correspondance trouvée",
      suggestions_show_more = "Afficher plus...",
      suggestions_show_authors = "Afficher les auteurs",

      # Results Export Module
      export_title = "Exporter les résultats",
      export_format = "Format d'export :",
      export_format_xlsx = "Excel (.xlsx) - recommandé",
      export_format_csv = "CSV (.csv)",
      export_format_rds = "Données R (.rds)",
      export_include = "Inclure les colonnes :",
      export_include_original = "Données originales",
      export_include_ids = "IDs appariés (idtax_n, idtax_good_n)",
      export_include_corrected = "Noms corrigés",
      export_include_metadata = "Métadonnées d'appariement (méthode, score)",
      export_include_hierarchy = "Hiérarchie taxonomique (famille, genre)",
      export_download = "Télécharger",
      export_filename = "taxonomie_standardisee",
      export_ready = "Résultats prêts pour l'export",
      export_not_ready = "Terminer l'appariement et la révision avant d'exporter",

      # Language Toggle Module
      language_label = "Langue :",
      language_en = "English",
      language_fr = "Français",

      # Messages and alerts
      msg_loading = "Chargement...",
      msg_processing = "Traitement...",
      msg_error = "Erreur :",
      msg_warning = "Avertissement :",
      msg_success = "Succès :",
      msg_no_data = "Aucune donnée chargée",
      msg_select_column = "Veuillez sélectionner une colonne à standardiser",
      msg_no_unmatched = "Tous les noms ont été appariés avec succès !",
      msg_file_uploaded = "Fichier téléchargé avec succès",
      msg_matching_complete = "Processus d'appariement terminé",

      # Buttons and actions
      btn_start = "Démarrer",
      btn_stop = "Arrêter",
      btn_continue = "Continuer",
      btn_reset = "Réinitialiser",
      btn_cancel = "Annuler",
      btn_close = "Fermer",
      btn_help = "Aide",

      # Units and formatting
      unit_percent = "%",
      unit_of = "de"
    )
  )

  return(translations[[language]])
}


#' Translate a key
#'
#' Helper function to translate a single key in the current language
#'
#' @param key Character, translation key
#' @param language Character, either "en" or "fr"
#'
#' @return Translated string
#'
#' @keywords internal
t_ <- function(key, language = "en") {
  translations <- get_translations(language)

  if (key %in% names(translations)) {
    return(translations[[key]])
  } else {
    cli::cli_alert_warning("Translation key not found: {key}")
    return(key)
  }
}
