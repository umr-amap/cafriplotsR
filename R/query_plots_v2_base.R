# =====================================
# ÉTAPE 1 : CLASSES DE BASE
# =====================================
# À sauver dans R/query_plots_v2_base.R

# Vérification des dépendances
if (!requireNamespace("R6", quietly = TRUE)) {
  stop("Package R6 requis. Installez-le avec: install.packages('R6')")
}

# =====================================
# 1. CLASSE PlotFilterBuilder
# =====================================

#' Constructeur de requêtes pour filtrer les plots
#' 
#' @description
#' Permet de construire progressivement une requête SQL pour filtrer
#' les plots selon différents critères. Utilise le pattern Builder.
#' 
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' query <- PlotFilterBuilder$new(con)$
#'   filter_country("Gabon")$
#'   filter_method("transect")$
#'   build()
#' }
#' @export
PlotFilterBuilder <- R6::R6Class(
  "PlotFilterBuilder",
  
  private = list(
    con = NULL,
    conditions = character(0),
    
    # Méthode interne pour ajouter une condition
    add_condition = function(condition) {
      if (!is.null(condition) && nchar(condition) > 0) {
        private$conditions <- c(private$conditions, condition)
      }
      invisible(self)
    }
  ),
  
  public = list(
    #' @description Initialiser le builder
    #' @param connection Connexion DBI à la base de données
    initialize = function(connection) {
      stopifnot("Connection must be provided" = !is.null(connection))
      private$con <- connection
    },
    
    #' @description Filtrer par pays
    #' @param country Nom(s) du/des pays
    #' @param interactive Si TRUE, utilise .link_table pour correspondance floue interactive
    filter_country = function(country, interactive = FALSE) {
      if (is.null(country)) return(self)
      
      if (interactive) {
        # Utilisation de .link_table pour correspondance interactive
        data_with_country <- tibble::tibble(country = country)
        
        linked_data <- .link_table(
          data_stand = data_with_country,
          column_searched = "country",
          column_name = "country",
          id_field = "id_country",
          id_table_name = "id_country",
          db_connection = private$con,
          table_name = "table_countries"
        )
        
        country_ids <- linked_data %>%
          filter(!is.na(id_country), id_country != 0) %>%
          pull(id_country)
        
        if (length(country_ids) == 0) {
          cli::cli_alert_warning("No valid countries selected")
          return(self)
        }
        
      } else {
        # Correspondance directe (non-interactive, insensible à la casse)
        countries_tbl <- try_open_postgres_table("table_countries", private$con) %>%
          dplyr::collect() %>%
          dplyr::filter(tolower(country) %in% tolower(!!country))
        
        if (nrow(countries_tbl) == 0) {
          cli::cli_alert_warning("No countries found matching: {paste(country, collapse = ', ')}")
          cli::cli_alert_info("Tip: Use interactive = TRUE for fuzzy matching")
          return(self)
        }
        
        country_ids <- countries_tbl$id_country
      }
      
      condition <- glue::glue_sql(
        "id_country IN ({ids*})", 
        ids = country_ids, 
        .con = private$con
      )
      private$add_condition(condition)
    },
    
    #' @description Filtrer par nom de plot
    #' @param plot_name Nom(s) du/des plots
    #' @param interactive Si TRUE, utilise .link_table pour correspondance floue interactive
    filter_plot_name = function(plot_name, interactive = FALSE) {
      if (is.null(plot_name)) return(self)
      
      if (interactive) {
        # Mode interactif avec .link_table
        data_with_plots <- tibble::tibble(plot_name = plot_name)
        
        linked_data <- .link_table(
          data_stand = data_with_plots,
          column_searched = "plot_name",
          column_name = "plot_name",
          id_field = "id_liste_plots",
          id_table_name = "id_liste_plots",
          db_connection = private$con,
          table_name = "data_liste_plots"
        )
        
        plot_ids <- linked_data %>%
          filter(!is.na(id_liste_plots), id_liste_plots != 0) %>%
          pull(id_liste_plots)
        
        if (length(plot_ids) == 0) {
          cli::cli_alert_warning("No valid plots selected")
          return(self)
        }
        
        # Utiliser filter_by_ids pour ces plots
        condition <- glue::glue_sql(
          "id_liste_plots IN ({ids*})", 
          ids = plot_ids, 
          .con = private$con
        )
        private$add_condition(condition)
        
      } else {
        # Mode non-interactif
        if (length(plot_name) == 1) {
          # Recherche avec pattern matching (insensible à la casse)
          condition <- glue::glue_sql(
            "LOWER(plot_name) LIKE LOWER({pattern})", 
            pattern = paste0("%", plot_name, "%"), 
            .con = private$con
          )
        } else {
          # Recherche exacte pour multiples noms (insensible à la casse)
          condition <- glue::glue_sql(
            "LOWER(plot_name) IN ({names*})", 
            names = tolower(plot_name), 
            .con = private$con
          )
        }
        private$add_condition(condition)
      }
      
      return(self)
    },
    
    #' @description Filtrer par méthode
    #' @param method Nom(s) de(s) méthode(s)
    #' @param interactive Si TRUE, utilise .link_table pour correspondance floue interactive
    filter_method = function(method, interactive = FALSE) {
      if (is.null(method)) return(self)
      
      if (interactive) {
        # Mode interactif avec .link_table
        data_with_methods <- tibble::tibble(method = method)
        
        linked_data <- .link_table(
          data_stand = data_with_methods,
          column_searched = "method",
          column_name = "method",
          id_field = "id_method",
          id_table_name = "id_method",
          db_connection = private$con,
          table_name = "methodslist"
        )
        
        method_ids <- linked_data %>%
          filter(!is.na(id_method), id_method != 0) %>%
          pull(id_method)
        
        if (length(method_ids) == 0) {
          cli::cli_alert_warning("No valid methods selected")
          return(self)
        }
        
        condition <- glue::glue_sql(
          "id_method IN ({ids*})", 
          ids = method_ids, 
          .con = private$con
        )
        private$add_condition(condition)
        
      } else {
        # Mode non-interactif (insensible à la casse)
        patterns <- lapply(method, function(x) {
          glue::glue_sql(
            "LOWER(method) LIKE LOWER({pattern})", 
            pattern = paste0("%", x, "%"), 
            .con = private$con
          )
        })
        
        query_method <- glue::glue_sql(
          "SELECT id_method, method FROM methodslist WHERE {DBI::SQL(glue::glue_collapse(patterns, sep = ' OR '))}",
          .con = private$con
        )
        
        methods_found <- func_try_fetch(con = private$con, sql = query_method)
        
        if (nrow(methods_found) == 0) {
          cli::cli_alert_warning("No methods found matching: {paste(method, collapse = ', ')}")
          cli::cli_alert_info("Tip: Use interactive = TRUE for fuzzy matching")
          return(self)
        }
        
        cli::cli_alert_info("Using methods: {paste(methods_found$method, collapse = ', ')}")
        
        condition <- glue::glue_sql(
          "id_method IN ({ids*})", 
          ids = methods_found$id_method, 
          .con = private$con
        )
        private$add_condition(condition)
      }
      
      return(self)
    },
    
    #' @description Filtrer par localité
    #' @param locality_name Nom(s) de(s) localité(s)
    filter_locality = function(locality_name) {
      if (is.null(locality_name)) return(self)
      
      # Mode non-interactif uniquement (insensible à la casse)
      if (length(locality_name) == 1) {
        condition <- glue::glue_sql(
          "LOWER(locality_name) LIKE LOWER({pattern})", 
          pattern = paste0("%", locality_name, "%"), 
          .con = private$con
        )
      } else {
        # Pour multiples localités, créer condition OR
        locality_conditions <- lapply(locality_name, function(loc) {
          glue::glue_sql(
            "LOWER(locality_name) LIKE LOWER({pattern})", 
            pattern = paste0("%", loc, "%"), 
            .con = private$con
          )
        })
        condition <- paste0("(", paste(locality_conditions, collapse = " OR "), ")")
      }
      private$add_condition(condition)
      
      return(self)
    },
    
    #' @description Construire la requête SQL finale
    #' @param operator Opérateur de jointure entre conditions ("AND" ou "OR")
    #' @return SQL query object
    build = function(operator = "AND") {
      base_query <- "SELECT * FROM data_liste_plots"
      
      if (length(private$conditions) > 0) {
        # Validation de l'opérateur
        operator <- match.arg(toupper(operator), c("AND", "OR"))
        
        where_clause <- paste(private$conditions, collapse = paste0(" ", operator, " "))
        full_query <- glue::glue_sql(
          "{DBI::SQL(base_query)} WHERE {DBI::SQL(where_clause)}", 
          .con = private$con
        )
        
        if (operator == "OR") {
          cli::cli_alert_info("Using OR operator between filter conditions")
        }
      } else {
        full_query <- glue::glue_sql("{DBI::SQL(base_query)}", .con = private$con)
      }
      
      return(full_query)
    },
    
    #' @description Construire avec opérateur OR explicite
    #' @return SQL query object  
    build_with_or = function() {
      return(self$build(operator = "OR"))
    },
    
    #' @description Ajouter une condition personnalisée
    #' @param condition Condition SQL brute
    #' @param wrap_parentheses Si TRUE, entoure la condition de parenthèses
    add_custom_condition = function(condition, wrap_parentheses = TRUE) {
      if (!is.null(condition) && nchar(condition) > 0) {
        if (wrap_parentheses) {
          condition <- paste0("(", condition, ")")
        }
        private$add_condition(condition)
      }
      invisible(self)
    },
    
    #' @description Afficher les conditions actuelles (pour debug)
    print_conditions = function() {
      if (length(private$conditions) == 0) {
        cli::cli_alert_info("No filter conditions set")
      } else {
        cli::cli_h3("Current filter conditions:")
        for (i in seq_along(private$conditions)) {
          cli::cli_li("{private$conditions[i]}")
        }
      }
      invisible(self)
    }
  )
)

# =====================================
# 2. CLASSE PlotFetcher
# =====================================



# =====================================
# 3. HELPER : Validation des paramètres
# =====================================

#' Valider et normaliser les paramètres de query_plots
#' 
#' @description
#' Fonction utilitaire pour valider les paramètres et résoudre les conflits
#' 
#' @param params Liste de paramètres
#' @return Liste de paramètres validés et normalisés
#' @export
validate_query_params <- function(params) {
  
  # Paramètres par défaut
  defaults <- list(
    plot_name = NULL,
    tag = NULL,
    country = NULL,
    locality_name = NULL,
    method = NULL,
    extract_individuals = FALSE,
    map = FALSE,
    id_individual = NULL,
    id_plot = NULL,
    id_tax = NULL,
    id_specimen = NULL,
    show_multiple_census = FALSE,
    show_all_coordinates = FALSE,
    remove_ids = TRUE,
    collapse_multiple_val = FALSE,
    extract_traits = TRUE,
    extract_individual_features = TRUE,
    traits_to_genera = FALSE,
    wd_fam_level = FALSE,
    include_liana = FALSE,
    extract_subplot_features = TRUE,
    concatenate_stem = FALSE,
    remove_obs_with_issue = TRUE
  )
  
  # Fusion avec defaults
  params <- modifyList(defaults, params)
  
  # Résolution des conflits
  if (params$show_multiple_census && params$remove_obs_with_issue) {
    cli::cli_alert_info("Disabling remove_obs_with_issue because show_multiple_census = TRUE")
    params$remove_obs_with_issue <- FALSE
  }
  
  # Force extract_individuals si nécessaire
  force_extract <- !is.null(params$tag) || 
    !is.null(params$id_individual) || 
    !is.null(params$id_tax) ||
    !is.null(params$id_specimen)
  
  if (force_extract && !params$extract_individuals) {
    cli::cli_alert_info("Setting extract_individuals = TRUE (required by other parameters)")
    params$extract_individuals <- TRUE
  }
  
  # Validations basiques
  stopifnot(
    "extract_individuals must be logical" = is.logical(params$extract_individuals),
    "map must be logical" = is.logical(params$map),
    "remove_ids must be logical" = is.logical(params$remove_ids)
  )
  
  return(params)
}

# =====================================
# 4. FONCTION DE TEST
# =====================================

#' Tester les nouvelles classes (pour développement)
#' 
#' @description
#' Fonction pour tester PlotFilterBuilder et PlotFetcher
#' 
#' @export
test_plot_filtering <- function() {
  cli::cli_h1("Test des classes de base")
  
  # Connexion
  con <- call.mydb()
  
  # Test 1: Construction de filtre simple
  cli::cli_h2("Test 1: Filtre par pays")
  builder <- PlotFilterBuilder$new(con)
  query1 <- builder$filter_country("Gabon")$build()
  cli::cli_alert_info("Query: {as.character(query1)}")
  
  # Test 2: Filtre multiple
  cli::cli_h2("Test 2: Filtre combiné")
  builder2 <- PlotFilterBuilder$new(con)
  query2 <- builder2$
    filter_country("Cameroon")$
    filter_method("transect")$
    print_conditions()$
    build()
  
  # Test 3: Extraction réelle
  cli::cli_h2("Test 3: Extraction de plots")
  fetcher <- PlotFetcher$new(con)
  
  # Par filtre
  plots_filtered <- fetcher$fetch_with_filter(query2)
  cli::cli_alert_success("Extracted {nrow(plots_filtered)} plots via filter")
  
  # Par IDs (si on a des résultats)
  if (nrow(plots_filtered) > 0) {
    test_ids <- head(plots_filtered$id_liste_plots, 3)
    plots_by_ids <- fetcher$fetch_by_ids(test_ids)
    cli::cli_alert_success("Extracted {nrow(plots_by_ids)} plots by IDs")
  }
  
  # Test 4: Validation de paramètres
  cli::cli_h2("Test 4: Validation de paramètres")
  test_params <- list(
    country = "Gabon",
    tag = "A01",
    show_multiple_census = TRUE,
    remove_obs_with_issue = TRUE
  )
  
  validated <- validate_query_params(test_params)
  cli::cli_alert_info("extract_individuals forcé à: {validated$extract_individuals}")
  cli::cli_alert_info("remove_obs_with_issue ajusté à: {validated$remove_obs_with_issue}")
  
  cli::cli_alert_success("Tous les tests passés !")
  
  return(invisible(list(
    plots_filtered = plots_filtered,
    validated_params = validated
  )))
}