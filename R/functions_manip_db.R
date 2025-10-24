


#' List of countries
#'
#' Provide list of countries where plots occur
#'
#' @return A tibble of all countries
#' @import dplyr
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
country_list <- function() {

  mydb <- call.mydb()

  nn <-
    dplyr::tbl(mydb, "data_liste_plots")
  nn <-
    dplyr::group_by(nn, country) %>%
    summarise(n = n())
  nn <-
    nn %>%
    collect() %>%
    dplyr::mutate(n = as.integer(n)) %>%
    arrange(country)

  return(nn)
}



#' List of method
#'
#' Provide list of method where plots occur
#'
#' @return A tibble of all method
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @importFrom stringr str_squish
#'
#' @export
method_list <- function() {

  mydb <- call.mydb()

  nn <- func_try_fetch(con = mydb, 
                       sql = glue::glue_sql("SELECT * FROM methodslist"))
  
  return(nn)
}



#' Add a method in method list
#'
#' Add method and associated descriptors in method list table
#'
#' @return nothing
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_method string value with new method descriptors, avoid space
#' @param new_description_method
#'
#'
#' @export
add_method <- function(new_method = NULL,
                       new_description_method = NULL) {

  if(is.null(new_method)) stop("define new method")

  mydb <- call.mydb()

  new_data_renamed <- tibble(
    method = new_method,
    description_method = ifelse(is.null(new_description_method), NA, new_description_method)
  )

  print(new_data_renamed)

  # Q <- utils::askYesNo("confirm adding this method ?")
  Q <- choose_prompt(message = "confirm adding this method ?")

  if(Q) DBI::dbWriteTable(mydb, "methodslist", new_data_renamed, append = TRUE, row.names = FALSE)

  
  on.exit(DBI::dbDisconnect(mydb), add = TRUE)
}






#' Query plots from database
#'
#' @description
#' This function queries a PostgreSQL inventory database to return a list of forest plots or individuals, 
#' with options to include associated traits and metadata, and to generate interactive maps.
#' 
#' @param plot_name Optional. A single string specifying plot name.
#' @param tag Optional. Tag identifier.
#' @param country Optional. A single string specifying country.
#' @param locality_name Optional. A single string specifying locality name.
#' @param method Optional. Method identifier.
#' @param extract_individuals Logical. Whether to extract individuals. Optional.
#' @param map Logical. Whether to generate map. Optional.
#' @param id_individual Optional. Individual identifiers.
#' @param id_plot Optional. Plot identifiers.
#' @param id_tax Optional. Taxonomic identifiers.
#' @param id_specimen Optional. Specimen identifiers.
#' @param show_multiple_census Logical. Whether to show multiple census data. Optional.
#' @param show_all_coordinates Logical. Whether to show all coordinates. Optional.
#' @param remove_ids Logical. Whether to remove ID columns from output. Optional.
#' @param collapse_multiple_val Logical. Whether to collapse multiple values. Optional.
#' @param extract_traits Logical. Whether to extract taxonomic traits. Optional.
#' @param extract_individual_features Logical. Whether to extract individual-level features. Optional.
#' @param traits_to_genera Logical. Whether to aggregate traits to genus level. Optional.
#' @param wd_fam_level Logical. Whether to use family-level wood density. Optional.
#' @param include_liana Logical. Whether to include lianas. Optional.
#' @param extract_subplot_features Logical. Whether to extract subplot features. Optional.
#' @param concatenate_stem Logical. Whether to concatenate multiple stems. Optional.
#' @param remove_obs_with_issue Logical. Whether to remove observations with issues. Optional.
#'
#' @returns 
#' A list or data frame containing plot data and associated information. When multiple 
#' components are requested, returns a list with elements like `extract`, `census_features`, 
#' `coordinates`, and `coordinates_sf`. If only one component is available, returns that 
#' component directly. Returns `NA` if no plots are found matching the criteria.
#'
#' @importFrom DBI dbSendQuery dbFetch dbClearResult dbWriteTable
#' @importFrom stringr str_flatten str_trim str_extract
#' @importFrom date as.date
#' @importFrom tidyselect vars_select_helpers
#' @importFrom BIOMASS correctCoordGPS
#' @importFrom glue glue_sql
#' @importFrom stringr str_c
#'
#' @examples
#' \dontrun{
#'   query_plots(country = "Gabon", extract_individuals = FALSE)
#'   
#'   query_plots(country = "Cameroon")
#'   
#'   query_plots(plot_name = "mbalmayo001")
#' }
#' 
#' 
#' @export
query_plots <- function(plot_name = NULL,
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
                        remove_obs_with_issue = TRUE,
                        include_issue = FALSE,
                        include_measurement_ids = FALSE,
                        output_style = c("auto", "minimal", "standard", "permanent_plot", "transect", "full")) {

  # Match output style argument
  output_style <- match.arg(output_style)

  mydb <- call.mydb()
  mydb.taxa <- call.mydb.taxa()
  
  if (show_multiple_census && remove_obs_with_issue)
    cli::cli_alert_info("Disabling `remove_obs_with_issue` because multiple censuses are shown")
  
  remove_obs_with_issue <- if (show_multiple_census) FALSE else remove_obs_with_issue
  
  
  if (!is.null(id_individual) | !is.null(id_specimen))
  {
    

    if (!is.null(id_specimen)) {

      if (!is.null(id_individual)) 
        cli::cli_alert_info("id_individual provided is not considered and individuals linked to id_specimen is used instead")
      
      id_individual <- .extract_by_specimen(id_specimen = id_specimen, con = mydb)
      
    }
    
    cli::cli_rule(left = "Extracting from queried individuals - id_individual")
    extract_individuals <- TRUE

    if(!is.null(id_plot))
      cli::cli_alert_warning("id_plot not null replaced by id_plot of the id_individuals")

    tbl <- "data_individuals"
    sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE id_n IN ({vals*})",
                         vals = id_individual, .con = mydb)
    

    
    res <- func_try_fetch(con = mydb, sql = sql)

    id_plot <-
      res %>%
      dplyr::distinct(id_table_liste_plots_n) %>%
      pull()
    
    

  }
  
  if (!is.null(tag) | !is.null(id_individual)) {
    if (!extract_individuals) 
      cli::cli_alert_info("extract_individuals is set as TRUE because tag or id_individual is not null")
    extract_individuals <- TRUE
  }

  if (!is.null(id_tax))
  {

    cli::cli_rule(left = "Extracting from queried taxa - idtax_n")
    extract_individuals <- TRUE

    if(!is.null(id_plot))
      cli::cli_alert_warning("id_plot not null replaced by id_plot where idtax_n are found")

    id_plot <-
      merge_individuals_taxa(id_tax = id_tax) %>%
      pull(id_table_liste_plots_n)

  }

  if (is.null(id_plot)) {
    cli::cli_rule(left = "Building plot filter query")
    
    query_builder <- PlotFilterBuilder$new(mydb)
    
    if (!is.null(country)) {
      query_builder <- query_builder$filter_country(country)
    }
    
    if (!is.null(plot_name)) {
      query_builder <- query_builder$filter_plot_name(plot_name)
    }
    
    if (!is.null(method)) {
      query_builder <- query_builder$filter_method(method)
    }
    
    if (!is.null(locality_name)) {
      query_builder <- query_builder$filter_locality(locality_name)
    }
    
    query <- query_builder$build()
    res <- func_try_fetch(con = mydb, sql = query)
    
  } else {
    cli::cli_rule(left = "Extracting from queried plot - id_plot")
    
    fetcher <- PlotFetcher$new(mydb)
    res <- fetcher$fetch_by_ids(id_plot)
  }
  
  res <-
    res %>%
    dplyr::select(-any_of(c("id_old")))
  
  res <- 
    .link_metadata_tables(res = res, con = mydb)

  if (extract_subplot_features & nrow(res) > 0) {
    
    # Use new function
    all_subplots <- query_plot_features(
      plot_ids = res$id_liste_plots,
      format = "wide",
      include_subplot_obs_features = TRUE
    )
    
    # Handle census features
    if (is.data.frame(all_subplots$census_info)) {
      census_features <- 
        all_subplots$features_raw %>%
        dplyr::filter(type == "census") %>%
        left_join(
          res %>% select(plot_name, id_liste_plots),
          by = c("id_table_liste_plots" = "id_liste_plots")
        ) %>%
        relocate(plot_name, .before = year)
    }
    
    # Clean up columns
    res <- res %>%
      select(-any_of(c("additional_people", "team_leader", "data_provider")))
    
    # Join aggregated features
    if (is.data.frame(all_subplots$features_aggregated)) {
      res <- res %>%
        left_join(
          all_subplots$features_aggregated,
          by = c("id_liste_plots" = "id_table_liste_plots")
        )
    }
    
    # Relocate fields
    relocate_fields <- c(
      "plot_name",
      "data_manager",
      "principal_investigator",
      "additional_people",
      "team_leader",
      "data_provider"
    )
    
    res <- res %>%
      relocate(any_of(relocate_fields), .after = "plot_name")
    
    # Handle coordinates if requested
    if (show_all_coordinates) {
      
      # Extract coordinate features
      if (is.data.frame(all_subplots$features_raw)) {
        
        all_ids_subplot_coordinates <- all_subplots$features_raw %>%
          dplyr::filter(grepl("ddlon|ddlat", type))
        
      } else {
        all_ids_subplot_coordinates <- tibble()
      }
      
      if (nrow(all_ids_subplot_coordinates) > 0) {
        
        cli::cli_alert_info('Extracting coordinates')
        
        all_coordinates_subplots_rf <- all_ids_subplot_coordinates %>%
          mutate(
            coord2 = map_chr(str_split(type, "_"), ~.x[length(.x)]),
            coord1 = map_chr(str_split(type, "_"), ~.x[length(.x) - 1]),
            coord3 = map_chr(str_split(type, "_"), ~.x[1]),
            coord4 = map_chr(str_split(type, "_"), ~.x[2])
          ) %>%
          select(
            coord1, coord2, coord3, coord4,
            type, typevalue, id_sub_plots, id_table_liste_plots
          ) %>%
          arrange(coord2) %>%
          left_join(
            res %>% select(id_liste_plots, method),
            by = c("id_table_liste_plots" = "id_liste_plots")
          )
        
        all_coordinates_subplots_rf <- all_coordinates_subplots_rf %>%
          dplyr::filter(coord4 == "plot")
        
        if (nrow(all_coordinates_subplots_rf) > 0) {
          
          coord_processed <- .process_coordinates(
            all_coordinates = all_coordinates_subplots_rf,
            res = res
          )
          
          coordinates_subplots_plot_sf <- coord_processed$coordinates_sf %>%
            left_join(
              res %>% select(id_liste_plots, plot_name),
              by = "id_liste_plots"
            )
          
          coordinates_subplots <- coord_processed$coordinates_raw %>%
            left_join(
              res %>% select(id_liste_plots, plot_name),
              by = c("id_table_liste_plots" = "id_liste_plots")
            )
        }
        
      } else {
        show_all_coordinates <- FALSE
        cli::cli_alert_danger("No coordinates for quadrat available")
      }
    }
  }
  

  if (nrow(res) == 0) {
    cli::cli_alert_danger("No plot are found based on inputs")
    return(NA)
  }

  res <- res %>% dplyr::arrange(plot_name)

  if (map) {

    cli::cli_rule(left = "Mapping")

    if(any(is.na(res$ddlat)) | any(is.na(res$ddlon))) {
      not_georef_plot <-
        dplyr::filter(res, is.na(ddlat), is.na(ddlon)) %>%
        dplyr::pull(plot_name)

      cli::cli_alert_warning("removing following plots because missing coordinates: {not_georef_plot}")

    }

    res <-
      res %>%
      dplyr::filter(!is.na(ddlat), !is.na(ddlon)) %>%
      dplyr::select(-id_senterre_db)

    data_sf <- sf::st_as_sf(res, coords = c("ddlon", "ddlat"), crs = 4326)
    bbox_data <- sf::st_bbox(data_sf)

    map_types <- c("OpenStreetMap.DE",
                   "Esri.WorldImagery",
                   "Esri.WorldPhysical")

    outputmap <-  mapview::mapview(data_sf, map.types = map_types)
    
    if(show_all_coordinates) {
      if(!is.null(unlist(coordinates_subplots_plot_sf)))
        outputmap <- outputmap +
          mapview::mapview(coordinates_subplots_plot_sf, map.types = map_types)
    }

    print(outputmap)

  }

  if (extract_individuals) {
    
    res <- process_individuals(
      plots_data = res,
      con = mydb, 
      con_taxa = mydb.taxa,
      id_individual = id_individual,
      id_tax = id_tax,
      tag = tag,
      include_liana = include_liana
    )
    
    res <- enrich_with_traits(individuals = res, 
                              con = mydb, 
                              extract_individual_features = extract_individual_features, 
                              extract_traits = extract_traits, 
                              traits_to_genera =  traits_to_genera, 
                              wd_fam_level = wd_fam_level, 
                              show_multiple_census = show_multiple_census, 
                              remove_obs_with_issue = remove_obs_with_issue,
                              include_issue = include_issue,
                              include_measurement_ids = include_measurement_ids)
    
    res <- process_stems(res, concatenate_stem)

  }
  
  if (remove_ids & extract_individuals) {

    cli::cli_alert_warning("ids removed - remove_ids = {remove_ids} ")

    res <-
      res %>%
      dplyr::rename(idDB = id_n) %>%
      dplyr::select(-tidyselect::starts_with("id_")) %>%
      dplyr::rename(id_n = idDB)

  }

  if (remove_ids & !extract_individuals) {

    cli::cli_alert_warning("Identifiers are removed because the parameter 'remove_ids' = {remove_ids} ")

    res <-
      res %>%
      dplyr::rename(idDB = id_liste_plots) %>%
      dplyr::select(-tidyselect::starts_with("id_")) %>%
      dplyr::rename(id_liste_plots = idDB)

  }

  res_list <-
    list(
      extract = NA,
      census_features = NA,
      coordinates = NA,
      coordinates_sf = NA
    )

  res_list$extract <- res

  if (nrow(res) < 100)
    print_table(res_print = res)

  if(show_multiple_census) {
    res_list$census_features <- census_features

    print_table(census_features)
  }

  if (show_all_coordinates)
    res_list$coordinates <- coordinates_subplots

  if (show_all_coordinates)
    res_list$coordinates_sf <- coordinates_subplots_plot_sf

  res_list <- res_list[!is.na(res_list)]

  if (length(res_list) == 1)
    res_list <- res_list[[1]]

  # Apply output style
  if (output_style == "auto") {
    detected_style <- .detect_style_from_method(res)
    cli::cli_alert_info("Auto-detected output style: '{detected_style}' based on method field")
    output_style <- detected_style
  }

  # Apply style restructuring (unless "full")
  if (output_style != "full") {
    res_list <- .apply_output_style(
      data = res_list,
      style = output_style,
      extract_individuals = extract_individuals,
      show_multiple_census = show_multiple_census
    )

    # Inform user about restructuring
    if (inherits(res_list, "plot_query_list")) {
      cli::cli_alert_success(
        "Output restructured using '{output_style}' style. Use names() to see available tables."
      )
    }
  }

  return(res_list)

}


#' Process individuals for query_plots
#' 
#' @description
#' Extract and process individuals with filters and plot metadata
#' 
#' @param plots_data Data frame of plots
#' @param con Database connection
#' @param id_individual Vector of individual IDs (optional)
#' @param id_tax Vector of taxonomic IDs (optional)
#' @param tag Vector of tags (optional)
#' @param include_liana Include lianas (logical)
#' @param con_taxa Database connection
#' 
#' @return Data frame of processed individuals
#' @export
process_individuals <- function(plots_data, 
                                con, 
                                con_taxa,
                                id_individual = NULL, 
                                id_tax = NULL,
                                tag = NULL, 
                                include_liana = FALSE) {
  
  cli::cli_rule(left = "Processing individuals")
  
  # Extraction des métadonnées de plots
  plot_metadata <- plots_data %>%
    select(
      plot_name, locality_name, id_liste_plots,
      contains("date_census"), contains("team_leader"),
      contains("principal_investigator"), ddlat, ddlon
    )
  
  # Extraction via merge_individuals_taxa_v2
  cli::cli_alert_info("Fetching individuals")
  
  individuals <- merge_individuals_taxa(
    id_individual = id_individual,
    id_plot = plot_metadata$id_liste_plots,
    id_tax = id_tax,
    clean_columns = TRUE,
    con_taxa = con_taxa, 
    con = con
  )
  
  # Filtrage par tag
  if (!is.null(tag)) {
    cli::cli_alert_info("Filtering by tag: {paste(tag, collapse = ', ')}")
    individuals <- individuals %>%
      dplyr::filter(tag %in% tag)
  }
  
  # Exclusion des lianes
  if (!include_liana) {
    individuals <- individuals %>%
      dplyr::filter(liana == FALSE) %>%
      dplyr::select(-any_of("liana"))
  }
  
  # Enrichissement avec métadonnées de plots
  individuals <- individuals %>%
    dplyr::left_join(plot_metadata, by = c("id_table_liste_plots_n" = "id_liste_plots"))
  
  # Réorganisation des colonnes
  individuals <- reorganize_individual_columns(individuals)
  
  cli::cli_alert_success("Processed {nrow(individuals)} individuals")
  
  return(individuals)
}



#' Enrich individuals with all traits
#' 
#' @description
#' Enrich with individual-level traits, taxonomic traits, and aggregate to genus level if needed
#' 
#' @param individuals Data frame of individuals
#' @param con Database connection
#' @param extract_individual_features Extract individual-level traits
#' @param extract_traits Extract taxonomic traits
#' @param traits_to_genera Aggregate traits to genus level
#' @param wd_fam_level Use family-level wood density
#' @param show_multiple_census Show multiple census data
#' @param remove_obs_with_issue Remove observations with issues
#' 
#' @return Data frame enriched with traits
#' @export
enrich_with_traits <- function(individuals, con,
                               extract_individual_features = TRUE,
                               extract_traits = TRUE,
                               traits_to_genera = FALSE,
                               wd_fam_level = FALSE,
                               show_multiple_census = FALSE,
                               remove_obs_with_issue = TRUE,
                               include_issue = FALSE,
                               include_measurement_ids = FALSE) {
  
  mydb <- call.mydb()
  
  cli::cli_rule(left = "Processing traits")
  
  # Traits individuels
  if (extract_individual_features) {
    individuals <- enrich_individual_traits(
      individuals = individuals, 
      con = con, 
      show_multiple_census = show_multiple_census, 
      remove_obs_with_issue = remove_obs_with_issue, 
      include_issue = include_issue, 
      include_measurement_ids = include_measurement_ids
    )
  }
  
  # Traits taxonomiques
  if (extract_traits) {
    individuals <- enrich_taxonomic_traits(individuals, con)
  }
  
  # Agrégation au niveau genre
  if (traits_to_genera) {
    individuals <- aggregate_traits_to_genus(individuals, wd_fam_level)
  }
  
  return(individuals)
}

#' Enrich with individual-level traits
#' @keywords internal
enrich_individual_traits <- function(individuals, con, show_multiple_census, remove_obs_with_issue, 
                                     include_issue = FALSE, 
                                     include_measurement_ids = FALSE) {
  
  cli::cli_alert_info("Enriching with individual-level traits")
  
  all_traits <- traits_list()
  traits_aggregated <- get_individual_aggregated_features(
    individual_ids = individuals$id_n,
    trait_ids = all_traits$id_trait,  # Tous les traits
    include_multi_census = show_multiple_census,
    remove_issues = remove_obs_with_issue,
    con = con, 
    include_issue = include_issue, 
    include_measurement_ids = include_measurement_ids
  )
  
  # Vérifier qu'il y a des résultats
  if (nrow(traits_aggregated) > 0 && ncol(traits_aggregated) > 1) {
    individuals <- individuals %>%
      left_join(traits_aggregated, by = c('id_n' = 'id_data_individuals'))
  } else {
    cli::cli_alert_info("No traits found to enrich")
  }
  
  return(individuals)
}

#' Enrich individuals with taxonomic-level traits
#' 
#' Adds trait data at the taxonomic level to individual records.
#' Uses the new query_taxa_traits() architecture.
#'
#' @param individuals Data frame with individual data
#' @param con Database connection
#' @return Data frame with added taxonomic traits
#' @keywords internal
enrich_taxonomic_traits <- function(individuals, con) {
  
  cli::cli_alert_info("Enriching with taxonomic-level traits")
  
  unique_taxa <- unique(individuals$idtax_individual_f)
  
  if (length(unique_taxa) == 0 || all(is.na(unique_taxa))) {
    cli::cli_alert_info("No valid taxa IDs found - skipping taxonomic traits")
    return(individuals)
  }
  
  # Remove NAs
  unique_taxa <- unique_taxa[!is.na(unique_taxa)]
  
  # Query traits using new function
  queried_traits_tax <- query_taxa_traits(
    idtax = unique_taxa,
    include_synonyms = FALSE,
    format = "wide",
    categorical_mode = "mode"
  )
  
  # Check if traits were found
  if (is.null(queried_traits_tax$traits_raw) || 
      nrow(queried_traits_tax$traits_raw) == 0) {
    cli::cli_alert_info("No taxonomic traits found for extracted taxa")
    return(individuals)
  }
  
  # Join numeric traits if available
  if (is.data.frame(queried_traits_tax$traits_numeric)) {
    
    # Remove basisofrecord columns
    traits_num_clean <- queried_traits_tax$traits_numeric %>%
      select(-starts_with("basisofrecord_"))
    
    individuals <- individuals %>%
      left_join(
        traits_num_clean,
        by = c("idtax_individual_f" = "idtax")
      )
    
    cli::cli_alert_success(
      "Added {ncol(traits_num_clean) - 1} numeric taxonomic trait column(s)"
    )
  }
  
  # Join categorical traits if available
  if (is.data.frame(queried_traits_tax$traits_categorical)) {
    
    # Remove basisofrecord columns
    traits_cat_clean <- queried_traits_tax$traits_categorical %>%
      select(-starts_with("basisofrecord_"))
    
    individuals <- individuals %>%
      left_join(
        traits_cat_clean,
        by = c("idtax_individual_f" = "idtax")
      )
    
    cli::cli_alert_success(
      "Added {ncol(traits_cat_clean) - 1} categorical taxonomic trait column(s)"
    )
  }
  
  return(individuals)
}

#' Aggregate traits to genus level
#' @keywords internal
aggregate_traits_to_genus <- function(individuals, wd_fam_level) {
  
  cli::cli_alert_info("Aggregating traits to genus level")
  cli::cli_alert_info("Source information added to columns starting with 'source_'")
  
  res_traits_to_genera <- .traits_to_genera_aggreg(
    dataset = individuals,
    wd_fam_level = wd_fam_level
  )
  
  # Traitement des traits catégoriels
  if (length(res_traits_to_genera$dataset_pivot_wider_char) > 1) {
    col_names_char <- res_traits_to_genera$dataset_pivot_wider_char %>%
      select(-id_n, -tax_gen) %>%
      names()
    
    col_names_dataset <- names(individuals)
    
    individuals <- individuals %>%
      select(-all_of(col_names_dataset[which(col_names_dataset %in% col_names_char)])) %>%
      left_join(
        res_traits_to_genera$dataset_pivot_wider_char %>% select(-tax_gen),
        by = "id_n"
      )
  }
  
  # Traitement des traits numériques
  if (length(res_traits_to_genera$dataset_pivot_wider_num) > 1) {
    col_names_num <- res_traits_to_genera$dataset_pivot_wider_num %>%
      select(-id_n, -tax_gen) %>%
      names()
    
    col_names_dataset <- names(individuals)
    
    individuals <- individuals %>%
      select(-all_of(col_names_dataset[which(col_names_dataset %in% col_names_num)])) %>%
      left_join(
        res_traits_to_genera$dataset_pivot_wider_num %>% select(-tax_gen),
        by = "id_n"
      )
  }
  
  return(individuals)
}

#' Process multiple stems
#' 
#' @description
#' Concatenate multiple stems if requested
#' 
#' @param individuals Data frame of individuals
#' @param concatenate If TRUE, concatenate multiple stems
#' 
#' @return Data frame with processed stems
#' @export
process_stems <- function(individuals, concatenate = FALSE) {
  
  if (!concatenate) {
    # Ajouter la colonne number_of_stem à NA
    individuals <- individuals %>%
      mutate(number_of_stem = NA_integer_)
    return(individuals)
  }
  
  cli::cli_alert_info("Concatenating multiple stems - column 'number_of_stem' shows stem count")
  
  row_multiple_stems <- individuals %>%
    filter(!is.na(stem_grouping)) %>%
    nrow()
  
  if (row_multiple_stems > 0) {
    individuals <- individuals %>%
      mutate(id_n = ifelse(!is.na(stem_grouping), stem_grouping, id_n)) %>%
      group_by(id_n) %>%
      summarise(
        across(everything(), ~first(., na_rm = TRUE)),
        number_of_stem = n(),
        .groups = "drop"
      ) %>%
      mutate(number_of_stem = na_if(number_of_stem, 1))
  } else {
    individuals <- individuals %>%
      mutate(number_of_stem = NA_integer_)
  }
  
  return(individuals)
}

#' Reorganize columns for individuals data
#' 
#' @param individuals Data frame of individuals
#' @return Data frame with reorganized columns
#' @keywords internal
reorganize_individual_columns <- function(individuals) {
  
  individuals <- individuals %>%
    dplyr::arrange(id_n) %>%
    dplyr::relocate(tax_infra_level, .before = 3) %>%
    dplyr::relocate(tax_gen, .before = 3) %>%
    dplyr::relocate(tax_fam, .before = 3) %>%
    dplyr::relocate(colnam_specimen, .before = 3) %>%
    dplyr::relocate(colnbr, .before = 3) %>%
    dplyr::relocate(suffix, .before = 3) %>%
    dplyr::relocate(locality_name, .before = 3) %>%
    dplyr::relocate(tag, .before = 3) %>%
    dplyr::relocate(tax_sp_level, .before = 3) %>%
    dplyr::relocate(plot_name, .before = 3)

  # Colonnes conditionnelles
  if ("stem_diameter" %in% names(individuals)) {
    individuals <- individuals %>%
      dplyr::relocate(stem_diameter, .before = tag)
  }

  if ("tree_height" %in% names(individuals)) {
    individuals <- individuals %>%
      dplyr::relocate(tree_height, .before = tag)
  }
  
  return(individuals)
}

.process_coordinates <- function(all_coordinates, res) {
  
  all_plots_coord <- unique(all_coordinates$id_table_liste_plots)
  coordinates_sf_list <- vector('list', length(all_plots_coord))
  coordinates_data_list <- vector('list', length(all_plots_coord))
  
  for (j in seq_along(all_plots_coord)) {
    id_plot <- all_plots_coord[j]
    grouped <- all_coordinates %>%
      filter(id_table_liste_plots == id_plot) %>%
      group_by(coord1, coord2, coord3, id_table_liste_plots) %>%
      summarise(
        typevalue = mean(typevalue),
        id_sub_plots = stringr::str_c(id_sub_plots, collapse = ", "),
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = coord3, values_from = c(typevalue, id_sub_plots)) %>%
      mutate(
        coord1 = as.numeric(coord1),
        coord2 = as.numeric(coord2),
        Xrel = coord1 - min(coord1),
        Yrel = coord2 - min(coord2)
      )
    
    if (nrow(grouped) == 0) next
    
    cor_coord <- suppressMessages(suppressWarnings(BIOMASS::correctCoordGPS(
      longlat = grouped[, c("typevalue_ddlon", "typevalue_ddlat")],
      rangeX = c(0, diff(range(grouped$coord1))),
      rangeY = c(0, diff(range(grouped$coord2))),
      coordRel = grouped %>% select(Xrel, Yrel),
      drawPlot = FALSE, rmOutliers = TRUE
    )))
    
    poly_plot <- st_as_sf(cor_coord$polygon) %>%
      st_set_crs(cor_coord$codeUTM) %>%
      st_transform(4326) %>%
      mutate(id_liste_plots = id_plot)
    
    coordinates_sf_list[[j]] <- poly_plot
    coordinates_data_list[[j]] <- grouped
  }
  
  list(
    coordinates_sf = do.call(bind_rows, coordinates_sf_list),
    coordinates_raw = bind_rows(coordinates_data_list)
  )
}

.link_metadata_tables <- function(res, con) {
  # Liste des tables de métadonnées connues à joindre
  metadata_tables <- list(
    id_country = list(table = "table_countries", keep = c("country")),
    id_method = list(table = "methodslist", keep = c("method"))
  )
  
  # Colonnes présentes dans le tibble
  cols_in_res <- colnames(res)
  
  # Pour chaque clé étrangère détectée dans res
  for (id_col in names(metadata_tables)) {
    if (id_col %in% cols_in_res) {
      meta_info <- metadata_tables[[id_col]]
      table_name <- meta_info$table
      keep_cols <- meta_info$keep
      
      # Récupère la table de la base
      meta_tbl <- tryCatch({
        dplyr::tbl(con, table_name) %>% dplyr::collect()
      }, error = function(e) {
        warning(glue::glue("Impossible de collecter {table_name} : {e$message}"))
        return(NULL)
      })
      
      # Effectue la jointure si succès
      if (!is.null(meta_tbl)) {
        # Pour éviter les conflits de nom de colonnes
        keep_cols_clean <- setdiff(keep_cols, names(res))
        if (length(keep_cols_clean) < length(keep_cols))
          # res <- rm_field(res, field = keep_cols)
          res <-
            res %>%
            dplyr::select(-any_of(keep_cols))
        
        
        meta_tbl <- dplyr::select(meta_tbl, dplyr::all_of(c(id_col, keep_cols)))
        res <- dplyr::left_join(res, meta_tbl, by = id_col)
      }
    }
  }
  
  return(res)
}


if (!requireNamespace("R6", quietly = TRUE)) {
  stop("Package R6 requested. Install with install.packages('R6')")
}


#' Query builder for plot
#' 
#' @description
#' Allow building progressively a SQL query to filter plots
#' following different criteria using the builder pattern
#' 
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' query <- PlotFilterBuilder$new(con)$
#'   filter_country("Gabon")$
#'   filter_method("transect")$
#'   build()
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{new(connection)}}{
#'     Initialize the builder with a database connection.
#'     \itemize{
#'       \item \code{connection}: A DBI connection object to the database
#'     }
#'   }
#'   \item{\code{filter_country(country, interactive = FALSE)}}{
#'     Filter plots by country name(s).
#'     \itemize{
#'       \item \code{country}: Character vector of country name(s)
#'       \item \code{interactive}: Logical. If TRUE, uses .link_table for fuzzy matching
#'     }
#'   }
#'   \item{\code{filter_plot_name(plot_name, interactive = FALSE)}}{
#'     Filter plots by plot name(s).
#'     \itemize{
#'       \item \code{plot_name}: Character vector of plot name(s)
#'       \item \code{interactive}: Logical. If TRUE, uses .link_table for fuzzy matching
#'     }
#'   }
#'   \item{\code{filter_method(method, interactive = FALSE)}}{
#'     Filter plots by method(s).
#'     \itemize{
#'       \item \code{method}: Character vector of method name(s)
#'       \item \code{interactive}: Logical. If TRUE, uses .link_table for fuzzy matching
#'     }
#'   }
#'   \item{\code{filter_locality(locality_name)}}{
#'     Filter plots by locality name(s).
#'     \itemize{
#'       \item \code{locality_name}: Character vector of locality name(s)
#'     }
#'   }
#'   \item{\code{build(operator = "AND")}}{
#'     Build the final SQL query.
#'     \itemize{
#'       \item \code{operator}: Character. Join operator between conditions ("AND" or "OR"). Default is "AND"
#'     }
#'     Returns a SQL query object
#'   }
#'   \item{\code{build_with_or()}}{
#'     Build the SQL query with OR operator between conditions.
#'     Returns a SQL query object
#'   }
#'   \item{\code{add_custom_condition(condition, wrap_parentheses = TRUE)}}{
#'     Add a custom SQL condition.
#'     \itemize{
#'       \item \code{condition}: Character. Raw SQL condition string
#'       \item \code{wrap_parentheses}: Logical. If TRUE, wraps condition in parentheses
#'     }
#'   }
#'   \item{\code{print_conditions()}}{
#'     Display current filter conditions (for debugging).
#'   }
#' }
#' 
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
    # Initialize the builder
    initialize = function(connection) {
      stopifnot("Connection must be provided" = !is.null(connection))
      private$con <- connection
    },
    
    # Filter by country
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
    
    # Filter by plot name
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
    
    # Filter by method
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
    
    # Filter by locality
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
    
    # Build the final SQL query
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
    
    # Build with OR operator explicitly
    build_with_or = function() {
      return(self$build(operator = "OR"))
    },
    
    # Add a custom SQL condition
    add_custom_condition = function(condition, wrap_parentheses = TRUE) {
      if (!is.null(condition) && nchar(condition) > 0) {
        if (wrap_parentheses) {
          condition <- paste0("(", condition, ")")
        }
        private$add_condition(condition)
      }
      invisible(self)
    },
    
    # Display current filter conditions (for debugging)
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

#' Fetch plot data
#' 
#' @description
#' Class for extracting metadata from database
#' 
#' @export
PlotFetcher <- R6::R6Class(
  "PlotFetcher",
  
  private = list(
    con = NULL,
    
    # Enrichissement avec tables de métadonnées liées
    enrich_metadata = function(plots) {
      metadata_tables <- list(
        id_country = list(table = "table_countries", keep = c("country")),
        id_method = list(table = "methodslist", keep = c("method"))
      )
      
      for (id_col in names(metadata_tables)) {
        if (id_col %in% colnames(plots)) {
          meta_info <- metadata_tables[[id_col]]
          
          meta_tbl <- tryCatch({
            try_open_postgres_table(meta_info$table, private$con) %>%
              dplyr::select(dplyr::all_of(c(id_col, meta_info$keep))) %>%
              dplyr::collect()
          }, error = function(e) {
            cli::cli_alert_warning("Could not fetch {meta_info$table}: {e$message}")
            return(NULL)
          })
          
          if (!is.null(meta_tbl)) {
            # Éviter les doublons de colonnes
            keep_cols <- setdiff(meta_info$keep, names(plots))
            if (length(keep_cols) > 0) {
              meta_tbl <- dplyr::select(meta_tbl, dplyr::all_of(c(id_col, keep_cols)))
              plots <- dplyr::left_join(plots, meta_tbl, by = id_col)
            }
          }
        }
      }
      
      return(plots)
    }
  ),
  
  public = list(
    #' @description Initialiser le fetcher
    #' @param connection Connexion DBI
    initialize = function(connection) {
      private$con <- connection
    },
    
    #' @description Récupérer plots par IDs
    #' @param plot_ids Vecteur d'IDs de plots
    fetch_by_ids = function(plot_ids) {
      if (is.null(plot_ids) || length(plot_ids) == 0) {
        return(tibble::tibble())
      }
      
      cli::cli_alert_info("Fetching {length(plot_ids)} plots by ID")
      
      sql <- glue::glue_sql(
        "SELECT * FROM data_liste_plots WHERE id_liste_plots IN ({ids*})",
        ids = plot_ids,
        .con = private$con
      )
      
      plots <- func_try_fetch(con = private$con, sql = sql) %>%
        dplyr::select(-any_of("id_old"))
      
      # Enrichissement
      plots <- private$enrich_metadata(plots)
      
      return(plots)
    },
    
    #' @description Récupérer plots avec filtre (requête SQL)
    #' @param query Requête SQL construite par PlotFilterBuilder
    fetch_with_filter = function(query) {
      plots <- func_try_fetch(con = private$con, sql = query) %>%
        dplyr::select(-any_of("id_old"))
      
      if (nrow(plots) == 0) {
        cli::cli_alert_warning("No plots found matching filter criteria")
        return(plots)
      }
      
      cli::cli_alert_success("Found {nrow(plots)} plots")
      
      # Enrichissement
      plots <- private$enrich_metadata(plots)
      
      return(plots)
    }
  )
)


.extract_by_specimen <- function(id_specimen, con) {
  tbl <- "data_link_specimens"
  sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE id_specimen IN ({vals*})",
                        vals = id_specimen, .con = con)
  res <- func_try_fetch(con = con, sql = sql)
  if (nrow(res) == 0) stop("No individuals linked to this specimen")
  return(res$id_n)
}

.traits_to_genera_aggreg <- function(dataset, wd_fam_level_add = wd_fam_level) {
  
  list_genus <- dataset %>%
    # dplyr::filter(is.na(tax_sp_level)) %>%
    dplyr::select(id_n, tax_gen)
  
  all_sp_genera <- query_taxa(
    genus = list_genus %>%
      dplyr::filter(!is.na(tax_gen)) %>%
      dplyr::distinct(tax_gen) %>%
      dplyr::pull(tax_gen),
    class = NULL,
    extract_traits = FALSE,
    verbose = FALSE,
    exact_match = TRUE
  )
  
  all_sp_genera <-
    all_sp_genera %>%
    filter(tax_gen %in% unique(list_genus$tax_gen),
           !is.na(tax_infra_level))
  
  all_val_sp <- query_taxa_traits(idtax = all_sp_genera %>%
                                        filter(!is.na(tax_esp)) %>%
                                        pull(idtax_n), format = "long",
                                  include_synonyms = T, 
                                      add_taxa_info = T)
  
  
  if (any(class(all_val_sp$traits_categorical) == "data.frame")) {
    
    traits_idtax_char <- 
      pivot_categorical_traits_generic(
      data = all_val_sp$traits_raw %>%
        dplyr::filter(valuetype == "categorical") %>% 
        filter(!is.na(tax_gen)),
      id_col = "tax_gen",
      include_id_measures = TRUE,
      name_prefix = "taxa_"
    )
    
    # traits_idtax_char <-
    #   all_val_sp$traits_found %>%
    #   dplyr::filter(valuetype == "categorical") %>%
    #   dplyr::select(idtax,
    #                 trait,
    #                 traitvalue_char,
    #                 basisofrecord,
    #                 id_trait_measures) %>%
    #   dplyr::mutate(rn = data.table::rowid(trait)) %>%
    #   tidyr::pivot_wider(
    #     names_from = trait,
    #     values_from = c(traitvalue_char, basisofrecord, id_trait_measures), 
    #     names_prefix = "taxa_level_"
    #   ) %>%
    #   dplyr::select(-rn) %>%
    #   left_join(all_val_sp$traits_idtax_char %>%
    #               dplyr::select(idtax, tax_gen),
    #             by = c("idtax" = "idtax"))
    
    # names(traits_idtax_char) <- gsub("traitvalue_char_", "", names(traits_idtax_char))
    
    # traits_idtax_concat <-
    #   traits_idtax_char %>%
    #   dplyr::select(tax_gen, starts_with("id_trait_")) %>%
    #   dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
    #   dplyr::group_by(tax_gen) %>%
    #   dplyr::mutate(dplyr::across(where(is.character),
    #                               ~ stringr::str_c(.[!is.na(.)],
    #                                                collapse = ", "))) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::distinct()
    # 
    # cli::cli_alert_info("Extracting most frequent value for categorical traits at genus level")
    # 
    # traits_idtax_char <-
    #   traits_idtax_char %>%
    #   dplyr::select(-starts_with("id_trait_")) %>%
    #   group_by(tax_gen, across(where(is.character))) %>%
    #   count() %>%
    #   arrange(tax_gen, desc(n)) %>%
    #   ungroup() %>%
    #   group_by(tax_gen) %>%
    #   dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))
    # 
    # traits_idtax_char <-
    #   left_join(traits_idtax_char,
    #             traits_idtax_concat, by = c("tax_gen" = "tax_gen"))
    
    colnames_traits <- names(traits_idtax_char %>%
                               dplyr::select(
                                 -tax_gen,
                                 -starts_with("id_trait_"),
                                 -starts_with("basisofrecord_")
                               ))
    
    colnames_data <- names(dataset)
    
    dataset_subset <- 
      dataset %>% 
      select(id_n, 
             tax_gen,
             all_of(colnames_traits[which(colnames_traits %in% colnames_data)]))
    
    dataset_pivot <- 
      dataset_subset %>% 
      pivot_longer(cols = colnames_traits[which(colnames_traits %in% colnames_data)],
                   names_to = "trait") %>% 
      arrange(tax_gen, trait)
    
    dataset_traits_pivot <- 
      traits_idtax_char %>% 
      select(tax_gen,
             all_of(colnames_traits)) %>% 
      pivot_longer(cols = colnames_traits[which(colnames_traits %in% colnames_data)],
                   names_to = "trait") %>% 
      arrange(tax_gen, trait)
    
    dataset_genus_level <- 
      dataset_pivot %>% 
      filter(is.na(value)) %>% 
      select(-value) %>% 
      left_join(dataset_traits_pivot,
                by = c("tax_gen" = "tax_gen",
                       "trait" = "trait"))
    
    dataset_sp_level <- 
      dataset_pivot %>% 
      filter(!is.na(value)) %>% 
      select(-value) %>% 
      left_join(dataset_traits_pivot,
                by = c("tax_gen" = "tax_gen",
                       "trait" = "trait")) %>% 
      mutate(source = "species")
    
    
    dataset_genus_level_filled <- 
      dataset_genus_level  %>% 
      filter(!is.na(value)) %>% 
      mutate(source = "genus")
    
    
    dataset_genus_level_unfilled <- 
      dataset_genus_level  %>% 
      filter(is.na(value)) %>% 
      mutate(source = NA_character_)
    
    
    dataset_pivot_wider_char <- 
      bind_rows(dataset_sp_level, dataset_genus_level_filled, dataset_genus_level_unfilled) %>% 
      pivot_wider(names_from = trait, 
                  values_from = c(value, source))
    
    names(dataset_pivot_wider_char) <- 
      gsub("value_", "", names(dataset_pivot_wider_char))
    
    
  } else {
    dataset_pivot_wider_char <- NA
  }
  
  if (any(class(all_val_sp$traits_numeric) == "data.frame")) {
    
    traits_idtax_num <- 
      pivot_numeric_traits_generic(
      data = all_val_sp$traits_numeric %>%
        dplyr::filter(valuetype == "numeric") %>% 
        filter(!is.na(tax_gen)),
      id_col = "tax_gen",
      include_stats = TRUE,
      include_id_measures = TRUE,
      name_prefix = "taxa_"
    )
    
    colnames_data <- names(dataset)
    
    # traits_idtax_num <-
    #   all_val_sp$traits_found %>%
    #   dplyr::filter(valuetype == "numeric") %>%
    #   dplyr::select(idtax,
    #                 trait,
    #                 traitvalue,
    #                 basisofrecord,
    #                 id_trait_measures) %>%
    #   dplyr::mutate(rn = data.table::rowid(trait)) %>%
    #   tidyr::pivot_wider(
    #     names_from = trait,
    #     values_from = c(traitvalue, basisofrecord, id_trait_measures), 
    #     names_prefix = "taxa_level_"
    #   ) %>%
    #   dplyr::select(-rn) %>%
    #   dplyr::left_join(all_val_sp$traits_idtax_num %>%
    #                      dplyr::select(idtax, tax_gen),
    #                    by = c("idtax" = "idtax"))
    
    # names(traits_idtax_num) <- gsub("traitvalue_", "", names(traits_idtax_num))
    
    # traits_idtax_concat <-
    #   traits_idtax_num %>%
    #   dplyr::select(tax_gen, starts_with("id_trait_")) %>%
    #   dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
    #   dplyr::group_by(tax_gen) %>%
    #   dplyr::mutate(dplyr::across(where(is.character),
    #                               ~ stringr::str_c(.[!is.na(.)],
    #                                                collapse = ", "))) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::distinct()
    # 
    # traits_idtax_num <-
    #   traits_idtax_num %>%
    #   dplyr::select(-starts_with("id_trait_")) %>%
    #   dplyr::group_by(tax_gen) %>%
    #   dplyr::summarise(dplyr::across(where(is.numeric),
    #                                  .fns= list(mean = ~mean(., na.rm= TRUE),
    #                                             sd = ~sd(., na.rm= TRUE),
    #                                             n = ~sum(!is.na(.))),
    #                                  .names = "{.col}_{.fn}"))
    
    
    colnames_traits <- names(traits_idtax_num %>%
                               dplyr::select(
                                 -tax_gen,
                                 -starts_with("id_trait_"),
                                 -starts_with("basisofrecord_")
                               ))
    
    dataset_subset <- 
      dataset %>% 
      select(id_n, 
             tax_gen,
             tax_fam,
             plot_name,
             all_of(colnames_traits[which(colnames_traits %in% colnames_data)]))
    
    dataset_pivot <- 
      dataset_subset %>% 
      pivot_longer(cols = starts_with("taxa_"),
                   names_to = "trait") %>% 
      arrange(tax_fam, tax_gen, trait)
    
    dataset_traits_pivot <- 
      traits_idtax_num %>% 
      select(tax_gen,
             all_of(colnames_traits)) %>% 
      pivot_longer(cols = starts_with("taxa_"),
                   names_to = "trait") %>% 
      arrange(tax_gen, trait) %>% 
      filter(!is.na(value))
    
    ## traits with no values at species level
    if (any(!colnames_traits %in% colnames_data)) {
      
      no_val_genus_level <- 
        expand_grid(id_n = unique(dataset_pivot$id_n),
                    trait = colnames_traits[!colnames_traits %in% colnames_data]) %>% 
        left_join(dataset %>% select(id_n, tax_gen, tax_fam)) %>% 
        left_join(dataset_traits_pivot,
                  by = c("tax_gen" = "tax_gen",
                         "trait" = "trait"))
    } else {
      no_val_genus_level <- NULL
    }
    
    dataset_genus_level <- 
      dataset_pivot %>% 
      filter(is.na(value)) %>% 
      select(-value) %>% 
      left_join(dataset_traits_pivot,
                by = c("tax_gen" = "tax_gen",
                       "trait" = "trait"))
    
    dataset_genus_level <- bind_rows(dataset_genus_level, no_val_genus_level)
    
    dataset_sp_level <- 
      dataset_pivot %>% 
      filter(!is.na(value)) %>% 
      # select(-value) %>% 
      # left_join(dataset_traits_pivot,
      #           by = c("tax_gen" = "tax_gen",
      #                  "trait" = "trait")) %>% 
      mutate(source = "species")
    
    dataset_genus_level_filled <- 
      dataset_genus_level  %>% 
      filter(!is.na(value)) %>% 
      mutate(source = "genus")
    
    dataset_genus_level_unfilled <- 
      dataset_genus_level  %>% 
      filter(is.na(value)) %>% 
      mutate(source = NA_character_)
    
    
    dataset_pivot_wider_num <- 
      bind_rows(dataset_sp_level, dataset_genus_level_filled, dataset_genus_level_unfilled) %>% 
      pivot_wider(names_from = trait, 
                  values_from = c(value, source))
    
    names(dataset_pivot_wider_num) <- 
      gsub("value_", "", names(dataset_pivot_wider_num))
    
    
    if (any(colnames_traits == "taxa_mean_wood_density")) {
      
      if (any(names(dataset_pivot_wider_num) == "taxa_sd_wood_density")) {
        cli::cli_alert_info("Setting wood density SD to averaged species and genus level according to BIOMASS dataset")
        
        sd_10 <- BIOMASS::sd_10
        
        
        ### replacing wd sd to species and genus level sd from biomass
        dataset_pivot_wider_num <-
          dataset_pivot_wider_num %>%
          mutate(taxa_sd_wood_density = replace(taxa_sd_wood_density,
                                                source_taxa_mean_wood_density == "species",
                                                      sd_10$sd[1])) %>%
          mutate(taxa_sd_wood_density = replace(taxa_sd_wood_density,
                                                      source_taxa_mean_wood_density == "genus",
                                                      sd_10$sd[2]))
        
        if (wd_fam_level_add) {
          
          dataset_pivot_wider_num <-
            dataset_pivot_wider_num %>%
            mutate(taxa_sd_wood_density = replace(
              taxa_sd_wood_density,
              is.na(tax_gen) & !is.na(tax_fam),
              sd_10$sd[3]
            ))
          
        }
      }
      
    }
    
    ### averaged wd for plots
    wd_plot_level <- 
      dataset_pivot_wider_num %>%
      dplyr::group_by(plot_name) %>%
      dplyr::summarise(taxa_mean_wood_density_plot_level = mean(taxa_mean_wood_density, na.rm = T),
                       taxa_sd_wood_density_plot_level = mean(taxa_sd_wood_density, na.rm = T))
    
    dataset_pivot_wider_num <- 
      dataset_pivot_wider_num %>%
      dplyr::left_join(wd_plot_level,
                       by = c("plot_name" = "plot_name")) %>%
      dplyr::mutate(taxa_mean_wood_density = 
                      ifelse(is.na(taxa_mean_wood_density),
                             taxa_mean_wood_density_plot_level,
                             taxa_mean_wood_density),
                    taxa_sd_wood_density = 
                      ifelse(is.na(taxa_sd_wood_density),
                             taxa_sd_wood_density_plot_level,
                             taxa_sd_wood_density),
                    source_taxa_sd_wood_density = 
                      ifelse(is.na(source_taxa_sd_wood_density),
                             "plot_mean",
                             source_taxa_sd_wood_density),
                    source_taxa_mean_wood_density = 
                      ifelse(is.na(source_taxa_mean_wood_density),
                             "plot_mean",
                             source_taxa_mean_wood_density)) %>%
      dplyr::select(-taxa_mean_wood_density_plot_level, taxa_sd_wood_density)
    
    
  } else {
    dataset_pivot_wider_num <- NA
  }
  
  return(list(dataset_pivot_wider_char = dataset_pivot_wider_char,
              dataset_pivot_wider_num = dataset_pivot_wider_num))
  
}





#' Explore allometric relation
#'
#' Provide allometric data and graph dbh-height of selected taxa
#'
#' @return A tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param genus_searched string
#' @param tax_esp_searched string
#' @param tax_fam_searched string
#' @param id_search integer
#'
#' @return A tibble of taxa or individuals if extract_individuals is TRUE
#' @export
explore_allometric_taxa <- function(genus_searched = NULL,
                                    tax_esp_searched = NULL,
                                    tax_fam_searched = NULL,
                                    id_search = NULL) {

  mydb <- call.mydb()
  mydb_taxa <- call.mydb.taxa()

  res_taxa <- query_taxa(
    genus = genus_searched,
    species = tax_esp_searched,
    ids =  id_search, verbose = F)

  tax_data <-
    query_plots(id_tax = res_taxa$idtax_n)

  if(nrow(tax_data)>0) {

    data_allo1 <-
      tax_data %>%
      # dplyr::select(tree_height, stem_diameter) %>%
      dplyr::filter(!is.na(tree_height), tree_height>0, stem_diameter>0) %>%
      dplyr::collect()

    cat(paste0("\n The number of individuals with both tree height and stem_diameter values is ", nrow(data_allo1)))

    if(nrow(data_allo1)>1) {
      gg_plot1 <-
        ggplot2::ggplot() +
        ggplot2::geom_point(data = data_allo1,
                            mapping = ggplot2::aes(x = stem_diameter, y = tree_height)) +
        ggplot2::xlab("Stem diameter (cm)") +
        ggplot2::ylab("Tree height (m)")

    }else{
      gg_plot1 <- NA
    }

    data_allo2 <-
      tax_data %>%
      # dplyr::select(tree_height, dbh, crown_spread, id_n, plot_name, country, full_name_no_auth) %>%
      dplyr::filter(!is.na(crown_width), crown_width>0, stem_diameter>0) %>%
      dplyr::collect()

    cat(paste0("\n The number of individuals with both crown_width and stem_diameter values is ", nrow(data_allo2)))
    cat("\n")

    if(nrow(data_allo2)>1) {
      gg_plot2 <-
        ggplot2::ggplot() +
        ggplot2::geom_point(data = data_allo2, mapping =
                              ggplot2::aes(x = stem_diameter, y = crown_width)) +
        ggplot2::xlab("Stem diameter (cm)") +
        ggplot2::ylab("Crown width (m)")
    }else{
      gg_plot2 <- NA
    }
  }else{
    cli::cli_alert_danger("No taxa found. Select at least one taxa")
    # cat(paste0("\n You currently selected ", nrow(tax_data), "taxa"))
    print(tax_data)
  }

  if (nrow(tax_data) > 0)
    return(
      list(
        data_height_dbh = data_allo1,
        data_crow_dbh = data_allo2,
        taxa_data = tax_data,
        plot_height_dbh = gg_plot1,
        plot_crown_dbh = gg_plot2
      )
    )
}


#' Add new plot metadata
#'
#' Add metadata for new plots
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string a vector of string indicating columns names of new_data
#' @param col_names_corresp string a vector of string indicating to which columns selected columns of new_data corresponds
#'
#' @importFrom methods new
#' @importFrom stats dist sd
#' @importFrom kableExtra cell_spec kable_styling
#'
#' @return No return value, new plots are added
#' @export
add_plots <- function(new_data,
                      col_names_select,
                      col_names_corresp) {

  mydb <- call.mydb()

  new_data_renamed <-
    new_data

  for (i in 1:length(col_names_select)) {
    if(any(colnames(new_data_renamed)==col_names_select[i])){
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    }else{
      stop(paste("Column name provided not found in provided new dataset", col_names_corresp[i]))

    }
  }

  ## Checking dates
  if (any(colnames(new_data_renamed) == "date_y"))
    if (any(new_data_renamed$date_y[!is.na(new_data_renamed$date_y)] > lubridate::year(Sys.Date())) |
        any(new_data_renamed$date_y[!is.na(new_data_renamed$date_y)] < 1900))
      stop("ERREUR dans date_y, year provided impossible")
  if (any(colnames(new_data_renamed) == "date_m"))
    if (any(new_data_renamed$date_m[!is.na(new_data_renamed$date_m)] > 12) |
        any(new_data_renamed$date_m[!is.na(new_data_renamed$date_m)] < 1))
      stop("ERREUR dans date_m, month provided impossible")
  if (any(colnames(new_data_renamed) == "data_d"))
    if (any(new_data_renamed$data_d[!is.na(new_data_renamed$data_d)] > 31) |
        any(new_data_renamed$data_d[!is.na(new_data_renamed$data_d)] < 1))
      stop("ERREUR dans data_d, day provided impossible")
  if (any(colnames(new_data_renamed) == "ddlon"))
    if (any(new_data_renamed$ddlon > 180) |
        any(new_data_renamed$ddlon < -180))
      stop("ERREUR dans ddlon, longitude provided impossible")
  if (any(colnames(new_data_renamed) == "ddlat"))
    if (any(new_data_renamed$ddlat > 90) |
        any(new_data_renamed$ddlon < -90))
      stop("ERREUR dans ddlat, latitude provided impossible")

  ## Checking if names plot are already in the database
  if(any(colnames(new_data_renamed) == "plot_name")) {

    found_plot <-
      try_open_postgres_table(table = "data_liste_plots", con = mydb) %>%
      dplyr::filter(plot_name %in% !!new_data_renamed$plot_name) %>%
      dplyr::collect()

    if (nrow(found_plot) > 0) {
      print(found_plot)
      stop("Some plot_name in new data already in the plot list table. No duplicate allowed.")
    }
  }

  ## Checking method
  if(!any(names(new_data_renamed) == "method")) {

    stop("missing method information")

  } else {

    new_data_renamed <-
      .link_table(
      data_stand = new_data_renamed,
      column_searched = "method",
      column_name = "method",
      id_field = "id_method",
      id_table_name = "id_method",
      db_connection = mydb,
      table_name = "methodslist"
    )

    # new_data_renamed <-
    #   new_data_renamed %>%
    #  dplyr::select(-method)

    col_names_corresp[which(col_names_corresp == "method")] <-
      "id_method"

  }

  ## Checking country
  if(!any(names(new_data_renamed) == "country")) {

    stop("missing country information")

  } else {

    new_data_renamed <-
      .link_table(
      data_stand = new_data_renamed,
      column_searched = "country",
      column_name = "country",
      id_field = "id_country",
      id_table_name = "id_country",
      db_connection = mydb,
      table_name = "table_countries"
    )


    col_names_corresp[which(col_names_corresp == "country")] <-
      "id_country"

  }

  ## Checking team_leader
  if(!any(names(new_data_renamed) == "team_leader")) {

    cli::cli_alert_danger("missing team_leader column")

    chose_pi <- choose_prompt(message = "Choose one team_leader for all plot ?")
    
    if (chose_pi) {

      id_team_leader <- .link_colnam(
        data_stand = tibble(team_leader = " "),
        column_searched = "team_leader",
        column_name = "colnam",
        id_field = "id_team_leader",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )

      id_team_leader <-
        tibble(plot_name = new_data_renamed$plot_name,
               team_leader = id_team_leader$id_team_leader)

    }

  } else {

    cli::cli_alert_info("Identifying team_leader")

    team_leader_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, team_leader) %>%
      tidyr::separate_rows(team_leader, sep = ",") %>%
      mutate(team_leader = stringr::str_squish(team_leader))

    id_team_leader <-
      .link_colnam(
      data_stand = team_leader_sep,
      column_searched = "team_leader",
      column_name = "colnam",
      id_field = "team_leader",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

    # col_names_corresp[which(col_names_corresp == "team_leader")] <-
    #   "id_colnam"

  }

  ## Checking team_leader
  if(!any(names(new_data_renamed) == "PI")) {

    cli::cli_alert_danger("missing PI column")

    chose_pi <- choose_prompt(message = "Choose one PI for all plot ?")
    
    if (chose_pi) {
      # id_pi <- .link_colnam(data_stand = tibble(PI = " "),
      #                       collector_field = "PI", id_colnam = "id_pi")


      id_pi <-
        .link_colnam(
        data_stand = tibble(PI = " "),
        column_searched = "PI",
        column_name = "colnam",
        id_field = "id_pi",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )

      id_pi <-
        tibble(plot_name = new_data_renamed$plot_name,
               PI = id_pi$id_pi)

    }

  } else {

    cli::cli_alert_info("Identifying PI")

    pi_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, PI) %>%
      tidyr::separate_rows(PI, sep = ",") %>%
      mutate(PI = stringr::str_squish(PI))

    # id_pi <-
    #   .link_colnam(data_stand = pi_sep,
    #                collector_field = "PI", id_colnam = "PI")

    id_pi <-
      .link_colnam(
      data_stand = pi_sep,
      column_searched = "PI",
      column_name = "colnam",
      id_field = "PI",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

  }


  ## Checking data manager
  if(!any(names(new_data_renamed) == "data_manager")) {

    cli::cli_alert_danger("missing data_manager column")

    chose_data_manager <- choose_prompt(message = "Choose one data_manager for all plot ?")
    
    if (chose_data_manager) {
      # data_manager <- .link_colnam(data_stand = tibble(data_manager = " "),
      #                       collector_field = "data_manager",
      #                       id_colnam = "id_data_manager")

      data_manager <- .link_colnam(
        data_stand = tibble(data_manager = " "),
        column_searched = "data_manager",
        column_name = "colnam",
        id_field = "id_data_manager",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )

      id_data_manager <-
        tibble(plot_name = new_data_renamed$plot_name,
               data_manager = data_manager$id_data_manager)

    }

  } else {

    cli::cli_alert_info("Identifying data manager")

    data_manager_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, data_manager) %>%
      tidyr::separate_rows(data_manager, sep = ",") %>%
      mutate(data_manager = stringr::str_squish(data_manager))

    # data_manager_sep <-
    #   .link_colnam(data_stand = data_manager_sep,
    #                collector_field = "data_manager", id_colnam = "data_manager")


    data_manager_sep <-
      .link_colnam(
      data_stand = data_manager_sep,
      column_searched = "data_manager",
      column_name = "colnam",
      id_field = "data_manager",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

  }

  if(!any(names(new_data_renamed) == "additional_people")) {

    cli::cli_alert_danger("missing additional_people information")

  } else {

    cli::cli_alert_info("Identifying additional people list for the plot/transect")

    add_col_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, additional_people) %>%
      tidyr::separate_rows(additional_people, sep = ",") %>%
      mutate(additional_people = stringr::str_squish(additional_people))

    # add_col_sep <-
    #   .link_colnam(data_stand = add_col_sep,
    #                collector_field = "additional_people", id_colnam = "additional_people")


    add_col_sep <- .link_colnam(
      data_stand = add_col_sep,
      column_searched = "additional_people",
      column_name = "colnam",
      id_field = "additional_people",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )


  }

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-any_of(c("team_leader", "PI", "additional_people", "data_manager")))

  col_names_corresp <-
    col_names_corresp[which(!col_names_corresp %in% c("team_leader", "PI", "additional_people", "data_manager"))]

  ## Checking coordinates
  if (any(names(new_data_renamed) == "ddlat"))
    if (any(new_data_renamed$ddlat > 90) | any(new_data_renamed$ddlat < -90)) stop("ddlat impossible")

  if (any(names(new_data_renamed) == "ddlon"))
    if (any(new_data_renamed$ddlon > 180) | any(new_data_renamed$ddlon < -180)) stop("ddlon impossible")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  new_data_renamed <-
    new_data_renamed %>%
    mutate(
      data_modif_d = lubridate::day(Sys.Date()),
      data_modif_m = lubridate::month(Sys.Date()),
      data_modif_y = lubridate::year(Sys.Date())
    )

  add <- choose_prompt(message = "Add these data to the table of plot data?")
  
  
  
  if(add) {
    DBI::dbWriteTable(mydb, "data_liste_plots", new_data_renamed, append = TRUE, row.names = FALSE)
    cli::cli_alert_success("{nrow(new_data_renamed)} plot imported in data_liste_plots")

    ids_list_plot <-
      try_open_postgres_table(table = "data_liste_plots", con = mydb) %>%
      filter(plot_name %in% !!new_data_renamed$plot_name) %>%
      collect() %>%
      dplyr::select(id_liste_plots, plot_name)

    if (exists("id_team_leader")) {

      id_team_leader <-
        id_team_leader %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name"))

      add_subplot_features(new_data = id_team_leader,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("team_leader"),
                           add_data = T,
                           ask_before_update = F)

    }

    if (exists("id_pi")) {

      id_pi <-
        id_pi %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name")) %>%
        rename(principal_investigator = PI)

      add_subplot_features(new_data = id_pi,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("principal_investigator"),
                           add_data = T,
                           ask_before_update = F)

    }

    if (exists("add_col_sep")) {

      add_col_sep <-
        add_col_sep %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name"))

      add_subplot_features(new_data = add_col_sep,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("additional_people"),
                           add_data = T,
                           ask_before_update = F)

    }

    if (exists("data_manager_sep")) {

      data_manager_sep <-
        data_manager_sep %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name"))

      add_subplot_features(new_data = data_manager_sep,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("data_manager"),
                           add_data = T,
                           ask_before_update = F)

    }

  }

  if(!add)
     message("no data added")

  return(new_data_renamed)

}










#' Add new individuals data
#'
#' Add new individuals data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble new data to be import
#' @param col_names_select string
#' @param col_names_corresp string
#' @param id_col integer indicate which name of col_names_select is the id for matching plot in metadata
#' @param launch_adding_data logical FALSE whether adding should be done or not
#' @param features_field vector string of field names in new_data containing the features associated with individual or stem data
#'
#'
#' @return No return value individuals updated
#' @export
add_individuals <- function(new_data ,
                            col_names_select,
                            col_names_corresp,
                            id_col,
                            features_field = NULL,
                            launch_adding_data = FALSE) {

  logs <-
    dplyr::tibble(
      column = as.character(),
      note = as.character()
    )

  mydb <- call.mydb()
  mydb_taxa <- call.mydb.taxa()

  if(length(col_names_select) != length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")
  
  if (!is.null(features_field)) for (i in 1:length(features_field))
    if (!any(colnames(new_data) == features_field[i]))
      stop(paste("features_field provide not found in new_data", features_field[i]))

  # new_data_renamed <-
  #   new_data %>%
  #   dplyr::rename_at(dplyr::vars(col_names_select[id_col]), ~ col_names_corresp[id_col])

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  new_data_renamed <-
    .link_table(data_stand = new_data_renamed,
              column_searched = "plot_name",
              column_name = "plot_name",
              id_field = "id_liste_plots",
              id_table_name = "id_liste_plots",
              db_connection = mydb,
              table_name = "data_liste_plots",
              keep_columns = "plot_name")


  ids_plot <-
    new_data_renamed %>%
    dplyr::select(plot_name, id_liste_plots) %>%
    dplyr::distinct(plot_name, id_liste_plots)

  if(any(is.na(ids_plot$id_liste_plots))) {
    warning("some plot are not found in metadata")
    print(ids_plot %>%
            dplyr::filter(is.na(id_liste_plots)))
    ids_plot <-
      ids_plot %>%
      dplyr::filter(is.na(id_liste_plots))

    logs <-
      dplyr::bind_rows(logs,
                     dplyr::tibble(
                       column = "id_liste_plots",
                       note = paste(nrow(ids_plot %>%
                                            dplyr::filter(is.na(id_liste_plots))),
                                     "some plot are not found in metadata")
                     ))
  }

  plots_already_in_db <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::filter(id_table_liste_plots_n %in% !!ids_plot$id_liste_plots) %>%
    dplyr::distinct(id_table_liste_plots_n) %>%
    dplyr::collect()

  if (nrow(plots_already_in_db) > 0) {
    print(
      plots_already_in_db %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "data_liste_plots") %>%
            dplyr::select(plot_name, id_liste_plots) %>%
            dplyr::collect(),
          by = c("id_table_liste_plots_n" = "id_liste_plots")
        ) %>%
        dplyr::pull(plot_name)
    )
    warning("data for some plots already in database")

  }


  new_data_renamed <-
    new_data_renamed %>%
    dplyr::left_join(ids_plot) %>%
    dplyr::rename(id_table_liste_plots_n = id_liste_plots)


  col_names_select <-
    col_names_select[-id_col]
  col_names_corresp <-
    col_names_corresp[-id_col]



  col_names_corresp <-
    c(col_names_corresp, "id_table_liste_plots_n")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  ### CHECKS
  method <-
    ids_plot %>%
    dplyr::left_join(
      dplyr::tbl(mydb, "data_liste_plots") %>%
        dplyr::select(plot_name, id_liste_plots, id_method) %>%
        dplyr::left_join(dplyr::tbl(mydb, "methodslist")) %>%
        dplyr::collect(),
      by = c("id_liste_plots" = "id_liste_plots")
    ) %>%
    dplyr::distinct(method)

  if (nrow(method) > 1) {
    print(method)
    stop("More than one method selected, import plot of one method at a time")
  }

  if (!any(colnames(new_data_renamed) == "idtax_n"))
    stop("idtax_n column missing")

  if (any(new_data_renamed$idtax_n == 0))
    stop(paste(
      "idtax_n is NULL for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  if (any(is.na(new_data_renamed$idtax_n)))
    stop(paste(
      "idtax_n is missing for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(idtax_n) %>%
    dplyr::left_join(
      try_open_postgres_table(table = "table_taxa", con = mydb_taxa) %>%
      # dplyr::tbl(mydb_taxa, "table_taxa") %>%
        dplyr::select(idtax_n, id_tax_famclass) %>%
        filter(idtax_n %in% !!new_data_renamed$idtax_n) %>%
        dplyr::collect(),
      by = c("idtax_n" = "idtax_n")
    ) %>%
    dplyr::filter(is.na(id_tax_famclass)) %>%
    dplyr::pull(idtax_n)

  if (length(unmatch_id_diconame) > 0)
    stop(paste("idtax_n not found in diconame", unmatch_id_diconame))

  if(any(is.na(names(new_data_renamed) == "dbh"))) {

    message("\n dbh and others traits measure should be added independantly using add_traits_measures function")

  }

  ## checking column given method
  if(dplyr::pull(method) == "Large") {

    # if (!any(colnames(new_data_renamed) == "tra"))
    #   stop("sous_plot_name column missing")
    if (!any(colnames(new_data_renamed) == "tag"))
      stop("tag column missing")
    

  }

  if (dplyr::pull(method) == "1ha-IRD" | dplyr::pull(method) == " ") {
    if (!any(colnames(new_data_renamed) == "tag"))
      stop("tag column missing - Tag individual")


    ### checking duplicated tags within plots
    duplicated_tags <-
      new_data_renamed %>%
      group_by(id_table_liste_plots_n, tag) %>%
      count() %>%
      filter(n > 1)

    duplicated_tags <-
      new_data_renamed %>%
      dplyr::left_join(
        duplicated_tags ,
        by = c(
          "id_table_liste_plots_n" = "id_table_liste_plots_n",
          "tag" = "tag"
        )
      ) %>%
      dplyr::filter(!is.na(n)) %>%
      dplyr::left_join(
        tbl(mydb, "data_liste_plots") %>%
          dplyr::select(id_liste_plots, plot_name) %>%
          dplyr::collect(),
        by = c("id_table_liste_plots_n" = "id_liste_plots")
      )

    if(nrow(duplicated_tags) > 0) {
      warning("\n Duplicated tags in some plots")
      print(duplicated_tags)

      readr::write_excel_csv(duplicated_tags, "duplicated_tags.csv")
    }

    if(any(names(new_data_renamed) == "multi_tiges_id")) {
      cli::cli_alert_info("Checking multi tiges")



    }

  }

  ## checking tag

  if(!is.numeric(new_data_renamed$tag)) {

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(tag = as.numeric(tag))

    if(any(is.na(new_data_renamed$tag)))
      new_data_renamed %>%
      filter(is.na(tag)) %>%
      print()
      stop("tag missing after converting to numeric")
  }

  # check herbarium specimen coherence

  if (!any(colnames(new_data_renamed) == "herbarium_nbe_type"))
    cli::cli_alert_danger("herbarium_nbe_type column missing")
  if (!any(colnames(new_data_renamed) == "herbarium_nbe_char"))
    cli::cli_alert_danger("herbarium_nbe_char column missing")

  if (any(colnames(new_data_renamed) == "herbarium_nbe_char")) {
    all_herb_ref <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_char) %>%
      dplyr::filter(!is.na(herbarium_nbe_char))
  }


  if (any(colnames(new_data_renamed) == "herbarium_nbe_type")) {

    all_herb_type <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_type) %>%
      dplyr::filter(!is.na(herbarium_nbe_type))

    if (nrow(all_herb_type) != nrow(all_herb_ref)) {

      print(all_herb_type)
      print(all_herb_ref)
      cli::cli_alert_warning("Number of herbarium specimen type and reference are not identical")

      missing_herb_ref <-
        all_herb_type %>%
        filter(!herbarium_nbe_type %in% all_herb_ref$herbarium_nbe_char)

      if(nrow(missing_herb_ref) > 0) {
        print(missing_herb_ref)
        stop("Specimen in type not found in reference specimens")
      }

      missing_herb_type <- all_herb_ref %>%
        filter(!herbarium_nbe_char %in% all_herb_type$herbarium_nbe_type)


      if(nrow(missing_herb_type) > 0) {

        cli::cli_alert_danger("Some specimens type not represented in specimens links")

        print(missing_herb_type)

        complete_type_specimen <-
          choose_prompt(message = "Complete automatically type specimen by taking the first individual?")
        
        

        if(complete_type_specimen) {

          new_data_renamed <-
            new_data_renamed %>%
            mutate(id_temp = 1:nrow(.))

          for (i in 1:nrow(missing_herb_type)) {

            id_selected <-
              new_data_renamed %>%
              filter(herbarium_nbe_char == missing_herb_type$herbarium_nbe_char[i]) %>%
              arrange(tag, id_table_liste_plots_n) %>%
              dplyr::slice(1) %>%
              dplyr::select(id_temp)

            new_data_renamed <-
              new_data_renamed %>%
              mutate(herbarium_nbe_type = replace(herbarium_nbe_type,
                                                  id_temp == id_selected$id_temp,
                                                  missing_herb_type$herbarium_nbe_char[i]))
                }

          new_data_renamed <-
            new_data_renamed %>%
            dplyr::select(-id_temp)

        }
      }
    }

    herb_type_dups <-
      new_data_renamed %>%
      dplyr::group_by(herbarium_nbe_type) %>%
      dplyr::count() %>%
      dplyr::filter(n > 1,!is.na(herbarium_nbe_type))


    if (nrow(herb_type_dups) > 0) {

      warning(paste(
        "herbarium_nbe_type is duplicated for",
        nrow(herb_type_dups),
        "specimen"
      ))

      new_data_renamed %>%
        dplyr::filter(herbarium_nbe_type %in% dplyr::pull(herb_type_dups, herbarium_nbe_type))

      logs <-
        dplyr::bind_rows(logs,
                         dplyr::tibble(
                           column = "herbarium_nbe_type",
                           note = paste(
                             "herbarium_nbe_type is duplicated for",
                             paste(
                               dplyr::pull(herb_type_dups, herbarium_nbe_type),
                               collapse = ";"
                             ),
                             "specimen"
                           )
                         ))
    }
  }

  ## check herbarium specimen reference coherence
  if(any(colnames(new_data_renamed)=="herbarium_nbe_char")) {
    herb_ref_multiple_taxa <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_char, idtax_n) %>%
      dplyr::filter(!is.na(herbarium_nbe_char)) %>%
      dplyr::group_by(herbarium_nbe_char) %>%
      dplyr::count() %>%
      dplyr::filter(n>1)

    herb_ref_multiple_taxa <-
      new_data_renamed %>%
      dplyr::filter(herbarium_nbe_char %in% dplyr::pull(herb_ref_multiple_taxa, herbarium_nbe_char)) %>%
      dplyr::select(herbarium_nbe_char, original_tax_name, idtax_n) %>%
      dplyr::distinct()

    if(nrow(herb_ref_multiple_taxa) > 0) {
      logs <-
        dplyr::bind_rows(logs,
                         dplyr::tibble(
                           column = "herbarium_nbe_char",
                           note = paste("herbarium_nbe_char carry different identification for",
                                        paste(herb_ref_multiple_taxa %>%
                                                dplyr::distinct(herbarium_nbe_char) %>%
                                                dplyr::pull(),
                                              collapse = "; "),
                                        paste(herb_ref_multiple_taxa %>%
                                                dplyr::distinct(original_tax_name) %>%
                                                dplyr::pull(),
                                              collapse = ", "))
                         ))
    }

  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(
      data_modif_d = lubridate::day(Sys.Date()),
      data_modif_m = lubridate::month(Sys.Date()),
      data_modif_y = lubridate::year(Sys.Date())
    )

  if(launch_adding_data) {

    print(list(new_data_renamed, logs))

    confirmed <- choose_prompt(message = "Confirm adding?")
    

    if(confirmed) {

      DBI::dbWriteTable(mydb, "data_individuals", new_data_renamed, append = TRUE, row.names = FALSE)
      cli::cli_alert_success("Added individuals : {nrow(new_data_renamed)} rows to individuals table")
    }
  }

  return(list(new_data_renamed, logs))

}








#' Exploring specimens data
#'
#' Exploring specimens data and if necessary export labels
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param collector string collector name
#' @param id_colnam integer id of collector
#' @param number integer specimen number
#' @param number_min integer minimum specimen number
#' @param number_max integer maximum specimen number
#' @param genus_searched string specimens of genus searched
#' @param tax_esp_searched string specimens of species searched
#' @param tax_fam_searched string specimens of family searched
#' @param id_search integer id searched
#' @param subset_columns logical if only a subset of columns should be provided
#' @param show_previous_modif logical if you want to see previous modification of the entry - useful to see previous identification for example
#' @param generate_labels logical if labels should be produced
#' @param project_title string if labels are produced title of the label
#' @param file_labels string if labels are produced name of the rtf file
#' @param extract_linked_individuals logical extract individuals linked to selected specimens
#'
#' @return A tibble
#' @export
query_specimens <- function(collector = NULL,
                            id_colnam = NULL,
                            number = NULL,
                            number_min = NULL,
                            number_max = NULL,
                            genus_searched = NULL,
                            tax_esp_searched = NULL,
                            tax_fam_searched = NULL,
                            id_search = NULL,
                            subset_columns = TRUE,
                            show_previous_modif = TRUE,
                            generate_labels = FALSE,
                            project_title = "Reference specimens collected in trees inventory",
                            file_labels = "labels",
                            extract_linked_individuals = FALSE) {

  mydb <- call.mydb()
  
  

  diconames_id <-
    try_open_postgres_table(table = "table_idtax", con = mydb) %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    dplyr::mutate(idtax_f = ifelse(is.na(idtax_good_n), idtax_n, idtax_good_n))
  
  
  
  query_speci <-
    try_open_postgres_table(table = "specimens", con = mydb) %>%
    dplyr::left_join(
      diconames_id %>%
        dplyr::select(-idtax_good_n),
      by = c("idtax_n" = "idtax_n")
    ) %>%
    # left_join(add_taxa_table_taxa(),
    #           by = c("idtax_f" = "idtax_n")) %>%
    dplyr::left_join(dplyr::tbl(mydb, "table_colnam"),
                     by = c("id_colnam" = "id_table_colnam"))

  
  # %>%
  #   dplyr::select(-id_specimen_old, -id_diconame, -photo_tranche, -id_colnam, -id_good, -id, -id_good_n)

  if (subset_columns & !generate_labels)
    query_speci <-
    query_speci %>%
    dplyr::select(
      colnam,
      colnbr,
      suffix,
      ddlat,
      ddlon,
      country,
      locality,
      detby,
      detd,
      detm,
      dety,
      add_col,
      cold,
      colm,
      coly,
      detvalue,
      description,
      id_specimen,
      idtax_f,
      id_tropicos,
      id_colnam
    )

  ## filter by collector or id_colnam (id of people table)
  if ((!is.null(collector) |
       !is.null(id_colnam)) & is.null(id_search)) {

    if (is.null(id_colnam)) {

      id_colnam <- .link_colnam(data_stand = tibble(colnam = collector),column_searched = "colnam")$id_colnam

    }

    query_speci <-
      query_speci %>%
      dplyr::filter(id_colnam == !!id_colnam)

  }

  if(!is.null(number) & is.null(id_search)) {

    var <- rlang::enquo(number)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr %in% var)


  }

  if(!is.null(id_colnam) & is.null(id_search)) {

    var <- rlang::enquo(id_colnam)

    query_speci <-
      query_speci %>%
      dplyr::filter(id_colnam %in% var)
  }



  if(!is.null(number_min) & is.null(id_search)) {

    var <- rlang::enquo(number_min)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr >= var)
  }

  if(!is.null(number_max) & is.null(id_search)) {

    var <- rlang::enquo(number_max)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr<=var)
  }

  if(!is.null(genus_searched) & is.null(id_search)) {

    var <- rlang::enquo(genus_searched)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, tax_gen))
  }

  if(!is.null(tax_fam_searched) & is.null(id_search)) {

    var <- rlang::enquo(tax_fam_searched)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, tax_fam))
  }

  if(!is.null(tax_esp_searched) & is.null(id_search)) {

    var <- rlang::enquo(tax_esp_searched)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, tax_esp))
  }

  if(!is.null(id_search)) {

    var <- rlang::enquo(id_search)

    query_speci <-
      query_speci %>%
      dplyr::filter(id_specimen %in% var)
  }
  query <-
    query_speci %>%
    dplyr::collect()


  query_tax <- add_taxa_table_taxa(ids = unique(query$idtax_f))
  query_tax <- query_tax %>% collect()

  query <- left_join(
    query,
    query_tax %>%
      dplyr::select(-data_modif_d,-data_modif_m,-data_modif_y),
    by = c("idtax_f" = "idtax_n")
  )

  # print(query)

  if(extract_linked_individuals) {

    linked_ind <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_specimen == !!query$id_specimen) %>%
      dplyr::select(id_n, id_specimen) %>%
      dplyr::collect()

    linked_ind <-
      query_plots(
        id_individual = linked_ind$id_n,
        extract_individuals = TRUE,
        remove_ids = FALSE
      )

    cli::cli_alert_info(
      "This specimen is linked to {nrow(linked_ind)} individuals from {length(unique(linked_ind$plot_name))} plot(s)"
    )

  }

  if (nrow(query) == 1 & show_previous_modif) {

  }


  nrow_query <-
    nrow(query)

  if(nrow(query) < 50)
  {
    res_html <-
      tibble(columns = names(query), data.frame(t(query),
                                                fix.empty.names = T)) %>%
      mutate_all(~ tidyr::replace_na(., ""))


    res_html %>%
      kableExtra::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling("striped", full_width = F) %>%
      print()
  }

  if(!extract_linked_individuals) return(query)

  if(extract_linked_individuals) return(list(linked_ind = linked_ind,
                                             query = query))

}








#' Add new specimens data
#'
#' Add new specimens data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble new data to be imported
#' @param col_names_select string plot name of the selected plots
#' @param col_names_corresp string country of the selected plots
#' @param plot_name_field integer indicate which name of col_names_select is the id for matching liste plots table
#' @param collector_field integer indicate which name of col_names_select is the id for matching collector
#'
#' @param launch_adding_data logical FALSE whether adding should be done or not
#'
#' @return No return value individuals updated
#' @export
add_specimens <- function(new_data ,
                          col_names_select,
                          col_names_corresp,
                          # id_col,
                          plot_name_field = NULL,
                          collector_field = NULL,
                          launch_adding_data = FALSE) {

  # logs <-
  #   dplyr::tibble(
  #     column = as.character(),
  #     note = as.character()
  #   )

  mydb <- call.mydb()
  mydb_taxa <- call.mydb.taxa()

  if(length(col_names_select)!=length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")

  new_data_renamed <-
    new_data %>%
    mutate(id_new_data=1:nrow(.))

  for (i in 1:length(col_names_select)) {
    if (any(colnames(new_data_renamed) == col_names_select[i])) {
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename(!!col_names_corresp[i] := !!col_names_select[i])
        # dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    } else{
      stop(paste(
        "Column name provided not found in provided new dataset",
        col_names_select[i]
      ))
    }
  }

  col_names_corresp <-
    c(col_names_corresp, "id_new_data")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  ### check diconame id
  if(!any(colnames(new_data_renamed)=="idtax_n")) stop("idtax_n column missing")

  if (any(new_data_renamed$idtax_n == 0))
    stop(paste(
      "idtax_n is NULL for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  if (any(is.na(new_data_renamed$idtax_n)))
    stop(paste(
      "idtax_n is missing for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(idtax_n) %>%
    dplyr::left_join(try_open_postgres_table(table = "table_taxa", con = mydb_taxa) %>%
                       dplyr::select(idtax_n, idtax_good_n) %>%
                       dplyr::filter(idtax_n %in% !!new_data_renamed$idtax_n) %>%
                       dplyr::collect() %>%
                       dplyr::mutate(tag = 1), by=c("idtax_n" = "idtax_n")) %>%
    dplyr::filter(is.na(tag)) %>%
    dplyr::pull(idtax_n)

  if (length(unmatch_id_diconame) > 0)
    stop(paste("idtax_n not found in table_taxa", unmatch_id_diconame))


  ### check locality and adding it if link to plots
  if(!any(colnames(new_data_renamed) == "locality"))
    warning("locality column missing"
    )

  ### Linking collectors names
  if (!is.null(collector_field)) {


    new_data_renamed <-
      .link_colnam(data_stand = new_data_renamed,
                   column_searched = collector_field)

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-original_colnam)

  } else{
    if (!any(colnames(new_data_renamed) == "id_colnam"))
      stop("indicate the field if of collector name for standardizing")
  }

  ### check determination data
  if (any(colnames(new_data_renamed) == "detd")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(detd = as.numeric(detd))
  }

  if(any(colnames(new_data_renamed) == "detm")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(detm = as.numeric(detm))
  }

  if (any(colnames(new_data_renamed) == "dety")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(dety = as.numeric(dety))
  }

  if (!any(names(new_data_renamed) == "suffix")) {

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(suffix = NA) %>%
      dplyr::mutate(suffix = as.character(suffix))

  }

  ## check if not duplicates in new specimens
  dup_imported_datasets <-
    new_data_renamed %>%
    dplyr::select(colnbr, id_colnam, suffix, id_new_data) %>%
    group_by(colnbr, id_colnam, suffix) %>%
    count() %>%
    filter(n > 1)

  if (nrow(dup_imported_datasets) > 0) {
    print(dup_imported_datasets)
    stop("Duplicates in imported dataset")
  }


  ## check if specimens are not already in database
  matched_specimens <-
    dplyr::tbl(mydb, "specimens") %>%
    dplyr::select(colnbr, id_colnam, id_specimen) %>%
    dplyr::filter(!is.na(id_colnam)) %>%
    dplyr::collect() %>%
    dplyr::left_join(
      new_data_renamed %>%
        dplyr::select(colnbr, id_colnam, id_new_data),
      by = c("colnbr" = "colnbr", "id_colnam" = "id_colnam")
    ) %>%
    dplyr::filter(!is.na(id_new_data))


  if (nrow(matched_specimens) > 0) {
    warning(paste("New specimens already in database", nrow(matched_specimens)))
    print(matched_specimens)
  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(data_modif_d=lubridate::day(Sys.Date()),
                       data_modif_m=lubridate::month(Sys.Date()),
                       data_modif_y=lubridate::year(Sys.Date()))

  if (any(colnames(new_data_renamed) == "col_name"))
    new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-col_name)

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-id_new_data)

  if(launch_adding_data) {

    print(list(new_data_renamed))

    confirmed <- choose_prompt(message = "Confirm adding?")

    if(confirmed) {

      DBI::dbWriteTable(mydb, "specimens", new_data_renamed, append = TRUE, row.names = FALSE)

      message(paste0(nrow(new_data_renamed), " records added to specimens table"))
    }

  }

  return(list(new_data_renamed))

}










#' Query in colnam table
#'
#' Query in colnam table by id or pattern
#'
#' @return tibble with query results
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param id_trait integer id of trait to select
#' @param pattern string vector trait to look for in the table
#'
#' @export
query_colnam <- function(id_colnam = NULL, pattern = NULL) {

  mydb <- call.mydb()

  if (!is.null(id_colnam)) {
    cli::cli_alert_info("query colnam by id")

    table_colnam <- try_open_postgres_table(table = "table_colnam", con = mydb)

    valuetype <-
      table_colnam %>%
      dplyr::filter(id_table_colnam %in% !!id_colnam) %>%
      dplyr::collect()
  }

  if (is.null(id_colnam) & !is.null(pattern)) {

    cli::cli_alert_info("query colnam by string pattern")

    sql <- glue::glue_sql(paste0("SELECT * FROM table_colnam WHERE colnam ILIKE '%", pattern, "%'"))

    valuetype <- func_try_fetch(con = mydb, sql = sql)
  }

  return(valuetype)

}







#' Add 1ha IRd plot coordinates
#'
#' print table as html in viewer reordered
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset tibble
#' @param ddlat column name of dataset containing latitude in decimal degrees
#' @param ddlon column name of dataset containing longitude in decimal degrees
#' @param launch_add_data whether addd data or not
#' @param X_theo column that contain the X quadrat name
#' @param Y_theo column that contain the Y quadrat name
#' @param check_existing_data check if data already exists
#' @param add_cols string character vectors with columns names of dataset of additonal information
#' @param cor_cols string character vectors with colums names corresponding to add_cols
#' @param collector_field string vector of size one with column name containing the name of the person collecting data
#'
#' @return print html in viewer
#' @export
add_plot_coordinates <-
  function(dataset,
           ddlat = "Latitude",
           ddlon = "Longitude",
           launch_add_data = FALSE,
           X_theo = "X_theo",
           Y_theo = "Y_theo",
           check_existing_data = TRUE,
           add_cols = NULL,
           cor_cols = NULL,
           collector_field = NULL) {
    
    X_theo_p <- dplyr::sym(X_theo)
    Y_theo_p <- dplyr::sym(Y_theo)
    
    dataset <- 
      dataset %>% 
      mutate(quadrat = paste(!!X_theo_p, !!Y_theo_p, sep = "_"))
    
    all_q <- dataset %>%
      distinct(quadrat) %>% pull()
    
    all_cols <- c(ddlat, ddlon)
    
    res_l <- vector('list', length(all_cols))
    for (i in 1:length(all_cols)) {
      col_s <- dplyr::sym(all_cols[i])
      
      if (!any(names(dataset) == col_s))
        stop(glue::glue("{col_s} column not found"))
      
      if (i == 1)
        names_pref <- "ddlat_plot_X_Y_"
      if (i == 2)
        names_pref <- "ddlon_plot_X_Y_"
      
      dataset <-
        dataset %>%
        mutate(!!col_s := as.numeric(!!col_s))
      
      res_l[[i]] <-
        tidyr::pivot_wider(
          data = dataset,
          names_from = quadrat,
          values_from = !!col_s,
          names_prefix = names_pref
        ) %>%
        group_by(plot_name) %>%
        summarise(across(starts_with(names_pref), ~ mean(.x, na.rm = TRUE)),
                  across(all_of(add_cols), ~ first(.x)),
                  across(all_of(collector_field), ~ first(.x)))
      
      print(res_l[[i]])
      
      if (launch_add_data) {
        
        add_subplot_features(new_data = res_l[[i]], 
                             col_names_select = add_cols, 
                             col_names_corresp = cor_cols, 
                             plot_name_field = "plot_name", 
                             subplottype_field = res_l[[i]] %>% 
                               dplyr::select(starts_with("ddl")) %>% names(), 
                             add_data = TRUE,
                             ask_before_update = FALSE,
                             check_existing_data = check_existing_data)
        
      } else {
        cli::cli_alert_danger("No data added because launch_add_data is FALSE")
      }
    }
    
    return(res_l)
    
  }



