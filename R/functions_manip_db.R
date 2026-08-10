


#' List of countries
#'
#' Provide list of valid countries from lookup table
#'
#' @return A tibble of all countries from table_countries
#' @import dplyr
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
country_list <- function() {

  mydb <- call.mydb()

  nn <- func_try_fetch(con = mydb,
                       sql = DBI::SQL("SELECT * FROM table_countries"))

  nn <- nn %>% arrange(country)

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
                       sql = DBI::SQL("SELECT * FROM methodslist"))
  
  return(nn)
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
#' @param interactive Logical. Whether the query should be interactive. TRUE by default.
#' @param show_multiple_census Logical. Whether to show multiple census data. Optional.
#' @param extract_coordinates Logical. Whether to extract subplot coordinates as separate tables.
#'   When TRUE, returns `coordinates` (raw coordinate data) and `coordinates_sf` (spatial features)
#'   in the output list. Default is FALSE.
#' @param show_all_coordinates `r lifecycle::badge("deprecated")` Use `extract_coordinates` instead.
#' @param remove_ids Logical. Whether to remove ID columns from output. Optional.
#' @param extract_traits Logical. Whether to extract taxonomic traits. Optional.
#' @param extract_individual_features Logical. Whether to extract individual-level features. Optional.
#' @param traits_to_genera Logical. Whether to aggregate traits to genus level. Optional.
#' @param wd_fam_level Logical. Whether to use family-level wood density. Optional.
#' @param include_liana Logical. Whether to include lianas. Optional.
#' @param extract_subplot_features Logical. Whether to extract subplot features. Optional.
#' @param concatenate_stem Logical. Whether to concatenate multiple stems. Optional.
#' @param issues Character. How to handle flagged measurements. Options:
#'   \itemize{
#'     \item \code{"remove"} (default): Drop flagged measurements before aggregation.
#'     \item \code{"include"}: Keep flagged measurements and add an issue column in output.
#'     \item \code{"ignore"}: Keep flagged measurements but do not show the issue column.
#'   }
#' @param census_strategy Character. Strategy for selecting census when `show_multiple_census = FALSE`.
#'   Options: "last" (default, most recent census), "first" (earliest census), or "mean" (average across all censuses).
#'   When "first" or "last" is selected, individuals recruited after the first census or dead before the last census
#'   will have NA values, reflecting biological reality.
#' @param include_measurement_ids Logical. Whether to include measurement IDs in aggregated output. Optional.
#' @param individual_features_format Character. Format for individual-level feature measurements.
#'   `"wide"` (default) returns one row per individual with one column per trait (aggregated).
#'   `"long"` returns one row per individual per measurement: if an individual has two diameter
#'   measurements, it will appear in two rows. Adds columns `trait`, `traitvalue`,
#'   `traitvalue_char`, `valuetype`, `census_name`, and `census_date` (NA when not census-linked).
#'   Census filtering via `show_multiple_census` / `census_strategy` is still applied.
#'   Incompatible with `concatenate_stem = TRUE`.
#'   `"census_pairs"` returns one row per consecutive pair of censuses per individual.
#'   Census pairing is plot-level: every individual is crossed against all consecutive
#'   census pairs of its plot, receiving `NA` at any census where it has no measurement
#'   (e.g. recruited at census_2 → `stem_diameter_0 = NA`; dead at census_2 →
#'   `stem_diameter_1 = NA`). All individual-level traits appear as `<trait>_0` and
#'   `<trait>_1` columns. Additional columns: `census_name_0`, `census_name_1`,
#'   `date_census0`, `date_census1`, `time` (days between the two censuses).
#'   All available censuses are used regardless of `show_multiple_census`.
#'   Incompatible with `concatenate_stem = TRUE`.
#' @param output_style Either a character scalar -- one of `"auto"`,
#'   `"minimal"`, `"standard"`, `"permanent_plot"`,
#'   `"permanent_plot_multi_census"`, `"transect"`, `"full"`,
#'   `"census_pairs"` -- or a `plot_output_style` object built with
#'   [output_style()]. Defaults to `"auto"`, which picks a style from
#'   the `method` field of the queried plots. When
#'   `individual_features_format = "census_pairs"`, character values
#'   (other than `"full"`) are overridden to `"census_pairs"`; a custom
#'   `plot_output_style` object is respected as-is.
#'   Use [list_output_styles()] for an overview of available built-in
#'   styles, [get_output_style()] to inspect what a style does, and
#'   [output_style()] to build a custom one (typically with `based_on`
#'   to inherit from a built-in and override a few fields). The
#'   configuration schema is documented in [output_style()].
#' @param con Optional database connection to main database. If NULL, will call call.mydb() to establish connection.
#'   If you've already connected with `mydb <- call.mydb()`, pass `con = mydb` to avoid re-prompting.
#' @param con.taxa Optional database connection to taxa database. If NULL, will check for `mydb.taxa` in calling environment,
#'   otherwise will call call.mydb.taxa() to establish connection. Pass explicitly to avoid credential prompts.
#'
#' @returns 
#' A list or data frame containing plot data and associated information. When multiple 
#' components are requested, returns a list with elements like `extract`, `census_features`, 
#' `coordinates`, and `coordinates_sf`. If only one component is available, returns that 
#' component directly. Returns `NA` if no plots are found matching the criteria.
#'
#' @importFrom DBI dbSendQuery dbFetch dbClearResult dbWriteTable
#' @importFrom stringr str_flatten str_trim str_extract
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
                        interactive = TRUE,
                        show_multiple_census = FALSE,
                        extract_coordinates = FALSE,
                        show_all_coordinates = lifecycle::deprecated(),
                        remove_ids = TRUE,
                        extract_traits = TRUE,
                        extract_individual_features = TRUE,
                        traits_to_genera = FALSE,
                        wd_fam_level = FALSE,
                        include_liana = FALSE,
                        extract_subplot_features = TRUE,
                        concatenate_stem = FALSE,
                        issues = c("remove", "include", "ignore"),
                        include_measurement_ids = FALSE,
                        exact_match = FALSE,
                        census_strategy = c("last", "first", "mean"),
                        individual_features_format = c("wide", "long", "census_pairs"),
                        output_style = "auto",
                        backbone = c("internal", "wcvp"),
                        con = NULL,
                        con.taxa = NULL) {

  backbone <- match.arg(backbone)

  # Match arguments
  census_strategy <- match.arg(census_strategy)
  individual_features_format <- match.arg(individual_features_format)
  issues <- match.arg(issues)

  # Validate `output_style`: accepts a character name (incl. "auto"), a
  # `plot_output_style` object built with output_style(), or a raw list
  # that passes validate_output_style().
  output_style <- .validate_query_plots_output_style(output_style)

  # When output_style is explicitly "census_pairs", auto-switch individual_features_format
  # to "census_pairs" for consistent paired-census output.
  if (!inherits(output_style, "plot_output_style") &&
      identical(output_style, "census_pairs") &&
      individual_features_format != "census_pairs") {
    cli::cli_alert_info(
      "`output_style = 'census_pairs'` detected: automatically setting `individual_features_format = 'census_pairs'` for consistent output."
    )
    individual_features_format <- "census_pairs"
  }

  if (individual_features_format %in% c("long", "census_pairs") && isTRUE(concatenate_stem)) {
    cli::cli_alert_warning(
      "`individual_features_format = '{individual_features_format}'` is incompatible with `concatenate_stem = TRUE`. Setting `concatenate_stem = FALSE`."
    )
    concatenate_stem <- FALSE
  }

  # Handle deprecated parameter
  if (lifecycle::is_present(show_all_coordinates)) {
    lifecycle::deprecate_warn(
      when = "1.9.4",
      what = "query_plots(show_all_coordinates)",
      with = "query_plots(extract_coordinates)",
      details = "The parameter name has been changed for clarity."
    )
    extract_coordinates <- show_all_coordinates
  }

  # Use provided connection or create new one
  mydb <- if (!is.null(con)) con else call.mydb()

  # Use provided taxa connection or check environment, else create new one
  if (!is.null(con.taxa)) {
    mydb.taxa <- con.taxa
  } else if (exists("mydb.taxa", envir = parent.frame()) && test_connection(get("mydb.taxa", envir = parent.frame()))) {
    mydb.taxa <- get("mydb.taxa", envir = parent.frame())
  } else {
    mydb.taxa <- call.mydb.taxa()
  }

  # When showing multiple censuses, issues must be kept (removing would lose census data)
  if (show_multiple_census && issues == "remove") {
    cli::cli_alert_info("Setting issues to 'ignore' because multiple censuses are shown")
    issues <- "ignore"
  }
  
  
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
      query_builder <- query_builder$filter_country(country, interactive = interactive)
    }
    
    if (!is.null(plot_name)) {
      query_builder <- query_builder$filter_plot_name(plot_name, exact_match = exact_match)
    }
    
    if (!is.null(method)) {
      query_builder <- query_builder$filter_method(method, interactive = interactive)
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
    
    # Summarise census info per plot and join into res
    if (exists("census_features") && is.data.frame(census_features) && nrow(census_features) > 0) {
      census_summary <- census_features %>%
        mutate(
          census_date = dplyr::case_when(
            !is.na(day)   ~ paste(year, sprintf("%02d", month), sprintf("%02d", day), sep = "-"),
            !is.na(month) ~ paste(year, sprintf("%02d", month), sep = "-"),
            TRUE           ~ as.character(year)
          )
        ) %>%
        group_by(id_table_liste_plots) %>%
        summarise(
          n_census     = dplyr::n(),
          first_census = census_date[which.min(typevalue)],
          last_census  = census_date[which.max(typevalue)],
          .groups = "drop"
        )

      res <- res %>%
        left_join(census_summary, by = c("id_liste_plots" = "id_table_liste_plots"))
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
    if (extract_coordinates) {

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
            coord2 = purrr::map_chr(stringr::str_split(type, "_"), ~.x[length(.x)]),
            coord1 = purrr::map_chr(stringr::str_split(type, "_"), ~.x[length(.x) - 1]),
            coord3 = purrr::map_chr(stringr::str_split(type, "_"), ~.x[1]),
            coord4 = purrr::map_chr(stringr::str_split(type, "_"), ~.x[2])
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

          coord_processed <- tryCatch({
            .process_coordinates(
              all_coordinates = all_coordinates_subplots_rf,
              res = res
            )
          }, error = function(e) {
            cli::cli_alert_warning("Coordinate processing failed: {e$message}")
            cli::cli_alert_info("Returning raw coordinate data without polygon corrections")
            list(coordinates_sf = NULL, coordinates_raw = all_coordinates_subplots_rf)
          })

          # Only create sf objects if we have processed coordinates
          if (!is.null(coord_processed$coordinates_sf) && nrow(coord_processed$coordinates_sf) > 0) {
            sf_joined <- coord_processed$coordinates_sf %>%
              left_join(
                res %>% select(id_liste_plots, plot_name),
                by = "id_liste_plots"
              )
            coordinates_subplots_plot_sf <- tryCatch(
              sf::st_as_sf(sf_joined),
              error = function(e) {
                cli::cli_alert_warning("Could not convert processed coordinates to sf object: {e$message}")
                NULL
              }
            )
          }

          coordinates_subplots <- coord_processed$coordinates_raw %>%
            left_join(
              res %>% select(id_liste_plots, plot_name),
              by = c("id_table_liste_plots" = "id_liste_plots")
            )
        }
        
      } else {
        extract_coordinates <- FALSE
        cli::cli_alert_danger("No coordinates for quadrat available")
      }
    }
  }
  

  if (nrow(res) == 0) {
    cli::cli_alert_danger("No plot are found based on inputs")
    return(NA)
  }

  res <- res %>% dplyr::arrange(plot_name)
  
  res_meta_data <- res

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

    # Wrap map creation in tryCatch to handle graphics parameter errors
    tryCatch({
      outputmap <- leaflet::leaflet(data_sf) %>%
        leaflet::addProviderTiles("OpenStreetMap.DE",  group = "OpenStreetMap.DE") %>%
        leaflet::addProviderTiles("Esri.WorldImagery",  group = "Esri.WorldImagery") %>%
        leaflet::addProviderTiles("Esri.WorldPhysical", group = "Esri.WorldPhysical") %>%
        leaflet::addCircleMarkers(label = ~plot_name, popup = ~plot_name) %>%
        leaflet::addLayersControl(
          baseGroups = map_types,
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addScaleBar(position = "bottomleft")

      if (extract_coordinates && exists("coordinates_subplots_plot_sf") && !is.null(coordinates_subplots_plot_sf)) {
        outputmap <- outputmap %>%
          leaflet::addPolygons(data = coordinates_subplots_plot_sf,
                               color = "red", weight = 2, fillOpacity = 0.1,
                               label = ~plot_name)
      }

      print(outputmap)
    }, error = function(e) {
      # Check if it's the graphics parameter error
      if (grepl("par.*pin|graphique.*pin", e$message, ignore.case = TRUE)) {
        cli::cli_alert_warning("Cannot create interactive map due to graphics device issue")
        cli::cli_alert_info("This is often caused by RStudio plot window size or display settings")
        cli::cli_alert_info("Workarounds:")
        cli::cli_alert_info("  1. Try resizing your RStudio Plots pane")
        cli::cli_alert_info("  2. Run: dev.new() to create a new graphics device")
        cli::cli_alert_info("  3. Run: options(device = 'RStudioGD') and restart R")
        cli::cli_alert_info("  4. Use map = FALSE to skip map creation")
        cli::cli_alert_info("Map creation skipped - data returned without visualization")
      } else {
        # Re-throw other errors
        cli::cli_alert_danger("Error creating map: {e$message}")
        stop(e)
      }
    })

  }

  if (extract_individuals) {
    
    res <- process_individuals(
      plots_data = res,
      con = mydb,
      con_taxa = mydb.taxa,
      id_individual = id_individual,
      id_tax = id_tax,
      tag = tag,
      include_liana = include_liana,
      census_strategy = census_strategy,
      show_multiple_census = show_multiple_census,
      backbone = backbone
    )
    
    res <- enrich_with_traits(individuals = res,
                              con = mydb,
                              extract_individual_features = extract_individual_features,
                              extract_traits = extract_traits,
                              traits_to_genera =  traits_to_genera,
                              wd_fam_level = wd_fam_level,
                              show_multiple_census = show_multiple_census,
                              issues = issues,
                              include_measurement_ids = include_measurement_ids,
                              census_strategy = census_strategy,
                              individual_features_format = individual_features_format)

    # Ensure concatenate_stem is logical
    if (!is.logical(concatenate_stem)) {
      concatenate_stem <- isTRUE(concatenate_stem)
    }

    res <- process_stems(res, concatenate_stem)

    # Fetch all-census height-diameter data for the H-D table.
    # This is done regardless of show_multiple_census so that heights measured
    # in non-selected censuses are always available for the height_diameter table.
    hd_source <- tryCatch({
      hd_all_traits <- traits_list()
      hd_trait_ids <- hd_all_traits$id_trait[
        hd_all_traits$trait %in% c("tree_height", "stem_diameter", "height_of_stem_diameter")
      ]
      if (length(hd_trait_ids) > 0 && length(unique(res$id_n)) > 0) {
        hd_raw <- query_individual_features(
          individual_ids = unique(res$id_n),
          trait_ids      = hd_trait_ids,
          include_multi_census = TRUE,
          format         = "long",
          issues         = issues,
          con            = mydb
        )
        if (nrow(hd_raw) > 0) hd_raw else NULL
      } else {
        NULL
      }
    }, error = function(e) {
      cli::cli_alert_warning("Could not fetch all-census H-D data: {e$message}")
      NULL
    })

    # Build citations × traits pivot when taxonomic traits were extracted
    data_sources <- NULL
    if (extract_traits && "idtax_individual_f" %in% names(res)) {
      unique_taxa_cit <- unique(res$idtax_individual_f)
      unique_taxa_cit <- unique_taxa_cit[!is.na(unique_taxa_cit)]
      if (length(unique_taxa_cit) > 0) {
        tryCatch({
          traits_cit <- query_taxa_traits(
            idtax            = unique_taxa_cit,
            include_synonyms = FALSE,
            include_citation = TRUE,
            format           = "long",
            con              = mydb
          )
          raw_cit <- traits_cit$traits_raw
          if (!is.null(raw_cit) && nrow(raw_cit) > 0) {
            data_sources <- build_data_sources_table(raw_cit)
          }
        }, error = function(e) {
          cli::cli_alert_warning("Could not build data_sources table: {e$message}")
        })
      }
    }

  }

  if (remove_ids & extract_individuals) {

    cli::cli_alert_warning("ids removed - remove_ids = {remove_ids} ")

    res <-
      res %>%
      dplyr::rename(idDB = id_n) %>%
      dplyr::select(-dplyr::starts_with("id_")) %>%
      dplyr::rename(id_n = idDB)

  }

  if (remove_ids & !extract_individuals) {

    cli::cli_alert_warning("Identifiers are removed because the parameter 'remove_ids' = {remove_ids} ")

    res <-
      res %>%
      dplyr::rename(idDB = id_liste_plots) %>%
      dplyr::select(-dplyr::starts_with("id_")) %>%
      dplyr::rename(id_liste_plots = idDB)

  }

  res_list <-
    list(
      extract = NA,
      meta_data = NA,
      census_features = NA,
      hd_source = NA,
      coordinates = NA,
      coordinates_sf = NA
    )

  res_list$extract <- res
  res_list$meta_data <- res_meta_data

  if (nrow(res) < 100)
    print_table(res_print = res)

  if (show_multiple_census && exists("census_features")) {
    res_list$census_features <- census_features

    print_table(census_features)
  }

  # Include all-census H-D source data when available
  if (exists("hd_source") && !is.null(hd_source)) {
    res_list$hd_source <- hd_source
  }

  if (extract_coordinates && exists("coordinates_subplots"))
    res_list$coordinates <- coordinates_subplots

  if (extract_coordinates && exists("coordinates_subplots_plot_sf"))
    res_list$coordinates_sf <- coordinates_subplots_plot_sf

  res_list <- res_list[!is.na(res_list)]

  if (exists("data_sources") && !is.null(data_sources)) {
    res_list$data_sources <- data_sources
  }

  if (length(res_list) == 1)
    res_list <- res_list[[1]]

  # Apply output style ---------------------------------------------------
  # `output_style` can be a character ("auto" or a built-in name) or a
  # `plot_output_style` object (custom). Compute a display name and an
  # "is_full" flag without forcing string semantics on custom objects.
  is_custom_style <- inherits(output_style, "plot_output_style")

  if (!is_custom_style && identical(output_style, "auto")) {
    detected_style <- .detect_style_from_method(data = res_meta_data)
    cli::cli_alert_info("Auto-detected output style: '{detected_style}' based on method field")
    output_style <- detected_style
  }

  # census_pairs individual format requires its own output style config
  # (unless the user explicitly requested "full" style). Custom style
  # objects are respected as-is -- the user made an explicit choice.
  if (!is_custom_style &&
      individual_features_format == "census_pairs" &&
      !identical(output_style, "full")) {
    output_style <- "census_pairs"
  }

  is_full <- (!is_custom_style && identical(output_style, "full")) ||
             (is_custom_style && identical(attr(output_style, "style_name"), "full"))

  style_display_name <- if (is_custom_style) {
    attr(output_style, "style_name") %||% "<custom>"
  } else {
    output_style
  }

  # Apply style restructuring (unless "full")
  if (!is_full) {
    res_list <- .apply_output_style(
      data = res_list,
      style = output_style,
      extract_individuals = extract_individuals,
      show_multiple_census = show_multiple_census
    )

    # Inform user about restructuring
    if (inherits(res_list, "plot_query_list")) {
      cli::cli_alert_success(
        "Output restructured using '{style_display_name}' style. Use names() to see available tables."
      )
    }
  } else {
    # For "full" style, rename internal names to user-friendly equivalents
    if (is.list(res_list) && !is.data.frame(res_list)) {
      if ("meta_data" %in% names(res_list))
        names(res_list)[names(res_list) == "meta_data"] <- "metadata"
      if ("extract" %in% names(res_list))
        names(res_list)[names(res_list) == "extract"] <- "individuals"
      # Process raw hd_source into a clean height_diameter table
      if ("hd_source" %in% names(res_list) && extract_individuals &&
          !is.null(res_list$hd_source) && is.data.frame(res_list$hd_source)) {
        hd_pairs <- .extract_height_diameter_pairs(
          data                 = res_list$individuals,
          show_multiple_census = show_multiple_census,
          hd_source            = res_list$hd_source
        )
        res_list$hd_source <- NULL
        if (!is.null(hd_pairs) && nrow(hd_pairs) > 0)
          res_list$height_diameter <- hd_pairs
      } else {
        res_list$hd_source <- NULL
      }
      class(res_list) <- c("plot_query_list", "list")
      attr(res_list, "style") <- "full"
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
                                include_liana = FALSE,
                                census_strategy = c("last", "first", "mean"),
                                show_multiple_census = FALSE,
                                backbone = c("internal", "wcvp")) {

  backbone <- match.arg(backbone)

  census_strategy <- match.arg(census_strategy)
  cli::cli_rule(left = "Processing individuals")

  # Extraction des métadonnées de plots
  plot_metadata <- plots_data %>%
    select(
      plot_name, locality_name, id_liste_plots,
      contains("date_census"), contains("team_leader"),
      contains("principal_investigator"), ddlat, ddlon
    )

  # Handle census date selection when not showing multiple censuses
  if (!show_multiple_census && census_strategy %in% c("first", "last")) {
    census_cols <- names(plot_metadata)[grepl("^date_census_\\d+$", names(plot_metadata))]

    if (length(census_cols) > 0) {
      # Extract census numbers and find first/last
      census_numbers <- as.numeric(gsub("date_census_", "", census_cols))

      if (census_strategy == "first") {
        selected_col <- paste0("date_census_", min(census_numbers, na.rm = TRUE))
      } else {
        selected_col <- paste0("date_census_", max(census_numbers, na.rm = TRUE))
      }

      if (selected_col %in% names(plot_metadata)) {
        # Keep only the selected census date and rename it
        plot_metadata <- plot_metadata %>%
          mutate(census_date = !!sym(selected_col)) %>%
          select(-all_of(census_cols))

        cli::cli_alert_info("Selected {census_strategy} census date column: {selected_col}")
      }
    }
  }
  
  # Extraction via merge_individuals_taxa_v2
  cli::cli_alert_info("Fetching individuals")
  
  individuals <- merge_individuals_taxa(
    id_individual = id_individual,
    id_plot = plot_metadata$id_liste_plots,
    id_tax = id_tax,
    clean_columns = TRUE,
    con_taxa = con_taxa,
    con = con,
    backbone = backbone
  )
  
  # Filtrage par tag
  if (!is.null(tag)) {
    cli::cli_alert_info("Filtering by tag: {paste(tag, collapse = ', ')}")
    individuals <- individuals %>%
      dplyr::filter(tag %in% .env$tag)
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
#' @param issues Character. How to handle flagged measurements: "remove", "include", or "ignore".
#'
#' @return Data frame enriched with traits
#' @export
enrich_with_traits <- function(individuals, con,
                               extract_individual_features = TRUE,
                               extract_traits = TRUE,
                               traits_to_genera = FALSE,
                               wd_fam_level = FALSE,
                               show_multiple_census = FALSE,
                               issues = c("remove", "include", "ignore"),
                               include_measurement_ids = FALSE,
                               census_strategy = c("last", "first", "mean"),
                               individual_features_format = c("wide", "long", "census_pairs")) {

  census_strategy <- match.arg(census_strategy)
  individual_features_format <- match.arg(individual_features_format)
  issues <- match.arg(issues)
  mydb <- call.mydb()

  cli::cli_rule(left = "Processing traits")

  # Traits individuels
  if (extract_individual_features) {
    individuals <- enrich_individual_traits(
      individuals = individuals,
      con = con,
      show_multiple_census = show_multiple_census,
      issues = issues,
      include_measurement_ids = include_measurement_ids,
      census_strategy = census_strategy,
      individual_features_format = individual_features_format
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
enrich_individual_traits <- function(individuals, con, show_multiple_census,
                                     issues = c("remove", "include", "ignore"),
                                     include_measurement_ids = FALSE,
                                     census_strategy = c("last", "first", "mean"),
                                     individual_features_format = c("wide", "long", "census_pairs")) {

  census_strategy <- match.arg(census_strategy)
  individual_features_format <- match.arg(individual_features_format)
  issues <- match.arg(issues)
  cli::cli_alert_info("Enriching with individual-level traits")

  all_traits <- traits_list()

  if (individual_features_format == "census_pairs") {

    # Census-pairs format: one row per consecutive census pair per individual.
    # Uses plot-level census timeline so recruited/dead individuals get NA at
    # the census where they have no measurement.
    raw_traits <- query_individual_features(
      individual_ids = individuals$id_n,
      trait_ids = all_traits$id_trait,
      include_multi_census = TRUE,
      format = "long",
      issues = issues,
      census_strategy = census_strategy,
      con = con
    )

    if (nrow(raw_traits) == 0) {
      cli::cli_alert_info("No traits found to enrich")
      return(individuals)
    }

    # Build census_date from year/month/day components
    if ("census_year" %in% names(raw_traits)) {
      raw_traits <- raw_traits %>%
        dplyr::mutate(
          census_date = suppressWarnings(
            lubridate::make_date(
              year  = census_year,
              month = dplyr::coalesce(census_month, 1L),
              day   = dplyr::coalesce(census_day,   1L)
            )
          )
        ) %>%
        dplyr::select(-dplyr::any_of(c("census_typevalue", "census_day", "census_month", "census_year")))
    }

    # Restrict to census-linked rows only (id_table_liste_plots needed for plot-level pairing)
    census_raw <- raw_traits %>%
      dplyr::filter(!is.na(census_name), !is.na(id_table_liste_plots))

    if (nrow(census_raw) == 0) {
      cli::cli_alert_warning("No census-linked traits found for census_pairs format")
      return(individuals)
    }

    # Build plot-level census timeline with consecutive pairs
    plot_censuses <- census_raw %>%
      dplyr::distinct(id_table_liste_plots, census_name, census_date) %>%
      dplyr::arrange(id_table_liste_plots, census_date, census_name) %>%
      dplyr::group_by(id_table_liste_plots) %>%
      dplyr::mutate(
        census_name_1 = dplyr::lead(census_name),
        date_census1  = dplyr::lead(census_date)
      ) %>%
      dplyr::filter(!is.na(census_name_1)) %>%
      dplyr::rename(census_name_0 = census_name, date_census0 = census_date) %>%
      dplyr::ungroup()

    if (nrow(plot_censuses) == 0) {
      cli::cli_alert_warning("No consecutive census pairs found (need >= 2 censuses per plot)")
      return(individuals)
    }

    # Skeleton: all individuals × all census pairs of their plot
    skeleton <- individuals %>%
      dplyr::select(id_n, id_table_liste_plots_n) %>%
      dplyr::left_join(
        plot_censuses,
        by = c("id_table_liste_plots_n" = "id_table_liste_plots")
      ) %>%
      dplyr::filter(!is.na(census_name_0)) %>%
      dplyr::select(-id_table_liste_plots_n)

    # Pivot all numeric traits wide per individual × census
    numeric_wide <- census_raw %>%
      dplyr::filter(valuetype %in% c("numeric", "integer"), !is.na(traitvalue)) %>%
      dplyr::distinct(id_data_individuals, census_name, trait, .keep_all = TRUE) %>%
      dplyr::select(id_data_individuals, census_name, trait, traitvalue) %>%
      tidyr::pivot_wider(
        id_cols = c("id_data_individuals", "census_name"),
        names_from = "trait",
        values_from = "traitvalue",
        values_fn = dplyr::first
      )

    # Pivot all character traits wide per individual × census
    char_wide <- census_raw %>%
      dplyr::filter(valuetype %in% c("character", "ordinal", "categorical"), !is.na(traitvalue_char)) %>%
      dplyr::distinct(id_data_individuals, census_name, trait, .keep_all = TRUE) %>%
      dplyr::select(id_data_individuals, census_name, trait, traitvalue_char) %>%
      tidyr::pivot_wider(
        id_cols = c("id_data_individuals", "census_name"),
        names_from = "trait",
        values_from = "traitvalue_char",
        values_fn = dplyr::first
      )

    # Combine numeric and character wide tables
    n_num <- nrow(numeric_wide)
    n_chr <- nrow(char_wide)
    if (n_num > 0 && n_chr > 0) {
      ind_census_wide <- dplyr::full_join(numeric_wide, char_wide, by = c("id_data_individuals", "census_name"))
    } else if (n_num > 0) {
      ind_census_wide <- numeric_wide
    } else if (n_chr > 0) {
      ind_census_wide <- char_wide
    } else {
      cli::cli_alert_warning("No trait measurements found for census_pairs format")
      return(individuals)
    }

    # Create _0 and _1 versions of every trait column
    trait_cols <- setdiff(names(ind_census_wide), c("id_data_individuals", "census_name"))

    ind_census_0 <- ind_census_wide %>%
      dplyr::rename_with(~ paste0(., "_0"), .cols = dplyr::all_of(trait_cols)) %>%
      dplyr::rename(census_name_0 = census_name)

    ind_census_1 <- ind_census_wide %>%
      dplyr::rename_with(~ paste0(., "_1"), .cols = dplyr::all_of(trait_cols)) %>%
      dplyr::rename(census_name_1 = census_name)

    # Join skeleton with measurements at each census (NA where individual was absent)
    pairs <- skeleton %>%
      dplyr::left_join(ind_census_0, by = c("id_n" = "id_data_individuals", "census_name_0")) %>%
      dplyr::left_join(ind_census_1, by = c("id_n" = "id_data_individuals", "census_name_1")) %>%
      dplyr::mutate(
        time = as.numeric(difftime(date_census1, date_census0, units = "days"))
      )

    # Remove pairs where the individual has no measurements at either census
    all_trait_pair_cols <- intersect(
      c(paste0(trait_cols, "_0"), paste0(trait_cols, "_1")),
      names(pairs)
    )
    if (length(all_trait_pair_cols) > 0) {
      pairs <- pairs[rowSums(!is.na(pairs[, all_trait_pair_cols, drop = FALSE])) > 0, ]
    }

    if (nrow(pairs) == 0) {
      cli::cli_alert_warning("No census pairs with measurements found")
      return(individuals)
    }

    # Drop any existing census_date artefacts from individuals before expanding.
    # Also remove date_census_julian_N columns (1-indexed plot-level artifacts from
    # extract_census_dates): the pairs join below provides date_census0/1 which the
    # output style renames to date_census_0/1 with the correct _0/_1 convention.
    individuals <- individuals %>%
      dplyr::select(-dplyr::any_of("census_date")) %>%
      dplyr::select(-dplyr::matches("^date_census_(julian_)?\\d+$"))

    # Expand individuals: one row per (individual, census pair)
    individuals <- individuals %>%
      dplyr::left_join(pairs, by = "id_n") %>%
      dplyr::filter(!is.na(census_name_0))

    cli::cli_alert_success(
      "Census pairs format: {nrow(individuals)} row(s) across all consecutive census pairs"
    )

  } else if (individual_features_format == "wide") {

    traits_aggregated <- get_individual_aggregated_features(
      individual_ids = individuals$id_n,
      trait_ids = all_traits$id_trait,
      include_multi_census = show_multiple_census,
      issues = issues,
      con = con,
      include_measurement_ids = include_measurement_ids,
      census_strategy = census_strategy
    )

    if (nrow(traits_aggregated) > 0 && ncol(traits_aggregated) > 1) {
      individuals <- individuals %>%
        left_join(traits_aggregated, by = c('id_n' = 'id_data_individuals'))

      # Remove dead/presumed_dead individuals when selecting a single census
      if (!show_multiple_census && census_strategy %in% c("first", "last") &&
          "stem_status" %in% names(individuals)) {
        dead_ids <- individuals %>%
          dplyr::filter(stem_status %in% c("dead", "presumed_dead")) %>%
          dplyr::pull(id_n) %>%
          unique()
        if (length(dead_ids) > 0) {
          individuals <- individuals %>% dplyr::filter(!id_n %in% dead_ids)
          cli::cli_alert_info(
            "Removed {length(dead_ids)} dead/presumed_dead individual(s) at {census_strategy} census"
          )
        }
      } else if (!show_multiple_census && census_strategy %in% c("first", "last") &&
                 !"stem_status" %in% names(individuals)) {
        cli::cli_alert_warning(
          "No stem_status data found. Dead/presumed_dead individuals cannot be filtered. Consider running the stem_status workflow."
        )
      }
    } else {
      cli::cli_alert_info("No traits found to enrich")
    }

  } else {

    # Long format: one row per individual x measurement
    raw_traits <- query_individual_features(
      individual_ids = individuals$id_n,
      trait_ids = all_traits$id_trait,
      include_multi_census = show_multiple_census,
      format = "long",
      issues = issues,
      census_strategy = census_strategy,
      con = con
    )

    if (nrow(raw_traits) == 0) {
      cli::cli_alert_info("No traits found to enrich")
      return(individuals)
    }

    # Build a clean census_date column from day/month/year components when available
    if ("census_year" %in% names(raw_traits)) {
      raw_traits <- raw_traits %>%
        mutate(
          census_date = suppressWarnings(
            lubridate::make_date(
              year  = census_year,
              month = coalesce(census_month, 1L),
              day   = coalesce(census_day,   1L)
            )
          )
        ) %>%
        select(-any_of(c("census_typevalue", "census_day", "census_month", "census_year")))
    }

    # Drop columns from raw_traits that are already in individuals
    drop_from_raw <- intersect(
      names(raw_traits),
      c("id_table_liste_plots", "id_sub_plots")
    )
    raw_traits <- raw_traits %>% select(-any_of(drop_from_raw))

    # Drop plot-level census date columns from individuals: the measurement-level
    # census_date from raw_traits (NA for non-census measurements) is more
    # informative and should take precedence in long format.
    # Also remove date_census_N columns (e.g. date_census_1, date_census_2)
    # that come from the plot metadata join — they are wide-format artefacts
    # irrelevant in long format.
    individuals <- individuals %>%
      select(-any_of("census_date")) %>%
      select(-matches("^date_census_\\d+$"))

    # Join: expands individuals to one row per measurement
    individuals <- individuals %>%
      left_join(raw_traits, by = c('id_n' = 'id_data_individuals'))

    cli::cli_alert_success(
      "Long format: {nrow(individuals)} row(s) after joining measurements"
    )

    # Remove dead/presumed_dead individuals when selecting a single census
    if (!show_multiple_census && census_strategy %in% c("first", "last")) {
      stem_rows <- individuals %>%
        dplyr::filter(trait == "stem_status")
      if (nrow(stem_rows) > 0) {
        dead_ids <- stem_rows %>%
          dplyr::filter(traitvalue_char %in% c("dead", "presumed_dead")) %>%
          dplyr::pull(id_n) %>%
          unique()
        if (length(dead_ids) > 0) {
          individuals <- individuals %>% dplyr::filter(!id_n %in% dead_ids)
          cli::cli_alert_info(
            "Removed {length(dead_ids)} dead/presumed_dead individual(s) at {census_strategy} census"
          )
        }
      } else {
        cli::cli_alert_warning(
          "No stem_status data found. Dead/presumed_dead individuals cannot be filtered. Consider running the stem_status workflow."
        )
      }
    }
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
    wd_fam_level_add = wd_fam_level
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

  # Ensure concatenate is logical
  if (!is.logical(concatenate) || length(concatenate) != 1) {
    stop("concatenate must be a single logical value (TRUE or FALSE)")
  }

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
      tidyr::pivot_wider(names_from = coord3, values_from = c(typevalue, id_sub_plots)) %>%
      mutate(
        coord1 = as.numeric(coord1),
        coord2 = as.numeric(coord2),
        Xrel = coord1 - min(coord1),
        Yrel = coord2 - min(coord2)
      )

    if (nrow(grouped) == 0) next

    if (!requireNamespace("BIOMASS", quietly = TRUE))
      stop("Package 'BIOMASS' is required for GPS coordinate correction. Install it with install.packages('BIOMASS').")

    # Wrap BIOMASS function in tryCatch to handle graphics/device issues
    cor_coord <- tryCatch({
      suppressMessages(suppressWarnings(BIOMASS::correctCoordGPS(
        longlat = grouped[, c("typevalue_ddlon", "typevalue_ddlat")],
        rangeX = c(0, diff(range(grouped$coord1))),
        rangeY = c(0, diff(range(grouped$coord2))),
        coordRel = grouped %>% select(Xrel, Yrel),
        drawPlot = FALSE, rmOutliers = TRUE
      )))
    }, error = function(e) {
      # Handle graphics parameter errors from BIOMASS
      if (grepl("par.*pin|graphique.*pin", e$message, ignore.case = TRUE)) {
        cli::cli_alert_warning("BIOMASS coordinate correction encountered graphics device issue")
        cli::cli_alert_info("Proceeding with raw coordinates instead of corrected plot polygons")
        return(NULL)
      } else {
        stop(e)
      }
    })

    if (is.null(cor_coord)) {
      # If BIOMASS failed, just use raw data without polygon correction
      coordinates_data_list[[j]] <- grouped
      next
    }

    # Wrap CRS operations in tryCatch to handle PROJ database issues
    poly_plot <- tryCatch({
      st_as_sf(cor_coord$polygon) %>%
        st_set_crs(cor_coord$codeUTM) %>%
        st_transform(4326) %>%
        mutate(id_liste_plots = id_plot)
    }, error = function(e) {
      if (grepl("proj\\.db|Cannot find proj", e$message, ignore.case = TRUE)) {
        cli::cli_alert_warning("PROJ database not found - cannot transform coordinates to standard format")
        cli::cli_alert_info("Ensure PROJ library is properly installed (try: system('proj'))")
        return(NULL)
      } else {
        stop(e)
      }
    })

    if (!is.null(poly_plot)) {
      coordinates_sf_list[[j]] <- poly_plot
    }
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
        data_with_country <- dplyr::tibble(country = country)
        
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
    filter_plot_name = function(plot_name, interactive = FALSE, exact_match = FALSE) {
      if (is.null(plot_name)) return(self)

      if (interactive) {
        # Mode interactif avec .link_table
        data_with_plots <- dplyr::tibble(plot_name = plot_name)

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
        if (exact_match || length(plot_name) > 1) {
          # Recherche exacte (insensible à la casse)
          # Utilisé quand: exact_match=TRUE OU multiples noms
          condition <- glue::glue_sql(
            "LOWER(plot_name) IN ({names*})",
            names = tolower(plot_name),
            .con = private$con
          )
        } else {
          # Recherche avec pattern matching (insensible à la casse)
          # Uniquement si: exact_match=FALSE ET un seul nom
          condition <- glue::glue_sql(
            "LOWER(plot_name) LIKE LOWER({pattern})",
            pattern = paste0("%", plot_name, "%"),
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
        data_with_methods <- dplyr::tibble(method = method)
        
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
        return(dplyr::tibble())
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


#' Specimen Filter Builder
#'
#' @description
#' R6 class for building SQL queries to filter specimens with a fluent API
#'
#' @export
SpecimenFilterBuilder <- R6::R6Class(
  "SpecimenFilterBuilder",

  private = list(
    con = NULL,
    conditions = character(0),

    # Internal method to add a condition
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

    # Filter by collector name or ID
    filter_collector = function(collector = NULL, id_colnam = NULL, interactive = FALSE) {
      if (is.null(collector) && is.null(id_colnam)) return(self)

      if (!is.null(id_colnam)) {
        # Direct ID filtering (specimens table uses 'id_colnam' column)
        condition <- glue::glue_sql(
          "id_colnam IN ({ids*})",
          ids = id_colnam,
          .con = private$con
        )
        private$add_condition(condition)
        return(self)
      }

      if (interactive) {
        # Interactive mode with .link_table
        data_with_collector <- dplyr::tibble(colnam = collector)

        linked_data <- .link_table(
          data_stand = data_with_collector,
          column_searched = "colnam",
          column_name = "colnam",
          id_field = "id_table_colnam",
          id_table_name = "id_table_colnam",
          db_connection = private$con,
          table_name = "table_colnam"
        )

        collector_ids <- linked_data %>%
          filter(!is.na(id_table_colnam), id_table_colnam != 0) %>%
          pull(id_table_colnam)

        if (length(collector_ids) == 0) {
          cli::cli_alert_warning("No valid collectors selected")
          return(self)
        }

      } else {
        # Non-interactive: fuzzy match by name
        collectors_tbl <- try_open_postgres_table("table_colnam", private$con) %>%
          dplyr::collect() %>%
          dplyr::filter(tolower(colnam) %in% tolower(!!collector))

        if (nrow(collectors_tbl) == 0) {
          cli::cli_alert_warning("No collectors found matching: {paste(collector, collapse = ', ')}")
          cli::cli_alert_info("Tip: Use interactive = TRUE for fuzzy matching")
          return(self)
        }

        collector_ids <- collectors_tbl$id_table_colnam
      }

      condition <- glue::glue_sql(
        "id_colnam IN ({ids*})",
        ids = collector_ids,
        .con = private$con
      )
      private$add_condition(condition)

      return(self)
    },

    # Filter by specimen number (exact or range)
    filter_number = function(number = NULL, number_min = NULL, number_max = NULL) {
      if (is.null(number) && is.null(number_min) && is.null(number_max)) return(self)

      if (!is.null(number)) {
        # Exact number(s)
        condition <- glue::glue_sql(
          "colnbr IN ({nums*})",
          nums = number,
          .con = private$con
        )
        private$add_condition(condition)
      }

      if (!is.null(number_min)) {
        condition <- glue::glue_sql(
          "colnbr >= {num}",
          num = number_min,
          .con = private$con
        )
        private$add_condition(condition)
      }

      if (!is.null(number_max)) {
        condition <- glue::glue_sql(
          "colnbr <= {num}",
          num = number_max,
          .con = private$con
        )
        private$add_condition(condition)
      }

      return(self)
    },

    # Filter by taxonomy
    filter_taxonomy = function(genus = NULL, species = NULL, family = NULL, idtax_n = NULL) {
      # Note: taxonomy filtering requires join with taxa info, handled in build()

      if (!is.null(idtax_n)) {
        condition <- glue::glue_sql(
          "idtax_n IN ({ids*})",
          ids = idtax_n,
          .con = private$con
        )
        private$add_condition(condition)
      }

      # Store taxonomy filters for later use in join
      if (!is.null(genus)) {
        private$genus_filter <- genus
      }
      if (!is.null(species)) {
        private$species_filter <- species
      }
      if (!is.null(family)) {
        private$family_filter <- family
      }

      return(self)
    },

    # Filter by specimen ID (direct)
    filter_by_ids = function(specimen_ids) {
      if (is.null(specimen_ids) || length(specimen_ids) == 0) return(self)

      condition <- glue::glue_sql(
        "id_specimen IN ({ids*})",
        ids = specimen_ids,
        .con = private$con
      )
      private$add_condition(condition)

      return(self)
    },

    # Build the final SQL query
    build = function(operator = "AND") {
      base_query <- "SELECT * FROM specimens"

      if (length(private$conditions) > 0) {
        # Validate operator
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


#' Specimen Fetcher
#'
#' @description
#' R6 class for fetching specimen data from database
#'
#' @export
SpecimenFetcher <- R6::R6Class(
  "SpecimenFetcher",

  private = list(
    con = NULL,

    # Enrich with metadata (collector info, taxonomy)
    enrich_metadata = function(specimens) {
      # Join with collector names
      collectors_tbl <- tryCatch({
        try_open_postgres_table("table_colnam", private$con) %>%
          dplyr::select(id_table_colnam, colnam, surname, family_name) %>%
          dplyr::collect()
      }, error = function(e) {
        cli::cli_alert_warning("Could not fetch table_colnam: {e$message}")
        return(NULL)
      })

      if (!is.null(collectors_tbl) && "id_colnam" %in% colnames(specimens)) {
        specimens <- dplyr::left_join(
          specimens,
          collectors_tbl,
          by = c("id_colnam" = "id_table_colnam")
        )
      }

      return(specimens)
    }
  ),

  public = list(
    # Initialize the fetcher
    initialize = function(connection) {
      private$con <- connection
    },

    # Fetch specimens by IDs
    fetch_by_ids = function(specimen_ids) {
      if (is.null(specimen_ids) || length(specimen_ids) == 0) {
        return(dplyr::tibble())
      }

      cli::cli_alert_info("Fetching {length(specimen_ids)} specimens by ID")

      sql <- glue::glue_sql(
        "SELECT * FROM specimens WHERE id_specimen IN ({ids*})",
        ids = specimen_ids,
        .con = private$con
      )

      specimens <- func_try_fetch(con = private$con, sql = sql)

      # Enrichment
      specimens <- private$enrich_metadata(specimens)

      return(specimens)
    },

    # Fetch specimens by collector and number (exact match)
    fetch_by_collector_and_number = function(id_table_colnam, colnbr) {
      if (is.null(id_table_colnam) || is.null(colnbr)) {
        return(dplyr::tibble())
      }

      cli::cli_alert_info("Fetching specimens for collector ID {id_table_colnam}, number(s) {paste(colnbr, collapse=', ')}")

      sql <- glue::glue_sql(
        "SELECT * FROM specimens WHERE id_colnam = {id} AND colnbr IN ({nums*})",
        id = id_table_colnam,
        nums = colnbr,
        .con = private$con
      )

      specimens <- func_try_fetch(con = private$con, sql = sql)

      # Enrichment
      specimens <- private$enrich_metadata(specimens)

      return(specimens)
    },

    # Fetch with filter (SQL query from builder)
    fetch_with_filter = function(query) {
      specimens <- func_try_fetch(con = private$con, sql = query)

      if (nrow(specimens) == 0) {
        cli::cli_alert_warning("No specimens found matching filter criteria")
        return(specimens)
      }

      cli::cli_alert_success("Found {nrow(specimens)} specimens")

      # Enrichment
      specimens <- private$enrich_metadata(specimens)

      return(specimens)
    }
  )
)


# Helper functions for specimen queries

.extract_by_specimen <- function(id_specimen, con) {
  tbl <- "data_link_specimens"
  sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE id_specimen IN ({vals*})",
                        vals = id_specimen, .con = con)
  res <- func_try_fetch(con = con, sql = sql)
  if (nrow(res) == 0) stop("No individuals linked to this specimen")
  return(res$id_n)
}

#' Enrich Specimens with Taxonomy
#'
#' @description
#' Internal helper to add taxonomic information to specimens.
#' Uses table_idtax from main database (con) for synonym resolution,
#' then fetches full taxonomy from taxa database (con.taxa).
#' Note: table_idtax must be updated via update_taxa_link_table() first.
#'
#' @param specimens Data frame with specimen data including idtax_n
#' @param con Main database connection (for table_idtax)
#' @param con.taxa Taxa database connection (for table_taxa)
#'
#' @return Data frame with taxonomy columns added
#' @keywords internal
.enrich_specimens_with_taxonomy <- function(specimens, con, con.taxa = NULL) {

  if (nrow(specimens) == 0 || !"idtax_n" %in% names(specimens)) {
    return(specimens)
  }

  # Check if taxa connection is provided and valid
  if (is.null(con.taxa)) {
    cli::cli_alert_info("Skipping taxonomy enrichment (no taxa database connection provided)")
    return(specimens)
  }

  # Test the connection before using it
  conn_valid <- tryCatch({
    test_connection(con.taxa)
  }, error = function(e) {
    FALSE
  })

  if (!conn_valid) {
    cli::cli_alert_warning("Taxa connection is not valid, skipping taxonomy enrichment")
    return(specimens)
  }

  # Wrap entire enrichment process in error handler
  tryCatch({
    # Resolve synonyms from main database (table_idtax is in main DB, not taxa DB)
    diconames_id <-
      try_open_postgres_table(table = "table_idtax", con = con) %>%
      dplyr::select("idtax_n", "idtax_good_n") %>%
      dplyr::mutate(idtax_f = ifelse(is.na(.data$idtax_good_n), .data$idtax_n, .data$idtax_good_n)) %>%
      dplyr::collect()

    # Join with specimens
    specimens <- specimens %>%
      dplyr::left_join(
        diconames_id %>% dplyr::select(-dplyr::all_of("idtax_good_n")),
        by = "idtax_n"
      )

    # Get unique taxa IDs
    unique_idtax <- unique(specimens$idtax_f)
    unique_idtax <- unique_idtax[!is.na(unique_idtax)]

    if (length(unique_idtax) == 0) {
      return(specimens)
    }

    # Fetch taxonomy (add_taxa_table_taxa creates its own connection)
    taxa_info <- add_taxa_table_taxa(ids = unique_idtax)

    # if (is.data.frame(taxa_info)) {
      taxa_info <- taxa_info %>%
        dplyr::collect() %>%
        dplyr::select(-any_of(c("data_modif_d", "data_modif_m", "data_modif_y")))

      specimens <- specimens %>%
        dplyr::left_join(taxa_info, by = c("idtax_f" = "idtax_n"))
    # }

    return(specimens)

  }, error = function(e) {
    cli::cli_alert_warning("Could not enrich specimens with taxonomy: {e$message}")
    cli::cli_alert_info("Returning specimens without taxonomy enrichment")
    return(specimens)
  })
}

#' Extract Linked Individuals from Specimens
#'
#' @description
#' Internal helper to get individuals linked to specimens
#'
#' @param specimen_ids Vector of specimen IDs
#' @param con Database connection
#'
#' @return Data frame with linked individuals
#' @keywords internal
.extract_linked_individuals_from_specimens <- function(specimen_ids, con) {

  if (is.null(specimen_ids) || length(specimen_ids) == 0) {
    cli::cli_alert_warning("No specimen IDs provided")
    return(dplyr::tibble())
  }

  # Get links
  linked_ind <-
    dplyr::tbl(con, "data_link_specimens") %>%
    dplyr::filter(.data$id_specimen %in% !!specimen_ids) %>%
    dplyr::select("id_n", "id_specimen") %>%
    dplyr::collect()

  if (nrow(linked_ind) == 0) {
    cli::cli_alert_info("No individuals linked to these specimens")
    return(dplyr::tibble())
  }

  # Get individual details
  linked_ind_details <-
    query_plots(
      id_individual = linked_ind$id_n,
      extract_individuals = TRUE,
      remove_ids = FALSE,
      output_style = "minimal",  # Force minimal output to get simple data frame
      con = con
    )

  # Handle case where query_plots returns a list structure
  if (is.list(linked_ind_details) && !is.data.frame(linked_ind_details)) {
    # If it's a list, try to extract the main data
    if ("individuals" %in% names(linked_ind_details)) {
      linked_ind_details <- linked_ind_details$individuals
    } else if ("data" %in% names(linked_ind_details)) {
      linked_ind_details <- linked_ind_details$data
    } else {
      # If we can't find the data, return empty
      cli::cli_alert_warning("Could not extract individual data from query_plots result")
      return(dplyr::tibble())
    }
  }

  # Ensure it's a data frame
  if (!is.data.frame(linked_ind_details)) {
    cli::cli_alert_warning("query_plots did not return a data frame")
    return(dplyr::tibble())
  }

  # Now safe to check nrow
  if (nrow(linked_ind_details) > 0) {
    n_plots <- if ("plot_name" %in% names(linked_ind_details)) {
      length(unique(linked_ind_details$plot_name))
    } else {
      "?"
    }

    cli::cli_alert_info(
      "Found {nrow(linked_ind_details)} individuals from {n_plots} plot(s)"
    )
  } else {
    cli::cli_alert_info("No individual details found")
  }

  return(linked_ind_details)
}

.traits_to_genera_aggreg <- function(dataset, wd_fam_level_add = wd_fam_level) {
  
  list_genus <- dataset %>%
    # dplyr::filter(is.na(tax_sp_level)) %>%
    dplyr::select(id_n, tax_gen)
  
  
  ### query all taxa of genera found in dataset
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
  
  ### filter to keep only genera found in dataset (in case the query_taxa found other genus
  all_sp_genera <-
    all_sp_genera %>%
    filter(tax_gen %in% unique(list_genus$tax_gen),
           !is.na(tax_infra_level))
  
  all_val_sp <- query_taxa_traits(idtax = all_sp_genera %>%
                                        filter(!is.na(tax_esp)) %>%
                                        pull(idtax_n), 
                                  format = "long",
                                  include_synonyms = T, 
                                      add_taxa_info = T)
  
  
  if (any(class(all_val_sp$traits_categorical) == "data.frame")) {
    
    traits_idtax_char <-
      pivot_categorical_traits_generic(
      data = all_val_sp$traits_raw %>%
        dplyr::filter(valuetype == "categorical") %>%
        filter(!is.na(tax_gen)),
      id_col = "tax_gen",
      aggregation_mode = "mode",
      include_id_measures = TRUE,
      name_prefix = "taxa_"
    )
    
    
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
             any_of("tax_level"),
             all_of(colnames_traits[which(colnames_traits %in% colnames_data)]))

    dataset_pivot <-
      dataset_subset %>%
      tidyr::pivot_longer(cols = colnames_traits[which(colnames_traits %in% colnames_data)],
                   names_to = "trait") %>%
      arrange(tax_gen, trait)

    dataset_traits_pivot <-
      traits_idtax_char %>%
      select(tax_gen,
             all_of(colnames_traits)) %>%
      tidyr::pivot_longer(cols = colnames_traits[which(colnames_traits %in% colnames_data)],
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
      mutate(source = dplyr::case_when(
        "tax_level" %in% names(.) & tax_level %in% c("species", "infraspecific") ~ "species",
        "tax_level" %in% names(.) & !is.na(tax_level) ~ tax_level,
        TRUE ~ "species"
      ))


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
      select(-any_of("tax_level")) %>%
      tidyr::pivot_wider(names_from = trait,
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
             any_of("tax_level"),
             all_of(colnames_traits[which(colnames_traits %in% colnames_data)]))

    dataset_pivot <-
      dataset_subset %>%
      tidyr::pivot_longer(cols = starts_with("taxa_"),
                   names_to = "trait") %>%
      arrange(tax_fam, tax_gen, trait)

    dataset_traits_pivot <-
      traits_idtax_num %>%
      select(tax_gen,
             all_of(colnames_traits)) %>%
      tidyr::pivot_longer(cols = starts_with("taxa_"),
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
      mutate(source = dplyr::case_when(
        "tax_level" %in% names(.) & tax_level %in% c("species", "infraspecific") ~ "species",
        "tax_level" %in% names(.) & !is.na(tax_level) ~ tax_level,
        TRUE ~ "species"
      ))

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
      select(-any_of("tax_level")) %>%
      tidyr::pivot_wider(names_from = trait,
                  values_from = c(value, source))
    
    names(dataset_pivot_wider_num) <- 
      gsub("value_", "", names(dataset_pivot_wider_num))
    
    
    if (any(colnames_traits == "taxa_mean_wood_density")) {
      
      if (any(names(dataset_pivot_wider_num) == "taxa_sd_wood_density")) {
        cli::cli_alert_info("Setting wood density SD to averaged species and genus level according to BIOMASS dataset")
        
        if (!requireNamespace("BIOMASS", quietly = TRUE))
          stop("Package 'BIOMASS' (>= 2.2.4) is required for wood density SD. Install it with install.packages('BIOMASS').")
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












#' Query Specimens
#'
#' @description
#' Modern query interface for specimens with builder pattern support
#'
#' @param collector Character vector of collector names (fuzzy match)
#' @param id_colnam Integer vector of collector IDs (exact match)
#' @param number Integer vector of specimen numbers (exact match)
#' @param number_min Minimum specimen number (range query)
#' @param number_max Maximum specimen number (range query)
#' @param genus Character vector of genus names to filter
#' @param species Character vector of species names to filter
#' @param family Character vector of family names to filter
#' @param id_specimen Integer vector of specimen IDs (direct fetch)
#' @param idtax_n Integer vector of taxonomy IDs
#' @param interactive Logical, use interactive fuzzy matching for collectors
#' @param extract_linked_individuals Logical, also fetch linked individuals
#' @param subset_columns Logical, return subset of columns vs all columns
#' @param show_html Logical. If TRUE (default) and the number of returned
#'   specimens is at most \code{html_max}, the results are displayed as a
#'   transposed HTML table in the RStudio Viewer (one column per specimen)
#'   using \code{print_table()}.
#' @param html_max Integer. Maximum number of specimens for which the HTML
#'   visualisation is triggered automatically (default 20).
#' @param con Database connection (if NULL, creates new connection)
#' @param con.taxa Taxa database connection (if NULL, creates new connection)
#'
#' @return Data frame with specimen records, or list if extract_linked_individuals=TRUE
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Query by collector name
#' query_specimens(collector = "Dauby")
#'
#' # Query by collector ID and number range
#' query_specimens(id_colnam = 123, number_min = 1000, number_max = 2000)
#'
#' # Query by specimen IDs
#' query_specimens(id_specimen = c(123, 456, 789))
#'
#' # Query with linked individuals
#' result <- query_specimens(collector = "Dauby", extract_linked_individuals = TRUE)
#' result$specimens  # Specimen records
#' result$linked_individuals  # Linked individual records
#' }
query_specimens <- function(collector = NULL,
                            id_colnam = NULL,
                            number = NULL,
                            number_min = NULL,
                            number_max = NULL,
                            genus = NULL,
                            species = NULL,
                            family = NULL,
                            id_specimen = NULL,
                            idtax_n = NULL,
                            interactive = TRUE,
                            extract_linked_individuals = FALSE,
                            subset_columns = TRUE,
                            show_html = TRUE,
                            html_max = 20,
                            con = NULL,
                            con.taxa = NULL) {

  # Use provided connection or create new one
  mydb <- if (!is.null(con)) con else call.mydb()

  # Use provided taxa connection or check environment, else create new one
  if (!is.null(con.taxa)) {
    mydb.taxa <- con.taxa
  } else if (exists("mydb.taxa", envir = parent.frame()) && test_connection(get("mydb.taxa", envir = parent.frame()))) {
    mydb.taxa <- get("mydb.taxa", envir = parent.frame())
  } else {
    mydb.taxa <- call.mydb.taxa()
  }

  # Branch on whether direct ID fetch or filtered query
  if (!is.null(id_specimen)) {
    # Direct fetch by specimen IDs
    cli::cli_rule(left = "Fetching specimens by ID")

    fetcher <- SpecimenFetcher$new(mydb)
    specimens <- fetcher$fetch_by_ids(id_specimen)

  } else {
    # Build filtered query
    cli::cli_rule(left = "Building specimen filter query")

    # Check if we need any filtering at all
    needs_filtering <- !is.null(collector) || !is.null(id_colnam) ||
                       !is.null(number) || !is.null(number_min) || !is.null(number_max) ||
                       !is.null(genus) || !is.null(species) || !is.null(family) || !is.null(idtax_n)

    if (!needs_filtering) {
      # No filters - fetch all specimens (with warning)
      cli::cli_alert_warning("No filters specified - fetching all specimens")
      fetcher <- SpecimenFetcher$new(mydb)
      specimens <- try_open_postgres_table("specimens", con = mydb) %>%
        dplyr::collect()
    } else {
      # Build query with filters
      query_builder <- SpecimenFilterBuilder$new(mydb)

      # Apply collector filter
      if (!is.null(collector) || !is.null(id_colnam)) {
        query_builder <- query_builder$filter_collector(
          collector = collector,
          id_colnam = id_colnam,
          interactive = interactive
        )
      }

      # Apply number filters
      if (!is.null(number) || !is.null(number_min) || !is.null(number_max)) {
        query_builder <- query_builder$filter_number(
          number = number,
          number_min = number_min,
          number_max = number_max
        )
      }

      # Apply taxonomy filters
      if (!is.null(genus) || !is.null(species) || !is.null(family) || !is.null(idtax_n)) {
        query_builder <- query_builder$filter_taxonomy(
          genus = genus,
          species = species,
          family = family,
          idtax_n = idtax_n
        )
      }

      # Build and execute query
      query <- query_builder$build()
      specimens <- func_try_fetch(con = mydb, sql = query)
    }
  }

  # Check if we got any results
  if (nrow(specimens) == 0) {
    cli::cli_alert_warning("No specimens found matching the criteria")
    if (extract_linked_individuals) {
      return(list(specimens = specimens, linked_individuals = data.frame()))
    } else {
      return(specimens)
    }
  }

  # Enrich with collector metadata (only if not already enriched)
  if (!"colnam" %in% names(specimens)) {
    cli::cli_alert_info("Enriching with collector information...")

    collectors_tbl <- tryCatch({
      try_open_postgres_table("table_colnam", mydb) %>%
        dplyr::select(id_table_colnam, colnam, surname, family_name) %>%
        dplyr::collect()
    }, error = function(e) {
      cli::cli_alert_warning("Could not fetch collector information: {e$message}")
      NULL
    })

    if (!is.null(collectors_tbl) && nrow(collectors_tbl) > 0) {
      specimens <- specimens %>%
        dplyr::left_join(collectors_tbl, by = c("id_colnam" = "id_table_colnam"))
    } else {
      cli::cli_alert_warning("Collector information unavailable, continuing without it")
    }
  }

  # Enrich with taxonomy
  cli::cli_alert_info("Enriching with taxonomy information...")

  # Debug: Check if taxa connection is valid
  if (!is.null(mydb.taxa)) {
    taxa_conn_test <- tryCatch({
      test_connection(mydb.taxa)
    }, error = function(e) {
      cli::cli_alert_warning("Taxa connection test failed: {e$message}")
      FALSE
    })

    if (!taxa_conn_test) {
      cli::cli_alert_warning("Taxa connection is not valid, attempting to create new connection")
      mydb.taxa <- tryCatch({
        call.mydb.taxa()
      }, error = function(e) {
        cli::cli_alert_warning("Could not create taxa connection: {e$message}")
        NULL
      })
    }
  }

  specimens <- .enrich_specimens_with_taxonomy(specimens, con = mydb, con.taxa = mydb.taxa)

  # Subset columns if requested
  if (subset_columns) {
    # Select core columns that exist
    core_columns <- c(
      "id_specimen",
      "colnam",
      "colnbr",
      "suffix",
      "ddlat",
      "ddlon",
      "country",
      "locality",
      "detby",
      "detd",
      "detm",
      "dety",
      "add_col",
      "cold",
      "colm",
      "coly",
      "detvalue",
      "description",
      "idtax_n",
      "idtax_f",
      "tax_gen",
      "tax_esp",
      "tax_fam",
      "tax_infra_level",
      "tax_infra",
      "surname",
      "family_name",
      "id_tropicos",
      "id_colnam"
    )

    # Only select columns that actually exist
    specimens <- specimens %>%
      dplyr::select(dplyr::any_of(core_columns))
  }

  # Extract linked individuals if requested
  if (extract_linked_individuals) {
    cli::cli_alert_info("Extracting linked individuals...")

    linked_individuals <- .extract_linked_individuals_from_specimens(
      specimen_ids = specimens$id_specimen,
      con = mydb
    )

    # Check if we have results (handle NULL and empty data frames)
    has_results <- !is.null(linked_individuals) &&
                   is.data.frame(linked_individuals) &&
                   nrow(linked_individuals) > 0

    if (has_results) {
      cli::cli_alert_success(
        "Found {nrow(linked_individuals)} linked individuals from {length(unique(linked_individuals$plot_name))} plot(s)"
      )
    } else {
      cli::cli_alert_warning("No linked individuals found for these specimens")
      # Ensure we return an empty data frame, not NULL
      if (is.null(linked_individuals) || !is.data.frame(linked_individuals)) {
        linked_individuals <- data.frame()
      }
    }

    if (show_html && nrow(specimens) <= html_max) {
      cli::cli_alert_info("Displaying {nrow(specimens)} specimen(s) as HTML (one column per specimen)")
      print_table(specimens)
    }

    return(list(
      specimens = specimens,
      linked_individuals = linked_individuals
    ))
  }

  # HTML visualisation (transposed: one column per specimen)
  if (show_html && nrow(specimens) <= html_max) {
    cli::cli_alert_info("Displaying {nrow(specimens)} specimen(s) as HTML (one column per specimen)")
    print_table(specimens)
  }

  # Return specimens only
  return(specimens)

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

    sql <- glue::glue_sql(
      "SELECT * FROM table_colnam WHERE colnam ILIKE {pattern_like}",
      pattern_like = paste0("%", pattern, "%"),
      .con = mydb
    )

    valuetype <- func_try_fetch(con = mydb, sql = sql)
  }

  return(valuetype)

}








