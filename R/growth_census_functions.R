# Growth and Census Analysis Functions
#
# This file contains functions for computing tree growth rates and analyzing
# census data from forest plot inventories. These functions handle multiple
# census periods, outlier detection, and mortality/recruitment calculations.
#
# Main functions:
# - .split_censuses(): Split plot data into individual census periods
# - .time_diff(): Calculate time intervals between census pairs
# - .trim.growth(): Identify and flag problematic growth measurements
# - growth_computing(): Main function for computing growth rates across multiple censuses
#
# Dependencies: dplyr, rlang, stringr, date, cli
# Adapted from CTFS R Package: http://ctfs.si.edu/Public/CTFSRPackage/
#' Split plot data into census
#'
#' split plot data into a list where each element is a census
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param meta tibble output of query_plot with no export individuals
#' @param dataset tibble output of query_plot with export individuals
#'
#' @importFrom rlang parse_expr
#'
#' @return A list with as many tibble as census
#' @export
.split_censuses <- function(meta, dataset) {

  split_census <- list()
  for (i in 1:nrow(meta)) {
    select_census <-
      dataset %>%
      dplyr::select(dplyr::contains(paste0("census_", i)),
                    dplyr::contains(paste0("date_census_", i)),
                    dplyr::contains(paste0("date_census_julian_", i)),
             id_n,
             tag,
             plot_name)

    stem_census <- paste0("stem_diameter_census_", i)
    stem_census_enquo <-
      rlang::parse_expr(rlang::quo_name(rlang::enquo(stem_census)))

    select_census <-
      select_census %>%
      dplyr::filter(!is.na(!!stem_census_enquo),
             !!stem_census_enquo>0)

    split_census[[i]] <- select_census
    names(split_census)[[i]] <- paste0("census_", meta$typevalue[i])
  }

  return(split_census)

}



#' Add time difference in number of days for two census
#'
#' Add
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param census1 tibble with first census
#' @param census2 tibble with second census
#'
#' @return tibble joining both census and with a added column indicating time intervall
#' @export
.time_diff <- function(census1, census2) {

  ## renaming first and second census to 1 and 2
  select_census_1 <-
    census1 %>%
    dplyr::select(dplyr::contains("date_census_julian")) %>%
    colnames() %>%
    strsplit(split = "_") %>%
    unlist()
  select_census_1 <- select_census_1[length(select_census_1)]

  census1 <-
    census1 %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_1))),
                          funs(stringr::str_replace(., paste0("_", select_census_1), "_1")))

  select_census_2 <-
    census2 %>%
    dplyr::select(dplyr::contains("date_census_julian")) %>%
    colnames() %>%
    strsplit(split = "_") %>%
    unlist()
  select_census_2 <- select_census_2[length(select_census_2)]

  census2 <-
    census2 %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_2))),
                          funs(stringr::str_replace(., paste0("_", select_census_2), "_2")))

  ## excluding NA stem diameter (new recruits and dead stems)
  joined_census <-
    dplyr::left_join(census1, census2, by=c("id_n"="id_n")) %>%
    dplyr::filter(!is.na(stem_diameter_census_1), !is.na(stem_diameter_census_2))

  joined_census <-
    joined_census %>%
    dplyr::mutate(time_diff =  (date_census_julian_2 - date_census_julian_1 )/365.25)

  joined_census
}



#' Identify potential errors for estimating growth
#'
#' Add a column of individuals to be excluded because of potential errors
#' Adapted from http://ctfs.si.edu/Public/CTFSRPackage/
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param censuses tibble with first census
#' @param slope numeric see http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth
#' @param intercept numeric see http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth
#' @param err.limit integer any measure of second diameter higher than err.limit standard deviation below the first measure will be excluded
#' @param maxgrow numeric any growth in mm/year higher than maxgrow will be excluded
#' @param mindbh numeric minimum diameter in mm for excluding measures
#'
#' @return tibble joining both census and with a added column indicating logical whether inidividual should be excluded
#' @export
.trim.growth <- function(censuses,
                         slope = 0.006214,
                         intercept = 0.9036,
                         err.limit = 4,
                         maxgrow = 75,
                         # pomcut = 0.05,
                         mindbh = 100
                         # dbhunit = "cm",
) {

  # if(dbhunit=='cm') intercept=intercept/10

  censuses <-
    censuses %>%
    dplyr::mutate(dbh_mm_census1 = stem_diameter_census_1*10) %>%
    dplyr::mutate(dbh_mm_census2 = stem_diameter_census_2*10)


  ## get the standard deviation of linear model
  stdev.dbh1 <- slope * censuses$dbh_mm_census1 + intercept
  growth <- (censuses$dbh_mm_census2- censuses$dbh_mm_census1) / censuses$time_diff
  bad.neggrow <- which(censuses$dbh_mm_census2 <= (censuses$dbh_mm_census1 - err.limit *
                                                     stdev.dbh1))

  bad.posgrow <- which(growth > maxgrow)
  # homdiff <- abs(as.numeric(cens2$hom) - as.numeric(cens1$hom)) / as.numeric(cens1$hom)

  accept <- rep(TRUE, nrow(censuses))
  # accept[homdiff > pomcut] <- FALSE
  accept[bad.neggrow] <- FALSE
  accept[bad.posgrow] <- FALSE
  accept[is.na(growth)] <- FALSE
  # if (exclude.stem.change) {
  #   accept[cens1$stemID != cens2$stemID] <- FALSE
  # }
  accept[censuses$dbh_mm_census1 < mindbh] <- FALSE

  accept[is.na(censuses$dbh_mm_census1) | is.na(censuses$dbh_mm_census2) | censuses$dbh_mm_census2 <= 0] <- FALSE

  censuses <-
    censuses %>%
    mutate(accepted_growth = accept)
  return(censuses)
}



#' Growth computing for multiple census
#'
#' Growth computing for multiple census
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param dataset tibble, ouput of query_plots
#' @param metadata tibble, ouput of query_plots
#' @param mindbh numeric see http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth
#' @param err.limit integer any measure of second diameter higher than err.limit standard deviation below the first measure will be excluded
#' @param maxgrow numeric any growth in mm/year higher than maxgrow will be excluded
#' @param method string either 'I' or 'E'
#' @param export_ind_growth whether growth per individuals should be exported
#'
#' @importFrom date as.date date.ddmmmyy
#'
#' @return tibble
#'
#' @details
#' growthrate is in
#'
#'
#' @export
growth_computing <- function(dataset,
                             metadata,
                             mindbh = NULL,
                             err.limit = 4, # any measure of second diameter higher than err.limit standard deviation below the first measure will be excluded
                             maxgrow = 75, # any growth (mm/year) higher than maxgrow will be excluded
                             method = "I",
                             export_ind_growth = TRUE) {

  if (!any(rownames(utils::installed.packages()) == "date")) {
    stop("date package needed, please install it")
  }


  if (!is.list(metadata)) {
    stop("metadata should be a list obtained with the function query_plot with show_multiple_census = T")
  }
  
  if (!is.list(dataset)) {
    stop("dataset should be a list obtained with the function query_plot with show_multiple_census = T and extract_individuals = T")
  }
  

  if (!any(metadata[[2]]$typevalue > 1))
    stop("Only one census recorded for all selected plots")

  if (!any(names(dataset$extract) == "id_table_liste_plots_n"))
    stop("id_table_liste_plots_n column missing in dataset, make sure you obtain dataset with remove_ids = F")

  all_ids_plot <-
    metadata[[2]] %>%
    dplyr::filter(typevalue >1) %>%
    dplyr::distinct(id_table_liste_plots) %>%
    dplyr::pull()

  plot_names <-
    metadata[[2]] %>%
    dplyr::filter(typevalue >1) %>%
    dplyr::distinct(id_table_liste_plots, plot_name) %>%
    dplyr::pull(plot_name)

  cli::cli_alert_info(paste("Multiple census recorded for", length(all_ids_plot), "plots"))

  full_results <-
    full_results_ind <-
    full_results_mortality <-
    vector('list', length = length(all_ids_plot)*10)

  for (plot in 1:length(all_ids_plot)) {

    cli::cli_alert_info(plot_names[plot])

    selected_dataset <-
      dataset$extract %>%
      dplyr::filter(id_table_liste_plots_n == all_ids_plot[plot])

    selected_metadata_census <-
      metadata[[2]] %>%
      dplyr::filter(id_table_liste_plots == all_ids_plot[plot])

    skipped_census_missing_dates <-
      selected_metadata_census %>%
      dplyr::filter(is.na(year) | is.na(month))

    not_run <- FALSE
    if (nrow(skipped_census_missing_dates) > 0) {
      message(paste("Census excluded because missing year and/or month"))
      print(skipped_census_missing_dates)
      not_run <- TRUE

    }

    if (length(unique(selected_metadata_census$year)) == 1 &
        length(unique(selected_metadata_census$month)) == 1 &
        length(unique(selected_metadata_census$day)) == 1) {

      cli::cli_alert_danger("Dates do not differ between censuses")
      not_run <- TRUE

    }

    selected_metadata_census <-
      selected_metadata_census %>%
      dplyr::arrange(typevalue) %>%
      dplyr::mutate(date =
                      paste(ifelse(!is.na(month),
                                   month, 1), # if day is missing, by default 1
                            ifelse(!is.na(day),
                                   day, 1), # if month is missing, by default 1
                            ifelse(!is.na(year),
                                   year, ""),
                            sep = "/")) %>%
      dplyr::mutate(date_julian = date::as.date(date))

    arranged_sub_plots <-
      selected_metadata_census %>%
      dplyr::arrange(typevalue) %>%
      dplyr::select(date_julian, id_sub_plots) %>%
      dplyr::arrange(date_julian) %>%
      dplyr::pull(id_sub_plots)

    if (!paste(arranged_sub_plots, collapse = "_") ==
        paste(selected_metadata_census$id_sub_plots, collapse = "_")) {
      message(paste("Dates are not in chronological order"))
      not_run <- TRUE
    }

    selected_metadata_census <-
      selected_metadata_census %>%
      dplyr::filter(!is.na(year) & !is.na(month))

    if (!not_run) {
      for (i in 1:(nrow(selected_metadata_census) - 1)) {

        splitted_census <-
          .split_censuses(meta = selected_metadata_census,
                          dataset = selected_dataset)


        ## renaming first and second census to 1 and 2
        select_census_1 <-
          splitted_census[[i]] %>%
          dplyr::select(dplyr::contains('stem_diameter_census')) %>%
          colnames() %>%
          strsplit(split = "_") %>%
          unlist()

        select_census_1 <-
          select_census_1[length(select_census_1)]

        # splitted_census[[i]] <-
        #   splitted_census[[i]] %>%
        #   dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_1))),
        #                    funs(stringr::str_replace(., paste0("_", select_census_1), "_1")))

        splitted_census[[i]] <-
          splitted_census[[i]] %>%
          dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_1))),
                           list(~ stringr::str_replace(., paste0("_", select_census_1), "_1")))


        select_census_2 <-
          splitted_census[[i+1]] %>%
          dplyr::select(dplyr::contains('stem_diameter_census')) %>%
          colnames() %>%
          strsplit(split = "_") %>%
          unlist()

        select_census_2 <- select_census_2[length(select_census_2)]

        # splitted_census[[i+1]] <-
        #   splitted_census[[i+1]] %>%
        #   dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_2))),
        #                    funs(stringr::str_replace(., paste0("_", select_census_2), "_2")))

        splitted_census[[i+1]] <-
          splitted_census[[i+1]] %>%
          dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_2))),
                           list(~ stringr::str_replace(., paste0("_", select_census_2), "_2")))


        ### detecting dead stems by filtering either 0 stem diameter or NA
        deads <-
          splitted_census[[i]] %>%
          dplyr::select(id_n, stem_diameter_census_1) %>%
          dplyr::left_join(
            splitted_census[[i + 1]] %>%
              dplyr::select(id_n, stem_diameter_census_2),
            by = c("id_n" = "id_n")
          ) %>%
          dplyr::filter(is.na(stem_diameter_census_2) |
                          stem_diameter_census_2 == 0)

        recruits <-
          splitted_census[[i + 1]] %>%
          dplyr::select(id_n, stem_diameter_census_2) %>%
          dplyr::left_join(
            splitted_census[[i]] %>%
              dplyr::select(id_n, stem_diameter_census_1),
            by = c("id_n" = "id_n")
          ) %>%
          dplyr::filter(is.na(stem_diameter_census_1) |
                          stem_diameter_census_1 == 0)

        censuses <- .time_diff(census1 = splitted_census[[i]], census2 = splitted_census[[i+1]])

        censuses <-
          .trim.growth(
            censuses = censuses,
            err.limit = err.limit,
            maxgrow = maxgrow,
            mindbh = mindbh
          )

        size1 <- censuses$dbh_mm_census1
        size2 <- censuses$dbh_mm_census2

        if (method == "I") {

          growthrate <- (size2 - size1) / censuses$time_diff

        } else if (method == "E") {

          growthrate <- (log(size2) - log(size1)) / censuses$time_diff

        }

        # setting NA to values considered to be problematic
        growthrate[!censuses$accepted_growth] <- NA

        censuses <-
          censuses %>%
          mutate(growthrate = growthrate)

        if (export_ind_growth) {

          ind_growth <-
            censuses %>%
            left_join(
              selected_dataset %>%
                dplyr::select(
                  id_n,
                  idtax_individual_f,
                  tax_sp_level,
                  tax_fam,
                  tax_gen,
                  tax_esp,
                  plot_name,
                  # sous_plot_name,
                  tag,
                  id_table_liste_plots_n
                ),
              by = c("id_n" = "id_n")
            ) %>%
            dplyr::relocate(plot_name,
                            # sous_plot_name,
                            tag,
                            tax_sp_level,
                            tax_fam,
                            tax_gen,
                            tax_esp,
                            .before = date_census_1)

          full_results_ind[[length(full_results_ind[unlist(lapply(full_results_ind, function(x) !is.null(x)))]) + 1]] <-
            ind_growth

        }


        ### growth computation

        ## number of stems at the outset (first census) excluding "exclude" stems
        N_outset <-
          splitted_census[[i]] %>%
          dplyr::left_join(censuses %>%
                      dplyr::select(id_n, growthrate) %>%
                      dplyr::filter(is.na(growthrate)) %>%
                      dplyr::mutate(growthrate = stringr::str_replace_na(growthrate, "no")) %>%
                      dplyr::rename(exclude = growthrate) %>%
                        distinct(),
                    by = c("id_n" = "id_n")) %>%
          dplyr::filter(is.na(exclude)) %>%
          nrow()

        N_survivor <-
          N_outset - nrow(deads)

        averaged_time_diff <-
          mean(censuses$time_diff)

        mortality_rate <-
          (log(N_outset)-log(N_survivor))/averaged_time_diff



        result <-
          list(
          plot_name = selected_metadata_census$plot_name[1],
          censuses = paste(names(splitted_census[i]), names(splitted_census[i+1]), collapse = "_", sep = "_"),
          growthrate = mean(growthrate, na.rm = TRUE),
          nbe_dead = nrow(deads),
          dbhmean_deads = ifelse(nrow(deads)>0, mean(deads$stem_diameter_census_1), NA),
          mortality_rate = mortality_rate,
          nbe_recruits = nrow(recruits),
          dbhmean_recruits = ifelse(nrow(recruits) > 0, mean(recruits$stem_diameter_census_2), NA),
          dbhmax_recruits = ifelse(nrow(recruits) > 0, max(recruits$stem_diameter_census_2), NA),
          N_survivor = N_survivor,
          N_outset = N_outset,
          N_excluded = length(growthrate[is.na(growthrate)]),
          sd_growthrate = sd(growthrate, na.rm = TRUE),
          dbhmean1 = mean(censuses$dbh_mm_census1[censuses$accepted_growth], na.rm = TRUE),
          dbhmean2 = mean(censuses$dbh_mm_census2[censuses$accepted_growth], na.rm = TRUE),
          nbe_days_intervall = mean(censuses$time_diff[censuses$accepted_growth], na.rm = TRUE)*365.25,
          date1 = date::date.ddmmmyy(mean(censuses$date_census_julian_1[censuses$accepted_growth],
                                          na.rm = TRUE)),
          date2 = date::date.ddmmmyy(mean(censuses$date_census_julian_2[censuses$accepted_growth],
                                          na.rm = TRUE))
        )

        full_results[[length(full_results[unlist(lapply(full_results, function(x) !is.null(x)))]) + 1]] <-
          result

        deads <-  deads %>%
          left_join(
            selected_dataset %>%
              dplyr::select(
                id_n,
                idtax_individual_f,
                tax_sp_level,
                tax_fam,
                tax_gen,
                tax_esp,
                plot_name,
                # sous_plot_name,
                tag,
                id_table_liste_plots_n
              ),
            by = c("id_n" = "id_n")
          )


        full_results_mortality[[length(full_results_mortality[unlist(lapply(full_results_mortality, function(x) !is.null(x)))]) + 1]] <-
          deads

      }
    }else{

      cli::cli_alert_warning(paste(
        "No growth analysis for",
        selected_metadata_census$plot_name[1],
        "because dates are not conform"
      ))
    }
  }

  full_results <-
    full_results[unlist(lapply(full_results, function(x) !is.null(x)))]

  full_results_ind <-
    full_results_ind[unlist(lapply(full_results_ind, function(x) !is.null(x)))]

  full_results_mortality <-
    full_results_mortality[unlist(lapply(full_results_mortality, function(x) !is.null(x)))]

  if(!export_ind_growth)
    return(plot_results = bind_rows(lapply(full_results,
                                           FUN = function(x) bind_rows(x))),
           mortality = full_results_mortality)

  if(export_ind_growth)
    return(list(plot_results = bind_rows(lapply(full_results,
                                                FUN = function(x) bind_rows(x))),
                ind_results = full_results_ind,
                mortality = full_results_mortality))

}
