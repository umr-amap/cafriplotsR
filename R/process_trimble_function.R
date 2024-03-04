
#' Process trimble data
#'
#' Process raw trimble data and provide individuals data ready to import and log file with potential problems
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param PATH string path to trimble data
#' @param plot_name string plot name to give to plot
#'
#' @return A list with two tibbles
#' @export
process_trimble_data <- function(PATH = NULL, plot_name = NULL, format = "dbf") {

  if (!any(rownames(utils::installed.packages()) == "foreign"))
    stop("foreign package needed, please install it")

  all_dirs <-
    list.dirs(path = PATH,
              full.names = FALSE, recursive = FALSE)

  all_dirs <-
    list.dirs(path = PATH,
              full.names = FALSE, recursive = FALSE)


  regexp <- "[[:digit:]]+"

  if(format == "dbf") {
    files_prop <-
      dplyr::tibble(dirs = all_dirs,
                    number = as.numeric(stringr::str_extract(all_dirs, regexp)),
                    gps = grepl("GPS", all_dirs))

    count_nbe_file <-
      files_prop %>%
      dplyr::filter(!is.na(number)) %>%
      dplyr::group_by(number) %>%
      dplyr::count()
    if (any(count_nbe_file$n != 2))
      stop("some plot do not have two directories!")

  }

  if(format == "excell") {
    files_prop <-
      dplyr::tibble(dirs = all_dirs,
                    number = as.numeric(stringr::str_extract(all_dirs, regexp)),
                    gps = grepl("GPS", all_dirs))

    count_nbe_file <-
      files_prop %>%
      dplyr::filter(!is.na(number)) %>%
      dplyr::group_by(number) %>%
      dplyr::count()

  }

  ncol_list <- list()
  nbe_herb <- list()
  all_plot_list <- list()
  all_traits_list <- list()
  logs_process <- list()
  gps_plot_list <- list()
  gps_quadrat_list <- vector(mode = 'list', length(unique(files_prop$number)))
  for (j in sort(unique(files_prop$number))) {

    logs_tb <-
      dplyr::tibble(plot_name = as.character(),
                    field = as.character(),
                    issue = as.character(),
                    id = as.character())
    # print(j)
    select_plot_dir <-
      files_prop %>%
      dplyr::filter(number == j)

    # if(format == "dbf") {
    occ_data_dir <-
      select_plot_dir %>%
      dplyr::filter(gps == FALSE) %>%
      dplyr::select(dirs) %>%
      dplyr::pull()
    # } else {
    #
    #   occ_data_dir <-
    #     select_plot_dir %>%
    #     dplyr::select(dirs) %>%
    #     dplyr::pull()
    #
    # }

    # if(format == "dbf") {
    gps_data_dir <-
      select_plot_dir %>%
      dplyr::filter(gps==TRUE) %>%
      dplyr::select(dirs) %>%
      dplyr::pull()
    # } else {
    #
    # }


    ## gps data
    data.gps <-
      list.files(path = paste0(PATH,
                               gps_data_dir, "/"), full.names = TRUE, pattern = "^[^~]")

    if (format == "dbf") {

      data.gps <- data.gps[grep(".dbf", data.gps)]
      gps_data <-
        foreign::read.dbf(file = data.gps) %>%
        dplyr::as_tibble()
    } else {

      data.gps <- data.gps[grep(".xlsx", data.gps)]
      gps_data <-
        readxl::read_excel(path = data.gps)

    }

    missig_coord <- gps_data %>%
      filter(is.na(Longitude),
             is.na(Latitude))

    if (nrow(missig_coord) >0) {
      cli::cli_alert("GPS coordinates missing for {nrow(missig_coord)} point(s)")

      gps_data <-
        gps_data %>%
        filter(!is.na(Longitude),
               !is.na(Latitude))

    }

    gps_data_subset <-
      gps_data %>%
      dplyr::mutate(date = as.character(Date)) %>%
      dplyr::mutate(date_y = as.numeric(unlist(lapply(
        strsplit(date, "-"),
        FUN = function(x)
          x[[1]]
      )))) %>%
      dplyr::mutate(date_m = as.numeric(unlist(lapply(
        strsplit(date, "-"),
        FUN = function(x)
          x[[2]]
      )))) %>%
      dplyr::mutate(date_d = as.numeric(unlist(lapply(
        strsplit(date, "-"),
        FUN = function(x)
          x[[3]]
      )))) %>%
      dplyr::select(Longitude, Latitude, date, date_y, date_m, date_d, X_theo, Y_theo)

    gps_quadrat_list[[j]] <-
      gps_data_subset %>%
      dplyr::rename(ddlat = Latitude,
                    ddlon = Longitude) %>%
      dplyr::mutate(plot_name = rep(paste0(plot_name, ifelse(j < 10, "00", "0") , j), nrow(.)))

    gps_data_subset_one_plot <-
      gps_data_subset %>%
      dplyr::mutate(plot_name = rep(paste0(plot_name, ifelse(j < 10, "00", "0") , j), nrow(.))) %>%
      dplyr::group_by(plot_name) %>%
      dplyr::summarise(
        ddlat = mean(Latitude),
        ddlon =  mean(Longitude),
        date_y = min(date_y),
        date_m = min(date_m),
        date_d = min(date_d)
      )


    gps_data_subset_one_plot <-
      gps_data_subset_one_plot %>%
      dplyr::mutate(data_provider = "AMAP") %>%
      dplyr::mutate(co_authorship = "AMAP") %>%
      dplyr::mutate(country = "Cameroun") %>%
      dplyr::mutate(area_plot = 10000) %>%
      dplyr::mutate(method = "1ha-IRD") %>%
      dplyr::mutate(locality_name = plot_name)


    gps_plot_list[[j]] <- gps_data_subset_one_plot

    ### individuals data
    print(occ_data_dir)

    if(format == "dbf") {

      data. <-
        list.files(path = paste0(PATH,
                                 occ_data_dir, "/"), full.names = TRUE, pattern = "^[^~]")
      data. <- data.[grep(".dbf", data.)]

      occ_data <-
        foreign::read.dbf(file = data.) %>%
        dplyr::as_tibble()

    } else {

      data. <-
        list.files(path = paste0(PATH,
                                 occ_data_dir, "/"), full.names = TRUE, pattern = "^[^~]")
      data. <- data.[grep(".xlsx", data.)]

      if(length(data.)==0) stop("not xlsx file in directory")

      occ_data <-
        readxl::read_excel(data.)

    }


    # check ID
    dupl_id <-
      occ_data %>%
      dplyr::filter(duplicated(.[["ID"]]))

    if(nrow(dupl_id)>0) {
      stop(paste("id duplicated", "plot", j))
      print(dupl_id)
    }

    if (occ_data$ID[1] != 1) {
      corrected_id <- vector(mode = "numeric", length = nrow(occ_data))
      for (m in 1:nrow(occ_data))
        corrected_id[m] <- occ_data$ID[m] - occ_data$ID[1] + 1

      logs_tb <- dplyr::bind_rows(
        logs_tb,
        dplyr::tibble(
          plot_name =
            paste0(plot_name, ifelse(j <
                                       10, "00", "0") , j),
          field = "ID",
          issue = "ID not starting at 1",
          id = "all"
        )
      )
      occ_data$ID <- corrected_id

      message("replacing ID by continuous ID")
    }

    ## counting number of columns
    ncol_list[[j]] <-
      ncol(occ_data)

    ## counting of herbarium field different of No
    nbe_herb[[j]] <-
      sum(occ_data$Herbarium_!="No")

    ## counting pIRD different of empty
    nbe_herb[[j]] <-
      sum(!is.na(occ_data$pIRD))

    ## quadrat identification
    if (sum(occ_data$New_quadra[!is.na(occ_data$New_quadra)] == "yes") !=
        25) {
      warning(paste(
        "25 quadrat expected but",
        sum(occ_data$New_quadra == "yes"),
        "found"
      ))
    }

    all_expected_combination <-
      expand.grid(seq(0, 80, 20),seq(0, 80, 20)) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(quadrat = paste(Var1, Var2, sep="_"))

    all_quadrat <-
      occ_data %>%
      dplyr::filter(New_quadra == "yes") %>%
      dplyr::select(ID, New_quadra, X_theo, Y_theo) %>%
      dplyr::mutate(quadrat = paste(X_theo, Y_theo, sep = "_"))


    missing_quadrats <-
      all_expected_combination %>%
      dplyr::left_join(all_quadrat, by = c("quadrat" = "quadrat")) %>%
      dplyr::filter(is.na(ID)) %>%
      dplyr::select(quadrat) %>%
      dplyr::pull()

    if(length(missing_quadrats)>0) {
      cli::cli_alert("missing quadrat")
      print(missing_quadrats)
    }

    duplicates_quadrat <-
      all_quadrat %>%
      dplyr::group_by(quadrat) %>%
      dplyr::count() %>%
      dplyr::filter(n>1)

    if (nrow(duplicates_quadrat) > 0) {
      all_quadrat %>%
        dplyr::filter(quadrat %in% duplicates_quadrat$quadrat) %>%
        print()

      ### ### ### ### ### ### ### ### ### ### ### ###
      ### correcting quadrat names ### ### ### ### ###
      ### ### ### ### ### ### ### ### ### ### ### ###
      path <- NULL
      quad1 <-
        all_quadrat %>%
        dplyr::slice(1)

      quad2 <-
        all_quadrat %>%
        dplyr::slice(2)

      if (quad1$X_theo == quad2$X_theo)
        path <- "vertical"
      if (quad1$Y_theo == quad2$Y_theo)
        path <- "horizontal"
      if (is.null(path))
        stop("quadrat progression not logical")

      Y <-
        c(seq(0, 80, 20), rev(seq(0, 80, 20)), c(seq(0, 80, 20), rev(seq(0, 80, 20))),
          seq(0, 80, 20))
      X <-
        c(rep(0, 5), rep(20, 5), rep(40, 5), rep(60, 5), rep(80, 5))

      if (path == "horizontal") {
        expected_quadrats <-
          dplyr::tibble(x = Y, y = X) %>%
          dplyr::mutate(quadrat = paste(x, y, sep = "_"))
      }
      if (path == "vertical") {
        expected_quadrats <-
          dplyr::tibble(x = X, y = Y) %>%
          dplyr::mutate(quadrat = paste(x, y, sep = "_"))
      }

      quadrat_corrected <-
        vector(mode = "character", length = nrow(all_quadrat))
      for (k in 1:nrow(all_quadrat)) {
        quadrat_found <-
          all_quadrat %>%
          dplyr::slice(k) %>%
          dplyr::pull(quadrat)
        quadrat_expected <-
          expected_quadrats %>%
          dplyr::slice(k) %>%
          dplyr::pull(quadrat)
        quadrat_found
        quadrat_expected
        if (quadrat_found != quadrat_expected) {
          message(paste(
            "replacing observed quadrat",
            quadrat_found,
            "by",
            quadrat_expected
          ))
          quadrat_corrected[k] <- quadrat_expected

          ids <-
            all_quadrat %>%
            dplyr::slice(k) %>% dplyr::pull(ID)
          ids <- paste(ids, collapse = ", ")

          logs_tb <- dplyr::bind_rows(
            logs_tb,
            dplyr::tibble(
              plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
              field = "New_quadra",
              issue = "error in quadrat automatically corrected",
              id = ids
            )
          )

        } else
          (
            quadrat_corrected[k] <- quadrat_found
          )
      }

      if (length(unique(quadrat_corrected)) != 25)
        stop("still not 25 unique quadrat name")

      all_quadrat <-
        all_quadrat %>%
        tibble::add_column(quadrat_corrected = quadrat_corrected)

      occ_data <-
        occ_data %>%
        dplyr::left_join(all_quadrat %>%
                           dplyr::select(ID, quadrat_corrected),
                         by = c("ID" = "ID"))

    }

    if (any(colnames(all_quadrat) == "quadrat_corrected"))
      all_quadrat <-
      all_quadrat %>%
      dplyr::rename(quadrat_good_format = quadrat_corrected)

    # add_column(quadrat_good_format = unlist(lapply(strsplit(x = all_quadrat$quadrat_corrected, split = "_"), FUN = function(x) paste("(", x[1], ";", x[2], ")",sep=""))))

    if (!any(colnames(all_quadrat) == "quadrat_corrected"))
      all_quadrat <-
      all_quadrat %>%
      dplyr::mutate(quadrat_good_format = quadrat)
    # add_column(quadrat_good_format = unlist(lapply(strsplit(x = all_quadrat$quadrat, split = "_"), FUN = function(x) paste("(", x[1], ";", x[2], ")",sep=""))))

    quadrats <-
      vector(mode = "character", length = nrow(occ_data))
    for (k in 1:(nrow(all_quadrat))) {
      if (k == nrow(all_quadrat)) {
        quadrats[all_quadrat$ID[k]:nrow(occ_data)] <-
          all_quadrat$quadrat_good_format[k]
      } else {
        quadrats[all_quadrat$ID[k]:all_quadrat$ID[k + 1]] <-
          all_quadrat$quadrat_good_format[k]
        # print(all_quadrat$quadrat_good_format[k])
      }
    }

    occ_data <-
      occ_data %>%
      tibble::add_column(quadrat = quadrats)

    ### adding plot name
    occ_data <-
      occ_data %>%
      tibble::add_column(plot_name = rep(paste0(plot_name, ifelse(j < 10, "00", "0") , j),
                                         nrow(occ_data)))

    ### rename and check dbh
    occ_data <-
      occ_data %>%
      dplyr::rename(dbh = DBH)
    if(any(occ_data$dbh>400)) stop("unrealistic dbh value")

    if (any(occ_data$dbh < 10)) {
      message("dbh value lower than 10")

      ids <-
        occ_data %>%
        dplyr::filter(dbh < 10) %>% dplyr::pull(ID)
      ids <- paste(ids, collapse = ", ")

      logs_tb <-
        logs_tb %>%
        dplyr::bind_rows(
          dplyr::tibble(
            plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
            field = "dbh",
            issue = "dbh lower than 10",
            id = as.character(ids)
          )
        )

    }

    if(any(is.na(occ_data$dbh))) stop("missing dbh value")

    ### rename and check DBH_height
    if (!any(colnames(occ_data) == "dbh_height")) {
      occ_data <-
        occ_data %>%
        dplyr::rename(dbh_height = DBH_height)
    }

    if (any(is.na(occ_data$dbh_height))) {
      print(occ_data %>%
              dplyr::filter(is.na(dbh_height)))
      message("missing dbh_height value")

      ids <-
        occ_data %>%
        dplyr::filter(is.na(dbh_height)) %>%
        dplyr::pull(ID)
      ids <- paste(ids, collapse = ", ")

      logs_tb <-
        logs_tb %>%
        dplyr::bind_rows(
          dplyr::tibble(
            plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
            field = "dbh_height",
            issue = "missing values",
            id = ids
          )
        )

    }
    if (any(occ_data$dbh_height[!is.na(occ_data$dbh_height)] > 20))
      stop("unrealistic dbh_height value")

    ### rename and check original taxa name
    occ_data <-
      occ_data %>%
      dplyr::mutate(Species = as.character(Species),
                    Genus = as.character(Genus),
                    Family = as.character(Family),
                    Identif_co = as.character(Identif_co)) %>%
      dplyr::mutate(original_tax_name =
                      ifelse(!is.na(Species), Species, ifelse(!is.na(Genus), Genus, Family))) %>%
      dplyr::mutate(original_tax_name =
                      ifelse(!is.na(original_tax_name), original_tax_name, Identif_co)) %>%
      dplyr::mutate(original_tax_name = tidyr::replace_na(original_tax_name, "indet."))

    ### multistem check
    multi_stem_ind <-
      occ_data %>%
      dplyr::select(ID, Rainfor_Tr, original_tax_name, Observatio) %>%
      dplyr::filter(grepl("multiple", Rainfor_Tr) | grepl("multiple", Observatio))

    ## identify and check coherence of multi stem
    list_multiple_stem <- list()
    if (nrow(multi_stem_ind) > 0) {
      for (k in 1:nrow(multi_stem_ind)) {
        # print(k)
        if (length(list_multiple_stem) > 0) {
          ids <- unlist(lapply(list_multiple_stem, function(w)
            w$ID))
        } else{
          ids <- 0
        }

        if (!multi_stem_ind$ID[k] %in% ids) {
          ### screnning of multiple stem : check for ID tag continuity and taxa similarity. Stop when one is not true
          q <- 1
          found <- TRUE
          add_tag <- NULL
          while (found) {
            ## if ID tag continuity is true
            if (multi_stem_ind$ID[k] + q == multi_stem_ind$ID[ifelse((k +
                                                                      q) > nrow(multi_stem_ind),
                                                                     nrow(multi_stem_ind),
                                                                     k + q)]) {
              q <- q + 1

              multi_stem_chunk <-
                multi_stem_ind %>%
                dplyr::slice(k:(k + q - 1))

              if (length(unique(multi_stem_chunk$original_tax_name)) > 1) {
                dist. <-
                  .pairwise_string_similarity(string_vector =
                                                multi_stem_chunk$original_tax_name[!is.na(multi_stem_chunk$original_tax_name)])
                dist.[is.na(dist.)] <- 1
              } else{
                dist. <- 1
              }

              if (any(dist. < 0.8)) {
                found <- FALSE
                q <- q - 1

                if (multi_stem_ind %>%
                    dplyr::slice(k:(k + q - 1)) %>%
                    nrow() == 1) {
                  message("multiple stem chunk with very different identification")

                  ### putting into log the problems
                  ids <-
                    multi_stem_chunk %>%
                    dplyr::pull(ID)

                  ids <- paste(ids, collapse = ", ")

                  logs_tb <-
                    logs_tb %>%
                    dplyr::bind_rows(
                      dplyr::tibble(
                        plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
                        field = "multistem",
                        issue = "different identification",
                        id = ids
                      )
                    )


                }
              }

            } else{
              if (q == 1) {
                ## check if following Tag is same identification and has not been put into multiple stem
                next_tag <-
                  occ_data %>%
                  dplyr::filter(ID == multi_stem_ind$ID[k] + 1) %>%
                  dplyr::select(ID, Rainfor_Tr, original_tax_name)

                dist. <-
                  .pairwise_string_similarity(
                    string_vector = c(
                      next_tag$original_tax_name,
                      multi_stem_ind$original_tax_name[k]
                    )
                  )
                dist.[is.na(dist.)] <- 1

                if (dist. < 0.8) {
                  print(multi_stem_ind %>%
                          slice(k))
                  print(next_tag)
                  message(
                    "expected following tag in multiple stem and next individual of different taxa"
                  )

                  found <- FALSE

                  ### putting into log the problems
                  ids <-
                    multi_stem_ind %>%
                    dplyr::slice(k) %>%
                    dplyr::bind_rows(next_tag) %>%
                    dplyr::pull(ID)

                  ids <- paste(ids, collapse = ", ")

                  logs_tb <-
                    logs_tb %>%
                    dplyr::bind_rows(
                      dplyr::tibble(
                        plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
                        field = "multistem",
                        issue = "expected following tag in multiple stem and next individual of different taxa",
                        id = as.character(ids)
                      )
                    )

                } else{
                  add_tag <-
                    next_tag
                  found <- FALSE
                }

              } else{
                found <- FALSE
              }
            }
          }

          multi_stem_chunk <-
            multi_stem_ind %>%
            dplyr::slice(k:(k + q - 1))

          if (!is.null(add_tag))
            multi_stem_chunk <-
            dplyr::bind_rows(multi_stem_chunk, add_tag)

          if (nrow(multi_stem_chunk) > 1) {
            list_multiple_stem[[length(list_multiple_stem) + 1]] <-
              multi_stem_chunk
          }
        }
      }
    }

    multi_tiges_id <-
      vector(mode = "numeric", length = nrow(occ_data))
    if (length(list_multiple_stem) > 0) {
      for (k in 1:length(list_multiple_stem)) {
        multi_stem_chunk <-
          list_multiple_stem[[k]]
        for (d in 2:nrow(multi_stem_chunk))
          multi_tiges_id[occ_data$ID == multi_stem_chunk$ID[d]] <-
            multi_stem_chunk$ID[1]
      }
    }

    multi_tiges_id <-
      replace(multi_tiges_id, multi_tiges_id == 0, NA)

    occ_data <-
      occ_data %>%
      tibble::add_column(multi_tiges_id = multi_tiges_id)

    ### rename observations
    occ_data <-
      occ_data %>%
      dplyr::rename(observation = Observatio)

    ### rename TAG number
    occ_data <-
      occ_data %>%
      dplyr::rename(id = ID)

    if(any(occ_data$Total_heig > 0)) {

      cli::cli_alert_info("tree height data processing")

      occ_data <-
        occ_data %>%
        mutate(tree_height = Total_heig) %>%
        mutate(tree_height = replace(tree_height, tree_height == 0, NA))

      # stop("height values different of 0")

    }

    if(any(occ_data$H_branch > 0)) stop("branch values different of 0")

    ## subsetting traits datasets
    ## collection date
    missing_coll_date <-
      occ_data %>%
      dplyr::filter(is.na(Date))

    if (nrow(missing_coll_date) > 0) {
      message(paste("missing collection date for", missing_coll_date$id))
      for (i in 1:nrow(missing_coll_date)) {
        date_for_miss <-
          occ_data %>%
          dplyr::filter(id == missing_coll_date$id[i] - 1) %>%
          dplyr::pull(Date)
        occ_data[occ_data$id == missing_coll_date$id[i], "Date"] <-
          date_for_miss
      }
    }

    occ_data <-
      occ_data %>%
      dplyr::mutate(date_char = as.character(Date)) %>%
      dplyr::mutate(year = as.numeric(unlist(lapply(strsplit(date_char, "-"), function(x)
        x[[1]])))) %>%
      dplyr::mutate(month = as.numeric(unlist(lapply(strsplit(date_char, "-"), function(x)
        x[[2]])))) %>%
      dplyr::mutate(day = as.numeric(unlist(lapply(strsplit(date_char, "-"), function(x)
        x[[3]]))))

    if (any(occ_data$day > 31) |
        any(occ_data$day < 1))
      stop("day out of range")
    if (any(occ_data$month > 12) |
        any(occ_data$month < 1))
      stop("month out of range")
    if (any(occ_data$year > lubridate::year(Sys.Date())) |
        any(occ_data$year < 1900))
      stop("year out of range")

    traits_data <-
      occ_data %>%
      dplyr::select(plot_name, id, dbh, original_tax_name, year, month, day, dbh_height, Total_heig, Crown_spre, observation)

    all_traits_list[[j]] <-
      traits_data

    all_plot_list[[j]] <-
      occ_data %>%
      dplyr::select(plot_name, id, original_tax_name, quadrat, multi_tiges_id)

    logs_process[[j]] <- logs_tb

  }

  final_output <-
    dplyr::bind_rows(all_plot_list)

  final_output_traits <-
    dplyr::bind_rows(all_traits_list)

  final_logs <-
    dplyr::bind_rows(logs_process)

  final_gps <-
    dplyr::bind_rows(gps_plot_list)

  final_gps_quadrat <-
    dplyr::bind_rows(gps_quadrat_list)

  writexl::write_xlsx(final_gps, paste("final_meta_data_", plot_name,".xlsx", sep = ""))

  writexl::write_xlsx(final_output, paste("final_process_plot_", plot_name,".xlsx", sep = ""))

  writexl::write_xlsx(final_output_traits, paste("final_process_traits_", plot_name,".xlsx", sep = ""))

  writexl::write_xlsx(final_logs, paste("final_process_plot_logs_", plot_name,".xlsx", sep = ""))

  writexl::write_xlsx(final_gps_quadrat, paste("final_gps_quadrat_", plot_name,".xlsx", sep = ""))


  return(list(final_output = final_output,
              final_output_traits = final_output_traits,
              final_gps = final_gps,
              final_logs = final_logs,
              final_gps_quadrat = final_gps_quadrat))
}




#' Internal function
#'
#' Compute pairwise string similarity
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param string_vector string vector
.pairwise_string_similarity <- function(string_vector) {
  dist. <- c()
  for (h in 1:(length(string_vector)-1))
    for (d in (h+1):length(string_vector))
      dist. <- c(dist.,
                 RecordLinkage::levenshteinSim(str1 =  tolower(string_vector[h]),
                                               str2 = tolower(string_vector[d])))
  return(dist.)
}
