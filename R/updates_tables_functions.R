

# update_plot_data <- function(team_lead = NULL,
#                              plot_name = NULL,
#                              country = NULL,
#                              method = NULL,
#                              id_table_plot = NULL,
#                              new_plot_name = NULL,
#                              new_team_leader = NULL,
#                              new_principal_investigator = NULL,
#                              new_data_manager = NULL,
#                              new_additional_people = NULL,
#                              new_country = NULL,
#                              new_ddlat = NULL,
#                              new_ddlon = NULL,
#                              new_elevation = NULL,
#                              new_method = NULL,
#                              new_province = NULL,
#                              new_data_provider = NULL,
#                              new_locality_name = NULL,
#                              new_topo_comment = NULL,
#                              add_backup = TRUE,
#                              ask_before_update = TRUE) {
# 
#   mydb <- call.mydb()
# 
#   if (is.null(id_table_plot)) {
#     quer_plots <-
#       query_plots(
#         plot_name = plot_name,
#         country = country,
#         method = method,
#         remove_ids = FALSE
#       )
#   } else {
# 
#     quer_plots <-
#       query_plots(id_plot = id_table_plot, remove_ids = FALSE)
# 
#   }
# 
#   if (nrow(quer_plots) == 1) {
# 
#     if (!is.null(new_team_leader) | !is.null(new_principal_investigator) | !is.null(new_data_manager) | !is.null(new_additional_people) | !is.null(new_data_provider)) {
# 
#       if (!is.null(new_team_leader)) {new_colnam <- new_team_leader; colname_type = "team_leader"}
#       if (!is.null(new_principal_investigator)) {new_colnam <- new_principal_investigator; colname_type = "principal_investigator"}
#       if (!is.null(new_data_manager)) {new_colnam <- new_data_manager; colname_type = "data_manager"}
#       if (!is.null(new_additional_people)) {new_colnam <- new_additional_people; colname_type = "additional_people"}
#       if (!is.null(new_data_provider)) {stop("Implement data_provider update")}
#       
#       all_new_colnam <- tibble(colnam = new_colnam) %>%
#         tidyr::separate_rows(colnam, sep = ",") %>% pull()
# 
# 
#       new_id_colnam <-
#         .link_table(
#           data_stand = tibble(colnam = all_new_colnam),
#           column_searched = "colnam",
#           column_name = "colnam",
#           id_field = "id_colnam",
#           id_table_name = "id_table_colnam",
#           db_connection = mydb,
#           table_name = "table_colnam"
#         )
# 
#       subplots_list <-
#         query_subplots(ids_plots = quer_plots$id_liste_plots, verbose = FALSE)
# 
#       if (any(!is.na(subplots_list$all_subplots))) {
#         existing_data <-
#           subplots_list$all_subplots %>%
#           filter(type == colname_type)
#       } else {
#         existing_data <- tibble()
#       }
# 
#       if (nrow(existing_data) > 0) {
# 
#         if (any(existing_data$typevalue == new_id_colnam$id_colnam)) {
#           cli::cli_alert_warning(glue::glue("{colname_type} information already linked to plot"))
#           conf <- FALSE
#         } else {
# 
#           cli::cli_alert_info(glue::glue("{colname_type} information already available for this plot"))
#           print(subplots_list$all_subplot_pivot %>% pull(colname_type))
# 
#           # conf <- askYesNo(msg = glue::glue("Add {colname_type} information ?"))
#           conf <- choose_prompt(message =  glue::glue("Add {colname_type} information ?"))
#         }
# 
#       } else {
# 
#         conf <- TRUE
# 
#       }
# 
#       if (conf) {
# 
#         if (ask_before_update) {
# 
#           choose_prompt(message =  "Confirm these modifications?")
#         } else {
# 
#           Q <- TRUE
# 
#         }
# 
#         if (Q) {
# 
#           add_subplot_features(new_data = tibble({{colname_type}} := new_id_colnam$id_colnam,
#                                                  id_liste_plot = quer_plots$id_liste_plots),
#                                id_plot_name = "id_liste_plot",
#                                subplottype_field = colname_type,
#                                ask_before_update = FALSE,
#                                verbose = TRUE,
#                                add_data = TRUE)
# 
#           cli::cli_alert_success(glue::glue("added {colname_type} information"))
#         }
# 
#       }
# 
# 
# 
# 
#     } else {
# 
#       if(!is.null(new_method)) {
# 
# 
#         id_new_method <- .link_table(
#           data_stand = tibble(method = new_method),
#           column_searched = "method",
#           column_name = "method",
#           id_field = "id_method",
#           id_table_name = "id_method",
#           db_connection = mydb,
#           table_name = "methodslist"
#         )
# 
#         id_new_method <- id_new_method$id_method
# 
#       }
# 
#       if(!is.null(new_country)) {
# 
#         new_id_country <-
#           .link_table(
#             data_stand = tibble(colnam = new_country),
#             column_searched = "colnam",
#             column_name = "country",
#             id_field = "id_country",
#             id_table_name = "id_country",
#             db_connection = mydb,
#             table_name = "table_countries"
#           )
# 
#         new_id_country <-
#           new_id_country$id_country
# 
#       } else {
#         new_id_country <- NULL
#       }
# 
#       new_values <-
#         dplyr::tibble(
#           plot_name = ifelse(!is.null(new_plot_name), new_plot_name, quer_plots$plot_name),
#           id_method  = ifelse(!is.null(new_method), id_new_method, quer_plots$id_method),
#            id_country = ifelse(
#             !is.null(new_id_country),
#             new_id_country,
#             quer_plots$id_country
#           ),
#           ddlat = ifelse(!is.null(new_ddlat), new_ddlat, quer_plots$ddlat),
#           ddlon = ifelse(!is.null(new_ddlon), new_ddlon, quer_plots$ddlon),
#           elevation = ifelse(!is.null(new_elevation),
#                              new_elevation, quer_plots$elevation),
#           province = ifelse(!is.null(new_province),
#                             new_province, quer_plots$province),
#           locality_name = ifelse(!is.null(new_locality_name),
#                                  new_locality_name, quer_plots$locality_name),
#           topo_comment = ifelse(!is.null(new_topo_comment),
#                                 new_topo_comment, quer_plots$topo_comment)
#         )
# 
#       comp_res <- .comp_print_vec(vec_1 = quer_plots  %>%
#                                     dplyr::select(!!colnames(new_values)),
#                                   vec_2 = new_values)
# 
#       print(comp_res$comp_html)
# 
#       comp_values <- comp_res$comp_tb
# 
# 
#       if (!is.vector(comp_values)) {
# 
#         if (any(comp_values %>% pull())) {
# 
#           modif <- TRUE
# 
#         } else {
#           modif <- FALSE
#         }
# 
#       } else {
# 
#         if (comp_values) {
# 
#           modif <- TRUE
# 
#         } else {
#           modif <- FALSE
#         }
# 
#       }
# 
#       if (modif) {
# 
# 
#         if (ask_before_update) {
# 
#           Q <- choose_prompt(message =  "Confirm these modifications?")
# 
#         } else {
# 
#           Q <- TRUE
# 
#         }
# 
# 
#         if(Q) {
# 
#           modif_types <-
#             paste0(names(comp_values), sep="__")
# 
#           if(add_backup) {
# 
#             colnames_plots <-
#               dplyr::tbl(mydb, "followup_updates_liste_plots")  %>%
#               dplyr::select(-date_modified, -modif_type, -id_fol_up_plots) %>%
#               # dplyr::top_n(1) %>%
#               dplyr::collect(n=1) %>%
#               colnames()
# 
#             quer_plots <-
#               quer_plots %>%
#               dplyr::select(dplyr::one_of(colnames_plots))
# 
#             quer_plots <-
#               quer_plots %>%
#               tibble::add_column(date_modified=Sys.Date()) %>%
#               tibble::add_column(modif_type=paste0(modif_types, collapse = ""))
# 
#             DBI::dbWriteTable(mydb, "followup_updates_liste_plots", quer_plots, append = TRUE, row.names = FALSE)
#           }
# 
#           rs <-
#             DBI::dbSendQuery(mydb, statement="UPDATE data_liste_plots SET plot_name = $2, id_method = $3, id_country = $4, ddlat = $5, ddlon = $6, elevation = $7, province = $8, locality_name = $9, topo_comment = $10, data_modif_d=$11, data_modif_m=$12, data_modif_y=$13 WHERE id_liste_plots = $1",
#                              params=list(quer_plots$id_liste_plots, # $1
#                                          new_values$plot_name, # $2
#                                          new_values$id_method, # $3
#                                          # new_values$id_colnam, # $4
#                                          new_values$id_country, # $5
#                                          new_values$ddlat, # $6
#                                          new_values$ddlon, # $7
#                                          new_values$elevation, # $8
#                                          new_values$province, # $9
#                                          # new_values$data_provider, # $10,
#                                          new_values$locality_name, # $11
#                                          new_values$topo_comment, # $12
#                                          lubridate::day(Sys.Date()), # $13
#                                          lubridate::month(Sys.Date()), # $14
#                                          lubridate::year(Sys.Date()))) # $15
# 
#           # if(show_results) print(dbFetch(rs))
#           DBI::dbClearResult(rs)
# 
#         }
# 
#       } else {
# 
#         cli::cli_alert_info("no update because no values differents from the entry")
# 
#       }
# 
#     }
# 
#   } else {
# 
#     if (nrow(quer_plots) > 1)
#       cli::cli_alert_info("More than 1 plot selected. Select only one.")
# 
#     if (nrow(quer_plots) == 0)
#       cli::cli_alert_info("No plot to be update found.")
#   }
# }




# update_plot_data_batch <- function(new_data,
#                                    col_names_select = NULL,
#                                    col_names_corresp = NULL,
#                                    id_col = 1,
#                                    launch_update = FALSE,
#                                    add_backup = TRUE) {
# 
#   if(!exists("mydb")) call.mydb()
# 
#   if (is.null(col_names_select)) {
#     col_names_select <- names(new_data)
#     cli::cli_alert_info("col_names_select is set as all names of new_data")
#   }
# 
#   if (is.null(col_names_corresp)) {
#     col_names_corresp <- col_names_select
#     cli::cli_alert_info("col_names_corresp is set to names of col_names_select (it should be names of columns of data_liste_plots)")
#   }
# 
#   all_colnames_ind <-
#     dplyr::tbl(mydb, "data_liste_plots") %>%
#     colnames()
# 
#   if(length(col_names_select) != length(col_names_corresp))
#     stop("col_names_select and col_names_corresp should have same length")
# 
#   for (i in 1:length(col_names_select))
#     if(!any(col_names_select[i] == colnames(new_data)))
#       stop(paste(col_names_select[i], "not found in new_data"))
# 
#   for (i in 1:length(col_names_corresp))
#     if(!any(col_names_corresp[i] == all_colnames_ind))
#       stop(paste(col_names_corresp[i], "not found in data_liste_plots, check others tables, subplots features should be updated in data_liste_sub_plots table"))
# 
#   id_db <- col_names_corresp[id_col]
# 
#   if(!any(id_db == c("id_liste_plots"))) stop("id for matching should be id_liste_plots")
# 
#   new_data_renamed <-
#     .rename_data(dataset = new_data,
#                  col_old = col_names_select,
#                  col_new = col_names_corresp)
# 
# 
#   output_matches <- .find_ids(dataset = new_data_renamed,
#                               col_new = col_names_corresp,
#                               id_col_nbr = id_col,
#                               type_data = "plot_data")
# 
#   matches_all <-
#     output_matches[[2]]
# 
#   for (i in 1:length(matches_all)) {
# 
#     field <- names(matches_all)[i]
#     var_new <- paste0(field, "_new")
#     matches <- matches_all[[i]]
# 
#     if(launch_update & nrow(matches) > 0) {
# 
#       matches <-
#         matches %>%
#         dplyr::select(id, dplyr::contains("_new"))
#       matches <-
#         .add_modif_field(matches)
# 
#       all_id_match <- dplyr::pull(dplyr::select(matches, id))
# 
#       if(add_backup) {
# 
#         quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))
# 
#         all_rows_to_be_updated <-
#           dplyr::tbl(mydb, "data_liste_plots") %>%
#           dplyr::filter(!!quo_var_id %in% all_id_match) %>%
#           dplyr::collect()
# 
#         colnames_plots <-
#           dplyr::tbl(mydb, "followup_updates_liste_plots")  %>%
#           dplyr::select(-date_modified, -modif_type, -id_fol_up_plots) %>%
#           dplyr::collect() %>%
#           dplyr::top_n(1) %>%
#           colnames()
# 
#         all_rows_to_be_updated <-
#           all_rows_to_be_updated %>%
#           dplyr::select(dplyr::one_of(colnames_plots))
# 
#         all_rows_to_be_updated <-
#           all_rows_to_be_updated %>%
#           mutate(date_modified = Sys.Date()) %>%
#           mutate(modif_type = field)
# 
#         print(all_rows_to_be_updated %>%
#                 dplyr::select(modif_type, date_modified))
# 
#         DBI::dbWriteTable(mydb, "followup_updates_liste_plots",
#                           all_rows_to_be_updated,
#                           append = TRUE,
#                           row.names = FALSE)
#       }
# 
#       # if(any(names(matches) == "idtax_n_new"))
#       #   matches <-
#       #   matches %>%
#       #   dplyr::mutate(idtax_n_new == as.integer(idtax_n_new))
# 
#       ## create a temporary table with new data
#       DBI::dbWriteTable(mydb, "temp_table", matches,
#                         overwrite=T, fileEncoding = "UTF-8", row.names=F)
# 
#       query_up <-
#         paste0("UPDATE data_liste_plots t1 SET (",field,", data_modif_d, data_modif_m, data_modif_y) = (t2.",var_new, ", t2.date_modif_d, t2.date_modif_m, t2.date_modif_y) FROM temp_table t2 WHERE t1.", id_db," = t2.id")
# 
#       rs <-
#         DBI::dbSendStatement(mydb, query_up)
# 
#       cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
#       rs@sql
#       DBI::dbClearResult(rs)
# 
#       cli::cli_alert_success("Successful update")
# 
#     } else{
# 
#       if (launch_update & nrow(matches) == 0)
#         cat("\n No new values found")
# 
#       if (!launch_update)
#         cli::cli_alert_danger("No update because launch_update is FALSE")
# 
#     }
#   }
#   return(matches_all)
# }


# update_subplots_table <- function(subplots_id = NULL,
#                                   new_id_type_sub_plot = NULL,
#                                   new_typevalue = NULL,
#                                   new_year = NULL,
#                                   new_month = NULL,
#                                   new_day = NULL,
#                                   new_colnam = NULL,
#                                   new_add_people = NULL,
#                                   ask_before_update = TRUE,
#                                   add_backup = TRUE,
#                                   show_results = TRUE) {
# 
#   if(!exists("mydb")) call.mydb()
# 
#   if(all(is.null(c(subplots_id))))
#     stop("\n Provide subplots_id to update")
# 
#   ### checking if at least one modification is asked
#   new_vals <- c(new_id_type_sub_plot,
#                 new_typevalue,
#                 new_year,
#                 new_month,
#                 new_day,
#                 new_colnam,
#                 new_add_people)
# 
#   if (!any(!is.null(new_vals)))
#     stop("\n No new values to be updated.")
# 
#   ### querying for entries to be modified
#   query_subplots <-
#     dplyr::tbl(mydb, "data_liste_sub_plots") %>%
#     dplyr::filter(id_sub_plots == subplots_id) %>%
#     dplyr::collect()
# 
#   print(query_subplots %>% as.data.frame())
#   if (nrow(query_subplots) > 1)
#     stop("more than one subplots selected, select one")
#   if (nrow(query_subplots) == 0)
#     stop("no subplots selected, select one")
# 
#   if(!is.null(new_colnam)) {
#     # new_id_colnam <-
#     #   .link_colnam(data_stand = tibble(colnam = new_colnam),
#     #                collector_field = 1)
# 
#     new_id_colnam <- .link_table(
#       data_stand = tibble(colnam = colnam),
#       column_searched = "colnam",
#       column_name = "colnam",
#       id_field = "id_colnam",
#       id_table_name = "id_table_colnam",
#       db_connection = mydb,
#       table_name = "table_colnam"
#     )
# 
#     new_id_colnam <-
#       new_id_colnam$id_colnam
# 
#   }else{
#     new_id_colnam <- NULL
#   }
# 
#   modif_types <-
#     vector(mode = "character", length = nrow(query_subplots))
# 
#   new_vals <-
#     dplyr::tibble(id_type_sub_plot = ifelse(!is.null(new_id_type_sub_plot), as.numeric(new_id_type_sub_plot),
#                                             query_subplots$id_type_sub_plot),
#                   year = ifelse(!is.null(new_year), as.numeric(new_year),
#                                 query_subplots$year),
#                   month = ifelse(!is.null(new_month), as.numeric(new_month),
#                                  query_subplots$month),
#                   day = ifelse(!is.null(new_day), as.numeric(new_day),
#                                query_subplots$day),
#                   typevalue = ifelse(!is.null(new_typevalue), as.numeric(new_typevalue),
#                                      query_subplots$typevalue),
#                   id_colnam = ifelse(!is.null(new_id_colnam), as.numeric(new_id_colnam),
#                                      query_subplots$id_colnam),
#                   additional_people = ifelse(!is.null(new_add_people), new_add_people,
#                                              query_subplots$additional_people))
# 
# 
# 
#   # new_vals <-
#   #   new_vals %>%
#   #   replace(., is.na(.), -9999)
# 
#   sel_query_subplots <-
#     dplyr::bind_rows(
#       new_vals,
#       query_subplots %>%
#         dplyr::select(id_type_sub_plot, year, month, day, typevalue, id_colnam, additional_people)
#     )
# 
# 
#   sel_query_subplots <- replace_NA(df = sel_query_subplots)
# 
#   # sel_query_subplots <-
#   #   sel_query_subplots %>%
#   #   mutate_if(is.character, ~ tidyr::replace_na(., "-9999")) %>%
#   #   mutate_if(is.numeric, ~ tidyr::replace_na(., -9999))
# 
#   comp_vals <-
#     apply(
#       sel_query_subplots,
#       MARGIN = 2,
#       FUN = function(x)
#         x[1] != x[2:length(x)]
#     )
# 
#   # if(!is.null(nrow(comp_vals))) {
#   #   query_trait <-
#   #     query_trait[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
#   #   comp_vals <-
#   #     apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))
#   # }else{
#   #   query_trait <- query_trait
#   # }
# 
#   if(any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]
# 
#   modif_types[1:length(modif_types)] <-
#     paste(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")
# 
#   # if(!any(comp_vals)) stop("No update performed because no values are different.")
# 
#   if(any(comp_vals)) {
# 
#     cat(paste("\n Number of rows selected to be updated :", nrow(query_subplots), "\n"))
# 
#     if(ask_before_update) {
# 
#       sel_query_subplots %>%
#         dplyr::select(!!names(comp_vals)) %>%
#         dplyr::select(which(comp_vals)) %>%
#         print()
# 
#       Q <-
#         choose_prompt(message =  "Do you confirm you want to update these rows for selected fields?")
#     }else{
#       Q <- TRUE
#     }
# 
#     if(Q) {
# 
#       if(add_backup) {
#         message("no back up for this table yet")
#         # query_trait <-
#         #   query_trait %>%
#         #   tibble::add_column(date_modified=Sys.Date()) %>%
#         #   tibble::add_column(modif_type=modif_types)
#         #
#         #
#         # DBI::dbWriteTable(mydb, "followup_updates_diconames", query_tax, append = TRUE, row.names = FALSE)
# 
#       }
# 
#       query_subplots <-
#         query_subplots %>%
#         dplyr::select(-date_modif_d, -date_modif_m, -date_modif_y)
# 
#       query_subplots <-
#         .add_modif_field(query_subplots)
# 
#       rs <-
#         DBI::dbSendQuery(mydb,
#                          statement = "UPDATE data_liste_sub_plots SET id_type_sub_plot=$2, year=$3, month=$4, day=$5, typevalue=$6, date_modif_d=$7, date_modif_m=$8, date_modif_y=$9, id_colnam=$10, additional_people=$11 WHERE id_sub_plots = $1",
#                          params = list(query_subplots$id_sub_plots, # $1
#                                        rep(ifelse(!is.null(new_id_type_sub_plot), as.numeric(new_id_type_sub_plot),
#                                                   query_subplots$id_type_sub_plot), nrow(query_subplots)), # $2
#                                        rep(ifelse(!is.null(new_year), as.numeric(new_year),
#                                                   query_subplots$year), nrow(query_subplots)), # $3
#                                        rep(ifelse(!is.null(new_month), as.numeric(new_month),
#                                                   query_subplots$month), nrow(query_subplots)), # $4
#                                        rep(ifelse(!is.null(new_day), as.numeric(new_day),
#                                                   query_subplots$day), nrow(query_subplots)), # $5
#                                        rep(ifelse(!is.null(new_typevalue), as.numeric(new_typevalue),
#                                                   query_subplots$typevalue), nrow(query_subplots)), # $6
#                                        rep(query_subplots$date_modif_d, nrow(query_subplots)), # $7
#                                        rep(query_subplots$date_modif_m, nrow(query_subplots)), # $8
#                                        rep(query_subplots$date_modif_y, nrow(query_subplots)), # $9
#                                        rep(ifelse(!is.null(new_id_colnam), as.numeric(new_id_colnam),
#                                                   query_subplots$id_colnam), nrow(query_subplots)), # $10
#                                        rep(ifelse(!is.null(new_add_people), as.character(new_add_people),
#                                                   query_subplots$additional_people), nrow(query_subplots))) # 11
# 
#         )
# 
#       DBI::dbClearResult(rs)
# 
#       rs <-
#         DBI::dbSendQuery(mydb, statement="SELECT *FROM data_liste_sub_plots WHERE id_sub_plots = $1",
#                          params=list(query_subplots$id_sub_plots))
#       if(show_results) print(DBI::dbFetch(rs))
#       DBI::dbClearResult(rs)
# 
#     }
#   }else{
# 
#     if(!any(comp_vals)) print("No update performed because no values are different.")
#   }
# 
# 
# }




# update_subplot_data_batch <- function(new_data,
#                                       col_names_select = NULL,
#                                       col_names_corresp = NULL,
#                                       id_col = 1,
#                                       launch_update = FALSE,
#                                       add_backup = TRUE) {
# 
#   if(!exists("mydb")) call.mydb()
# 
#   if (is.null(col_names_select)) {
#     col_names_select <- names(new_data)
#     cli::cli_alert_info("col_names_select is set as all names of new_data")
#   }
# 
#   if (is.null(col_names_corresp)) {
#     col_names_corresp <- col_names_select
#     cli::cli_alert_info("col_names_corresp is set to names of col_names_select (it should be names of columns of data_liste_sub_plots")
#   }
# 
#   all_colnames_ind <-
#     dplyr::tbl(mydb, "data_liste_sub_plots") %>%
#     colnames()
# 
#   if(length(col_names_select) != length(col_names_corresp))
#     stop("col_names_select and col_names_corresp should have same length")
# 
#   for (i in 1:length(col_names_select))
#     if(!any(col_names_select[i] == colnames(new_data)))
#       stop(paste(col_names_select[i], "not found in new_data"))
# 
#   for (i in 1:length(col_names_corresp))
#     if(!any(col_names_corresp[i] == all_colnames_ind))
#       stop(paste(col_names_corresp[i], "not found in data_liste_sub_plots, check others tables"))
# 
#   id_db <- col_names_corresp[id_col]
# 
#   if(!any(id_db == c("id_sub_plots"))) stop("id for matching should be id_sub_plots")
# 
#   new_data_renamed <-
#     .rename_data(dataset = new_data,
#                  col_old = col_names_select,
#                  col_new = col_names_corresp)
# 
# 
#   output_matches <- .find_ids(dataset = new_data_renamed,
#                               col_new = col_names_corresp,
#                               id_col_nbr = id_col,
#                               type_data = "data_liste_sub_plots")
# 
#   matches_all <-
#     output_matches[[2]]
# 
#   for (i in 1:length(matches_all)) {
# 
#     field <- names(matches_all)[i]
#     var_new <- paste0(field, "_new")
#     matches <- matches_all[[i]]
# 
#     if(launch_update & nrow(matches) > 0) {
# 
#       matches <-
#         matches %>%
#         dplyr::select(id, dplyr::contains("_new"))
# 
#       matches <-
#         .add_modif_field(matches)
# 
#       all_id_match <- dplyr::pull(dplyr::select(matches, id))
# 
#       if(add_backup) {
# 
#         quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))
# 
#         all_rows_to_be_updated <-
#           dplyr::tbl(mydb, "data_liste_sub_plots") %>%
#           dplyr::filter(!!quo_var_id %in% all_id_match) %>%
#           dplyr::collect()
# 
#         colnames_plots <-
#           dplyr::tbl(mydb, "followup_updates_data_liste_sub_plots")  %>%
#           dplyr::select(-date_modified, -modif_type, -id_fol_up_sub_plots) %>%
#           dplyr::collect() %>%
#           dplyr::top_n(1) %>%
#           colnames()
# 
#         all_rows_to_be_updated <-
#           all_rows_to_be_updated %>%
#           dplyr::select(dplyr::one_of(colnames_plots))
# 
#         all_rows_to_be_updated <-
#           all_rows_to_be_updated %>%
#           mutate(date_modified = Sys.Date()) %>%
#           mutate(modif_type = field)
# 
#         print(all_rows_to_be_updated %>%
#                 dplyr::select(modif_type, date_modified))
# 
#         DBI::dbWriteTable(mydb, "followup_updates_data_liste_sub_plots",
#                           all_rows_to_be_updated,
#                           append = TRUE,
#                           row.names = FALSE)
#       }
# 
# 
#       field_ <- rlang::parse_expr(quo_name(rlang::enquo(field)))
# 
#       matches <- matches %>%
#         rename(!!field_ := paste0(field, "_new"))
# 
#       ## create a temporary table with new data
#       DBI::dbWriteTable(mydb, "temp_table", matches,
#                         overwrite=T, fileEncoding = "UTF-8", row.names=F)
# 
#       query_up <-
#         paste0("UPDATE data_liste_sub_plots t1 SET (",field,", date_modif_d, date_modif_m, date_modif_y) = (t2.",field, ", t2.date_modif_d, t2.date_modif_m, t2.date_modif_y) FROM temp_table t2 WHERE t1.", id_db," = t2.id")
# 
#       rs <-
#         DBI::dbSendStatement(mydb, query_up)
# 
#       cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
#       rs@sql
#       DBI::dbClearResult(rs)
# 
#       cli::cli_alert_success("Successful update")
# 
#     } else{
# 
#       if (launch_update & nrow(matches) == 0)
#         cat("\n No new values found")
# 
#       if (!launch_update)
#         cli::cli_alert_danger("No update because launch_update is FALSE")
# 
#     }
#   }
#   return(matches_all)
# }





# update_individuals <- function(new_data,
#                                col_names_select = NULL,
#                                col_names_corresp = NULL,
#                                id_col = 1,
#                                launch_update = FALSE,
#                                add_backup = TRUE) {
# 
#   if(!exists("mydb")) call.mydb()
# 
# 
#   if (is.null(col_names_select)) {
#     col_names_select <- names(new_data)
#     cli::cli_alert_info("col_names_select is set as all names of new_data")
#   }
# 
#   if (is.null(col_names_corresp)) {
#     col_names_corresp <- col_names_select
#     cli::cli_alert_info("col_names_corresp is set to names of col_names_select (it should be names of columns of data_individuals")
#   }
# 
#   all_colnames_ind <-
#     dplyr::tbl(mydb, "data_individuals") %>%
#     dplyr::select(-dbh, -liane, -tree_height, -branch_height, -branchlet_height, -crown_spread, -dbh_height) %>%
#     colnames()
# 
#   if(length(col_names_select) != length(col_names_corresp))
#     stop("col_names_select and col_names_corresp should have same length")
# 
#   for (i in 1:length(col_names_select))
#     if(!any(col_names_select[i] == colnames(new_data)))
#       stop(paste(col_names_select[i], "not found in new_data"))
# 
#   for (i in 1:length(col_names_corresp))
#     if(!any(col_names_corresp[i] == all_colnames_ind))
#       stop(paste(col_names_corresp[i], "not found in data_individuals, check others tables, observation/traits should be updated in traits_measurements table"))
# 
#   id_db <- col_names_corresp[id_col]
# 
#   if(!any(id_db == c("id_old", "id_n"))) stop("id for matching should be one of id_old or id_n")
# 
#   new_data_renamed <-
#     .rename_data(dataset = new_data,
#                  col_old = col_names_select,
#                  col_new = col_names_corresp)
# 
#   # new_data_renamed <-
#   #   new_data %>%
#   #   dplyr::rename_at(dplyr::vars(col_names_select[-id_col]), ~ col_names_corresp[-id_col])
# 
#   # dataset = new_data_renamed
#   # col_new = col_names_corresp
#   # id_col_nbr = id_col
#   # type_data = "individuals"
# 
#   output_matches <- .find_ids(dataset = new_data_renamed,
#                               col_new = col_names_corresp,
#                               id_col_nbr = id_col,
#                               type_data = "individuals")
# 
#   matches_all <-
#     output_matches[[2]]
# 
#   for (i in 1:length(matches_all)) {
# 
#     field <- names(matches_all)[i]
#     var_new <- paste0(field, "_new")
#     matches <- matches_all[[i]]
# 
#     if(launch_update & nrow(matches) > 0) {
# 
#       matches <-
#         matches %>%
#         dplyr::select(id, dplyr::contains("_new"))
#       matches <-
#         .add_modif_field(matches)
# 
#       all_id_match <- dplyr::pull(dplyr::select(matches, id))
# 
#       if(add_backup) {
# 
#         quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))
# 
#         all_rows_to_be_updated <-
#           dplyr::tbl(mydb, "data_individuals") %>%
#           dplyr::filter(!!quo_var_id %in% all_id_match) %>%
#           dplyr::collect()
# 
#         colnames_plots <-
#           dplyr::tbl(mydb, "followup_updates_individuals")  %>%
#           dplyr::select(-date_modified, -modif_type, -id_fol_up_ind) %>%
#           dplyr::collect() %>%
#           dplyr::top_n(1) %>%
#           colnames()
# 
#         all_rows_to_be_updated <-
#           all_rows_to_be_updated %>%
#           dplyr::select(dplyr::one_of(colnames_plots))
# 
#         all_rows_to_be_updated <-
#           all_rows_to_be_updated %>%
#           tibble::add_column(date_modified=Sys.Date()) %>%
#           tibble::add_column(modif_type=field)
# 
#         print(all_rows_to_be_updated %>%
#                 dplyr::select(modif_type, date_modified))
# 
#         DBI::dbWriteTable(mydb, "followup_updates_individuals",
#                           all_rows_to_be_updated, append = TRUE, row.names = FALSE)
#       }
# 
#       if(any(names(matches) == "idtax_n_new"))
#         matches <-
#         matches %>%
#         dplyr::mutate(idtax_n_new == as.integer(idtax_n_new))
# 
#       ## create a temporary table with new data
#       DBI::dbWriteTable(mydb, "temp_table", matches,
#                         overwrite=T, fileEncoding = "UTF-8", row.names=F)
# 
#       query_up <-
#         paste0("UPDATE data_individuals t1 SET (",field,", data_modif_d, data_modif_m, data_modif_y) = (t2.",var_new, ", t2.date_modif_d, t2.date_modif_m, t2.date_modif_y) FROM temp_table t2 WHERE t1.", id_db," = t2.id")
# 
#       rs <-
#         DBI::dbSendStatement(mydb, query_up)
# 
#       cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
#       rs@sql
#       DBI::dbClearResult(rs)
# 
#       cli::cli_alert_success("Successful update")
# 
#     } else{
# 
#       if (launch_update & nrow(matches) == 0)
#         cat("\n No new values found")
# 
#       if (!launch_update)
#         cli::cli_alert_danger("No update because launch_update is FALSE")
# 
#     }
#   }
#   return(matches_all)
# }


#' Update specimens table
#'
#' Update specimens table
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param colnam string collector name
#' @param number integer specimen number
#' @param id_speci integer id of specimen
#' @param id_colnam integer id of collector name
#' @param new_genus string new genus name
#' @param new_species string new species name
#' @param new_family string new family name
#' @param id_new_taxa integer id of the new taxa
#' @param new_detd integer day of identification
#' @param new_detm logical if you want to see previous modification of the entry - useful to see previous identification for example
#' @param new_dety logical if labels should be produced
#' @param new_detby string if labels are produced title of the label
#' @param new_detvalue string if labels are produced name of the rtf file
#' @param add_backup string if labels are produced name of the rtf file
#' @param show_results string if labels are produced name of the rtf file
#' @param only_new_ident string if labels are produced name of the rtf file
#'
#' @return A tibble
#' @export
update_ident_specimens <- function(colnam = NULL,
                                   number = NULL,
                                   id_speci = NULL,
                                   id_colnam = NULL,
                                   new_genus = NULL,
                                   new_species = NULL,
                                   new_family = NULL,
                                   id_new_taxa = NULL,
                                   new_detd = NULL,
                                   new_detm = NULL,
                                   new_dety = NULL,
                                   new_detby = NULL,
                                   new_detvalue = NULL,
                                   new_colnbr = NULL,
                                   new_suffix = NULL,
                                   add_backup = TRUE,
                                   show_results = TRUE,
                                   only_new_ident = TRUE,
                                   ask_before_update = TRUE) {

  mydb <- call.mydb()

  if(is.null(id_speci)) {

    if(!is.numeric(number)) stop("number specimen is not a numeric, it must be numeric")

    if (is.null(id_colnam)) {
      
      new_data_renamed <- .link_table(
        data_stand = tibble(collector = colnam),
        column_searched = "collector",
        column_name = "colnam",
        id_field = "id_colnam",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )
      
      id_colnam <- new_data_renamed$id_colnam
      
    }
    
    queried_speci <-
      query_specimens(id_colnam = id_colnam,
                      number = number, subset_columns = FALSE)
    

  } else {

    queried_speci <-
      query_specimens(id_search = id_speci, subset_columns = FALSE)

  }

  if (nrow(queried_speci) > 0) {

    print(
      queried_speci %>%
        dplyr::select(
          family_name,
          surname,
          colnbr,
          suffix,
          detd,
          detm,
          dety,
          detby,
          cold,
          colm,
          coly,
          country,
          id_specimen
        )
    )


    if (nrow(queried_speci) == 1) {
      nbr_queried_speci_ok <- TRUE
    } else{
      nbr_queried_speci_ok <- FALSE
    }

    if(nbr_queried_speci_ok)
      modif_types <- vector(mode = "character", length = nrow(queried_speci))

    if (is.null(id_new_taxa)) {

      if (!is.null(new_genus) | !is.null(new_family) | !is.null(new_species)) {

        query_new_taxa <-
          query_taxa(genus = new_genus,
                     species = new_species,
                     family = new_family,
                     check_synonymy = F,
                     extract_traits = F,
                     class = NULL,
                     verbose = FALSE)

      } else {

        query_new_taxa <- tibble(1)
      }

    } else {

      query_new_taxa <-
        query_taxa(
          ids = id_new_taxa,
          check_synonymy = F,
          extract_traits = F,
          class = NULL,
          verbose = FALSE
        )

    }

    if (nrow(query_new_taxa) == 1) {

      nbr_new_taxa_ok <- TRUE

    } else{

      nbr_new_taxa_ok <- FALSE

    }

    if(nbr_new_taxa_ok & nbr_queried_speci_ok) {

      new_values <-
        dplyr::tibble(
          idtax_n =
            ifelse(
              !is.null(new_genus) |
                !is.null(new_species) |
                !is.null(new_family) |
                !is.null(id_new_taxa),
              query_new_taxa$idtax_n,
              queried_speci$idtax_n
            ),
          detd = ifelse(!is.null(new_detd), as.numeric(new_detd), queried_speci$detd),
          detm = ifelse(!is.null(new_detm), as.numeric(new_detm), queried_speci$detm),
          dety = ifelse(!is.null(new_dety), as.numeric(new_dety), queried_speci$dety),
          detby = ifelse(!is.null(new_detby), new_detby, queried_speci$detby),
          detvalue = ifelse(!is.null(new_detvalue), new_detvalue, queried_speci$detvalue),
          colnbr = ifelse(!is.null(new_colnbr), new_colnbr, queried_speci$colnbr),
          suffix = ifelse(!is.null(new_suffix), new_suffix, queried_speci$suffix)
        )

      ## correcting if NA is not well coded and null
      if (!is.na(new_values$detby))
        if (new_values$detby == "NA")
          new_values$detby <- NA


      comp_res <- .comp_print_vec(vec_1 = queried_speci  %>%
                                    dplyr::select(!!colnames(new_values)),
                                  vec_2 = new_values)

      if (!is.na(comp_res$comp_html))
        print(comp_res$comp_html)


      print(queried_speci %>%
              dplyr::select(family_name, surname, colnbr, suffix, detd, detm, dety, detby, cold, colm, coly, country, id_specimen))

      comp_values <- comp_res$comp_tb

    } else{
      comp_values <- TRUE
    }

    if (only_new_ident & nbr_new_taxa_ok & any(comp_values == TRUE)) {
      if (any(comp_values %>%
              dplyr::select_if( ~ sum(.) > 0) %>%
              colnames() == "idtax_n")) {

        new_ident <- TRUE

      } else{

        new_ident <- FALSE

      }

    } else {

      new_ident <- TRUE

    }

    if (nbr_new_taxa_ok & any(comp_values == TRUE) &
        nbr_queried_speci_ok & new_ident) {

      modif_types <-
        paste0(colnames(as.matrix(comp_values))[which(as.matrix(comp_values))], sep="__")

      if (ask_before_update) {

        # confirmed <- utils::askYesNo("Confirm this update?")
        confirmed <- choose_prompt(message =  "Confirm this update?")

      } else
      {
        confirmed <- TRUE
      }

      if(confirmed) {
        if(add_backup) {

          colnames_speci <-
            dplyr::tbl(mydb, "followup_updates_specimens") %>%
            colnames()

          colnames_speci <-
            colnames_speci[which(!colnames_speci %in% c("date_modified",
                                                     "modif_type",
                                                     "id_fol_up_specimens"))]


          queried_speci <-
            queried_speci %>%
            dplyr::select(dplyr::one_of(colnames_speci))

          queried_speci <-
            queried_speci %>%
            mutate(date_modified = Sys.Date()) %>%
            mutate(modif_type = paste0(modif_types, collapse = ""))

          DBI::dbWriteTable(mydb, "followup_updates_specimens", queried_speci, append = TRUE, row.names = FALSE)
        }

        rs <-
          DBI::dbSendQuery(mydb, statement="UPDATE specimens SET idtax_n=$2, detd=$3, detm=$4, dety=$5, detby=$6, detvalue=$7, colnbr=$8, suffix=$9, data_modif_d=$10, data_modif_m=$11, data_modif_y=$12 WHERE id_specimen = $1",
                           params=list(queried_speci$id_specimen, # $1
                                       new_values$idtax_n, # $2
                                       as.numeric(new_values$detd), # $3
                                       as.numeric(new_values$detm), # $4
                                       as.numeric(new_values$dety), # $5
                                       new_values$detby, # $6
                                       new_values$detvalue, # $7
                                       new_values$colnbr, # $8
                                       new_values$suffix, # $9
                                       lubridate::day(Sys.Date()), # $10
                                       lubridate::month(Sys.Date()), # $11
                                       lubridate::year(Sys.Date()))) # $12

        # if(show_results) print(dbFetch(rs))
        DBI::dbClearResult(rs)

        if(show_results) query_specimens(id_search = queried_speci$id_specimen)
      }

    } else{
      if (!nbr_new_taxa_ok)
        cat(
          "\n NO UPDATE. The number of taxa selected for new identification is ",
          nrow(query_new_taxa),
          ". Select one taxa."
        )
      if (!any(comp_values == TRUE))
        cat("\n NO UPDATE. No different values entered.")
      if (!nbr_queried_speci_ok)
        cat(
          "\n NO UPDATE. The number of specimen selected is ",
          nrow(queried_speci),
          ". Select ONE specimen. Not less, not more."
        )
      if (!new_ident)
        cat("\n No new identification")
    }

    return(NA)
  } else {

    cat("\n SPECIMEN NOT FOUND")
    return(dplyr::tibble(collector = dplyr::tbl(mydb, "table_colnam") %>%
                           dplyr::filter(id_table_colnam == !!id_colnam) %>%
                           dplyr::select(colnam) %>%
                           dplyr::collect() %>%
                           dplyr::pull(),
                         number = number))

  }
}




#' Update specimens data data
#'
#' Update specimens data plot _ at a time
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data data frame data containing id and new values
#' @param col_names_select string plot name of the selected plots
#' @param col_names_corresp string of the selected plots
#' @param id_col integer indicate which name of col_names_select is the id for matching data
#' @param launch_update logical if TRUE updates are performed
#' @param add_backup logical whether backup of modified data should be recorded
#'
#'
#' @return No return value individuals updated
#' @export
update_specimens_batch <- function(new_data,
                                   col_names_select = NULL,
                                   col_names_corresp = NULL,
                                   id_col = 1,
                                   launch_update = FALSE,
                                   add_backup = TRUE) {


  mydb <- call.mydb()

  if (is.null(col_names_select)) {
    col_names_select <- names(new_data)
    cli::cli_alert_info("col_names_select is set as all names of new_data")
  }

  if (is.null(col_names_corresp)) {
    col_names_corresp <- col_names_select
    cli::cli_alert_info("col_names_corresp is set to names of col_names_select (it should be names of columns of specimens")
  }

  all_specimens <-
    dplyr::tbl(mydb, "specimens") %>%
    colnames()

  if(length(col_names_select) != length(col_names_corresp))
    stop("col_names_select and col_names_corresp should have same length")

  for (i in 1:length(col_names_select))
    if(!any(col_names_select[i] == colnames(new_data)))
      stop(paste(col_names_select[i], "not found in new_data"))

  for (i in 1:length(col_names_corresp))
    if(!any(col_names_corresp[i] == all_specimens))
      stop(paste(col_names_corresp[i], "not found in specimens table"))

  id_db <- col_names_corresp[id_col]

  if(!any(id_db == c("id_specimen"))) stop("id for matching should be id_specimens")

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)



  output_matches <- .find_ids(dataset = new_data_renamed,
                              col_new = col_names_corresp,
                              id_col_nbr = id_col,
                              type_data = "specimens")

  matches_all <-
    output_matches[[2]]

  for (i in 1:length(matches_all)) {

    field <- names(matches_all)[i]
    var_new <- paste0(field, "_new")
    matches <- matches_all[[i]]

    if(launch_update & nrow(matches) > 0) {

      matches <-
        matches %>%
        dplyr::select(id, dplyr::contains("_new"))
      matches <-
        .add_modif_field(matches)

      all_id_match <- dplyr::pull(dplyr::select(matches, id))

      if(add_backup) {

      }


      ## create a temporary table with new data
      DBI::dbWriteTable(mydb, "temp_table", matches,
                        overwrite=T, fileEncoding = "UTF-8", row.names=F)

      query_up <-
        paste0("UPDATE specimens t1 SET (",field,", data_modif_d, data_modif_m, data_modif_y) = (t2.",var_new, ", t2.date_modif_d, t2.date_modif_m, t2.date_modif_y) FROM temp_table t2 WHERE t1.", id_db," = t2.id")

      rs <-
        DBI::dbSendStatement(mydb, query_up)

      cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
      rs@sql
      DBI::dbClearResult(rs)

      cli::cli_alert_success("Successful update")

    } else{

      if (launch_update & nrow(matches) == 0)
        cat("\n No new values found")

      if (!launch_update)
        cli::cli_alert_danger("No update because launch_update is FALSE")

    }
  }
  return(matches_all)
}




# update_trait_list_table <- function(trait_searched = NULL,
#                                     trait_id = NULL,
#                                     new_trait_name = NULL,
#                                     new_relatedterm = NULL,
#                                     new_maxallowedvalue = NULL,
#                                     new_minallowedvalue = NULL,
#                                     new_traitdescription = NULL,
#                                     new_expectedunit = NULL,
#                                     ask_before_update = TRUE,
#                                     add_backup = TRUE,
#                                     show_results=TRUE) {
# 
#   if(!exists("mydb")) call.mydb()
# 
#   if(all(is.null(c(trait_searched, trait_id))))
#     stop("\n Provide trait_searched or trait_id to update")
# 
#   ### checking if at least one modification is asked
#   new_vals <- c(new_trait_name, new_relatedterm, new_maxallowedvalue,
#                 new_minallowedvalue, new_traitdescription, new_expectedunit)
#   if(!any(!is.null(new_vals))) stop("\n No new values to be updated.")
# 
#   ### querying for entries to be modified
#   if(!is.null(trait_searched)) {
#     query <- 'SELECT * FROM traitlist WHERE MMM'
#     query <- gsub(pattern = "MMM", replacement = paste0(" trait ILIKE '%",
#                                                         trait_searched, "%'"), x=query)
# 
#     rs <- DBI::dbSendQuery(mydb, query)
#     query_trait <- DBI::dbFetch(rs)
#     DBI::dbClearResult(rs)
# 
#   }else{
#     query_trait <-
#       dplyr::tbl(mydb, "traitlist") %>%
#       dplyr::filter(id_trait == !!trait_id) %>%
#       dplyr::collect()
#   }
#   print(query_trait %>% as.data.frame())
#   if(nrow(query_trait)>1) stop("more than one trait selected, select one")
#   if(nrow(query_trait)==0) stop("no trait selected, select one")
# 
#   modif_types <-
#     vector(mode = "character", length = nrow(query_trait))
# 
#   new_vals <-
#     dplyr::tibble(trait = ifelse(!is.null(new_trait_name), as.character(new_trait_name),
#                                  query_trait$trait),
#                   relatedterm = ifelse(!is.null(new_relatedterm), as.character(new_relatedterm),
#                                        query_trait$relatedterm),
#                   maxallowedvalue = ifelse(!is.null(new_maxallowedvalue), as.numeric(new_maxallowedvalue),
#                                            query_trait$maxallowedvalue),
#                   minallowedvalue = ifelse(!is.null(new_minallowedvalue), as.numeric(new_minallowedvalue),
#                                            query_trait$minallowedvalue),
#                   traitdescription = ifelse(!is.null(new_traitdescription), as.character(new_traitdescription),
#                                             query_trait$traitdescription),
#                   expectedunit = ifelse(!is.null(new_expectedunit), as.character(new_expectedunit),
#                                         query_trait$expectedunit))
# 
# 
#   sel_query_trait <-
#     dplyr::bind_rows(new_vals, query_trait %>%
#                        dplyr::select(-valuetype, -id_trait, -date_modif_d, -date_modif_m, -date_modif_y))
# 
#   sel_query_trait <-
#     replace_NA(df = sel_query_trait)
# 
#   comp_vals <-
#     apply(sel_query_trait, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])
# 
#   if (any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]
# 
#   modif_types[1:length(modif_types)] <-
#     paste(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")
# 
#   # if(!any(comp_vals)) stop("No update performed because no values are different.")
# 
#   if(any(comp_vals)) {
# 
#     cat(paste("\n Number of rows selected to be updated :", nrow(query_trait), "\n"))
# 
#     if(ask_before_update) {
# 
#       sel_query_trait %>%
#         dplyr::select(!!names(comp_vals)) %>%
#         dplyr::select(which(comp_vals)) %>%
#         print()
# 
#       Q <-
#         choose_prompt(message =  "Do you confirm you want to update these rows for selected fields?")
#     }else{
#       Q <- TRUE
#     }
# 
#     if(Q) {
# 
#       if(add_backup) {
#         message("no back up for this table yet")
#         # query_trait <-
#         #   query_trait %>%
#         #   tibble::add_column(date_modified=Sys.Date()) %>%
#         #   tibble::add_column(modif_type=modif_types)
#         #
#         #
#         # DBI::dbWriteTable(mydb, "followup_updates_diconames", query_tax, append = TRUE, row.names = FALSE)
# 
#       }
# 
#       query_trait <-
#         query_trait %>%
#         dplyr::select(-date_modif_d, -date_modif_m, -date_modif_y)
# 
#       query_trait <-
#         .add_modif_field(query_trait)
# 
#       rs <-
#         DBI::dbSendQuery(mydb,
#                          statement="UPDATE traitlist SET trait=$2, relatedterm=$3, valuetype=$4, maxallowedvalue=$5, minallowedvalue=$6, traitdescription=$7, factorlevels=$8, expectedunit=$9, date_modif_d=$10, date_modif_m=$11, date_modif_y=$12  WHERE id_trait = $1",
#                          params=list(query_trait$id_trait, # $1
#                                      rep(ifelse(!is.null(new_trait_name), as.character(new_trait_name),
#                                                 query_trait$trait), nrow(query_trait)), # $2
#                                      rep(ifelse(!is.null(new_relatedterm), as.character(new_relatedterm),
#                                                 query_trait$relatedterm), nrow(query_trait)), # $3
#                                      rep(query_trait$valuetype, nrow(query_trait)), # $4
#                                      rep(ifelse(!is.null(new_maxallowedvalue), as.numeric(new_maxallowedvalue),
#                                                 query_trait$maxallowedvalue), nrow(query_trait)), # $5
#                                      rep(ifelse(!is.null(new_minallowedvalue), as.numeric(new_minallowedvalue),
#                                                 query_trait$minallowedvalue), nrow(query_trait)), # $6
#                                      rep(ifelse(!is.null(new_traitdescription), as.character(new_traitdescription),
#                                                 query_trait$traitdescription), nrow(query_trait)), # $7
#                                      rep(query_trait$factorlevels, nrow(query_trait)), # $8
#                                      rep(ifelse(!is.null(new_expectedunit), as.character(new_expectedunit),
#                                                 query_trait$expectedunit), nrow(query_trait)), # $9
#                                      rep(query_trait$date_modif_d, nrow(query_trait)), # $10
#                                      rep(query_trait$date_modif_m, nrow(query_trait)), # $11
#                                      rep(query_trait$date_modif_y, nrow(query_trait))) # $12
#         )
# 
#       DBI::dbClearResult(rs)
# 
#       rs <-
#         DBI::dbSendQuery(mydb, statement="SELECT *FROM traitlist WHERE id_trait = $1",
#                          params=list(query_trait$id_trait))
#       if(show_results) print(DBI::dbFetch(rs))
#       DBI::dbClearResult(rs)
# 
#     }
#   }else{
# 
#     if(!any(comp_vals)) print("No update performed because no values are different.")
#   }
# 
#   # dbDisconnect(mydb)
# 
# }






# update_individuals_features_measures <- function(new_data,
#                                    col_names_select = NULL,
#                                    col_names_corresp = NULL,
#                                    id_trait = NULL,
#                                    id_col,
#                                    launch_update = FALSE,
#                                    add_backup = TRUE) {
# 
#   if(!exists("mydb")) call.mydb()
# 
#   if (is.null(col_names_select)) {
#     col_names_select <- names(new_data)
#     cli::cli_alert_info("col_names_select is set as all names of new_data")
#   }
# 
#   if (is.null(col_names_corresp)) {
#     col_names_corresp <- col_names_select
#     cli::cli_alert_info("col_names_corresp is set to names of col_names_select (it should be names of columns of data_traits_measures")
#   }
# 
#   for (i in 1:length(col_names_select))
#     if(!any(col_names_select[i] == colnames(new_data)))
#       stop(paste(col_names_select[i], "not found in new_data"))
# 
#   if (!is.null(id_trait)) {
# 
#     new_data <-
#       .link_trait(data_stand = new_data,
#                   trait = col_names_select[id_trait], issues = new_data$issue)
# 
#     found_trait <- traits_list() %>% filter(id_trait %in% unique(new_data$id_trait))
# 
#     if (found_trait$valuetype == "numeric") {
# 
#       col_names_corresp[id_trait] <- "traitvalue"
#       col_names_select[id_trait] <- "trait"
# 
#     }
#   }
# 
# 
#   # new_data <-
#   #   new_data %>%
#   #   rename(traitvalue = trait)
# 
# 
#   all_colnames_trait <-
#     dplyr::tbl(mydb, "data_traits_measures") %>%
#     colnames()
# 
#   if(length(col_names_select) != length(col_names_corresp))
#     stop("col_names_select and col_names_corresp should have same length")
# 
#   for (i in 1:length(col_names_corresp))
#     if(!any(col_names_corresp[i] == all_colnames_trait))
#       stop(paste(col_names_corresp[i], "not found in data_traits_measures"))
# 
#   id_db <- col_names_corresp[id_col]
# 
#   if (!any(id_db == c("id_trait_measures"))) stop("id for matching should be id_trait_measures")
# 
#   new_data_renamed <-
#     .rename_data(dataset = new_data,
#                  col_old = col_names_select,
#                  col_new = col_names_corresp)
# 
#   output_matches <- .find_ids(dataset = new_data_renamed,
#                               col_new = col_names_corresp,
#                               id_col_nbr = id_col,
#                               type_data = "trait_measures")
# 
#   matches_all <-
#     output_matches[[2]]
# 
#   for (i in 1:length(matches_all)) {
# 
#     field <- names(matches_all)[i]
#     var_new <- paste0(field, "_new")
#     matches <- matches_all[[i]]
# 
#     if(launch_update & nrow(matches) > 0) {
# 
#       matches <-
#         matches %>%
#         dplyr::select(id, dplyr::contains("_new"))
#       # matches <-
#       #   .add_modif_field(matches)
# 
#       all_id_match <- dplyr::pull(dplyr::select(matches, id))
# 
#       if(add_backup) {
# 
#         quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))
# 
#         all_rows_to_be_updated <-
#           dplyr::tbl(mydb, "data_traits_measures") %>%
#           dplyr::filter(!!quo_var_id %in% all_id_match) %>%
#           dplyr::collect()
# 
#         colnames_plots <-
#           dplyr::tbl(mydb, "followup_updates_traits_measures")  %>%
#           dplyr::select(-date_modified, -modif_type, -id_fol_up_traits_measures) %>%
#           dplyr::collect() %>%
#           dplyr::top_n(1) %>%
#           colnames()
# 
#         all_rows_to_be_updated <-
#           all_rows_to_be_updated %>%
#           dplyr::select(dplyr::one_of(colnames_plots))
# 
#         all_rows_to_be_updated <-
#           all_rows_to_be_updated %>%
#           mutate(date_modified = Sys.Date()) %>%
#           mutate(modif_type = field)
# 
#         print(all_rows_to_be_updated %>%
#                 dplyr::select(modif_type, date_modified))
# 
#         DBI::dbWriteTable(mydb, "followup_updates_traits_measures",
#                           all_rows_to_be_updated, append = TRUE, row.names = FALSE)
#       }
# 
#       ## create a temporary table with new data
#       DBI::dbWriteTable(mydb, "temp_table", matches,
#                         overwrite=T, fileEncoding = "UTF-8", row.names=F)
# 
#       query_up <-
#         paste0("UPDATE data_traits_measures t1 SET ",field," = t2.",var_new, " FROM temp_table t2 WHERE t1.", id_db," = t2.id")
# 
# 
#       rs <-
#         DBI::dbSendStatement(mydb, query_up)
# 
#       cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
#       rs@sql
#       DBI::dbClearResult(rs)
# 
#     } else{
# 
#       if (launch_update & nrow(matches) == 0)
#         cat("\n No new values found")
# 
#     }
#   }
# 
#   return(matches_all)
# 
#   # if(!is.null(trait_values_new_data)) { #  & !is.null(col_names_trait_corresp)
#   #
#   #   # if (length(trait_values_new_data) != length(col_names_trait_corresp))
#   #   #   stop("trait_values_new_data and col_names_trait_corresp should have same length")
#   #
#   #   new_data <-
#   #     new_data %>%
#   #     rename(trait := all_of(trait_values_new_data))
#   #
#   #   new_data <-
#   #     .link_trait(data_stand = new_data, trait = trait_values_new_data)
#   #
#   #   col_names_trait_corresp
#   #
#   #   output_matches <-
#   #     .find_ids(dataset = new_data,
#   #               col_new = c("trait", col_name_id_corresp),
#   #               id_col_nbr = 2,
#   #               type_data = "trait")
#   #
#   #   # all_colnames_ind <- traits_list()
#   #   #
#   #   # for (i in 1:length(col_names_trait_corresp))
#   #   #   if(!any(col_names_trait_corresp[i] == all_colnames_ind$trait)) {
#   #   #     stop(paste(col_names_trait_corresp[i], "not found in trait list"))
#   #   #     print("check")
#   #   #     print(all_colnames_ind$trait)
#   #   #   }
#   # }
#   #
#   # if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp)) {
#   #
#   #   if(length(measures_property_new_data)!=length(col_names_property_corresp))
#   #     stop("measures_property_new_data and col_names_property_corresp should have same length")
#   #
#   #   colnames_property <-
#   #     dplyr::tbl(mydb, "data_traits_measures") %>%
#   #     dplyr::select(country, decimallatitude, decimallongitude, elevation,
#   #                   verbatimlocality,
#   #                   basisofrecord, year, month, day,
#   #                   issue, measurementmethod) %>%
#   #     colnames()
#   #
#   #   colnames_property <- c(colnames_property, "collector")
#   #
#   #   for (i in 1:length(col_names_property_corresp))
#   #     if(!any(col_names_property_corresp[i] == colnames_property)) {
#   #       stop(paste(col_names_property_corresp[i], "not found in property measureament"))
#   #       print("check")
#   #       print(colnames_property)
#   #     }
#   # }
#   #
#   # # id_db <- col_id
#   #
#   # if(!any(col_name_id_corresp == c("id_trait_measures", "id_n", "id_old")))
#   #   stop("id for matching should be one of id_trait_measures, id_n, id_old")
#   #
#   # # if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp)) {
#   # #   new_data <-
#   # #     .rename_data(dataset = new_data,
#   # #                  col_old = c(trait_values_new_data, id_new_data),
#   # #                  col_new = c(col_names_trait_corresp, col_name_id_corresp))
#   # # }
#   #
#   # if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp)) {
#   #   new_data <-
#   #     .rename_data(dataset = new_data,
#   #                  col_old = c(measures_property_new_data, id_new_data),
#   #                  col_new = c(col_names_property_corresp, col_name_id_corresp))
#   # }
#   #
#   # all_corresponding_matches <- list()
#   #
#   # if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp))
#   #   nbe_col_cor <- length(col_names_trait_corresp)
#   # if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp))
#   #   nbe_col_cor <- length(col_names_property_corresp)
#   #
#   # for (k in 1:nbe_col_cor) {
#   #
#   #   if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp))
#   #     output_matches <-
#   #       .find_ids(dataset = new_data,
#   #                               col_new = c(col_names_trait_corresp[k], col_name_id_corresp),
#   #                               id_col_nbr = 2,
#   #                               type_data = "trait")
#   #
#   #   if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp))
#   #     output_matches <-
#   #       .find_ids(dataset = new_data,
#   #               col_new = c(col_names_property_corresp[k], col_name_id_corresp),
#   #               id_col_nbr = 2,
#   #               type_data = "trait")
#   #
#   #   matches <-
#   #     output_matches[[2]][[1]]
#   #
#   #   if(launch_update & nrow(matches) > 0) {
#   #     matches <-
#   #       matches %>%
#   #       dplyr::select(id, dplyr::contains("_new"))
#   #     matches <-
#   #       .add_modif_field(matches)
#   #
#   #     all_id_match <- dplyr::pull(dplyr::select(matches, id))
#   #
#   #     if(col_name_id_corresp %in% c("id_n", "id_old")) {
#   #       ids_traits_measures <-
#   #         output_matches[[1]] %>%
#   #         dplyr::filter(id %in% all_id_match) %>%
#   #         dplyr::select(dplyr::contains("id_trait_measures"))
#   #
#   #       matches <-
#   #         matches %>%
#   #         dplyr::mutate(id = dplyr::pull(ids_traits_measures))
#   #     }
#   #
#   #     if(dplyr::tbl(mydb, "data_traits_measures") %>%
#   #       dplyr::filter(id_trait_measures %in% !!matches$id) %>%
#   #       dplyr::distinct(traitid) %>%
#   #       dplyr::collect() %>%
#   #       nrow()>2) stop("more than one trait to be updated whereas only one expected")
#   #
#   #     if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp))
#   #       field <- "traitvalue"
#   #     if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp))
#   #       field <- col_names_property_corresp[k]
#   #
#   #     ## create a temporary table with new data
#   #     DBI::dbWriteTable(mydb, "temp_table", matches,
#   #                       overwrite=T, fileEncoding = "UTF-8", row.names=F)
#   #
#   #     var_new <- matches %>%
#   #       dplyr::select(dplyr::contains("_new")) %>%
#   #       colnames()
#   #
#   #     query_up <-
#   #       paste0("UPDATE data_traits_measures t1 SET (", field ,", date_modif_d, date_modif_m, date_modif_y) = (t2.", var_new, ", t2.date_modif_d, t2.date_modif_m, t2.date_modif_y) FROM temp_table t2 WHERE t1.id_trait_measures = t2.id")
#   #
#   #     rs <-
#   #       DBI::dbSendStatement(mydb, query_up)
#   #
#   #     cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
#   #     rs@sql
#   #     DBI::dbClearResult(rs)
#   #
#   #     if(add_backup) {
#   #       field <- col_names_trait_corresp[k]
#   #
#   #       ids_measures <- matches$id
#   #
#   #       all_rows_to_be_updated <-
#   #         dplyr::tbl(mydb, "data_traits_measures") %>%
#   #         dplyr::filter(id_trait_measures %in% ids_measures) %>%
#   #         dplyr::collect()
#   #
#   #       colnames_measures <-
#   #         dplyr::tbl(mydb, "followup_updates_traits_measures") %>%
#   #         dplyr::select(-date_modified, -modif_type, -id_fol_up_traits_measures) %>%
#   #         dplyr::collect() %>%
#   #         dplyr::top_n(1) %>%
#   #         colnames()
#   #
#   #       all_rows_to_be_updated <-
#   #         all_rows_to_be_updated %>%
#   #         dplyr::select(dplyr::one_of(colnames_measures))
#   #
#   #       all_rows_to_be_updated <-
#   #         all_rows_to_be_updated %>%
#   #         tibble::add_column(date_modified=Sys.Date()) %>%
#   #         tibble::add_column(modif_type=field)
#   #
#   #       print(all_rows_to_be_updated %>% dplyr::select(modif_type, date_modified))
#   #
#   #       DBI::dbWriteTable(mydb, "followup_updates_traits_measures",
#   #                         all_rows_to_be_updated, append = TRUE, row.names = FALSE)
#   #     }
#   #   }else{
#   #     if(launch_update & nrow(matches)==0) cat("\n No new values found")
#   #   }
#   #
#   #   all_corresponding_matches[[k]] <- output_matches[[2]][[1]]
#   #   if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp)) names(all_corresponding_matches)[k] <- col_names_trait_corresp[k]
#   #   if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp)) names(all_corresponding_matches)[k] <- col_names_property_corresp[k]
#   # }
#   #
#   # return(all_corresponding_matches)
# }




update_trait_measures <- function(new_data,
                                  col_names_select = NULL,
                                  col_names_corresp = NULL,
                                  id_col,
                                  launch_update = FALSE,
                                  add_backup = TRUE,
                                  ask_before_update = FALSE,
                                  only_new_ident = FALSE) {

  if (exists("mydb_taxa")) rm(mydb_taxa)
  if (!exists("mydb_taxa")) call.mydb.taxa()

  all_colnames_rec <-
    try_open_postgres_table(table = "table_traits_measures", con = mydb_taxa) %>%
    # dplyr::tbl(mydb, "table_traits_measures") %>%
    colnames()

  if (is.null(col_names_select))
    col_names_select <- names(new_data)

  if (is.null(col_names_corresp))
    col_names_corresp <- col_names_select

  if (length(col_names_select) != length(col_names_corresp))
    stop("col_names_select and col_names_corresp should have same length")

  for (i in 1:length(col_names_select))
    if(!any(col_names_select[i] == names(new_data)))
      stop(paste(col_names_select[i], "not found in new_data"))

  for (i in 1:length(col_names_corresp))
    if(!any(col_names_corresp[i] == all_colnames_rec))
      stop(paste(col_names_corresp[i], "not found in table_records"))

  id_db <- col_names_corresp[id_col]

  if(!any(id_db == c("id_trait_measures")))
    stop("id for matching should be id_trait_measures")

  new_data_renamed <-
    .rename_data(dataset = new_data %>%
                   dplyr::select(all_of(col_names_select)),
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  output_matches <-
    .find_ids(
      dataset = new_data_renamed,
      col_new = col_names_corresp,
      id_col_nbr = id_col,
      type_data = "sp_trait_measures"
    )

  matches_all <-
    output_matches[[2]]


  if (any(names(matches_all) == "idtax")) {

    if (nrow(matches_all$idtax_n) > 0) {

      tax_new <-
        query_taxa(
          ids = matches_all$idtax$idtax_new,
          verbose = T,
          check_synonymy = T,
          class = NULL,
          extract_traits = FALSE
        )

      if (!any(is.na(matches_all$idtax$idtax_old))) {
        tax_old <-
          query_taxa(
            ids = matches_all$idtax$idtax_old,
            verbose = TRUE,
            check_synonymy = TRUE,
            class = NULL
          )
      } else{
        tax_old <-
          query_taxa(
            ids = 1,
            verbose = TRUE,
            check_synonymy = TRUE,
            extract_traits = FALSE
          ) %>%
          slice(0)
      }

      matches_all$idtax <-
        matches_all$idtax %>%
        left_join(
          tax_new %>%
            dplyr::select(idtax_n, tax_fam, tax_gen, tax_sp_level),
          by = c("idtax_new" = "idtax_n")
        ) %>%
        rename(
          tax_gen_new = tax_gen,
          tax_sp_level_new = tax_sp_level,
          tax_fam_new = tax_fam
        )

      matches_all$idtax <-
        matches_all$idtax %>%
        left_join(
          tax_old %>%
            dplyr::select(idtax_n, tax_fam, tax_gen, tax_sp_level),
          by = c("idtax_old" = "idtax_n")
        ) %>%
        rename(
          tax_gen_old = tax_gen,
          tax_sp_level_old = tax_sp_level,
          tax_fam_old = tax_fam
        )

    }

  }


  print(lapply(matches_all, function(x) as.data.frame(x)))

  # if(only_new_ident) {
  #
  #   if (nrow(matches_all$idtax_n) > 0) {
  #
  #     confirm_new_ident <- TRUE
  #
  #   } else{
  #
  #     confirm_new_ident <- FALSE
  #
  #   }
  #
  # }else{
  #
  #   if (any(unlist(lapply(matches_all, nrow)) > 0)) {
  #
  #     confirm_new_ident <- TRUE
  #
  #   } else{
  #
  #     confirm_new_ident <- FALSE
  #
  #   }
  # }


  if(ask_before_update & confirm_new_ident) {

    # confirm <-
    #   askYesNo(msg = 'Confirm update ?')
    confirm <- choose_prompt(message =  "Confirm update ?")

    print(confirm)

  }else{

    confirm <- TRUE

  }


  if(confirm & confirm_new_ident) {


    for (i in 1:length(matches_all)) {

      field <- names(matches_all)[i]
      var_new <- paste0(field, "_new")
      matches <- matches_all[[i]]

      if(launch_update & nrow(matches) > 0) {

        matches <-
          matches %>%
          dplyr::select(id, dplyr::contains("_new"))
        matches <-
          .add_modif_field(matches)

        all_id_match <- dplyr::pull(dplyr::select(matches, id))

        if(add_backup) {

          # quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))
          #
          # all_rows_to_be_updated <-
          #   dplyr::tbl(mydb, "table_records") %>%
          #   dplyr::filter(!!quo_var_id %in% all_id_match) %>%
          #   dplyr::collect()
          #
          # colnames_plots <-
          #   dplyr::tbl(mydb, "followup_updates_table_records")  %>%
          #   dplyr::select(-date_modified, -modif_type) %>%
          #   dplyr::collect() %>%
          #   dplyr::top_n(1) %>%
          #   colnames()
          #
          # all_rows_to_be_updated <-
          #   all_rows_to_be_updated %>%
          #   dplyr::select(dplyr::one_of(colnames_plots))
          #
          # all_rows_to_be_updated <-
          #   all_rows_to_be_updated %>%
          #   tibble::add_column(date_modified = Sys.Date()) %>%
          #   tibble::add_column(modif_type = field)
          #
          # print(all_rows_to_be_updated %>%
          #         dplyr::select(modif_type, date_modified))
          #
          # DBI::dbWriteTable(mydb, "followup_updates_table_records",
          #                   all_rows_to_be_updated, append = TRUE, row.names = FALSE)
        }

        ## create a temporary table with new data
        DBI::dbWriteTable(mydb_taxa, "temp_table", matches,
                          overwrite=T, fileEncoding = "UTF-8", row.names=F)

        query_up <-
          paste0("UPDATE table_traits_measures t1 SET (", field,", date_modif_d, date_modif_m, date_modif_y) = (t2.", var_new, ", t2.data_modif_d, t2.data_modif_m, t2.data_modif_y) FROM temp_table t2 WHERE t1.",
                 id_db," = t2.id")

        rs <-
          DBI::dbSendStatement(mydb_taxa, query_up)

        cat("\nRows updated", RPostgres::dbGetRowsAffected(rs))
        rs@sql
        DBI::dbClearResult(rs)


      } else{

        if (launch_update & nrow(matches) == 0)
          cat("\n No new values found")

      }
    }
  }

  if(ask_before_update & !confirm)
    message("\n NO Update Done")

  return(matches_all)

}


update_trait_table <- function(new_data,
                               col_names_select = NULL,
                               col_names_corresp = NULL,
                               id_col,
                               launch_update = FALSE,
                               add_backup = TRUE) {

  if (exists("mydb_taxa")) rm(mydb_taxa)
  if (!exists("mydb_taxa")) call.mydb.taxa()

  all_colnames_traits <-
    dplyr::tbl(mydb_taxa, "table_traits") %>%
    colnames()

  if (is.null(col_names_select) & is.null(col_names_corresp))
    col_names_corresp <- col_names_select <- names(new_data)

  if (length(col_names_select) != length(col_names_corresp))
    stop("col_names_select and col_names_corresp should have same length")

  for (i in 1:length(col_names_select))
    if(!any(col_names_select[i] == colnames(new_data)))
      stop(paste(col_names_select[i], "not found in new_data"))

  for (i in 1:length(col_names_corresp))
    if(!any(col_names_corresp[i] == all_colnames_traits))
      stop(paste(col_names_corresp[i], "not found in table_records"))

  id_db <- col_names_corresp[id_col]

  if(!any(id_db == c("id_trait")))
    stop("id for matching should be id_trait")

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  output_matches <- .find_ids(dataset = new_data_renamed,
                              col_new = col_names_corresp,
                              id_col_nbr = id_col,
                              type_data = "trait")

  matches_all <-
    output_matches[[2]]

  for (i in 1:length(matches_all)) {

    field <- names(matches_all)[i]
    var_new <- paste0(field, "_new")
    matches <- matches_all[[i]]

    if(launch_update & nrow(matches)>0) {

      matches <-
        matches %>%
        dplyr::select(id, dplyr::contains("_new"))
      # matches <-
      #   .add_modif_field(matches)

      all_id_match <- dplyr::pull(dplyr::select(matches, id))

      if(add_backup) {

        # quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))
        #
        # all_rows_to_be_updated <-
        #   dplyr::tbl(mydb, "table_records") %>%
        #   dplyr::filter(!!quo_var_id %in% all_id_match) %>%
        #   dplyr::collect()
        #
        # colnames_plots <-
        #   dplyr::tbl(mydb, "followup_updates_table_records")  %>%
        #   dplyr::select(-date_modified, -modif_type) %>%
        #   dplyr::collect() %>%
        #   dplyr::top_n(1) %>%
        #   colnames()
        #
        # all_rows_to_be_updated <-
        #   all_rows_to_be_updated %>%
        #   dplyr::select(dplyr::one_of(colnames_plots))
        #
        # all_rows_to_be_updated <-
        #   all_rows_to_be_updated %>%
        #   tibble::add_column(date_modified = Sys.Date()) %>%
        #   tibble::add_column(modif_type = field)
        #
        # print(all_rows_to_be_updated %>%
        #         dplyr::select(modif_type, date_modified))
        #
        # DBI::dbWriteTable(mydb, "followup_updates_table_records",
        #                   all_rows_to_be_updated, append = TRUE, row.names = FALSE)
      }

      ## create a temporary table with new data
      DBI::dbWriteTable(mydb_taxa, "temp_table", matches,
                        overwrite=T, fileEncoding = "UTF-8", row.names=F)

      query_up <-
        paste0("UPDATE table_traits t1 SET ", field," = t2.", var_new, " FROM temp_table t2 WHERE t1.",
               id_db," = t2.id")

      rs <-
        DBI::dbSendStatement(mydb_taxa, query_up)

      cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
      rs@sql
      DBI::dbClearResult(rs)

    }else{
      if(launch_update & nrow(matches)==0) cat("\n No new values found")
    }
  }

  return(matches_all)
}






#' Update taxonomic data
#'
#' Update taxonomic data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param genus_searched string genus name searched
#' @param tax_esp_searched string species name searched
#' @param tax_fam_searched string family name searched
#' @param new_tax_gen string new genus name
#' @param new_tax_esp string new species name
#' @param new_full_name_auth string new full name with authors
#' @param new_tax_fam string new family name
#' @param new_tax_rank1 string new rank
#' @param new_tax_name1 string new name of rank1
#' @param new_taxook integer new tax code
#' @param new_morphocat integer new morphocat code
#' @param new_detvalue integer new detvalue code
#' @param new_full_name_no_auth string new full name without authors - if not provided concatenate of new_esp and new_genus
#' @param new_full_name_used string new full_name_used
#' @param new_full_name_used2 string new full_name_used2
#' @param id_search integer id of the taxa searched
#' @param ask_before_update logical TRUE by default, ask for confirmation before updating
#' @param add_backup logical TRUE by default, add backup of modified data
#' @param show_results logical TRUE by default, show the data that has been modified
#' @param no_synonym_modif logical FALSE by default, if TRUE and if the selected taxa is considered as synonym, then this will be modified and the selected taxa will not longer be a synonym
#' @param synonym_of list if the selected taxa should be put in synonymy with an existing taxa, add in a list at least one values to identify to which taxa it will be put in synonymy: genus, species or id
#'
#'
#' @return No return value individuals updated
#' @export
update_dico_name <- function(genus_searched = NULL,
                             tax_esp_searched = NULL,
                             tax_fam_searched = NULL,
                             tax_order_searched = NULL,
                             id_searched = NULL,
                             new_tax_gen = NULL,
                             new_tax_esp = NULL,
                             new_tax_fam = NULL,
                             new_tax_order = NULL,
                             new_tax_rank1 = NULL,
                             new_tax_rank = NULL,
                             new_tax_name1 = NULL,
                             new_tax_famclass = NULL,
                             new_introduced_status = NULL,
                             new_tax_rankesp = NULL,
                             ask_before_update = TRUE,
                             add_backup = TRUE,
                             show_results = TRUE,
                             cancel_synonymy= FALSE,
                             synonym_of = NULL,
                             exact_match = FALSE) {

  if (!exists("mydb_taxa")) call.mydb.taxa()

  if(all(is.null(c(genus_searched, tax_esp_searched,
                   tax_fam_searched, synonym_of,
                   id_searched, new_tax_rankesp)) & !cancel_synonymy))
    stop("Provide the species to be updated or precise new synonymy")

  if(!is.null(new_tax_famclass)) {
    new_id_tax_famclass <-
      try_open_postgres_table(table = "table_tax_famclass", con = mydb_taxa) %>%
      dplyr::filter(tax_famclass == new_tax_famclass) %>%
      dplyr::collect() %>%
      pull(id_tax_famclass)

    if(length(new_id_tax_famclass) == 0)
      stop("new tax_famclass not recorded in table_tax_famclass")

  } else {
    new_id_tax_famclass = NULL
  }

  ### checking if at least one modification is asked
  new_vals <- c(new_tax_gen = new_tax_gen,
                new_tax_esp = new_tax_esp,
                new_tax_order = new_tax_order,
                new_tax_fam = new_tax_fam,
                new_introduced_status = new_introduced_status,
                new_id_tax_famclass = new_id_tax_famclass,
                new_tax_rank = new_tax_rank,
                new_tax_rank1 = new_tax_rank1,
                new_tax_rankesp = new_tax_rankesp)

  if (!any(!is.null(new_vals)) &
      is.null(synonym_of) &
      !cancel_synonymy)
    stop("\n No new values to be updated.")


  ### querying for entries to be modified
  if(is.null(id_searched)) {

    cat(paste("\n", genus_searched, " - ", tax_esp_searched, "-", tax_fam_searched))
    query_tax <-
      query_taxa(
        genus = genus_searched,
        species = tax_esp_searched,
        family = tax_fam_searched,
        order = tax_order_searched,
        check_synonymy = FALSE,
        exact_match = exact_match,
        extract_traits = FALSE
      )

  } else {

    query_tax <-
      query_taxa(
        ids = id_searched,
        check_synonymy = FALSE,
        class = NULL,
        extract_traits = FALSE
      )

  }

  if(is.null(query_tax)) query_tax <- dplyr::tibble()

  if (nrow(query_tax) > 0) {

    cli::cli_alert_info(cli::col_blue("{nrow(query_tax)} taxa selected"))
    print(query_tax %>% as.data.frame())
    nrow_query = TRUE

  } else{

    nrow_query = FALSE

  }

  if(nrow_query)
    modif_types <-
    vector(mode = "character", length = nrow(query_tax))

  ## if the modification does not concern synonymies, check if provided values are different for those existing
  if(nrow_query & !cancel_synonymy & is.null(synonym_of)) {

    query_tax_n <- query_tax
    col_new <- c()
    for (i in c(
      "tax_order",
      "tax_esp",
      "tax_fam",
      "tax_gen",
      "tax_rank01",
      "tax_rank",
      "tax_nam01",
      "introduced_status",
      "tax_rankesp",
      "id_tax_famclass"
    )) {
      if (any(i == gsub("new_", "", names(new_vals)))) {
        col_new <- c(col_new, i)
        var <- enquo(i)
        query_tax_n <-
          query_tax_n %>%
          dplyr::mutate(!!var := new_vals[grep(i, names(new_vals))])
      }
    }

    query_tax_n <-
      query_tax_n %>%
      dplyr::select(all_of(col_new))


    query_tax_n <-
      query_tax_n %>%
      mutate_if(is.numeric,
                ~ tidyr::replace_na(. , -9999)) %>%
      mutate_if(is.character,
                ~ tidyr::replace_na(. , "-9999"))

    sel_query_tax <-
      dplyr::bind_rows(query_tax_n, query_tax %>%
                         dplyr::select(all_of(col_new)))

    sel_query_tax <-
      sel_query_tax %>%
      mutate_if(is.numeric,
                ~ tidyr::replace_na(. , -9999)) %>%
      mutate_if(is.character,
                ~ tidyr::replace_na(. , "-9999"))

    print(sel_query_tax)

    comp_vals <-
      apply(
        sel_query_tax,
        MARGIN = 2,
        FUN = function(x)
          unique(x[nrow(query_tax_n)]) != x[(nrow(query_tax_n) + 1):length(x)]
      )

    # comp_vals <-
    #   apply(sel_query_tax, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])

    if(!is.null(nrow(comp_vals))) {

      query_tax <-
        query_tax[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
      modif_types <- modif_types[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x))]
      comp_vals <-
        apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))

    } else {

      query_tax <- query_tax

    }

    if(any(is.na(comp_vals)))
      comp_vals <-
      comp_vals[!is.na(comp_vals)]

    modif_types[1:length(modif_types)] <-
      paste0(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")

  } else {

    comp_vals <- TRUE

  }

  new_id_diconame_good <- NULL
  if (nrow_query & cancel_synonymy) {
    if (is.na(query_tax$idtax_good_n)) {

      cli::cli_alert_info("This taxa is not considered as synonym. No modification is thus done on its synonymy")
      comp_vals <- FALSE

    }else{

      new_id_diconame_good <- NA

      modif_types[1:length(modif_types)] <-
        paste(modif_types, "cancel_synonymy", sep="__")

    }

  }

  Q.syn2 <- FALSE
  if(nrow_query & !is.null(synonym_of)) {
    Q.syn <- TRUE

    ## checking if taxa selected is already a synonym of another taxa
    if(!is.na(query_tax$idtax_good_n)) {

      if(query_tax$idtax_good_n != query_tax$idtax_n) {

        query_taxa(ids = query_tax$idtax_good_n)
        # Q.syn <-
        #   utils::askYesNo("Taxa selected is already a synonym of this taxa. Are you sure you want to modify this?", default = FALSE)
        Q.syn <- choose_prompt(message =  "Taxa selected is already a synonym of this taxa. Are you sure you want to modify this?")
      }
    }

    if (Q.syn) {

      ## checking if others names are pointing to the selected taxa as synonyms
      syn_of_new_syn <-
        tbl(mydb_taxa, "table_taxa") %>%
        filter(idtax_good_n == !!query_tax$idtax_n) %>%
        collect()

      if(nrow(syn_of_new_syn) > 0) {

        cli::cli_alert_info("Some names are considered synonyms of the selected taxa:")

        print(syn_of_new_syn %>%
                dplyr::select(tax_fam, tax_gen, tax_esp, tax_rank01, tax_nam01, idtax_n, idtax_good_n))

        # Q.syn2 <-
        #   utils::askYesNo("Do you confirm to also modify the synonymies of these selected names?", default = FALSE)
        
        Q.syn2 <- choose_prompt(message =  "Do you confirm to also modify the synonymies of these selected names?")

        if(Q.syn2)
          ids_others_names_synonyms <-
          syn_of_new_syn$idtax_n

      }

      # if(Q.syn2) {

      if (!any(names(synonym_of) == "genus"))
        synonym_of$genus <- NULL
      if (!any(names(synonym_of) == "species"))
        synonym_of$species <- NULL
      if (!any(names(synonym_of) == "id"))
        synonym_of$id <- NULL

      new_syn <-
        query_taxa(genus = synonym_of$genus, species = synonym_of$species,
                   ids =  synonym_of$id, check_synonymy = F)

      if (nrow(new_syn) == 0) {

        cli::cli_alert_warning("No taxa found for new synonymy. Select one.")
        Q.syn <- FALSE

      }

      if(nrow(new_syn) > 1) {

        cli::cli_alert_warning("More than one taxa found for new synonymy. Select only one.")
        Q.syn <- FALSE

      }

      if(nrow(new_syn) == 1) {

        cli::cli_h1("Synonym of:")
        print(new_syn %>% as.data.frame())

        new_id_diconame_good <- new_syn$idtax_n

        modif_types[1:length(modif_types)] <-
          paste(modif_types, "new_synonymy", sep="__")



      }
    }

  } else {

    Q.syn <- TRUE

  }

  if(any(comp_vals) & Q.syn & nrow_query) {

    cat(paste("\n Number of rows selected to be updated :", nrow(query_tax), "\n"))

    if(ask_before_update) {

      # Q <-
      #   utils::askYesNo(msg = "Do you confirm you want to update these rows for selected fields?", default = FALSE)

      Q <- choose_prompt(message =  "Do you confirm you want to update these rows for selected fields?")
      
    } else{

      Q <- TRUE

    }

    if(Q) {

      if(add_backup) {

        query_tax <-
          query_tax %>%
          mutate(date_modified = Sys.Date()) %>%
          mutate(modif_type = modif_types) %>%
          dplyr::select(-tax_sp_level, -tax_infra_level, -tax_infra_level_auth)

        DBI::dbWriteTable(mydb_taxa, "followup_updates_table_taxa",
                          query_tax, append = TRUE, row.names = FALSE)

        if(Q.syn2) {
          syn_of_new_syn <-
            syn_of_new_syn %>%
            mutate(date_modified = Sys.Date()) %>%
            mutate(modif_type = modif_types)

          DBI::dbWriteTable(mydb_taxa, "followup_updates_table_taxa",
                            syn_of_new_syn, append = TRUE, row.names = FALSE)


        }
      }

      rs <-
        DBI::dbSendQuery(mydb_taxa, statement="UPDATE table_taxa SET tax_fam=$2, tax_gen=$3, tax_esp=$4, tax_order=$5, idtax_good_n=$6, tax_rank01=$7, tax_nam01=$8, introduced_status=$9, id_tax_famclass=$10, tax_rank=$11, tax_rankesp=$12 WHERE idtax_n = $1",
                         params= list(query_tax$idtax_n, # $1
                                      rep(ifelse(!is.null(new_tax_fam), new_tax_fam, query_tax$tax_fam), nrow(query_tax)), # $2
                                      rep(ifelse(!is.null(new_tax_gen), new_tax_gen, query_tax$tax_gen), nrow(query_tax)), # $3
                                      rep(ifelse(!is.null(new_tax_esp), new_tax_esp, query_tax$tax_esp), nrow(query_tax)), # $4
                                      rep(ifelse(!is.null(new_tax_order), new_tax_order, query_tax$tax_order), nrow(query_tax)), # $5
                                      rep(ifelse(!is.null(new_id_diconame_good), new_id_diconame_good, query_tax$idtax_good_n), nrow(query_tax)), # $6
                                      rep(ifelse(!is.null(new_tax_rank1), new_tax_rank1, query_tax$tax_rank01), nrow(query_tax)), # $7
                                      rep(ifelse(!is.null(new_tax_name1), new_tax_name1, query_tax$tax_nam01), nrow(query_tax)), # $8
                                      rep(ifelse(!is.null(new_introduced_status), as.character(new_introduced_status), query_tax$introduced_status), nrow(query_tax)), # $9
                                      rep(ifelse(!is.null(new_id_tax_famclass), new_id_tax_famclass, query_tax$id_tax_famclass), nrow(query_tax)), # $10
                                      rep(ifelse(!is.null(new_tax_rank), new_tax_rank, query_tax$tax_rank), nrow(query_tax)), # $11
                                      # rep(ifelse(!is.null(new_habit), new_habit, query_tax$a_habit), nrow(query_tax)),  # $12
                                      rep(ifelse(!is.null(new_tax_rankesp), new_tax_rankesp, query_tax$tax_rankesp), nrow(query_tax))))  # $12

      DBI::dbClearResult(rs)

      rs <-
        DBI::dbSendQuery(mydb_taxa, statement="SELECT *FROM table_taxa WHERE idtax_n = $1",
                         params=list(query_tax$idtax_n))
      if(show_results) print(DBI::dbFetch(rs))
      DBI::dbClearResult(rs)


      if(Q.syn2) {
        message("\n updating synonymies for others taxa")
        rs <-
          DBI::dbSendQuery(mydb_taxa, statement="UPDATE table_taxa SET idtax_good_n=$2 WHERE idtax_n = $1",
                           params= list(ids_others_names_synonyms, # $1
                                        rep(ifelse(!is.null(new_id_diconame_good),
                                                   new_id_diconame_good, syn_of_new_syn$idtax_good_n),
                                            nrow(syn_of_new_syn)) # $2
                           ))

        DBI::dbClearResult(rs)

        rs <-
          DBI::dbSendQuery(mydb_taxa, statement="SELECT *FROM table_taxa WHERE idtax_n = $1",
                           params=list(ids_others_names_synonyms))
        if(show_results) print(DBI::dbFetch(rs))
        DBI::dbClearResult(rs)


      }


    }

  }else{

    cli::cli_h2("No update")

    if(nrow(query_tax)==0) cli::cli_alert_warning("No update because no taxa found.")

    if(!any(comp_vals)) cli::cli_alert_warning("No update performed because no values are different.")

    if(!Q.syn) cli::cli_alert_warning("No update because new synonymy not correctly defined.")

    if(!nrow_query) cli::cli_alert_warning("No updates because none taxa were found based on query parameters (genus/species/family/id)")

  }

}






#' Update diconame data based on id of taxa
#'
#' Update diconame _ one or more entry at a time
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select vector string of new_data to be used for update
#' @param col_names_corresp vector string of corresponding fields to update
#' @param id_col integer indicate which name of col_names_select is the id for matching data
#' @param launch_update logical if TRUE updates are performed
#' @param add_backup logical whether backup of modified data should be recorded
#'
#'
#' @return No return value individuals updated
#' @export
update_dico_name_batch <- function(new_data,
                                   col_names_select = NULL,
                                   col_names_corresp = NULL,
                                   id_col = 1,
                                   launch_update = FALSE,
                                   add_backup = TRUE,
                                   ask_before_update = FALSE) {

  if (exists("mydb_taxa")) rm(mydb_taxa)
  if (!exists("mydb_taxa")) call.mydb.taxa()

  if (is.null(col_names_select)) {
    col_names_select <- names(new_data)
    cli::cli_alert_info("col_names_select is set as all names of new_data")
  }

  if (is.null(col_names_corresp)) {
    col_names_corresp <- col_names_select
    cli::cli_alert_info("col_names_corresp is set to names of col_names_select (it should be names of columns of table_taxa")
  }

  all_colnames_tx <-
    try_open_postgres_table(table = "table_taxa", con = mydb_taxa) %>%
    colnames()

  if (length(col_names_select) != length(col_names_corresp))
    stop("col_names_select and col_names_corresp should have same length")

  for (i in 1:length(col_names_select))
    if(!any(col_names_select[i] == colnames(new_data)))
      stop(paste(col_names_select[i], "not found in new_data"))

  for (i in 1:length(col_names_corresp))
    if(!any(col_names_corresp[i] == all_colnames_tx))
      stop(paste(col_names_corresp[i], "not found in table_taxa"))

  id_db <- col_names_corresp[id_col]

  if(!any(id_db == c("idtax_n")))
    stop("id for matching should be idtax_n")

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  output_matches <-
    .find_ids(
      dataset = new_data_renamed,
      col_new = col_names_corresp,
      id_col_nbr = id_col,
      type_data = "taxa"
    )

  matches_all <-
    output_matches[[2]]

  if(ask_before_update) {

    # confirm <-
    #   askYesNo(msg = 'Confirm update ?')
    confirm <- choose_prompt(message =  "Confirm update ?")

    print(confirm)

  }else{

    confirm <- TRUE

  }


  if(confirm) {

    for (i in 1:length(matches_all)) {

      field <- names(matches_all)[i]
      var_new <- paste0(field, "_new")
      matches <- matches_all[[i]]

      if(launch_update & nrow(matches) > 0) {

        matches <-
          matches %>%
          dplyr::select(id, dplyr::contains("_new"))
        matches <-
          .add_modif_field(matches)

        all_id_match <- dplyr::pull(dplyr::select(matches, id))

        if(add_backup) {

          quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))

          all_rows_to_be_updated <-
            dplyr::tbl(mydb_taxa, "table_taxa") %>%
            dplyr::filter(!!quo_var_id %in% all_id_match) %>%
            dplyr::collect()

          colnames_plots <-
            dplyr::tbl(mydb_taxa, "followup_updates_table_taxa")  %>%
            dplyr::select(-date_modified, -modif_type) %>%
            dplyr::collect() %>%
            dplyr::top_n(1) %>%
            colnames()

          all_rows_to_be_updated <-
            all_rows_to_be_updated %>%
            dplyr::select(dplyr::one_of(colnames_plots))

          all_rows_to_be_updated <-
            all_rows_to_be_updated %>%
            mutate(date_modified = Sys.Date()) %>%
            dplyr::mutate(modif_type = field)

          print(all_rows_to_be_updated %>%
                  dplyr::select(modif_type, date_modified))

          DBI::dbWriteTable(mydb_taxa, "followup_updates_table_taxa",
                            all_rows_to_be_updated, append = TRUE, row.names = FALSE)
        }

        ## create a temporary table with new data
        DBI::dbWriteTable(mydb_taxa, "temp_table", matches,
                          overwrite=T, fileEncoding = "UTF-8", row.names=F)

        query_up <-
          paste0("UPDATE table_taxa t1 SET (", field,", data_modif_d, data_modif_m, data_modif_y) = (t2.", var_new, ", t2.date_modif_d, t2.date_modif_m, t2.date_modif_y) FROM temp_table t2 WHERE t1.",
                 id_db," = t2.id")

        rs <-
          DBI::dbSendStatement(mydb_taxa, query_up)

        cat("\nRows updated", RPostgres::dbGetRowsAffected(rs))
        rs@sql
        DBI::dbClearResult(rs)


      } else {

        if (launch_update & nrow(matches) == 0)
          cat("\n No new values found")

      }
    }
  }

  if(ask_before_update & !confirm)
    cli::cli_alert_warning('No Update Done')

  return(matches_all)

}



# update_subplottype_list_table <- function(subplottype_searched = NULL,
#                                           id_subplotype = NULL,
#                                           new_subplottype = NULL,
#                                           new_maxallowedvalue = NULL,
#                                           new_minallowedvalue = NULL,
#                                           new_typedescription = NULL,
#                                           new_expectedunit = NULL,
#                                           ask_before_update = TRUE,
#                                           add_backup = TRUE,
#                                           show_results=TRUE) {
#   
#   if(!exists("mydb")) call.mydb()
#   
#   if(all(is.null(c(subplottype_searched, id_subplotype))))
#     stop("\n Provide subplottype_searched or id_subplotype to update")
#   
#   ### checking if at least one modification is asked
#   new_vals <- c(new_subplottype, new_maxallowedvalue, new_minallowedvalue,
#                 new_typedescription, new_expectedunit)
#   if(!any(!is.null(new_vals))) stop("\n No new values to be updated.")
#   
#   sql_query <- if (!is.null(subplottype_searched)) {
#     glue::glue_sql(
#       "SELECT * FROM subplotype_list WHERE type ILIKE {pattern}",
#       pattern = paste0("%", subplottype_searched, "%"),
#       .con = mydb
#     )
#   } else {
#     glue::glue_sql(
#       "SELECT * FROM subplotype_list WHERE id_subplotype = {id}",
#       id = id_subplotype,
#       .con = mydb
#     )
#   }
#   
#   query_subplotype <- func_try_fetch(mydb, sql_query)
#   
#   print(query_subplotype %>% as.data.frame())
#   
#   if (nrow(query_subplotype) > 1)
#     stop("more than one subplotype selected, select one")
#   if (nrow(query_subplotype) == 0)
#     stop("no subplotype selected, select one")
#   
#   modif_types <-
#     vector(mode = "character", length = nrow(query_subplotype))
#   
#   new_vals <-
#     dplyr::tibble(type = ifelse(!is.null(new_subplottype), as.character(new_subplottype),
#                                 query_subplotype$type),
#                   maxallowedvalue = ifelse(!is.null(new_maxallowedvalue), as.numeric(new_maxallowedvalue),
#                                            query_subplotype$maxallowedvalue),
#                   minallowedvalue = ifelse(!is.null(new_minallowedvalue), as.numeric(new_minallowedvalue),
#                                            query_subplotype$minallowedvalue),
#                   typedescription = ifelse(!is.null(new_typedescription), as.character(new_typedescription),
#                                            query_subplotype$typedescription),
#                   expectedunit = ifelse(!is.null(new_expectedunit), as.character(new_expectedunit),
#                                         query_subplotype$expectedunit))
#   
#   new_vals <- replace_NA(df = new_vals)
#   
#   sel_query_subplotype <-
#     dplyr::bind_rows(new_vals, query_subplotype %>%
#                        dplyr::select(-valuetype, -id_subplotype))
# 
#   
#   sel_query_subplotype <-
#     replace_NA(sel_query_subplotype)
#   
#   comp_vals <-
#     apply(sel_query_subplotype, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])
#   
#   if(any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]
#   
#   modif_types[1:length(modif_types)] <-
#     paste(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "),
#                            length(modif_types)), collapse ="__")
#   
#   if(any(comp_vals)) {
#     
#     cat(paste("\n Number of rows selected to be updated :", nrow(query_subplotype), "\n"))
#     
#     if (ask_before_update) {
#       
#       
#       .comp_print_vec(vec_1 = sel_query_subplotype[2,],
#                       vec_2 = sel_query_subplotype[1,])$comp_html
#       
#       Q <- choose_prompt(message =  "Do you confirm you want to update these rows for selected fields?")
#       
#     } else{
#       
#       Q <- TRUE
#       
#     }
#     
#     if (Q) {
#       
#       if (add_backup) {
#         message("no back up for this table yet")
#         
#       }
#       
#       
#       rs <-
#         DBI::dbSendQuery(mydb,
#                          statement="UPDATE subplotype_list SET type=$2, valuetype=$3, maxallowedvalue=$4, minallowedvalue=$5, typedescription=$6, expectedunit=$7 WHERE id_subplotype = $1", # , date_modif_d=$9 date_modif_m=$10, date_modif_y=$11
#                          params=list(query_subplotype$id_subplotype, # $1
#                                      rep(ifelse(!is.null(new_subplottype), as.character(new_subplottype),
#                                                 query_subplotype$type), nrow(query_subplotype)), # $2
#                                      rep(query_subplotype$valuetype, nrow(query_subplotype)), # $3
#                                      rep(ifelse(!is.null(new_maxallowedvalue), as.numeric(new_maxallowedvalue),
#                                                 query_subplotype$maxallowedvalue), nrow(query_subplotype)), # $4
#                                      rep(ifelse(!is.null(new_minallowedvalue), as.numeric(new_minallowedvalue),
#                                                 query_subplotype$minallowedvalue), nrow(query_subplotype)), # $5
#                                      rep(ifelse(!is.null(new_typedescription), as.character(new_typedescription),
#                                                 query_subplotype$typedescription), nrow(query_subplotype)), # $6
#                                      rep(ifelse(!is.null(new_expectedunit), as.character(new_expectedunit),
#                                                 query_subplotype$expectedunit), nrow(query_subplotype)))  # $7
#         )
#       
#       DBI::dbClearResult(rs)
#       
#       rs <-
#         DBI::dbSendQuery(mydb, statement="SELECT *FROM subplotype_list WHERE id_subplotype = $1",
#                          params=list(query_subplotype$id_subplotype))
#       if(show_results) print(DBI::dbFetch(rs))
#       DBI::dbClearResult(rs)
#       
#     }
#   } else{
#     
#     if(!any(comp_vals)) print("No update performed because no values are different.")
#   }
#   
#   
# }





# update_method_batch <- function(new_data,
#                                    col_names_select = NULL,
#                                    col_names_corresp = NULL,
#                                    id_col = 1,
#                                    launch_update = FALSE,
#                                    add_backup = TRUE) {
#   
#   if(!exists("mydb")) call.mydb()
#   
#   if (is.null(col_names_select)) {
#     col_names_select <- names(new_data)
#     cli::cli_alert_info("col_names_select is set as all names of new_data")
#   }
#   
#   if (is.null(col_names_corresp)) {
#     col_names_corresp <- col_names_select
#     cli::cli_alert_info("col_names_corresp is set to names of col_names_select (it should be names of columns of methodslist)")
#   }
#   
#   all_colnames_ind <-
#     dplyr::tbl(mydb, "methodslist") %>%
#     colnames()
#   
#   if(length(col_names_select) != length(col_names_corresp))
#     stop("col_names_select and col_names_corresp should have same length")
#   
#   for (i in 1:length(col_names_select))
#     if(!any(col_names_select[i] == colnames(new_data)))
#       stop(paste(col_names_select[i], "not found in new_data"))
#   
#   for (i in 1:length(col_names_corresp))
#     if(!any(col_names_corresp[i] == all_colnames_ind))
#       stop(paste(col_names_corresp[i], "not found in methodslist"))
#   
#   id_db <- col_names_corresp[id_col]
#   
#   if(!any(id_db == c("id_method"))) stop("id for matching should be id_method")
#   
#   new_data_renamed <-
#     .rename_data(dataset = new_data,
#                  col_old = col_names_select,
#                  col_new = col_names_corresp)
#   
#   # new_data_renamed <-
#   #   new_data %>%
#   #   dplyr::rename_at(dplyr::vars(col_names_select[-id_col]), ~ col_names_corresp[-id_col])
#   
#   # dataset = new_data_renamed
#   # col_new = col_names_corresp
#   # id_col_nbr = id_col
#   # type_data = "individuals"
#   
#   output_matches <- .find_ids(dataset = new_data_renamed,
#                               col_new = col_names_corresp,
#                               id_col_nbr = id_col,
#                               type_data = "methodslist")
#   
#   matches_all <-
#     output_matches[[2]]
#   
#   for (i in 1:length(matches_all)) {
#     
#     field <- names(matches_all)[i]
#     var_new <- paste0(field, "_new")
#     matches <- matches_all[[i]]
#     
#     if(launch_update & nrow(matches) > 0) {
#       
#       matches <-
#         matches %>%
#         dplyr::select(id, dplyr::contains("_new"))
#       # matches <-
#       #   .add_modif_field(matches)
#       
#       all_id_match <- dplyr::pull(dplyr::select(matches, id))
#       
#       # if(add_backup) {
#       #   
#       #   quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))
#       #   
#       #   all_rows_to_be_updated <-
#       #     dplyr::tbl(mydb, "data_liste_plots") %>%
#       #     dplyr::filter(!!quo_var_id %in% all_id_match) %>%
#       #     dplyr::collect()
#       #   
#       #   colnames_plots <-
#       #     dplyr::tbl(mydb, "followup_updates_liste_plots")  %>%
#       #     dplyr::select(-date_modified, -modif_type, -id_fol_up_plots) %>%
#       #     dplyr::collect() %>%
#       #     dplyr::top_n(1) %>%
#       #     colnames()
#       #   
#       #   all_rows_to_be_updated <-
#       #     all_rows_to_be_updated %>%
#       #     dplyr::select(dplyr::one_of(colnames_plots))
#       #   
#       #   all_rows_to_be_updated <-
#       #     all_rows_to_be_updated %>%
#       #     mutate(date_modified = Sys.Date()) %>%
#       #     mutate(modif_type = field)
#       #   
#       #   print(all_rows_to_be_updated %>%
#       #           dplyr::select(modif_type, date_modified))
#       #   
#       #   DBI::dbWriteTable(mydb, "followup_updates_liste_plots",
#       #                     all_rows_to_be_updated,
#       #                     append = TRUE,
#       #                     row.names = FALSE)
#       # }
#       
#       # if(any(names(matches) == "idtax_n_new"))
#       #   matches <-
#       #   matches %>%
#       #   dplyr::mutate(idtax_n_new == as.integer(idtax_n_new))
#       
#       ## create a temporary table with new data
#       DBI::dbWriteTable(mydb, "temp_table", matches,
#                         overwrite=T, fileEncoding = "UTF-8", row.names=F)
#       
#       query_up <-
#         paste0("UPDATE methodslist t1 SET ",field," = t2.",var_new, " FROM temp_table t2 WHERE t1.", id_db," = t2.id")
#       
#       rs <-
#         DBI::dbSendStatement(mydb, query_up)
#       
#       cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
#       rs@sql
#       DBI::dbClearResult(rs)
#       
#       cli::cli_alert_success("Successful update")
#       
#     } else{
#       
#       if (launch_update & nrow(matches) == 0)
#         cat("\n No new values found")
#       
#       if (!launch_update)
#         cli::cli_alert_danger("No update because launch_update is FALSE")
#       
#     }
#   }
#   
#   return(matches_all)
# }





# update_colnam <- function(colnam_searched = NULL,
#                           colnam_id = NULL,
#                           new_colnam = NULL,
#                           new_surname = NULL,
#                           new_family_name = NULL,
#                           new_nationality = NULL,
#                           new_contact = NULL,
#                           new_institute = NULL,
#                           ask_before_update = TRUE,
#                           add_backup = TRUE,
#                           show_results = TRUE)
# {
#   
#   if(!exists("mydb")) call.mydb()
#   
#   if(all(is.null(c(colnam_searched, colnam_id))))
#     stop("\n Provide colnam_searched or colnam_id to update")
#   
#   ### checking if at least one modification is asked
#   new_vals <- c(new_colnam, new_surname, new_family_name, new_contact, new_institute)
#   if(!any(!is.null(new_vals))) stop("\n No new values to be updated.")
#   
#   ### querying for entries to be modified
#   
#   queried_colnam <- query_colnam(id_colnam = colnam_id, pattern = colnam_searched)
#   
#   print(queried_colnam %>% as.data.frame())
#   if(nrow(queried_colnam)>1) stop("more than one colnam selected, select one")
#   if(nrow(queried_colnam)==0) stop("no colnam selected, select one")
#   
#   modif_types <-
#     vector(mode = "character", length = nrow(queried_colnam))
#   
#   new_vals <-
#     dplyr::tibble(colnam = ifelse(!is.null(new_colnam), as.character(new_colnam),
#                                   queried_colnam$colnam),
#                   family_name = ifelse(!is.null(new_family_name), as.character(new_family_name),
#                                        queried_colnam$family_name),
#                   surname = ifelse(!is.null(new_surname), as.character(new_surname),
#                                    queried_colnam$surname),
#                   nationality = ifelse(!is.null(new_nationality), as.character(new_nationality),
#                                        queried_colnam$nationality),
#                   contact = ifelse(!is.null(new_contact), as.character(new_contact),
#                                    queried_colnam$contact),
#                   institute = ifelse(!is.null(new_institute), as.character(new_institute),
#                                      queried_colnam$institute))
#   
#   new_vals <-
#     replace_NA(df = new_vals)
#   
#   sel_query_colnam <-
#     dplyr::bind_rows(new_vals, queried_colnam %>%
#                        dplyr::select(-id_table_colnam))
#   
#   sel_query_colnam <-
#     replace_NA(df = sel_query_colnam)
#   
#   comp_vals <-
#     apply(sel_query_colnam, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])
#   
#   if(any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]
#   
#   if(any(comp_vals)) {
#     
#     cat(paste("\n Number of rows selected to be updated :", nrow(queried_colnam), "\n"))
#     
#     if(ask_before_update) {
#       
#       
#       .comp_print_vec(vec_1 = sel_query_colnam[2,],
#                       vec_2 = sel_query_colnam[1,])
#       
#       Q <- choose_prompt(message =  "Do you confirm you want to update these rows for selected fields?")
#       
#     }else{
#       Q <- TRUE
#     }
#     
#     if(Q) {
#       
#       if(add_backup) {
#         message("no back up for this table yet")
#       }
#       
#       
#       rs <-
#         DBI::dbSendQuery(mydb,
#                          statement = "UPDATE table_colnam SET colnam=$2, family_name=$3, surname=$4, nationality=$5, contact=$6, institute=$7 WHERE id_table_colnam = $1",
#                          params = list(queried_colnam$id_table_colnam, # $1
#                                        rep(ifelse(!is.null(new_colnam), as.character(new_colnam),
#                                                   queried_colnam$colnam), nrow(queried_colnam)), # $2
#                                        rep(ifelse(!is.null(new_family_name), as.character(new_family_name),
#                                                   queried_colnam$family_name), nrow(queried_colnam)), # $3
#                                        rep(ifelse(!is.null(new_surname), as.character(new_surname),
#                                                   queried_colnam$surname), nrow(queried_colnam)), # $4
#                                        rep(ifelse(!is.null(new_nationality), as.character(new_nationality),
#                                                   queried_colnam$nationality), nrow(queried_colnam)), # $5
#                                        rep(ifelse(!is.null(new_contact), as.character(new_contact),
#                                                   queried_colnam$contact), nrow(queried_colnam)), # $6
#                                        rep(ifelse(!is.null(new_institute), as.character(new_institute),
#                                                   queried_colnam$institute), nrow(queried_colnam))) # $7
#         )
#       
#       DBI::dbClearResult(rs)
#       
#       rs <-
#         DBI::dbSendQuery(mydb, statement="SELECT *FROM table_colnam WHERE id_table_colnam = $1",
#                          params=list(queried_colnam$id_table_colnam))
#       if(show_results) print(DBI::dbFetch(rs))
#       DBI::dbClearResult(rs)
#       
#     }
#   }else{
#     
#     if(!any(comp_vals)) print("No update performed because no values are different.")
#   }
#   
#   
# }






#' Update plot data data
#'
#' Update plot data plot _ at a time
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data data frame data containing id and new values
#' @param col_names_select string plot name of the selected plots
#' @param col_names_corresp string of the selected plots
#' @param id_col integer indicate which name of col_names_select is the id for matching data
#' @param launch_update logical if TRUE updates are performed
#' @param add_backup logical whether backup of modified data should be recorded
#'
#'
#' @return No return value individuals updated
#' @export
update_link_specimens_batch <- function(new_data,
                                   col_names_select = NULL,
                                   col_names_corresp = NULL,
                                   id_col = 1,
                                   launch_update = FALSE,
                                   add_backup = TRUE) {
  
  mydb <- call.mydb()
  
  if (is.null(col_names_select)) {
    col_names_select <- names(new_data)
    cli::cli_alert_info("col_names_select is set as all names of new_data")
  }
  
  if (is.null(col_names_corresp)) {
    col_names_corresp <- col_names_select
    cli::cli_alert_info("col_names_corresp is set to names of col_names_select (it should be names of columns of data_link_specimens)")
  }
  
  all_colnames_ind <-
    try_open_postgres_table(table = "data_link_specimens", con = mydb) %>% 
    colnames()
  
  if(length(col_names_select) != length(col_names_corresp))
    stop("col_names_select and col_names_corresp should have same length")
  
  for (i in 1:length(col_names_select))
    if(!any(col_names_select[i] == colnames(new_data)))
      stop(paste(col_names_select[i], "not found in new_data"))
  
  for (i in 1:length(col_names_corresp))
    if(!any(col_names_corresp[i] == all_colnames_ind))
      stop(paste(col_names_corresp[i], "not found in data_liste_plots, check others tables, subplots features should be updated in data_liste_sub_plots table"))
  
  id_db <- col_names_corresp[id_col]
  
  if(!any(id_db == c("id_link_specimens"))) stop("id for matching should be id_link_specimens")
  
  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)
  
  # new_data_renamed <-
  #   new_data %>%
  #   dplyr::rename_at(dplyr::vars(col_names_select[-id_col]), ~ col_names_corresp[-id_col])
  
  # dataset = new_data_renamed
  # col_new = col_names_corresp
  # id_col_nbr = id_col
  # type_data = "individuals"
  
  output_matches <- .find_ids(dataset = new_data_renamed,
                              col_new = col_names_corresp,
                              id_col_nbr = id_col,
                              type_data = "data_link_specimens")
  
  matches_all <-
    output_matches[[2]]
  
  for (i in 1:length(matches_all)) {
    
    field <- names(matches_all)[i]
    var_new <- paste0(field, "_new")
    matches <- matches_all[[i]]
    
    if(launch_update & nrow(matches) > 0) {
      
      matches <-
        matches %>%
        dplyr::select(id, dplyr::contains("_new"))
      # matches <-
      #   .add_modif_field(matches)
      
      all_id_match <- dplyr::pull(dplyr::select(matches, id))
      
      # if(add_backup) {
      #   
      #   quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))
      #   
      #   all_rows_to_be_updated <-
      #     dplyr::tbl(mydb, "data_liste_plots") %>%
      #     dplyr::filter(!!quo_var_id %in% all_id_match) %>%
      #     dplyr::collect()
      #   
      #   colnames_plots <-
      #     dplyr::tbl(mydb, "followup_updates_liste_plots")  %>%
      #     dplyr::select(-date_modified, -modif_type, -id_fol_up_plots) %>%
      #     dplyr::collect() %>%
      #     dplyr::top_n(1) %>%
      #     colnames()
      #   
      #   all_rows_to_be_updated <-
      #     all_rows_to_be_updated %>%
      #     dplyr::select(dplyr::one_of(colnames_plots))
      #   
      #   all_rows_to_be_updated <-
      #     all_rows_to_be_updated %>%
      #     mutate(date_modified = Sys.Date()) %>%
      #     mutate(modif_type = field)
      #   
      #   print(all_rows_to_be_updated %>%
      #           dplyr::select(modif_type, date_modified))
      #   
      #   DBI::dbWriteTable(mydb, "followup_updates_liste_plots",
      #                     all_rows_to_be_updated,
      #                     append = TRUE,
      #                     row.names = FALSE)
      # }
      
      # if(any(names(matches) == "idtax_n_new"))
      #   matches <-
      #   matches %>%
      #   dplyr::mutate(idtax_n_new == as.integer(idtax_n_new))
      
      ## create a temporary table with new data
      DBI::dbWriteTable(mydb, "temp_table", matches,
                        overwrite=T, fileEncoding = "UTF-8", row.names=F)
      
      query_up <-
        paste0("UPDATE data_link_specimens t1 SET ",field," = t2.",var_new, " FROM temp_table t2 WHERE t1.", id_db," = t2.id")
      
      rs <-
        DBI::dbSendStatement(mydb, query_up)
      
      cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
      rs@sql
      DBI::dbClearResult(rs)
      
      cli::cli_alert_success("Successful update")
      
    } else{
      
      if (launch_update & nrow(matches) == 0)
        cat("\n No new values found")
      
      if (!launch_update)
        cli::cli_alert_danger("No update because launch_update is FALSE")
      
    }
  }
  return(matches_all)
}



# =============================================================================
# SYSTÈME COMPLET UPDATE_RECORDS AVEC MATCHING INTERACTIF
# =============================================================================

# =============================================================================
# PARTIE 1 : MATCHING INTERACTIF
# =============================================================================

calculate_similarities <- function(input, compared_table, column_name) {
  compared_values <- compared_table %>% dplyr::pull(!!sym(column_name))
  sims <- stringdist::stringsim(tolower(input), tolower(compared_values), method = "jw")
  compared_table %>% dplyr::mutate(similarity = sims) %>% dplyr::arrange(dplyr::desc(similarity))
}

find_similar_strings <- function(input, compared_table, column_name, threshold = 0.6) {
  all_similarities <- calculate_similarities(input, compared_table, column_name)
  suggestions <- all_similarities %>% dplyr::filter(similarity >= threshold)
  if (nrow(suggestions) == 0) {
    cli::cli_alert_warning("No suggestions above threshold {threshold}")
  }
  return(suggestions)
}

display_suggestions <- function(suggestions, page, page_size, original_value) {
  if (nrow(suggestions) == 0) {
    cli::cli_alert_warning("No suggestions available")
    cli::cli_text("Original value: {.val {original_value}}")
    return()
  }
  
  total_pages <- ceiling(nrow(suggestions) / page_size)
  start_idx <- (page - 1) * page_size + 1
  end_idx <- min(page * page_size, nrow(suggestions))
  
  page_data <- suggestions %>%
    dplyr::slice(start_idx:end_idx) %>%
    dplyr::mutate(ID = dplyr::row_number() + start_idx - 1, .before = 1)
  
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    table_display <- page_data %>%
      kableExtra::kable(format = "html", escape = FALSE) %>%
      kableExtra::kable_styling("striped", full_width = FALSE)
    print(table_display)
  } else {
    print(page_data, n = page_size)
  }
  
  cli::cli_text("")
  cli::cli_text("Searching for: {.val {original_value}}")
  cli::cli_text("Page {page}/{total_pages} ({nrow(suggestions)} total matches)")
  cli::cli_text("")
}

handle_grep_search <- function(full_table, column_name, id_column, original_value, allow_add, table_name, con) {
  pattern <- readline(prompt = "Enter search pattern (regex): ")
  
  if (pattern == "") {
    cli::cli_alert_info("Empty pattern, returning to suggestions")
    return(NULL)
  }
  
  grep_results <- full_table %>%
    dplyr::filter(grepl(pattern, !!sym(column_name), ignore.case = TRUE))
  
  if (nrow(grep_results) == 0) {
    cli::cli_alert_warning("No matches found for pattern: {.val {pattern}}")
    readline(prompt = "Press Enter to continue...")
    return(NULL)
  }
  
  cli::cli_alert_success("Found {nrow(grep_results)} match(es)")
  
  selected_id <- interactive_selection_loop(
    value_to_search = original_value,
    initial_suggestions = grep_results %>% dplyr::mutate(similarity = NA),
    full_table = full_table,
    column_name = column_name,
    id_column = id_column,
    allow_add = allow_add,
    table_name = table_name,
    con = con
  )
  
  return(selected_id)
}

add_new_reference_value <- function(original_value, column_name, id_column, table_name, con) {
  cli::cli_h3("Add new value to {.val {table_name}}")
  cli::cli_alert_info("Original value: {.val {original_value}}")
  
  confirm <- readline(prompt = "Confirm addition? (y/n): ")
  if (tolower(confirm) != "y") {
    cli::cli_alert_info("Addition cancelled")
    return(NA)
  }
  
  new_value <- readline(prompt = glue::glue("Enter standardized '{column_name}' value: "))
  
  if (new_value == "") {
    cli::cli_alert_warning("Empty value, addition cancelled")
    return(NA)
  }
  
  new_record <- tibble::tibble(!!sym(column_name) := new_value)
  
  table_cols <- DBI::dbListFields(con, table_name)
  other_cols <- setdiff(table_cols, c(id_column, column_name, "date_creation_d", "date_creation_m", 
                                      "date_creation_y", "user_creation", "date_modif_d", "date_modif_m", 
                                      "date_modif_y", "user_modif"))
  
  for (col in other_cols) {
    col_value <- readline(prompt = glue::glue("Enter '{col}' (or Enter to skip): "))
    if (col_value != "") {
      new_record[[col]] <- col_value
    }
  }
  
  tryCatch({
    DBI::dbWriteTable(con, table_name, new_record, append = TRUE, row.names = FALSE)
    cli::cli_alert_success("New value added to {.val {table_name}}")
    
    new_id <- dplyr::tbl(con, table_name) %>%
      dplyr::filter(!!sym(column_name) == new_value) %>%
      dplyr::select(!!sym(id_column)) %>%
      dplyr::collect() %>%
      dplyr::pull(!!sym(id_column))
    
    if (length(new_id) > 0) {
      cli::cli_alert_success("New ID: {new_id[1]}")
      return(new_id[1])
    } else {
      cli::cli_alert_danger("Could not retrieve new ID")
      return(NA)
    }
  }, error = function(e) {
    cli::cli_alert_danger("Failed to add new value: {e$message}")
    return(NA)
  })
}

interactive_selection_loop <- function(value_to_search, initial_suggestions, full_table, column_name, 
                                       id_column, allow_add = FALSE, table_name = NULL, con = NULL) {
  current_suggestions <- initial_suggestions
  selected_id <- NULL
  page <- 1
  page_size <- 10
  
  while (is.null(selected_id)) {
    display_suggestions(current_suggestions, page, page_size, value_to_search)
    
    prompt_msg <- "Choose ID"
    if (nrow(current_suggestions) > page_size) {
      prompt_msg <- paste0(prompt_msg, " (Enter = next page)")
    }
    prompt_msg <- paste0(prompt_msg, " [G = grep search, 0 = no match")
    if (allow_add) {
      prompt_msg <- paste0(prompt_msg, ", A = add new")
    }
    prompt_msg <- paste0(prompt_msg, "]: ")
    
    choice <- readline(prompt = prompt_msg)
    
    if (choice == "") {
      page <- page + 1
      total_pages <- ceiling(nrow(current_suggestions) / page_size)
      if (page > total_pages) page <- 1
      
    } else if (toupper(choice) == "G") {
      selected_id <- handle_grep_search(full_table, column_name, id_column, value_to_search, 
                                        allow_add, table_name, con)
      if (!is.null(selected_id)) break
      
    } else if (toupper(choice) == "A" && allow_add) {
      selected_id <- add_new_reference_value(value_to_search, column_name, id_column, table_name, con)
      
    } else if (choice == "0") {
      cli::cli_alert_warning("Skipping value: {.val {value_to_search}}")
      selected_id <- NA
      
    } else {
      choice_num <- suppressWarnings(as.integer(choice))
      
      if (!is.na(choice_num) && choice_num > 0 && choice_num <= nrow(current_suggestions)) {
        selected_id <- current_suggestions %>%
          dplyr::slice(choice_num) %>%
          dplyr::pull(!!sym(id_column))
        
        selected_value <- current_suggestions %>%
          dplyr::slice(choice_num) %>%
          dplyr::pull(!!sym(column_name))
        
        cli::cli_alert_success("Matched '{value_to_search}' → '{selected_value}' (ID: {selected_id})")
      } else {
        cli::cli_alert_warning("Invalid choice. Try again.")
      }
    }
  }
  
  return(selected_id)
}

resolve_value_interactively <- function(value_to_search, lookup_table, column_name, id_column,
                                        similarity_threshold = 0.6, allow_add = FALSE, 
                                        table_name = NULL, con = NULL) {
  cli::cli_h3("Resolving: {.val {value_to_search}}")
  
  perfect_match <- lookup_table %>%
    dplyr::filter(tolower(!!sym(column_name)) == tolower(value_to_search))
  
  if (nrow(perfect_match) > 0) {
    matched_id <- perfect_match %>% dplyr::slice(1) %>% dplyr::pull(!!sym(id_column))
    matched_value <- perfect_match %>% dplyr::slice(1) %>% dplyr::pull(!!sym(column_name))
    cli::cli_alert_success("Perfect match: '{value_to_search}' → '{matched_value}' (ID: {matched_id})")
    return(matched_id)
  }
  
  suggestions <- find_similar_strings(value_to_search, lookup_table, column_name, similarity_threshold)
  
  if (nrow(suggestions) > 0) {
    cli::cli_alert_info("No perfect match. Found {nrow(suggestions)} similar value(s)")
  } else {
    cli::cli_alert_warning("No similar values found above threshold")
    suggestions <- lookup_table %>% dplyr::slice_head(n = 20) %>% dplyr::mutate(similarity = NA)
  }
  
  selected_id <- interactive_selection_loop(
    value_to_search = value_to_search,
    initial_suggestions = suggestions,
    full_table = lookup_table,
    column_name = column_name,
    id_column = id_column,
    allow_add = allow_add,
    table_name = table_name,
    con = con
  )
  
  return(selected_id)
}

resolve_multiple_values <- function(missing_values, lookup_table, column_name, id_column,
                                    similarity_threshold = 0.6, allow_add = FALSE,
                                    table_name = NULL, con = NULL) {
  unique_values <- unique(missing_values)
  cli::cli_h2("Interactive matching: {length(unique_values)} value(s) to resolve")
  
  resolved_ids <- rep(NA, length(unique_values))
  names(resolved_ids) <- unique_values
  
  for (i in seq_along(unique_values)) {
    value <- unique_values[i]
    cli::cli_text("")
    cli::cli_rule("Value {i}/{length(unique_values)}")
    
    resolved_id <- resolve_value_interactively(
      value_to_search = value,
      lookup_table = lookup_table,
      column_name = column_name,
      id_column = id_column,
      similarity_threshold = similarity_threshold,
      allow_add = allow_add,
      table_name = table_name,
      con = con
    )
    
    resolved_ids[value] <- resolved_id
  }
  
  cli::cli_text("")
  cli::cli_h3("Resolution summary")
  n_resolved <- sum(!is.na(resolved_ids))
  n_skipped <- sum(is.na(resolved_ids))
  
  cli::cli_alert_success("Resolved: {n_resolved}/{length(unique_values)}")
  if (n_skipped > 0) {
    cli::cli_alert_warning("Skipped: {n_skipped}/{length(unique_values)}")
  }
  
  return(resolved_ids)
}

reverse_map_metadata_interactive <- function(data, config, con, interactive = TRUE,
                                             similarity_threshold = 0.6, allow_add = FALSE) {
  if (is.null(config$metadata_mappings)) return(data)
  
  mappings <- config$metadata_mappings
  cols_in_data <- names(data)
  
  for (friendly_col in names(mappings)) {
    if (!friendly_col %in% cols_in_data) next
    
    mapping_info <- mappings[[friendly_col]]
    cli::cli_h2("Resolving column: {.field {friendly_col}}")
    
    lookup_tbl <- dplyr::tbl(con, mapping_info$lookup_table) %>%
      dplyr::select(!!sym(mapping_info$lookup_key), !!sym(mapping_info$lookup_value)) %>%
      dplyr::collect()
    
    data_clean <- data %>%
      dplyr::select(-!!sym(friendly_col))
    
    data_with_lookup <- data_clean %>%
      dplyr::mutate(.original_value = data[[friendly_col]], .lower_value = tolower(data[[friendly_col]])) %>%
      dplyr::left_join(
        lookup_tbl %>% dplyr::mutate(.lower_lookup = tolower(!!sym(mapping_info$lookup_value))),
        by = c(".lower_value" = ".lower_lookup")
      )
    
    missing_values <- data_with_lookup %>%
      dplyr::filter(is.na(!!sym(mapping_info$lookup_key))) %>%
      dplyr::pull(.original_value)
    
    if (length(missing_values) == 0) {
      cli::cli_alert_success("All values matched in '{friendly_col}'")
      data <- data_with_lookup %>%
        dplyr::select(-dplyr::any_of(c(".original_value", ".lower_value", ".lower_lookup", 
                                       mapping_info$lookup_value))) %>%
        dplyr::rename(!!sym(mapping_info$id_col) := !!sym(mapping_info$lookup_key))
      next
    }
    
    cli::cli_alert_warning("{length(missing_values)} unique value(s) not found in '{friendly_col}'")
    
    if (!interactive) {
      cli::cli_abort("Missing values in '{friendly_col}': {paste(unique(missing_values), collapse = ', ')}")
    }
    
    resolved_ids <- resolve_multiple_values(
      missing_values = missing_values,
      lookup_table = lookup_tbl,
      column_name = mapping_info$lookup_value,
      id_column = mapping_info$lookup_key,
      similarity_threshold = similarity_threshold,
      allow_add = allow_add,
      table_name = mapping_info$lookup_table,
      con = con
    )
    
    for (orig_value in names(resolved_ids)) {
      resolved_id <- resolved_ids[orig_value]
      if (!is.na(resolved_id)) {
        data_with_lookup <- data_with_lookup %>%
          dplyr::mutate(
            !!sym(mapping_info$lookup_key) := dplyr::if_else(
              .original_value == orig_value, resolved_id, !!sym(mapping_info$lookup_key)
            )
          )
      }
    }
    
    remaining_nas <- data_with_lookup %>% dplyr::filter(is.na(!!sym(mapping_info$lookup_key)))
    
    if (nrow(remaining_nas) > 0) {
      cli::cli_alert_warning("{nrow(remaining_nas)} row(s) still unresolved in '{friendly_col}' (will be NA)")
    }
    
    data <- data_with_lookup %>%
      dplyr::select(-dplyr::any_of(c(".original_value", ".lower_value", ".lower_lookup",
                                     mapping_info$lookup_value))) %>%
      dplyr::rename(!!sym(mapping_info$id_col) := !!sym(mapping_info$lookup_key))
  }
  
  return(data)
}

is_admin_user <- function() {
  user <- Sys.getenv("USER")
  admin_users <- c("gdauby", "admin", "root")
  is_admin <- user %in% admin_users
  if (is_admin) cli::cli_alert_info("Admin permissions: enabled")
  return(is_admin)
}

# =============================================================================
# PARTIE 2 : SYSTÈME UPDATE_RECORDS
# =============================================================================

get_column_routing <- function(table_type, con) {
  configs <- list(
    individuals = list(
      table = "data_individuals",
      id_column = "id_n",
      backup_table = "followup_updates_individuals",
      direct_columns = get_table_columns("data_individuals", con),
      feature_columns = get_available_individual_features(con),
      metadata_mappings = get_metadata_mappings_individuals(con)
    ),
    plots = list(
      table = "data_liste_plots",
      id_column = "id_liste_plots",
      backup_table = "followup_updates_liste_plots",
      direct_columns = get_table_columns("data_liste_plots", con),
      subplot_features = get_available_subplot_types(con),
      metadata_mappings = get_metadata_mappings_plots(con)
    ),
    individual_features = list(
      table = "data_traits_measures",
      id_column = "id_trait_measures",
      backup_table = "followup_updates_traits_measures",
      direct_columns = c("references", "year", "month", "day", "measurementremarks", 
                         "measurementmethod", "traitvalue", "traitvalue_char", "issue"),
      feature_columns = c()
    ),
    subplot_features = list(
      table = "data_liste_sub_plots",
      id_column = "id_sub_plots",
      backup_table = NULL,
      direct_columns = c("year", "month", "day", "typevalue", "typevalue_char", "issue"),
      feature_columns = c()
    ),
    individual_features_metadata = list(
      table = "data_ind_measures_feat",
      id_column = "id_ind_meas_feat",
      foreign_key = "id_trait_measures",
      backup_table = NULL,
      direct_columns = c("typevalue", "typevalue_char"),
      feature_columns = c(),
      has_table_references = TRUE
    ),
    methodslist = list(
      table = "methodslist",
      id_column = "id_method",
      backup_table = NULL,
      direct_columns = c("method", "description_method", "area_min", "area_max", "area"),
      feature_columns = c(),
      has_table_references = FALSE
    ),
    table_colnam = list(
      table = "table_colnam",
      id_column = "id_table_colnam",
      backup_table = NULL,
      direct_columns = c("colnam", "family_name", "surname", "nationality", "institute", "contact"),
      feature_columns = c(),
      has_table_references = FALSE
    ),
    traitlist = list(
      table = "traitlist",
      id_column = "id_trait",
      backup_table = NULL,
      direct_columns = c("trait", "maxallowedvalue", "minallowedvalue", "traitdescription", "expectedunit", "comments"),
      feature_columns = c(),
      has_table_references = FALSE
    ),
    subplotype_list = list(
      table = "subplotype_list",
      id_column = "id_subplotype",
      backup_table = NULL,
      direct_columns = c("type", "maxallowedvalue", "minallowedvalue", "typedescription", "expectedunit", "comments"),
      feature_columns = c(),
      has_table_references = FALSE
    )
  )
  
  if (!table_type %in% names(configs)) {
    cli::cli_abort("Unknown table_type: {.val {table_type}}")
  }
  
  configs[[table_type]]
}

get_table_columns <- function(table_name, con) {
  cols <- DBI::dbListFields(con, table_name)
  
  system_cols <- c("date_creation_d", "date_creation_m", "date_creation_y",
                   "date_modif_d", "date_modif_m", "date_modif_y",
                   "user_creation", "user_modif")
  
  if (table_name == "data_liste_plots") {
    system_cols <- c(system_cols,
                     "team_leader", "province", "region_name2", "region_name1",
                     "additional_people", "data_provider", "co_authorship",
                     "forest_type", "area_plot", "topo_comment", "notes")
  }
  
  if (table_name == "data_individuals") {
    system_cols <- c(system_cols,
                     "position_x", "position_y", "tree_height", "observations")
  }
  
  setdiff(cols, system_cols)
}

get_available_individual_features <- function(con) {
  dplyr::tbl(con, "traitlist") %>%
    dplyr::select(trait) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull(trait)
}

get_available_subplot_types <- function(con) {
  dplyr::tbl(con, "subplotype_list") %>%
    dplyr::select(type) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull(type)
}

get_metadata_mappings_individuals <- function(con) {
  list(
    country = list(
      id_col = "id_country",
      lookup_table = "table_countries",
      lookup_key = "id_country",
      lookup_value = "country"
    ),
    method = list(
      id_col = "id_method",
      lookup_table = "methodslist",
      lookup_key = "id_method",
      lookup_value = "method"
    )
  )
}

get_metadata_mappings_plots <- function(con) {
  list(
    country = list(
      id_col = "id_country",
      lookup_table = "table_countries",
      lookup_key = "id_country",
      lookup_value = "country"
    )
  )
}

reverse_map_metadata <- function(data, config, con, interactive = TRUE, similarity_threshold = 0.6) {
  if (is.null(config$metadata_mappings)) return(data)
  
  allow_add <- is_admin_user()
  
  if (interactive) {
    return(
      reverse_map_metadata_interactive(data, config, con, interactive, similarity_threshold, allow_add)
    )
  }
  
  mappings <- config$metadata_mappings
  cols_in_data <- names(data)
  
  for (friendly_col in names(mappings)) {
    if (!friendly_col %in% cols_in_data) next
    
    mapping_info <- mappings[[friendly_col]]
    cli::cli_alert_info("Resolving '{friendly_col}' to '{mapping_info$id_col}'")
    
    lookup_tbl <- dplyr::tbl(con, mapping_info$lookup_table) %>%
      dplyr::select(!!sym(mapping_info$lookup_key), !!sym(mapping_info$lookup_value)) %>%
      dplyr::collect()
    
    data <- data %>%
      dplyr::left_join(lookup_tbl, by = setNames(mapping_info$lookup_value, friendly_col)) %>%
      dplyr::rename(!!sym(mapping_info$id_col) := !!sym(mapping_info$lookup_key))
    
    if (any(is.na(data[[mapping_info$id_col]]))) {
      missing <- unique(data[[friendly_col]][is.na(data[[mapping_info$id_col]])])
      cli::cli_abort("Values not found in '{friendly_col}': {paste(missing, collapse=', ')}")
    }
    
    data <- data %>% dplyr::select(-!!sym(friendly_col))
  }
  
  return(data)
}

reverse_map_table_references <- function(data, con) {
  if (!"valuetype" %in% names(data)) return(data)
  
  table_refs <- data %>% dplyr::filter(grepl("^table_", valuetype))
  if (nrow(table_refs) == 0) return(data)
  
  for (vtype in unique(table_refs$valuetype)) {
    table_name <- sub("^table_", "", vtype)
    
    if (!DBI::dbExistsTable(con, table_name)) next
    
    cols <- DBI::dbListFields(con, table_name)
    id_col <- cols[grepl("^id_", cols)][1]
    value_col <- cols[!grepl("^id_|^date_|^user_", cols)][1]
    
    if (is.na(id_col) || is.na(value_col)) next
    
    lookup_tbl <- dplyr::tbl(con, table_name) %>%
      dplyr::select(!!sym(id_col), !!sym(value_col)) %>%
      dplyr::collect()
    
    rows_to_resolve <- which(data$valuetype == vtype & !is.na(data$typevalue_char) & is.na(data$typevalue))
    
    for (i in rows_to_resolve) {
      char_val <- data$typevalue_char[i]
      matched_id <- lookup_tbl %>% dplyr::filter(!!sym(value_col) == char_val) %>% dplyr::pull(!!sym(id_col))
      
      if (length(matched_id) == 1) {
        data$typevalue[i] <- matched_id
        data$typevalue_char[i] <- NA
      }
    }
  }
  
  return(data)
}

#' Display a single change with visual comparison
#'
#' @param id Record ID
#' @param column Column name
#' @param old_value Old value
#' @param new_value New value
#' @param id_column Name of ID column
#'
#' @keywords internal
.display_change <- function(id, column, old_value, new_value, id_column) {
  
  # Format values (handle NA, NULL, empty strings)
  format_value <- function(x) {
    if (is.na(x) || is.null(x) || x == "") {
      return(crayon::silver("<empty>"))
    }
    # Truncate long values
    if (nchar(as.character(x)) > 50) {
      return(paste0(substr(x, 1, 47), "..."))
    }
    return(as.character(x))
  }
  
  old_fmt <- format_value(old_value)
  new_fmt <- format_value(new_value)
  
  # Display with visual arrow
  cli::cli_text(
    "  {crayon::cyan(id_column)}: {crayon::bold(id)} | ",
    "{crayon::yellow(column)}: ",
    crayon::red(old_fmt),
    " {crayon::silver('→')} ",
    crayon::green(new_fmt)
  )
}


#' Display multiple changes grouped by column
#'
#' @param changes_df Dataframe with columns: id_column_value, column, old_value, new_value
#' @param id_column Name of ID column
#' @param max_display Maximum number of changes to display per column
#'
#' @keywords internal
.display_changes_grouped <- function(changes_df, id_column, max_display = 10) {
  
  for (col in unique(changes_df$column)) {
    col_changes <- changes_df %>% filter(column == col)
    n_changes <- nrow(col_changes)
    
    cli::cli_alert_success("{col}: {crayon::bold(n_changes)} change(s)")
    
    # Display individual changes
    if (n_changes <= max_display) {
      # Show all changes
      for (i in 1:n_changes) {
        .display_change(
          id = col_changes[[id_column]][i],
          column = col,
          old_value = col_changes$old_value[i],
          new_value = col_changes$new_value[i],
          id_column = id_column
        )
      }
    } else {
      # Show first few changes + summary
      for (i in 1:max_display) {
        .display_change(
          id = col_changes[[id_column]][i],
          column = col,
          old_value = col_changes$old_value[i],
          new_value = col_changes$new_value[i],
          id_column = id_column
        )
      }
      cli::cli_text(
        crayon::silver("  ... and {n_changes - max_display} more change(s)")
      )
    }
    cli::cli_text("")  # Empty line between columns
  }
}


#' Detect changes in direct columns with visual display
#'
#' @param data Tibble with new data
#' @param columns Vector of column names to check
#' @param config Configuration list from get_column_routing()
#' @param con Database connection
#' @param max_display Maximum number of changes to display per column (default: 10)
#'
#' @return Tibble with changes or NULL if no changes
#' @export
detect_direct_changes <- function(data, columns, config, con, max_display = 10) {
  if (length(columns) == 0) return(NULL)
  
  ids <- data[[config$id_column]]
  
  # Fetch current data from database
  current_data <- dplyr::tbl(con, config$table) %>%
    dplyr::filter(!!sym(config$id_column) %in% ids) %>%
    dplyr::select(!!sym(config$id_column), dplyr::all_of(columns)) %>%
    dplyr::collect()
  
  # Join to compare
  comparison <- data %>%
    dplyr::select(!!sym(config$id_column), dplyr::all_of(columns)) %>%
    dplyr::inner_join(current_data, by = config$id_column, suffix = c("_new", "_old"))
  
  changes_list <- list()
  
  # Detect changes for each column
  for (col in columns) {
    old_col <- paste0(col, "_old")
    new_col <- paste0(col, "_new")
    
    # Find rows where value changed
    changed_rows <- comparison %>%
      dplyr::filter(
        !is.na(!!sym(new_col)) & 
          (is.na(!!sym(old_col)) | !!sym(old_col) != !!sym(new_col))
      )
    
    if (nrow(changed_rows) > 0) {
      changes_list[[col]] <- changed_rows %>%
        dplyr::select(
          !!sym(config$id_column), 
          old_value = !!sym(old_col), 
          new_value = !!sym(new_col)
        ) %>%
        dplyr::mutate(
          column = col,
          old_value = as.character(old_value),
          new_value = as.character(new_value)
        )
    }
  }
  
  if (length(changes_list) == 0) {
    cli::cli_alert_info("No changes detected in direct columns")
    return(NULL)
  }
  
  # Combine all changes
  all_changes <- dplyr::bind_rows(changes_list)
  
  # Display changes grouped by column with visual comparison
  .display_changes_grouped(
    changes_df = all_changes,
    id_column = config$id_column,
    max_display = max_display
  )
  
  # Return the changes dataframe
  return(all_changes)
}

detect_feature_changes <- function(data, feature_columns, config, table_type, con) {
  if (length(feature_columns) == 0) return(list())
  
  results <- list()
  
  for (feat in feature_columns) {
    cli::cli_alert_info("Checking feature: {feat}")
    
    if (grepl("_census_\\d+$", feat)) {
      base_feat <- sub("_census_\\d+$", "", feat)
      census_num <- sub(".*_census_(\\d+)$", "\\1", feat)
      
      cli::cli_alert_danger("Cannot update '{feat}': CENSUS-SPECIFIC feature")
      cli::cli_alert_warning("Census-suffixed columns cannot be updated directly")
      cli::cli_text("")
      cli::cli_alert_info("To update:")
      cli::cli_bullets(c(
        "i" = "Extract: query_individual_features(..., include_multi_census = TRUE)",
        "i" = "Filter to census {census_num} for '{base_feat}'",
        "i" = "Update via: update_records(..., 'individual_features')"
      ))
      
      results[[feat]] <- list(error = "census_specific", base_feature = base_feat, census = census_num)
      next
    }
    
    if (table_type == "individuals") {
      ids <- data[[config$id_column]]
      
      counts <- dplyr::tbl(con, "data_traits_measures") %>%
        dplyr::inner_join(dplyr::tbl(con, "traitlist"), by = c("traitid" = "id_trait")) %>%
        dplyr::filter(id_data_individuals %in% ids, trait == feat) %>%
        dplyr::group_by(id_data_individuals) %>%
        dplyr::summarise(n_measures = n()) %>%
        dplyr::collect()
      
      multi <- counts %>% dplyr::filter(n_measures > 1)
      
      if (nrow(multi) > 0) {
        cli::cli_alert_danger("Cannot update '{feat}': {nrow(multi)} record(s) have multiple measurements")
        cli::cli_alert_warning("Value is an AGGREGATION")
        cli::cli_text("")
        cli::cli_alert_info("To update:")
        cli::cli_bullets(c(
          "i" = "Extract: query_individual_features(individual_ids = ..., format = 'long')",
          "i" = "Modify specific measurements",
          "i" = "Update via: update_records(..., 'individual_features')"
        ))
        results[[feat]] <- list(error = "multiple_measurements", n_records = nrow(multi))
        next
      }
      
      current_values <- dplyr::tbl(con, "data_traits_measures") %>%
        dplyr::inner_join(dplyr::tbl(con, "traitlist"), by = c("traitid" = "id_trait")) %>%
        dplyr::filter(id_data_individuals %in% ids, trait == feat) %>%
        dplyr::group_by(id_data_individuals) %>%
        dplyr::summarise(
          current_value_num = min(traitvalue, na.rm = TRUE),
          current_value_char = str_squish(min(traitvalue_char, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::collect()
      
      comparison <- data %>%
        dplyr::select(id_data_individuals = !!sym(config$id_column), new_value = !!sym(feat)) %>%
        dplyr::left_join(current_values, by = "id_data_individuals") %>%
        dplyr::filter(
          !is.na(new_value) & 
            ((!is.na(current_value_num) & is.numeric(new_value) & current_value_num != new_value) |
               (!is.na(current_value_char) & current_value_char != as.character(new_value)) |
               (is.na(current_value_num) & is.na(current_value_char)))
        )
      
      if (nrow(comparison) == 0) {
        cli::cli_alert_info("{feat}: No changes detected")
        next
      }
      
      cli::cli_alert_success("{feat}: {nrow(comparison)} change(s) detected")
      results[[feat]] <- list(success = TRUE, n_changes = nrow(comparison))
      
    } else if (table_type == "plots") {
      ids <- data[[config$id_column]]
      
      counts <- dplyr::tbl(con, "data_liste_sub_plots") %>%
        dplyr::inner_join(dplyr::tbl(con, "subplotype_list"), by = c("id_type_sub_plot" = "id_subplotype")) %>%
        dplyr::filter(id_table_liste_plots %in% ids, type == feat) %>%
        dplyr::group_by(id_table_liste_plots) %>%
        dplyr::summarise(n_subplots = n()) %>%
        dplyr::collect()
      
      multi <- counts %>% dplyr::filter(n_subplots > 1)
      
      if (nrow(multi) > 0) {
        cli::cli_alert_danger("Cannot update '{feat}': {nrow(multi)} plot(s) have multiple subplots")
        cli::cli_alert_warning("Value is an AGGREGATION")
        cli::cli_text("")
        cli::cli_alert_info("To update:")
        cli::cli_bullets(c(
          "i" = "Extract: query_plot_features(plot_ids = ..., format = 'long')",
          "i" = "Modify specific subplots",
          "i" = "Update via: update_records(..., 'subplot_features')"
        ))
        results[[feat]] <- list(error = "multiple_subplots", n_records = nrow(multi))
        next
      }
      
      current_values <- dplyr::tbl(con, "data_liste_sub_plots") %>%
        dplyr::inner_join(dplyr::tbl(con, "subplotype_list"), by = c("id_type_sub_plot" = "id_subplotype")) %>%
        dplyr::filter(id_table_liste_plots %in% ids, type == feat) %>%
        dplyr::group_by(id_table_liste_plots) %>%
        dplyr::summarise(
          current_value_num = min(typevalue, na.rm = TRUE),
          current_value_char = min(typevalue_char, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::collect()
      
      comparison <- data %>%
        dplyr::select(id_table_liste_plots = !!sym(config$id_column), new_value = !!sym(feat)) %>%
        dplyr::left_join(current_values, by = "id_table_liste_plots") %>%
        dplyr::filter(
          !is.na(new_value) & 
            ((!is.na(current_value_num) & is.numeric(new_value) & current_value_num != new_value) |
               (!is.na(current_value_char) & current_value_char != as.character(new_value)) |
               (is.na(current_value_num) & is.na(current_value_char)))
        )
      
      if (nrow(comparison) == 0) {
        cli::cli_alert_info("{feat}: No changes detected")
        next
      }
      
      cli::cli_alert_success("{feat}: {nrow(comparison)} change(s) detected")
      results[[feat]] <- list(success = TRUE, n_changes = nrow(comparison))
    }
  }
  
  return(results)
}

backup_direct_records <- function(changes, config, con) {
  if (is.null(config$backup_table)) {
    cli::cli_alert_info("No backup table, skipping")
    return(invisible(NULL))
  }
  
  cli::cli_alert_info("Backing up to {config$backup_table}")
  
  ids <- unique(changes[[config$id_column]])
  
  current <- dplyr::tbl(con, config$table) %>%
    dplyr::filter(!!sym(config$id_column) %in% ids) %>%
    dplyr::collect()
  
  backup_cols <- DBI::dbListFields(con, config$backup_table)
  backup_cols <- setdiff(backup_cols, c("date_modified", "modif_type", grep("^id_fol_up_", backup_cols, value = TRUE)))
  
  backup_data <- current %>%
    dplyr::select(dplyr::any_of(backup_cols)) %>%
    dplyr::mutate(date_modified = Sys.Date(), modif_type = paste(unique(changes$column), collapse = ","))
  
  DBI::dbWriteTable(con, config$backup_table, backup_data, append = TRUE, row.names = FALSE)
  
  cli::cli_alert_success("Backed up {nrow(backup_data)} record(s)")
  invisible(backup_data)
}

execute_direct_updates_single <- function(changes, config, con) {
  for (col in unique(changes$column)) {
    col_changes <- changes %>% dplyr::filter(column == col)
    cli::cli_alert_info("Updating {col}: {nrow(col_changes)} record(s)")
    
    for (i in seq_len(nrow(col_changes))) {
      id_val <- col_changes[[config$id_column]][i]
      new_val <- col_changes$new_value[i]
      
      sql <- glue::glue_sql("UPDATE {`config$table`} SET {`col`} = {new_val} WHERE {`config$id_column`} = {id_val}", .con = con)
      DBI::dbExecute(con, sql)
    }
    
    cli::cli_alert_success("{col}: {nrow(col_changes)} updated")
  }
}

execute_direct_updates_batch <- function(changes, config, con) {
  update_data <- changes %>%
    tidyr::pivot_wider(id_cols = !!sym(config$id_column), names_from = column, values_from = new_value)
  
  temp_table <- paste0("temp_update_", format(Sys.time(), "%Y%m%d%H%M%S"))
  DBI::dbWriteTable(con, temp_table, update_data, temporary = TRUE)
  
  cols_to_update <- setdiff(names(update_data), config$id_column)
  
  for (col in cols_to_update) {
    sql <- glue::glue_sql("UPDATE {`config$table`} t INNER JOIN {`temp_table`} tmp ON t.{`config$id_column`} = tmp.{`config$id_column`} SET t.{`col`} = tmp.{`col`} WHERE tmp.{`col`} IS NOT NULL", .con = con)
    n <- DBI::dbExecute(con, sql)
    cli::cli_alert_success("{col}: {n} updated")
  }
  
  DBI::dbRemoveTable(con, temp_table)
}

execute_direct_updates <- function(changes, config, method, con) {
  if (is.null(changes) || nrow(changes) == 0) {
    cli::cli_alert_info("No updates to execute")
    return(invisible(NULL))
  }
  
  backup_direct_records(changes, config, con)
  
  if (method == "batch" && nrow(changes) > 5) {
    execute_direct_updates_batch(changes, config, con)
  } else {
    execute_direct_updates_single(changes, config, con)
  }
}

execute_feature_updates <- function(changes, config, table_type, con) {
  cli::cli_alert_warning("Feature updates not implemented")
  cli::cli_alert_info("Use 'individual_features' or 'subplot_features' instead")
  invisible(NULL)
}

#' Update records with optional single-record comparison display
#'
#' @param data Tibble with records to update
#' @param table_type Character: type of table
#' @param execute Logical: if FALSE (default), dry run only
#' @param method Character: "single" or "batch"
#' @param con Database connection
#' @param interactive Logical: enable interactive prompts
#' @param similarity_threshold Numeric: threshold for metadata mapping
#' @param show_comparison Logical: if TRUE and method="single", display HTML comparison
#'
#' @export
update_records <- function(data, 
                           table_type = c("individuals", "plots", "individual_features", 
                                          "subplot_features", "individual_features_metadata",
                                          "methodslist", "table_colnam",
                                          "traitlist",
                                          "subplotype_list"), 
                           execute = FALSE, 
                           method = c("single", "batch"), 
                           con = NULL,
                           interactive = TRUE, 
                           similarity_threshold = 0.6,
                           show_comparison = TRUE) {
  
  table_type <- match.arg(table_type)
  method <- match.arg(method)
  
  if (is.null(con)) con <- call.mydb()
  
  cli::cli_h2("Update records: {table_type}")
  
  config <- get_column_routing(table_type, con)
  
  # ========================================
  # ORIGINAL LOGIC: Metadata mapping and change detection
  # ========================================
  
  if (!is.null(config$metadata_mappings)) {
    data <- reverse_map_metadata(data, config, con, interactive, similarity_threshold)
  }
  
  if (table_type == "individual_features_metadata" && isTRUE(config$has_table_references)) {
    data <- reverse_map_table_references(data, con)
  }
  
  cols_in_data <- setdiff(names(data), config$id_column)
  direct_cols <- intersect(cols_in_data, config$direct_columns)
  
  if (table_type == "individuals") {
    feature_cols <- intersect(cols_in_data, config$feature_columns)
  } else if (table_type == "plots") {
    feature_cols <- intersect(cols_in_data, config$subplot_features)
  } else {
    feature_cols <- c()
  }
  
  unknown_cols <- setdiff(cols_in_data, c(direct_cols, feature_cols))
  
  if (length(unknown_cols) > 0) {
    cli::cli_alert_warning("Unknown columns ignored: {paste(unknown_cols, collapse=', ')}")
  }
  
  cli::cli_alert_info("Direct columns: {length(direct_cols)}")
  if (length(feature_cols) > 0) {
    cli::cli_alert_info("Feature columns: {length(feature_cols)}")
  }
  
  changes <- list()
  
  if (length(direct_cols) > 0) {
    cli::cli_h3("Detecting changes in direct columns")
    changes$direct <- detect_direct_changes(data, columns = direct_cols, config, con)
  }
  
  if (length(feature_cols) > 0) {
    cli::cli_h3("Detecting changes in features")
    changes$features <- detect_feature_changes(data, feature_columns = feature_cols, config, table_type, con)
  }
  
  if (!execute) {
    cli::cli_alert_warning("!!")
    cli::cli_h1 ("DRY RUN - NO CHANGES APPLIED (rerun with 'execute = TRUE' to apply)")
    
    # Return with comparison if available
    result <- list(changes = changes)
    if (exists("comparison", inherits = FALSE)) {
      result$comparison <- comparison
    }
    return(invisible(result))
  }
  
  cli::cli_h3("Executing updates")
  
  if (!is.null(changes$direct)) {
    execute_direct_updates(changes$direct, config, method, con)
  }
  
  if (!is.null(changes$features) && length(changes$features) > 0) {
    has_errors <- any(sapply(changes$features, function(x) !is.null(x$error)))
    if (!has_errors) {
      execute_feature_updates(changes$features, config, table_type, con)
    }
  }
  
  cli::cli_alert_success("Updates completed")
  
  # Return with comparison if available
  result <- list(changes = changes)
  if (exists("comparison", inherits = FALSE)) {
    result$comparison <- comparison
  }
  invisible(result)
}


