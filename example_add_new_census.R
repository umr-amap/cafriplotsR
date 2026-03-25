
dataset <- 
  readxl::read_excel(
    "D:/Papiers-Bourses-colloques/sejours_etrangers_bourses_financements_missions/mission_mbalmayo_avril_2025/plot/arbre.xlsx",
    guess_max = 5000)


meta_data <- 
  readxl::read_excel(
    "D:/Papiers-Bourses-colloques/sejours_etrangers_bourses_financements_missions/mission_mbalmayo_avril_2025/plot/plot.xlsx"
  )

dataset %>% select(specimen_number) %>% 
  distinct(specimen_number) %>% 
  arrange(specimen_number)


meta_data <- 
  meta_data %>% 
  select(plot_name, date_year, date_month, date_day) %>% 
  mutate(team_leader = "Géraldine Nguemo",
         additional_people = "Gilles Dauby, Théo Ayol, Sikiro, Oreste Yemdji, Hugo Leblanc, Rachelle, David Bauman, Isaac Zombo Dikele") %>% 
  mutate(census = 2)

# tmp <- query_plots(plot_name = meta_data$plot_name, show_multiple_census = T)
# tmp <- query_subplots(plot_name = meta_data$plot_name)
# ids <- 
#   tmp$census_features %>% 
#   filter(typevalue == 2) %>% 
#   pull(id_sub_plots)

tmp <- query_plots(plot_name = meta_data$plot_name, show_multiple_census = T, extract_individuals = F)


add_subplot_features(
  new_data = meta_data,
  col_names_select = c("date_day", "date_month", "date_year"),
  col_names_corresp = c("day", "month", "year"),
  plot_name_field = "plot_name",
  subplottype_field = c("census"),
  add_data = T, 
  features_field = c("team_leader", "additional_people")
)


## recruit

recruits <- 
  dataset %>%
  filter(state == "recruted") %>%
  rename(tag = label_recrut) %>%
  select(
    plot_plot_name,
    tag,
    species_code,
    species_scientific_name,
    quadrat,
    specimen_number,
    herbarium_nbe_char,
    starts_with("observation"),
    starts_with("pom_observation"),
    position_x,
    position_y,
    multi_stem,
    number_multi_stem,
    remaining_main_axis,
    crown_left
  ) %>%
  rename(plot_name = plot_plot_name,
         original_tax_name = species_scientific_name,
         idtax = species_code) %>%
  mutate(idtax = as.numeric(idtax)) %>%
  mutate(
    specimen_number = ifelse(!is.na(specimen_number), paste("PIRD", as.character(specimen_number)), NA)
    ,
    herbarium_nbe_char = ifelse(!is.na(herbarium_nbe_char), paste(
      "PIRD", as.character(herbarium_nbe_char)
    ), NA)
  ) %>%
  mutate(ID = 1:nrow(.)) %>% 
  mutate(tag = as.numeric(tag))

recruits %>% group_by(plot_name, tag) %>% 
  count() %>% 
  filter(n > 1)

logs <-
  add_individuals(
    new_data = recruits,
    col_names_select = c(
      "plot_name",
      "tag",
      "quadrat",
      "original_tax_name",
      "specimen_number",
      "herbarium_nbe_char",
      "idtax",
      "ID"
    ),
    col_names_corresp = c(
      "plot_name",
      "ind_num_sous_plot",
      "sous_plot_name",
      "original_tax_name",
      "herbarium_nbe_type",
      "herbarium_nbe_char",
      "idtax_n",
      "id_importation"
    ),
    id_col = 1,
    launch_adding_data = T
  )

data_up <-
  query_plots(
    plot_name = unique(dataset$plot_plot_name),
    extract_individuals = T,
    remove_ids = F,
    extract_traits = F,
    show_multiple_census = T
  )

dataset_cleaned_full <-
  dataset %>%
  mutate(
    species_code = replace(species_code, species_code == "UNK", 351364),
    species_code = replace(species_code, is.na(species_code), 351364)
  ) %>%
  rename(plot_name = plot_plot_name,
         tag = arbre,
         original_tax_name = species_scientific_name) %>%
  mutate(tag = ifelse(is.na(label_recrut), tag, label_recrut)) %>%
  mutate(
    idtax_n = as.numeric(species_code),
    tag = as.numeric(tag),
    stem_diameter = as.numeric(dbh)
  ) %>%
  select(
    original_tax_name,
    tag,
    plot_name,
    stem_diameter,
    height_of_stem_diameter,
    starts_with("pom_obs"),
    starts_with("observation"),
    starts_with("dead_cause"),
    multi_stem,
    number_multi_stem,
    light,
    specimen_number,
    specimes_descriptipn,
    comment,
    idtax_n,
    any_voucher,
    herbarium_nbe_char,
    state,
    starts_with("stem_status")
  )


dataset_cleaned <- 
  dataset_cleaned_full %>% 
  filter(state %in% c("not_recruited"))


dataset_cleaned %>% group_by(plot_name, tag) %>%
  count() %>%
  filter(n > 1)

multi_ind <-
  data_up$extract %>%
  arrange(plot_name, ind_num_sous_plot) %>%
  select(id_n, plot_name, ind_num_sous_plot, idtax_individual_f) %>%
  left_join(
    dataset_cleaned %>% select(multi_stem, 
                               number_multi_stem, plot_name, tag, original_tax_name),
    by = c("plot_name" = "plot_name", "ind_num_sous_plot" = "tag")
  ) %>%
  mutate(stem_grouping = NA_integer_) %>%
  mutate(id_importation = 1:nrow(.))

multi_ind_ <- multi_ind %>% filter(multi_stem == "yes")

all_ind_multi <- vector('list', nrow(multi_ind_))
for (i in 1:nrow(multi_ind_)) {
  
  multi_ind_sel <- multi_ind_ %>% slice(i)
  
  multi_ind_sel_all <-
    multi_ind %>%
    filter(
      id_importation %in% multi_ind_sel$id_importation:(
        multi_ind_sel$id_importation + (multi_ind_sel$number_multi_stem - 1)
      ),
      plot_name == multi_ind_sel$plot_name
    )
  
  if (length(unique(multi_ind_sel_all$idtax_individual_f)) > 1) {
    
    warning(paste("more than one idtax", i))
    
    print(multi_ind_sel_all)
    
    skip_ <- choose_prompt(message =  "Skip one stem ?")
    
    if (skip_) {
      
      seq_id_imp <- 
        multi_ind_sel$id_importation:(
          multi_ind_sel$id_importation  + (multi_ind_sel$number_multi_stem - 1 + 1))
      
      seq_id_imp <- 
        seq_id_imp[-2]
      
      multi_ind_sel_all <- 
        multi_ind %>%
        filter(
          id_importation %in% seq_id_imp,
          plot_name == multi_ind_sel$plot_name
        )
      
      if (length(unique(multi_ind_sel_all$idtax_individual_f)) > 1)
        warning(paste("still more than one idtax", i))
      
    }
    
  }
  
  
  all_ind_multi[[i]] <-
    multi_ind_sel_all %>%
    slice(2:nrow(.)) %>%
    mutate(stem_grouping = multi_ind_sel %>% slice(1) %>% pull(id_n))
  
}

update_individuals(
  new_data = bind_rows(all_ind_multi) %>% 
    select(id_n, stem_grouping),
  launch_update = T
)





of_observations <- read_csv(
  "D:/Papiers-Bourses-colloques/openforis/code_list_observations5296412383766868985.csv"
)

individuals_observations <- 
  dataset_cleaned %>%
  select(starts_with("observation"), plot_name, tag) %>%
  mutate(across(starts_with("observation"), as.numeric)) %>%
  pivot_longer(cols = starts_with("observation")) %>%
  filter(!is.na(value)) %>%
  left_join(
    of_observations %>% dplyr::select(item_code, item_label_en),
    by = c("value" = "item_code")
  ) %>%
  mutate(value = str_to_lower(item_label_en)) %>%
  select(plot_name, tag, value) %>%
  mutate(flag1 = "") %>%
  mutate(
    flag1 = case_when(
      grepl("dying", value) ~ "z",
      grepl("leaning", value) ~ "c",
      grepl("broken stem", value) ~ "b",
      grepl("lying", value) ~ "d",
      grepl("termites", value) ~ "y",
      grepl("hollow", value) ~ "f",
      grepl("large liana", value) ~ "l",
      grepl("human", value) ~ "w",
      grepl("small liana (d<10 cm) with > 50", value) ~ "m",
      grepl("stangler", value) ~ "s",
      grepl("more than half defoliated", value) ~ "i"
    )
  ) %>% 
  # %>%
  #   group_by(plot_name, tag) %>%
  #   summarise(observations = coalesce(unique(str_c(
  #     str_to_lower(value), collapse = ","
  #   ))), flag1 = coalesce(unique(str_c(
  #     str_to_lower(flag1), collapse = ","
  #   )))) %>%
  #   ungroup %>%
  left_join(
    data_up$extract %>% select(plot_name, ind_num_sous_plot, id_n),
    by = c("plot_name" = "plot_name", "tag" = "ind_num_sous_plot")
  )

logs <-
  add_traits_measures(
    new_data = individuals_observations,
    # col_names_select = c("original_tax_name"),
    # col_names_corresp = c("original_tax_name"),
    plot_name_field = "plot_name",
    id_tag_plot = "id_n",
    traits_field = c("value", "flag1"),
    add_data = T
  )

of_pom <- read_csv(
  "D:/Papiers-Bourses-colloques/openforis/code_list_pom_observations8428434127832253256.csv"
)

individuals_pom_observations <-
  dataset_cleaned %>%
  select(starts_with("pom_observat"), plot_name, tag, stem_diameter) %>%
  mutate(across(starts_with("pom_observat"), as.numeric)) %>%
  pivot_longer(cols = starts_with("pom_observat")) %>%
  filter(!is.na(value)) %>%
  left_join(of_pom %>% dplyr::select(item_code, item_label_en),
            by = c("value" = "item_code")) %>%
  mutate(value = str_to_lower(item_label_en)) %>%
  select(plot_name, tag, value) %>%
  filter(!is.na(value)) %>%
  # group_by(plot_name, tag) %>%
  # summarise(stem_diameter_observations = coalesce(unique(str_c(
  #   str_to_lower(value), collapse = ","
  # )))) %>%
  # ungroup %>%
  left_join(
    data_up$extract %>% select(plot_name, ind_num_sous_plot, id_n),
    by = c("plot_name" = "plot_name", "tag" = "ind_num_sous_plot")
  )

logs <-
  add_traits_measures(
    new_data = individuals_pom_observations,
    # col_names_select = c("original_tax_name"),
    # col_names_corresp = c("original_tax_name"),
    plot_name_field = "plot_name",
    id_tag_plot = "id_n",
    traits_field = c("value"),
    add_data = T
  )

# tmp <- 
#   query_individual_features(id = unique(individuals_pom_observations$id_n), 
#                             pivot_table = F, id_traits = 13)
# ids <- 
#   tmp$traits_char[[1]] %>% select(id_sub_plots, id_table_liste_plots) %>% distinct() %>% pull(id_table_liste_plots)
# tmp2 <- query_subplots(ids_plots = ids)
# ids2 <- tmp2$all_subplots %>% filter(type == "census", typevalue == 2) %>% pull(id_sub_plots)
# 
# tmp3 <- 
#   tmp$traits_char[[1]] %>% filter(id_sub_plots %in% ids2)
# 
# .delete_entry_trait_measure(id = tmp3$id_trait_measures)


of_light <- read_csv("D:/Papiers-Bourses-colloques/openforis/code_light.csv")
of_light <- of_light %>% mutate(light_code = as.numeric(light_code))

light_information <-
  dataset_cleaned %>%
  mutate(light = as.numeric(light)) %>%
  left_join(of_light %>% dplyr::select(light_code, light_label_en),
            by = c("light" = "light_code")) %>%
  mutate(light_label_en = str_to_lower(light_label_en)) %>%
  select(light_label_en, plot_name, tag, plot_name) %>%
  filter(!is.na(light_label_en)) %>%
  left_join(
    data_up$extract %>% select(plot_name, ind_num_sous_plot, id_n),
    by = c("plot_name" = "plot_name", "tag" = "ind_num_sous_plot")
  )

logs <-
  add_traits_measures(
    new_data = light_information,
    # col_names_select = c("original_tax_name"),
    # col_names_corresp = c("original_tax_name"),
    plot_name_field = "plot_name",
    id_tag_plot = "id_n",
    traits_field = c("light_label_en"),
    add_data = T
  )


diameter_information <-
  dataset_cleaned %>%
  select(stem_diameter, height_of_stem_diameter, plot_name, tag) %>%
  left_join(
    data_up$extract %>% 
      rename(dbh1 = stem_diameter_census_1) %>% 
      select(plot_name, ind_num_sous_plot, id_n, dbh1),
    by = c("plot_name" = "plot_name", "tag" = "ind_num_sous_plot")
  )

logs <-
  add_traits_measures(
    new_data = diameter_information,
    plot_name_field = "plot_name",
    id_tag_plot = "id_n",
    traits_field = c("stem_diameter",
                     "height_of_stem_diameter"),
    add_data = T
  )

dataset %>% 
  distinct(state)


## death and status stems
of_status <- 
  read_csv("D:/Papiers-Bourses-colloques/openforis/code_recensus_status.csv")


of_status <- 
  of_status %>% 
  mutate(
    status_code_conc = paste0(as.character(status1_code), 
                              as.character(status2_code))
  ) %>% 
  mutate(status_code_conc = as.numeric(str_replace_all(string = status_code_conc, pattern = "NA", replacement = ""))) %>% 
  mutate(flag1 = case_when(
    grepl("alive stem with", status1_label_en) & 
      grepl("standing", status2_label_en) ~ "z",
    grepl("alive stem with", status1_label_en) & 
      grepl("broken", status2_label_en) ~ "b",
    grepl("alive stem with", status1_label_en) & 
      grepl("uprooted", status2_label_en) ~ "b"
  )) %>% 
  mutate(flag2 = case_when(
    grepl("dead stem", status1_label_en) & 
      grepl("standing", status2_label_en) ~ "a",
    grepl("dead stem", status1_label_en) & 
      grepl("broken", status2_label_en) ~ "b",
    grepl("dead stem", status1_label_en) & 
      grepl("uprooted", status2_label_en) ~ "i",
    grepl("stem and tag not found", status1_label_en) ~ "k"
  ))

status_stem <- 
  dataset_cleaned_full %>%
  select(plot_name, tag, stem_status, stem_status2) %>%
  mutate(status_code_conc = paste0(as.character(stem_status), as.character(stem_status2))) %>%
  mutate(status_code_conc =
           as.numeric(
             str_replace_all(
               string = status_code_conc,
               pattern = "NA",
               replacement = ""
             )
           )) %>% 
  left_join(
    of_status %>% select(status_code_conc, flag1, flag2, status1_label_en),
    by = c("status_code_conc" = "status_code_conc")
  ) %>%
  left_join(
    data_up$extract %>% select(plot_name, ind_num_sous_plot, id_n),
    by = c("plot_name" = "plot_name", "tag" = "ind_num_sous_plot")
  )

status_stem %>% group_by(status1_label_en) %>% 
  count()

logs <-
  add_traits_measures(
    new_data = status_stem ,
    plot_name_field = "plot_name",
    id_tag_plot = "id_n",
    traits_field = c("flag1", "flag2", "status1_label_en"),
    add_data = T
  )






specimens <- dataset_cleaned %>% 
  select(plot_name, tag, herbarium_nbe_char, specimen_number, specimes_descriptipn) %>% 
  filter(!is.na(herbarium_nbe_char)) %>% 
  mutate(herbarium_nbe_char = paste("PIRD", herbarium_nbe_char),
         specimen_number = ifelse(!is.na(specimen_number), 
                                  paste("PIRD", specimen_number),
                                  NA)) %>% 
  left_join(dataset_ind$extract %>% 
              select(id_n, plot_name,ind_num_sous_plot, idtax_n, stem_diameter_census_2),
            by = c("plot_name" = "plot_name",
                   "tag" = "ind_num_sous_plot"))


list_specimens <- specimens %>% 
  filter(!is.na(specimen_number)) %>% 
  mutate(colnbr = as.numeric(unlist(str_extract_all(specimen_number, "[[:digit:]]+")))) %>% 
  arrange(colnbr) %>% 
  left_join(metadata$extract %>% 
              select(plot_name, ddlat, ddlon),
            by = c("plot_name" = "plot_name")) %>% 
  mutate(additional_people = "Gilles Dauby, Géraldine Nguemo, Hugo Leblanc, Théophile Ayol, Isaac Zombo, Oreste Yemdji") %>% 
  mutate(description = paste("Tree with diameter of", stem_diameter_census_2, "cm measured at breast height")) %>% 
  mutate(locality = "Mbalmayo, Centre",
         country = "Cameroon",
         colnam = "PIRD",
         colm = 4,
         coly = 2025)


add_specimens(new_data = list_specimens,
              col_names_select = c("colnbr",
                                   "colm",
                                   "coly",
                                   "locality",
                                   "country",
                                   "additional_people",
                                   "ddlat",
                                   "ddlon",
                                   "idtax_n",
                                   "colnam"),
              col_names_corresp = c("colnbr", 
                                    "colm",
                                    "coly",
                                    "locality",
                                    "country",
                                    "add_col",
                                    "ddlat",
                                    "ddlon",
                                    "idtax_n",
                                    "colnam"), 
              collector_field = "colnam",
              launch_adding_data = T)
