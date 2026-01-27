



uploaded_metadata ### this data frame contains the metadata that has just been uploaded

census <- 
  tibble(plot_name = c("Plot1", "Plot2"), ### either plot_name to link the subplot feature
         id_plot_name = c(192, 3021), ### of the id of the plot
         month = c(2, 2, 2),
         year = rep(2024, 3),
         day = c(7, 11, 15),
         census = rep(1, 3),
         team_leader = rep("Fernandez Ngoula", 3),
         additional_people = "Fabrice Djonko, Orestes Yemdji, Théophile Ayol, Gilles Dauby, David Bauman, Géraldine Djamnou, Antoine Tekam"
  )

add_subplot_features(
  new_data = census,
  col_names_select = c("date_day", "date_month", "date_year"),
  col_names_corresp = c("day", "month", "year"),
  plot_name_field = "plot_name",
  subplottype_field = c("census"),
  add_data = T, 
  features_field = c("team_leader", "additional_people")
)
