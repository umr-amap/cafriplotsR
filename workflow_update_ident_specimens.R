## load data
new_ident <- readxl::read_excel("Identifications_Transects_2025_part2.xlsx")

# Standardizing colnam by using the IDs
new_ident <- 
  data_manager_sep <-
  .link_colnam(
    data_stand = new_ident,
    column_searched = "colnam",
    column_name = "colnam",
    id_field = "colnam",
    id_table_name = "id_table_colnam",
    db_connection = mydb,
    table_name = "table_colnam"
  )

## Remove all entries with ID.dico.name = 0 or NA
new_ident <- 
  new_ident %>% 
  dplyr::filter(ID.dico.name > 0, colnam != 0) #Doesnt take into account the collectors not in the database

## Bulk upload identifications ##
# Loop to update identification one-by-one
for (i in 1:nrow(new_ident)) {
  update_ident_specimens(
    id_colnam = new_ident$colnam[i], 
    number = new_ident$colnbr[i], 
    # id_speci = new_ident$id_specimen[i],
    id_new_taxa = new_ident$ID.dico.name[i],
    new_dety = new_ident$dety[i],
    new_detm = new_ident$detm[i],
    new_detd = new_ident$detd[i],
    new_detby = new_ident$detby[i],
    only_new_ident = FALSE, # updates only if Ids. has changed
    ask_before_update = T, # requires confirmation before change
    show_results = TRUE
  ) }