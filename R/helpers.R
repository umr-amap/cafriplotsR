
#' Choose from prompt
#'
#' @description
#' Show prompt in console to choose which directions to take
#' 
#' @param choices_vec Optional. A character vector of choices.
#' @param message Optional. A single string for the prompt message.
#'
#' @returns 
#' A logical value: `TRUE` for the first choice, `FALSE` for the second choice,
#' and `NA` for the third choice. For invalid selections, the function will
#' print an error message but may return an unexpected value.
#'
#' @export
choose_prompt <- function(choices_vec = c("Yes", "No", "Cancel"),
                          message ="") {
  
  cli_h1(message)
  cli_ul(choices_vec)
  
  # Prompt
  selection <- readline(prompt = "Choose from 1 (above) to x (below)")
  
  
  if (selection == "")
    selection <- 1
  print(selection)
  
  # Validate and respond
  selection <- as.integer(selection)
  if (!is.na(selection) && selection >= 1 && selection <= length(choices_vec)) {
    cli_alert_success("You selected {.strong {choices_vec[selection]}}.")
  } else {
    cli_alert_danger("Invalid selection.")
  }
  
  if (selection == 1) 
    val_logical <- TRUE
  
  if (selection == 2) 
    val_logical <- FALSE
  
  if (selection == 3) 
    val_logical <- NA
  
  return(val_logical)
  
}





#' print table as html in viewer
#'
#' print table as html in viewer reordered
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param res_print tibble
#'
#'
#' @return print html in viewer
print_table <- function(res_print) {
  
  res_print <-
    res_print %>%
    mutate(across(where(is.character), ~ tidyr::replace_na(., "")))
  
  res_print <- suppressMessages(as_tibble(cbind(columns = names(res_print), record = t(res_print)),
                                          .name_repair = "universal"))
  
  res_print %>%
    kableExtra::kable(format = "html", escape = F) %>%
    kableExtra::kable_styling("striped", full_width = F) %>%
    print()
  
}





#' Get species-plot data frame
#'
#' Convert the extract of database into a species-plot data frame
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#'
#' @return A data frame with taxa as row, plot as columns and values as number of individuals
#' @export
species_plot_matrix <- function(data_tb, tax_col = "tax_sp_level", plot_col = "plot_name") {
  
  tax_col_enquo <-
    rlang::parse_expr(rlang::quo_name(rlang::enquo(tax_col)))
  plot_col_enquo <-
    rlang::parse_expr(rlang::quo_name(rlang::enquo(plot_col)))
  
  nbe_row_identified <-
    data_tb %>%
    filter(!is.na(!!tax_col_enquo)) %>%
    nrow()
  
  if (nbe_row_identified != nrow(data_tb)) {
    
    cli::cli_alert_info("Removing {nrow(data_tb) - nbe_row_identified} unidentified individuals")
    
    data_tb <-
      data_tb %>%
      filter(!is.na(!!tax_col_enquo))
    
  }
  
  data_tb_grouped <-
    data_tb %>%
    dplyr::mutate(ab = 1) %>%
    dplyr::group_by(!!plot_col_enquo, !!tax_col_enquo) %>%
    summarise(ab = sum(ab)) %>%
    ungroup()
  
  data_mat_tb <-
    tidyr::pivot_wider(data = data_tb_grouped,
                       names_from = !!plot_col_enquo,
                       values_from = ab,
                       values_fill = list(ab = 0))
  
  ## format the tibble into a matrix
  data_mat <- data_mat_tb %>% dplyr::select(-!!tax_col_enquo) %>%  as.data.frame()
  row.names(data_mat) <-
    data_mat_tb %>%
    dplyr::select(!!tax_col_enquo) %>%
    dplyr::pull()
  
  return(data_mat)
}



#' Query fuzzy match
#'
#' Extract from a sql database an exact match on a given field
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param tbl tibble with one field listing names to be searched
#' @param field string column name to be search
#' @param values_q string names to be searched
#' @param con PqConnection connection to RPostgres database
#'
#'
#' @return A list of two elements, one with the extract if any, two with the names with id not NA when matched
#' @export
query_fuzzy_match <- function(tbl, field, values_q, con) {
  
  # if (length(field) == 0) sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE SIMILARITY (lower({`field`}), {values_q}) > {sim_thres} ;",
  #                      .con = con)
  
  if (length(field) == 1) sql <- glue::glue_sql("SELECT * FROM {`tbl`} ORDER BY SIMILARITY (lower({`field`}), {values_q}) DESC LIMIT 1;",
                                                .con = con)
  
  # if (length(field) > 0) sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE SIMILARITY (lower(concat({`field[1]`},' ',{`field[2]`})), {values_q}) > {sim_thres} ;",
  #                                              .con = con)
  
  if (length(field) > 1)  sql <- glue::glue_sql("SELECT * FROM {`tbl`} ORDER BY SIMILARITY (lower(concat({`field[1]`},' ',{`field[2]`})), {values_q}) DESC LIMIT 5;",
                                                .con = con)
  
  res_q <- func_try_fetch(con = con, sql = sql)
  
  # rs <- DBI::dbSendQuery(con, sql)
  # res_q <-DBI::dbFetch(rs) %>% as_tibble
  # DBI::dbClearResult(rs)
  
  if (nrow(res_q) == 0) {
    
    cli::cli_alert_warning("Failed fuzzy match for {values_q[i]} in {field} field in {tbl}")
    
  }
  
  return(res_q)
  
}




#' Query exact match
#'
#' Extract from a sql database an exact match on a given field
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param tbl tibble with one field listing names to be searched
#' @param field string column name to be search
#' @param values_q string names to be searched
#' @param con PqConnection connection to RPostgres database
#'
#'
#' @return A list of two elements, one with the extract if any, two with the names with id not NA when matched
#' @export
query_exact_match <- function(tbl, field, values_q, con) {
  
  if (length(field) == 1) {
    
    field_col <- dplyr::sym(field)
    
    query_tb <- tibble(!!field_col := tolower(values_q))
    
  } else {
    
    query_tb <- tibble(species := tolower(values_q))
    
  }
  
  if (length(field) == 1) sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE lower({`field`}) IN ({vals*})",
                                                vals = tolower(values_q), .con = con)
  if (length(field) > 1) sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE lower(concat({`field[1]`},' ',{`field[2]`})) IN ({vals*})",
                                               vals = tolower(values_q), .con = con)
  
  res_q <- func_try_fetch(con = con, sql = sql)
  
  if (length(field) == 1) query_tb <- query_tb %>%
    left_join(res_q %>% dplyr::select(!!field_col) %>% mutate(!!field_col := tolower(!!field_col)) %>% distinct() %>%
                mutate(id = seq_len(nrow(.))))
  
  if (length(field) > 1) query_tb <- query_tb %>%
    left_join(res_q %>% dplyr::select(dplyr::all_of(field)) %>%
                mutate(species = paste(!!dplyr::sym(field[1]), !!dplyr::sym(field[2]), sep = " ")) %>%
                mutate(species = tolower(species)) %>%
                distinct() %>%
                mutate(id = seq_len(nrow(.))))
  
  return(list(res_q = res_q,
              query_tb = query_tb))
  
}



#' Internal function
#'
#' Add modification day month and year column before adding/updating
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset string tibble to add dates fields
#'
#' @export
.add_modif_field <- function(dataset) {
  dataset <-
    dataset %>%
    tibble::add_column(date_modif_d = lubridate::day(Sys.Date()),
                       date_modif_m = lubridate::month(Sys.Date()),
                       date_modif_y = lubridate::year(Sys.Date()))
  return(dataset)
}




#' Internal function
#'
#' rename columns based on new and old columns names
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset tibble
#' @param col_old string vector
#' @param col_new string vector
#'
#' @export
.rename_data <- function(dataset, col_old, col_new) {
  
  if (length(col_old) != length(col_new))
    stop("number of new columns names different of number of selected column names")
  
  for (i in 1:length(col_old)) {
    if (any(colnames(dataset) == col_old[i])) {
      dataset <-
        dataset %>%
        dplyr::rename_at(dplyr::vars(col_old[i]), ~ col_new[i])
    } else{
      stop(paste(
        "Column name provided not found in provided new dataset",
        col_old[i]
      ))
    }
  }
  return(dataset)
}



#' Replace or restore missing values in a data frame
#'
#' Replaces NA values with -9999 for numeric columns and "-9999" for character columns,
#' or inversely restores NAs from those values if `inv = TRUE`.
#'
#' @param df A data frame or tibble.
#' @param inv Logical. If TRUE, replaces standard sentinel values back to NA.
#'
#' @return A data frame with modified missing values.
#'
#' @export
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
replace_NA <- function(df, inv = FALSE) {
  
  num_fun <- if (inv) ~ dplyr::na_if(.x, -9999) else ~ tidyr::replace_na(.x, -9999)
  chr_fun <- if (inv) ~ dplyr::na_if(.x, "-9999") else ~ tidyr::replace_na(.x, "-9999")
  
  df %>%
    dplyr::mutate(dplyr::across(where(is.numeric), num_fun)) %>%
    dplyr::mutate(dplyr::across(where(is.character), chr_fun))
}




#' Compare two row-tibbles and generate HTML with differences
#'
#' @param vec_1 A tibble with one row
#' @param vec_2 A tibble with one row
#' 
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' 
#' @return A list: (1) tibble of differing columns, (2) HTML table highlighting differences
#' @export
.comp_print_vec <- function(vec_1, vec_2) {
  
  stopifnot(nrow(vec_1) == 1, nrow(vec_2) == 1)
  stopifnot(ncol(vec_1) == ncol(vec_2), all(names(vec_1) == names(vec_2)))
  
  vec_1 <- replace_NA(vec_1)
  vec_2 <- replace_NA(vec_2)
  
  comp_val <- vec_1 != vec_2
  comp_val <- as_tibble(comp_val)
  diff_cols <- comp_val %>% select(where(~ any(.)))
  
  if (ncol(diff_cols) == 0) {
    return(list(comp_tb = FALSE, comp_html = NA))
  }
  
  if ("idtax_n" %in% names(vec_1)) {
    old_tax <- query_taxa(ids = vec_2$idtax_n, check_synonymy = FALSE,
                          class = NULL, extract_traits = FALSE)
    new_tax <- query_taxa(ids = vec_1$idtax_n, check_synonymy = FALSE,
                          class = NULL, extract_traits = FALSE)
    
    vec_1 <- dplyr::left_join(vec_1, new_tax %>% dplyr::select(idtax_n, tax_fam, tax_gen, tax_esp), by = "idtax_n")
    vec_2 <- dplyr::left_join(vec_2, old_tax %>% dplyr::select(idtax_n, tax_fam, tax_gen, tax_esp), by = "idtax_n")
  }
  
  comp_tb <- 
    tibble(
      cols = names(vec_1),
      current = unlist(replace_NA(vec_1, inv = T), use.names = FALSE),
      new = unlist(replace_NA(vec_2, inv = T), use.names = FALSE),
      current_comp = unlist(vec_1, use.names = FALSE),
      new_comp = unlist(vec_2, use.names = FALSE)
    )
  
  comp_tb_html <- 
    comp_tb %>%
    mutate(
      new = kableExtra::cell_spec(
        new, "html",
        color = if_else(tidyr::replace_na(current_comp, "") != tidyr::replace_na(new_comp, ""), 
                        "red", "blue")
      )
    ) %>%
    select(-new_comp, -current_comp) %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling("striped", full_width = FALSE)
  
  return(list(comp_tb = diff_cols, comp_html = comp_tb_html))
}





