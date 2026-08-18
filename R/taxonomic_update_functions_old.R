# Taxonomic Update Functions
#
# This file contains functions for updating and managing taxonomic data in the database.
# These functions handle adding new taxa entries, updating taxonomy links, and merging
# individual records with taxonomic information.
#
# Main functions:
# - add_entry_taxa(): Add new taxonomic entry to table_taxa with synonym handling
# - update_taxa_link_table(): Update local table_idtax with latest synonymy info
# - merge_individuals_taxa(): Merge individual records with resolved taxonomy
#
# Dependencies: DBI, dplyr, cli, stringr, tidyr, taxize, kableExtra

#' Merge individual records with taxonomic information
#'
#' Merges individual tree records with resolved taxonomy from both direct links
#' and specimen links, handling synonym resolution
#'
#' @return A tibble with individuals and resolved taxonomy
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id_individual vector of individual IDs to filter
#' @param id_plot vector of plot IDs to filter
#' @param id_tax vector of taxa IDs to filter final results
#' @param clean_columns logical, whether to remove redundant columns
#' @param con_taxa connexion
#' @param con connexion
#' @param backbone character. \code{"internal"} (default) or \code{"wcvp"}.
#'   When \code{"wcvp"}, results are enriched with WCVP names via the link table.
#'
#' @export
merge_individuals_taxa <- function(id_individual = NULL,
                                    id_plot = NULL,
                                    id_tax = NULL,
                                    clean_columns = TRUE,
                                   con_taxa = NULL,
                                   con = NULL,
                                   backbone = c("internal", "wcvp")) {

  backbone <- match.arg(backbone)

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()
  if (is.null(con)) con <- call.mydb()

  # ===== STEP 1: Fetch individuals first =====
  cli::cli_alert_info("Fetching individuals...")

  if (!is.null(id_individual)) {
    res_individuals_query <- dplyr::tbl(con, "data_individuals") %>%
      dplyr::filter(id_n %in% !!id_individual)
  } else {
    res_individuals_query <- dplyr::tbl(con, "data_individuals")
  }

  # Filtrage par plot si demandé
  if (!is.null(id_plot)) {
    res_individuals_query <- res_individuals_query %>%
      dplyr::filter(id_table_liste_plots_n %in% !!id_plot)
  }

  # Collect individuals
  res_individuals_full <- res_individuals_query %>% dplyr::collect()
  individual_ids <- res_individuals_full$id_n
  individual_taxa <- unique(res_individuals_full$idtax_n)

  cli::cli_alert_success("Fetched {nrow(res_individuals_full)} individual(s)")

  # ===== STEP 2: Get specimen links for these individuals only =====
  cli::cli_alert_info("Loading specimen links...")

  # Liens individus-spécimens (priorité: type_individual > referenced_individual, puis date détermination)
  # Get links with priority from linktypelist
  links_raw <- try_open_postgres_table(table = "data_link_specimens", con = con) %>%
    dplyr::filter(id_n %in% !!individual_ids)

  # Try to join with linktypelist for priority (table may not exist yet)
  links_with_priority <- tryCatch({
    links_raw %>%
      dplyr::left_join(
        dplyr::tbl(con, "linktypelist") %>%
          dplyr::select(id_linktype, priority),
        by = "id_linktype"
      )
  }, error = function(e) {
    # linktypelist doesn't exist yet - use default priority 0
    links_raw %>%
      dplyr::mutate(priority = 0L)
  })

  # Join specimen determination date for tie-breaking
  links_with_dates <- links_with_priority %>%
    dplyr::left_join(
      try_open_postgres_table(table = "specimens", con = con) %>%
        dplyr::select(id_specimen, dety, detm, detd),
      by = "id_specimen"
    ) %>%
    dplyr::select(id_n, id_specimen, priority, dety, detm, detd) %>%
    dplyr::collect()

  # Handle NULL priority values
  links_with_dates <- links_with_dates %>%
    dplyr::mutate(priority = dplyr::coalesce(priority, 0L))

  # Create determination date for sorting (higher = more recent)
  links_with_dates <- links_with_dates %>%
    dplyr::mutate(
      det_date = lubridate::make_date(
        dplyr::coalesce(dety, 1900L),
        dplyr::coalesce(detm, 1L),
        dplyr::coalesce(detd, 1L)
      )
    )

  # Sort by priority (desc), then determination date (desc), take first per individual
  links_specimens <- links_with_dates %>%
    dplyr::arrange(id_n, dplyr::desc(priority), dplyr::desc(det_date)) %>%
    dplyr::group_by(id_n) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(id_n, id_specimen)

  # ===== STEP 3: Fetch only the specimens we need =====
  specimen_ids <- unique(links_specimens$id_specimen)

  if (length(specimen_ids) > 0) {
    cli::cli_alert_info("Loading {length(specimen_ids)} specimen(s)...")

    specimens_id_diconame <- try_open_postgres_table(table = "specimens", con = con) %>%
      dplyr::filter(id_specimen %in% !!specimen_ids) %>%
      dplyr::select(id_specimen, idtax_n, id_colnam, colnbr, suffix,
                    id_tropicos, id_brlu) %>%
      dplyr::collect()

    specimen_taxa <- unique(specimens_id_diconame$idtax_n)
  } else {
    cli::cli_alert_info("No specimens linked")
    specimens_id_diconame <- dplyr::tibble()
    specimen_taxa <- integer()
  }

  # ===== STEP 4: Fetch ONLY the synonyms we need =====
  all_taxa <- unique(c(individual_taxa, specimen_taxa))
  all_taxa <- all_taxa[!is.na(all_taxa)]

  cli::cli_alert_info("Loading synonyms for {length(all_taxa)} taxa...")

  diconames_id <- try_open_postgres_table(table = "table_idtax", con = con) %>%
    dplyr::filter(idtax_n %in% !!all_taxa) %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    dplyr::mutate(idtax_f = ifelse(is.na(idtax_good_n), idtax_n, idtax_good_n)) %>%
    dplyr::collect()

  cli::cli_alert_success("Loaded {nrow(diconames_id)} synonym record(s)")

  # ===== STEP 5: Process specimens with synonyms =====
  if (nrow(specimens_id_diconame) > 0) {
    # Get unique collector IDs
    collector_ids <- unique(specimens_id_diconame$id_colnam)
    collector_ids <- collector_ids[!is.na(collector_ids)]

    if (length(collector_ids) > 0) {
      collectors <- dplyr::tbl(con, "table_colnam") %>%
        dplyr::filter(id_table_colnam %in% !!collector_ids) %>%
        dplyr::select(id_table_colnam, colnam) %>%
        dplyr::collect()
    } else {
      collectors <- dplyr::tibble(id_table_colnam = integer(), colnam = character())
    }

    specimens_linked <- specimens_id_diconame %>%
      dplyr::left_join(diconames_id, by = c("idtax_n" = "idtax_n")) %>%
      dplyr::rename(idtax_specimen_f = idtax_f) %>%
      dplyr::left_join(collectors, by = c("id_colnam" = "id_table_colnam")) %>%
      dplyr::rename(colnam_specimen = colnam)
  } else {
    # Create empty tibble with correct column structure for joining
    specimens_linked <- dplyr::tibble(
      id_specimen = integer(),
      idtax_specimen_f = integer(),
      colnam_specimen = character(),
      colnbr = character(),
      suffix = character()
    )
  }

  # ===== STEP 6: Assemble everything =====
  cli::cli_alert_info("Assembling individual data...")

  res_individuals_full <- res_individuals_full %>%
    dplyr::select(-any_of("id_specimen")) %>%
    dplyr::left_join(links_specimens, by = c("id_n" = "id_n")) %>%
    dplyr::left_join(
      diconames_id %>% dplyr::select(idtax_n, idtax_f),
      by = c("idtax_n" = "idtax_n")
    ) %>%
    dplyr::left_join(
      specimens_linked %>% dplyr::select(id_specimen, idtax_specimen_f, colnam_specimen, colnbr, suffix),
      by = c("id_specimen" = "id_specimen")
    ) %>%
    # Logique de résolution finale (IDENTIQUE à l'original)
    dplyr::mutate(
      idtax_individual_f = ifelse(!is.na(idtax_specimen_f), idtax_specimen_f, idtax_f),
      original_tax_name = stringr::str_trim(original_tax_name)
    )

  # Nettoyage des colonnes résiduelles/inutiles
  if (clean_columns) {
    columns_to_remove <- c(
      "photo_tranche", "liane", "dbh", "dbh_height", "tree_height",
      "branch_height", "branchlet_height", "crown_spread", "observations",
      "observations_census_2", "id_census2", "dbh_census2", "id_specimen_old",
      "tax_tax", "id_korup_ctfs", "tag_korup_ctfs", "id_table_data_senterre",
      "id_diconame", "code_individu", "author1", "author2", "author3",
      "tax_source", "citation", "id_tropicos", "id_brlu", "fktax",
      "multi_tiges_id", "multi_tiges_id_good", "multi_stem_id_good",
      "id_table_liste_plots", "strate_cat", "position_transect",
      "position_x", "position_y", "id_old", "sous_plot_name"
    )

    res_individuals_full <- res_individuals_full %>%
      dplyr::select(-any_of(columns_to_remove))
  }

  # Enrichissement avec le backbone taxonomique
  cli::cli_alert_info("Enriching with taxonomic backbone...")
  taxa_extract <- add_taxa_table_taxa(
    ids = res_individuals_full %>% pull(idtax_individual_f),
    backbone = backbone
  ) %>%
    collect()

  # taxa_extract already has WCVP columns replaced when backbone = "wcvp"
  # (handled inside add_taxa_table_taxa)
  res_individuals_full <- res_individuals_full %>%
    dplyr::left_join(
      taxa_extract %>% dplyr::select(-any_of(c("data_modif_d", "data_modif_m", "data_modif_y"))),
      by = c("idtax_individual_f" = "idtax_n")
    )

  # Filtrage final par taxa si demandé
  if (!is.null(id_tax)) {
    res_individuals_full <- res_individuals_full %>%
      filter(idtax_individual_f %in% id_tax)
  }

  cli::cli_alert_success("Successfully fetched {nrow(res_individuals_full)} individual(s)")

  return(res_individuals_full)
}




# ============================================================================
# DEPRECATED: update_taxa_link_table()
#
# This function has been replaced by the version in taxonomic_update_functions.R
# which includes materialized view support, error handling, and fallback logic.
#
# The old implementation is kept here (commented out) for reference only.
# Use the new version from taxonomic_update_functions.R instead.
# ============================================================================

# #' Update local taxonomy link table
# #'
# #' Updates the table_idtax in main database with latest synonym information
# #' from the taxa database
# #'
# #' @return NULL (updates database table)
# #'
# #' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
# #'
# #' @export
# update_taxa_link_table <- function() {
#
#   # if (exists("mydb_taxa")) rm(mydb_taxa)
#   mydb_taxa <- call.mydb.taxa()
#
#   mydb <- call.mydb()
#
#   id_taxa_table <- try_open_postgres_table(table = "table_taxa", con = mydb_taxa) %>%
#     # dplyr::tbl(mydb_taxa, "table_taxa") %>%
#     dplyr::select(idtax_n, idtax_good_n) %>%
#     dplyr::collect()
#
#   dbWriteTable(mydb,
#                name = "table_idtax",
#                value = id_taxa_table,
#                append = FALSE,
#                overwrite = TRUE)
#
#   cli::cli_alert_success("table_idtax updated")
#
#   # dplyr::tbl(mydb, "table_idtax")
#
#
# }




#' Add new entry to taxonomic table
#'
#' Add new entry to taxonomic table
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param tax_gen string genus name
#' @param tax_esp string species name
#' @param tax_fam string family name
#' @param tax_rank1 string tax_rank1 name
#' @param tax_name1 string tax_name1 name
#' @param detvalue integer detvalue code
#' @param morphocat integer morphocat code
#' @param full_name string full name : genus + species + authors
#' @param synonym_of list if the new entry should be put in synonymy with an existing taxa, add in a list at least one values to identify to which taxa it will be put in synonymy: genus, species or id
#'
#'
#' @return A tibble
#' @export


add_entry_taxa <- function(search_name_tps = NULL,
                           tax_gen = NULL,
                           tax_esp = NULL,
                           tax_fam = NULL,
                           tax_order = NULL,
                           tax_famclass = NULL,
                           tax_rank1 = NULL,
                           tax_name1 = NULL,
                           tax_rank2 = NULL,
                           tax_name2 = NULL,
                           author1 = NULL,
                           author2 = NULL,
                           author3 = NULL,
                           year_description = NULL,
                           synonym_of = NULL,
                           morpho_species = FALSE,
                           TPS_KEY = "15ad0b4c-f0d3-46ab-b649-178f2c75724f",
                           tax_tax = NULL)
{

  # if (!exists("mydb")) call.mydb()
  # if (exists("mydb_taxa")) rm(mydb_taxa)
  mydb_taxa <- call.mydb.taxa()

  if (is.null(search_name_tps) &
      is.null(tax_gen) & is.null(tax_esp) & is.null(tax_fam))
    search_name_tps <-
      readline(prompt = "Enter name to search on Tropicos  : ")

  if (!is.null(search_name_tps)) {

    if (search_name_tps != "") {
      res_tps <- taxize::tp_search(sci = search_name_tps, key = TPS_KEY)

      if (ncol(res_tps) == 1) {

        cli::cli_alert_info("Not found on Tropicos")

        tax_gen <- readline(prompt = "Enter tax_gen  : ")
        tax_gen <- stringr::str_squish(tax_gen)
        if (tax_gen == '')
          tax_gen <- NULL

        tax_esp <- readline(prompt = "Enter tax_esp  : ")
        tax_esp <- stringr::str_squish(tax_esp)
        if (tax_esp == '')
          tax_esp <- NULL

        author1 <- readline(prompt = "Enter author1  : ")
        author1 <- stringr::str_squish(author1)
        if (author1 == '')
          author1 <- NULL

        tax_rank1 <- readline(prompt = "Enter tax_rank1 (one of 'var.' or 'subsp.')  : ")
        tax_rank1 <- stringr::str_squish(tax_rank1)
        if (tax_rank1 == '')
          tax_rank1 <- NULL

        if (!is.null(tax_rank1)) {
          if (!tax_rank1 %in% c('var.', 'subsp.')) stop("tax_rank1 must be 'var.' or 'subsp.'")
        }

        tax_name1 <- readline(prompt = "Enter tax_name1 (var. or subsp. name)  : ")
        tax_name1 <- stringr::str_squish(tax_name1)
        if (tax_name1 == '')
          tax_name1 <- NULL

        tax_order <- readline(prompt = "Enter tax_order  : ")
        if (tax_order == '')
          tax_order <- NULL

        morpho_species <-
          choose_prompt(message = "This is NOT a morphotaxa, confirm or set NO if it is a morphotaxa (Press Enter for YES)")

        morpho_species <- !morpho_species

      } else {

        cli::cli_alert_success("Match on Tropicos")

        res_tps_view <- tibble(feat = names(res_tps))

        for (h in 1:nrow(res_tps))
          res_tps_view <- res_tps_view %>%
          bind_cols(unlist(res_tps[h,]))

        names(res_tps_view) <- c("feat", seq(1, nrow(res_tps), 1))

        res_tps_view %>%
          kableExtra::kable(format = "html", escape = F) %>%
          kableExtra::kable_styling("striped", full_width = F) %>%
          print()

        tax_sel <- readline(prompt = "Choose the taxa  (0 if none) : ")

        if (tax_sel != "0") {

          res_tps_selected <- res_tps %>% slice(as.numeric(tax_sel))

          tax_tax <- res_tps_selected$scientificnamewithauthors
          cat(cli::bg_red(paste(tax_tax)))
          cf_ <- readline(prompt = paste("Confirm tax_tax ? press enter"))
          if (cf_ != '') {
            tax_tax <- readline(prompt = "Enter tax_tax  : ")
            if (tax_tax == '')
              tax_tax <- NULL
          }

          tax_gen <- unlist(strsplit(res_tps_selected$scientificname, " "))[1]
          cat(cli::bg_red(paste(tax_gen)))
          cf_ <- readline(prompt = paste("Confirm tax_gen ? press enter"))
          if (cf_ != '') {
            tax_gen <- readline(prompt = "Enter tax_gen  : ")
            if (tax_gen == '')
              tax_gen <- NULL
          }

          tax_esp <- unlist(strsplit(res_tps_selected$scientificname, " "))[2]
          cat(cli::bg_red(paste(tax_esp)))
          cf_ <- readline(prompt = paste("Confirm tax_esp ? press enter"))
          if (cf_ != '') {
            tax_esp <- readline(prompt = "Enter tax_esp  : ")
            if (tax_esp == '')
              tax_esp <- NULL
          }

          tax_rank1 <- res_tps_selected$rankabbreviation
          if (tax_rank1 != "sp.") {
            cat(cli::bg_red(paste(tax_rank1)))
            cf_ <- readline(prompt = paste("Confirm tax_rank1 ? press enter"))
            if (cf_ != '') {
              tax_rank1 <- readline(prompt = "Enter tax_rank1  : ")
              if (tax_rank1 == '')
                tax_rank1 <- NULL
            }
          } else {
            tax_rank1 <- NULL
          }

          tax_name1 <- unlist(strsplit(res_tps_selected$scientificname, " "))[4]
          if (!is.na(tax_name1)) {
            cat(cli::bg_red(paste(tax_name1)))
            cf_ <- readline(prompt = paste("Confirm tax_name1 ? press enter"))
            if (cf_ != '') {
              tax_name1 <- readline(prompt = "Enter tax_name1  : ")
              if (tax_name1 == '')
                tax_name1 <- NULL
            }
          } else {
            tax_name1 <- NULL
          }

          tax_fam <- res_tps_selected$family
          cat(cli::bg_red(paste(tax_fam)))
          cf_ <- readline(prompt = paste("Confirm tax_fam ? press enter"))
          if (cf_ != '') {
            tax_fam <- readline(prompt = "Enter tax_fam  : ")
            if (tax_fam == '')
              tax_fam <- NULL
          }

          author1 <- res_tps_selected$author
          cat(cli::bg_red(paste(author1)))
          cf_ <- readline(prompt = paste("Confirm author1 ? press enter"))
          if (cf_ != '') {
            author1 <- readline(prompt = "Enter author1  : ")
            if (author1 == '')
              author1 <- NULL
          }

          if (!is.null(tax_name1)) {
            author2 <- readline(prompt = "Enter author2  : ")
            if (author2 == '')
              author2 <- NULL
          } else {
            author2 <- NULL
          }

          year_description <- as.numeric(res_tps_selected$displaydate)

        }

      }

    }

  }

  all_growth_form <- choose_growth_form()

  if (is.null(tax_tax) & !is.null(tax_esp) & is.null(author1))
    stop("Provide full name with authors")

  if (is.null(tax_gen) & is.null(tax_esp) & is.null(tax_fam) & is.null(tax_order) & is.null(tax_famclass))
    stop("Provide at least one genus/family/order/class new name to enter")

  if (!is.null(tax_fam) & is.null(tax_tax) & is.null(tax_gen)) {
    tax_tax <- tax_fam
  }

  if(!is.null(tax_order) & is.null(tax_tax) & is.null(tax_gen)) {
    tax_tax <- tax_order
  }

  if(!is.null(tax_famclass) & is.null(tax_tax) & is.null(tax_gen)) {
    tax_tax <- tax_famclass
  }

  check_taxo <- TRUE

  if (is.null(tax_fam) & !is.null(tax_gen)) {

    tax_fam <-
      query_taxa(
        genus = tax_gen,
        verbose = F,
        exact_match = T,
        class = NULL,
        check_synonymy = FALSE,
        extract_traits = FALSE
      )

    if (is.null(nrow(tax_fam)))
      stop("genus not in database")

    tax_fam <- tax_fam %>%
      dplyr::distinct(tax_fam) %>%
      dplyr::pull()
    tax_fam <- tax_fam[which(!is.na(tax_fam))]

    if (length(tax_fam) > 1)
      cli::cli_alert_warning("No tax_fam provided, and two different family names for this genus {tax_fam}.")
    if (length(tax_fam) > 1)
      check_taxo <- FALSE
    if (length(tax_fam) == 1)
      cli::cli_alert_info("No tax_fam provided, based on genus, the following family is chosen {tax_fam}.")
  }

  if(is.null(tax_order) & !is.null(tax_fam)) {
    tax_order <-
      query_taxa(
        family = tax_fam,
        verbose = F,
        exact_match = T,
        class = NULL,
        check_synonymy = FALSE,
        extract_traits = FALSE
      )

    if (is.null(tax_order))
      stop("Family not existing, define order")

    tax_order <-
      tax_order %>%
      dplyr::distinct(tax_order) %>%
      dplyr::pull()

    tax_order <- tax_order[which(!is.na(tax_order))]
    if (length(tax_order) > 1)
      cli::cli_alert_warning("No tax_order provided, and two different order names for this family: {tax_order}")
    if (length(tax_order) > 1)
      check_taxo <- FALSE
    if (length(tax_order) == 1)
      cli::cli_alert_info("No tax_order provided, based on family, the following order is chosen: {tax_order}")
  }

  if(is.null(tax_famclass) & !is.null(tax_order)) {
    tax_famclass <-
      query_taxa(
        order = tax_order,
        verbose = F,
        exact_match = T,
        class = NULL,
        check_synonymy = FALSE,
        extract_traits = FALSE
      ) %>%
      dplyr::distinct(tax_famclass) %>%
      dplyr::pull()
    tax_famclass <- tax_famclass[which(!is.na(tax_famclass))]
    if (length(tax_famclass) > 1)
      cli::cli_alert_warning("No tax_famclass provided, and two different class names for this order: {tax_famclass}")
    if (length(tax_famclass) > 1)
      check_taxo <- FALSE
    if (length(tax_famclass) == 1)
      cli::cli_alert_info("No tax_famclass provided, based on order, the following class is chosen: {tax_famclass}")
  }

  tax_fam_new <- TRUE
  if(!is.null(tax_fam) & check_taxo) {
    searched_tax_fam <-
      dplyr::tbl(mydb_taxa, "table_taxa") %>%
      dplyr::distinct(tax_fam) %>%
      dplyr::filter(tax_fam == !!tax_fam) %>%
      dplyr::collect()
    if(nrow(searched_tax_fam)==0) {
      tax_fam_new <-
        choose_prompt(message = "The provided family name is currently not present in the dictionnary. Are you sure it is correctly spelled? (Press Enter for YES)")


    }
  }

  if(!is.null(tax_tax) & !is.null(tax_gen)) {
    if(!grepl(tax_gen, tax_tax)) stop("\n Genus and tax_tax are provided, but genus is not found within full name, there must be an ERROR")
  }

  if(!is.null(tax_tax) & !is.null(tax_esp)) {
    if(!grepl(tax_esp, tax_tax)) stop("\n Species and tax_tax are provided, but tax_esp is not found within tax_tax, there must be an ERROR")
  }

  if(is.null(tax_gen) & !is.null(tax_esp)) {
    stop("\n species epithet provided but no genus (provide tax_gen)")
  }

  if(!is.null(tax_gen)) {

    family_check <-
      query_taxa(family = tax_fam, exact_match = T, verbose = F, class = NULL, check_synonymy = FALSE, extract_traits = F)

    genus_check <-
      query_taxa(genus = tax_gen,
                                exact_match = T,
                                verbose = F,
                                class = NULL,
                                check_synonymy = FALSE, extract_traits = F)

    family_check %>%
      filter(tax_gen == tax_gen)

    if (!is.null(genus_check)) {
      if (nrow(genus_check) > 0 & !any(family_check$tax_gen[!is.na(family_check$tax_gen)] == tax_gen)) {
        cat(
          paste(
            "\n The provided genus is present in the taxonomic backbone, but with different family name:",
            genus_check$tax_fam[1]
          )
        )
        check_taxo <- FALSE
      }
    }
  }

  # tbl(mydb, "diconame") %>% collect() %>% slice(n())

  if (check_taxo & tax_fam_new) {

    if (!is.null(tax_gen) &
        !is.null(tax_esp))
      paste_taxa <- paste(tax_gen, tax_esp)
    if (!is.null(tax_gen) & is.null(tax_esp))
      paste_taxa <- tax_gen
    if (!is.null(tax_fam) & is.null(tax_gen))
      paste_taxa <- tax_fam
    if (is.null(tax_tax) &
        !is.null(tax_gen) & is.null(tax_esp))
      tax_tax <- tax_gen

    if (is.null(tax_esp))
      tax_esp <- NA
    if (is.null(tax_gen))
      tax_gen <- NA
    if (is.null(tax_fam))
      tax_fam <- NA
    if (is.null(tax_order))
      tax_order <- NA
    if (is.null(tax_rank1))
      tax_rank1 <- NA
    if (is.null(tax_name1))
      tax_name1 <- NA
    if (is.null(tax_rank2))
      tax_rank2 <- NA
    if (is.null(tax_name2))
      tax_name2 <- NA
    # if(is.null(a_habit)) a_habit <- NA
    if (is.null(author1))
      author1 <- NA
    if (is.null(author2))
      author2 <- NA
    if (is.null(author3))
      author3 <- NA

    tax_rank <- NA
    if(!is.na(tax_esp) & is.na(tax_name1) & is.na(tax_name1))
      tax_rank <- "ESP"
    if(is.na(tax_esp) & is.na(tax_name1) & is.na(tax_name2))
      tax_rank <- NA
    if(!is.na(tax_esp) & !is.na(tax_rank1)) {
      if(tax_rank1=="subsp.") tax_rank <- "SUBSP"
      if(tax_rank1=="var.") tax_rank <- "VAR"
      if(tax_rank1=="f.") tax_rank <- "F"
    }

    if(!is.na(tax_rank)) {
      if(tax_rank=="VAR") tax_rankinf <- "VAR"
      if(tax_rank=="SUBSP") tax_rankinf <- "SUBSP"
    }

    if (!is.na(tax_fam) &
        is.na(tax_gen) & is.na(tax_esp))
      tax_rankinf <- "FAM"

    if (!is.na(tax_fam) &
        !is.na(tax_gen) & is.na(tax_esp))
      tax_rankinf <- "GEN"

    if (!is.na(tax_fam) &
        !is.na(tax_gen) & !is.na(tax_esp) & is.na(tax_rank))
      tax_rankinf <- "ESP"

    if (!is.na(tax_fam) &
        !is.na(tax_gen) & !is.na(tax_esp) & tax_rank == "ESP")
      tax_rankinf <- "ESP"

    if (!is.na(tax_order) &
        is.na(tax_fam) &
        is.na(tax_gen) & is.na(tax_esp) & is.na(tax_rank))
      tax_rankinf <- "ORDER"

    if (!is.null(tax_famclass) &
        is.na(tax_order) &
        is.na(tax_fam) &
        is.na(tax_gen) & is.na(tax_esp) & is.na(tax_rank))
      tax_rankinf <- "CLASS"

    if(!is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "ORDER"
    if(!is.null(tax_famclass) & is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "CLASS"
    if(!is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "FAM"
    if(!is.na(tax_fam) & !is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "GEN"
    if(!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp))
      tax_rankesp <- "ESP"

    ## get id of class
    id_tax_fam_class <-
      try_open_postgres_table(table = "table_tax_famclass", con = mydb_taxa) %>%
      # tbl(mydb_taxa, "table_tax_famclass") %>%
      filter(tax_famclass == !!tax_famclass) %>%
      collect()

    new_rec <-
      dplyr::tibble(
        tax_order = tax_order,
        tax_famclass = tax_famclass,
        tax_fam = tax_fam,
        tax_gen = tax_gen,
        tax_esp = tax_esp,
        tax_rank01 = tax_rank1,
        tax_nam01 = tax_name1,
        tax_rank02 = tax_rank2,
        tax_nam02 = tax_name2,
        tax_source = "NEW",
        tax_rank = tax_rank,
        tax_rankinf = tax_rankinf,
        tax_rankesp = tax_rankesp,
        fktax = NA,
        author1 = author1,
        author2 = author2,
        author3 = author3,
        citation = NA,
        year_description = ifelse(!is.null(year_description), year_description, NA),
        idtax_good_n = NA,
        id_tax_famclass = id_tax_fam_class$id_tax_famclass,
        morpho_species = morpho_species
      )

    seek_dup <-
      try_open_postgres_table(table = "table_taxa", con = mydb_taxa)


    if(!is.na(new_rec$tax_famclass))
      seek_dup <- seek_dup %>%
      filter(tax_famclass == !!new_rec$tax_famclass)
    if(!is.na(new_rec$tax_order)) {
      seek_dup <- seek_dup %>%
        filter(tax_order == !!new_rec$tax_order)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_order))
    }

    if(!is.na(new_rec$tax_fam)) {
      seek_dup <- seek_dup %>%
        filter(tax_fam == !!new_rec$tax_fam)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_fam))
    }

    if(!is.na(new_rec$tax_gen)) {
      seek_dup <- seek_dup %>%
        filter(tax_gen == !!new_rec$tax_gen)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_gen))
    }

    if(!is.na(new_rec$tax_esp)) {
      seek_dup <- seek_dup %>%
        filter(tax_esp == !!new_rec$tax_esp)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_esp))
    }

    if(!is.na(new_rec$tax_rank01)) {
      seek_dup <- seek_dup %>%
        filter(tax_rank01 == !!new_rec$tax_rank01)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_rank01))
    }

    if(!is.na(new_rec$tax_nam01)) {
      seek_dup <- seek_dup %>%
        filter(tax_nam01 == !!new_rec$tax_nam01)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_nam01))
    }

    if(!is.na(new_rec$tax_nam02)) {
      seek_dup <- seek_dup %>%
        filter(tax_nam02 == !!new_rec$tax_nam02)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_nam02))
    }

    seek_dup <-
      seek_dup %>%
      collect()

    launch_adding_data <- TRUE

    if (nrow(seek_dup) > 0) {
      cli::cli_h3(cli::bg_red("new entry fit to one entry already in table_taxa"))
      print(as.data.frame(seek_dup))
      launch_adding_data <- FALSE
    }

    if(launch_adding_data) {

      new_rec <-
        .add_modif_field(new_rec)

      new_rec <-
        new_rec %>%
        rename(data_modif_m = date_modif_m,
               data_modif_y = date_modif_y,
               data_modif_d = date_modif_d)

      cli::cli_alert_success(cli::col_yellow("Adding new entry"))
      DBI::dbWriteTable(mydb_taxa, "table_taxa", new_rec, append = TRUE, row.names = FALSE)

      rs <- DBI::dbSendQuery(mydb_taxa, statement = "SELECT MAX(idtax_n) FROM table_taxa")
      lastval <- DBI::dbFetch(rs)
      DBI::dbClearResult(rs)

      new_entry <-
        dplyr::tbl(mydb_taxa, "table_taxa") %>%
        dplyr::filter(idtax_n == !!lastval$max) %>%
        dplyr::collect()

      ### add growth form data
      if(nrow(all_growth_form) > 0) {
        cli::cli_alert_info("add growth form data")

        all_growth_form <- all_growth_form %>%
          dplyr::mutate(idtax = new_entry$idtax_n)

        all_growth_form_pivot <-
          all_growth_form %>%
          tidyr::pivot_wider(names_from = trait,
                             values_from = value)

        add_sp_traits_measures(new_data = all_growth_form_pivot,
                            traits_field = names(all_growth_form_pivot)[2:ncol(all_growth_form_pivot)],
                            idtax = "idtax",
                            add_data = T,
                            ask_before_update = FALSE)

      }

      if(!is.null(synonym_of)) {

        if(!is.list(synonym_of)) {
          stop("synonym_of should be a list with the first element \nbeing the genus and the second element the species epiteth of the taxa of the correct name \nOR the idtax_n")
        }

        if(!any(names(synonym_of)=="genus") & !any(names(synonym_of)=="species") & !any(names(synonym_of)=="id"))
          stop("synonym_of should have at least of the thre following element : genus, species or idtax_n")

        if(!any(names(synonym_of)=="genus")) synonym_of$genus <- NULL
        if(!any(names(synonym_of)=="species")) synonym_of$species <- NULL
        if(!any(names(synonym_of)=="id")) synonym_of$id <- NULL

        syn_searched <-
          query_taxa(genus = synonym_of$genus,
                                    species =synonym_of$species,
                                    ids = synonym_of$id, extract_traits = F)

        print(syn_searched)
        if(nrow(syn_searched)>1) stop("More than 1 taxa as synonym. Select only one.")
        if(nrow(syn_searched)==0) stop("No taxa found in the dictionnary. Select one.")

        update_dico_name(
          synonym_of = list(id = syn_searched$idtax_good_n),
          id_searched = new_entry$idtax_n,
          ask_before_update = FALSE,
          add_backup = FALSE,
          show_results = FALSE
        )

      } else {
        # update_dico_name(new_id_diconame_good = new_entry$id_n, id_search = new_entry$id_n,
        #                  ask_before_update = FALSE, add_backup = FALSE, show_results = FALSE)
      }

      # print(dplyr::tbl(mydb, "table_taxa") %>%
      #         dplyr::collect() %>%
      #         dplyr::filter(idtax_n == max(idtax_n)))

      print(dplyr::tbl(mydb_taxa, "table_taxa") %>%
              dplyr::filter(idtax_n == !!new_entry$idtax_n) %>%
              collect() %>%
              as.data.frame())
    }

  } else {

    cli::cli_alert_warning("NO ADDED ENTRY")
  }
}






#' Get backups of modified taxonomic data
#'
#' List taxonomic data that has been modified
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id look backups for a specific id (of taxonomic table)
#' @param last_months look backups performed this last month
#' @param last_10_entry look the last 10 backups performed
#' @param last look the last backup
#'
#' @return A tibble
#' @export
get_updates_diconame <- function(id = NULL,
                                 last_months = NULL,
                                 last_10_entry = TRUE,
                                 last = NULL) {
  
  
  mydb <- call.mydb()
  
  tb <-
    dplyr::tbl(mydb, "followup_updates_diconames")
  
  if (!is.null(id) & !last_10_entry) {
    var <- rlang::enquo(id)
    entry <-
      tb %>%
      dplyr::filter(id_n == !!var) %>%
      dplyr::collect()
  }
  
  if(!is.null(last_months) & !last_10_entry) {
    
    one_month_earlier <-
      lubridate::month(Sys.Date() %m-% months(last_months))
    if(one_month_earlier < 10) one_month_earlier <- paste0("0", one_month_earlier)
    one_month_earlier <-
      paste(lubridate::year(Sys.Date()), one_month_earlier, sep="-")
    
    this_month <-
      lubridate::month(Sys.Date())
    if(this_month<10) this_month <- paste0("0", this_month)
    this_month <-
      paste(lubridate::year(Sys.Date()), this_month, sep="-")
    
    query <-
      paste0("SELECT * FROM followup_updates_diconames WHERE date_modified ILIKE '%",one_month_earlier,"%' OR date_modified ILIKE '%", this_month, "%'")
    
    rs <- DBI::dbSendQuery(mydb, query)
    entry <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    entry <- dplyr::as_tibble(entry)
  }
  
  if(last_10_entry) {
    max_id <-
      tb %>%
      dplyr::arrange(dplyr::desc(id_fol_up_diconame)) %>%
      dplyr::select(id_fol_up_diconame) %>%
      dplyr::slice_max() %>%
      dplyr::collect()
    
    if(is.null(last)) {
      last_10 <- (dplyr::pull(max_id)-10)
    }else{
      last_10 <- (dplyr::pull(max_id)-last)
    }
    
    entry <-
      dplyr::tbl %>%
      dplyr::filter(id_fol_up_diconame > last_10) %>%
      dplyr::collect()
    
  }
  
  return(entry)
}


