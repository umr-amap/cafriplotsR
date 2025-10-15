# Taxonomic Name Matching and Standardization Functions
#
# This file contains functions for intelligent matching of taxonomic names to the backbone.
# Implements genus-constrained fuzzy matching for improved precision.
#
# Main functions:
# - parse_taxonomic_name(): Parse names into components (genus, species, etc.)
# - match_taxonomic_names(): Intelligently match names with scoring
# - .match_exact(): Helper for exact matching
# - .match_genus_constrained(): Helper for genus-constrained matching
# - .score_matches(): Helper for scoring and ranking matches
#
# Dependencies: DBI, dplyr, stringr, cli, RecordLinkage


#' Parse taxonomic name into components
#'
#' @description
#' Parse a taxonomic name string into its components: genus, species epithet,
#' infraspecific ranks/names, and author names.
#'
#' @param name Character string of taxonomic name
#'
#' @return A list with components:
#'   - genus: Genus name (first word)
#'   - species: Species epithet (second word if present)
#'   - infraspecific: Full infraspecific part (everything after species)
#'   - full_name_no_auth: Genus + species + infraspecific without authors
#'   - input_name: Original input
#'
#' @author Claude Code Assistant
#'
#' @examples
#' parse_taxonomic_name("Gilbertiodendron dewevrei")
#' parse_taxonomic_name("Anthonotha macrophylla var. oblongifolia")
#' parse_taxonomic_name("Brachystegia")
#'
#' @keywords internal
#' @export
parse_taxonomic_name <- function(name) {

  # Trim and clean
  name <- stringr::str_trim(name)

  if (is.na(name) || name == "") {
    return(list(
      genus = NA_character_,
      species = NA_character_,
      infraspecific = NA_character_,
      full_name_no_auth = NA_character_,
      input_name = name
    ))
  }

  # Split by whitespace
  parts <- stringr::str_split(name, "\\s+")[[1]]

  if (length(parts) == 0) {
    return(list(
      genus = NA_character_,
      species = NA_character_,
      infraspecific = NA_character_,
      full_name_no_auth = NA_character_,
      input_name = name
    ))
  }

  # Extract genus (first part, capitalize first letter)
  genus <- parts[1]
  genus <- stringr::str_to_title(genus)

  # Extract species if present (second part, must be lowercase to be species epithet)
  species <- NA_character_
  infraspecific_start <- 3

  if (length(parts) >= 2) {
    # Check if second part looks like species epithet (lowercase)
    if (stringr::str_detect(parts[2], "^[a-z]")) {
      species <- tolower(parts[2])
    } else {
      # Second part is not species, might be author or something else
      infraspecific_start <- 2
    }
  }

  # Extract infraspecific parts (everything after genus+species)
  infraspecific <- NA_character_
  if (length(parts) >= infraspecific_start) {
    infraspecific <- paste(parts[infraspecific_start:length(parts)], collapse = " ")
  }

  # Build full name without authors
  full_parts <- c(genus)
  if (!is.na(species)) full_parts <- c(full_parts, species)
  if (!is.na(infraspecific) && infraspecific != "") full_parts <- c(full_parts, infraspecific)
  full_name_no_auth <- paste(full_parts, collapse = " ")

  return(list(
    genus = genus,
    species = species,
    infraspecific = infraspecific,
    full_name_no_auth = full_name_no_auth,
    input_name = name
  ))
}



#' Match taxonomic names to backbone with intelligent strategy
#'
#' @description
#' Match taxonomic names using a hierarchical strategy:
#' 1. Exact match on full name
#' 2. Genus-constrained fuzzy match (if genus matches, search species within genus)
#' 3. Full fuzzy match (last resort)
#' Results are scored and ranked by match quality.
#'
#' @param names Character vector of taxonomic names to match
#' @param method Matching method: "auto" (default), "exact", "genus_constrained", "fuzzy"
#' @param max_matches Maximum number of suggestions per name (default: 10)
#' @param min_similarity Minimum Levenshtein similarity score (0-1, default: 0.7)
#' @param include_synonyms Include synonyms in results (default: TRUE)
#' @param return_scores Return similarity scores (default: TRUE)
#' @param include_authors Try matching with author names (default: FALSE)
#' @param con Database connection (if NULL, will call call.mydb.taxa())
#' @param verbose Show progress messages (default: TRUE)
#'
#' @return A tibble with columns:
#'   - input_name: Original name provided
#'   - match_rank: Rank of this match (1 = best)
#'   - matched_name: Matched name from backbone
#'   - idtax_n: Taxa ID
#'   - idtax_good_n: Accepted taxa ID (for synonyms)
#'   - match_method: How the match was found (exact, genus_constrained, fuzzy)
#'   - match_score: Similarity score (if return_scores = TRUE)
#'   - is_synonym: Whether matched name is a synonym
#'   - accepted_name: Accepted name (if synonym)
#'   - genus: Matched genus
#'   - species: Matched species epithet
#'   - family: Matched family
#'
#' @author Claude Code Assistant
#'
#' @examples
#' \dontrun{
#' # Match a single name
#' match_taxonomic_names("Gilbertodendron dewevrei")  # Note typo
#'
#' # Match multiple names
#' names <- c("Brachystegia laurentii", "Julbernardia seretii", "Unknown species")
#' matches <- match_taxonomic_names(names, max_matches = 5)
#'
#' # Exact match only
#' matches <- match_taxonomic_names(names, method = "exact")
#' }
#'
#' @export
match_taxonomic_names <- function(names,
                                  method = c("auto", "exact", "genus_constrained", "fuzzy", "hierarchical"),
                                  max_matches = 10,
                                  min_similarity = 0.7,
                                  include_synonyms = TRUE,
                                  return_scores = TRUE,
                                  include_authors = FALSE,
                                  con = NULL,
                                  verbose = TRUE) {

  method <- match.arg(method)

  # Get database connection
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Validate inputs
  if (length(names) == 0) {
    cli::cli_alert_warning("No names provided")
    return(tibble())
  }

  # Remove NAs and empty strings
  valid_indices <- !is.na(names) & names != ""
  if (sum(valid_indices) == 0) {
    cli::cli_alert_warning("No valid names provided")
    return(tibble())
  }

  valid_names <- names[valid_indices]

  if (verbose) {
    cli::cli_h2("Matching {length(valid_names)} taxonomic name(s)")
  }

  # Parse all names
  if (verbose) cli::cli_alert_info("Parsing taxonomic names...")
  parsed_names <- lapply(valid_names, parse_taxonomic_name)

  # Get full taxonomy table for matching
  if (verbose) cli::cli_alert_info("Loading taxonomic backbone...")

  backbone <- tbl(con, "table_taxa") %>%
    select(idtax_n, idtax_good_n, tax_gen, tax_esp, tax_fam,
           tax_rank01, tax_nam01, tax_rank02, tax_nam02,
           author1, author2, author3) %>%
    collect()

  # Build searchable names
  backbone <- backbone %>%
    mutate(
      tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), tax_gen),
      tax_infra_level = ifelse(!is.na(tax_esp),
                               paste0(tax_gen, " ", tax_esp,
                                      ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                      ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                      ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                      ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
                               tax_gen),
      tax_infra_level_auth = ifelse(!is.na(tax_esp),
                                    paste0(tax_gen, " ", tax_esp,
                                           ifelse(!is.na(author1), paste0(" ", author1), ""),
                                           ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                           ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                           ifelse(!is.na(author2), paste0(" ", author2), ""),
                                           ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                           ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
                                           ifelse(!is.na(author3), paste0(" ", author3), "")),
                                    tax_gen)
    )

  # Process each name
  all_matches <- list()

  for (i in seq_along(parsed_names)) {
    parsed <- parsed_names[[i]]

    if (verbose && length(parsed_names) > 1) {
      cli::cli_alert_info("Processing ({i}/{length(parsed_names)}): {parsed$input_name}")
    }

    matches <- .match_single_name(
      parsed = parsed,
      backbone = backbone,
      method = method,
      max_matches = max_matches,
      min_similarity = min_similarity,
      include_authors = include_authors,
      verbose = verbose && length(parsed_names) == 1
    )

    all_matches[[i]] <- matches
  }

  # Combine results
  result <- bind_rows(all_matches)

  # Add synonym information if requested
  if (include_synonyms && nrow(result) > 0) {
    result <- .add_synonym_info(result, backbone)
  }

  # Remove score column if not requested
  if (!return_scores && "match_score" %in% names(result)) {
    result <- result %>% select(-match_score)
  }

  if (verbose) {
    n_exact <- sum(result$match_method == "exact" & result$match_rank == 1, na.rm = TRUE)
    n_genus <- sum(result$match_method == "genus_constrained" & result$match_rank == 1, na.rm = TRUE)
    n_fuzzy <- sum(result$match_method == "fuzzy" & result$match_rank == 1, na.rm = TRUE)
    n_no_match <- sum(is.na(result$idtax_n) & result$match_rank == 1, na.rm = TRUE)

    cli::cli_alert_success("Matching complete!")
    cli::cli_ul(c(
      "Exact matches: {n_exact}",
      "Genus-constrained matches: {n_genus}",
      "Fuzzy matches: {n_fuzzy}",
      "No matches: {n_no_match}"
    ))
  }

  return(result)
}



#' Match a single parsed name (internal helper)
#' @keywords internal
.match_single_name <- function(parsed, backbone, method, max_matches,
                               min_similarity, include_authors, verbose) {

  # Try exact match first (unless method restricts it)
  if (method %in% c("auto", "exact", "hierarchical")) {
    exact_matches <- .match_exact(parsed, backbone, include_authors)

    if (nrow(exact_matches) > 0) {
      if (verbose) cli::cli_alert_success("Found exact match")
      return(exact_matches %>%
               slice_head(n = max_matches) %>%
               mutate(match_rank = row_number()))
    }
  }

  # If no exact match and method allows, try genus-constrained
  if (method %in% c("auto", "genus_constrained", "hierarchical") && !is.na(parsed$genus)) {
    genus_matches <- .match_genus_constrained(parsed, backbone, min_similarity, include_authors)

    if (nrow(genus_matches) > 0) {
      if (verbose) cli::cli_alert_info("Found genus-constrained matches")
      scored <- .score_matches(genus_matches, parsed$input_name, "genus_constrained")
      return(scored %>%
               slice_head(n = max_matches) %>%
               mutate(match_rank = row_number()))
    }
  }

  # Last resort: full fuzzy match
  if (method %in% c("auto", "fuzzy", "hierarchical")) {
    fuzzy_matches <- .match_fuzzy(parsed, backbone, min_similarity, include_authors)

    if (nrow(fuzzy_matches) > 0) {
      if (verbose) cli::cli_alert_info("Found fuzzy matches")
      scored <- .score_matches(fuzzy_matches, parsed$input_name, "fuzzy")
      return(scored %>%
               slice_head(n = max_matches) %>%
               mutate(match_rank = row_number()))
    }
  }

  # No matches found
  if (verbose) cli::cli_alert_warning("No matches found")
  return(tibble(
    input_name = parsed$input_name,
    match_rank = 1,
    matched_name = NA_character_,
    idtax_n = NA_integer_,
    idtax_good_n = NA_integer_,
    match_method = "no_match",
    match_score = NA_real_,
    genus = NA_character_,
    species = NA_character_,
    family = NA_character_
  ))
}



#' Exact matching helper
#' @keywords internal
.match_exact <- function(parsed, backbone, include_authors) {

  search_field <- if (include_authors) "tax_infra_level_auth" else "tax_infra_level"

  matches <- backbone %>%
    filter(tolower(!!sym(search_field)) == tolower(parsed$input_name))

  if (nrow(matches) > 0) {
    matches <- matches %>%
      transmute(
        input_name = parsed$input_name,
        matched_name = !!sym(search_field),
        idtax_n = idtax_n,
        idtax_good_n = idtax_good_n,
        match_method = "exact",
        match_score = 1.0,
        genus = tax_gen,
        species = tax_esp,
        family = tax_fam
      )
  }

  return(matches)
}



#' Genus-constrained fuzzy matching helper
#' @keywords internal
.match_genus_constrained <- function(parsed, backbone, min_similarity, include_authors) {

  if (is.na(parsed$genus)) {
    return(tibble())
  }

  # First, find matching genera (exact or fuzzy)
  genus_matches <- backbone %>%
    filter(!is.na(tax_gen)) %>%
    distinct(tax_gen) %>%
    pull(tax_gen)

  # Calculate similarity to genera
  genus_sims <- RecordLinkage::levenshteinSim(tolower(parsed$genus), tolower(genus_matches))

  # Keep genera above threshold
  matching_genera <- genus_matches[genus_sims >= min_similarity]

  if (length(matching_genera) == 0) {
    return(tibble())
  }

  # Now search for species within these genera
  search_field <- if (include_authors) "tax_infra_level_auth" else "tax_infra_level"

  constrained_backbone <- backbone %>%
    filter(tax_gen %in% matching_genera)

  # Calculate similarity scores
  sims <- RecordLinkage::levenshteinSim(
    tolower(parsed$input_name),
    tolower(constrained_backbone[[search_field]])
  )

  # Filter by threshold
  matches <- constrained_backbone[sims >= min_similarity, ]

  if (nrow(matches) > 0) {
    matches <- matches %>%
      transmute(
        input_name = parsed$input_name,
        matched_name = !!sym(search_field),
        idtax_n = idtax_n,
        idtax_good_n = idtax_good_n,
        match_method = "genus_constrained",
        match_score = sims[sims >= min_similarity],
        genus = tax_gen,
        species = tax_esp,
        family = tax_fam
      )
  }

  return(matches)
}



#' Full fuzzy matching helper (last resort)
#' @keywords internal
.match_fuzzy <- function(parsed, backbone, min_similarity, include_authors) {

  search_field <- if (include_authors) "tax_infra_level_auth" else "tax_infra_level"

  # Calculate similarity scores for entire backbone
  sims <- RecordLinkage::levenshteinSim(
    tolower(parsed$input_name),
    tolower(backbone[[search_field]])
  )

  # Filter by threshold
  matches <- backbone[sims >= min_similarity, ]

  if (nrow(matches) > 0) {
    matches <- matches %>%
      transmute(
        input_name = parsed$input_name,
        matched_name = !!sym(search_field),
        idtax_n = idtax_n,
        idtax_good_n = idtax_good_n,
        match_method = "fuzzy",
        match_score = sims[sims >= min_similarity],
        genus = tax_gen,
        species = tax_esp,
        family = tax_fam
      )
  }

  return(matches)
}



#' Score and rank matches
#' @keywords internal
.score_matches <- function(matches, input_name, method) {

  if (nrow(matches) == 0) return(matches)

  # Sort by score (descending) and taxonomic completeness
  matches <- matches %>%
    arrange(
      desc(match_score),
      desc(!is.na(species)),  # Prefer species-level matches
      desc(!is.na(genus)),
      matched_name
    )

  return(matches)
}



#' Add synonym information to matches
#' @keywords internal
.add_synonym_info <- function(matches, backbone) {

  if (nrow(matches) == 0) return(matches)

  # Check which matches are synonyms
  matches <- matches %>%
    mutate(
      is_synonym = !is.na(idtax_good_n) & idtax_n != idtax_good_n
    )

  # Get accepted names for synonyms
  synonym_ids <- matches %>%
    filter(is_synonym) %>%
    pull(idtax_good_n) %>%
    unique()

  if (length(synonym_ids) > 0) {
    accepted_names <- backbone %>%
      filter(idtax_n %in% synonym_ids) %>%
      select(idtax_n, accepted_name = tax_infra_level)

    matches <- matches %>%
      left_join(accepted_names, by = c("idtax_good_n" = "idtax_n"))
  } else {
    matches <- matches %>%
      mutate(accepted_name = NA_character_)
  }

  return(matches)
}



#' Standardize taxonomic names in a data frame
#'
#' @description
#' Process a data frame with taxonomic names and add standardized matches.
#' This is a batch wrapper around `match_taxonomic_names()` that returns
#' the original data with added columns for the best match.
#'
#' @param data A data frame or tibble containing taxonomic names
#' @param name_column Name of column containing taxonomic names (quoted or unquoted)
#' @param method Matching method: "auto" (default), "exact", "genus_constrained", "fuzzy"
#' @param min_similarity Minimum similarity score (0-1, default: 0.7)
#' @param include_synonyms Include synonym information (default: TRUE)
#' @param include_authors Try matching with author names (default: FALSE)
#' @param con Database connection (if NULL, will call call.mydb.taxa())
#' @param verbose Show progress messages (default: TRUE)
#' @param keep_all_matches Keep all matches (default: FALSE, only keeps best match)
#'
#' @return The input data frame with added columns:
#'   - matched_name: Best matching name from backbone (or NA if no match)
#'   - idtax_n: Taxa ID for matched name
#'   - idtax_good_n: Accepted taxa ID (for synonyms)
#'   - match_method: How the match was found
#'   - match_score: Similarity score
#'   - match_genus: Matched genus
#'   - match_species: Matched species epithet
#'   - match_family: Matched family
#'   - is_synonym: Whether match is a synonym
#'   - accepted_name: Accepted name (if synonym)
#'   If keep_all_matches = TRUE, returns one row per match with match_rank column
#'
#' @author Claude Code Assistant
#'
#' @examples
#' \dontrun{
#' # Standardize names in a data frame
#' data <- tibble(
#'   plot_id = c(1, 1, 2),
#'   tree_id = c("A01", "A02", "B01"),
#'   species = c("Pericopsis elata", "Garcinea kola", "Brachystegia laurentii")
#' )
#'
#' # Add best match for each name
#' data_matched <- standardize_taxonomic_batch(data, name_column = "species")
#'
#' # Keep all matches (for manual review)
#' data_all_matches <- standardize_taxonomic_batch(
#'   data,
#'   name_column = "species",
#'   keep_all_matches = TRUE
#' )
#' }
#'
#' @export
standardize_taxonomic_batch <- function(data,
                                       name_column,
                                       method = c("auto", "exact", "genus_constrained", "fuzzy"),
                                       min_similarity = 0.7,
                                       include_synonyms = TRUE,
                                       include_authors = FALSE,
                                       con = NULL,
                                       verbose = TRUE,
                                       keep_all_matches = FALSE) {

  method <- match.arg(method)

  # Handle name_column as quoted or unquoted
  name_col_quo <- rlang::enquo(name_column)
  name_col_str <- rlang::as_name(name_col_quo)

  # Validate column exists
  if (!name_col_str %in% names(data)) {
    cli::cli_abort("Column {.field {name_col_str}} not found in data")
  }

  # Get unique names to match
  unique_names <- data %>%
    dplyr::pull(!!name_col_quo) %>%
    unique()

  if (verbose) {
    cli::cli_h1("Batch Taxonomic Standardization")
    cli::cli_alert_info("Processing {nrow(data)} rows with {length(unique_names)} unique names")
  }

  # Match unique names
  matches <- match_taxonomic_names(
    names = unique_names,
    method = method,
    max_matches = if (keep_all_matches) 10 else 1,
    min_similarity = min_similarity,
    include_synonyms = include_synonyms,
    return_scores = TRUE,
    include_authors = include_authors,
    con = con,
    verbose = verbose
  )

  # Filter to best match unless keep_all_matches = TRUE
  if (!keep_all_matches && nrow(matches) > 0) {
    matches <- matches %>%
      dplyr::filter(match_rank == 1)
  }

  # Rename columns to avoid conflicts
  matches <- matches %>%
    dplyr::rename(
      !!name_col_str := input_name,
      match_genus = genus,
      match_species = species,
      match_family = family
    )

  # Join back to original data
  result <- data %>%
    dplyr::left_join(matches, by = name_col_str)

  if (verbose) {
    n_matched <- result %>%
      dplyr::filter(!is.na(idtax_n)) %>%
      dplyr::distinct(!!name_col_quo) %>%
      nrow()

    n_unmatched <- result %>%
      dplyr::filter(is.na(idtax_n)) %>%
      dplyr::distinct(!!name_col_quo) %>%
      nrow()

    cli::cli_alert_success("Batch processing complete!")
    cli::cli_ul(c(
      "Matched: {n_matched}/{length(unique_names)} unique names",
      "Unmatched: {n_unmatched}/{length(unique_names)} unique names"
    ))

    if (keep_all_matches && nrow(result) > nrow(data)) {
      cli::cli_alert_info("Returned {nrow(result)} rows (multiple matches per name)")
    }
  }

  return(result)
}
