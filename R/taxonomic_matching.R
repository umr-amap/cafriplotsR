# Taxonomic Name Matching and Standardization Functions
#
# This file contains functions for intelligent matching of taxonomic names to the backbone.
# Implements genus-constrained fuzzy matching using SQL-side SIMILARITY for improved performance.
#
# Main functions:
# - clean_taxonomic_name(): Clean and normalize taxonomic names
# - parse_taxonomic_name(): Parse names into components (genus, species, etc.)
# - match_taxonomic_names(): Intelligently match names with scoring
# - .match_exact_sql(): Helper for SQL-side exact matching
# - .match_genus_constrained_sql(): Helper for SQL-side genus-constrained matching
# - .match_fuzzy_sql(): Helper for SQL-side fuzzy matching
#
# Dependencies: DBI, dplyr, stringr, cli, glue

#' Clean and normalize taxonomic name
#'
#' @description
#' Clean taxonomic names by removing common botanical annotation patterns
#' that interfere with matching:
#' - "sp.", "sp", "spp.", "spp" after genus (e.g., "Garcinia sp." → "Garcinia")
#' - "cf.", "cf", "aff.", "?" between genus and species (e.g., "Garcinia cf. kola" → "Garcinia kola")
#' - Extra whitespace and punctuation
#'
#' @param name Character string of taxonomic name
#'
#' @return Cleaned taxonomic name (character string)
#'
#' @author Claude Code Assistant
#'
#' @examples
#' clean_taxonomic_name("Fabaceae sp.")        # → "Fabaceae"
#' clean_taxonomic_name("Garcinia cf. kola")   # → "Garcinia kola"
#' clean_taxonomic_name("Brachystegia spp")    # → "Brachystegia"
#' clean_taxonomic_name("Gilbertiodendron  ?  dewevrei")  # → "Gilbertiodendron dewevrei"
#'
#' @keywords internal
#' @export
clean_taxonomic_name <- function(name) {

  if (is.na(name) || name == "") {
    return(name)
  }

  # Trim whitespace
  name <- stringr::str_trim(name)

  # Remove "sp.", "sp", "spp.", "spp" when it's the last word (genus sp. pattern)
  # Match: word + whitespace + sp/spp with optional period + optional whitespace + end
  name <- stringr::str_replace(name, "\\s+spp?\\.?\\s*$", "")

  # Remove uncertainty markers between genus and species: "cf.", "cf", "aff.", "aff", "?"
  # These typically appear between first and second word
  # Pattern: whitespace + (cf|aff) with optional period or ? + whitespace
  name <- stringr::str_replace_all(name, "\\s+(cf|aff)\\.?\\s+", " ")
  name <- stringr::str_replace_all(name, "\\s+\\?\\s+", " ")

  # Clean up multiple spaces
  name <- stringr::str_replace_all(name, "\\s+", " ")
  
  # replace underscore by space
  name <- stringr::str_replace_all(name, "_", " ")

  # Final trim
  name <- stringr::str_trim(name)

  return(name)
}


#' Parse taxonomic name into components
#'
#' @description
#' Parse a taxonomic name string into its components: genus, species epithet,
#' infraspecific ranks/names, and author names.
#'
#' @param name Character string of taxonomic name
#'
#' @return A list with components:
#'   - rank: Detected rank ("family", "order", "genus", "species", or "unknown")
#'   - genus: Genus name (first word, or NA if family/order detected)
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
      rank = "unknown",
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
      rank = "unknown",
      genus = NA_character_,
      species = NA_character_,
      infraspecific = NA_character_,
      full_name_no_auth = NA_character_,
      input_name = name
    ))
  }

  # Detect rank based on botanical nomenclature patterns
  first_word <- stringr::str_to_title(parts[1])
  rank <- "genus"  # Default assumption

  # Check if it's a class (ends in -opsida or -psida)
  if (stringr::str_detect(first_word, "(o|p)psida$")) {
    rank <- "class"
  }
  # Check if it's a family (ends in -aceae)
  else if (stringr::str_detect(first_word, "aceae$")) {
    rank <- "family"
  }
  # Check if it's an order (ends in -ales)
  else if (stringr::str_detect(first_word, "ales$")) {
    rank <- "order"
  }
  # If single word, keep as genus
  # If multiple words with lowercase second word, it's species or infraspecific
  else if (length(parts) >= 2 && stringr::str_detect(parts[2], "^[a-z]")) {
    rank <- "species"
  }

  # For class/family/order, return simplified structure
  if (rank %in% c("class", "family", "order")) {
    return(list(
      rank = rank,
      genus = NA_character_,
      species = NA_character_,
      infraspecific = NA_character_,
      full_name_no_auth = first_word,
      input_name = name
    ))
  }

  # For genus/species, parse normally
  genus <- first_word

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
    rank = rank,
    genus = genus,
    species = species,
    infraspecific = infraspecific,
    full_name_no_auth = full_name_no_auth,
    input_name = name
  ))
}



#' Match taxonomic names to backbone with intelligent SQL-side strategy
#'
#' @description
#' Match taxonomic names using a hierarchical strategy with SQL-side fuzzy matching:
#' 1. Exact match on full name (SQL)
#' 2. Genus-constrained fuzzy match (SQL SIMILARITY within matched genera)
#' 3. Full fuzzy match (SQL SIMILARITY on full database - last resort)
#' Results are scored and ranked by match quality.
#'
#' This approach minimizes data transfer and leverages PostgreSQL's optimized
#' SIMILARITY function, making it much faster especially with slow connections.
#'
#' @param names Character vector of taxonomic names to match
#' @param method Matching method: "auto" (default), "exact", "genus_constrained", "fuzzy"
#' @param max_matches Maximum number of suggestions per name (default: 10)
#' @param min_similarity Minimum similarity threshold (0-1, default: 0.3 for SQL SIMILARITY)
#' @param include_synonyms Include synonyms in results (default: TRUE)
#' @param return_scores Return similarity scores (default: TRUE)
#' @param include_authors Try matching with author names (default: FALSE)
#' @param con Database connection. If NULL and `backbone` is also NULL, the
#'   function will try the cached backbone first and fall back to opening a
#'   database connection via `call.mydb.taxa()`.
#' @param backbone Optional cached backbone tibble (as returned by
#'   `load_backbone_cache()`). When supplied, all matching runs in R against
#'   this in-memory backbone — no database round-trips, works fully offline.
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
#'   - tax_gen: Matched genus
#'   - tax_esp: Matched species epithet
#'   - tax_fam: Matched family
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
                                  min_similarity = 0.3,
                                  include_synonyms = TRUE,
                                  return_scores = TRUE,
                                  include_authors = FALSE,
                                  con = NULL,
                                  backbone = NULL,
                                  verbose = TRUE) {

  method <- match.arg(method)

  # Decide engine: R-side (cached backbone) vs SQL-side (live DB).
  # Preference order:
  #   1. Caller-provided backbone (R-side, no DB needed)
  #   2. Caller-provided con   (SQL-side)
  #   3. Cached backbone if it exists (R-side, fully offline)
  #   4. Open con via call.mydb.taxa() (SQL-side, last resort)
  use_r_side <- !is.null(backbone)

  if (!use_r_side && is.null(con)) {
    if (cache_exists()) {
      backbone <- load_backbone_cache()
      if (!is.null(backbone)) {
        use_r_side <- TRUE
        if (verbose) cli::cli_alert_info("Using cached backbone (R-side matching)")
      }
    }
    if (!use_r_side) {
      con <- call.mydb.taxa()
    }
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

  # Clean names first (remove sp., cf., etc.)
  if (verbose) cli::cli_alert_info("Cleaning taxonomic names...")
  cleaned_names <- sapply(valid_names, clean_taxonomic_name)

  # Parse all names
  if (verbose) cli::cli_alert_info("Parsing taxonomic names...")
  parsed_names <- lapply(cleaned_names, parse_taxonomic_name)

  # Store original input name in parsed results
  for (i in seq_along(parsed_names)) {
    parsed_names[[i]]$original_input <- valid_names[i]
  }

  # Process each name (R-side or SQL-side)
  all_matches <- list()

  for (i in seq_along(parsed_names)) {
    parsed <- parsed_names[[i]]

    if (verbose && length(parsed_names) > 1) {
      cli::cli_alert_info("Processing ({i}/{length(parsed_names)}): {parsed$input_name}")
    }

    if (use_r_side) {
      matches <- .match_single_name_r(
        parsed = parsed,
        backbone = backbone,
        method = method,
        max_matches = max_matches,
        min_similarity = min_similarity,
        include_authors = include_authors,
        verbose = verbose && length(parsed_names) == 1
      )
    } else {
      matches <- .match_single_name_sql(
        parsed = parsed,
        con = con,
        method = method,
        max_matches = max_matches,
        min_similarity = min_similarity,
        include_authors = include_authors,
        verbose = verbose && length(parsed_names) == 1
      )
    }

    all_matches[[i]] <- matches
  }

  # Combine results
  result <- bind_rows(all_matches)

  # Add synonym information if requested
  if (include_synonyms && nrow(result) > 0 && any(!is.na(result$idtax_n))) {
    if (use_r_side) {
      result <- .add_synonym_info_r(result, backbone)
    } else {
      result <- .add_synonym_info_sql(result, con)
    }
  }

  # Remove score column if not requested
  if (!return_scores && "match_score" %in% names(result)) {
    result <- result %>% select(-match_score)
  }

  if (verbose && nrow(result) > 0) {
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



#' Match a single parsed name using SQL-side queries (internal helper)
#' @keywords internal
.match_single_name_sql <- function(parsed, con, method, max_matches,
                                   min_similarity, include_authors, verbose) {

  # Try exact match first (unless method restricts it)
  if (method %in% c("auto", "exact", "hierarchical")) {
    exact_matches <- .match_exact_sql(parsed, con, include_authors, max_matches)

    if (nrow(exact_matches) > 0) {
      if (verbose) cli::cli_alert_success("Found exact match")
      return(exact_matches %>%
               mutate(match_rank = row_number()))
    }
  }

  # If no exact match and method allows, try genus-constrained
  if (method %in% c("auto", "genus_constrained", "hierarchical") && !is.na(parsed$genus)) {
    genus_matches <- .match_genus_constrained_sql(parsed, con, min_similarity,
                                                   include_authors, max_matches)

    if (nrow(genus_matches) > 0) {
      if (verbose) cli::cli_alert_info("Found genus-constrained matches")
      return(genus_matches %>%
               mutate(match_rank = row_number()))
    }
  }

  # Last resort: full fuzzy match
  if (method %in% c("auto", "fuzzy", "hierarchical")) {
    fuzzy_matches <- .match_fuzzy_sql(parsed, con, min_similarity,
                                      include_authors, max_matches)

    if (nrow(fuzzy_matches) > 0) {
      if (verbose) cli::cli_alert_info("Found fuzzy matches")
      return(fuzzy_matches %>%
               mutate(match_rank = row_number()))
    }
  }

  # No matches found
  if (verbose) cli::cli_alert_warning("No matches found")
  return(tibble(
    input_name = parsed$original_input %||% parsed$input_name,
    match_rank = 1,
    matched_name = NA_character_,
    idtax_n = NA_integer_,
    idtax_good_n = NA_integer_,
    match_method = "no_match",
    match_score = NA_real_,
    tax_gen = NA_character_,
    tax_esp = NA_character_,
    tax_fam = NA_character_,
    tax_level = NA_character_
  ))
}



#' Exact matching helper using SQL
#' @keywords internal
.match_exact_sql <- function(parsed, con, include_authors, max_matches) {

  # Handle class level searches
  if (parsed$rank == "class") {
    # Search in tax_famclass column
    sql <- glue::glue_sql("
      SELECT
        idtax_n,
        idtax_good_n,
        tax_gen,
        tax_esp,
        tax_fam,
        tax_level,
        tax_famclass AS matched_name,
        1.0 AS similarity_score
      FROM table_taxa
      WHERE lower(tax_famclass) = lower({search_name})
        AND tax_level = 'higher'
      LIMIT {max_matches}
    ", search_name = parsed$full_name_no_auth, max_matches = max_matches, .con = con)

    result <- func_try_fetch(con = con, sql = sql)

    if (nrow(result) > 0) {
      result <- result %>%
        mutate(
          input_name = parsed$original_input %||% parsed$input_name,
          match_method = "exact",
          match_score = 1.0
        ) %>%
        select(input_name, matched_name, idtax_n, idtax_good_n,
               match_method, match_score, tax_gen, tax_esp, tax_fam, tax_level) %>%
        slice(1)  # Only return one representative match for class
    }

    return(result)
  }

  # Handle family/order searches differently
  if (parsed$rank == "family") {
    # Search in tax_fam column
    sql <- glue::glue_sql("
      SELECT
        idtax_n,
        idtax_good_n,
        tax_gen,
        tax_esp,
        tax_fam,
        tax_level,
        tax_fam AS matched_name,
        1.0 AS similarity_score
      FROM table_taxa
      WHERE lower(tax_fam) = lower({search_name})
        AND tax_level = 'family'
      LIMIT {max_matches}
    ", search_name = parsed$full_name_no_auth, max_matches = max_matches, .con = con)

    result <- func_try_fetch(con = con, sql = sql)

    if (nrow(result) > 0) {
      result <- result %>%
        mutate(
          input_name = parsed$original_input %||% parsed$input_name,
          match_method = "exact",
          match_score = 1.0
        ) %>%
        select(input_name, matched_name, idtax_n, idtax_good_n,
               match_method, match_score, tax_gen, tax_esp, tax_fam, tax_level) %>%
        slice(1)  # Only return one representative match for family
    }

    return(result)
  }

  if (parsed$rank == "order") {
    # Search in tax_order column
    sql <- glue::glue_sql("
      SELECT
        idtax_n,
        idtax_good_n,
        tax_gen,
        tax_esp,
        tax_fam,
        tax_level,
        tax_order AS matched_name,
        1.0 AS similarity_score
      FROM table_taxa
      WHERE lower(tax_order) = lower({search_name})
        AND tax_level = 'order'
      LIMIT {max_matches}
    ", search_name = parsed$full_name_no_auth, max_matches = max_matches, .con = con)

    result <- func_try_fetch(con = con, sql = sql)

    if (nrow(result) > 0) {
      result <- result %>%
        mutate(
          input_name = parsed$original_input %||% parsed$input_name,
          match_method = "exact",
          match_score = 1.0
        ) %>%
        select(input_name, matched_name, idtax_n, idtax_good_n,
               match_method, match_score, tax_gen, tax_esp, tax_fam, tax_level) %>%
        slice(1)  # Only return one representative match for order
    }

    return(result)
  }

  # For genus/species, use normal matching
  # Build the name field to search (with or without authors)
  if (include_authors) {
    name_field <- glue::glue_sql("
      CASE WHEN tax_esp IS NOT NULL THEN
        concat(tax_gen, ' ', tax_esp,
               COALESCE(' ' || author1, ''),
               COALESCE(' ' || tax_rank01, ''),
               COALESCE(' ' || tax_nam01, ''),
               COALESCE(' ' || author2, ''),
               COALESCE(' ' || tax_rank02, ''),
               COALESCE(' ' || tax_nam02, ''),
               COALESCE(' ' || author3, ''))
      ELSE tax_gen
      END", .con = con)
  } else {
    name_field <- glue::glue_sql("
      CASE WHEN tax_esp IS NOT NULL THEN
        concat(tax_gen, ' ', tax_esp,
               COALESCE(' ' || tax_rank01, ''),
               COALESCE(' ' || tax_nam01, ''),
               COALESCE(' ' || tax_rank02, ''),
               COALESCE(' ' || tax_nam02, ''))
      ELSE tax_gen
      END", .con = con)
  }

  sql <- glue::glue_sql("
    SELECT
      idtax_n,
      idtax_good_n,
      tax_gen,
      tax_esp,
      tax_fam,
      tax_level,
      tax_rank01,
      tax_nam01,
      tax_rank02,
      tax_nam02,
      author1,
      author2,
      author3,
      {name_field} AS matched_name,
      1.0 AS similarity_score
    FROM table_taxa
    WHERE lower({name_field}) = lower({search_name})
    LIMIT {max_matches}
  ", search_name = parsed$input_name, max_matches = max_matches, .con = con)

  result <- func_try_fetch(con = con, sql = sql)

  if (nrow(result) > 0) {
    result <- result %>%
      mutate(
        input_name = parsed$original_input %||% parsed$input_name,
        match_method = "exact",
        match_score = 1.0
      ) %>%
      select(input_name, matched_name, idtax_n, idtax_good_n,
             match_method, match_score, tax_gen, tax_esp, tax_fam, tax_level)
  }

  return(result)
}



#' Genus-constrained fuzzy matching helper using SQL SIMILARITY
#' @keywords internal
.match_genus_constrained_sql <- function(parsed, con, min_similarity,
                                         include_authors, max_matches) {

  if (is.na(parsed$genus)) {
    return(tibble())
  }

  # Step 1: Find matching genera using SQL SIMILARITY
  sql_genus <- glue::glue_sql("
    SELECT DISTINCT tax_gen,
           SIMILARITY(lower(tax_gen), lower({genus_search})) AS genus_sim
    FROM table_taxa
    WHERE tax_gen IS NOT NULL
      AND SIMILARITY(lower(tax_gen), lower({genus_search})) >= {min_sim}
    ORDER BY genus_sim DESC
    LIMIT 10
  ", genus_search = parsed$genus, min_sim = min_similarity, .con = con)

  matching_genera <- func_try_fetch(con = con, sql = sql_genus)

  if (nrow(matching_genera) == 0) {
    return(tibble())
  }

  # Step 2: Search for species within matched genera using SQL SIMILARITY
  genera_list <- matching_genera$tax_gen

  # Build the name field to search
  if (include_authors) {
    name_field <- glue::glue_sql("
      CASE WHEN tax_esp IS NOT NULL THEN
        concat(tax_gen, ' ', tax_esp,
               COALESCE(' ' || author1, ''),
               COALESCE(' ' || tax_rank01, ''),
               COALESCE(' ' || tax_nam01, ''),
               COALESCE(' ' || author2, ''),
               COALESCE(' ' || tax_rank02, ''),
               COALESCE(' ' || tax_nam02, ''),
               COALESCE(' ' || author3, ''))
      ELSE tax_gen
      END", .con = con)
  } else {
    name_field <- glue::glue_sql("
      CASE WHEN tax_esp IS NOT NULL THEN
        concat(tax_gen, ' ', tax_esp,
               COALESCE(' ' || tax_rank01, ''),
               COALESCE(' ' || tax_nam01, ''),
               COALESCE(' ' || tax_rank02, ''),
               COALESCE(' ' || tax_nam02, ''))
      ELSE tax_gen
      END", .con = con)
  }

  sql_species <- glue::glue_sql("
    SELECT
      idtax_n,
      idtax_good_n,
      tax_gen,
      tax_esp,
      tax_fam,
      tax_level,
      tax_rank01,
      tax_nam01,
      tax_rank02,
      tax_nam02,
      author1,
      author2,
      author3,
      {name_field} AS matched_name,
      SIMILARITY(lower({name_field}), lower({search_name})) AS similarity_score
    FROM table_taxa
    WHERE tax_gen IN ({genera_list*})
      AND SIMILARITY(lower({name_field}), lower({search_name})) >= {min_sim}
    ORDER BY similarity_score DESC, tax_esp IS NOT NULL DESC
    LIMIT {max_matches}
  ", search_name = parsed$input_name, genera_list = genera_list,
     min_sim = min_similarity, max_matches = max_matches, .con = con)

  result <- func_try_fetch(con = con, sql = sql_species)

  if (nrow(result) > 0) {
    result <- result %>%
      mutate(
        input_name = parsed$original_input %||% parsed$input_name,
        match_method = "genus_constrained",
        match_score = similarity_score
      ) %>%
      select(input_name, matched_name, idtax_n, idtax_good_n,
             match_method, match_score, tax_gen, tax_esp, tax_fam, tax_level)
  }

  return(result)
}



#' Full fuzzy matching helper using SQL SIMILARITY (last resort)
#' @keywords internal
.match_fuzzy_sql <- function(parsed, con, min_similarity, include_authors, max_matches) {

  # Handle class level fuzzy matching
  if (parsed$rank == "class") {
    sql <- glue::glue_sql("
      SELECT DISTINCT ON (tax_famclass)
        idtax_n,
        idtax_good_n,
        tax_gen,
        tax_esp,
        tax_fam,
        tax_level,
        tax_famclass AS matched_name,
        SIMILARITY(lower(tax_famclass), lower({search_name})) AS similarity_score
      FROM table_taxa
      WHERE tax_famclass IS NOT NULL
        AND tax_level = 'higher'
        AND SIMILARITY(lower(tax_famclass), lower({search_name})) >= {min_sim}
      ORDER BY tax_famclass, similarity_score DESC
      LIMIT {max_matches}
    ", search_name = parsed$full_name_no_auth, min_sim = min_similarity,
       max_matches = max_matches, .con = con)

    result <- func_try_fetch(con = con, sql = sql)

    if (nrow(result) > 0) {
      result <- result %>%
        mutate(
          input_name = parsed$original_input %||% parsed$input_name,
          match_method = "fuzzy",
          match_score = similarity_score
        ) %>%
        select(input_name, matched_name, idtax_n, idtax_good_n,
               match_method, match_score, tax_gen, tax_esp, tax_fam, tax_level)
    }

    return(result)
  }

  # Handle family/order searches with fuzzy matching on appropriate column
  if (parsed$rank == "family") {
    sql <- glue::glue_sql("
      SELECT DISTINCT ON (tax_fam)
        idtax_n,
        idtax_good_n,
        tax_gen,
        tax_esp,
        tax_fam,
        tax_level,
        tax_fam AS matched_name,
        SIMILARITY(lower(tax_fam), lower({search_name})) AS similarity_score
      FROM table_taxa
      WHERE tax_fam IS NOT NULL
        AND SIMILARITY(lower(tax_fam), lower({search_name})) >= {min_sim}
      ORDER BY tax_fam, similarity_score DESC
      LIMIT {max_matches}
    ", search_name = parsed$full_name_no_auth, min_sim = min_similarity,
       max_matches = max_matches, .con = con)

    result <- func_try_fetch(con = con, sql = sql)

    if (nrow(result) > 0) {
      result <- result %>%
        mutate(
          input_name = parsed$original_input %||% parsed$input_name,
          match_method = "fuzzy",
          match_score = similarity_score
        ) %>%
        select(input_name, matched_name, idtax_n, idtax_good_n,
               match_method, match_score, tax_gen, tax_esp, tax_fam, tax_level)
    }

    return(result)
  }

  if (parsed$rank == "order") {
    # Try fuzzy match on tax_order if column exists
    sql <- glue::glue_sql("
      SELECT DISTINCT ON (tax_order)
        idtax_n,
        idtax_good_n,
        tax_gen,
        tax_esp,
        tax_fam,
        tax_level,
        tax_order AS matched_name,
        SIMILARITY(lower(tax_order), lower({search_name})) AS similarity_score
      FROM table_taxa
      WHERE tax_order IS NOT NULL
        AND SIMILARITY(lower(tax_order), lower({search_name})) >= {min_sim}
      ORDER BY tax_order, similarity_score DESC
      LIMIT {max_matches}
    ", search_name = parsed$full_name_no_auth, min_sim = min_similarity,
       max_matches = max_matches, .con = con)

    result <- tryCatch({
      func_try_fetch(con = con, sql = sql)
    }, error = function(e) {
      # If tax_order column doesn't exist, return empty tibble
      tibble()
    })

    if (nrow(result) > 0) {
      result <- result %>%
        mutate(
          input_name = parsed$original_input %||% parsed$input_name,
          match_method = "fuzzy",
          match_score = similarity_score
        ) %>%
        select(input_name, matched_name, idtax_n, idtax_good_n,
               match_method, match_score, tax_gen, tax_esp, tax_fam, tax_level)
    }

    return(result)
  }

  # For genus/species, use normal fuzzy matching
  # Build the name field to search
  if (include_authors) {
    name_field <- glue::glue_sql("
      CASE WHEN tax_esp IS NOT NULL THEN
        concat(tax_gen, ' ', tax_esp,
               COALESCE(' ' || author1, ''),
               COALESCE(' ' || tax_rank01, ''),
               COALESCE(' ' || tax_nam01, ''),
               COALESCE(' ' || author2, ''),
               COALESCE(' ' || tax_rank02, ''),
               COALESCE(' ' || tax_nam02, ''),
               COALESCE(' ' || author3, ''))
      ELSE tax_gen
      END", .con = con)
  } else {
    name_field <- glue::glue_sql("
      CASE WHEN tax_esp IS NOT NULL THEN
        concat(tax_gen, ' ', tax_esp,
               COALESCE(' ' || tax_rank01, ''),
               COALESCE(' ' || tax_nam01, ''),
               COALESCE(' ' || tax_rank02, ''),
               COALESCE(' ' || tax_nam02, ''))
      ELSE tax_gen
      END", .con = con)
  }

  sql <- glue::glue_sql("
    SELECT
      idtax_n,
      idtax_good_n,
      tax_gen,
      tax_esp,
      tax_fam,
      tax_level,
      tax_rank01,
      tax_nam01,
      tax_rank02,
      tax_nam02,
      author1,
      author2,
      author3,
      {name_field} AS matched_name,
      SIMILARITY(lower({name_field}), lower({search_name})) AS similarity_score
    FROM table_taxa
    WHERE SIMILARITY(lower({name_field}), lower({search_name})) >= {min_sim}
    ORDER BY similarity_score DESC, tax_esp IS NOT NULL DESC
    LIMIT {max_matches}
  ", search_name = parsed$input_name, min_sim = min_similarity,
     max_matches = max_matches, .con = con)

  result <- func_try_fetch(con = con, sql = sql)

  if (nrow(result) > 0) {
    result <- result %>%
      mutate(
        input_name = parsed$original_input %||% parsed$input_name,
        match_method = "fuzzy",
        match_score = similarity_score
      ) %>%
      select(input_name, matched_name, idtax_n, idtax_good_n,
             match_method, match_score, tax_gen, tax_esp, tax_fam, tax_level)
  }

  return(result)
}



#' Add synonym information to matches using SQL
#' @keywords internal
.add_synonym_info_sql <- function(matches, con) {

  if (nrow(matches) == 0) return(matches)

  # Check which matches are synonyms
  matches <- matches %>%
    dplyr::mutate(
      is_synonym = !is.na(.data$idtax_good_n) & .data$idtax_n != .data$idtax_good_n
    )

  # Get accepted names for synonyms
  synonym_ids <- matches %>%
    dplyr::filter(.data$is_synonym) %>%
    dplyr::pull("idtax_good_n") %>%
    unique() %>%
    stats::na.omit()

  if (length(synonym_ids) > 0) {
    # Build name field for accepted names
    name_field <- glue::glue_sql("
      CASE WHEN tax_esp IS NOT NULL THEN
        concat(tax_gen, ' ', tax_esp,
               COALESCE(' ' || tax_rank01, ''),
               COALESCE(' ' || tax_nam01, ''),
               COALESCE(' ' || tax_rank02, ''),
               COALESCE(' ' || tax_nam02, ''))
      ELSE tax_gen
      END", .con = con)

    sql <- glue::glue_sql("
      SELECT idtax_n,
             {name_field} AS accepted_name
      FROM table_taxa
      WHERE idtax_n IN ({synonym_ids*})
    ", synonym_ids = synonym_ids, .con = con)

    accepted_names <- func_try_fetch(con = con, sql = sql)

    matches <- matches %>%
      left_join(accepted_names, by = c("idtax_good_n" = "idtax_n"))
  } else {
    matches <- matches %>%
      mutate(accepted_name = NA_character_)
  }

  return(matches)
}



# ===========================================================================
# R-side matching (no DB) — operates on cached backbone tibble.
# Mirrors the .match_*_sql() helpers above but uses stringdist instead of
# PostgreSQL's pg_trgm SIMILARITY(). The backbone tibble is the one produced
# by mod_auto_matching.R / cache_backbone.R and contains the precomputed
# tax_sp_level / tax_gen_level / tax_fam_level / tax_class_level columns.
# ===========================================================================

#' Build the searchable full-name field from a backbone tibble
#'
#' Mirrors the SQL `concat(tax_gen, ' ', tax_esp, ' ' || tax_rank01, ...)`
#' construction but in R, returning a character vector aligned with the
#' input rows. Where `tax_esp` is NA, falls back to `tax_gen`.
#'
#' @keywords internal
.build_backbone_name_field <- function(backbone, include_authors = FALSE) {
  gen <- backbone$tax_gen
  na_gen <- is.na(gen)

  # Start with tax_gen (NA rows will be set to NA at the end)
  out <- ifelse(na_gen, "", gen)

  has_esp <- !is.na(backbone$tax_esp) & backbone$tax_esp != ""
  out <- paste0(out, ifelse(has_esp, paste0(" ", backbone$tax_esp), ""))

  if (isTRUE(include_authors) && "author1" %in% names(backbone)) {
    has_a1 <- !is.na(backbone$author1) & backbone$author1 != ""
    out <- paste0(out, ifelse(has_a1, paste0(" ", backbone$author1), ""))
  }

  for (col in c("tax_rank01", "tax_nam01", "tax_rank02", "tax_nam02")) {
    if (col %in% names(backbone)) {
      val <- backbone[[col]]
      has_val <- !is.na(val) & val != ""
      out <- paste0(out, ifelse(has_val, paste0(" ", val), ""))
    }
  }

  # Rows where tax_gen was NA: leave NA (no usable name to search against)
  out[na_gen] <- NA_character_
  out
}


#' Trigram-Jaccard similarity (R-side equivalent of pg_trgm SIMILARITY)
#'
#' PostgreSQL's `SIMILARITY()` from `pg_trgm` is Jaccard on padded trigrams.
#' `stringdist::stringsim(method = "jaccard", q = 3)` gives the same
#' coefficient on q-grams; padding is not identical to pg_trgm's leading-two-
#' spaces / trailing-one-space padding, so absolute scores differ slightly.
#' Empirically (see commit history) the two agree at correlation ~0.99 with
#' mean absolute delta ~0.03 on a representative sample of taxonomic names,
#' so the same `min_similarity` thresholds carry over without retuning.
#'
#' @param x Character vector
#' @param y Single character string to compare each `x` against
#' @keywords internal
.trigram_sim <- function(x, y) {
  if (length(x) == 0L) return(numeric(0))
  stringdist::stringsim(tolower(x), tolower(y), method = "jaccard", q = 3L)
}


#' Match a single parsed name using R-side queries on cached backbone
#' @keywords internal
.match_single_name_r <- function(parsed, backbone, method, max_matches,
                                 min_similarity, include_authors, verbose) {

  if (method %in% c("auto", "exact", "hierarchical")) {
    exact_matches <- .match_exact_r(parsed, backbone, include_authors, max_matches)
    if (nrow(exact_matches) > 0) {
      if (verbose) cli::cli_alert_success("Found exact match")
      return(exact_matches %>% mutate(match_rank = row_number()))
    }
  }

  if (method %in% c("auto", "genus_constrained", "hierarchical") && !is.na(parsed$genus)) {
    genus_matches <- .match_genus_constrained_r(parsed, backbone, min_similarity,
                                                include_authors, max_matches)
    if (nrow(genus_matches) > 0) {
      if (verbose) cli::cli_alert_info("Found genus-constrained matches")
      return(genus_matches %>% mutate(match_rank = row_number()))
    }
  }

  if (method %in% c("auto", "fuzzy", "hierarchical")) {
    fuzzy_matches <- .match_fuzzy_r(parsed, backbone, min_similarity,
                                    include_authors, max_matches)
    if (nrow(fuzzy_matches) > 0) {
      if (verbose) cli::cli_alert_info("Found fuzzy matches")
      return(fuzzy_matches %>% mutate(match_rank = row_number()))
    }
  }

  if (verbose) cli::cli_alert_warning("No matches found")
  tibble(
    input_name   = parsed$original_input %||% parsed$input_name,
    match_rank   = 1L,
    matched_name = NA_character_,
    idtax_n      = NA_integer_,
    idtax_good_n = NA_integer_,
    match_method = "no_match",
    match_score  = NA_real_,
    tax_gen      = NA_character_,
    tax_esp      = NA_character_,
    tax_fam      = NA_character_,
    tax_level    = NA_character_
  )
}


#' Exact match against cached backbone
#' @keywords internal
.match_exact_r <- function(parsed, backbone, include_authors, max_matches) {

  search_lc <- tolower(parsed$full_name_no_auth %||% parsed$input_name)

  if (parsed$rank == "class") {
    hits <- backbone %>%
      dplyr::filter(
        !is.na(.data$tax_famclass),
        tolower(.data$tax_famclass) == search_lc,
        .data$tax_level == "higher"
      ) %>%
      dplyr::slice_head(n = 1L)

    if (nrow(hits) == 0L) return(tibble())
    return(.shape_r_match(hits, parsed, "exact", 1.0,
                          name_col = "tax_famclass"))
  }

  if (parsed$rank == "family") {
    hits <- backbone %>%
      dplyr::filter(
        !is.na(.data$tax_fam),
        tolower(.data$tax_fam) == search_lc,
        .data$tax_level == "family"
      ) %>%
      dplyr::slice_head(n = 1L)

    if (nrow(hits) == 0L) return(tibble())
    return(.shape_r_match(hits, parsed, "exact", 1.0,
                          name_col = "tax_fam"))
  }

  if (parsed$rank == "order") {
    # The cache does not include tax_order — order-level exact match isn't
    # supported offline. Fall through (caller will try fuzzy or return no_match).
    return(tibble())
  }

  # Genus / species: build the full name field and case-insensitive equal
  full_names <- .build_backbone_name_field(backbone, include_authors)
  hits_idx <- which(tolower(full_names) == tolower(parsed$input_name))

  if (length(hits_idx) == 0L) return(tibble())

  hits <- backbone[hits_idx[seq_len(min(length(hits_idx), max_matches))], , drop = FALSE]
  hits$matched_name <- full_names[hits_idx[seq_len(nrow(hits))]]

  .shape_r_match(hits, parsed, "exact", 1.0, name_col = NULL)
}


#' Genus-constrained fuzzy match against cached backbone
#' @keywords internal
.match_genus_constrained_r <- function(parsed, backbone, min_similarity,
                                       include_authors, max_matches) {

  if (is.na(parsed$genus)) return(tibble())

  # Step 1: candidate genera by trigram similarity on tax_gen
  genera <- backbone %>%
    dplyr::filter(!is.na(.data$tax_gen)) %>%
    dplyr::distinct(.data$tax_gen) %>%
    dplyr::pull(.data$tax_gen)

  if (length(genera) == 0L) return(tibble())

  genus_sim <- .trigram_sim(genera, parsed$genus)
  keep <- which(genus_sim >= min_similarity)
  if (length(keep) == 0L) return(tibble())

  # Top 10 genera (matches SQL helper)
  ord <- order(genus_sim[keep], decreasing = TRUE)
  top_keep <- keep[ord[seq_len(min(10L, length(ord)))]]
  candidate_genera <- genera[top_keep]

  # Step 2: rows within those genera, score full-name similarity
  sub <- backbone %>%
    dplyr::filter(.data$tax_gen %in% candidate_genera)

  if (nrow(sub) == 0L) return(tibble())

  full_names <- .build_backbone_name_field(sub, include_authors)
  scores <- .trigram_sim(full_names, parsed$input_name)

  pass <- which(scores >= min_similarity)
  if (length(pass) == 0L) return(tibble())

  ord2 <- order(scores[pass], !is.na(sub$tax_esp[pass]), decreasing = TRUE)
  picked <- pass[ord2[seq_len(min(max_matches, length(ord2)))]]

  hits <- sub[picked, , drop = FALSE]
  hits$matched_name <- full_names[picked]
  hits$.score <- scores[picked]

  .shape_r_match(hits, parsed, "genus_constrained", hits$.score,
                 name_col = NULL)
}


#' Full fuzzy match against cached backbone (last resort)
#' @keywords internal
.match_fuzzy_r <- function(parsed, backbone, min_similarity,
                           include_authors, max_matches) {

  if (parsed$rank == "class") {
    candidates <- backbone %>%
      dplyr::filter(!is.na(.data$tax_famclass), .data$tax_level == "higher") %>%
      dplyr::distinct(.data$tax_famclass, .keep_all = TRUE)

    if (nrow(candidates) == 0L) return(tibble())
    scores <- .trigram_sim(candidates$tax_famclass, parsed$full_name_no_auth)
    pass <- which(scores >= min_similarity)
    if (length(pass) == 0L) return(tibble())

    ord <- order(scores[pass], decreasing = TRUE)
    picked <- pass[ord[seq_len(min(max_matches, length(ord)))]]
    hits <- candidates[picked, , drop = FALSE]
    hits$matched_name <- candidates$tax_famclass[picked]
    hits$.score <- scores[picked]
    return(.shape_r_match(hits, parsed, "fuzzy", hits$.score, name_col = NULL))
  }

  if (parsed$rank == "family") {
    candidates <- backbone %>%
      dplyr::filter(!is.na(.data$tax_fam)) %>%
      dplyr::distinct(.data$tax_fam, .keep_all = TRUE)

    if (nrow(candidates) == 0L) return(tibble())
    scores <- .trigram_sim(candidates$tax_fam, parsed$full_name_no_auth)
    pass <- which(scores >= min_similarity)
    if (length(pass) == 0L) return(tibble())

    ord <- order(scores[pass], decreasing = TRUE)
    picked <- pass[ord[seq_len(min(max_matches, length(ord)))]]
    hits <- candidates[picked, , drop = FALSE]
    hits$matched_name <- candidates$tax_fam[picked]
    hits$.score <- scores[picked]
    return(.shape_r_match(hits, parsed, "fuzzy", hits$.score, name_col = NULL))
  }

  if (parsed$rank == "order") {
    # tax_order not in cache — offline fuzzy not supported at order level.
    return(tibble())
  }

  # Genus / species: full-name fuzzy across the whole backbone
  full_names <- .build_backbone_name_field(backbone, include_authors)
  scores <- .trigram_sim(full_names, parsed$input_name)

  pass <- which(scores >= min_similarity)
  if (length(pass) == 0L) return(tibble())

  ord <- order(scores[pass], !is.na(backbone$tax_esp[pass]), decreasing = TRUE)
  picked <- pass[ord[seq_len(min(max_matches, length(ord)))]]
  hits <- backbone[picked, , drop = FALSE]
  hits$matched_name <- full_names[picked]
  hits$.score <- scores[picked]

  .shape_r_match(hits, parsed, "fuzzy", hits$.score, name_col = NULL)
}


#' Shape an R-side match result to the same columns the SQL path returns
#' @keywords internal
.shape_r_match <- function(hits, parsed, method, score, name_col) {
  if (!is.null(name_col)) {
    hits$matched_name <- hits[[name_col]]
  }

  tibble(
    input_name   = parsed$original_input %||% parsed$input_name,
    matched_name = hits$matched_name,
    idtax_n      = hits$idtax_n,
    idtax_good_n = hits$idtax_good_n,
    match_method = method,
    match_score  = score,
    tax_gen      = hits$tax_gen,
    tax_esp      = if ("tax_esp" %in% names(hits)) hits$tax_esp else NA_character_,
    tax_fam      = hits$tax_fam,
    tax_level    = if ("tax_level" %in% names(hits)) hits$tax_level else NA_character_
  )
}


#' Add synonym information to matches using cached backbone
#' @keywords internal
.add_synonym_info_r <- function(matches, backbone) {

  if (nrow(matches) == 0L) return(matches)

  matches <- matches %>%
    dplyr::mutate(
      is_synonym = !is.na(.data$idtax_good_n) & .data$idtax_n != .data$idtax_good_n
    )

  synonym_ids <- matches %>%
    dplyr::filter(.data$is_synonym) %>%
    dplyr::pull("idtax_good_n") %>%
    unique() %>%
    stats::na.omit()

  if (length(synonym_ids) == 0L) {
    matches$accepted_name <- NA_character_
    return(matches)
  }

  acc_rows <- backbone %>%
    dplyr::filter(.data$idtax_n %in% synonym_ids) %>%
    dplyr::distinct(.data$idtax_n, .keep_all = TRUE)

  if (nrow(acc_rows) == 0L) {
    matches$accepted_name <- NA_character_
    return(matches)
  }

  acc_names <- .build_backbone_name_field(acc_rows, include_authors = FALSE)
  accepted <- tibble(
    idtax_n       = acc_rows$idtax_n,
    accepted_name = acc_names
  )

  matches %>%
    dplyr::left_join(accepted, by = c("idtax_good_n" = "idtax_n"))
}



#' Standardize taxonomic names in a data frame
#'
#' @description
#' Process a data frame with taxonomic names and add standardized matches.
#' This is a batch wrapper around `match_taxonomic_names()` that returns
#' the original data with added columns for the best match.
#'
#' Uses SQL-side fuzzy matching for optimal performance with slow connections.
#'
#' @param data A data frame or tibble containing taxonomic names
#' @param name_column Name of column containing taxonomic names (quoted or unquoted)
#' @param method Matching method: "auto" (default), "exact", "genus_constrained", "fuzzy"
#' @param min_similarity Minimum similarity score (0-1, default: 0.3)
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
                                       min_similarity = 0.3,
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
  if (nrow(matches) > 0) {
    matches <- matches %>%
      dplyr::rename(
        !!name_col_str := input_name,
        match_genus = tax_gen,
        match_species = tax_esp,
        match_family = tax_fam
      )
  }

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
