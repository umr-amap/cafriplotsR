# OpenForis Collect data pre-processing
#
# Internal (non-exported) functions for converting raw OpenForis Collect
# tree/plot exports into clean formats ready for the Feature Wizard or
# direct use with add_subplot_features() / add_traits_measures().
#
# These are specific to the OpenForis field data collection workflow
# used for Central African forest plots.


#' Process an OpenForis census export into clean data frames
#'
#' Reads the raw tree and plot xlsx files exported from OpenForis Collect,
#' decodes coded columns using the provided code-list CSVs, and returns
#' a list with three components ready for import:
#' \itemize{
#'   \item \code{census_metadata}: plot-level census info for \code{add_subplot_features()}
#'   \item \code{recruits}: new individuals ready for the Import Wizard
#'   \item \code{measurements}: long-format trait table ready for the Feature Wizard
#' }
#'
#' @param data_dir Path to the directory containing the OpenForis data exports
#'   (xlsx files). The function looks for a tree file matching
#'   \code{tree_file_pattern} and a plot file matching \code{plot_file_pattern}.
#'   Ignored if \code{tree_file} is provided explicitly.
#' @param codes_dir Path to the directory containing the OpenForis code-list
#'   CSVs. The function auto-detects files by pattern:
#'   \code{code_list_observation*}, \code{code_list_pom*},
#'   \code{code_light*}, \code{code_*status*}.
#'   Ignored for any code file provided explicitly.
#' @param tree_file Path to the tree-level xlsx. If NULL, auto-detected from
#'   \code{data_dir} using \code{tree_file_pattern}.
#' @param plot_file Path to the plot-level xlsx. If NULL, auto-detected from
#'   \code{data_dir} using \code{plot_file_pattern}. Set to FALSE to skip.
#' @param observation_codes Path to the observation code-list CSV. If NULL,
#'   auto-detected from \code{codes_dir}. Set to FALSE to skip.
#' @param pom_codes Path to the POM observation code-list CSV. If NULL,
#'   auto-detected from \code{codes_dir}. Set to FALSE to skip.
#' @param light_codes Path to the light code-list CSV. If NULL,
#'   auto-detected from \code{codes_dir}. Set to FALSE to skip.
#' @param status_codes Path to the stem-status code-list CSV. If NULL,
#'   auto-detected from \code{codes_dir}. Set to FALSE to skip.
#' @param tree_file_pattern Glob pattern to find the tree xlsx in
#'   \code{data_dir} (default \code{"arbre*"}).
#' @param plot_file_pattern Glob pattern to find the plot xlsx in
#'   \code{data_dir} (default \code{"plot*"}).
#' @param team_leader Character. Team leader name(s).
#' @param principal_investigator Character. PI name(s).
#' @param data_manager Character. Data manager name(s).
#' @param additional_people Character. Comma-separated additional people.
#' @param census Integer. Census number. If NULL, must be provided later.
#' @param specimen_prefix Character prefix for specimen numbers (e.g. "PIRD").
#'   If NULL, specimen columns are left as-is.
#' @param specimen_remap_file Path or filename of an xlsx file with two columns:
#'   the first contains the original specimen number (as in the tree file), the
#'   second contains the replacement number. If just a filename (no directory),
#'   it is looked up in \code{data_dir}. The original values are kept in a
#'   \code{specimen_number_original} column. The function stops if the new
#'   numbers contain duplicates. NULL (default) skips remapping.
#' @param specimen_locality Character. Locality string for specimens
#'   (e.g. "Mbalmayo, Centre"). NULL to omit.
#' @param specimen_country Character. Country name for specimens. NULL to omit.
#' @param specimen_col_month Integer. Collection month for specimens. NULL to omit.
#' @param specimen_col_year Integer. Collection year for specimens. NULL to omit.
#' @param specimen_collector Character. Collector code for specimens
#'   (e.g. "PIRD"). Stored as \code{colnam}. NULL to omit.
#' @param specimen_description_col Column name from the tree file used to build
#'   a description string (default \code{"stem_diameter"}). Set to NULL to skip.
#' @param specimen_branch_position_col Column name from the tree file giving the
#'   origin of the collected branch (default \code{"branch_position"}). Rows
#'   with the value \code{"rejet"} get "Echantillon collecté sur un rejet"
#'   appended to the description; \code{"shade_branch"} and
#'   \code{"light_branch"} are ignored.
#' @param recruit_state Character. Value in the \code{state} column that marks
#'   recruits (default \code{"recruted"} — the OpenForis spelling).
#' @param plot_name_col Column name for plot name in tree file
#'   (default \code{"plot_plot_name"}).
#' @param tag_col Column name for existing-tree tag
#'   (default \code{"arbre"}).
#' @param recruit_tag_col Column name for recruit tag
#'   (default \code{"label_recrut"}).
#' @param herbarium_col Column name for the herbarium/collection number in the
#'   tree file (default \code{"herbarium_nbe_char"}). This column is used to
#'   identify trees with specimens. If the column is absent and no alternative
#'   name is provided, a warning is issued and the specimen list will be empty.
#'
#' @return A list with components:
#' \describe{
#'   \item{census_metadata}{Tibble with plot_name, date_year, date_month,
#'     date_day, census, and people columns. NULL if no plot file.}
#'   \item{recruits}{Tibble of recruited individuals with wide-format
#'     measurements. NULL if no recruits found.}
#'   \item{measurements}{Tibble in long format with columns: plot_name,
#'     tag, trait_name, traitvalue (numeric), traitvalue_char (character).
#'     Ready for the Feature Wizard measurements step.}
#'   \item{specimens}{Tibble of specimens ready for \code{add_specimens()},
#'     with columns: plot_name, tag, specimen_number, herbarium_nbe_char,
#'     colnbr, idtax_n, description, locality, country, colnam, colm, coly,
#'     additional_people. NULL if no specimens found.}
#'   \item{multi_stems}{Tibble of candidate multi-stem groupings with columns:
#'     plot_name, tag, group_tag (parent tag), stem_order (position within
#'     group), original_tax_name, idtax, flag (validation issues). NULL if
#'     no multi-stem individuals detected. Review flags before uploading.}
#'   \item{all_stems}{Tibble in the same wide format as \code{recruits} but
#'     covering every stem in the dataset (recruits and existing individuals
#'     alike). Useful for bulk imports or cross-census checks where you need
#'     all stems in one table.}
#'   \item{duplicated_stems}{Tibble of all rows involved in duplicated
#'     \code{plot_name + tag} combinations (i.e. every row that shares a
#'     plot_name/tag pair with at least one other row), with columns
#'     plot_name, tag, state, species_scientific_name, species_code,
#'     stem_diameter, quadrat (whichever are present). NULL if no duplicates.
#'     A \code{warning()} is also raised when duplicates are found.}
#'   \item{summary}{List with counts: n_plots, n_recruits, n_existing,
#'     n_measurements, n_specimens, n_multi_stem_groups, n_all_stems,
#'     n_duplicated_stems, trait_names.}
#' }
#'
#' @examples
#' \dontrun{
#' # Simplest usage — just point to the two directories:
#' result <- process_openforis_census(
#'   data_dir = "path/to/mission/plot/",
#'   codes_dir = "path/to/openforis/",
#'   team_leader = "Jane Doe",
#'   additional_people = "John Smith, Alice Brown",
#'   census = 2,
#'   specimen_prefix = "PIRD"
#' )
#'
#' # Or specify files individually:
#' result <- process_openforis_census(
#'   tree_file = "path/to/arbre.xlsx",
#'   plot_file = "path/to/plot.xlsx",
#'   observation_codes = "path/to/code_list_observations.csv",
#'   status_codes = "path/to/code_recensus_status.csv",
#'   light_codes = FALSE,   # skip light decoding
#'   census = 2
#' )
#'
#' # Long-format measurements — upload to Feature Wizard
#' head(result$measurements)
#'
#' # Write to xlsx for the app
#' writexl::write_xlsx(result$measurements, "measurements_long.xlsx")
#' writexl::write_xlsx(result$recruits, "recruits.xlsx")
#' }
#'
#' @keywords internal
process_openforis_census <- function(data_dir = NULL,
                                     codes_dir = NULL,
                                     tree_file = NULL,
                                     plot_file = NULL,
                                     observation_codes = NULL,
                                     pom_codes = NULL,
                                     light_codes = NULL,
                                     status_codes = NULL,
                                     tree_file_pattern = "arbre*",
                                     plot_file_pattern = "plot*",
                                     team_leader = NULL,
                                     principal_investigator = NULL,
                                     data_manager = NULL,
                                     additional_people = NULL,
                                     census = NULL,
                                     specimen_prefix = NULL,
                                     specimen_remap_file = NULL,
                                     specimen_locality = NULL,
                                     specimen_country = NULL,
                                     specimen_col_month = NULL,
                                     specimen_col_year = NULL,
                                     specimen_collector = NULL,
                                     specimen_description_col = "stem_diameter",
                                     specimen_branch_position_col = "branch_position",
                                     recruit_state = "recruted",
                                     plot_name_col = "plot_plot_name",
                                     tag_col = "arbre",
                                     recruit_tag_col = "label_recrut",
                                     herbarium_col = "herbarium_nbe_char") {

  # ---- Auto-detect files from directories ----
  if (is.null(tree_file) && !is.null(data_dir)) {
    tree_file <- .find_file_in_dir(data_dir, tree_file_pattern, ext = "xlsx",
                                   label = "tree data")
  }
  if (is.null(plot_file) && !is.null(data_dir)) {
    found <- .find_file_in_dir(data_dir, plot_file_pattern, ext = "xlsx",
                               label = "plot data", required = FALSE)
    plot_file <- if (!is.null(found)) found else NULL
  }
  if (!is.null(codes_dir)) {
    if (is.null(observation_codes))
      observation_codes <- .find_file_in_dir(
        codes_dir, "code_list_observation*", ext = "csv",
        label = "observation codes", required = FALSE)
    if (is.null(pom_codes))
      pom_codes <- .find_file_in_dir(
        codes_dir, "code_list_pom*", ext = "csv",
        label = "POM codes", required = FALSE)
    if (is.null(light_codes))
      light_codes <- .find_file_in_dir(
        codes_dir, "code_light*", ext = "csv",
        label = "light codes", required = FALSE)
    if (is.null(status_codes))
      status_codes <- .find_file_in_dir(
        codes_dir, "code*status*", ext = "csv",
        label = "status codes", required = FALSE)
  }

  # Handle FALSE = explicit skip
  if (identical(plot_file, FALSE)) plot_file <- NULL
  if (identical(observation_codes, FALSE)) observation_codes <- NULL
  if (identical(pom_codes, FALSE)) pom_codes <- NULL
  if (identical(light_codes, FALSE)) light_codes <- NULL
  if (identical(status_codes, FALSE)) status_codes <- NULL

  if (is.null(tree_file)) {
    stop("tree_file is required. Provide it directly or set data_dir to auto-detect.")
  }

  # ---- Read raw data ----
  cli::cli_alert_info("Reading tree data from {.file {tree_file}}")
  trees <- as.data.frame(readxl::read_excel(tree_file, guess_max = 5000))
  cli::cli_alert_success("Read {nrow(trees)} tree records")

  # Standardise plot_name and tag columns
  if (!plot_name_col %in% names(trees)) {
    stop(sprintf("Column '%s' not found in tree file. Available: %s",
                 plot_name_col, paste(names(trees), collapse = ", ")))
  }
  # Rename known OpenForis columns to standard trait names
  if ("dbh" %in% names(trees) && !"stem_diameter" %in% names(trees)) {
    names(trees)[names(trees) == "dbh"] <- "stem_diameter"
  }

  # Rename herbarium column to standard name
  if (herbarium_col != "herbarium_nbe_char") {
    if (!herbarium_col %in% names(trees)) {
      stop(sprintf(
        "Herbarium column '%s' not found in tree file. Available: %s",
        herbarium_col, paste(names(trees), collapse = ", ")
      ))
    }
    names(trees)[names(trees) == herbarium_col] <- "herbarium_nbe_char"
  } else if (!"herbarium_nbe_char" %in% names(trees)) {
    warning(
      "Column 'herbarium_nbe_char' not found in tree file. ",
      "Set the 'herbarium_col' parameter to the correct column name. ",
      "Specimen list will be empty."
    )
  }

  trees$plot_name <- trees[[plot_name_col]]
  trees$tag_existing <- if (tag_col %in% names(trees)) trees[[tag_col]] else NA
  trees$tag_recruit <- if (recruit_tag_col %in% names(trees)) trees[[recruit_tag_col]] else NA

  # Resolve tag: use recruit tag for recruits, existing tag otherwise
  trees$tag <- ifelse(
    !is.na(trees$tag_recruit) & trees$state == recruit_state,
    trees$tag_recruit,
    trees$tag_existing
  )
  trees$tag <- as.numeric(trees$tag)

  # ---- Check for duplicated plot_name + tag combinations ----
  dup_key <- paste(trees$plot_name, trees$tag, sep = "__")
  dup_mask <- duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)
  duplicated_stems <- NULL
  if (any(dup_mask)) {
    dup_cols <- intersect(
      c("plot_name", "tag", "state", "species_scientific_name", "species_code",
        "stem_diameter", "quadrat"),
      names(trees)
    )
    duplicated_stems <- trees[dup_mask, dup_cols, drop = FALSE]
    duplicated_stems <- duplicated_stems[order(duplicated_stems$plot_name,
                                               duplicated_stems$tag), ]
    rownames(duplicated_stems) <- NULL
    n_dup_pairs <- length(unique(dup_key[dup_mask]))
    warning(sprintf(
      "%d duplicated plot_name + tag combination(s) found (%d rows). Check result$duplicated_stems.",
      n_dup_pairs, sum(dup_mask)
    ), call. = FALSE)
    cli::cli_alert_warning(
      "{n_dup_pairs} duplicated plot_name + tag combination(s) detected ({sum(dup_mask)} rows) — review result$duplicated_stems"
    )
  }

  # ---- Specimen number remapping ----
  if (!is.null(specimen_remap_file)) {
    # Auto-detect from data_dir if just a filename (no path separator)
    if (!grepl("[/\\\\]", specimen_remap_file) && !is.null(data_dir)) {
      specimen_remap_file <- file.path(data_dir, specimen_remap_file)
    }
    if (!file.exists(specimen_remap_file)) {
      stop(sprintf("Specimen remap file not found: %s", specimen_remap_file))
    }

    cli::cli_alert_info("Reading specimen remap table from {.file {specimen_remap_file}}")
    remap <- as.data.frame(readxl::read_excel(specimen_remap_file))

    if (ncol(remap) < 2) {
      stop("Specimen remap file must have at least 2 columns (old number, new number)")
    }
    # Use first two columns regardless of names
    remap_old <- remap[[1]]
    remap_new <- remap[[2]]

    # Check for duplicates in the new numbers
    dup_new <- remap_new[duplicated(remap_new) & !is.na(remap_new)]
    if (length(dup_new) > 0) {
      stop(sprintf(
        "Duplicate new specimen numbers in remap file: %s",
        paste(unique(dup_new), collapse = ", ")
      ))
    }

    # Apply remapping to specimen_number column
    if ("specimen_number" %in% names(trees)) {
      trees$specimen_number_original <- trees$specimen_number
      match_idx <- match(trees$specimen_number, remap_old)
      remapped <- !is.na(match_idx)
      trees$specimen_number[remapped] <- remap_new[match_idx[remapped]]
      n_remapped <- sum(remapped)
      cli::cli_alert_success("Remapped {n_remapped} specimen_number(s) ({sum(!remapped & !is.na(trees$specimen_number))} unchanged)")
    } else {
      cli::cli_alert_warning("No 'specimen_number' column found — remap skipped")
    }

    # Apply same remapping to herbarium_nbe_char column
    if ("herbarium_nbe_char" %in% names(trees)) {
      trees$herbarium_nbe_char_original <- trees$herbarium_nbe_char
      match_idx_h <- match(trees$herbarium_nbe_char, remap_old)
      remapped_h <- !is.na(match_idx_h)
      trees$herbarium_nbe_char[remapped_h] <- remap_new[match_idx_h[remapped_h]]
      n_remapped_h <- sum(remapped_h)
      cli::cli_alert_success("Remapped {n_remapped_h} herbarium_nbe_char(s)")
    }
  }

  # ---- Census metadata ----
  census_metadata <- NULL
  if (!is.null(plot_file)) {
    cli::cli_alert_info("Reading plot metadata from {.file {plot_file}}")
    plots <- as.data.frame(readxl::read_excel(plot_file))

    census_metadata <- plots[, intersect(
      c("plot_name", "date_year", "date_month", "date_day"),
      names(plots)
    ), drop = FALSE]

    if (!is.null(census)) census_metadata$census <- census
    if (!is.null(team_leader)) census_metadata$team_leader <- team_leader
    if (!is.null(principal_investigator)) census_metadata$principal_investigator <- principal_investigator
    if (!is.null(data_manager)) census_metadata$data_manager <- data_manager
    if (!is.null(additional_people)) census_metadata$additional_people <- additional_people

    cli::cli_alert_success("Census metadata for {nrow(census_metadata)} plot(s)")
  }

  # ---- Split recruits vs existing ----
  is_recruit <- if ("state" %in% names(trees)) {
    !is.na(trees$state) & trees$state == recruit_state
  } else {
    rep(FALSE, nrow(trees))
  }

  recruits_raw <- trees[is_recruit, , drop = FALSE]
  existing_raw <- trees[!is_recruit, , drop = FALSE]

  cli::cli_alert_info("{nrow(recruits_raw)} recruit(s), {nrow(existing_raw)} existing individual(s)")

  # ---- Read code lists early (needed for both recruits and measurements) ----
  obs_codes <- if (!is.null(observation_codes)) {
    utils::read.csv(observation_codes, stringsAsFactors = FALSE)
  }
  pom_code_list <- if (!is.null(pom_codes)) {
    utils::read.csv(pom_codes, stringsAsFactors = FALSE)
  }
  light_code_list <- if (!is.null(light_codes)) {
    utils::read.csv(light_codes, stringsAsFactors = FALSE)
  }
  status_code_list <- if (!is.null(status_codes)) {
    utils::read.csv(status_codes, stringsAsFactors = FALSE)
  }

  # ---- Process recruits (with wide-format measurements) ----
  recruits <- NULL
  if (nrow(recruits_raw) > 0) {
    recruits <- .prepare_openforis_recruits(
      recruits_raw, specimen_prefix,
      observation_codes = obs_codes,
      pom_codes = pom_code_list,
      light_codes = light_code_list,
      status_codes = status_code_list
    )
    cli::cli_alert_success("Prepared {nrow(recruits)} recruit(s)")
  }

  # ---- Process all stems (same wide format as recruits, no recruit filter) ----
  all_stems <- .prepare_openforis_recruits(
    trees, specimen_prefix,
    observation_codes = obs_codes,
    pom_codes = pom_code_list,
    light_codes = light_code_list,
    status_codes = status_code_list
  )
  cli::cli_alert_success("Prepared {nrow(all_stems)} stem(s) in all_stems")

  # ---- Process measurements for existing individuals ----
  # Also include recruits for measurements (they get diameter etc. too)
  all_for_meas <- trees
  # Tag is already resolved above

  measurement_parts <- list()

  # Numeric traits (stem_diameter, height_of_stem_diameter)
  numeric_cols <- intersect(c("stem_diameter", "height_of_stem_diameter"), names(all_for_meas))
  if (length(numeric_cols) > 0) {
    cli::cli_alert_info("Extracting numeric measurements: {.val {numeric_cols}}")
    measurement_parts$numeric <- .extract_numeric_traits(
      all_for_meas, numeric_cols
    )
  }

  # Observations (coded → decoded → flags)
  if (!is.null(obs_codes)) {
    cli::cli_alert_info("Decoding observation codes")
    measurement_parts$observations <- .decode_openforis_observations(
      all_for_meas, obs_codes
    )
  }

  # POM observations
  if (!is.null(pom_code_list)) {
    cli::cli_alert_info("Decoding POM observation codes")
    measurement_parts$pom <- .decode_openforis_pom(
      all_for_meas, pom_code_list
    )
  }

  # Light
  if (!is.null(light_code_list)) {
    cli::cli_alert_info("Decoding light codes")
    measurement_parts$light <- .decode_openforis_light(
      all_for_meas, light_code_list
    )
  }

  # Stem status
  if (!is.null(status_code_list)) {
    cli::cli_alert_info("Decoding stem status codes")
    measurement_parts$status <- .decode_openforis_status(
      all_for_meas, status_code_list
    )
  }

  # ---- Add "recruit" observation for every recruit stem ----
  if (nrow(recruits_raw) > 0) {
    recruit_tags <- unique(recruits_raw[, c("plot_name", "tag"), drop = FALSE])
    measurement_parts$recruit_obs <- data.frame(
      plot_name = recruit_tags$plot_name,
      tag = recruit_tags$tag,
      trait_name = "observation",
      traitvalue = NA_real_,
      traitvalue_char = "recruit",
      stringsAsFactors = FALSE
    )
  }

  # ---- Add "observations" rows from free-text 'comment' column ----
  if ("comment" %in% names(all_for_meas)) {
    comment_rows <- all_for_meas[
      !is.na(all_for_meas$comment) & nzchar(trimws(all_for_meas$comment)),
      c("plot_name", "tag"),
      drop = FALSE
    ]
    if (nrow(comment_rows) > 0) {
      comment_rows$trait_name     <- "observation"
      comment_rows$traitvalue     <- NA_real_
      comment_rows$traitvalue_char <- trimws(
        all_for_meas$comment[
          !is.na(all_for_meas$comment) & nzchar(trimws(all_for_meas$comment))
        ]
      )
      measurement_parts$comments <- comment_rows
      cli::cli_alert_info(
        "Extracted {nrow(comment_rows)} 'observation' row(s) from 'comment' column"
      )
    }
  }

  # ---- Build "observations" rows from stem_status == 2 + stem_status2 ----
  if ("stem_status" %in% names(all_for_meas) && "stem_status2" %in% names(all_for_meas)) {
    mask_ss2 <- !is.na(all_for_meas$stem_status) &
                as.character(all_for_meas$stem_status) == "2" &
                !is.na(all_for_meas$stem_status2) &
                nzchar(trimws(as.character(all_for_meas$stem_status2)))

    if (any(mask_ss2)) {
      ss2_rows <- all_for_meas[mask_ss2, , drop = FALSE]
      ss2_code <- trimws(as.character(ss2_rows$stem_status2))

      obs_str <- vapply(seq_len(nrow(ss2_rows)), function(i) {
        code <- ss2_code[i]
        if (code == "2") {
          # Broken main stem — try remaining_main_axis
          val <- if ("remaining_main_axis" %in% names(ss2_rows)) {
            v <- ss2_rows$remaining_main_axis[i]
            if (!is.na(v) && nzchar(trimws(as.character(v)))) trimws(as.character(v)) else NA_character_
          } else NA_character_
          if (!is.na(val)) {
            paste0("broken main stem, ", val, " m remaining axis")
          } else {
            "broken main stem"
          }
        } else if (code == "1") {
          # Part of crown lost — try crown_left
          val <- if ("crown_left" %in% names(ss2_rows)) {
            v <- ss2_rows$crown_left[i]
            if (!is.na(v) && nzchar(trimws(as.character(v)))) trimws(as.character(v)) else NA_character_
          } else NA_character_
          if (!is.na(val)) {
            paste0("part of crown lost, ", val, "% crown remaining")
          } else {
            "part of crown lost"
          }
        } else if (code == "3") {
          "uprooted tree"
        } else {
          NA_character_
        }
      }, character(1))

      valid <- !is.na(obs_str)
      if (any(valid)) {
        stem_status_obs <- data.frame(
          plot_name    = ss2_rows$plot_name[valid],
          tag          = ss2_rows$tag[valid],
          trait_name   = "observation",
          traitvalue   = NA_real_,
          traitvalue_char = obs_str[valid],
          stringsAsFactors = FALSE
        )
        measurement_parts$stem_status_obs <- stem_status_obs
        cli::cli_alert_info(
          "Built {nrow(stem_status_obs)} 'observation' row(s) from stem_status2 codes"
        )
      }
    }
  }

  # ---- Combine all measurement parts ----
  measurements <- do.call(rbind, measurement_parts)
  if (!is.null(measurements)) {
    rownames(measurements) <- NULL
    # Remove rows where both values are NA
    measurements <- measurements[
      !is.na(measurements$traitvalue) | !is.na(measurements$traitvalue_char),
      , drop = FALSE
    ]
    cli::cli_alert_success(
      "Total: {nrow(measurements)} measurement rows across {length(unique(measurements$trait_name))} trait(s)"
    )
  } else {
    measurements <- dplyr::tibble(
      plot_name = character(0), tag = numeric(0),
      trait_name = character(0), traitvalue = numeric(0),
      traitvalue_char = character(0)
    )
    cli::cli_alert_warning("No measurements extracted")
  }

  # ---- Multi-stem grouping ----
  multi_stems <- .build_openforis_multi_stems(trees)
  if (!is.null(multi_stems)) {
    n_groups <- length(unique(paste(multi_stems$plot_name, multi_stems$group_tag)))
    n_flagged <- sum(!is.na(multi_stems$flag))
    cli::cli_alert_success(
      "Detected {n_groups} multi-stem group(s) ({nrow(multi_stems)} stems total)"
    )
    if (n_flagged > 0) {
      cli::cli_alert_warning("{n_flagged} stem(s) flagged — review recommended")
    }
  }

  # ---- Specimen list ----
  specimens <- .prepare_openforis_specimens(
    trees, census_metadata, specimen_prefix,
    locality = specimen_locality,
    country = specimen_country,
    col_month = specimen_col_month,
    col_year = specimen_col_year,
    collector = specimen_collector,
    additional_people = additional_people,
    description_col = specimen_description_col,
    branch_position_col = specimen_branch_position_col
  )
  if (!is.null(specimens)) {
    cli::cli_alert_success("Prepared {nrow(specimens)} specimen(s)")
  }

  # Convert all outputs to tibble
  if (!is.null(measurements) && nrow(measurements) > 0) {
    measurements <- dplyr::as_tibble(measurements)
  }
  if (!is.null(census_metadata)) {
    census_metadata <- dplyr::as_tibble(census_metadata)
  }
  if (!is.null(recruits)) {
    recruits <- dplyr::as_tibble(recruits)
  }
  if (!is.null(specimens)) {
    specimens <- dplyr::as_tibble(specimens)
  }
  if (!is.null(multi_stems)) {
    multi_stems <- dplyr::as_tibble(multi_stems)
  }
  if (!is.null(all_stems)) {
    all_stems <- dplyr::as_tibble(all_stems)
  }
  if (!is.null(duplicated_stems)) {
    duplicated_stems <- dplyr::as_tibble(duplicated_stems)
  }

  # ---- Summary ----
  summary_info <- list(
    n_plots = length(unique(trees$plot_name)),
    n_recruits = nrow(recruits_raw),
    n_existing = nrow(existing_raw),
    n_measurements = nrow(measurements),
    n_specimens = if (!is.null(specimens)) nrow(specimens) else 0L,
    n_multi_stem_groups = if (!is.null(multi_stems)) {
      length(unique(paste(multi_stems$plot_name, multi_stems$group_tag)))
    } else 0L,
    n_all_stems = if (!is.null(all_stems)) nrow(all_stems) else 0L,
    n_duplicated_stems = if (!is.null(duplicated_stems)) nrow(duplicated_stems) else 0L,
    trait_names = if (nrow(measurements) > 0) unique(measurements$trait_name) else character(0)
  )

  cli::cli_rule()
  cli::cli_alert_success("Processing complete:")
  cli::cli_bullets(c(
    "*" = "{summary_info$n_plots} plot(s)",
    "*" = "{summary_info$n_recruits} recruit(s)",
    "*" = "{summary_info$n_existing} existing individual(s)",
    "*" = "{summary_info$n_measurements} measurement rows",
    "*" = "{summary_info$n_specimens} specimen(s)",
    "*" = "{summary_info$n_multi_stem_groups} multi-stem group(s)",
    "*" = "{summary_info$n_all_stems} stem(s) in all_stems",
    "*" = "{summary_info$n_duplicated_stems} duplicated stem row(s)",
    "*" = "Traits: {paste(summary_info$trait_names, collapse = ', ')}"
  ))

  list(
    census_metadata = census_metadata,
    recruits = recruits,
    measurements = measurements,
    specimens = specimens,
    multi_stems = multi_stems,
    all_stems = all_stems,
    duplicated_stems = duplicated_stems,
    summary = summary_info
  )
}


# ===========================================================================
# Internal helpers
# ===========================================================================

#' Prepare recruit data from OpenForis export
#'
#' Builds a wide-format tibble with identity columns plus decoded
#' measurement/observation columns for each recruit.
#'
#' @param data Data frame of recruit rows (already filtered).
#' @param specimen_prefix Prefix for specimen numbers. NULL to skip.
#' @param observation_codes Parsed observation code list data frame, or NULL.
#' @param pom_codes Parsed POM code list data frame, or NULL.
#' @param light_codes Parsed light code list data frame, or NULL.
#' @param status_codes Parsed status code list data frame, or NULL.
#' @keywords internal
.prepare_openforis_recruits <- function(data, specimen_prefix = NULL,
                                        observation_codes = NULL,
                                        pom_codes = NULL,
                                        light_codes = NULL,
                                        status_codes = NULL) {

  result <- data.frame(
    plot_name = data$plot_name,
    tag = data$tag,
    stringsAsFactors = FALSE
  )

  # Taxonomy
  if ("species_scientific_name" %in% names(data))
    result$original_tax_name <- data$species_scientific_name
  if ("species_code" %in% names(data))
    result$idtax <- as.numeric(data$species_code)

  # Spatial
  if ("quadrat" %in% names(data)) result$quadrat <- data$quadrat
  if ("position_x" %in% names(data)) result$position_x <- data$position_x
  if ("position_y" %in% names(data)) result$position_y <- data$position_y

  # Specimens
  if ("specimen_number" %in% names(data)) {
    result$specimen_number <- if (!is.null(specimen_prefix)) {
      ifelse(!is.na(data$specimen_number),
             paste(specimen_prefix, as.character(data$specimen_number)),
             NA_character_)
    } else {
      as.character(data$specimen_number)
    }
  }

  if ("herbarium_nbe_char" %in% names(data)) {
    result$herbarium_nbe_char <- if (!is.null(specimen_prefix)) {
      ifelse(!is.na(data$herbarium_nbe_char),
             paste(specimen_prefix, as.character(data$herbarium_nbe_char)),
             NA_character_)
    } else {
      as.character(data$herbarium_nbe_char)
    }
  }

  # Multi-stem info
  if ("multi_stem" %in% names(data)) result$multi_stem <- data$multi_stem
  if ("number_multi_stem" %in% names(data)) result$number_multi_stem <- data$number_multi_stem

  # ---- Wide-format measurements/observations ----

  # Numeric traits: stem_diameter, height_of_stem_diameter
  if ("stem_diameter" %in% names(data))
    result$stem_diameter <- suppressWarnings(as.numeric(data$stem_diameter))
  if ("height_of_stem_diameter" %in% names(data))
    result$height_of_stem_diameter <- suppressWarnings(as.numeric(data$height_of_stem_diameter))

  # Observations (decoded text + flag)
  if (!is.null(observation_codes)) {
    obs_long <- .decode_openforis_observations(data, observation_codes)
    if (!is.null(obs_long) && nrow(obs_long) > 0) {
      # Aggregate per individual: concatenate multiple observations
      obs_text <- stats::aggregate(
        traitvalue_char ~ plot_name + tag,
        data = obs_long[obs_long$trait_name == "observation", , drop = FALSE],
        FUN = function(x) paste(unique(x), collapse = "; ")
      )
      idx <- match(
        paste(result$plot_name, result$tag),
        paste(obs_text$plot_name, obs_text$tag)
      )
      result$observation <- obs_text$traitvalue_char[idx]

      # Flags
      flag_sub <- obs_long[obs_long$trait_name == "observation_flag", , drop = FALSE]
      if (nrow(flag_sub) > 0) {
        flag_agg <- stats::aggregate(
          traitvalue_char ~ plot_name + tag,
          data = flag_sub,
          FUN = function(x) paste(unique(x), collapse = "")
        )
        idx_f <- match(
          paste(result$plot_name, result$tag),
          paste(flag_agg$plot_name, flag_agg$tag)
        )
        result$observation_flag <- flag_agg$traitvalue_char[idx_f]
      }
    }
  }

  # POM observations (decoded text)
  if (!is.null(pom_codes)) {
    pom_long <- .decode_openforis_pom(data, pom_codes)
    if (!is.null(pom_long) && nrow(pom_long) > 0) {
      pom_agg <- stats::aggregate(
        traitvalue_char ~ plot_name + tag,
        data = pom_long,
        FUN = function(x) paste(unique(x), collapse = "; ")
      )
      idx_p <- match(
        paste(result$plot_name, result$tag),
        paste(pom_agg$plot_name, pom_agg$tag)
      )
      result$pom_observation <- pom_agg$traitvalue_char[idx_p]
    }
  }

  # Light (decoded text)
  if (!is.null(light_codes)) {
    light_long <- .decode_openforis_light(data, light_codes)
    if (!is.null(light_long) && nrow(light_long) > 0) {
      idx_l <- match(
        paste(result$plot_name, result$tag),
        paste(light_long$plot_name, light_long$tag)
      )
      result$light <- light_long$traitvalue_char[idx_l]
    }
  }

  # Stem status (decoded text + alive/dead flags)
  if (!is.null(status_codes)) {
    status_long <- .decode_openforis_status(data, status_codes)
    if (!is.null(status_long) && nrow(status_long) > 0) {
      for (trait in c("stem_status", "stem_alive_flag", "stem_dead_flag")) {
        sub <- status_long[status_long$trait_name == trait, , drop = FALSE]
        if (nrow(sub) > 0) {
          idx_s <- match(
            paste(result$plot_name, result$tag),
            paste(sub$plot_name, sub$tag)
          )
          result[[trait]] <- sub$traitvalue_char[idx_s]
        }
      }
    }
  }

  result
}


#' Prepare specimen list from OpenForis tree data
#'
#' Filters trees that have a \code{herbarium_nbe_char} value, applies the
#' specimen prefix, extracts a numeric collection number (\code{colnbr}),
#' and attaches plot coordinates and user-supplied metadata. The output
#' is ready for \code{add_specimens()}. With \code{deduplicate = TRUE} the
#' result holds one row per unique voucher rather than one row per individual.
#'
#' @param trees Full tree data frame (all individuals, already tag-resolved).
#' @param census_metadata Census metadata tibble (used to get plot coordinates
#'   if a \code{ddlat}/\code{ddlon} column is present). Can be NULL.
#' @param specimen_prefix Character prefix (e.g. "PIRD"). NULL to skip.
#' @param locality Character. Locality string (e.g. "Mbalmayo, Centre").
#' @param country Character. Country name.
#' @param col_month Integer. Collection month.
#' @param col_year Integer. Collection year.
#' @param collector Character. Collector code (stored as \code{colnam}).
#' @param additional_people Character. Additional collectors.
#' @param additional_collector Character scalar applied to every specimen, or a
#'   data frame with columns \code{plot_name} and \code{additional_collector}
#'   giving the collecting team per plot. Written to an
#'   \code{additional_collector} column.
#' @param deduplicate Logical. If TRUE, keep a single row per unique specimen
#'   instead of one row per individual carrying it. Among duplicates the row
#'   with both a \code{specimen_number} and a \code{colnbr} wins, ties going to
#'   the first individual bearing the voucher; the full individual-to-specimen
#'   link stays available in \code{trees}.
#' @param description_col Column name in \code{trees} used to build description
#'   (default "stem_diameter"). Set to NULL to skip.
#' @param branch_position_col Column name in \code{trees} giving the origin of
#'   the collected branch (default "branch_position"). Only the value
#'   \code{"rejet"} is added to the description; \code{"shade_branch"} and
#'   \code{"light_branch"} are ignored.
#' @return Data frame ready for \code{add_specimens()}, or NULL if no specimens.
#' @keywords internal
.prepare_openforis_specimens <- function(trees, census_metadata = NULL,
                                         specimen_prefix = NULL,
                                         locality = NULL, country = NULL,
                                         col_month = NULL, col_year = NULL,
                                         collector = NULL,
                                         additional_people = NULL,
                                         additional_collector = NULL,
                                         deduplicate = FALSE,
                                         description_col = "stem_diameter",
                                         branch_position_col = "branch_position") {

  if (!"herbarium_nbe_char" %in% names(trees)) return(NULL)

  # Filter to trees with a specimen
  has_spec <- !is.na(trees$herbarium_nbe_char)
  if (!any(has_spec)) return(NULL)

  spec <- trees[has_spec, , drop = FALSE]

  result <- data.frame(
    plot_name = spec$plot_name,
    tag = spec$tag,
    stringsAsFactors = FALSE
  )

  # Specimen number and herbarium code (with prefix)
  if ("specimen_number" %in% names(spec)) {
    result$specimen_number <- if (!is.null(specimen_prefix)) {
      ifelse(!is.na(spec$specimen_number),
             paste(specimen_prefix, as.character(spec$specimen_number)),
             NA_character_)
    } else {
      as.character(spec$specimen_number)
    }
  }

  result$herbarium_nbe_char <- if (!is.null(specimen_prefix)) {
    paste(specimen_prefix, as.character(spec$herbarium_nbe_char))
  } else {
    as.character(spec$herbarium_nbe_char)
  }

  # Extract numeric collection number from specimen_number
  if ("specimen_number" %in% names(result)) {
    result$colnbr <- suppressWarnings(
      as.numeric(gsub("[^0-9]", "", result$specimen_number))
    )
  }

  # Taxonomy
  if ("species_scientific_name" %in% names(spec))
    result$original_tax_name <- spec$species_scientific_name
  if ("species_code" %in% names(spec))
    result$idtax_n <- suppressWarnings(as.numeric(spec$species_code))

  # Build description: merge field description and measurement description
  desc_field <- if ("specimes_descriptipn" %in% names(spec)) {
    as.character(spec$specimes_descriptipn)
  } else {
    rep(NA_character_, nrow(spec))
  }

  desc_meas <- if (!is.null(description_col) && description_col %in% names(spec)) {
    vals <- suppressWarnings(as.numeric(spec[[description_col]]))
    ifelse(!is.na(vals),
           paste("Arbre de diamètre", vals, "cm mesurée à hauteur de poitrine"),
           NA_character_)
  } else {
    rep(NA_character_, nrow(spec))
  }

  # Origin of the collected branch: only 'rejet' is informative
  desc_branch <- if (branch_position_col %in% names(spec)) {
    is_rejet <- tolower(trimws(as.character(spec[[branch_position_col]]))) == "rejet"
    is_rejet[is.na(is_rejet)] <- FALSE
    ifelse(is_rejet, "Echantillon collecté sur un rejet", NA_character_)
  } else {
    rep(NA_character_, nrow(spec))
  }

  desc_parts <- list(desc_field, desc_meas, desc_branch)
  result$description <- vapply(seq_len(nrow(spec)), function(i) {
    parts <- vapply(desc_parts, function(x) x[i], character(1))
    parts <- parts[!is.na(parts) & nzchar(trimws(parts))]
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = ". ")
  }, character(1))

  # Plot coordinates from census_metadata or from trees
  if (!is.null(census_metadata) && all(c("ddlat", "ddlon") %in% names(census_metadata))) {
    coord_idx <- match(result$plot_name, census_metadata$plot_name)
    result$ddlat <- census_metadata$ddlat[coord_idx]
    result$ddlon <- census_metadata$ddlon[coord_idx]
  }

  # User-supplied metadata
  if (!is.null(locality)) result$locality <- locality
  if (!is.null(country)) result$country <- country
  if (!is.null(collector)) result$colnam <- collector
  if (!is.null(col_month)) result$colm <- as.integer(col_month)
  if (!is.null(col_year)) result$coly <- as.integer(col_year)
  if (!is.null(additional_people)) result$additional_people <- additional_people

  # Collecting team, either a single string or one value per plot
  if (!is.null(additional_collector)) {
    result$additional_collector <- if (is.data.frame(additional_collector)) {
      additional_collector$additional_collector[
        match(result$plot_name, additional_collector$plot_name)
      ]
    } else {
      as.character(additional_collector)
    }
  }

  # One row per specimen: several individuals can carry the same voucher
  if (deduplicate) {
    key <- result$herbarium_nbe_char
    if (all(is.na(key)) && "specimen_number" %in% names(result))
      key <- result$specimen_number
    key <- trimws(as.character(key))
    blank <- is.na(key) | !nzchar(key)
    if (any(blank)) key[blank] <- paste0("__unkeyed__", which(blank))

    # Among duplicates, keep the row carrying both collection numbers
    numbered <- rep(0L, nrow(result))
    for (cl in c("specimen_number", "colnbr")) {
      if (cl %in% names(result)) numbered <- numbered + !is.na(result[[cl]])
    }
    ord <- order(-numbered, seq_len(nrow(result)))
    keep <- sort(ord[!duplicated(key[ord])])

    n_dropped <- nrow(result) - length(keep)
    if (n_dropped > 0) {
      cli::cli_alert_warning(paste(
        "{n_dropped} individual{?s} carr{?ies/y} a voucher already listed —",
        "keeping one row per specimen ({length(keep)} unique)"
      ))
      result <- result[keep, , drop = FALSE]
      rownames(result) <- NULL
    }
  }

  # Sort by collection number
  if ("colnbr" %in% names(result)) {
    result <- result[order(result$colnbr, na.last = TRUE), , drop = FALSE]
    rownames(result) <- NULL
  }

  result
}


#' Build candidate multi-stem groupings from OpenForis data
#'
#' In OpenForis, multi-stem individuals are indicated by
#' \code{multi_stem == "yes"} on the first stem row, with
#' \code{number_multi_stem} giving the total number of stems. The
#' subsequent N-1 rows (sorted by tag within the same plot) are assumed
#' to be the additional stems of that individual.
#'
#' This function reconstructs those groups and flags potential problems:
#' \itemize{
#'   \item \code{different_idtax}: stems in the group have different taxonomy
#'   \item \code{tag_gap}: tags are not consecutive
#'   \item \code{overlapping_group}: a stem belongs to more than one group
#' }
#'
#' @param trees Full tree data frame with columns: plot_name, tag,
#'   multi_stem, number_multi_stem, and optionally species_scientific_name
#'   and species_code.
#' @return Data frame with columns: plot_name, tag, group_tag, stem_order,
#'   original_tax_name, idtax, flag. NULL if no multi-stem detected.
#' @keywords internal
.build_openforis_multi_stems <- function(trees) {

  if (!"multi_stem" %in% names(trees)) return(NULL)

  # Sort by plot_name then tag (critical for consecutive-tag logic)
  trees <- trees[order(trees$plot_name, trees$tag), , drop = FALSE]
  trees$.row_idx <- seq_len(nrow(trees))

  # Find group leaders
  is_leader <- !is.na(trees$multi_stem) & tolower(trees$multi_stem) == "yes"
  if (!any(is_leader)) return(NULL)

  leaders <- trees[is_leader, , drop = FALSE]

  # Get number of stems per group
  n_stems <- if ("number_multi_stem" %in% names(leaders)) {
    suppressWarnings(as.integer(leaders$number_multi_stem))
  } else {
    rep(NA_integer_, nrow(leaders))
  }
  # Default to 2 if missing

  n_stems[is.na(n_stems) | n_stems < 2] <- 2L

  # Track which rows have been assigned to a group (for overlap detection)
  assigned <- rep(FALSE, nrow(trees))

  groups <- list()

  for (i in seq_len(nrow(leaders))) {
    leader <- leaders[i, , drop = FALSE]
    plot <- leader$plot_name
    leader_row <- leader$.row_idx
    n <- n_stems[i]

    # Find candidate stems: next n-1 rows in the same plot after the leader
    plot_rows <- which(trees$plot_name == plot & trees$.row_idx >= leader_row)
    if (length(plot_rows) < n) {
      # Not enough rows — take what we have
      candidate_rows <- plot_rows
    } else {
      candidate_rows <- plot_rows[1:n]
    }

    group_data <- trees[candidate_rows, , drop = FALSE]
    group_tag <- leader$tag

    # Build flags
    flags <- rep(NA_character_, nrow(group_data))

    # Flag: tags not consecutive
    expected_tags <- seq(from = group_tag, length.out = nrow(group_data))
    if (!all(group_data$tag == expected_tags)) {
      non_consec <- group_data$tag != expected_tags
      flags[non_consec] <- "tag_gap"
    }

    # Flag: different taxonomy within group
    if ("species_code" %in% names(group_data)) {
      idtax_vals <- suppressWarnings(as.numeric(group_data$species_code))
      unique_idtax <- unique(idtax_vals[!is.na(idtax_vals)])
      if (length(unique_idtax) > 1) {
        flags <- ifelse(
          is.na(flags),
          "different_idtax",
          paste(flags, "different_idtax", sep = "; ")
        )
      }
    }

    # Flag: overlapping groups
    already_assigned <- assigned[candidate_rows]
    if (any(already_assigned)) {
      overlap_idx <- which(already_assigned)
      flags[overlap_idx] <- ifelse(
        is.na(flags[overlap_idx]),
        "overlapping_group",
        paste(flags[overlap_idx], "overlapping_group", sep = "; ")
      )
    }

    assigned[candidate_rows] <- TRUE

    # Taxonomy columns
    tax_name <- if ("species_scientific_name" %in% names(group_data)) {
      group_data$species_scientific_name
    } else {
      rep(NA_character_, nrow(group_data))
    }

    idtax <- if ("species_code" %in% names(group_data)) {
      suppressWarnings(as.numeric(group_data$species_code))
    } else {
      rep(NA_real_, nrow(group_data))
    }

    groups[[i]] <- data.frame(
      plot_name = group_data$plot_name,
      tag = group_data$tag,
      group_tag = group_tag,
      stem_order = seq_len(nrow(group_data)),
      original_tax_name = tax_name,
      idtax = idtax,
      flag = flags,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, groups)
  rownames(result) <- NULL
  result
}


#' Extract direct numeric measurements into long format
#' @keywords internal
.extract_numeric_traits <- function(data, numeric_cols) {
  rows <- list()

  for (col in numeric_cols) {
    vals <- suppressWarnings(as.numeric(data[[col]]))
    valid <- !is.na(vals)
    if (!any(valid)) next

    rows[[col]] <- data.frame(
      plot_name = data$plot_name[valid],
      tag = data$tag[valid],
      trait_name = rep(col, sum(valid)),
      traitvalue = vals[valid],
      traitvalue_char = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}


#' Decode OpenForis observation columns and derive flags
#'
#' Takes the wide observation_* columns, pivots to long, joins with the
#' code list, and maps decoded labels to single-letter flag codes.
#'
#' @keywords internal
.decode_openforis_observations <- function(data, code_list,
                                           flag_mapping = NULL) {
  obs_cols <- grep("^observation", names(data), value = TRUE)
  # Exclude pom_observation columns
  obs_cols <- obs_cols[!grepl("^pom_observation", obs_cols)]
  if (length(obs_cols) == 0) return(NULL)

  if (is.null(flag_mapping)) {
    flag_mapping <- .default_observation_flags()
  }

  # Pivot long
  long <- .pivot_coded_columns(data, obs_cols, code_list,
                               code_col = "item_code",
                               label_col = "item_label_en")
  if (is.null(long) || nrow(long) == 0) return(NULL)

  # Observation text rows
  obs_rows <- data.frame(
    plot_name = long$plot_name,
    tag = long$tag,
    trait_name = "observation",
    traitvalue = NA_real_,
    traitvalue_char = long$decoded_value,
    stringsAsFactors = FALSE
  )

  # Flag rows
  flag_values <- .map_flags(long$decoded_value, flag_mapping)
  has_flag <- !is.na(flag_values)

  flag_rows <- NULL
  if (any(has_flag)) {
    flag_rows <- data.frame(
      plot_name = long$plot_name[has_flag],
      tag = long$tag[has_flag],
      trait_name = "observation_flag",
      traitvalue = NA_real_,
      traitvalue_char = flag_values[has_flag],
      stringsAsFactors = FALSE
    )
  }

  rbind(obs_rows, flag_rows)
}


#' Decode OpenForis POM observation columns
#' @keywords internal
.decode_openforis_pom <- function(data, code_list) {
  pom_cols <- grep("^pom_observat", names(data), value = TRUE)
  if (length(pom_cols) == 0) return(NULL)

  long <- .pivot_coded_columns(data, pom_cols, code_list,
                               code_col = "item_code",
                               label_col = "item_label_en")
  if (is.null(long) || nrow(long) == 0) return(NULL)

  data.frame(
    plot_name = long$plot_name,
    tag = long$tag,
    trait_name = "pom_observation",
    traitvalue = NA_real_,
    traitvalue_char = long$decoded_value,
    stringsAsFactors = FALSE
  )
}


#' Decode OpenForis light column
#' @keywords internal
.decode_openforis_light <- function(data, code_list) {
  if (!"light" %in% names(data)) return(NULL)

  vals <- suppressWarnings(as.numeric(data$light))
  valid <- !is.na(vals)
  if (!any(valid)) return(NULL)

  # Join with code list
  lookup <- data.frame(
    code = as.numeric(code_list$light_code),
    label = tolower(code_list$light_label_en),
    stringsAsFactors = FALSE
  )

  decoded <- lookup$label[match(vals[valid], lookup$code)]
  has_label <- !is.na(decoded)

  if (!any(has_label)) return(NULL)

  data.frame(
    plot_name = data$plot_name[valid][has_label],
    tag = data$tag[valid][has_label],
    trait_name = "light",
    traitvalue = NA_real_,
    traitvalue_char = decoded[has_label],
    stringsAsFactors = FALSE
  )
}


#' Decode OpenForis stem status columns and derive flags
#'
#' Handles the two-level status system: stem_status + stem_status2 are
#' concatenated into a combined code, looked up, and mapped to flag1/flag2.
#'
#' @keywords internal
.decode_openforis_status <- function(data, code_list,
                                     flag_mapping = NULL) {
  if (!"stem_status" %in% names(data)) return(NULL)

  if (is.null(flag_mapping)) {
    flag_mapping <- .default_status_flags()
  }

  # Build combined code (same logic as the original script)
  s1 <- as.character(data$stem_status)
  s2 <- if ("stem_status2" %in% names(data)) as.character(data$stem_status2) else ""
  combined <- paste0(
    ifelse(is.na(s1) | s1 == "NA", "", s1),
    ifelse(is.na(s2) | s2 == "NA", "", s2)
  )
  combined_num <- suppressWarnings(as.numeric(combined))

  # Build lookup from code list
  cl <- code_list
  cl$status_code_conc <- paste0(
    ifelse(is.na(cl$status1_code), "", as.character(cl$status1_code)),
    ifelse(is.na(cl$status2_code), "", as.character(cl$status2_code))
  )
  cl$status_code_conc <- suppressWarnings(as.numeric(
    gsub("NA", "", cl$status_code_conc)
  ))

  # Match
  match_idx <- match(combined_num, cl$status_code_conc)
  matched_label <- cl$status1_label_en[match_idx]
  matched_label2 <- if ("status2_label_en" %in% names(cl)) cl$status2_label_en[match_idx] else NA

  # Derive flags
  flag1 <- .map_status_flags(matched_label, matched_label2, type = "alive", flag_mapping)
  flag2 <- .map_status_flags(matched_label, matched_label2, type = "dead", flag_mapping)

  rows <- list()

  # Status label
  has_label <- !is.na(matched_label)
  if (any(has_label)) {
    rows$status <- data.frame(
      plot_name = data$plot_name[has_label],
      tag = data$tag[has_label],
      trait_name = "stem_status",
      traitvalue = NA_real_,
      traitvalue_char = tolower(matched_label[has_label]),
      stringsAsFactors = FALSE
    )
  }

  # Flag1 (alive stem conditions)
  has_f1 <- !is.na(flag1)
  if (any(has_f1)) {
    rows$flag1 <- data.frame(
      plot_name = data$plot_name[has_f1],
      tag = data$tag[has_f1],
      trait_name = "stem_alive_flag",
      traitvalue = NA_real_,
      traitvalue_char = flag1[has_f1],
      stringsAsFactors = FALSE
    )
  }

  # Flag2 (dead stem conditions)
  has_f2 <- !is.na(flag2)
  if (any(has_f2)) {
    rows$flag2 <- data.frame(
      plot_name = data$plot_name[has_f2],
      tag = data$tag[has_f2],
      trait_name = "stem_dead_flag",
      traitvalue = NA_real_,
      traitvalue_char = flag2[has_f2],
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}


# ===========================================================================
# Low-level helpers
# ===========================================================================

#' Pivot coded columns to long format and decode via code list
#' @keywords internal
.pivot_coded_columns <- function(data, coded_cols, code_list,
                                 code_col = "item_code",
                                 label_col = "item_label_en") {
  # Build subset with plot_name, tag, and coded columns
  subset <- data[, c("plot_name", "tag", coded_cols), drop = FALSE]

  # Convert coded columns to numeric
  for (col in coded_cols) {
    subset[[col]] <- suppressWarnings(as.numeric(subset[[col]]))
  }

  # Pivot to long
  rows <- list()
  for (col in coded_cols) {
    valid <- !is.na(subset[[col]])
    if (!any(valid)) next
    rows[[col]] <- data.frame(
      plot_name = subset$plot_name[valid],
      tag = subset$tag[valid],
      code = subset[[col]][valid],
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) return(NULL)
  long <- do.call(rbind, rows)

  # Join with code list
  lookup_codes <- as.numeric(code_list[[code_col]])
  lookup_labels <- tolower(as.character(code_list[[label_col]]))

  long$decoded_value <- lookup_labels[match(long$code, lookup_codes)]

  # Drop rows where decoding failed
  long <- long[!is.na(long$decoded_value), , drop = FALSE]

  long
}


#' Map decoded observation text to single-letter flag codes
#' @keywords internal
.map_flags <- function(decoded_values, flag_mapping) {
  result <- rep(NA_character_, length(decoded_values))
  for (pattern in names(flag_mapping)) {
    matches <- grepl(pattern, decoded_values, ignore.case = TRUE)
    result[matches & is.na(result)] <- flag_mapping[[pattern]]
  }
  result
}


#' Map stem status labels to flag codes
#' @keywords internal
.map_status_flags <- function(label1, label2, type = "alive", flag_mapping) {
  n <- length(label1)
  result <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    l1 <- if (!is.na(label1[i])) tolower(label1[i]) else ""
    l2 <- if (!is.na(label2[i])) tolower(label2[i]) else ""
    key <- paste0(l1, "|", l2)

    if (type == "alive" && grepl("alive stem with", l1)) {
      if (grepl("standing", l2)) result[i] <- flag_mapping$alive_standing
      else if (grepl("broken", l2)) result[i] <- flag_mapping$alive_broken
      else if (grepl("uprooted", l2)) result[i] <- flag_mapping$alive_uprooted
    } else if (type == "dead") {
      if (grepl("dead stem", l1) && grepl("standing", l2)) result[i] <- flag_mapping$dead_standing
      else if (grepl("dead stem", l1) && grepl("broken", l2)) result[i] <- flag_mapping$dead_broken
      else if (grepl("dead stem", l1) && grepl("uprooted", l2)) result[i] <- flag_mapping$dead_uprooted
      else if (grepl("stem and tag not found", l1)) result[i] <- flag_mapping$not_found
    }
  }

  result
}


#' Default observation flag mapping
#' @keywords internal
.default_observation_flags <- function() {
  list(
    "dying"                              = "z",
    "leaning"                            = "c",
    "broken stem"                        = "b",
    "lying"                              = "d",
    "termites"                           = "y",
    "hollow"                             = "f",
    "large liana"                        = "l",
    "human"                              = "w",
    "small liana.*> 50"                  = "m",
    "stangler"                           = "s",
    "more than half defoliated"          = "i"
  )
}


#' Default stem status flag mapping
#' @keywords internal
.default_status_flags <- function() {
  list(
    alive_standing = "z",
    alive_broken   = "b",
    alive_uprooted = "b",
    dead_standing  = "a",
    dead_broken    = "b",
    dead_uprooted  = "i",
    not_found      = "k"
  )
}


#' Find a file in a directory by glob pattern and extension
#'
#' @param dir Directory path
#' @param pattern Glob-style pattern (e.g. "arbre*", "code_light*")
#' @param ext File extension without dot (e.g. "xlsx", "csv")
#' @param label Human-readable label for messages
#' @param required If TRUE, stop with an error when not found
#' @return File path, or NULL if not found and not required
#' @keywords internal
.find_file_in_dir <- function(dir, pattern, ext = NULL, label = "file",
                              required = TRUE) {
  if (!dir.exists(dir)) {
    if (required) stop(sprintf("Directory not found: %s", dir))
    return(NULL)
  }

  # Build search pattern
  search <- if (!is.null(ext)) {
    paste0(pattern, ".", ext)
  } else {
    pattern
  }

  hits <- Sys.glob(file.path(dir, search))

  # Also try case-insensitive by listing all files if no hits
  if (length(hits) == 0 && !is.null(ext)) {
    all_files <- list.files(dir, full.names = TRUE)
    ext_match <- grepl(paste0("\\.", ext, "$"), all_files, ignore.case = TRUE)
    # Simple pattern: convert glob * to regex .*
    regex_pat <- gsub("\\*", ".*", pattern)
    name_match <- grepl(regex_pat, basename(all_files), ignore.case = TRUE)
    hits <- all_files[ext_match & name_match]
  }

  if (length(hits) == 0) {
    if (required) {
      stop(sprintf("Could not find %s in %s (pattern: %s)", label, dir, search))
    }
    cli::cli_alert_warning("No {label} found in {.path {dir}} (pattern: {search}) — skipped")
    return(NULL)
  }

  if (length(hits) > 1) {
    cli::cli_alert_warning("Multiple {label} files found, using first: {.file {basename(hits[1])}}")
  }

  cli::cli_alert_success("Found {label}: {.file {basename(hits[1])}}")
  hits[1]
}
