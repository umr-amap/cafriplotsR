# OpenForis Collect — new (first-census) plot pre-processing
#
# Internal (non-exported) functions for converting the raw OpenForis Collect
# exports of a *newly established* plot (plot.xlsx + tree_list.xlsx) into
# clean data frames ready for the Import Wizard / Feature Wizard.
#
# This is the counterpart of process_openforis_census(), which handles the
# re-measurement (recensus) exports. The two OpenForis forms use different
# column names, so the raw columns are normalised to a common vocabulary here
# and the decoding helpers in openforis_processing.R are reused.
#
# Nothing in this file writes to the database.


#' Process an OpenForis new-plot export into clean data frames
#'
#' Reads the raw \code{plot.xlsx} and \code{tree_list.xlsx} files exported from
#' OpenForis Collect for a newly established plot, decodes the coded columns
#' using the code-list CSVs, and returns a list of clean tables ready for
#' import. Unlike \code{\link{process_openforis_census}}, every stem is a new
#' individual — there is no recruit/existing split.
#'
#' Plot coordinates are **not** produced: they are not part of the OpenForis
#' raw export and must be added separately (e.g. with
#' \code{add_plot_coordinates()}).
#'
#' @param data_dir Path to the directory containing the OpenForis xlsx exports.
#'   The function looks for a tree file matching \code{tree_file_pattern} and a
#'   plot file matching \code{plot_file_pattern}. Ignored if \code{tree_file}
#'   is provided explicitly.
#' @param codes_dir Path to the directory containing the OpenForis code-list
#'   CSVs. Files are auto-detected by pattern: \code{code_observations*} (or
#'   \code{code_list_observation*}), \code{code_pom*}, \code{code_light*},
#'   \code{code_pheno*}, \code{code_quadrat*}, \code{code_morpho*},
#'   \code{code_forest_state*}, \code{code_forest_type*}, \code{code_country*},
#'   \code{code_list_team_leader*}. Ignored for any code file given explicitly.
#' @param tree_file Path to \code{tree_list.xlsx}. If NULL, auto-detected from
#'   \code{data_dir}.
#' @param plot_file Path to \code{plot.xlsx}. If NULL, auto-detected from
#'   \code{data_dir}. Set to FALSE to skip (no \code{plots} /
#'   \code{census_metadata} output).
#' @param tree_file_pattern Glob pattern for the tree xlsx
#'   (default \code{"tree_list*"}).
#' @param plot_file_pattern Glob pattern for the plot xlsx
#'   (default \code{"plot*"}).
#' @param observation_codes,pom_codes,light_codes,pheno_codes,quadrat_codes,morpho_codes
#'   Paths to the individual-level code-list CSVs. NULL to auto-detect from
#'   \code{codes_dir}, FALSE to skip that decoding.
#' @param forest_state_codes,forest_type_codes,country_codes,team_leader_codes
#'   Paths to the plot-level code-list CSVs. NULL to auto-detect from
#'   \code{codes_dir}, FALSE to skip that decoding.
#' @param method Character. Plot method (e.g. \code{"1ha-IRD"}). Not present in
#'   the OpenForis export — supply it here. NULL to omit.
#' @param province Character. Province/region. NULL to omit.
#' @param country Character. Overrides the country decoded from the export.
#'   NULL keeps the decoded value.
#' @param data_provider Character. Data provider (e.g. \code{"IRD"}).
#' @param principal_investigator Character. PI name(s).
#' @param data_manager Character. Data manager name(s).
#' @param additional_people Character. Overrides the \code{add_people} column
#'   from the export. NULL keeps the exported value. When given it is also
#'   written to the \code{additional_people} column of the specimen table, and
#'   feeds \code{additional_collector} wherever the plot file cannot supply a
#'   team (no plot file read, or a plot name missing from it).
#' @param census Integer. Census number for the census feature
#'   (default \code{1} — a newly established plot).
#' @param specimen_prefix Character prefix for specimen numbers (e.g.
#'   \code{"PIRD"}). NULL leaves the numbers as-is.
#' @param specimen_locality Character. Locality string for specimens.
#' @param specimen_country Character. Country for specimens. If NULL, falls
#'   back to the plot country.
#' @param specimen_col_month,specimen_col_year Integer. Collection month/year.
#'   If NULL, taken from the plot file dates when these are unambiguous.
#' @param specimen_collector Character. Collector code stored as \code{colnam}.
#'   If NULL, taken from the \code{colnam} column of the plot file when it holds
#'   a single value.
#' @param specimen_description_col Column used to build the specimen
#'   description (default \code{"stem_diameter"}). NULL to skip.
#' @param plot_name_col Column name for plot name in the tree file
#'   (default \code{"plot_plot_name"}).
#' @param tag_col Column name for the tree tag (default \code{"tag"}).
#'
#' @return A list with components:
#' \describe{
#'   \item{plots}{Tibble, one row per plot, ready for \code{add_plots()} /
#'     the Import Wizard: plot_name, country, province, method, data_provider,
#'     team_leader, principal_investigator, data_manager, additional_people,
#'     identified_by, forest_state, forest_type, date_y, date_m, date_d. A plot
#'     can carry several forest types; they are decoded and collapsed into a
#'     single comma-separated \code{forest_type} value. NULL if no plot file.}
#'   \item{census_metadata}{Tibble with plot_name, year, month, day, census,
#'     team_leader, additional_people, ready for \code{add_subplot_features()}.
#'     NULL if no plot file.}
#'   \item{individuals}{Tibble of every stem: plot_name, tag, quadrat,
#'     original_tax_name, idtax_n, tax_appendix, herbarium_nbe_char,
#'     herbarium_nbe_type, position_x, position_y, multi_stem,
#'     number_multi_stem.}
#'   \item{measurements}{Tibble in long format (plot_name, tag, trait_name,
#'     traitvalue, traitvalue_char) covering stem_diameter,
#'     height_of_stem_diameter, tree_height, position_x, position_y, light,
#'     observation (observations, phenology and free-text comments) and
#'     pom_observation. Stems with no value for a given trait produce no row.}
#'   \item{specimens}{Tibble ready for \code{add_specimens()}, one row per
#'     unique voucher (not one per individual), or NULL. The
#'     \code{additional_collector} column lists the collecting team of the plot
#'     — its team leader plus the additional people recorded in
#'     \code{plot.xlsx}; \code{additional_people} holds the argument of the
#'     same name when one was given.}
#'   \item{multi_stems}{Tibble of candidate multi-stem groupings with a
#'     \code{flag} column to review, or NULL.}
#'   \item{duplicated_stems}{Tibble of rows sharing a plot_name + tag pair, or
#'     NULL. A \code{warning()} is also raised.}
#'   \item{summary}{List of counts.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- process_openforis_new_plot(
#'   data_dir  = "path/to/mission/plots/",
#'   codes_dir = "path/to/openforis/",
#'   method = "1ha-IRD",
#'   province = "Centre",
#'   data_provider = "IRD",
#'   principal_investigator = "Jane Doe",
#'   data_manager = "John Smith",
#'   specimen_prefix = "PIRD"
#' )
#'
#' writexl::write_xlsx(result$plots, "plots.xlsx")
#' writexl::write_xlsx(result$individuals, "individuals.xlsx")
#' writexl::write_xlsx(result$measurements, "measurements_long.xlsx")
#' }
#'
#' @seealso \code{\link{process_openforis_census}} for re-measurement exports.
#' @keywords internal
process_openforis_new_plot <- function(data_dir = NULL,
                                       codes_dir = NULL,
                                       tree_file = NULL,
                                       plot_file = NULL,
                                       tree_file_pattern = "tree_list*",
                                       plot_file_pattern = "plot*",
                                       observation_codes = NULL,
                                       pom_codes = NULL,
                                       light_codes = NULL,
                                       pheno_codes = NULL,
                                       quadrat_codes = NULL,
                                       morpho_codes = NULL,
                                       forest_state_codes = NULL,
                                       forest_type_codes = NULL,
                                       country_codes = NULL,
                                       team_leader_codes = NULL,
                                       method = NULL,
                                       province = NULL,
                                       country = NULL,
                                       data_provider = NULL,
                                       principal_investigator = NULL,
                                       data_manager = NULL,
                                       additional_people = NULL,
                                       census = 1,
                                       specimen_prefix = NULL,
                                       specimen_locality = NULL,
                                       specimen_country = NULL,
                                       specimen_col_month = NULL,
                                       specimen_col_year = NULL,
                                       specimen_collector = NULL,
                                       specimen_description_col = "stem_diameter",
                                       plot_name_col = "plot_plot_name",
                                       tag_col = "tag") {

  # ---- Auto-detect files from directories ----
  if (is.null(tree_file) && !is.null(data_dir)) {
    tree_file <- .find_file_in_dir(data_dir, tree_file_pattern, ext = "xlsx",
                                   label = "tree list")
  }
  if (is.null(plot_file) && !is.null(data_dir)) {
    found <- .find_file_in_dir(data_dir, plot_file_pattern, ext = "xlsx",
                               label = "plot data", required = FALSE)
    plot_file <- if (!is.null(found)) found else NULL
  }

  if (!is.null(codes_dir)) {
    if (is.null(observation_codes))
      observation_codes <- .find_code_file(
        codes_dir, c("code_observations*", "code_list_observation*"),
        "observation codes")
    if (is.null(pom_codes))
      pom_codes <- .find_code_file(codes_dir, "code_pom*", "POM codes")
    if (is.null(light_codes))
      light_codes <- .find_code_file(codes_dir, "code_light*", "light codes")
    if (is.null(pheno_codes))
      pheno_codes <- .find_code_file(codes_dir, "code_pheno*", "phenology codes")
    if (is.null(quadrat_codes))
      quadrat_codes <- .find_code_file(codes_dir, "code_quadrat*", "quadrat codes")
    if (is.null(morpho_codes))
      morpho_codes <- .find_code_file(codes_dir, "code_morpho*", "morphospecies codes")
    if (is.null(forest_state_codes))
      forest_state_codes <- .find_code_file(codes_dir, "code_forest_state*",
                                            "forest state codes")
    if (is.null(forest_type_codes))
      forest_type_codes <- .find_code_file(codes_dir, "code_forest_type*",
                                           "forest type codes")
    if (is.null(country_codes))
      country_codes <- .find_code_file(codes_dir, "code_country*", "country codes")
    if (is.null(team_leader_codes))
      team_leader_codes <- .find_code_file(codes_dir, "code_list_team_leader*",
                                           "team leader codes")
  }

  # FALSE = explicit skip
  for (nm in c("plot_file", "observation_codes", "pom_codes", "light_codes",
               "pheno_codes", "quadrat_codes", "morpho_codes",
               "forest_state_codes", "forest_type_codes", "country_codes",
               "team_leader_codes")) {
    if (identical(get(nm), FALSE)) assign(nm, NULL)
  }

  if (is.null(tree_file)) {
    stop("tree_file is required. Provide it directly or set data_dir to auto-detect.")
  }

  # ---- Read code lists ----
  obs_code_list    <- .read_code_csv(observation_codes)
  pom_code_list    <- .read_code_csv(pom_codes)
  light_code_list  <- .read_code_csv(light_codes)
  pheno_code_list  <- .read_code_csv(pheno_codes)
  quad_code_list   <- .read_code_csv(quadrat_codes)
  morpho_code_list <- .read_code_csv(morpho_codes)
  fstate_code_list <- .read_code_csv(forest_state_codes)
  ftype_code_list  <- .read_code_csv(forest_type_codes)
  country_code_list <- .read_code_csv(country_codes)
  tl_code_list     <- .read_code_csv(team_leader_codes)

  # ---- Read and normalise the tree list ----
  cli::cli_alert_info("Reading tree list from {.file {tree_file}}")
  trees_raw <- as.data.frame(readxl::read_excel(tree_file, guess_max = 5000))
  cli::cli_alert_success("Read {nrow(trees_raw)} tree record{?s}")

  trees <- .normalise_openforis_new_plot_trees(trees_raw, plot_name_col, tag_col)

  # ---- Check for duplicated plot_name + tag combinations ----
  dup_key <- paste(trees$plot_name, trees$tag, sep = "__")
  dup_mask <- duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)
  duplicated_stems <- NULL
  if (any(dup_mask)) {
    dup_cols <- intersect(
      c("plot_name", "tag", "species_scientific_name", "species_code",
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

  # ---- Plot-level tables ----
  plots <- NULL
  census_metadata <- NULL

  if (!is.null(plot_file)) {
    cli::cli_alert_info("Reading plot metadata from {.file {plot_file}}")
    plots_raw <- as.data.frame(readxl::read_excel(plot_file))

    plots <- .prepare_openforis_new_plots(
      plots_raw,
      country_codes = country_code_list,
      forest_state_codes = fstate_code_list,
      forest_type_codes = ftype_code_list,
      team_leader_codes = tl_code_list,
      country = country, province = province, method = method,
      data_provider = data_provider,
      principal_investigator = principal_investigator,
      data_manager = data_manager,
      additional_people = additional_people
    )
    cli::cli_alert_success("Plot metadata for {nrow(plots)} plot{?s}")

    census_metadata <- .prepare_openforis_new_plot_census(plots, census)

    # Cross-check plot names between the two files
    missing_plots <- setdiff(unique(trees$plot_name), plots$plot_name)
    if (length(missing_plots) > 0) {
      cli::cli_alert_warning(
        "Plot name{?s} in the tree list absent from the plot file: {.val {missing_plots}} — check for spelling/padding differences"
      )
    }

    # Fall back on the plot file for specimen collector / date
    if (is.null(specimen_collector) && "colnam" %in% names(plots_raw)) {
      colnam_vals <- unique(stats::na.omit(as.character(plots_raw$colnam)))
      if (length(colnam_vals) == 1) {
        specimen_collector <- colnam_vals
        cli::cli_alert_info("Using {.val {specimen_collector}} as specimen collector (from plot file)")
      }
    }
    if (is.null(specimen_col_year) && "date_y" %in% names(plots)) {
      yrs <- unique(stats::na.omit(plots$date_y))
      if (length(yrs) == 1) specimen_col_year <- yrs
    }
    if (is.null(specimen_col_month) && "date_m" %in% names(plots)) {
      mths <- unique(stats::na.omit(plots$date_m))
      if (length(mths) == 1) specimen_col_month <- mths
    }
    if (is.null(specimen_country) && "country" %in% names(plots)) {
      ctr <- unique(stats::na.omit(plots$country))
      if (length(ctr) == 1) specimen_country <- ctr
    }
  }

  # ---- Individuals ----
  individuals <- .prepare_openforis_new_plot_individuals(
    trees, specimen_prefix,
    quadrat_codes = quad_code_list,
    morpho_codes = morpho_code_list
  )
  cli::cli_alert_success("Prepared {nrow(individuals)} individual{?s}")

  # ---- Measurements (long format) ----
  measurement_parts <- list()

  numeric_cols <- intersect(
    c("stem_diameter", "height_of_stem_diameter", "tree_height",
      "position_x", "position_y"),
    names(trees)
  )
  if (length(numeric_cols) > 0) {
    cli::cli_alert_info("Extracting numeric measurements: {.val {numeric_cols}}")
    measurement_parts$numeric <- .extract_numeric_traits(trees, numeric_cols)
  }

  if (!is.null(obs_code_list)) {
    cli::cli_alert_info("Decoding observation codes")
    measurement_parts$observations <- .decode_openforis_observations(
      trees, obs_code_list
    )
  }

  if (!is.null(pom_code_list)) {
    cli::cli_alert_info("Decoding POM observation codes")
    measurement_parts$pom <- .decode_openforis_pom(trees, pom_code_list)
  }

  if (!is.null(light_code_list)) {
    cli::cli_alert_info("Decoding light codes")
    measurement_parts$light <- .decode_openforis_light(trees, light_code_list)
  }

  if (!is.null(pheno_code_list)) {
    cli::cli_alert_info("Decoding phenology codes")
    measurement_parts$pheno <- .decode_openforis_pheno(trees, pheno_code_list)
  }

  # Estimated (not measured) diameters — dbh_measurement code 2
  measurement_parts$estimated_dbh <- .flag_estimated_diameters(trees)

  # Free-text comments become observations
  if ("comment" %in% names(trees)) {
    keep <- !is.na(trees$comment) & nzchar(trimws(as.character(trees$comment)))
    if (any(keep)) {
      measurement_parts$comments <- data.frame(
        plot_name = trees$plot_name[keep],
        tag = trees$tag[keep],
        trait_name = "observation",
        traitvalue = NA_real_,
        traitvalue_char = trimws(as.character(trees$comment[keep])),
        stringsAsFactors = FALSE
      )
      cli::cli_alert_info(
        "Extracted {sum(keep)} 'observation' row{?s} from the 'comment' column"
      )
    }
  }

  measurements <- do.call(rbind, measurement_parts)
  if (!is.null(measurements)) {
    rownames(measurements) <- NULL
    measurements <- measurements[
      !is.na(measurements$traitvalue) | !is.na(measurements$traitvalue_char),
      , drop = FALSE
    ]
    cli::cli_alert_success(
      "Total: {nrow(measurements)} measurement rows across {length(unique(measurements$trait_name))} trait{?s}"
    )
  } else {
    measurements <- data.frame(
      plot_name = character(0), tag = numeric(0),
      trait_name = character(0), traitvalue = numeric(0),
      traitvalue_char = character(0), stringsAsFactors = FALSE
    )
    cli::cli_alert_warning("No measurements extracted")
  }

  # ---- Multi-stem grouping ----
  multi_stems <- .build_openforis_multi_stems(trees)
  if (!is.null(multi_stems)) {
    n_groups <- length(unique(paste(multi_stems$plot_name, multi_stems$group_tag)))
    n_flagged <- sum(!is.na(multi_stems$flag))
    cli::cli_alert_success(
      "Detected {n_groups} multi-stem group{?s} ({nrow(multi_stems)} stems total)"
    )
    if (n_flagged > 0) {
      cli::cli_alert_warning("{n_flagged} stem{?s} flagged — review recommended")
    }
  }

  # ---- Specimens ----
  # Collecting team per plot: team leader plus the additional people
  spec_collectors <- .build_openforis_collector_team(plots)

  # With no plot file there is no team to derive, so honour the argument
  if (is.null(spec_collectors) && !is.null(additional_people)) {
    spec_collectors <- .collapse_people(additional_people)
    cli::cli_alert_info(
      "Using the {.arg additional_people} argument as specimen collecting team"
    )
  }

  specimens <- .prepare_openforis_specimens(
    trees, census_metadata, specimen_prefix,
    locality = specimen_locality,
    country = specimen_country,
    col_month = specimen_col_month,
    col_year = specimen_col_year,
    collector = specimen_collector,
    additional_people = additional_people,
    additional_collector = spec_collectors,
    deduplicate = TRUE,
    description_col = specimen_description_col
  )
  if (!is.null(specimens) && is.data.frame(spec_collectors) &&
      anyNA(specimens$additional_collector)) {
    orphans <- unique(specimens$plot_name[is.na(specimens$additional_collector)])
    # An explicit argument wins; otherwise only an unambiguous team can be used
    uniq_team <- if (!is.null(additional_people)) {
      .collapse_people(additional_people)
    } else {
      unique(stats::na.omit(spec_collectors$additional_collector))
    }
    if (length(uniq_team) == 1 && !is.na(uniq_team)) {
      specimens$additional_collector[is.na(specimens$additional_collector)] <- uniq_team
      if (!is.null(additional_people)) {
        cli::cli_alert_info(
          "No plot-file match for {.val {orphans}}; used the {.arg additional_people} argument as collecting team"
        )
      } else {
        cli::cli_alert_info(
          "No plot-file match for {.val {orphans}}; applied the single collecting team found"
        )
      }
    } else {
      cli::cli_alert_warning(
        "No collecting team for specimens of {.val {orphans}} — plot name absent from the plot file"
      )
    }
  }
  if (!is.null(specimens)) {
    cli::cli_alert_success("Prepared {nrow(specimens)} specimen{?s}")
  }

  # ---- Tibbles ----
  as_tbl <- function(x) if (!is.null(x)) dplyr::as_tibble(x) else NULL
  plots            <- as_tbl(plots)
  census_metadata  <- as_tbl(census_metadata)
  individuals      <- as_tbl(individuals)
  measurements     <- as_tbl(measurements)
  specimens        <- as_tbl(specimens)
  multi_stems      <- as_tbl(multi_stems)
  duplicated_stems <- as_tbl(duplicated_stems)

  # ---- Summary ----
  summary_info <- list(
    n_plots = length(unique(trees$plot_name)),
    n_individuals = nrow(individuals),
    n_measurements = nrow(measurements),
    n_specimens = if (!is.null(specimens)) nrow(specimens) else 0L,
    n_multi_stem_groups = if (!is.null(multi_stems)) {
      length(unique(paste(multi_stems$plot_name, multi_stems$group_tag)))
    } else 0L,
    n_duplicated_stems = if (!is.null(duplicated_stems)) nrow(duplicated_stems) else 0L,
    trait_names = if (nrow(measurements) > 0) unique(measurements$trait_name) else character(0)
  )

  cli::cli_rule()
  cli::cli_alert_success("Processing complete:")
  cli::cli_bullets(c(
    "*" = "{summary_info$n_plots} plot(s)",
    "*" = "{summary_info$n_individuals} individual(s)",
    "*" = "{summary_info$n_measurements} measurement rows",
    "*" = "{summary_info$n_specimens} specimen(s)",
    "*" = "{summary_info$n_multi_stem_groups} multi-stem group(s)",
    "*" = "{summary_info$n_duplicated_stems} duplicated stem row(s)",
    "*" = "Traits: {paste(summary_info$trait_names, collapse = ', ')}"
  ))
  cli::cli_alert_info(
    "Plot coordinates are not part of the OpenForis export — add them separately."
  )

  list(
    plots = plots,
    census_metadata = census_metadata,
    individuals = individuals,
    measurements = measurements,
    specimens = specimens,
    multi_stems = multi_stems,
    duplicated_stems = duplicated_stems,
    summary = summary_info
  )
}


# ===========================================================================
# Internal helpers — new-plot specific
# ===========================================================================

#' Normalise raw OpenForis new-plot tree columns
#'
#' The new-plot form and the recensus form use different names for the same
#' fields. This renames the new-plot columns to the recensus vocabulary so the
#' decoding helpers in \code{openforis_processing.R} can be reused unchanged.
#'
#' @param trees_raw Data frame read from \code{tree_list.xlsx}.
#' @param plot_name_col Column holding the plot name.
#' @param tag_col Column holding the tag.
#' @return Data frame with normalised column names, plus \code{plot_name} and
#'   a numeric \code{tag}.
#' @keywords internal
.normalise_openforis_new_plot_trees <- function(trees_raw,
                                                plot_name_col = "plot_plot_name",
                                                tag_col = "tag") {

  trees <- trees_raw

  if (!plot_name_col %in% names(trees)) {
    stop(sprintf("Column '%s' not found in tree file. Available: %s",
                 plot_name_col, paste(names(trees), collapse = ", ")))
  }
  if (!tag_col %in% names(trees)) {
    stop(sprintf("Column '%s' not found in tree file. Available: %s",
                 tag_col, paste(names(trees), collapse = ", ")))
  }

  # One-to-one renames (only applied when the target does not already exist)
  renames <- c(
    dbh                   = "stem_diameter",
    dbh_height            = "height_of_stem_diameter",
    height_measure        = "tree_height",
    taxa_scientific_name  = "species_scientific_name",
    taxa_code             = "species_code",
    number_stem           = "number_multi_stem",
    specimen_name         = "herbarium_nbe_char",
    specimen_nbr          = "specimen_number",
    voucher_description   = "specimes_descriptipn"
  )
  for (from in names(renames)) {
    to <- renames[[from]]
    if (from %in% names(trees) && !to %in% names(trees)) {
      names(trees)[names(trees) == from] <- to
    }
  }

  # Indexed columns: observations[1] -> observation_1, pom_measure[1] ->
  # pom_observations_1, pheno[1] -> pheno_1, leaves_sample_origin[1] ->
  # branch_position. The prefixes matter: the decoding helpers select columns
  # by regex.
  names(trees) <- .rename_indexed(names(trees), "observations", "observation_")
  names(trees) <- .rename_indexed(names(trees), "pom_measure", "pom_observations_")
  names(trees) <- .rename_indexed(names(trees), "pheno", "pheno_")

  lso <- grep("^leaves_sample_origin", names(trees), value = TRUE)
  if (length(lso) > 0 && !"branch_position" %in% names(trees)) {
    names(trees)[names(trees) == lso[1]] <- "branch_position"
  }

  trees$plot_name <- as.character(trees[[plot_name_col]])
  trees$tag <- suppressWarnings(as.numeric(trees[[tag_col]]))

  trees
}


#' Rename bracketed OpenForis columns (e.g. "pheno[1]" -> "pheno_1")
#'
#' @param nms Character vector of column names.
#' @param stem Base name without index (e.g. "pheno").
#' @param prefix Replacement prefix, index appended (e.g. "pheno_").
#' @return The modified character vector.
#' @keywords internal
.rename_indexed <- function(nms, stem, prefix) {
  pattern <- paste0("^", stem, "\\[([0-9]+)\\]$")
  hit <- grepl(pattern, nms)
  if (any(hit)) {
    nms[hit] <- paste0(prefix, sub(pattern, "\\1", nms[hit]))
  }
  # Unbracketed single column (some exports drop the index)
  exact <- nms == stem
  if (any(exact)) nms[exact] <- paste0(prefix, "1")
  nms
}


#' Build the plot-level table from a raw OpenForis plot export
#'
#' The \code{forest_type[1..n]} columns are decoded and collapsed into a single
#' comma-separated \code{forest_type} column, alongside \code{forest_state}.
#'
#' @param plots_raw Data frame read from \code{plot.xlsx}.
#' @param country_codes,forest_state_codes,forest_type_codes,team_leader_codes
#'   Parsed code lists, or NULL to skip that decoding.
#' @param country,province,method,data_provider,principal_investigator,data_manager
#'   Constants not present in the export. NULL to omit.
#' @param additional_people Overrides the exported \code{add_people} column.
#' @return Data frame with one row per plot.
#' @keywords internal
.prepare_openforis_new_plots <- function(plots_raw,
                                         country_codes = NULL,
                                         forest_state_codes = NULL,
                                         forest_type_codes = NULL,
                                         team_leader_codes = NULL,
                                         country = NULL, province = NULL,
                                         method = NULL, data_provider = NULL,
                                         principal_investigator = NULL,
                                         data_manager = NULL,
                                         additional_people = NULL) {

  if (!"plot_name" %in% names(plots_raw)) {
    stop("Column 'plot_name' not found in the plot file. Available: ",
         paste(names(plots_raw), collapse = ", "))
  }

  result <- data.frame(
    plot_name = as.character(plots_raw$plot_name),
    stringsAsFactors = FALSE
  )

  # Country: decoded, then overridden by the argument if supplied
  decoded_country <- if ("country" %in% names(plots_raw)) {
    .decode_code_column(plots_raw$country, country_codes,
                        "country_code", "country_label_en")
  } else {
    rep(NA_character_, nrow(plots_raw))
  }
  result$country <- if (!is.null(country)) country else decoded_country

  if (!is.null(province)) result$province <- province
  if (!is.null(method)) result$method <- method
  if (!is.null(data_provider)) result$data_provider <- data_provider

  # Team leader: coded, with a free-text fallback for "other"
  tl <- if ("team_leader" %in% names(plots_raw)) {
    .decode_code_column(plots_raw$team_leader, team_leader_codes,
                        "item_code", "item_label_en")
  } else {
    rep(NA_character_, nrow(plots_raw))
  }
  if ("team_leader_other" %in% names(plots_raw)) {
    other <- as.character(plots_raw$team_leader_other)
    needs_other <- is.na(tl) | grepl("^(other|autre)$", tl, ignore.case = TRUE)
    tl <- ifelse(needs_other & !is.na(other) & nzchar(trimws(other)),
                 trimws(other), tl)
  }
  result$team_leader <- tl

  if (!is.null(principal_investigator))
    result$principal_investigator <- principal_investigator
  if (!is.null(data_manager)) result$data_manager <- data_manager

  result$additional_people <- if (!is.null(additional_people)) {
    additional_people
  } else if ("add_people" %in% names(plots_raw)) {
    as.character(plots_raw$add_people)
  } else {
    NA_character_
  }

  if ("identification" %in% names(plots_raw))
    result$identified_by <- as.character(plots_raw$identification)

  if ("forest_state" %in% names(plots_raw)) {
    result$forest_state <- .decode_code_column(
      plots_raw$forest_state, forest_state_codes,
      "foreststate_code", "foreststate_label_en"
    )
  }

  # A plot can carry several forest types — collapse them into one column
  forest_types <- .prepare_openforis_forest_types(plots_raw, forest_type_codes)
  if (!is.null(forest_types)) {
    collapsed <- stats::aggregate(
      forest_type ~ plot_name, data = forest_types,
      FUN = function(x) paste(unique(x), collapse = ", ")
    )
    result$forest_type <- collapsed$forest_type[
      match(result$plot_name, collapsed$plot_name)
    ]
  }

  if ("date_year" %in% names(plots_raw))
    result$date_y <- suppressWarnings(as.integer(plots_raw$date_year))
  if ("date_month" %in% names(plots_raw))
    result$date_m <- suppressWarnings(as.integer(plots_raw$date_month))
  if ("date_day" %in% names(plots_raw))
    result$date_d <- suppressWarnings(as.integer(plots_raw$date_day))

  result
}


#' Pivot the forest_type[1..n] columns into a long table
#'
#' Feeds the collapsed \code{forest_type} column of the plot table built by
#' \code{.prepare_openforis_new_plots()}.
#'
#' @param plots_raw Data frame read from \code{plot.xlsx}.
#' @param code_list Parsed forest type code list, or NULL.
#' @return Data frame with plot_name and forest_type, or NULL.
#' @keywords internal
.prepare_openforis_forest_types <- function(plots_raw, code_list = NULL) {

  ft_cols <- grep("^forest_type", names(plots_raw), value = TRUE)
  if (length(ft_cols) == 0 || !"plot_name" %in% names(plots_raw)) return(NULL)

  rows <- list()
  for (col in ft_cols) {
    vals <- suppressWarnings(as.numeric(plots_raw[[col]]))
    valid <- !is.na(vals)
    if (!any(valid)) next
    rows[[col]] <- data.frame(
      plot_name = as.character(plots_raw$plot_name)[valid],
      code = vals[valid],
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return(NULL)

  long <- do.call(rbind, rows)
  long$forest_type <- .decode_code_column(long$code, code_list,
                                          "forest_code", "forest_label_en")

  # Keep the raw code when decoding is unavailable or failed
  long$forest_type <- ifelse(is.na(long$forest_type),
                             as.character(long$code), long$forest_type)

  result <- long[order(long$plot_name), c("plot_name", "forest_type"),
                 drop = FALSE]
  result <- result[!duplicated(paste(result$plot_name, result$forest_type)), ,
                   drop = FALSE]
  rownames(result) <- NULL
  result
}


#' Build the census feature table for a newly established plot
#'
#' @param plots Plot table produced by \code{.prepare_openforis_new_plots()}.
#' @param census Census number (default 1).
#' @return Data frame ready for \code{add_subplot_features()}, or NULL.
#' @keywords internal
.prepare_openforis_new_plot_census <- function(plots, census = 1) {

  if (is.null(plots) || nrow(plots) == 0) return(NULL)

  result <- data.frame(
    plot_name = plots$plot_name,
    stringsAsFactors = FALSE
  )
  result$year  <- if ("date_y" %in% names(plots)) plots$date_y else NA_integer_
  result$month <- if ("date_m" %in% names(plots)) plots$date_m else NA_integer_
  result$day   <- if ("date_d" %in% names(plots)) plots$date_d else NA_integer_
  if (!is.null(census)) result$census <- census
  if ("team_leader" %in% names(plots)) result$team_leader <- plots$team_leader
  if ("additional_people" %in% names(plots))
    result$additional_people <- plots$additional_people

  result
}


#' Collapse people strings into a single list of names
#'
#' Field teams separate names with either a comma or a semicolon, so both are
#' treated as separators. Names appearing in more than one input are kept once.
#'
#' @param ... Character vectors of people names.
#' @return Single comma-separated string, or NA when nothing is left.
#' @keywords internal
.collapse_people <- function(...) {

  parts <- as.character(c(...))
  parts <- parts[!is.na(parts) & nzchar(trimws(parts))]
  if (length(parts) == 0) return(NA_character_)

  people <- unique(trimws(unlist(strsplit(parts, "[,;]"))))
  people <- people[nzchar(people)]
  if (length(people) == 0) NA_character_ else paste(people, collapse = ", ")
}


#' Build the collecting team of each plot
#'
#' Merges the \code{team_leader} and \code{additional_people} values decoded
#' from \code{plot.xlsx} into a single comma-separated list of people, dropping
#' anyone named in both. Fed to the \code{additional_collector} column of the
#' specimen table.
#'
#' @param plots Plot table produced by \code{.prepare_openforis_new_plots()}.
#' @return Data frame with columns \code{plot_name} and
#'   \code{additional_collector}, or NULL when neither people column exists.
#' @keywords internal
.build_openforis_collector_team <- function(plots) {

  if (is.null(plots) || nrow(plots) == 0) return(NULL)
  people_cols <- intersect(c("team_leader", "additional_people"), names(plots))
  if (length(people_cols) == 0) return(NULL)

  team <- vapply(seq_len(nrow(plots)), function(i) {
    .collapse_people(vapply(people_cols,
                            function(cl) as.character(plots[[cl]][i]),
                            character(1)))
  }, character(1))

  data.frame(
    plot_name = plots$plot_name,
    additional_collector = team,
    stringsAsFactors = FALSE
  )
}


#' Build the individuals table from normalised OpenForis new-plot tree data
#'
#' Every stem of a newly established plot is a new individual. The taxon name
#' is the field identification with the morphospecies label appended when one
#' was recorded (e.g. "Drypetes sp." + "sp1"); \code{tax_appendix} is kept in
#' its own column for review rather than merged.
#'
#' @param trees Normalised tree data frame.
#' @param specimen_prefix Prefix applied to voucher numbers. NULL to skip.
#' @param quadrat_codes,morpho_codes Parsed code lists, or NULL.
#' @return Data frame with one row per stem.
#' @keywords internal
.prepare_openforis_new_plot_individuals <- function(trees,
                                                    specimen_prefix = NULL,
                                                    quadrat_codes = NULL,
                                                    morpho_codes = NULL) {

  result <- data.frame(
    plot_name = trees$plot_name,
    tag = trees$tag,
    stringsAsFactors = FALSE
  )

  # Quadrat (subplot) label
  if ("quadrat" %in% names(trees)) {
    decoded <- .decode_code_column(trees$quadrat, quadrat_codes,
                                   "quadrat_code", "quadrat_label_en")
    result$quadrat <- ifelse(is.na(decoded), as.character(trees$quadrat), decoded)
  }

  # Taxonomy: scientific name + morphospecies label
  tax_name <- if ("species_scientific_name" %in% names(trees)) {
    as.character(trees$species_scientific_name)
  } else {
    rep(NA_character_, nrow(trees))
  }

  if ("morpho_sp" %in% names(trees)) {
    morpho <- .decode_code_column(trees$morpho_sp, morpho_codes,
                                  "people_code", "people_label_en")
    # Fall back on the raw code when the code list is unavailable
    raw_morpho <- as.character(trees$morpho_sp)
    morpho <- ifelse(is.na(morpho) & !is.na(raw_morpho), raw_morpho, morpho)
    tax_name <- ifelse(!is.na(morpho) & !is.na(tax_name),
                       paste(tax_name, morpho), tax_name)
  }
  result$original_tax_name <- tax_name

  if ("species_code" %in% names(trees))
    result$idtax_n <- suppressWarnings(as.numeric(trees$species_code))
  if ("tax_appendix" %in% names(trees))
    result$tax_appendix <- as.character(trees$tax_appendix)

  # Vouchers
  if ("herbarium_nbe_char" %in% names(trees)) {
    voucher <- if (!is.null(specimen_prefix)) {
      ifelse(!is.na(trees$herbarium_nbe_char),
             paste(specimen_prefix, as.character(trees$herbarium_nbe_char)),
             NA_character_)
    } else {
      as.character(trees$herbarium_nbe_char)
    }
    result$herbarium_nbe_char <- voucher
    result$herbarium_nbe_type <- voucher
  }

  if ("position_x" %in% names(trees))
    result$position_x <- suppressWarnings(as.numeric(trees$position_x))
  if ("position_y" %in% names(trees))
    result$position_y <- suppressWarnings(as.numeric(trees$position_y))

  if ("multi_stem" %in% names(trees)) result$multi_stem <- trees$multi_stem
  if ("number_multi_stem" %in% names(trees))
    result$number_multi_stem <- suppressWarnings(as.integer(trees$number_multi_stem))

  result <- result[order(result$plot_name, result$tag), , drop = FALSE]
  rownames(result) <- NULL
  result
}


#' Decode OpenForis phenology columns into observation rows
#'
#' Phenology codes are emitted as \code{observation} rows, alongside the
#' \code{observations[]} codes and free-text comments.
#'
#' @param data Normalised tree data frame (pheno columns renamed \code{pheno_*}).
#' @param code_list Parsed phenology code list.
#' @return Long-format data frame, or NULL.
#' @keywords internal
.decode_openforis_pheno <- function(data, code_list) {
  pheno_cols <- grep("^pheno_[0-9]+$", names(data), value = TRUE)
  if (length(pheno_cols) == 0) return(NULL)

  long <- .pivot_coded_columns(data, pheno_cols, code_list,
                               code_col = "item_code",
                               label_col = "item_label_en")
  if (is.null(long) || nrow(long) == 0) return(NULL)

  data.frame(
    plot_name = long$plot_name,
    tag = long$tag,
    trait_name = "observation",
    traitvalue = NA_real_,
    traitvalue_char = long$decoded_value,
    stringsAsFactors = FALSE
  )
}


#' Flag estimated (as opposed to measured) diameters
#'
#' The OpenForis \code{dbh_measurement} field codes 1 = measured,
#' 2 = estimated. Only estimates are informative, so a single
#' \code{pom_observation} row is emitted for them.
#'
#' @param data Normalised tree data frame.
#' @return Long-format data frame, or NULL when nothing was estimated.
#' @keywords internal
.flag_estimated_diameters <- function(data) {
  if (!"dbh_measurement" %in% names(data)) return(NULL)

  vals <- suppressWarnings(as.numeric(data$dbh_measurement))
  estimated <- !is.na(vals) & vals == 2
  if (!any(estimated)) return(NULL)

  cli::cli_alert_info("{sum(estimated)} diameter{?s} flagged as estimated")

  data.frame(
    plot_name = data$plot_name[estimated],
    tag = data$tag[estimated],
    trait_name = "pom_observation",
    traitvalue = NA_real_,
    traitvalue_char = "estimated diameter",
    stringsAsFactors = FALSE
  )
}


#' Decode a coded column against an OpenForis code list
#'
#' @param values Vector of codes (numeric or character).
#' @param code_list Parsed code list data frame, or NULL.
#' @param code_col Name of the code column in \code{code_list}.
#' @param label_col Name of the label column in \code{code_list}.
#' @return Character vector of labels, NA where decoding failed.
#' @keywords internal
.decode_code_column <- function(values, code_list, code_col, label_col) {
  n <- length(values)
  if (is.null(code_list) || n == 0) return(rep(NA_character_, n))

  missing_cols <- setdiff(c(code_col, label_col), names(code_list))
  if (length(missing_cols) > 0) {
    cli::cli_alert_warning(
      "Code list missing column{?s} {.val {missing_cols}} — decoding skipped"
    )
    return(rep(NA_character_, n))
  }

  codes <- suppressWarnings(as.numeric(code_list[[code_col]]))
  labels <- as.character(code_list[[label_col]])
  labels[match(suppressWarnings(as.numeric(values)), codes)]
}


#' Read an OpenForis code-list CSV
#'
#' @param path File path, or NULL/FALSE to skip.
#' @return Data frame, or NULL.
#' @keywords internal
.read_code_csv <- function(path) {
  if (is.null(path) || identical(path, FALSE)) return(NULL)
  if (!file.exists(path)) {
    cli::cli_alert_warning("Code list not found: {.file {path}} — skipped")
    return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}


#' Find a code-list CSV, trying several glob patterns in turn
#'
#' Unlike \code{.find_file_in_dir()} this only reports once, after every
#' pattern has failed, so trying alternative names stays quiet.
#'
#' @param dir Directory to search.
#' @param patterns Character vector of glob patterns, tried in order.
#' @param label Human-readable label used in messages.
#' @return File path, or NULL when none matched.
#' @keywords internal
.find_code_file <- function(dir, patterns, label) {
  if (!dir.exists(dir)) {
    cli::cli_alert_warning("Directory not found: {.path {dir}}")
    return(NULL)
  }

  all_files <- list.files(dir, full.names = TRUE)
  csv_files <- all_files[grepl("\\.csv$", all_files, ignore.case = TRUE)]

  for (pattern in patterns) {
    regex_pat <- paste0("^", gsub("\\*", ".*", pattern))
    hits <- sort(csv_files[grepl(regex_pat, basename(csv_files),
                                 ignore.case = TRUE)])
    if (length(hits) > 0) {
      if (length(hits) > 1) {
        cli::cli_alert_warning(
          "Multiple {label} files found, using {.file {basename(hits[1])}}"
        )
      }
      cli::cli_alert_success("Found {label}: {.file {basename(hits[1])}}")
      return(hits[1])
    }
  }

  cli::cli_alert_warning("No {label} found in {.path {dir}} — skipped")
  NULL
}
