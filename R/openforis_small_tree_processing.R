# OpenForis Collect — small-tree (D < 10 cm) quadrat pre-processing
#
# Internal (non-exported) functions for converting the raw OpenForis Collect
# exports of a small-tree inventory (plot.xlsx + tree_list.xlsx) into clean
# data frames ready for the Import Wizard / Feature Wizard.
#
# This is the third of the OpenForis pre-processors, alongside
# process_openforis_census() (re-measurements) and process_openforis_new_plot()
# (a newly established 1-ha plot). The three forms share most of their coded
# columns, so the decoding helpers in openforis_processing.R and the table
# builders in openforis_new_plot_processing.R are reused; what is specific to
# the small-tree form lives here.
#
# Nothing in this file writes to the database.


#' Process an OpenForis small-tree export into clean data frames
#'
#' Reads the raw \code{plot.xlsx} and \code{tree_list.xlsx} files exported from
#' OpenForis Collect for a small-tree inventory — stems below the 10 cm
#' diameter threshold, censused inside a handful of 20 x 20 m quadrats of an
#' already established 1-ha plot — and returns clean tables ready for import.
#'
#' All the quadrats sampled in one parent plot together make **one small-tree
#' plot**, named \code{<parent plot><plot_suffix>} (e.g.
#' \code{"mbalmayo001_smalltree"}), with an area of the number of sampled
#' quadrats times \code{quadrat_area}. It is linked to its parent plot as a
#' \code{nested_subsample}. See the sections below for why.
#'
#' Plot coordinates are **not** produced: they are not part of the OpenForis
#' raw export and must be added separately (e.g. with
#' \code{add_plot_coordinates()}).
#'
#' @section Why a plot of its own:
#' The small trees are tagged 1..N within their parent plot, and those numbers
#' collide head-on with the tags the parent plot already uses for its large
#' stems — \code{mbalmayo001} holds tags 1-445 for D >= 10 cm and the small
#' trees restart at 1. A repeated plot + tag pair is treated as a data defect
#' everywhere else in this package (see \code{\link{split_census_table}}), and once
#' recorded it cannot be undone, so the small trees cannot be filed under the
#' parent plot without mangling their field tags.
#'
#' Within one parent plot, though, the \code{firsttag} column makes the tag
#' ranges of the quadrats disjoint, so the quadrats of a plot share one tag
#' sequence and are one sampling unit: every tag is kept exactly as written in
#' the field. The quadrat of each stem is not lost — it is recorded in the
#' \code{quadrat} feature of the individual, together with its 10 x 10 m
#' sub-quadrat (e.g. \code{"20_20_A"}).
#'
#' @section Link to the parent plot:
#' The plot table carries \code{parent_plot_name} and
#' \code{parent_relation = "nested_subsample"}, which the Import Wizard turns
#' into the \code{id_parent_plot} link. The relation says that the small-tree
#' plot overlaps the ground of its parent with another diameter threshold, so
#' the two must never be summed. The parent plot must already be in the
#' database.
#'
#' @section Quadrats sampled on different occasions:
#' \code{plot.xlsx} records date, team and forest type for each quadrat, and
#' one plot holds one value. The earliest date is kept, the team and forest
#' types of all quadrats are pooled, and any other column on which the
#' quadrats disagree keeps its first value. Each disagreement is announced.
#'
#' @section How stems are assigned to quadrats:
#' \code{tree_list.xlsx} does not name the quadrat a stem sits in. Its
#' \code{plot_plot_name} column collapses the quadrats of a plot into one
#' value, and \code{plot_plot_name_old} distinguishes them only by a trailing
#' underscore typed by hand — a convention the field teams do not always
#' follow.
#'
#' The reliable key is \code{firsttag}: \code{plot.xlsx} records, for every
#' quadrat, the tag its numbering started at. Stems are therefore assigned by
#' tag range — a stem belongs to the quadrat with the largest \code{firsttag}
#' not above its tag. The \code{plot_plot_name_old} grouping is used only as a
#' cross-check, and any disagreement is reported in \code{name_mismatches}.
#'
#' @section Deciding a mismatch:
#' A mismatch has no single right answer. A mistyped underscore makes the tag
#' right; a stem forgotten behind the team and tagged later, from the next
#' quadrat's range, makes the name right. Only the person who knows the field
#' work can tell, so the decision is theirs, taken in two runs:
#' \enumerate{
#'   \item Run once. The tag is used for every stem, and
#'     \code{name_mismatches} lists the disputed stems with the quadrat each
#'     side gives (\code{quadrat_by_tag}, \code{quadrat_by_name}) and a
#'     \code{keep} column set to \code{"tag"}. Set \code{mismatch_file} to get
#'     the table as an xlsx.
#'   \item Change \code{keep} to \code{"name"} for the stems that belong where
#'     the name puts them, then run again with that table as
#'     \code{quadrat_decisions}.
#' }
#' \code{quadrat_by_name} may also be edited, to send a stem to a third
#' quadrat of the same parent plot. A stem the decisions do not mention keeps
#' its tag quadrat. Only the stem's \code{quadrat} feature changes: every
#' quadrat of a parent plot is in the same small-tree plot. Multi-stem
#' grouping runs after the decisions, so a moved stem is grouped with its new
#' neighbours.
#'
#' @param data_dir Path to the directory containing the OpenForis xlsx exports.
#'   The function looks for a tree file matching \code{tree_file_pattern} and a
#'   plot file matching \code{plot_file_pattern}. Ignored if \code{tree_file}
#'   is provided explicitly.
#' @param codes_dir Path to the directory containing the OpenForis code-list
#'   CSVs. Files are auto-detected by pattern: \code{code_observations*} (or
#'   \code{code_list_observation*}), \code{code_pom*}, \code{code_light*},
#'   \code{code_pheno*}, \code{code_quadrat*},
#'   \code{code_subquadrat*} (or \code{code_sub_quadrat*}), \code{code_morpho*},
#'   \code{code_forest_state*}, \code{code_forest_type*}, \code{code_country*},
#'   \code{code_list_team_leader*}. Ignored for any code file given explicitly.
#' @param tree_file Path to \code{tree_list.xlsx}. If NULL, auto-detected from
#'   \code{data_dir}.
#' @param plot_file Path to \code{plot.xlsx}. If NULL, auto-detected from
#'   \code{data_dir}. Unlike the other pre-processors this file is
#'   **required** — it carries the quadrat of each unit and the
#'   \code{firsttag} that assigns stems to it.
#' @param tree_file_pattern Glob pattern for the tree xlsx
#'   (default \code{"tree_list*"}).
#' @param plot_file_pattern Glob pattern for the plot xlsx
#'   (default \code{"plot*"}).
#' @param observation_codes,pom_codes,light_codes,pheno_codes,morpho_codes
#'   Paths to the individual-level code-list CSVs. NULL to auto-detect from
#'   \code{codes_dir}, FALSE to skip that decoding.
#' @param quadrat_codes Path to the code list of the 20 x 20 m quadrats of the
#'   parent plot (\code{code_quadrat.csv}: codes 1-25, labels \code{"0_0"},
#'   \code{"20_20"}, ...). Decodes the \code{quadrat} column of
#'   \code{plot.xlsx}, the first part of the \code{quadrat} feature of each
#'   individual. NULL to auto-detect, FALSE to keep the raw codes.
#' @param subquadrat_codes Path to the code list of the 10 x 10 m sub-quadrats
#'   (\code{code_subquadrat_smalltree.csv}: codes 1-4, labels A-D). Decodes the
#'   \code{quadrat} column of \code{tree_list.xlsx}, the second part of the
#'   \code{quadrat} feature of each individual. NULL to auto-detect, FALSE to
#'   keep the raw codes.
#' @param forest_state_codes,forest_type_codes,country_codes,team_leader_codes
#'   Paths to the plot-level code-list CSVs. NULL to auto-detect from
#'   \code{codes_dir}, FALSE to skip that decoding.
#' @param method Character. Plot method for the small-tree plots. Not present in the
#'   OpenForis export — supply it here. Required by the Import Wizard, so a
#'   warning is raised when it is missing. NULL to omit.
#' @param province Character. Province/region. NULL to omit.
#' @param locality_name Character. Locality or site name. Not present in the
#'   OpenForis export — supply it here. NULL to omit.
#' @param quadrat_area Numeric. Area of one quadrat in hectares. Default
#'   \code{0.04} (a 20 x 20 m quadrat). \code{plots$plot_area} is this times
#'   the number of quadrats sampled in the plot. NULL to omit.
#' @param country Character. Overrides the country decoded from the export.
#'   NULL keeps the decoded value.
#' @param data_provider Character. Data provider (e.g. \code{"IRD"}).
#' @param principal_investigator Character. PI name(s).
#' @param data_manager Character. Data manager name(s).
#' @param additional_people Character. Overrides the \code{add_people} column
#'   from the export. NULL keeps the exported value.
#' @param census Integer. Census number, written to \code{plots$census} and to
#'   \code{individuals_wide$census_id} (default \code{1} — a small-tree
#'   inventory is a first census even though the parent plot is older).
#' @param plot_name_digits Integer. Number of digits the numeric suffix of a
#'   plot name is zero-padded to, so that the two- and three-digit spellings
#'   found in the raw files (\code{"mbalmayo01"}, \code{"mbalmayo010"}) resolve
#'   to the one the database uses (\code{"mbalmayo001"}, \code{"mbalmayo010"}).
#'   Default \code{3}, which is the convention the OpenForis form itself states
#'   for this field — \emph{"using the following format: Mbalmay005 or
#'   Somalomo010"} — and not merely what the exports happen to contain. NULL
#'   disables padding.
#' @param plot_name_map Named character vector of explicit plot-name
#'   replacements, applied to the raw value before padding and winning over it
#'   (e.g. \code{c("Mbalmayo-09" = "mbalmayo009")}). NULL for none.
#' @param plot_suffix Character appended to the parent plot name to name the
#'   small-tree plot (default \code{"_smalltree"}). It may not end with a
#'   digit, which \code{.normalise_plot_name()} would take for the plot number.
#' @param quadrat_sep Character joining the parts of a quadrat identifier: the
#'   parent plot name and the quadrat label in the review tables
#'   (\code{"mbalmayo001_20_20"}), and the quadrat label and the sub-quadrat in
#'   the \code{quadrat} feature (\code{"20_20_A"}). Default \code{"_"}.
#' @param quadrat_decisions The reviewed \code{name_mismatches} table, as a
#'   data frame or the path of an xlsx, or NULL to keep the tag quadrat for
#'   every stem. Needs \code{parent_plot_name}, \code{tag} and \code{keep}
#'   (\code{"tag"} or \code{"name"}; blank counts as \code{"tag"}), and uses
#'   \code{quadrat_by_name} when present, plus \code{plot_name_raw} to tell
#'   apart two stems sharing a tag. A relative path is taken inside
#'   \code{data_dir} when it is set. See the section "Deciding a mismatch".
#' @param mismatch_file Path of an xlsx to write \code{name_mismatches} to, for
#'   review, or NULL. A relative path is taken inside \code{data_dir} when it
#'   is set. Nothing is written when there is no mismatch. Output only: to
#'   apply an edited file, pass it as \code{quadrat_decisions} (both may name
#'   the same file, which is then rewritten with what was applied).
#' @param dbh_max Numeric. Diameter above which a stem is reported as outside
#'   the small-tree protocol (default \code{10}). NULL to skip the check.
#' @param specimen_prefix Character prefix for specimen numbers (e.g.
#'   \code{"PIRD"}). NULL leaves the numbers as-is. A prefix the field team
#'   already typed into the form is not repeated — the OpenForis
#'   \code{specimen_name} is calculated as \code{colnam} plus the number, so it
#'   usually arrives prefixed already, and inconsistently cased.
#' @param specimen_remap_file Path or filename of an xlsx whose first column
#'   holds the specimen number as recorded and second column its replacement;
#'   further columns are ignored. A bare filename is looked up in
#'   \code{data_dir}. Both \code{specimen_number} and \code{herbarium_nbe_char}
#'   are substituted and the values as recorded kept in
#'   \code{specimen_number_original} and \code{herbarium_nbe_char_original}.
#'   Numbers are matched as written and then, for whatever is left, on their
#'   digits alone with the prefix kept — so a table keyed on bare numbers
#'   reaches \code{"Pird 1"} as well as \code{1}. The function stops if either
#'   column of the table repeats a number. NULL (default) skips remapping.
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
#' @param plot_name_col Column of the tree file naming the parent plot
#'   (default \code{"plot_plot_name_old"}). It is the raw field entry and, once
#'   normalised, resolves the parent plot more reliably than the tidied
#'   \code{plot_plot_name}, which mixes two- and three-digit spellings.
#' @param tag_col Column name for the tree tag (default \code{"tag"}).
#'
#' @return A list with components:
#' \describe{
#'   \item{plots}{Tibble, one row per small-tree plot (one per parent plot),
#'     ready to upload to the Import Wizard as inventory metadata: plot_name
#'     (\code{<parent><plot_suffix>}), parent_plot_name, parent_relation,
#'     quadrats (the sampled quadrat labels), country, province, method,
#'     locality_name, plot_area, data_provider, team_leader,
#'     principal_investigator, data_manager, additional_people, identified_by,
#'     forest_state, forest_type, date_y, date_m, date_d, census.}
#'   \item{quadrats}{Tibble, one row per sampled quadrat: the small-tree
#'     plot_name it belongs to, its quadrat_unit identifier
#'     (\code{<parent>_<quadrat>}), and the numbers used to assign stems to it
#'     — firsttag, the tag range actually observed, the stem count, and the
#'     raw \code{plot_plot_name_old} spellings that fell in the range. A review
#'     table, not an upload.}
#'   \item{individuals}{Tibble of every stem: plot_name (the small-tree plot),
#'     parent_plot_name, tag, quadrat (the 20 x 20 m quadrat and the
#'     10 x 10 m sub-quadrat, e.g. \code{"20_20_A"}),
#'     original_tax_name, idtax_n, tax_appendix, herbarium_nbe_char,
#'     herbarium_nbe_type, position_x, position_y, multi_stem,
#'     number_multi_stem, multi_tiges_id (tag of the main stem, NA for the main
#'     stem itself). The two position columns are part of the form but the
#'     small-tree protocol does not map stems, so in practice they arrive
#'     empty. \code{herbarium_nbe_char} is repeated on every individual
#'     identified as the species of a voucher, while \code{herbarium_nbe_type}
#'     names each specimen once, on the individual it was collected from.
#'     \code{idtax_n} is copied from the OpenForis \code{species_code}
#'     without being checked against the taxonomic backbone — see the note
#'     below.}
#'   \item{individuals_wide}{The same stems with one column per trait and a
#'     \code{census_id} column — the flat table the Import Wizard expects.}
#'   \item{measurements}{Tibble in long format (plot_name, tag, trait_name,
#'     traitvalue, traitvalue_char) covering stem_diameter,
#'     height_of_stem_diameter, light, observation (observations, phenology and
#'     free-text comments) and pom_observation. Stems with no value for a given
#'     trait produce no row.}
#'   \item{specimens}{Tibble ready for \code{add_specimens()}, one row per
#'     unique voucher, or NULL.}
#'   \item{multi_stems}{Tibble of candidate multi-stem groupings with a
#'     \code{flag} column to review, or NULL.}
#'   \item{name_mismatches}{Tibble of stems whose \code{plot_plot_name_old}
#'     spelling puts them in a different quadrat than their tag does, or
#'     NULL: plot_name_raw, parent_plot_name, plot_name, tag,
#'     species_scientific_name, quadrat_by_tag, quadrat_by_name and keep (the
#'     side used, \code{"tag"} unless \code{quadrat_decisions} says
#'     otherwise). Edit \code{keep} and pass it back as
#'     \code{quadrat_decisions}.}
#'   \item{unassigned_stems}{Tibble of stems that could not be placed in any
#'     quadrat — an unknown parent plot, a missing tag, or a tag below every
#'     \code{firsttag} of its plot — or NULL. A \code{warning()} is also
#'     raised. These stems are excluded from every other table.}
#'   \item{duplicated_stems}{Tibble of rows sharing a plot + tag pair, or
#'     NULL. A \code{warning()} is also raised.}
#'   \item{oversized_stems}{Tibble of stems at or above \code{dbh_max}, or
#'     NULL. They belong to the large-stem census of the parent plot, not here.}
#'   \item{summary}{List of counts.}
#' }
#'
#' @section Taxonomy:
#' The OpenForis \code{species_code} is written to \code{idtax_n} as-is, on the
#' assumption that the form was built against the same taxonomic backbone as
#' the database. Nothing in this function verifies that, while the Import
#' Wizard requires a valid, non-empty \code{idtax_n} for every individual —
#' so run \code{\link{launch_taxonomic_match_app}} on the result before
#' importing. A warning is raised as a reminder.
#'
#' @examples
#' \dontrun{
#' result <- process_openforis_small_trees(
#'   data_dir  = "path/to/mission/plot/small_tree/",
#'   codes_dir = "path/to/openforis/",
#'   method = "small-tree quadrat",
#'   province = "Centre",
#'   locality_name = "Mbalmayo",
#'   data_provider = "IRD",
#'   principal_investigator = "Jane Doe",
#'   data_manager = "John Smith",
#'   specimen_prefix = "PIRD"
#' )
#'
#' # Always review these before importing
#' result$quadrats
#' result$name_mismatches
#' result$unassigned_stems
#'
#' # Settle the quadrat of the disputed stems: write the mismatches, set
#' # keep = "name" where the name is right, and run again with the file
#' result <- process_openforis_small_trees(
#'   data_dir  = "path/to/mission/plot/small_tree/",
#'   codes_dir = "path/to/openforis/",
#'   mismatch_file = "mismatches.xlsx"
#' )
#' # ... edit mismatches.xlsx ...
#' result <- process_openforis_small_trees(
#'   data_dir  = "path/to/mission/plot/small_tree/",
#'   codes_dir = "path/to/openforis/",
#'   quadrat_decisions = "mismatches.xlsx"
#' )
#'
#' # One file per Import Wizard upload
#' export_openforis_for_wizard(result, dir = "to_import")
#'
#' launch_import_wizard()
#' }
#'
#' @seealso \code{\link{process_openforis_new_plot}} for a newly established
#'   1-ha plot, \code{\link{process_openforis_census}} for re-measurement
#'   exports; \code{export_openforis_for_wizard()} to write the upload files.
#' @keywords internal
process_openforis_small_trees <- function(data_dir = NULL,
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
                                          subquadrat_codes = NULL,
                                          morpho_codes = NULL,
                                          forest_state_codes = NULL,
                                          forest_type_codes = NULL,
                                          country_codes = NULL,
                                          team_leader_codes = NULL,
                                          method = NULL,
                                          province = NULL,
                                          locality_name = NULL,
                                          quadrat_area = 0.04,
                                          country = NULL,
                                          data_provider = NULL,
                                          principal_investigator = NULL,
                                          data_manager = NULL,
                                          additional_people = NULL,
                                          census = 1,
                                          plot_name_digits = 3L,
                                          plot_name_map = NULL,
                                          plot_suffix = "_smalltree",
                                          quadrat_sep = "_",
                                          quadrat_decisions = NULL,
                                          mismatch_file = NULL,
                                          dbh_max = 10,
                                          specimen_prefix = NULL,
                                          specimen_remap_file = NULL,
                                          specimen_locality = NULL,
                                          specimen_country = NULL,
                                          specimen_col_month = NULL,
                                          specimen_col_year = NULL,
                                          specimen_collector = NULL,
                                          specimen_description_col = "stem_diameter",
                                          plot_name_col = "plot_plot_name_old",
                                          tag_col = "tag") {

  .check_small_tree_plot_suffix(plot_suffix)

  # ---- Auto-detect files from directories ----
  if (is.null(tree_file) && !is.null(data_dir)) {
    tree_file <- .find_file_in_dir(data_dir, tree_file_pattern, ext = "xlsx",
                                   label = "tree list")
  }
  if (is.null(plot_file) && !is.null(data_dir)) {
    plot_file <- .find_file_in_dir(data_dir, plot_file_pattern, ext = "xlsx",
                                   label = "plot data")
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
    if (is.null(subquadrat_codes))
      subquadrat_codes <- .find_code_file(
        codes_dir, c("code_subquadrat*", "code_sub_quadrat*"),
        "sub-quadrat codes")
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
  for (nm in c("observation_codes", "pom_codes", "light_codes", "pheno_codes",
               "quadrat_codes", "subquadrat_codes", "morpho_codes",
               "forest_state_codes", "forest_type_codes", "country_codes",
               "team_leader_codes")) {
    if (identical(get(nm), FALSE)) assign(nm, NULL)
  }

  if (is.null(tree_file)) {
    stop("tree_file is required. Provide it directly or set data_dir to auto-detect.")
  }
  if (is.null(plot_file)) {
    stop(paste("plot_file is required for a small-tree export: it carries the",
               "quadrat of each unit and the firsttag that assigns stems to it.",
               "Provide it directly or set data_dir to auto-detect."))
  }

  # ---- Read code lists ----
  obs_code_list     <- .read_code_csv(observation_codes)
  pom_code_list     <- .read_code_csv(pom_codes)
  light_code_list   <- .read_code_csv(light_codes)
  pheno_code_list   <- .read_code_csv(pheno_codes)
  quad_code_list    <- .read_code_csv(quadrat_codes)
  subquad_code_list <- .read_code_csv(subquadrat_codes)
  morpho_code_list  <- .read_code_csv(morpho_codes)
  fstate_code_list  <- .read_code_csv(forest_state_codes)
  ftype_code_list   <- .read_code_csv(forest_type_codes)
  country_code_list <- .read_code_csv(country_codes)
  tl_code_list      <- .read_code_csv(team_leader_codes)

  # ---- Quadrat units (pooled into one plot per parent plot) ----
  cli::cli_alert_info("Reading quadrat metadata from {.file {plot_file}}")
  plots_raw <- as.data.frame(readxl::read_excel(plot_file))

  quadrats <- .prepare_openforis_small_tree_quadrats(
    plots_raw,
    quadrat_codes = quad_code_list,
    plot_name_digits = plot_name_digits,
    plot_name_map = plot_name_map,
    quadrat_sep = quadrat_sep,
    plot_suffix = plot_suffix
  )
  cli::cli_alert_success(
    "{nrow(quadrats)} sampled quadrat{?s} across {length(unique(quadrats$parent_plot_name))} parent plot{?s}"
  )

  # ---- Read and normalise the tree list ----
  cli::cli_alert_info("Reading tree list from {.file {tree_file}}")
  trees_raw <- as.data.frame(readxl::read_excel(tree_file, guess_max = 5000))
  cli::cli_alert_success("Read {nrow(trees_raw)} stem record{?s}")

  trees <- .normalise_openforis_small_tree_trees(
    trees_raw, plot_name_col, tag_col,
    plot_name_digits = plot_name_digits,
    plot_name_map = plot_name_map
  )

  # ---- Assign each stem to a quadrat by tag range ----
  assignment <- .assign_small_tree_quadrats(trees, quadrats)
  trees <- assignment$trees
  unassigned_stems <- assignment$unassigned
  name_mismatches <- assignment$mismatches

  if (!is.null(unassigned_stems)) {
    warning(sprintf(
      "%d stem(s) could not be assigned to a quadrat and are excluded. Check result$unassigned_stems.",
      nrow(unassigned_stems)
    ), call. = FALSE)
    cli::cli_alert_warning(
      "{nrow(unassigned_stems)} stem{?s} left unassigned — review result$unassigned_stems"
    )
  }
  # ---- The processor's decisions on those mismatches ----
  # Before anything reads the quadrat of a stem: the individuals' quadrat
  # feature, the review table and the per-quadrat multi-stem grouping.
  if (!is.null(quadrat_decisions)) {
    decided <- .apply_small_tree_quadrat_decisions(
      trees, name_mismatches, quadrat_decisions, quadrats,
      data_dir = data_dir
    )
    trees <- decided$trees
    name_mismatches <- decided$mismatches
  }

  if (!is.null(name_mismatches)) {
    n_by_name <- sum(name_mismatches$keep == "name")
    n_by_tag <- nrow(name_mismatches) - n_by_name
    if (is.null(quadrat_decisions)) {
      cli::cli_alert_warning(paste(
        "{nrow(name_mismatches)} stem{?s} whose {.field {plot_name_col}} spelling",
        "disagrees with the quadrat its tag places it in — the tag was used.",
        "Review result$name_mismatches, set {.field keep} to {.val name} where",
        "the name is right and pass it back as {.arg quadrat_decisions}"
      ))
    } else {
      cli::cli_alert_info(paste(
        "Quadrat mismatches: {n_by_name} stem{?s} placed by name,",
        "{n_by_tag} by tag"
      ))
    }
    if (!is.null(mismatch_file)) {
      mismatch_file <- .resolve_in_data_dir(mismatch_file, data_dir)
      writexl::write_xlsx(dplyr::as_tibble(name_mismatches), mismatch_file)
      cli::cli_alert_info("Mismatches written to {.file {mismatch_file}}")
    }
  }
  if (nrow(trees) == 0) {
    stop("No stem could be assigned to a quadrat — check the firsttag column of the plot file.")
  }
  cli::cli_alert_success(paste(
    "Assigned {nrow(trees)} stem{?s} to {length(unique(trees$quadrat_unit))}",
    "quadrat{?s} of {length(unique(trees$plot_name))} small-tree plot{?s}"
  ))

  # ---- Quadrats seen in one file but not the other ----
  empty_quadrats <- setdiff(quadrats$quadrat_unit, trees$quadrat_unit)
  if (length(empty_quadrats) > 0) {
    cli::cli_alert_warning(
      "Quadrat{?s} in the plot file with no stem in the tree list: {.val {empty_quadrats}}"
    )
  }
  orphan_plots <- setdiff(unique(trees$parent_plot_name), quadrats$parent_plot_name)
  if (length(orphan_plots) > 0) {
    cli::cli_alert_warning(
      "Parent plot{?s} in the tree list absent from the plot file: {.val {orphan_plots}}"
    )
  }

  # ---- Duplicated plot + tag combinations ----
  dup_key <- paste(trees$plot_name, trees$tag, sep = "__")
  dup_mask <- duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)
  duplicated_stems <- NULL
  if (any(dup_mask)) {
    dup_cols <- intersect(
      c("plot_name", "parent_plot_name", "quadrat_unit", "tag",
        "species_scientific_name", "species_code", "stem_diameter", "quadrat"),
      names(trees)
    )
    duplicated_stems <- trees[dup_mask, dup_cols, drop = FALSE]
    duplicated_stems <- duplicated_stems[order(duplicated_stems$plot_name,
                                               duplicated_stems$tag), ]
    rownames(duplicated_stems) <- NULL
    n_dup_pairs <- length(unique(dup_key[dup_mask]))
    warning(sprintf(
      "%d duplicated plot + tag combination(s) found (%d rows). Check result$duplicated_stems.",
      n_dup_pairs, sum(dup_mask)
    ), call. = FALSE)
    cli::cli_alert_warning(
      "{n_dup_pairs} duplicated plot + tag combination(s) detected ({sum(dup_mask)} rows) — review result$duplicated_stems"
    )
  }

  # ---- Stems outside the small-tree diameter range ----
  oversized_stems <- .flag_oversized_small_trees(trees, dbh_max)

  # ---- Recorded fields with nowhere to go ----
  .warn_unused_openforis_columns(trees)

  # ---- Specimen number remapping ----
  # Before the individuals and specimens are built: both read the voucher
  # columns straight off `trees`, so remapping here reaches every table at once.
  if (!is.null(specimen_remap_file)) {
    trees <- .remap_specimen_numbers(trees, specimen_remap_file, data_dir)
  }

  # ---- Plot-level table, one row per small-tree plot ----
  plots <- .prepare_openforis_small_tree_plots(
    quadrats, plots_raw,
    country_codes = country_code_list,
    forest_state_codes = fstate_code_list,
    forest_type_codes = ftype_code_list,
    team_leader_codes = tl_code_list,
    country = country, province = province, method = method,
    data_provider = data_provider,
    locality_name = locality_name, plot_area = quadrat_area,
    principal_investigator = principal_investigator,
    data_manager = data_manager,
    additional_people = additional_people,
    census = census
  )
  cli::cli_alert_success("Plot metadata for {nrow(plots)} small-tree plot{?s}")

  # plot_name, method and country are required by the Import Wizard
  missing_req <- Filter(
    function(cl) !cl %in% names(plots) || all(is.na(plots[[cl]])),
    c("method", "country")
  )
  if (length(missing_req) > 0) {
    cli::cli_alert_warning(paste(
      "Plot table has no {.field {missing_req}} — the Import Wizard requires",
      "plot_name, method and country. Pass {.arg {missing_req}} to fill {?it/them} in."
    ))
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

  # ---- Individuals ----
  # The tree-level `quadrat` is the 10 x 10 m sub-quadrat (A-D), so it is
  # decoded against the sub-quadrat list, not the 20 x 20 m one. The 20 x 20 m
  # quadrat is then put in front of it: once the quadrats of a plot are pooled,
  # the individual is the only place left that says which one a stem was in.
  individuals <- .prepare_openforis_new_plot_individuals(
    trees, specimen_prefix,
    quadrat_codes = subquad_code_list,
    morpho_codes = morpho_code_list
  )
  # Row for row, not looked up by plot + tag: a tag used in two quadrats is
  # two stems, each with its own quadrat. The individuals are `trees` in the
  # stable order(plot_name, tag) that function sorts by.
  stem_idx <- order(trees$plot_name, trees$tag)
  stopifnot(nrow(individuals) == length(stem_idx),
            identical(individuals$tag, trees$tag[stem_idx]))
  individuals$quadrat <- .combine_small_tree_quadrat(
    trees$quadrat_label[stem_idx], individuals$quadrat, sep = quadrat_sep
  )
  individuals$parent_plot_name <- trees$parent_plot_name[stem_idx]
  individuals <- individuals[
    , union(c("plot_name", "parent_plot_name", "tag"), names(individuals)),
    drop = FALSE
  ]
  cli::cli_alert_success("Prepared {nrow(individuals)} individual{?s}")

  # species_code is assumed to be an idtax_n, but nothing here verifies it
  if ("idtax_n" %in% names(individuals)) {
    cli::cli_alert_warning(paste(
      "{.field idtax_n} is taken straight from the OpenForis",
      "{.field species_code} and is not checked against the taxonomic",
      "backbone — verify it with {.fn launch_taxonomic_match_app} before importing."
    ))
    n_no_tax <- sum(is.na(individuals$idtax_n))
    if (n_no_tax > 0) {
      cli::cli_alert_warning(
        "{n_no_tax} individual{?s} ha{?s/ve} no {.field idtax_n} — the Import Wizard rejects empty values"
      )
    }
  }

  # ---- Measurements (long format) ----
  measurement_parts <- list()

  # position_x / position_y are part of the form but were never filled for
  # small trees; .extract_numeric_traits() drops a column that is all NA.
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
  # Grouping takes the next tags of the same plot. It runs per quadrat, so the
  # last stem of one quadrat cannot be swept into a group opened at the end of
  # the previous one - the field protocol keeps a multi-stem tree in one
  # quadrat - and the result is then put back under the small-tree plot.
  trees_by_quadrat <- trees
  trees_by_quadrat$plot_name <- trees$quadrat_unit
  multi_stems <- .build_openforis_multi_stems(trees_by_quadrat)
  if (!is.null(multi_stems)) {
    multi_stems$plot_name <- trees$plot_name[
      match(multi_stems$plot_name, trees$quadrat_unit)
    ]
    n_groups <- length(unique(paste(multi_stems$plot_name, multi_stems$group_tag)))
    n_flagged <- sum(!is.na(multi_stems$flag))
    cli::cli_alert_success(
      "Detected {n_groups} multi-stem group{?s} ({nrow(multi_stems)} stems total)"
    )
    if (n_flagged > 0) {
      cli::cli_alert_warning("{n_flagged} stem{?s} flagged — review recommended")
    }

    # The Import Wizard has no multi-stem step, so carry the link as a column
    individuals$multi_tiges_id <- .build_multi_tiges_id(individuals, multi_stems)
    n_secondary <- sum(!is.na(individuals$multi_tiges_id))
    cli::cli_alert_info(
      "{n_secondary} stem{?s} flagged as secondary via {.field multi_tiges_id}"
    )
  }

  # ---- Specimens ----
  spec_collectors <- .build_openforis_collector_team(plots)

  if (is.null(spec_collectors) && !is.null(additional_people)) {
    spec_collectors <- .collapse_people(additional_people)
    cli::cli_alert_info(
      "Using the {.arg additional_people} argument as specimen collecting team"
    )
  }

  specimens <- .prepare_openforis_specimens(
    trees, plots, specimen_prefix,
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
    uniq_team <- if (!is.null(additional_people)) {
      .collapse_people(additional_people)
    } else {
      unique(stats::na.omit(spec_collectors$additional_collector))
    }
    if (length(uniq_team) == 1 && !is.na(uniq_team)) {
      specimens$additional_collector[is.na(specimens$additional_collector)] <- uniq_team
      cli::cli_alert_info(
        "No plot-file match for {.val {orphans}}; applied a single collecting team"
      )
    } else {
      cli::cli_alert_warning(
        "No collecting team for specimens of {.val {orphans}} — quadrat absent from the plot table"
      )
    }
  }
  if (!is.null(specimens)) {
    cli::cli_alert_success("Prepared {nrow(specimens)} specimen{?s}")
  }
  .check_openforis_voucher_flag(trees, specimens)

  # ---- Flat individuals table for the Import Wizard ----
  individuals_wide <- .build_openforis_individuals_wide(
    individuals, measurements, census = census
  )
  cli::cli_alert_success(
    "Built flat individuals table: {ncol(individuals_wide)} columns for the Import Wizard"
  )

  # ---- Observed tag ranges, for the quadrat review table ----
  quadrats <- .summarise_small_tree_quadrats(quadrats, trees, plot_name_col)

  # ---- Tibbles ----
  as_tbl <- function(x) if (!is.null(x)) dplyr::as_tibble(x) else NULL
  plots            <- as_tbl(plots)
  quadrats         <- as_tbl(quadrats)
  individuals      <- as_tbl(individuals)
  individuals_wide <- as_tbl(individuals_wide)
  measurements     <- as_tbl(measurements)
  specimens        <- as_tbl(specimens)
  multi_stems      <- as_tbl(multi_stems)
  name_mismatches  <- as_tbl(name_mismatches)
  unassigned_stems <- as_tbl(unassigned_stems)
  duplicated_stems <- as_tbl(duplicated_stems)
  oversized_stems  <- as_tbl(oversized_stems)

  # ---- Summary ----
  summary_info <- list(
    n_plots = nrow(plots),
    n_parent_plots = length(unique(trees$parent_plot_name)),
    n_quadrats = nrow(quadrats),
    n_individuals = nrow(individuals),
    n_measurements = nrow(measurements),
    n_specimens = if (!is.null(specimens)) nrow(specimens) else 0L,
    n_multi_stem_groups = if (!is.null(multi_stems)) {
      length(unique(paste(multi_stems$plot_name, multi_stems$group_tag)))
    } else 0L,
    n_unassigned_stems = if (!is.null(unassigned_stems)) nrow(unassigned_stems) else 0L,
    n_name_mismatches = if (!is.null(name_mismatches)) nrow(name_mismatches) else 0L,
    n_placed_by_name = if (!is.null(name_mismatches)) {
      sum(name_mismatches$keep == "name")
    } else 0L,
    n_duplicated_stems = if (!is.null(duplicated_stems)) nrow(duplicated_stems) else 0L,
    n_oversized_stems = if (!is.null(oversized_stems)) nrow(oversized_stems) else 0L,
    trait_names = if (nrow(measurements) > 0) unique(measurements$trait_name) else character(0)
  )

  cli::cli_rule()
  cli::cli_alert_success("Processing complete:")
  cli::cli_bullets(c(
    "*" = "{summary_info$n_plots} small-tree plot(s) pooling {summary_info$n_quadrats} quadrat(s)",
    "*" = "{summary_info$n_individuals} individual(s)",
    "*" = "{summary_info$n_measurements} measurement rows",
    "*" = "{summary_info$n_specimens} specimen(s)",
    "*" = "{summary_info$n_multi_stem_groups} multi-stem group(s)",
    "*" = "{summary_info$n_unassigned_stems} unassigned stem(s)",
    "*" = "{summary_info$n_name_mismatches} plot-name mismatch(es), {summary_info$n_placed_by_name} placed by name",
    "*" = "{summary_info$n_duplicated_stems} duplicated stem row(s)",
    "*" = "{summary_info$n_oversized_stems} stem(s) at or above the small-tree threshold",
    "*" = "Traits: {paste(summary_info$trait_names, collapse = ', ')}"
  ))
  cli::cli_alert_info(paste(
    "Each small-tree plot is linked to its parent plot through",
    "{.field parent_plot_name} as a {.val nested_subsample};",
    "the parent must already be in the database."
  ))
  cli::cli_alert_info(
    "Plot coordinates are not part of the OpenForis export — add them separately."
  )

  list(
    plots = plots,
    quadrats = quadrats,
    individuals = individuals,
    individuals_wide = individuals_wide,
    measurements = measurements,
    specimens = specimens,
    multi_stems = multi_stems,
    name_mismatches = name_mismatches,
    unassigned_stems = unassigned_stems,
    duplicated_stems = duplicated_stems,
    oversized_stems = oversized_stems,
    summary = summary_info
  )
}


# ===========================================================================
# Internal helpers — small-tree specific
# ===========================================================================

#' Normalise a plot name to the spelling the database uses
#'
#' The raw exports spell one plot several ways — \code{"Mbalmayo1__"},
#' \code{"Mbalmayo-09"}, \code{"mbalmayo010_"} — because the field entry is
#' free text and the tidied column pads its numbers inconsistently. Stripping
#' every separator and zero-padding the trailing number to a fixed width
#' collapses them onto the database spelling (\code{"mbalmayo001"},
#' \code{"mbalmayo009"}, \code{"mbalmayo010"}).
#'
#' A number already at or above \code{digits} wide is left alone, so a plot
#' whose name genuinely carries more digits is not mangled.
#'
#' @param x Character vector of raw plot names.
#' @param digits Width the trailing number is padded to. NULL disables padding.
#' @param map Named character vector of explicit replacements, applied to the
#'   raw value and winning over the derived one. NULL for none.
#' @return Character vector of normalised names.
#' @keywords internal
.normalise_plot_name <- function(x, digits = 3L, map = NULL) {

  raw <- trimws(as.character(x))
  out <- tolower(gsub("[^A-Za-z0-9]", "", raw))

  if (!is.null(digits) && !is.na(digits)) {
    digits <- as.integer(digits)
    pos <- regexpr("[0-9]+$", out)
    has_num <- pos > 0
    if (any(has_num, na.rm = TRUE)) {
      idx <- which(has_num)
      num <- substring(out[idx], pos[idx])
      stem <- substring(out[idx], 1L, pos[idx] - 1L)
      # formatC(flag = "0") pads a character vector with spaces, not zeros
      short <- nchar(num) < digits
      num[short] <- paste0(strrep("0", digits - nchar(num[short])), num[short])
      out[idx] <- paste0(stem, num)
    }
  }

  # An explicit mapping is the last word, matched on the untouched value
  if (!is.null(map) && length(map) > 0) {
    hit <- match(raw, names(map))
    out[!is.na(hit)] <- unname(map)[hit[!is.na(hit)]]
  }

  out[is.na(raw)] <- NA_character_
  out
}


#' Build the sampled-quadrat units from a raw OpenForis small-tree plot export
#'
#' One row of \code{plot.xlsx} is one 20 x 20 m quadrat of a parent plot. The
#' quadrat code is decoded to its grid label (\code{9} to \code{"20_20"}),
#' which, appended to the normalised parent plot name, identifies the quadrat
#' (\code{quadrat_unit}). Every quadrat of a parent plot belongs to the same
#' small-tree plot, named after the parent plus \code{plot_suffix}.
#'
#' @param plots_raw Data frame read from \code{plot.xlsx}.
#' @param quadrat_codes Parsed 20 x 20 m quadrat code list, or NULL to keep the
#'   raw codes.
#' @param plot_name_digits,plot_name_map Passed to
#'   \code{\link{.normalise_plot_name}}.
#' @param quadrat_sep Separator between parent plot name and quadrat label.
#' @param plot_suffix Appended to the parent plot name to name the small-tree
#'   plot.
#' @return Data frame with one row per quadrat: row_id (the row of
#'   \code{plots_raw} it came from), parent_plot_name, plot_name, quadrat_unit,
#'   quadrat, quadrat_code, firsttag.
#' @keywords internal
.prepare_openforis_small_tree_quadrats <- function(plots_raw,
                                                   quadrat_codes = NULL,
                                                   plot_name_digits = 3L,
                                                   plot_name_map = NULL,
                                                   quadrat_sep = "_",
                                                   plot_suffix = "_smalltree") {

  required <- c("plot_name", "quadrat", "firsttag")
  missing <- setdiff(required, names(plots_raw))
  if (length(missing) > 0) {
    stop(sprintf("Column(s) %s not found in the plot file. Available: %s",
                 paste(missing, collapse = ", "),
                 paste(names(plots_raw), collapse = ", ")))
  }

  parent <- .normalise_plot_name(plots_raw$plot_name,
                                 digits = plot_name_digits,
                                 map = plot_name_map)
  renamed <- unique(stats::na.omit(
    paste0(trimws(as.character(plots_raw$plot_name)), " -> ", parent)
  ))
  renamed <- renamed[!grepl("^(.*) -> \\1$", renamed)]
  if (length(renamed) > 0) {
    cli::cli_alert_info("Plot name{?s} normalised: {.val {renamed}}")
  }

  quad_code <- suppressWarnings(as.numeric(plots_raw$quadrat))
  quad_label <- .decode_code_column(quad_code, quadrat_codes,
                                    "quadrat_code", "quadrat_label_en")
  # Keep the raw code when decoding is unavailable or failed
  quad_label <- ifelse(is.na(quad_label), as.character(plots_raw$quadrat),
                       quad_label)

  result <- data.frame(
    row_id = seq_len(nrow(plots_raw)),
    parent_plot_name = parent,
    plot_name = ifelse(is.na(parent), NA_character_, paste0(parent, plot_suffix)),
    quadrat_unit = ifelse(is.na(parent), NA_character_,
                          paste0(parent, quadrat_sep, quad_label)),
    quadrat = quad_label,
    quadrat_code = quad_code,
    firsttag = suppressWarnings(as.numeric(plots_raw$firsttag)),
    stringsAsFactors = FALSE
  )

  no_firsttag <- is.na(result$firsttag)
  if (any(no_firsttag)) {
    cli::cli_alert_warning(paste(
      "{sum(no_firsttag)} quadrat{?s} with no {.field firsttag}",
      "({.val {result$quadrat_unit[no_firsttag]}}) — no stem can be assigned to {?it/them}"
    ))
  }

  dup <- duplicated(result$quadrat_unit) |
    duplicated(result$quadrat_unit, fromLast = TRUE)
  if (any(dup)) {
    warning(sprintf(
      "The plot file names the same quadrat more than once: %s",
      paste(unique(result$quadrat_unit[dup]), collapse = ", ")
    ), call. = FALSE)
    cli::cli_alert_warning(
      "Repeated quadrat{?s} in the plot file: {.val {unique(result$quadrat_unit[dup])}}"
    )
  }

  result <- result[order(result$parent_plot_name, result$firsttag), , drop = FALSE]
  rownames(result) <- NULL
  result
}


#' Normalise raw OpenForis small-tree tree columns
#'
#' Applies the same renaming as the new-plot form — the two share their
#' measurement vocabulary — and adds \code{parent_plot_name} and a numeric
#' \code{tag}. \code{plot_name} is deliberately not set here: it is the quadrat
#' the stem belongs to, which only \code{.assign_small_tree_quadrats()} can
#' work out.
#'
#' @param trees_raw Data frame read from \code{tree_list.xlsx}.
#' @param plot_name_col Column holding the parent plot name.
#' @param tag_col Column holding the tag.
#' @param plot_name_digits,plot_name_map Passed to
#'   \code{\link{.normalise_plot_name}}.
#' @return Data frame with normalised column names, plus
#'   \code{parent_plot_name}, \code{plot_name_raw} and a numeric \code{tag}.
#' @keywords internal
.normalise_openforis_small_tree_trees <- function(trees_raw,
                                                  plot_name_col = "plot_plot_name_old",
                                                  tag_col = "tag",
                                                  plot_name_digits = 3L,
                                                  plot_name_map = NULL) {

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

  trees$plot_name_raw <- as.character(trees[[plot_name_col]])
  trees$parent_plot_name <- .normalise_plot_name(trees$plot_name_raw,
                                                 digits = plot_name_digits,
                                                 map = plot_name_map)
  trees$tag <- suppressWarnings(as.numeric(trees[[tag_col]]))

  trees
}


#' Assign each small-tree stem to a sampled quadrat by tag range
#'
#' Within a parent plot the quadrats were tagged one after another, so
#' \code{firsttag} cuts the tag sequence into disjoint ranges: a stem belongs
#' to the quadrat with the largest \code{firsttag} not above its tag. This is
#' the only key the two files genuinely share — \code{plot_plot_name_old}
#' distinguishes quadrats by a hand-typed trailing underscore, which the teams
#' do not apply consistently.
#'
#' That column is still worth something as a cross-check: every stem sharing a
#' raw spelling ought to land in the same quadrat. Where they do not, the
#' minority rows are reported and the tag is believed.
#'
#' @param trees Normalised tree data frame (parent_plot_name, tag,
#'   plot_name_raw).
#' @param quadrats Quadrat units from
#'   \code{.prepare_openforis_small_tree_quadrats()}.
#' @return List with \code{trees} (assignable rows, with \code{quadrat_unit},
#'   \code{quadrat_label} and the small-tree \code{plot_name} set),
#'   \code{unassigned} (rows dropped, or NULL) and \code{mismatches} (rows the
#'   raw spelling disagrees about, or NULL).
#' @keywords internal
.assign_small_tree_quadrats <- function(trees, quadrats) {

  n <- nrow(trees)
  assigned <- rep(NA_character_, n)
  reason <- rep(NA_character_, n)

  known_parent <- trees$parent_plot_name %in% quadrats$parent_plot_name
  reason[!known_parent] <- "parent plot absent from the plot file"
  reason[is.na(trees$tag) & is.na(reason)] <- "missing tag"

  for (parent in unique(trees$parent_plot_name[known_parent])) {
    q <- quadrats[quadrats$parent_plot_name == parent &
                    !is.na(quadrats$firsttag), , drop = FALSE]
    if (nrow(q) == 0) next
    q <- q[order(q$firsttag), , drop = FALSE]

    rows <- which(trees$parent_plot_name == parent & !is.na(trees$tag))
    if (length(rows) == 0) next

    # findInterval() returns 0 for a tag below every firsttag
    k <- findInterval(trees$tag[rows], q$firsttag)
    placed <- k >= 1L
    assigned[rows[placed]] <- q$quadrat_unit[k[placed]]
    reason[rows[!placed]] <- sprintf("tag below the first firsttag of %s (%s)",
                                     parent, min(q$firsttag))
  }

  reason[is.na(assigned) & is.na(reason)] <-
    "no quadrat of this plot has a firsttag"

  keep <- !is.na(assigned)

  unassigned <- NULL
  if (any(!keep)) {
    cols <- intersect(
      c("plot_name_raw", "parent_plot_name", "tag", "quadrat",
        "species_scientific_name", "stem_diameter"),
      names(trees)
    )
    unassigned <- trees[!keep, cols, drop = FALSE]
    unassigned$reason <- reason[!keep]
    rownames(unassigned) <- NULL
  }

  unit_idx <- match(assigned, quadrats$quadrat_unit)
  trees$quadrat_unit <- assigned
  trees$quadrat_label <- quadrats$quadrat[unit_idx]
  trees$plot_name <- quadrats$plot_name[unit_idx]
  trees <- trees[keep, , drop = FALSE]
  rownames(trees) <- NULL

  mismatches <- .flag_small_tree_name_mismatches(trees)

  list(trees = trees, unassigned = unassigned, mismatches = mismatches)
}


#' Report stems whose raw plot spelling disagrees with their tag range
#'
#' Every stem carrying the same \code{plot_plot_name_old} value was entered as
#' one quadrat in the field, so they should all land in the same quadrat once
#' assigned by tag. Where a spelling straddles two quadrats, the rows in the
#' smaller share are the suspicious ones — either the tag or the trailing
#' underscore was mistyped.
#'
#' The table is laid out to be edited and handed back as
#' \code{quadrat_decisions}: quadrats are named by their label, which is what
#' a person types, and \code{keep} says which side is used.
#'
#' @param trees Assigned tree data frame (quadrat_unit, quadrat_label,
#'   plot_name_raw).
#' @return Data frame of the minority rows, with the quadrat their tag puts
#'   them in (\code{quadrat_by_tag}), the one most stems of that spelling
#'   landed in (\code{quadrat_by_name}) and \code{keep = "tag"}, or NULL when
#'   every spelling is consistent.
#' @keywords internal
.flag_small_tree_name_mismatches <- function(trees) {

  if (!"plot_name_raw" %in% names(trees) || nrow(trees) == 0) return(NULL)

  cols <- intersect(
    c("plot_name_raw", "parent_plot_name", "plot_name", "tag",
      "species_scientific_name"),
    names(trees)
  )

  out <- list()
  for (raw in unique(trees$plot_name_raw)) {
    rows <- which(trees$plot_name_raw == raw)
    tab <- table(trees$quadrat_unit[rows])
    if (length(tab) < 2) next

    majority <- names(tab)[which.max(tab)]
    odd <- rows[trees$quadrat_unit[rows] != majority]

    piece <- trees[odd, cols, drop = FALSE]
    piece$quadrat_by_tag <- trees$quadrat_label[odd]
    piece$quadrat_by_name <-
      trees$quadrat_label[rows[match(majority, trees$quadrat_unit[rows])]]
    piece$keep <- "tag"
    out[[raw]] <- piece
  }

  if (length(out) == 0) return(NULL)
  result <- do.call(rbind, out)
  result <- result[order(result$parent_plot_name, result$tag), , drop = FALSE]
  rownames(result) <- NULL
  result
}


#' Apply the processor's decisions on quadrat mismatches
#'
#' Moves the stems marked \code{keep = "name"} to the quadrat given by
#' \code{quadrat_by_name}, the one taken from the mismatch table unless the
#' decision row names another. A decision is matched to a stem on parent
#' plot and tag, so it survives a re-export as long as those do.
#'
#' Anything that cannot be applied as written is an error, not a skip: a
#' decision for a stem that is not there, a quadrat that was not sampled in
#' that parent plot, a \code{"name"} without a quadrat, or two decisions for
#' one stem. A silently dropped decision would put a stem back in the quadrat
#' someone had taken it out of.
#'
#' @param trees Assigned tree data frame (parent_plot_name, tag, quadrat_unit,
#'   quadrat_label).
#' @param mismatches Output of \code{.flag_small_tree_name_mismatches()}, or
#'   NULL.
#' @param decisions Data frame, or path of an xlsx, with parent_plot_name, tag,
#'   keep and optionally quadrat_by_name.
#' @param quadrats Quadrat units from
#'   \code{.prepare_openforis_small_tree_quadrats()}.
#' @param data_dir Directory a bare filename in \code{decisions} is looked up
#'   in, or NULL.
#' @return List with \code{trees} (stems moved) and \code{mismatches} (keep
#'   and quadrat_by_name set to what was applied; stems moved by a decision
#'   but not flagged are appended).
#' @keywords internal
.apply_small_tree_quadrat_decisions <- function(trees, mismatches, decisions,
                                                quadrats, data_dir = NULL) {

  if (is.character(decisions)) {
    path <- .resolve_in_data_dir(decisions, data_dir)
    if (!file.exists(path)) {
      stop("quadrat_decisions file not found: ", path, call. = FALSE)
    }
    decisions <- readxl::read_excel(path)
  }
  decisions <- as.data.frame(decisions, stringsAsFactors = FALSE)

  missing_cols <- setdiff(c("parent_plot_name", "tag", "keep"), names(decisions))
  if (length(missing_cols) > 0) {
    stop("quadrat_decisions lacks column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (nrow(decisions) == 0) {
    return(list(trees = trees, mismatches = mismatches))
  }

  parent <- trimws(as.character(decisions$parent_plot_name))
  tag <- suppressWarnings(as.numeric(decisions$tag))
  keep <- tolower(trimws(as.character(decisions$keep)))
  keep[is.na(keep) | keep == ""] <- "tag"
  wanted <- if ("quadrat_by_name" %in% names(decisions)) {
    trimws(as.character(decisions$quadrat_by_name))
  } else {
    rep(NA_character_, nrow(decisions))
  }
  wanted[!is.na(wanted) & wanted == ""] <- NA_character_
  row_label <- sprintf("%s tag %s", parent, decisions$tag)

  bad_keep <- !keep %in% c("tag", "name")
  if (any(bad_keep)) {
    stop("quadrat_decisions: keep must be \"tag\" or \"name\"; got ",
         paste(sprintf("\"%s\" (%s)", decisions$keep[bad_keep],
                       row_label[bad_keep]), collapse = ", "),
         call. = FALSE)
  }

  # A decision is matched to its stem on parent plot and tag. A tag typed
  # twice in one parent plot (a quadrat's numbering run on past the next
  # one's firsttag) names two stems; the raw spelling, which the mismatch
  # table carries, then tells them apart.
  base_tree_key <- paste(trees$parent_plot_name, trees$tag, sep = "\r")
  tree_raw <- if ("plot_name_raw" %in% names(trees)) {
    trimws(as.character(trees$plot_name_raw))
  } else rep(NA_character_, nrow(trees))
  find_stems <- function(p, t, r) {
    rows <- which(base_tree_key == paste(p, t, sep = "\r"))
    if (length(rows) > 1 && !is.na(r)) rows <- rows[tree_raw[rows] %in% r]
    rows
  }
  raw_of <- function(df) {
    if ("plot_name_raw" %in% names(df)) {
      trimws(as.character(df$plot_name_raw))
    } else rep(NA_character_, nrow(df))
  }

  dec_raw <- raw_of(decisions)
  matched <- lapply(seq_along(parent),
                    function(i) find_stems(parent[i], tag[i], dec_raw[i]))
  n_stems <- lengths(matched)
  if (any(n_stems == 0)) {
    stop("quadrat_decisions names stem(s) absent from the tree list: ",
         paste(row_label[n_stems == 0], collapse = ", "), call. = FALSE)
  }
  if (any(n_stems > 1)) {
    stop("quadrat_decisions names a tag that two stems of the plot share; ",
         "add plot_name_raw to tell them apart: ",
         paste(row_label[n_stems > 1], collapse = ", "), call. = FALSE)
  }
  stem_of <- unlist(matched)

  dup <- duplicated(stem_of) | duplicated(stem_of, fromLast = TRUE)
  if (any(dup)) {
    stop("quadrat_decisions has more than one row for: ",
         paste(unique(row_label[dup]), collapse = ", "), call. = FALSE)
  }

  # The quadrat a "name" decision sends a stem to: the row's own, else the
  # mismatch table's
  mm_stem <- if (!is.null(mismatches)) {
    mm_raw <- raw_of(mismatches)
    vapply(seq_len(nrow(mismatches)), function(i) {
      rows <- find_stems(mismatches$parent_plot_name[i], mismatches$tag[i],
                         mm_raw[i])
      if (length(rows) == 1) rows else NA_integer_
    }, integer(1))
  } else integer(0)
  flagged <- stem_of %in% mm_stem
  fill <- is.na(wanted) & flagged
  if (any(fill)) {
    wanted[fill] <- mismatches$quadrat_by_name[match(stem_of[fill], mm_stem)]
  }

  by_name <- keep == "name"
  no_target <- by_name & is.na(wanted)
  if (any(no_target)) {
    stop("quadrat_decisions: keep = \"name\" but no quadrat_by_name for ",
         paste(row_label[no_target], collapse = ", "), call. = FALSE)
  }

  target_unit <- paste(parent, wanted, sep = "\r")
  quadrat_key <- paste(quadrats$parent_plot_name, quadrats$quadrat, sep = "\r")
  unknown <- by_name & !target_unit %in% quadrat_key
  if (any(unknown)) {
    stop("quadrat_decisions: no sampled quadrat ",
         paste(sprintf("\"%s\" (%s)", wanted[unknown], row_label[unknown]),
               collapse = ", "),
         " in that parent plot", call. = FALSE)
  }

  # ---- Move the stems ----
  for (i in which(by_name)) {
    stem <- stem_of[i]
    q <- match(target_unit[i], quadrat_key)
    trees$quadrat_unit[stem] <- quadrats$quadrat_unit[q]
    trees$quadrat_label[stem] <- quadrats$quadrat[q]
  }

  # ---- Record what was applied ----
  if (!is.null(mismatches)) {
    d <- match(mm_stem, stem_of)
    hit <- !is.na(d)
    mismatches$keep[hit] <- keep[d[hit]]
    mismatches$quadrat_by_name[hit] <- ifelse(
      is.na(wanted[d[hit]]), mismatches$quadrat_by_name[hit], wanted[d[hit]]
    )
    n_undecided <- sum(!hit)
    if (n_undecided > 0) {
      cli::cli_alert_warning(paste(
        "{n_undecided} quadrat mismatch{?es} not in {.arg quadrat_decisions}",
        "— the tag was used for {?it/them}"
      ))
    }
  }

  # A stem moved without having been flagged (e.g. sent to a third quadrat)
  extra <- which(by_name & !flagged)
  if (length(extra) > 0) {
    stems <- stem_of[extra]
    cols <- intersect(
      c("plot_name_raw", "parent_plot_name", "plot_name", "tag",
        "species_scientific_name"),
      names(trees)
    )
    added <- trees[stems, cols, drop = FALSE]
    # The quadrat its tag gave, before the move
    tag_q <- quadrats[quadrats$parent_plot_name %in% parent[extra] &
                        !is.na(quadrats$firsttag), , drop = FALSE]
    added$quadrat_by_tag <- vapply(seq_along(extra), function(j) {
      q <- tag_q[tag_q$parent_plot_name == parent[extra[j]], , drop = FALSE]
      q <- q[order(q$firsttag), , drop = FALSE]
      k <- findInterval(tag[extra[j]], q$firsttag)
      if (k >= 1L) q$quadrat[k] else NA_character_
    }, character(1))
    added$quadrat_by_name <- wanted[extra]
    added$keep <- "name"
    mismatches <- rbind(mismatches, added)
    mismatches <- mismatches[order(mismatches$parent_plot_name,
                                   mismatches$tag), , drop = FALSE]
    rownames(mismatches) <- NULL
  }

  list(trees = trees, mismatches = mismatches)
}


#' Resolve a relative path against the export directory
#'
#' The review files of a mission belong next to its export, not wherever R
#' happens to be running, so a relative path is taken inside \code{data_dir}.
#' An absolute path, or any path when \code{data_dir} is NULL, is returned
#' as given.
#'
#' @param path Character path.
#' @param data_dir Export directory, or NULL.
#' @return Character path.
#' @keywords internal
.resolve_in_data_dir <- function(path, data_dir = NULL) {
  if (is.null(data_dir) || is.null(path)) return(path)
  absolute <- grepl("^([A-Za-z]:)?[/\\\\]|^~", path)
  if (absolute) path else file.path(data_dir, path)
}


#' Warn about recorded columns this pipeline does not carry forward
#'
#' Several fields of the small-tree form are conditional on a plot-level switch
#' and arrive empty in the exports seen so far: \code{angle} and
#' \code{distance_to_next_stem} are collected only when the plot sets
#' \code{distance_stems = Yes}, \code{height_estimate} only when a tree height
#' was measured. None of them has a home in the output, which is harmless while
#' they are empty and a silent loss the day a team switches them on.
#'
#' \code{taxa_vernacular_name} is in the same position for a different reason:
#' the form records it, the individuals table has nowhere to put it.
#'
#' @param trees Normalised tree data frame.
#' @return NULL, invisibly. Called for its messages.
#' @keywords internal
.warn_unused_openforis_columns <- function(trees) {

  unused <- c(
    angle = "stem mapping, recorded when the plot sets distance_stems = Yes",
    distance_to_next_stem = "stem mapping, recorded when the plot sets distance_stems = Yes",
    height_estimate = "whether tree_height was measured or estimated",
    taxa_vernacular_name = "vernacular name"
  )

  for (col in intersect(names(unused), names(trees))) {
    vals <- trees[[col]]
    filled <- !is.na(vals) & nzchar(trimws(as.character(vals)))
    if (!any(filled)) next
    cli::cli_alert_warning(paste(
      "{.field {col}} is filled for {sum(filled)} stem{?s} ({unused[[col]]})",
      "but has nowhere to go in the output — the values are dropped"
    ))
  }

  invisible(NULL)
}


#' Report stems at or above the small-tree diameter threshold
#'
#' The small-tree protocol covers stems below 10 cm; anything at or above that
#' belongs to the large-stem census of the parent plot and was probably
#' mis-entered. They are reported, not removed.
#'
#' @param trees Assigned tree data frame.
#' @param dbh_max Threshold in cm, or NULL to skip the check.
#' @return Data frame of the offending stems, or NULL.
#' @keywords internal
.flag_oversized_small_trees <- function(trees, dbh_max = 10) {

  if (is.null(dbh_max) || !"stem_diameter" %in% names(trees)) return(NULL)

  vals <- suppressWarnings(as.numeric(trees$stem_diameter))
  over <- !is.na(vals) & vals >= dbh_max
  if (!any(over)) return(NULL)

  cli::cli_alert_warning(paste(
    "{sum(over)} stem{?s} at or above {dbh_max} cm — these belong to the",
    "large-stem census of the parent plot; review result$oversized_stems"
  ))

  cols <- intersect(
    c("plot_name", "parent_plot_name", "tag", "stem_diameter",
      "species_scientific_name"),
    names(trees)
  )
  result <- trees[over, cols, drop = FALSE]
  result <- result[order(result$plot_name, result$tag), , drop = FALSE]
  rownames(result) <- NULL
  result
}


#' Warn when the voucher flag and the recorded voucher numbers disagree
#'
#' The form asks twice whether a stem was collected: \code{any_voucher} (code
#' 1 = yes) and the voucher number itself. A stem flagged as collected with no
#' number, or a number with no flag, means one of the two was missed.
#'
#' @param trees Normalised tree data frame.
#' @param specimens Prepared specimen table, or NULL.
#' @return NULL, invisibly. Called for its messages.
#' @keywords internal
.check_openforis_voucher_flag <- function(trees, specimens) {

  if (!"any_voucher" %in% names(trees) ||
      !"herbarium_nbe_char" %in% names(trees)) {
    return(invisible(NULL))
  }

  flagged <- suppressWarnings(as.numeric(trees$any_voucher)) == 1
  flagged[is.na(flagged)] <- FALSE
  has_number <- !is.na(trees$herbarium_nbe_char) &
    nzchar(trimws(as.character(trees$herbarium_nbe_char)))

  if (any(flagged & !has_number)) {
    cli::cli_alert_warning(
      "{sum(flagged & !has_number)} stem{?s} flagged as collected but carrying no voucher number"
    )
  }
  if (any(has_number & !flagged)) {
    cli::cli_alert_warning(
      "{sum(has_number & !flagged)} stem{?s} carrying a voucher number but not flagged as collected"
    )
  }

  invisible(NULL)
}


#' Build the plot-level table, one row per small-tree plot
#'
#' Reuses \code{.prepare_openforis_new_plots()} for everything the two forms
#' share — country, team, forest state and type, dates — one row per quadrat,
#' then pools the quadrats of each parent plot into its small-tree plot with
#' \code{.collapse_small_tree_plots()}.
#'
#' The quadrat units carry \code{row_id}, the row of \code{plots_raw} they came
#' from, so the join back is positional and unaffected by the repeated plot
#' names that make a quadrat export what it is.
#'
#' @param quadrats Quadrat units from
#'   \code{.prepare_openforis_small_tree_quadrats()}.
#' @param plots_raw Data frame read from \code{plot.xlsx}.
#' @param forest_state_codes,forest_type_codes,team_leader_codes,country_codes
#'   Parsed code lists, or NULL to skip that decoding.
#' @param country,province,method,data_provider,locality_name,principal_investigator,data_manager
#'   Constants not present in the export. NULL to omit.
#' @param plot_area Area of one quadrat in hectares, multiplied by the number
#'   of quadrats of each plot. NULL to omit.
#' @param additional_people Overrides the exported \code{add_people} column.
#' @param census Census number written to a \code{census} column. NULL to omit.
#' @return Data frame with one row per small-tree plot.
#' @keywords internal
.prepare_openforis_small_tree_plots <- function(quadrats, plots_raw,
                                                country_codes = NULL,
                                                forest_state_codes = NULL,
                                                forest_type_codes = NULL,
                                                team_leader_codes = NULL,
                                                country = NULL, province = NULL,
                                                method = NULL,
                                                data_provider = NULL,
                                                locality_name = NULL,
                                                plot_area = NULL,
                                                principal_investigator = NULL,
                                                data_manager = NULL,
                                                additional_people = NULL,
                                                census = NULL) {

  # .prepare_openforis_new_plots() collapses forest_type per plot_name, so each
  # row is named after its quadrat first; the pooling below then decides how
  # the quadrats of one plot combine.
  raw <- plots_raw
  raw$plot_name <- quadrats$quadrat_unit[match(seq_len(nrow(raw)), quadrats$row_id)]

  per_quadrat <- .prepare_openforis_new_plots(
    raw,
    country_codes = country_codes,
    forest_state_codes = forest_state_codes,
    forest_type_codes = forest_type_codes,
    team_leader_codes = team_leader_codes,
    country = country, province = province, method = method,
    data_provider = data_provider,
    locality_name = locality_name, plot_area = NULL,
    principal_investigator = principal_investigator,
    data_manager = data_manager,
    additional_people = additional_people,
    census = census
  )
  names(per_quadrat)[names(per_quadrat) == "plot_name"] <- "quadrat_unit"

  # A row of plots_raw with no quadrat unit cannot be named and is dropped
  per_quadrat <- per_quadrat[!is.na(per_quadrat$quadrat_unit), , drop = FALSE]

  .collapse_small_tree_plots(per_quadrat, quadrats, quadrat_area = plot_area)
}


#' Pool the per-quadrat metadata of each small-tree plot into one row
#'
#' \code{plot.xlsx} describes every quadrat, a plot row holds one value per
#' column. The rules:
#' \itemize{
#'   \item \code{plot_area} is the number of quadrats times \code{quadrat_area};
#'   \item \code{quadrats} lists the quadrat labels in firsttag order;
#'   \item the date is the earliest of the quadrats;
#'   \item \code{team_leader}, \code{additional_people} and
#'     \code{identified_by} pool the people of every quadrat;
#'   \item \code{forest_type} and \code{forest_state} pool their values;
#'   \item any other column keeps its first non-missing value.
#' }
#' A plot whose quadrats disagree on its date or on a column of the last kind
#' is announced, naming the values found.
#'
#' @param per_quadrat Plot metadata with one row per quadrat, keyed by
#'   \code{quadrat_unit}.
#' @param quadrats Quadrat units (quadrat_unit, plot_name, parent_plot_name,
#'   quadrat, firsttag).
#' @param quadrat_area Area of one quadrat in hectares, or NULL.
#' @param relation Value written to \code{parent_relation}.
#' @return Data frame with one row per small-tree plot: plot_name,
#'   parent_plot_name, parent_relation, quadrats, plot_area (when
#'   \code{quadrat_area} is given), then the pooled metadata columns.
#' @keywords internal
.collapse_small_tree_plots <- function(per_quadrat, quadrats,
                                       quadrat_area = NULL,
                                       relation = "nested_subsample") {

  q <- quadrats[!is.na(quadrats$plot_name), , drop = FALSE]
  q <- q[order(q$plot_name, q$firsttag, na.last = TRUE), , drop = FALSE]
  plot_names <- unique(q$plot_name)

  people_cols <- c("team_leader", "additional_people", "identified_by")
  pooled_cols <- c("forest_type", "forest_state")
  date_cols <- c("date_y", "date_m", "date_d")
  meta_cols <- setdiff(names(per_quadrat), "quadrat_unit")

  first_value <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) x[NA_integer_] else x[1]
  }
  pool_values <- function(x) {
    x <- trimws(unlist(strsplit(as.character(x[!is.na(x)]), ",")))
    x <- unique(x[nzchar(x)])
    if (length(x) == 0) NA_character_ else paste(x, collapse = ", ")
  }

  conflicts <- character(0)
  rows <- vector("list", length(plot_names))

  for (i in seq_along(plot_names)) {
    p <- plot_names[i]
    units <- q[q$plot_name == p, , drop = FALSE]
    sub <- per_quadrat[match(units$quadrat_unit, per_quadrat$quadrat_unit), ,
                       drop = FALSE]
    sub <- sub[!is.na(sub$quadrat_unit), , drop = FALSE]

    row <- data.frame(
      plot_name = p,
      parent_plot_name = units$parent_plot_name[1],
      parent_relation = relation,
      quadrats = paste(units$quadrat, collapse = ", "),
      stringsAsFactors = FALSE
    )
    if (!is.null(quadrat_area)) row$plot_area <- nrow(units) * quadrat_area

    for (cl in setdiff(meta_cols, date_cols)) {
      vals <- sub[[cl]]
      if (cl %in% people_cols) {
        row[[cl]] <- .collapse_people(vals)
      } else if (cl %in% pooled_cols) {
        row[[cl]] <- pool_values(vals)
      } else {
        distinct <- unique(vals[!is.na(vals)])
        if (length(distinct) > 1) {
          conflicts <- c(conflicts, sprintf(
            "%s: %s differs between quadrats (%s), kept %s",
            p, cl, paste(distinct, collapse = " / "), distinct[1]
          ))
        }
        row[[cl]] <- first_value(vals)
      }
    }

    present_dates <- intersect(date_cols, names(sub))
    if (length(present_dates) > 0 && nrow(sub) > 0) {
      ord <- do.call(order, c(unname(as.list(sub[present_dates])),
                              list(na.last = TRUE)))
      for (cl in present_dates) row[[cl]] <- sub[[cl]][ord[1]]

      stamps <- unique(do.call(paste, c(unname(as.list(sub[present_dates])),
                                        list(sep = "-"))))
      if (length(stamps) > 1) {
        conflicts <- c(conflicts, sprintf(
          "%s: quadrats sampled on different dates (%s), kept the earliest",
          p, paste(stamps, collapse = " / ")
        ))
      }
    }

    rows[[i]] <- row
  }

  if (length(rows) == 0) {
    return(data.frame(plot_name = character(0), parent_plot_name = character(0),
                      parent_relation = character(0), quadrats = character(0),
                      stringsAsFactors = FALSE))
  }

  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  if (length(conflicts) > 0) {
    cli::cli_alert_warning(
      "Quadrats of a plot disagree on {length(conflicts)} value{?s}:"
    )
    cli::cli_bullets(stats::setNames(conflicts, rep("*", length(conflicts))))
  }

  result
}


#' Join the 20 x 20 m quadrat and the 10 x 10 m sub-quadrat of each stem
#'
#' @param quadrat Character. Quadrat label (\code{"20_20"}).
#' @param subquadrat Character. Sub-quadrat label (\code{"A"}), or NULL.
#' @param sep Separator.
#' @return Character vector such as \code{"20_20_A"}; the quadrat alone when
#'   the sub-quadrat is missing, NA when the quadrat is.
#' @keywords internal
.combine_small_tree_quadrat <- function(quadrat, subquadrat = NULL, sep = "_") {

  quadrat <- as.character(quadrat)
  if (is.null(subquadrat)) return(quadrat)

  subquadrat <- trimws(as.character(subquadrat))
  has_sub <- !is.na(subquadrat) & nzchar(subquadrat)

  out <- ifelse(has_sub, paste0(quadrat, sep, subquadrat), quadrat)
  out[is.na(quadrat)] <- NA_character_
  out
}


#' Refuse a small-tree plot suffix that cannot name a plot safely
#'
#' The suffix must be non-empty, so the small-tree plot is never filed under
#' its parent's name, and must not end with a digit, which
#' \code{.normalise_plot_name()} would read as the plot number and pad.
#'
#' @param plot_suffix Value of the \code{plot_suffix} argument.
#' @return \code{plot_suffix}, invisibly.
#' @keywords internal
.check_small_tree_plot_suffix <- function(plot_suffix) {

  if (!is.character(plot_suffix) || length(plot_suffix) != 1 ||
      is.na(plot_suffix) || !nzchar(trimws(plot_suffix))) {
    stop("plot_suffix must be a single non-empty string, e.g. \"_smalltree\".",
         call. = FALSE)
  }
  if (grepl("[0-9]$", plot_suffix)) {
    stop(sprintf(paste(
      "plot_suffix '%s' ends with a digit, which would be read as the plot",
      "number. Use a suffix ending with a letter, e.g. \"_smalltree\"."
    ), plot_suffix), call. = FALSE)
  }

  invisible(plot_suffix)
}



#' Add the observed tag ranges to the quadrat review table
#'
#' \code{firsttag} says where a quadrat's numbering was meant to start; this
#' adds where it actually ran, how many stems fell in it, and which raw
#' \code{plot_plot_name_old} spellings they carried — the three things needed
#' to judge whether the assignment is right.
#'
#' @param quadrats Quadrat units.
#' @param trees Assigned tree data frame.
#' @param plot_name_col Name of the raw plot column, used in the message only.
#' @return \code{quadrats} with n_stems, tag_min, tag_max and raw_plot_names.
#' @keywords internal
.summarise_small_tree_quadrats <- function(quadrats, trees,
                                           plot_name_col = "plot_plot_name_old") {

  idx <- split(seq_len(nrow(trees)), trees$quadrat_unit)

  pick <- function(plot, fun, default) {
    rows <- idx[[plot]]
    if (is.null(rows) || length(rows) == 0) return(default)
    fun(rows)
  }

  quadrats$n_stems <- unname(vapply(quadrats$quadrat_unit, pick, integer(1),
                                    fun = length, default = 0L))
  quadrats$tag_min <- unname(vapply(
    quadrats$quadrat_unit, pick, numeric(1),
    fun = function(r) min(trees$tag[r], na.rm = TRUE), default = NA_real_))
  quadrats$tag_max <- unname(vapply(
    quadrats$quadrat_unit, pick, numeric(1),
    fun = function(r) max(trees$tag[r], na.rm = TRUE), default = NA_real_))
  quadrats$raw_plot_names <- unname(vapply(
    quadrats$quadrat_unit, pick, character(1),
    fun = function(r) paste(sort(unique(trees$plot_name_raw[r])), collapse = "; "),
    default = NA_character_
  ))

  drifted <- !is.na(quadrats$tag_min) & quadrats$tag_min != quadrats$firsttag
  if (any(drifted)) {
    cli::cli_alert_warning(paste(
      "{sum(drifted)} quadrat{?s} whose lowest observed tag is not its",
      "{.field firsttag} ({.val {quadrats$quadrat_unit[drifted]}}) — check the",
      "{.field {plot_name_col}} column for {?it/them}"
    ))
  }

  quadrats$row_id <- NULL
  quadrats
}
