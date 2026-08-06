# Tropicos Bulk Upload Export/Import Functions
#
# build_tropicos_upload_table() converts query_specimens() output into the
# Tropicos bulk upload template format (database -> Tropicos).
#
# build_specimens_from_tropicos() does the reverse: converts an already
# -imported Tropicos specimen export/search-results table (NOT the same
# column layout as the bulk upload template -- see
# inst/docs/example_tropicos.csv) into a specimens-shaped tibble (Tropicos
# -> database), ready for taxon/collector ID resolution and review before
# add_specimens().
#
# Related one-off/ad-hoc tools (not exported, kept as archives to re-run
# when needed) live in inst/scripts/tropicos_collector_matching_and_export.R:
# match_tropicos_person_ids() / apply_tropicos_person_ids() (fuzzy-match and
# backfill table_colnam.id_tropicos_person from an MBG collector spreadsheet)
# and write_tropicos_upload_table() (write build_tropicos_upload_table()'s
# output to xlsx).
#
# Dependencies: DBI, dplyr, cli

#' Build a Tropicos bulk-upload table from query_specimens() output
#'
#' Converts a specimens tibble (as returned by
#' `query_specimens(subset_columns = TRUE)`) into the 31-column layout of
#' the Tropicos bulk upload "Data" sheet.
#'
#' @section Column mapping and known gaps:
#' Most columns map directly or through a light transform (dates split into
#' day/month/year, `CollectionNumber` built as `colnbr` + `suffix` and
#' always coerced to character, taxon name built from
#' `tax_infra_level`/`tax_gen`, senior collector's Tropicos Person ID
#' joined from `table_colnam.id_tropicos_person`).
#'
#' If `SeniorCollectorPersonID` comes back `NA` for collectors you'd expect
#' to have a Tropicos Person ID, `table_colnam.id_tropicos_person` hasn't
#' been backfilled for them yet -- see
#' `inst/scripts/tropicos_collector_matching_and_export.R` for
#' `match_tropicos_person_ids()` (fuzzy-matches an MBG collector spreadsheet
#' against `table_colnam`) and `apply_tropicos_person_ids()` (writes
#' confirmed matches back to the database). That same script also has
#' `write_tropicos_upload_table()` for saving this function's output to
#' xlsx.
#'
#' A few columns have **no source in the database at all** and are always
#' `NA`, to be filled in manually: `DeterminationQualifier`,
#' `DeterminedByPersonID` (`detby` is free text, not linked to
#' `table_colnam`), `DeterminationInstitution`, `LocationID`,
#' `MinimumElevation` (specimens aren't tied to a plot with elevation),
#' `VegetationDescription`, `Duplicates`, `Institutions`,
#' `OtherCollectorIDs`, `GeneralKeywords`.
#'
#' `AuthorityKey` is also left blank by default for now -- the template's
#' example rows suggest it follows a convention (first initial of the
#' collector's first name + first three letters of their surname, lowercase,
#' + collection number + suffix, e.g. "Ehoarn Bidault" + 6362 + "A" ->
#' `"Ebid6362A"`), but this hasn't been confirmed as the actual rule yet, so
#' nothing is auto-generated until it is.
#'
#' `CollectorString` is the senior collector's name (`colnam`), with the
#' free-text `add_col` ("additional collectors") field from `specimens`
#' appended verbatim if present; it is not parsed into individual names.
#'
#' @param specimens Tibble from `query_specimens(subset_columns = TRUE)`
#'   (or containing at least the same columns).
#' @param con Database connection to the main database, used to look up
#'   `id_tropicos_person`. If NULL, calls [call.mydb()].
#' @param authority Character or `NA` (default `"Madagascar"`). Value for
#'   the `Authority` column (a fixed per-submission code in Tropicos' terms,
#'   not the specimens' actual collection country -- override per batch as
#'   needed).
#' @param coordinate_method Character, recycled to all rows (default
#'   `"GPS"`, matching every example row in the template).
#' @param elevation_unit Character, recycled to all rows (default `"m"`,
#'   matching every example row in the template).
#' @param elevation_method Character, recycled to all rows (default
#'   `"GPS"`, matching every example row in the template).
#' @param date_language Character or `NA` (default `"French"`), recycled to
#'   all rows. The template uses this to flag the language of
#'   `DescriptionNote`; there's no way to infer it from the database, so
#'   override it per batch as needed.
#' @return A tibble with the 31 Tropicos template columns, in template
#'   column order, one row per input specimen.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' specimens <- query_specimens(id_colnam = 123, subset_columns = TRUE,
#'                              show_html = FALSE, con = con)
#' tropicos_tbl <- build_tropicos_upload_table(specimens, con, authority = "Madagascar")
#' }
#'
#' @export
build_tropicos_upload_table <- function(specimens, con = NULL,
                                        authority = "Madagascar",
                                        coordinate_method = "GPS",
                                        elevation_unit = "m",
                                        elevation_method = "GPS",
                                        date_language = "French") {

  if (is.null(con)) con <- call.mydb()

  required_cols <- c(
    "id_specimen", "colnam", "colnbr", "suffix", "ddlat", "ddlon",
    "locality", "detby", "detd", "detm", "dety", "add_col",
    "cold", "colm", "coly", "description", "tax_gen", "tax_infra_level",
    "id_colnam"
  )
  missing_cols <- setdiff(required_cols, names(specimens))
  if (length(missing_cols) > 0) {
    stop(
      "specimens is missing expected column(s): ", paste(missing_cols, collapse = ", "),
      ". Use query_specimens(subset_columns = TRUE) to get the expected shape.",
      call. = FALSE
    )
  }

  # Senior collector's Tropicos Person ID, joined via id_colnam. Tolerate
  # the migration not having been run yet (id_tropicos_person column
  # missing) by falling back to NA for everyone.
  person_ids <- tryCatch({
    DBI::dbGetQuery(con, "SELECT id_table_colnam, id_tropicos_person FROM table_colnam")
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch id_tropicos_person ({e$message}); leaving SeniorCollectorPersonID blank.")
    data.frame(id_table_colnam = integer(0), id_tropicos_person = integer(0))
  })

  senior_id <- person_ids$id_tropicos_person[match(specimens$id_colnam, person_ids$id_table_colnam)]

  blank <- function(fill = NA_character_) rep(fill, nrow(specimens))

  suffix_txt <- ifelse(is.na(specimens$suffix), "", specimens$suffix)
  add_col_txt <- trimws(ifelse(is.na(specimens$add_col), "", specimens$add_col))

  dplyr::tibble(
    Authority = rep(authority, nrow(specimens)),
    AuthorityKey = blank(),
    CollectorString = ifelse(
      nzchar(add_col_txt),
      paste0(specimens$colnam, ", ", add_col_txt),
      specimens$colnam
    ),
    SeniorCollectorPersonID = senior_id,
    CollectionNumber = as.character(paste0(specimens$colnbr, suffix_txt)),
    DeterminationNameID = dplyr::coalesce(specimens$tax_infra_level, specimens$tax_gen),
    DeterminationQualifier = blank(),
    DeterminedBy = specimens$detby,
    DeterminedByPersonID = blank(NA_integer_),
    DeterminationDay = specimens$detd,
    DeterminationMonth = specimens$detm,
    DeterminationYear = specimens$dety,
    DeterminationInstitution = blank(),
    LocationID = blank(),
    LocalityNote = specimens$locality,
    MinimumLatitude = specimens$ddlat,
    MinimumLongitude = specimens$ddlon,
    CoordinateMethod = rep(coordinate_method, nrow(specimens)),
    MinimumElevation = blank(NA_real_),
    ElevationUnit = rep(elevation_unit, nrow(specimens)),
    ElevationMethod = rep(elevation_method, nrow(specimens)),
    MinimumDay = specimens$cold,
    MinimumMonth = specimens$colm,
    MinimumYear = specimens$coly,
    DateLanguage = rep(date_language, nrow(specimens)),
    DescriptionNote = specimens$description,
    VegetationDescription = blank(),
    Duplicates = blank(NA_integer_),
    Institutions = blank(),
    OtherCollectorIDs = blank(),
    GeneralKeywords = blank()
  )
}


#' Build a specimens-shaped table from a Tropicos specimen export
#'
#' Converts a Tropicos specimen export/search-results CSV (e.g. as
#' downloaded from a Tropicos search -- see `inst/docs/example_tropicos.csv`
#' for the expected layout; this is a **different** column set from the
#' bulk upload template used by [build_tropicos_upload_table()]) into a
#' tibble shaped like `query_specimens()` output, ready for review and
#' taxon/collector ID resolution before [add_specimens()].
#'
#' @section Column mapping and known gaps:
#' `colnbr`/`suffix` are parsed from `CollectionNumber` (leading digits /
#' trailing letters); if a value doesn't match that pattern, `colnbr` keeps
#' the raw string and `suffix` is `NA`. `colnam`/`add_col` are parsed from
#' `CollectorString` by splitting on the first comma -- the senior collector
#' before it, the rest (if any) after -- which is the exact inverse of how
#' [build_tropicos_upload_table()] builds `CollectorString` from `colnam` +
#' `add_col`. If `CollectorString` isn't present, `SeniorCollector`
#' (`"Lastname, Firstname"`) is reformatted to `"Firstname Lastname"`
#' instead, and `add_col` is left `NA`.
#'
#' `id_colnam` is resolved precisely via `SeniorCollectorPersonID` ->
#' `table_colnam.id_tropicos_person` when `con` is supplied -- the exact
#' inverse of how `build_tropicos_upload_table()` fills
#' `SeniorCollectorPersonID` from that same column. Collectors not yet
#' backfilled with an `id_tropicos_person` are left with `id_colnam = NA`
#' and `colnam` holds the raw parsed name for you to resolve manually (e.g.
#' via `.link_colnam()`) or by running `match_tropicos_person_ids()` /
#' `apply_tropicos_person_ids()`
#' (`inst/scripts/tropicos_collector_matching_and_export.R`) against an
#' updated MBG collector spreadsheet first, then re-running this function.
#'
#' `tax_gen`/`tax_esp`/`tax_fam`/`tax_infra` map directly from
#' `Genus`/`Species`/`FamilyName`/`Subspecific`. `tax_infra_level` uses
#' Tropicos' own pre-built `NameNoAuthors` when `Species` is present (`NA`
#' for genus-only records), matching this package's own convention (see
#' `.format_taxa_names()`).
#'
#' `id_tropicos_name` is the taxon's Tropicos Name ID, mapped directly from
#' `NameID`. **It is not the `specimens.id_tropicos` column**, which holds the
#' Tropicos *collection* ID (one per gathering event) -- a different
#' namespace. Do not map it onto `id_tropicos` when calling [add_specimens()];
#' use it to resolve the name against this database's backbone instead.
#'
#' **`idtax_n`/`idtax_f` are always `NA`** -- resolving a Tropicos name to
#' this database's taxon backbone (`table_taxa`) is a separate step; use the
#' matching tools in `R/taxonomic_query_functions.R` (e.g. `match_tax()`)
#' against `tax_gen`/`tax_esp`/`tax_fam`/`tax_infra_level`, and
#' `add_entry_taxa()` for names genuinely not yet in the database.
#' `id_specimen` is always `NA` (these are new records). `detvalue` has no
#' Tropicos equivalent and is always `NA`. `detd`/`detm` are only populated
#' if the export includes `DeterminationDay`/`DeterminationMonth` columns --
#' `example_tropicos.csv` doesn't, only `DeterminationYear`.
#'
#' An extra `id_tropicos_specimen` column (Tropicos' own `SpecimenID`, not a
#' `specimens` table column) is included for your own traceability/audit --
#' [add_specimens()] only keeps whatever columns you explicitly map, so it's
#' safe to leave in and simply not map. Note that `SpecimenID` identifies one
#' physical herbarium sheet, so several rows of a Tropicos export -- one per
#' institution holding a duplicate -- can correspond to a single row of
#' `specimens`; deduplicate on `colnbr`/`suffix` before comparing with the
#' database (see `inst/scripts/pird_collection_status.R`).
#'
#' @param tropicos_data Data frame/tibble already read from a Tropicos
#'   export CSV (e.g. via `readr::read_csv()` or `read.csv()`), with the
#'   same columns as `inst/docs/example_tropicos.csv`.
#' @param con Database connection to the main database, used to resolve
#'   `id_colnam` via `SeniorCollectorPersonID`. If NULL, calls [call.mydb()].
#' @return A tibble with one row per input record: `colnam`, `id_colnam`,
#'   `colnbr`, `suffix`, `ddlat`, `ddlon`, `country`, `locality`, `detby`,
#'   `detd`, `detm`, `dety`, `add_col`, `cold`, `colm`, `coly`, `detvalue`,
#'   `description`, `idtax_n`, `idtax_f`, `tax_gen`, `tax_esp`, `tax_fam`,
#'   `tax_infra_level`, `tax_infra`, `id_tropicos_name`, `id_tropicos_specimen`.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' tropicos_data <- readr::read_csv("inst/docs/example_tropicos.csv")
#' specimens_staging <- build_specimens_from_tropicos(tropicos_data, con)
#' }
#'
#' @export
build_specimens_from_tropicos <- function(tropicos_data, con = NULL) {

  if (is.null(con)) con <- call.mydb()

  n <- nrow(tropicos_data)
  .col_or_na <- function(col, na_val = NA_character_) {
    if (col %in% names(tropicos_data)) tropicos_data[[col]] else rep(na_val, n)
  }

  if (!"CollectionNumber" %in% names(tropicos_data)) {
    stop("tropicos_data is missing expected column: CollectionNumber", call. = FALSE)
  }
  if (!any(c("CollectorString", "SeniorCollector") %in% names(tropicos_data))) {
    stop(
      "tropicos_data is missing a collector column: need CollectorString or SeniorCollector",
      call. = FALSE
    )
  }

  # CollectionNumber -> colnbr (leading digits) + suffix (trailing letters).
  # Falls back to keeping the raw string in colnbr (suffix NA) rather than
  # dropping anything when a value doesn't match the expected pattern.
  collnum <- trimws(.col_or_na("CollectionNumber"))
  collnum_match <- regmatches(collnum, regexec("^([0-9]+)([A-Za-z]*)$", collnum))
  colnbr_num <- vapply(collnum_match, function(m) if (length(m) == 3) m[2] else NA_character_, character(1))
  suffix <- vapply(collnum_match, function(m) if (length(m) == 3 && nzchar(m[3])) m[3] else NA_character_, character(1))
  colnbr <- suppressWarnings(as.integer(ifelse(is.na(colnbr_num), collnum, colnbr_num)))

  # CollectorString -> colnam (senior collector) + add_col (rest), splitting
  # on the first comma -- the inverse of build_tropicos_upload_table()'s
  # CollectorString = paste0(colnam, ", ", add_col). Falls back to
  # reformatting SeniorCollector ("Lastname, Firstname" -> "Firstname
  # Lastname") when CollectorString isn't available.
  if ("CollectorString" %in% names(tropicos_data)) {
    collector_string <- trimws(.col_or_na("CollectorString"))
    comma_pos <- regexpr(",", collector_string, fixed = TRUE)
    colnam_raw <- ifelse(
      comma_pos > 0,
      trimws(substr(collector_string, 1, comma_pos - 1)),
      collector_string
    )
    add_col <- ifelse(
      comma_pos > 0,
      trimws(substr(collector_string, comma_pos + 1, nchar(collector_string))),
      NA_character_
    )
  } else {
    senior_collector <- trimws(.col_or_na("SeniorCollector"))
    name_parts <- strsplit(senior_collector, ",", fixed = TRUE)
    colnam_raw <- vapply(name_parts, function(p) {
      if (length(p) < 2) return(trimws(p[1]))
      paste(trimws(p[2]), trimws(p[1]))
    }, character(1))
    add_col <- rep(NA_character_, n)
  }

  # Resolve id_colnam precisely via SeniorCollectorPersonID ->
  # table_colnam.id_tropicos_person -- the exact inverse of
  # build_tropicos_upload_table()'s SeniorCollectorPersonID lookup. Kept as
  # character (not coerced to numeric) throughout: id_tropicos_person is
  # BIGINT in Postgres, which RPostgres returns as bit64::integer64 by
  # default, and match()/== don't reliably compare integer64 against a
  # plain numeric vector -- comparing as character sidesteps that entirely.
  senior_person_id <- trimws(.col_or_na("SeniorCollectorPersonID"))
  id_colnam <- rep(NA_integer_, n)
  colnam <- colnam_raw

  if (!all(is.na(senior_person_id))) {
    person_ids <- tryCatch({
      DBI::dbGetQuery(
        con,
        "SELECT id_table_colnam, id_tropicos_person, colnam FROM table_colnam WHERE id_tropicos_person IS NOT NULL"
      )
    }, error = function(e) {
      cli::cli_alert_warning("Could not fetch id_tropicos_person ({e$message}); leaving id_colnam unresolved.")
      data.frame(id_table_colnam = integer(0), id_tropicos_person = integer(0), colnam = character(0))
    })

    matched <- match(senior_person_id, as.character(person_ids$id_tropicos_person))
    id_colnam <- person_ids$id_table_colnam[matched]
    colnam <- ifelse(!is.na(matched), person_ids$colnam[matched], colnam_raw)
  }

  tax_esp <- .col_or_na("Species")
  tax_infra_level <- ifelse(!is.na(tax_esp), .col_or_na("NameNoAuthors"), NA_character_)

  dplyr::tibble(
    colnam = colnam,
    id_colnam = id_colnam,
    colnbr = colnbr,
    suffix = suffix,
    ddlat = suppressWarnings(as.numeric(.col_or_na("LatitudeDecimal", NA_real_))),
    ddlon = suppressWarnings(as.numeric(.col_or_na("LongitudeDecimal", NA_real_))),
    country = .col_or_na("CountryName"),
    locality = .col_or_na("Locality"),
    detby = .col_or_na("DeterminedBy"),
    detd = suppressWarnings(as.integer(.col_or_na("DeterminationDay", NA_integer_))),
    detm = suppressWarnings(as.integer(.col_or_na("DeterminationMonth", NA_integer_))),
    dety = suppressWarnings(as.integer(.col_or_na("DeterminationYear", NA_integer_))),
    add_col = add_col,
    cold = suppressWarnings(as.integer(.col_or_na("MinimumDay", NA_integer_))),
    colm = suppressWarnings(as.integer(.col_or_na("MinimumMonth", NA_integer_))),
    coly = suppressWarnings(as.integer(.col_or_na("MinimumYear", NA_integer_))),
    detvalue = rep(NA_character_, n),
    description = .col_or_na("Description"),
    idtax_n = rep(NA_integer_, n),
    idtax_f = rep(NA_integer_, n),
    tax_gen = .col_or_na("Genus"),
    tax_esp = tax_esp,
    tax_fam = .col_or_na("FamilyName"),
    tax_infra_level = tax_infra_level,
    tax_infra = .col_or_na("Subspecific"),
    # Deliberately NOT named id_tropicos: the specimens table's id_tropicos
    # holds the Tropicos *collection* ID, a different namespace. Mapping
    # NameID onto that name let taxon IDs be written into a collection-ID
    # column through add_specimens().
    id_tropicos_name = suppressWarnings(as.integer(.col_or_na("NameID", NA_integer_))),
    id_tropicos_specimen = suppressWarnings(as.integer(.col_or_na("SpecimenID", NA_integer_)))
  )
}
