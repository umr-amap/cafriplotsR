# =============================================================================
# African Plant Database (APD) - importer
#
# Loads an APD export (sent by the Conservatoire et Jardin botaniques de
# Geneve) into apd_names, which v_backbone_names_apd exposes to the generic
# backbone layer (R/backbone_core.R). Re-run at every new export.
#
# Schema: inst/migrations/apd_backbone.R
# Rules:  inst/docs/migration_plan_multi_backbone.md, section 7.2
# =============================================================================


#' Columns an APD export must have
#'
#' `STATUT_SYN` is not among them: it adds nothing to `taxon_status` and
#' `idtax_good_n`, and is dropped if present.
#' @noRd
.apd_export_columns <- c(
  "ID", "idtax_good_n", "id_PARENT", "taxon_name", "nom_standard",
  "tax_level", "taxrank", "tax_famclass", "fk_famille", "tax_gen", "tax_esp",
  "author1", "author2", "taxon_status", "citation", "year_description",
  "date_modification"
)


#' Infraspecific ranks in APD's tax_level
#' @noRd
.apd_infra_levels <- c("subspecies", "varietas", "subvarietas", "forma",
                       "cultivar")


#' Read an APD export
#'
#' Tab-separated, quoted, every column read as text. Exports have so far been
#' Latin-1. A file read with the wrong encoding is refused rather than stored
#' garbled: UTF-8 read as Latin-1 shows the two-character sequences starting
#' with a capital A tilde, Latin-1 read as UTF-8 is not valid UTF-8.
#'
#' @param file Path to the export.
#' @param encoding `"Latin-1"` or `"UTF-8"`.
#' @return A data frame with the columns of `.apd_export_columns`, as UTF-8
#'   text.
#' @noRd
.read_apd_export <- function(file, encoding = c("Latin-1", "UTF-8")) {
  encoding <- match.arg(encoding)
  if (!file.exists(file)) cli::cli_abort("File {.file {file}} not found.")

  raw <- data.table::fread(
    file, sep = "\t", quote = "\"", encoding = encoding,
    colClasses = "character", na.strings = c("", "NA", "NULL"),
    data.table = FALSE, showProgress = FALSE
  )

  missing_cols <- setdiff(.apd_export_columns, names(raw))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.file {basename(file)}} is not an APD export: missing column{?s} {.field {missing_cols}}."
    )
  }
  raw <- raw[, .apd_export_columns, drop = FALSE]

  text <- c(raw$taxon_name, raw$nom_standard, raw$author1, raw$author2)
  text <- text[!is.na(text)]
  if (encoding == "UTF-8" && !all(validUTF8(text))) {
    cli::cli_abort(c(
      "{.file {basename(file)}} is not valid UTF-8.",
      "i" = "Re-run with {.code encoding = \"Latin-1\"}."
    ))
  }

  raw[] <- lapply(raw, enc2utf8)

  if (encoding == "Latin-1") {
    # UTF-8 bytes of an accented letter, read as Latin-1
    mojibake <- paste0(intToUtf8(0xC3), "[", intToUtf8(0x80), "-",
                       intToUtf8(0xBF), "]")
    text <- c(raw$taxon_name, raw$nom_standard, raw$author1, raw$author2)
    n_bad <- sum(grepl(mojibake, text[!is.na(text)]))
    if (n_bad > 0) {
      cli::cli_abort(c(
        "{.file {basename(file)}} looks like UTF-8: {n_bad} value{?s} would be stored garbled.",
        "i" = "Re-run with {.code encoding = \"UTF-8\"}."
      ))
    }
  }

  raw
}


#' Version of an APD export
#'
#' Exports carry no version tag, so the version is the date the file was
#' created, `YYYY-MM-DD`. The creation time the file system reports is reset
#' whenever a file is copied, so the default is the last-modified date, which
#' survives copies.
#'
#' @param file Path to the export.
#' @param version `NULL` or a `YYYY-MM-DD` string.
#' @return The version string.
#' @noRd
.apd_export_version <- function(file, version = NULL) {
  if (is.null(version)) version <- format(file.mtime(file), "%Y-%m-%d")
  valid <- is.character(version) && length(version) == 1L && !is.na(version) &&
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", version) &&
    !is.na(as.Date(version, format = "%Y-%m-%d"))
  if (!valid) {
    cli::cli_abort(
      "{.arg version} must be the date of the export as {.val YYYY-MM-DD}, not {.val {version}}."
    )
  }
  version
}


#' Derive apd_names rows from an APD export
#'
#' Applies section 7.2 of the migration plan:
#' - the three ID columns renamed, pointers kept as they are;
#' - `taxon_name` without the rank prefix used above genus (`cla. Liliopsida`),
#'   family names capitalised as in `table_taxa` (`Acanthaceae`, not
#'   `ACANTHACEAE`);
#' - `family` from `fk_famille`, capitalised;
#' - `species`: `tax_esp`, cut to its first word when it also holds an
#'   infraspecific rank (`prostrata f. pedicellata`), kept whole otherwise
#'   (`sp. 1`, hybrids);
#' - `infra_rank`, `infra_epithet` split from `taxrank`; an autonym's epithet
#'   is the species epithet;
#' - `authors`: `author2` for infraspecific names (none for an autonym),
#'   `author1` for species, and for genus and above, which have no author
#'   columns, what `nom_standard` adds to the name.
#'
#' @param raw Data frame from `.read_apd_export()`.
#' @param version Export version, `YYYY-MM-DD`.
#' @return A data frame with the columns of `apd_names`, all as text, ready to
#'   be cast by the database.
#' @noRd
.prepare_apd_names <- function(raw, version) {

  blank_to_na <- function(x) {
    x <- trimws(x)
    x[!is.na(x) & !nzchar(x)] <- NA_character_
    x
  }
  raw[] <- lapply(raw, blank_to_na)

  for (col in c("ID", "idtax_good_n", "id_PARENT")) {
    bad <- !is.na(raw[[col]]) & !grepl("^[0-9]+$", raw[[col]])
    if (any(bad)) {
      cli::cli_abort(
        "{sum(bad)} value{?s} of {.field {col}} {?is/are} not a positive integer, e.g. {.val {utils::head(raw[[col]][bad], 3)}}."
      )
    }
  }
  if (anyNA(raw$ID)) {
    cli::cli_abort("{sum(is.na(raw$ID))} record{?s} without {.field ID}.")
  }
  dup <- unique(raw$ID[duplicated(raw$ID)])
  if (length(dup) > 0) {
    cli::cli_abort(
      "{length(dup)} {.field ID}{?s} occur{?s/} more than once, e.g. {.val {utils::head(dup, 5)}}."
    )
  }
  no_name <- is.na(raw$taxon_name)
  if (any(no_name)) {
    cli::cli_abort(
      "{sum(no_name)} record{?s} without {.field taxon_name}, e.g. ID {.val {utils::head(raw$ID[no_name], 5)}}."
    )
  }

  capitalise <- function(x) {
    upper <- !is.na(x) & grepl("^[A-Z][A-Z-]*$", x)
    x[upper] <- paste0(substr(x[upper], 1, 1), tolower(substring(x[upper], 2)))
    x
  }

  level     <- raw$tax_level
  is_family <- level %in% "familia"
  is_infra  <- level %in% .apd_infra_levels | !is.na(raw$taxrank)

  taxon_name <- raw$taxon_name
  taxon_name[is_family] <- capitalise(taxon_name[is_family])
  taxon_name <- sub("^[a-z]+\\.\\s+(?=[A-Z])", "", taxon_name, perl = TRUE)

  family <- capitalise(raw$fk_famille)
  family[is_family] <- taxon_name[is_family]

  species <- raw$tax_esp
  with_rank <- !is.na(species) &
    grepl("\\s(subsp|var|subvar|f|forma|cv)\\.\\s", species)
  species[with_rank] <- sub("\\s.*$", "", species[with_rank])

  has_rank <- is_infra & !is.na(raw$taxrank)
  infra_rank <- rep(NA_character_, nrow(raw))
  infra_epithet <- rep(NA_character_, nrow(raw))
  infra_rank[has_rank] <- sub("\\s.*$", "", raw$taxrank[has_rank])
  infra_epithet[has_rank] <- trimws(sub("^\\S+", "", raw$taxrank[has_rank]))
  autonym <- has_rank & !nzchar(infra_epithet)
  infra_epithet[autonym] <- species[autonym]

  after_name <- rep(NA_character_, nrow(raw))
  prefixed <- !is.na(raw$nom_standard) &
    startsWith(raw$nom_standard, paste0(raw$taxon_name, " "))
  after_name[prefixed] <- trimws(
    substring(raw$nom_standard[prefixed], nchar(raw$taxon_name[prefixed]) + 2)
  )
  authors <- raw$author1
  above_species <- !is_infra & is.na(raw$tax_esp) & is.na(authors)
  authors[above_species] <- after_name[above_species]
  authors[is_infra] <- raw$author2[is_infra]
  authors[!is.na(authors) & !nzchar(authors)] <- NA_character_

  year <- raw$year_description
  year[!is.na(year) & !grepl("^[0-9]{1,4}$", year)] <- NA_character_

  modified <- rep(NA_character_, nrow(raw))
  for (fmt in c("%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M", "%d.%m.%Y")) {
    todo <- is.na(modified) & !is.na(raw$date_modification)
    if (!any(todo)) break
    parsed <- strptime(raw$date_modification[todo], fmt, tz = "UTC")
    modified[todo] <- ifelse(is.na(parsed), NA_character_,
                             format(parsed, "%Y-%m-%d %H:%M:%S"))
  }

  data.frame(
    apd_id            = raw$ID,
    apd_accepted_id   = raw$idtax_good_n,
    apd_parent_id     = raw$id_PARENT,
    taxon_name        = taxon_name,
    nom_standard      = raw$nom_standard,
    tax_level         = level,
    taxrank           = raw$taxrank,
    tax_famclass      = raw$tax_famclass,
    fk_famille        = raw$fk_famille,
    tax_gen           = raw$tax_gen,
    tax_esp           = raw$tax_esp,
    author1           = raw$author1,
    author2           = raw$author2,
    taxon_status      = raw$taxon_status,
    citation          = raw$citation,
    year_description  = year,
    date_modification = modified,
    family            = family,
    species           = species,
    infra_rank        = infra_rank,
    infra_epithet     = infra_epithet,
    authors           = authors,
    apd_version       = rep(version, nrow(raw)),
    stringsAsFactors  = FALSE
  )
}


#' Summarise prepared APD names
#'
#' @param names Data frame from `.prepare_apd_names()`.
#' @param raw_dates Raw `date_modification` values, to count those not parsed.
#' @return Invisibly, a named integer vector of counts.
#' @noRd
.report_apd_names <- function(names, raw_dates) {
  pointer <- names$apd_accepted_id
  has_pointer <- !is.na(pointer)
  counts <- c(
    records            = nrow(names),
    pointers           = sum(has_pointer),
    pointers_dangling  = sum(has_pointer & !pointer %in% names$apd_id),
    pointers_self      = sum(has_pointer & pointer == names$apd_id),
    parents_dangling   = sum(!is.na(names$apd_parent_id) &
                               !names$apd_parent_id %in% names$apd_id),
    without_family     = sum(is.na(names$family)),
    without_authors    = sum(is.na(names$authors)),
    dates_not_parsed   = sum(!is.na(raw_dates) & is.na(names$date_modification))
  )

  top <- function(x, n = 6) {
    tab <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
    lab <- names(tab)
    lab[is.na(lab)] <- "NULL"
    paste0(utils::head(lab, n), " ", utils::head(as.integer(tab), n), collapse = ", ")
  }

  cli::cli_h2("APD export")
  cli::cli_alert_info("Records: {counts[['records']]}")
  cli::cli_alert_info("Ranks: {top(names$tax_level)}")
  cli::cli_alert_info("Status: {top(names$taxon_status)}")
  cli::cli_alert_info(
    "Synonym pointers: {counts[['pointers']]} ({counts[['pointers_dangling']]} to an ID absent from the export, {counts[['pointers_self']]} to the record itself)"
  )
  cli::cli_alert_info("Parents absent from the export: {counts[['parents_dangling']]}")
  cli::cli_alert_info(
    "Without family: {counts[['without_family']]}; without authors: {counts[['without_authors']]}"
  )
  if (counts[["dates_not_parsed"]] > 0) {
    cli::cli_alert_warning(
      "{counts[['dates_not_parsed']]} {.field date_modification} value{?s} not read as a date, stored as NULL"
    )
  }
  invisible(counts)
}


#' Import an African Plant Database export
#'
#' @description
#' Replaces the names in \code{apd_names} with those of an APD export and
#' records the import in \code{backbone_import}. Run it at every new export.
#'
#' Links in \code{taxa_backbone_link} are kept. Links whose APD ID is absent
#' from the new export are reported by \code{check_backbone_links("apd")},
#' run at the end.
#'
#' The export is read as it is sent (tab-separated, Latin-1). APD's
#' \code{idtax_good_n} is its own accepted-name pointer, stored as
#' \code{apd_accepted_id} and followed whatever \code{taxon_status} says.
#' Family names are capitalised as in \code{table_taxa}, rank prefixes above
#' genus are removed from \code{taxon_name}, and the canonical fields
#' (\code{family}, \code{species}, \code{infra_rank}, \code{infra_epithet},
#' \code{authors}) are derived; the raw columns are kept.
#'
#' Requires \code{inst/migrations/apd_backbone.R} on the taxa database.
#'
#' @param file Path to the export. Keep it outside the package.
#' @param version Character. Export version, the date the file was created,
#'   as \code{"YYYY-MM-DD"}. Default: the file's last-modified date, printed
#'   so it can be checked; give it explicitly if the file has been edited.
#' @param con_taxa Connection or pool to the taxa database, with write access.
#'   If \code{NULL}, calls \code{call.mydb.taxa()}.
#' @param encoding Character. Encoding of the export: \code{"Latin-1"}
#'   (default) or \code{"UTF-8"}. A mismatch is detected and refused.
#' @param dry_run Logical. If \code{TRUE} (default), read and check the export
#'   and report what would be imported, without writing.
#' @param force Logical. If \code{TRUE}, import even if this version is already
#'   the current one.
#' @param verbose Logical. Show progress. Default \code{TRUE}.
#'
#' @return Invisibly, a list with \code{version}, \code{record_count},
#'   \code{skipped}, \code{dry_run} and \code{names} (the rows prepared for
#'   \code{apd_names}).
#'
#' @examples
#' \dontrun{
#' con_taxa <- call.mydb.taxa()
#' import_apd_names("path/to/apd_export.txt", con_taxa = con_taxa)  # rehearsal
#' import_apd_names("path/to/apd_export.txt", version = "2026-07-29",
#'                  con_taxa = con_taxa, dry_run = FALSE)
#' }
#'
#' @export
import_apd_names <- function(file,
                             version = NULL,
                             con_taxa = NULL,
                             encoding = c("Latin-1", "UTF-8"),
                             dry_run = TRUE,
                             force = FALSE,
                             verbose = TRUE) {

  encoding <- match.arg(encoding)
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    cli::cli_abort("{.arg file} must be the path of an existing APD export.")
  }
  version <- .apd_export_version(file, version)

  if (verbose) cli::cli_alert_info("Reading {.file {basename(file)}} ({encoding})...")
  raw <- .read_apd_export(file, encoding)
  names_apd <- .prepare_apd_names(raw, version)
  n_total <- nrow(names_apd)

  if (verbose) {
    .report_apd_names(names_apd, raw$date_modification)
    cli::cli_alert_info("Version: {version}")
  }

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()
  info <- .backbone_info("apd", con_taxa)

  result <- list(version = version, record_count = n_total, skipped = FALSE,
                 dry_run = dry_run, names = names_apd)

  current <- .backbone_query(
    con_taxa,
    "SELECT version, record_count FROM backbone_import
      WHERE id_backbone = $1 AND is_current",
    params = list(info$id_backbone)
  )
  if (nrow(current) > 0 && identical(current$version[1], version) && !force) {
    cli::cli_alert_info(
      "APD version {version} already imported ({current$record_count[1]} records). Use {.code force = TRUE} to reimport."
    )
    result$skipped <- TRUE
    return(invisible(result))
  }

  if (dry_run) {
    cli::cli_alert_info(
      "Dry run - nothing was written. Re-run with {.code dry_run = FALSE} to replace {.field apd_names}."
    )
    return(invisible(result))
  }

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }
  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  DBI::dbBegin(actual_con)
  tryCatch({
    DBI::dbWriteTable(actual_con, "tmp_apd_names", names_apd,
                      temporary = TRUE, overwrite = TRUE)

    # links have no foreign key to apd_names, so they survive the replacement
    DBI::dbExecute(actual_con, "DELETE FROM apd_names")
    n_inserted <- DBI::dbExecute(actual_con, "
      INSERT INTO apd_names
             (apd_id, apd_accepted_id, apd_parent_id, taxon_name, nom_standard,
              tax_level, taxrank, tax_famclass, fk_famille, tax_gen, tax_esp,
              author1, author2, taxon_status, citation, year_description,
              date_modification, family, species, infra_rank, infra_epithet,
              authors, apd_version)
      SELECT apd_id::integer, apd_accepted_id::integer, apd_parent_id::integer,
             taxon_name, nom_standard, tax_level, taxrank, tax_famclass,
             fk_famille, tax_gen, tax_esp, author1, author2, taxon_status,
             citation, year_description::integer,
             date_modification::timestamp, family, species, infra_rank,
             infra_epithet, authors, apd_version
        FROM tmp_apd_names")
    if (n_inserted != n_total) {
      stop(sprintf("%d rows inserted, %d expected", n_inserted, n_total))
    }

    DBI::dbExecute(
      actual_con,
      "UPDATE backbone_import SET is_current = false
        WHERE id_backbone = $1 AND is_current",
      params = list(info$id_backbone)
    )
    DBI::dbExecute(
      actual_con,
      "INSERT INTO backbone_import
              (id_backbone, version, imported_by, record_count, source_version, is_current)
       VALUES ($1, $2, $3, $4, $5, true)",
      params = list(info$id_backbone, version, unname(Sys.info()["user"]),
                    n_total, basename(file))
    )

    DBI::dbExecute(actual_con, "DROP TABLE tmp_apd_names")
    DBI::dbCommit(actual_con)
  }, error = function(e) {
    DBI::dbRollback(actual_con)
    cli::cli_abort("APD import failed, nothing was changed: {conditionMessage(e)}")
  })

  if (verbose) {
    cli::cli_alert_success("Imported {n_total} APD names (version {version})")
    check_backbone_links("apd", con_taxa = actual_con)
  }

  invisible(result)
}
