# Duplicate detection for the specimen import wizard
#
# A specimen is identified by its collector (id_colnam), collection number
# (colnbr) and suffix. The specimens table has no unique constraint on these,
# so the import must refuse rows that are already in the database, and rows
# repeated within the uploaded file.


#' Build a specimen identity key
#'
#' Normalises collector id, collection number and suffix into one string so
#' that values typed differently (`"123"` vs `123`, `"a "` vs `"A"`, `""` vs
#' `NA`) compare equal.
#'
#' @param id_colnam Integer vector of collector ids.
#' @param colnbr Vector of collection numbers (numeric or character).
#' @param suffix Character vector of suffixes, or `NULL` when there are none.
#' @return Character vector of keys, `NA` where collector or number is missing.
#' @keywords internal
#' @noRd
.specimen_key <- function(id_colnam, colnbr, suffix = NULL) {
  n <- length(id_colnam)

  colnbr_chr <- trimws(as.character(colnbr))
  colnbr_num <- suppressWarnings(as.numeric(colnbr_chr))
  is_num <- !is.na(colnbr_num)
  colnbr_chr[is_num] <- sprintf("%.15g", colnbr_num[is_num])
  colnbr_chr[colnbr_chr == ""] <- NA_character_

  if (is.null(suffix)) suffix <- rep(NA_character_, n)
  suffix_chr <- tolower(trimws(as.character(suffix)))
  suffix_chr[is.na(suffix_chr)] <- ""

  key <- paste(id_colnam, colnbr_chr, suffix_chr, sep = "|")
  key[is.na(id_colnam) | is.na(colnbr_chr)] <- NA_character_
  key
}


#' Flag duplicate specimens in an import batch
#'
#' Pure function (no database access): compares the upload with itself and
#' with the specimens already in the database.
#'
#' @param data Data frame of matched upload data, with an `id_colnam` column.
#' @param colnbr_col Name of the column holding the collection number.
#' @param suffix_col Name of the suffix column, or `NULL`.
#' @param existing Data frame of existing specimens with columns
#'   `id_specimen`, `id_colnam`, `colnbr`, `suffix`.
#' @return `data` with added columns:
#'   \describe{
#'     \item{file_line}{Line in the uploaded spreadsheet (header is line 1).}
#'     \item{dup_in_file}{TRUE for every row of a group repeated in the file.}
#'     \item{dup_file_lines}{The file lines of that group, e.g. "3, 7".}
#'     \item{in_db}{TRUE when the specimen is already in the database.}
#'     \item{existing_id_specimen}{The matching id_specimen(s), e.g. "12, 40".}
#'     \item{is_duplicate}{`dup_in_file | in_db`; these rows are not imported.}
#'   }
#' @keywords internal
#' @noRd
.flag_specimen_duplicates <- function(data, colnbr_col, suffix_col = NULL,
                                      existing = NULL) {
  n <- nrow(data)
  key <- .specimen_key(
    data$id_colnam,
    data[[colnbr_col]],
    if (!is.null(suffix_col)) data[[suffix_col]]
  )

  data$file_line <- seq_len(n) + 1L

  # Within-file: every occurrence is flagged, not only the repeats, since
  # nothing tells us which copy carries the right data
  counts <- table(key, useNA = "no")
  data$dup_in_file <- !is.na(key) & key %in% names(counts)[counts > 1]
  lines_by_key <- tapply(data$file_line, key, function(x) paste(x, collapse = ", "))
  data$dup_file_lines <- ifelse(data$dup_in_file, unname(lines_by_key[key]), NA_character_)

  # Against the database
  data$in_db <- rep(FALSE, n)
  data$existing_id_specimen <- rep(NA_character_, n)
  if (!is.null(existing) && nrow(existing) > 0) {
    existing_key <- .specimen_key(existing$id_colnam, existing$colnbr, existing$suffix)
    ids_by_key <- tapply(existing$id_specimen, existing_key,
                         function(x) paste(sort(x), collapse = ", "))
    data$in_db <- !is.na(key) & key %in% names(ids_by_key)
    data$existing_id_specimen[data$in_db] <- unname(ids_by_key[key[data$in_db]])
  }

  data$is_duplicate <- data$dup_in_file | data$in_db
  data
}


#' Fetch the existing specimens of the collectors in an upload
#'
#' @param con DBI connection or pool to the main database.
#' @param id_colnam Collector ids found in the upload.
#' @return Data frame with `id_specimen`, `id_colnam`, `colnbr`, `suffix`.
#' @keywords internal
#' @noRd
.fetch_existing_specimen_keys <- function(con, id_colnam) {
  ids <- unique(stats::na.omit(as.integer(id_colnam)))
  if (length(ids) == 0) {
    return(data.frame(id_specimen = integer(), id_colnam = integer(),
                      colnbr = character(), suffix = character()))
  }

  dplyr::tbl(con, "specimens") %>%
    dplyr::filter(id_colnam %in% !!ids) %>%
    dplyr::select(id_specimen, id_colnam, colnbr, suffix) %>%
    dplyr::collect()
}


#' Check an import batch for duplicate specimens
#'
#' @param data,colnbr_col,suffix_col See [.flag_specimen_duplicates()].
#' @param con DBI connection or pool to the main database.
#' @return The flagged data frame.
#' @keywords internal
#' @noRd
.check_specimen_duplicates <- function(data, colnbr_col, suffix_col, con) {
  existing <- .fetch_existing_specimen_keys(con, data$id_colnam)
  .flag_specimen_duplicates(data, colnbr_col, suffix_col, existing)
}
