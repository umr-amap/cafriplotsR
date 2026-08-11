# Census table splitting
#
# A field table for a new census mixes stems already known to the database
# (remeasures) with stems seen for the first time (recruits). Which is which
# depends on what the database holds, not on anything readable off the
# spreadsheet, so splitting the table by hand is guesswork — and a mistyped
# tag on a remeasure row silently becomes a duplicate tree.
#
# These functions do the split from the database's own tag list and report
# every case where the answer is doubtful. The logic is pure: pass `existing`
# and no connection is needed.


#' Normalise a tag for matching
#'
#' Tags travel between Excel (often numeric), the database (often integer or
#' text) and free typing, so they have to be compared on a common form.
#' Numeric tags are formatted without scientific notation — `as.character()`
#' would turn a six-digit tag into `"1e+05"` and break every match.
#'
#' @param x Vector of tags.
#' @return Character vector, blanks returned as `NA`.
#' @keywords internal
#' @export
.normalize_tag <- function(x) {
  if (is.numeric(x)) {
    x <- ifelse(
      is.na(x),
      NA_character_,
      format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
    )
  }
  x <- trimws(as.character(x))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}


#' Is `a` `b` with two adjacent characters swapped?
#'
#' Transposing two digits is one of the commonest tag typos, yet it costs 2 in
#' edit distance and so hides below a distance-1 threshold. It gets its own
#' test.
#'
#' @param a,b Single strings.
#' @return `TRUE` when the two differ only by one adjacent swap.
#' @keywords internal
#' @export
.is_adjacent_transposition <- function(a, b) {
  if (length(a) != 1 || length(b) != 1) return(FALSE)
  if (is.na(a) || is.na(b) || nchar(a) != nchar(b) || nchar(a) < 2) return(FALSE)
  ca <- strsplit(a, "")[[1]]
  cb <- strsplit(b, "")[[1]]
  d  <- which(ca != cb)
  length(d) == 2 && d[2] == d[1] + 1 &&
    ca[d[1]] == cb[d[2]] && ca[d[2]] == cb[d[1]]
}


#' Numeric value of a tag, or `NA` when it is not a plain number
#'
#' @param x Character vector of normalised tags.
#' @return Numeric vector.
#' @keywords internal
#' @export
.tag_numeric <- function(x) {
  numeric_like <- !is.na(x) & grepl("^[0-9]+([.][0-9]+)?$", x)
  out <- rep(NA_real_, length(x))
  out[numeric_like] <- as.numeric(x[numeric_like])
  out
}


#' Find the nearest existing tag for each candidate
#'
#' @param candidates Character vector of tags with no exact match.
#' @param pool Character vector of tags known for the same plot.
#' @param max_dist Maximum edit distance to report.
#' @return Data frame with `tag`, `nearest_tag`, `distance` and `transposed`;
#'   `nearest_tag` is `NA` when nothing is close enough.
#' @keywords internal
#' @export
.nearest_tags <- function(candidates, pool, max_dist = 1L) {

  candidates <- as.character(candidates)
  n <- length(candidates)
  out <- data.frame(
    tag         = candidates,
    nearest_tag = rep(NA_character_, n),
    distance    = rep(NA_integer_, n),
    transposed  = rep(FALSE, n),
    stringsAsFactors = FALSE
  )

  pool <- unique(pool[!is.na(pool)])
  if (length(candidates) == 0 || length(pool) == 0) return(out)

  d <- utils::adist(candidates, pool)

  for (i in seq_along(candidates)) {
    if (is.na(candidates[i])) next
    di   <- d[i, ]
    j    <- which.min(di)
    best <- di[j]
    transposed <- FALSE

    # Not near enough on edit distance — look for a swapped pair of characters
    if (best > max_dist) {
      near <- which(di == 2L)
      if (length(near) > 0) {
        hit <- near[vapply(pool[near], .is_adjacent_transposition,
                           logical(1), b = candidates[i])]
        if (length(hit) > 0) {
          j <- hit[1]
          best <- di[j]
          transposed <- TRUE
        }
      }
    }

    if (best <= max_dist || transposed) {
      out$nearest_tag[i] <- pool[j]
      out$distance[i]    <- as.integer(best)
      out$transposed[i]  <- transposed
    }
  }

  out
}


#' Fetch the individuals already recorded for a set of plots
#'
#' Thin database layer behind [split_census_table()]. Kept separate so the
#' splitting logic stays testable without a connection.
#'
#' @param plot_names Character vector of plot names.
#' @param con Database connection or pool.
#' @param with_status Logical. Also fetch each stem's most recent
#'   `stem_status`? Failure to do so is not fatal — `last_status` comes back
#'   as `NA`.
#' @return Data frame with `plot_name`, `id_n`, `tag`, `idtax_n`,
#'   `stem_grouping` and `last_status`.
#' @keywords internal
#' @export
.fetch_plot_individuals <- function(plot_names, con, with_status = TRUE) {

  plot_names <- unique(plot_names[!is.na(plot_names)])

  empty <- data.frame(
    plot_name = character(0), id_n = integer(0), tag = character(0),
    idtax_n = integer(0), stem_grouping = integer(0),
    last_status = character(0), stringsAsFactors = FALSE
  )
  if (length(plot_names) == 0) return(empty)

  is_pool    <- inherits(con, "Pool")
  actual_con <- if (is_pool) pool::poolCheckout(con) else con
  on.exit(if (is_pool) pool::poolReturn(actual_con), add = TRUE)

  individuals <- DBI::dbGetQuery(actual_con, glue::glue_sql(
    "SELECT dlp.plot_name,
            di.id_n,
            di.tag,
            di.idtax_n,
            di.stem_grouping
       FROM data_individuals di
       JOIN data_liste_plots dlp
         ON di.id_table_liste_plots_n = dlp.id_liste_plots
      WHERE dlp.plot_name IN ({plot_names*})",
    plot_names = plot_names, .con = actual_con
  ))

  if (nrow(individuals) == 0) return(empty)
  individuals$last_status <- NA_character_

  if (!with_status) return(individuals)

  # Latest recorded vital status per stem. Optional context for the
  # missing-stem report, never a reason to fail the split.
  status <- tryCatch({
    DBI::dbGetQuery(actual_con, glue::glue_sql(
      "SELECT DISTINCT ON (m.id_data_individuals)
              m.id_data_individuals AS id_n,
              TRIM(m.traitvalue_char) AS last_status
         FROM data_traits_measures m
         LEFT JOIN data_liste_sub_plots sp
           ON m.id_sub_plots = sp.id_sub_plots
        WHERE m.traitid = 107
          AND m.id_data_individuals IN ({ids*})
        ORDER BY m.id_data_individuals,
                 sp.year DESC NULLS LAST,
                 sp.month DESC NULLS LAST",
      ids = as.integer(individuals$id_n), .con = actual_con
    ))
  }, error = function(e) {
    message("Note: could not fetch stem status (", e$message,
            "). Missing stems will be reported without it.")
    NULL
  })

  if (!is.null(status) && nrow(status) > 0) {
    individuals$last_status <- status$last_status[match(individuals$id_n, status$id_n)]
  }

  individuals
}


#' Split a flat census table into remeasures and recruits
#'
#' @description
#' Takes the single flat table a field team actually produces for a new census
#' — existing stems and recruits interleaved — and classifies every row against
#' the individuals already recorded for those plots. The classification comes
#' from the database, not from the user, which is the whole point: a field team
#' has no way of knowing which tags the database already holds.
#'
#' Rows whose tag is suspiciously close to an existing tag are **not** called
#' recruits. They are set aside as `"review"`, because creating a new
#' individual from a mistyped tag is silent and hard to undo, whereas a
#' rejected review row costs one look.
#'
#' @details
#' Roles assigned to each row of `data`:
#' \describe{
#'   \item{`remeasure`}{plot + tag already exists — measurements attach to the
#'     existing `id_n`.}
#'   \item{`recruit`}{tag unknown for that plot and unlike any existing tag —
#'     a new individual.}
#'   \item{`review`}{tag unknown but within `typo_max_dist` of an existing tag,
#'     or an adjacent-character swap away from one, *and* not a continuation of
#'     the plot's numbering (see `assume_new_block`). Needs a human decision.}
#'   \item{`invalid`}{plot name or tag missing.}
#' }
#'
#' Pass `existing` to run without a database — that is the form used by the
#' tests and by anyone working from an exported tag list.
#'
#' @param data Data frame: the flat census table, one row per stem.
#' @param plot_names Character vector restricting the split to these plots.
#'   Rows of `data` for other plots are returned in `out_of_scope`. Defaults to
#'   every plot present in `data`.
#' @param con Database connection or pool. Required unless `existing` is given.
#' @param existing Data frame of individuals already recorded, with at least
#'   `plot_name` and `tag`; `id_n`, `idtax_n` and `last_status` are used when
#'   present. Fetched from `con` when `NULL`.
#' @param plot_col,tag_col,idtax_col Column names in `data`.
#' @param typo_max_dist Maximum edit distance at which an unknown tag is
#'   treated as a possible typo rather than a recruit. `0` disables the check.
#' @param assume_new_block Logical. Treat a numeric tag above every tag already
#'   used in that plot as a genuine recruit, whatever its edit distance.
#'   Recruits are normally tagged by continuing the plot's numbering, so
#'   without this nearly every recruit is one character from its predecessor
#'   and the real typos are lost in the noise. Turn it off for plots whose
#'   tags are not sequential.
#' @param exclude_status Vital statuses to leave out of the missing-stem
#'   report; stems already recorded dead are not expected to reappear.
#'
#' @return An object of class `census_split`, a list with:
#'   \describe{
#'     \item{`data`}{`data` plus `row_id`, `row_role`, `id_n` and `split_note`.}
#'     \item{`remeasures`, `recruits`, `review`, `invalid`}{the four subsets.}
#'     \item{`possible_typos`}{review rows with their nearest existing tag.}
#'     \item{`taxon_drift`}{remeasures whose taxon differs from the database.}
#'     \item{`missing_stems`}{recorded stems with no row in `data`.}
#'     \item{`duplicates`}{plot + tag appearing more than once in `data`.}
#'     \item{`out_of_scope`}{rows for plots outside `plot_names`.}
#'     \item{`summary`}{per-plot counts.}
#'   }
#'
#' @examples
#' existing <- data.frame(
#'   plot_name = c("P1", "P1", "P1"),
#'   tag       = c("101", "102", "103"),
#'   id_n      = c(11L, 12L, 13L),
#'   stringsAsFactors = FALSE
#' )
#' census <- data.frame(
#'   plot_name = c("P1", "P1", "P1", "P1"),
#'   tag       = c("101", "102", "104", "1O3"),
#'   dbh       = c(21.1, 33.4, 10.2, 45.0),
#'   stringsAsFactors = FALSE
#' )
#' split <- split_census_table(census, existing = existing)
#' split
#'
#' # 101 and 102 are remeasures, 104 continues the numbering so it is a
#' # recruit, and 1O3 (letter O) is held for review against tag 103.
#' split$data[, c("tag", "row_role", "id_n")]
#'
#' @seealso [export_census_split()] to write the pieces out for the wizards.
#' @export
split_census_table <- function(data,
                               plot_names     = NULL,
                               con            = NULL,
                               existing       = NULL,
                               plot_col       = "plot_name",
                               tag_col        = "tag",
                               idtax_col      = "idtax_n",
                               typo_max_dist  = 1L,
                               assume_new_block = TRUE,
                               exclude_status = "dead") {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(c(plot_col, tag_col), names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("`data` is missing column(s): %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!is.numeric(typo_max_dist) || length(typo_max_dist) != 1 || typo_max_dist < 0) {
    stop("`typo_max_dist` must be a single non-negative number.", call. = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$row_id <- seq_len(nrow(data))

  file_plot <- trimws(as.character(data[[plot_col]]))
  file_plot[!is.na(file_plot) & !nzchar(file_plot)] <- NA_character_
  file_tag  <- .normalize_tag(data[[tag_col]])

  if (is.null(plot_names)) {
    plot_names <- unique(file_plot[!is.na(file_plot)])
  } else {
    plot_names <- unique(trimws(as.character(plot_names)))
  }

  # Rows for plots the caller did not ask about are set aside, not classified
  in_scope <- !is.na(file_plot) & file_plot %in% plot_names
  out_of_scope <- data[!in_scope & !is.na(file_plot), , drop = FALSE]

  if (is.null(existing)) {
    if (is.null(con)) {
      stop("Supply either `existing` or `con`.", call. = FALSE)
    }
    existing <- .fetch_plot_individuals(plot_names, con)
  }
  existing <- as.data.frame(existing, stringsAsFactors = FALSE)
  if (!all(c("plot_name", "tag") %in% names(existing))) {
    stop("`existing` must have `plot_name` and `tag` columns.", call. = FALSE)
  }
  for (cl in c("id_n", "idtax_n", "last_status")) {
    if (!cl %in% names(existing)) existing[[cl]] <- NA
  }

  db_plot <- trimws(as.character(existing$plot_name))
  db_tag  <- .normalize_tag(existing$tag)
  existing <- existing[db_plot %in% plot_names, , drop = FALSE]
  db_tag   <- db_tag[db_plot %in% plot_names]
  db_plot  <- db_plot[db_plot %in% plot_names]

  file_key <- paste(file_plot, file_tag, sep = "\r")
  db_key   <- paste(db_plot, db_tag, sep = "\r")

  # ---- role assignment ----------------------------------------------------

  role <- rep(NA_character_, nrow(data))
  note <- rep(NA_character_, nrow(data))
  id_n <- rep(NA_integer_, nrow(data))

  role[!in_scope] <- "out_of_scope"
  note[!in_scope] <- "plot not among the plots being imported"

  invalid <- in_scope & (is.na(file_plot) | is.na(file_tag))
  role[is.na(file_plot)] <- "invalid"
  note[is.na(file_plot)] <- "missing plot name"
  role[in_scope & is.na(file_tag)] <- "invalid"
  note[in_scope & is.na(file_tag)] <- "missing tag"

  undecided <- is.na(role)
  hit <- match(file_key, db_key)

  is_remeasure <- undecided & !is.na(hit)
  role[is_remeasure] <- "remeasure"
  id_n[is_remeasure] <- suppressWarnings(
    as.integer(existing$id_n[hit[is_remeasure]])
  )

  candidate <- which(undecided & is.na(hit))
  role[candidate] <- "recruit"

  # ---- typo guard ---------------------------------------------------------

  typos <- data.frame(
    row_id = integer(0), plot_name = character(0), tag = character(0),
    nearest_tag = character(0), distance = integer(0),
    transposed = logical(0), nearest_id_n = integer(0),
    stringsAsFactors = FALSE
  )

  if (typo_max_dist > 0 && length(candidate) > 0) {
    for (pl in unique(file_plot[candidate])) {
      rows <- candidate[file_plot[candidate] == pl]
      pool <- db_tag[db_plot == pl]
      near <- .nearest_tags(file_tag[rows], pool, max_dist = as.integer(typo_max_dist))

      flagged <- !is.na(near$nearest_tag)

      # Recruits are normally tagged by continuing the plot's numbering, so a
      # new tag sits one character from its predecessor by construction —
      # flagging those would bury the real typos. Only tags falling back
      # *inside* the range already used are suspicious.
      if (assume_new_block && any(flagged)) {
        max_existing <- suppressWarnings(max(.tag_numeric(pool), na.rm = TRUE))
        if (is.finite(max_existing)) {
          value <- .tag_numeric(file_tag[rows])
          continues_series <- !is.na(value) & value > max_existing
          flagged <- flagged & !continues_series
        }
      }

      if (!any(flagged)) next

      role[rows[flagged]] <- "review"
      note[rows[flagged]] <- sprintf(
        "tag unknown but %s existing tag %s",
        ifelse(near$transposed[flagged], "a swapped pair away from",
               sprintf("%d character(s) from", near$distance[flagged])),
        near$nearest_tag[flagged]
      )

      typos <- rbind(typos, data.frame(
        row_id       = data$row_id[rows[flagged]],
        plot_name    = pl,
        tag          = file_tag[rows[flagged]],
        nearest_tag  = near$nearest_tag[flagged],
        distance     = near$distance[flagged],
        transposed   = near$transposed[flagged],
        nearest_id_n = suppressWarnings(as.integer(
          existing$id_n[match(paste(pl, near$nearest_tag[flagged], sep = "\r"), db_key)]
        )),
        stringsAsFactors = FALSE
      ))
    }
  }

  data$row_role   <- role
  data$id_n       <- id_n
  data$split_note <- note

  # ---- duplicated stems in the file ---------------------------------------

  dup_keys <- file_key[in_scope & !invalid]
  dup_tbl  <- table(dup_keys)
  dup_tbl  <- dup_tbl[dup_tbl > 1]
  duplicates <- if (length(dup_tbl) == 0) {
    data.frame(plot_name = character(0), tag = character(0),
               n = integer(0), stringsAsFactors = FALSE)
  } else {
    parts <- do.call(rbind, strsplit(names(dup_tbl), "\r", fixed = TRUE))
    data.frame(
      plot_name = parts[, 1], tag = parts[, 2],
      n = as.integer(dup_tbl), stringsAsFactors = FALSE
    )
  }

  # ---- taxon drift on remeasured stems ------------------------------------

  drift <- data.frame(
    row_id = integer(0), plot_name = character(0), tag = character(0),
    idtax_file = character(0), idtax_db = character(0),
    stringsAsFactors = FALSE
  )

  if (idtax_col %in% names(data) && any(!is.na(existing$idtax_n))) {
    rows <- which(data$row_role == "remeasure")
    if (length(rows) > 0) {
      in_file <- trimws(as.character(data[[idtax_col]][rows]))
      in_db   <- trimws(as.character(existing$idtax_n[hit[rows]]))
      differs <- !is.na(in_file) & nzchar(in_file) & !is.na(in_db) & in_file != in_db
      if (any(differs)) {
        drift <- data.frame(
          row_id     = data$row_id[rows[differs]],
          plot_name  = file_plot[rows[differs]],
          tag        = file_tag[rows[differs]],
          idtax_file = in_file[differs],
          idtax_db   = in_db[differs],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # ---- recorded stems absent from the file --------------------------------

  seen <- db_key %in% file_key[in_scope]
  drop_status <- !is.na(existing$last_status) &
    trimws(existing$last_status) %in% exclude_status
  keep <- !seen & !drop_status

  missing_stems <- data.frame(
    plot_name   = db_plot[keep],
    tag         = db_tag[keep],
    id_n        = suppressWarnings(as.integer(existing$id_n[keep])),
    last_status = as.character(existing$last_status[keep]),
    stringsAsFactors = FALSE
  )

  # ---- per-plot summary ---------------------------------------------------

  counts <- function(pl, what) sum(data$row_role[file_plot %in% pl] == what, na.rm = TRUE)
  summary_df <- do.call(rbind, lapply(plot_names, function(pl) data.frame(
    plot_name    = pl,
    n_rows       = sum(file_plot %in% pl, na.rm = TRUE),
    n_remeasure  = counts(pl, "remeasure"),
    n_recruit    = counts(pl, "recruit"),
    n_review     = counts(pl, "review"),
    n_invalid    = counts(pl, "invalid"),
    n_missing    = sum(missing_stems$plot_name == pl),
    n_in_db      = sum(db_plot == pl),
    stringsAsFactors = FALSE
  )))
  if (is.null(summary_df)) {
    summary_df <- data.frame(
      plot_name = character(0), n_rows = integer(0), n_remeasure = integer(0),
      n_recruit = integer(0), n_review = integer(0), n_invalid = integer(0),
      n_missing = integer(0), n_in_db = integer(0), stringsAsFactors = FALSE
    )
  }

  pick <- function(what) data[which(data$row_role == what), , drop = FALSE]

  structure(
    list(
      data           = data,
      remeasures     = pick("remeasure"),
      recruits       = pick("recruit"),
      review         = pick("review"),
      invalid        = pick("invalid"),
      possible_typos = typos,
      taxon_drift    = drift,
      missing_stems  = missing_stems,
      duplicates     = duplicates,
      out_of_scope   = out_of_scope,
      summary        = summary_df
    ),
    class = "census_split"
  )
}


#' @param x A `census_split` object.
#' @param ... Ignored.
#' @rdname split_census_table
#' @export
print.census_split <- function(x, ...) {

  cli::cli_h2("Census table split")
  cli::cli_text("{nrow(x$data)} row{?s} over {nrow(x$summary)} plot{?s}")

  cli::cli_ul()
  cli::cli_li("{nrow(x$remeasures)} remeasure{?s} (stem already in the database)")
  cli::cli_li("{nrow(x$recruits)} recruit{?s} (new individual{?s} to create)")
  cli::cli_end()

  if (nrow(x$review) > 0) {
    cli::cli_alert_warning(paste(
      "{nrow(x$review)} row{?s} held for review — the tag is unknown but",
      "close to an existing one. See `$possible_typos`."
    ))
  }
  if (nrow(x$invalid) > 0) {
    cli::cli_alert_danger("{nrow(x$invalid)} row{?s} without a usable plot name or tag.")
  }
  if (nrow(x$duplicates) > 0) {
    cli::cli_alert_warning("{nrow(x$duplicates)} plot+tag combination{?s} appear{?s/} more than once.")
  }
  if (nrow(x$taxon_drift) > 0) {
    cli::cli_alert_warning("{nrow(x$taxon_drift)} remeasured stem{?s} carr{?ies/y} a different taxon than the database.")
  }
  if (nrow(x$missing_stems) > 0) {
    cli::cli_alert_info("{nrow(x$missing_stems)} recorded stem{?s} ha{?s/ve} no row in this table.")
  }
  if (nrow(x$out_of_scope) > 0) {
    cli::cli_alert_info("{nrow(x$out_of_scope)} row{?s} belong{?s/} to plots outside the split.")
  }

  invisible(x)
}


#' Write a census split out for the import wizards
#'
#' Produces the files the existing wizards already accept, so the split can be
#' used today without waiting on a dedicated import mode: the recruit file
#' goes to `launch_import_wizard()`, the measurement file to
#' `launch_feature_wizard()`'s *Add Individual Measurements* mode.
#'
#' Review rows are written to their own file and are deliberately absent from
#' the recruit file — importing them unchecked is what the split exists to
#' prevent.
#'
#' @param x A `census_split` object from [split_census_table()].
#' @param dir Directory to write into. Created if absent.
#' @param prefix Optional file name prefix.
#' @param overwrite Overwrite existing files?
#' @return Invisibly, the character vector of paths written.
#'
#' @examples
#' \dontrun{
#' split <- split_census_table(census, plot_names = "P1", con = con)
#' export_census_split(split, dir = "census_2026")
#' }
#' @export
export_census_split <- function(x, dir = ".", prefix = NULL, overwrite = FALSE) {

  if (!inherits(x, "census_split")) {
    stop("`x` must be a census_split object from split_census_table().",
         call. = FALSE)
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  pieces <- list(
    "01_recruits"      = x$recruits,
    "02_measurements"  = x$data[x$data$row_role %in% c("remeasure", "recruit"), , drop = FALSE],
    "03_review"        = x$review,
    "04_missing_stems" = x$missing_stems,
    "05_report"        = x$summary
  )

  written <- character(0)
  for (nm in names(pieces)) {
    piece <- pieces[[nm]]
    if (is.null(piece) || nrow(piece) == 0) next

    file <- file.path(dir, paste0(if (is.null(prefix)) "" else paste0(prefix, "_"),
                                  nm, ".xlsx"))
    if (file.exists(file) && !overwrite) {
      cli::cli_alert_warning("{.file {file}} exists — skipped (use overwrite = TRUE)")
      next
    }
    writexl::write_xlsx(dplyr::as_tibble(piece), file)
    written <- c(written, file)
  }

  if (length(written) > 0) {
    cli::cli_alert_success("Wrote {length(written)} file{?s} to {.file {dir}}")
  } else {
    cli::cli_alert_info("Nothing to write.")
  }

  invisible(written)
}
