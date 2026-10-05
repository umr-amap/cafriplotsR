#' @title Generate printable specimen labels (A4 sticker sheets)
#'
#' @description
#' Produce a print-ready, multi-page PDF of specimen labels laid out on a
#' 3 x 8 grid of 70 mm x 37 mm cells (24 labels per A4 page), reproducing the
#' formatting of the herbarium label template used for IRD forest-plot
#' vouchers. Each label shows, on vertically-centred lines, the specimen
#' number, the family (bold, right-aligned), the scientific name (italic) and
#' the locality (small font).
#'
#' The function relies only on base R graphics devices (\pkg{grDevices} and
#' \pkg{grid}), which ship with every R installation, so it adds \strong{no
#' external dependency}. Geometry is fully parameterised so the grid can be
#' calibrated against a specific physical sticker sheet.
#'
#' @param data A data.frame (or tibble) with one row per specimen / label.
#' @param output_file Character. Path to the PDF file to create.
#' @param specimen_col,family_col,name_col,locality_col Character. Column names
#'   in \code{data} holding, respectively, the specimen number, family,
#'   scientific name and locality. A column that is absent, wholly \code{NA},
#'   or a literal \code{"NA"} / \code{NaN} text value is rendered as a blank
#'   line, preserving the vertical layout. A stray \code{NA} token embedded in
#'   otherwise valid text (e.g. \code{"Xylopia NA"}, from an upstream
#'   \code{paste(genus, species)} where \code{species} was \code{NA}) is
#'   removed and the surrounding whitespace collapsed; words that merely
#'   contain "NA" (e.g. "Annonaceae") are left untouched.
#' @param determination_col Character or \code{NULL}. Optional column holding
#'   ready-made determination text. When it is present in \code{data}, its
#'   non-empty values are printed as-is and take precedence over
#'   \code{det_cols}. Only used when \code{include_determination = TRUE}.
#' @param include_determination Logical. If \code{TRUE}, an extra line with the
#'   determination is inserted below the scientific name, built from
#'   \code{det_cols} as \code{"det. <determiner> (<date>)"}, e.g.
#'   \code{"det. G. Dauby (mar 2024)"}, with the date formatted as for
#'   \code{include_date}. Missing parts are dropped and the line is left blank
#'   when there is neither a determiner nor a date. Default \code{FALSE}
#'   (matches the original template, which leaves this field blank).
#' @param det_cols Named character vector giving the \code{by} (determiner),
#'   \code{day}, \code{month} and \code{year} columns of the determination.
#'   Defaults to the \code{detby}, \code{detd}, \code{detm}, \code{dety}
#'   columns returned by \code{\link{query_specimens}()}. A missing column is
#'   treated as all \code{NA}.
#' @param include_date Logical. If \code{TRUE}, the collection date is
#'   appended to the specimen line, in parentheses with a lower-case month
#'   abbreviation, e.g. \code{"IRD plot 2508 (12 mar 2024)"}. Missing parts
#'   are dropped: \code{"(mar 2024)"}, \code{"(2024)"}; a day without a month
#'   is dropped too, and nothing is appended when all three are missing.
#'   Default \code{FALSE}.
#' @param date_cols Named character vector giving the \code{day},
#'   \code{month} and \code{year} columns of the collection date. Defaults to
#'   the \code{cold}, \code{colm}, \code{coly} columns returned by
#'   \code{\link{query_specimens}()}. A missing column is treated as all
#'   \code{NA}; out-of-range days or months are ignored.
#' @param extra_cols Character vector or \code{NULL}. Additional columns
#'   (e.g. \code{"description"}) each printed in a small font below the
#'   locality. Long text is word-wrapped onto at most \code{extra_max_lines}
#'   lines and cropped with \code{"..."} where it no longer fits. Default
#'   \code{NULL} (no extra lines).
#' @param extra_max_lines Integer. Maximum number of lines per extra column
#'   (default 2).
#' @param size_extra Numeric. Font size in points of the extra lines
#'   (default 7). Unlike the other lines, extra lines are cropped rather than
#'   shrunk to fit.
#' @param page_width,page_height Numeric. Page size in millimetres
#'   (default A4: 210 x 297).
#' @param n_cols,n_rows Integer. Number of label columns and rows per page
#'   (default 3 x 8 = 24 labels per page).
#' @param label_width,label_height Numeric. Label (cell) size in millimetres
#'   (default 70 x 37).
#' @param offset_x,offset_y Numeric or \code{NULL}. Millimetre offset of the
#'   label grid from the top-left corner of the page, for fine calibration
#'   against a physical sticker sheet. When \code{NULL} (default) the grid is
#'   centred on the page.
#' @param pad_x Numeric. Horizontal text inset inside each label, in
#'   millimetres (default 3.7, matching the template's paragraph indent).
#' @param family Character. Font family. Default \code{"Calibri"}, matching the
#'   original template (requires \code{device = "cairo_pdf"}, the default, and
#'   that the font be installed). On the base \code{"pdf"} device, which only
#'   knows its built-in PostScript families, an unavailable font is replaced by
#'   \code{"sans"} with a message.
#' @param size_specimen,size_family,size_name,size_locality Numeric. Font sizes
#'   in points for the four lines (defaults 11, 12, 12, 8).
#' @param shrink_to_fit Logical. If \code{TRUE} (default), the font size of any
#'   line whose text is wider than the label is reduced until it fits,
#'   preventing overflow onto neighbouring stickers.
#' @param draw_guides Logical. If \code{TRUE}, thin rectangles are drawn around
#'   each label (useful for calibration or when printing on plain paper).
#'   Default \code{FALSE} (borderless, like the template).
#' @param device Character. Graphics device: \code{"cairo_pdf"} (default,
#'   better system-font support) or \code{"pdf"}. Falls back to \code{"pdf"}
#'   automatically if cairo is unavailable.
#'
#' @return (Invisibly) the path to the created PDF file.
#'
#' @details
#' Labels are filled left-to-right then top-to-bottom, matching the original
#' Word mail-merge output. The number of pages is
#' \code{ceiling(nrow(data) / (n_cols * n_rows))}; the last page is padded with
#' blank cells as needed.
#'
#' To reproduce the original template's font on Windows, use
#' \code{device = "cairo_pdf", family = "Calibri"}.
#'
#' @examples
#' df <- data.frame(
#'   Specimen = c("IRD plot 2508", "IRD plot 2509"),
#'   Family   = c("Fabaceae", "Annonaceae"),
#'   Name     = c("Amphimas pterocarpoides", "Uvariastrum pierreanum"),
#'   Locality = c("mpem008", "mpem008")
#' )
#' out <- tempfile(fileext = ".pdf")
#' generate_specimen_labels(df, out)
#'
#' # With the collection date and a (cropped) description, using the
#' # column names returned by query_specimens()
#' df$cold <- c(12, NA)
#' df$colm <- c(3, 11)
#' df$coly <- c(2024, 2023)
#' df$description <- c(
#'   "Tree 25 m, bark smooth, slash red with abundant white latex, fruits green",
#'   NA
#' )
#' df$detby <- c("G. Dauby", NA)
#' df$detm <- c(5, NA)
#' df$dety <- c(2025, NA)
#' generate_specimen_labels(df, out, include_date = TRUE,
#'                          include_determination = TRUE,
#'                          extra_cols = "description")
#'
#' @export
generate_specimen_labels <- function(
    data,
    output_file,
    specimen_col = "Specimen",
    family_col = "Family",
    name_col = "Name",
    locality_col = "Locality",
    determination_col = "Determination",
    include_determination = FALSE,
    det_cols = c(by = "detby", day = "detd", month = "detm", year = "dety"),
    include_date = FALSE,
    date_cols = c(day = "cold", month = "colm", year = "coly"),
    extra_cols = NULL,
    extra_max_lines = 2L,
    size_extra = 7,
    page_width = 210,
    page_height = 297,
    n_cols = 3L,
    n_rows = 8L,
    label_width = 70,
    label_height = 37,
    offset_x = NULL,
    offset_y = NULL,
    pad_x = 3.7,
    family = "Calibri",
    size_specimen = 11,
    size_family = 12,
    size_name = 12,
    size_locality = 8,
    shrink_to_fit = TRUE,
    draw_guides = FALSE,
    device = c("cairo_pdf", "pdf")) {

  device <- match.arg(device)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (missing(output_file) || !is.character(output_file) ||
      length(output_file) != 1L) {
    stop("`output_file` must be a single file path.", call. = FALSE)
  }
  if (dir.exists(output_file)) {
    stop("`output_file` must be a path to a file, not a directory: ",
         output_file, call. = FALSE)
  }
  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    stop("Output directory does not exist: ", out_dir, call. = FALSE)
  }

  n <- nrow(data)
  if (n == 0L) {
    stop("`data` has no rows.", call. = FALSE)
  }

  ## --- pull a column as clean character, blanks for missing / NA ---
  ## `is.na()` is checked on the raw column first (catches NA of any type,
  ## including NaN, which `as.character()` would otherwise turn into the
  ## literal string "NaN" rather than NA_character_). Stray "NA" tokens
  ## embedded in otherwise non-missing text (e.g. from an upstream
  ## `paste(genus, species)` where species was NA, producing "Xylopia NA")
  ## are also stripped, without touching words that merely contain "NA"
  ## (e.g. "Annonaceae").
  get_col <- function(col) {
    if (is.null(col) || !col %in% names(data)) {
      return(rep("", n))
    }
    raw <- data[[col]]
    is_missing <- is.na(raw)
    v <- as.character(raw)
    v[is_missing] <- ""
    v <- gsub("(?<![A-Za-z])(NA|NaN|<NA>)(?![A-Za-z])", "", v, perl = TRUE)
    v <- gsub("\\s+", " ", v)
    trimws(v)
  }

  specimen <- get_col(specimen_col)
  family_v <- get_col(family_col)
  name_v   <- get_col(name_col)
  locality <- get_col(locality_col)
  extras   <- lapply(as.character(extra_cols), get_col)

  ## raw column looked up by role (e.g. "day") in a named vector of columns
  raw_col <- function(cols, key) {
    col <- unname(cols[key])
    if (length(col) != 1L || is.na(col) || !col %in% names(data)) {
      return(rep(NA, n))
    }
    data[[col]]
  }

  determ <- NULL
  if (include_determination) {
    det_by <- get_col(unname(det_cols["by"]))
    det_date <- .format_label_date(raw_col(det_cols, "day"),
                                   raw_col(det_cols, "month"),
                                   raw_col(det_cols, "year"))
    det_date[nzchar(det_date)] <- paste0("(", det_date[nzchar(det_date)], ")")
    determ <- trimws(paste(det_by, det_date))
    determ[nzchar(determ)] <- paste("det.", determ[nzchar(determ)])
    ## ready-made text, where provided, wins
    free_txt <- get_col(determination_col)
    determ[nzchar(free_txt)] <- free_txt[nzchar(free_txt)]
  }

  if (include_date) {
    date_txt <- .format_label_date(raw_col(date_cols, "day"),
                                   raw_col(date_cols, "month"),
                                   raw_col(date_cols, "year"))
    has_date <- nzchar(date_txt)
    specimen[has_date] <- trimws(
      paste0(specimen[has_date], " (", date_txt[has_date], ")")
    )
  }

  ## --- grid geometry ---
  per_page <- n_cols * n_rows
  if (is.null(offset_x)) offset_x <- (page_width  - n_cols * label_width)  / 2
  if (is.null(offset_y)) offset_y <- (page_height - n_rows * label_height) / 2
  max_text_w <- label_width - 2 * pad_x

  ## --- per-label line specification (text, size, fontface, alignment) ---
  ## fontface codes: 1 = plain, 2 = bold, 3 = italic
  ## `crop` marks lines that are wrapped/cropped instead of shrunk to fit.
  build_lines <- function(i) {
    txt   <- c(specimen[i], family_v[i], name_v[i])
    size  <- c(size_specimen, size_family, size_name)
    face  <- c(1L, 2L, 3L)
    align <- c("left", "right", "left")
    crop  <- c(FALSE, FALSE, FALSE)
    if (!is.null(determ)) {
      txt   <- c(txt, determ[i])
      size  <- c(size, size_locality)
      face  <- c(face, 1L)
      align <- c(align, "left")
      crop  <- c(crop, FALSE)
    }
    txt   <- c(txt, locality[i])
    size  <- c(size, size_locality)
    face  <- c(face, 1L)
    align <- c(align, "left")
    crop  <- c(crop, FALSE)
    for (ex in extras) {
      ## an empty extra line would only waste vertical space
      if (!nzchar(ex[i])) next
      txt   <- c(txt, ex[i])
      size  <- c(size, size_extra)
      face  <- c(face, 1L)
      align <- c(align, "left")
      crop  <- c(crop, TRUE)
    }
    list(txt = txt, size = size, face = face, align = align, crop = crop)
  }

  text_w <- function(txt, fs, face) {
    grid::convertWidth(
      grid::grobWidth(grid::textGrob(
        txt,
        gp = grid::gpar(fontsize = fs, fontface = face, fontfamily = family)
      )),
      "mm", valueOnly = TRUE
    )
  }

  ## --- shrink a line's font until it fits the label width ---
  fit_size <- function(txt, size, face) {
    if (!shrink_to_fit || !nzchar(txt)) return(size)
    fs <- size
    while (fs > 5) {
      if (text_w(txt, fs, face) <= max_text_w) break
      fs <- fs - 0.5
    }
    fs
  }

  ## --- longest prefix of `txt` (in characters) that fits, given a suffix ---
  fit_chars <- function(txt, fs, face, suffix = "") {
    lo <- 0L
    hi <- nchar(txt)
    while (lo < hi) {
      mid <- (lo + hi + 1L) %/% 2L
      if (text_w(paste0(substr(txt, 1L, mid), suffix), fs, face) <= max_text_w) {
        lo <- mid
      } else {
        hi <- mid - 1L
      }
    }
    lo
  }

  ## --- crop a string with an ellipsis so that it fits the label width ---
  crop_line <- function(txt, fs, face) {
    if (text_w(txt, fs, face) <= max_text_w) return(txt)
    k <- fit_chars(txt, fs, face, suffix = "...")
    paste0(trimws(substr(txt, 1L, k), which = "right"), "...")
  }

  ## --- word-wrap onto at most `extra_max_lines` lines, cropping the last ---
  wrap_crop <- function(txt, fs, face) {
    max_lines <- max(1L, as.integer(extra_max_lines))
    words <- strsplit(txt, " ", fixed = TRUE)[[1]]
    out <- character(0)
    while (length(words) > 0L) {
      if (length(out) == max_lines - 1L) {
        out <- c(out, crop_line(paste(words, collapse = " "), fs, face))
        break
      }
      k <- 1L
      while (k < length(words) &&
             text_w(paste(words[seq_len(k + 1L)], collapse = " "),
                    fs, face) <= max_text_w) {
        k <- k + 1L
      }
      line <- paste(words[seq_len(k)], collapse = " ")
      words <- words[-seq_len(k)]
      if (k == 1L && text_w(line, fs, face) > max_text_w) {
        ## a single word wider than the label: hard-break it
        cut <- max(1L, fit_chars(line, fs, face))
        words <- c(substr(line, cut + 1L, nchar(line)), words)
        line <- substr(line, 1L, cut)
      }
      out <- c(out, line)
    }
    out
  }

  ## --- draw one label in cell (col, row), both 0-based, top-left origin ---
  draw_label <- function(i, col, row) {
    x_left <- offset_x + col * label_width
    y_top  <- page_height - (offset_y + row * label_height)
    vp <- grid::viewport(
      x = grid::unit(x_left, "mm"), y = grid::unit(y_top, "mm"),
      width = grid::unit(label_width, "mm"),
      height = grid::unit(label_height, "mm"),
      just = c("left", "top"), clip = "on"
    )
    grid::pushViewport(vp)
    on.exit(grid::popViewport(), add = TRUE)

    if (draw_guides) {
      grid::grid.rect(gp = grid::gpar(col = "grey70", fill = NA, lwd = 0.3))
    }

    ln <- build_lines(i)
    ## expand croppable lines into their wrapped sub-lines
    if (any(ln$crop)) {
      pieces <- lapply(seq_along(ln$txt), function(j) {
        if (ln$crop[j]) wrap_crop(ln$txt[j], ln$size[j], ln$face[j])
        else ln$txt[j]
      })
      reps <- lengths(pieces)
      ln <- list(
        txt   = unlist(pieces, use.names = FALSE),
        size  = rep(ln$size, reps),
        face  = rep(ln$face, reps),
        align = rep(ln$align, reps),
        crop  = rep(ln$crop, reps)
      )
    }
    fs <- vapply(
      seq_along(ln$txt),
      function(j) {
        if (ln$crop[j]) ln$size[j]
        else fit_size(ln$txt[j], ln$size[j], ln$face[j])
      },
      numeric(1)
    )
    ## line heights (mm) with a 1.15 leading factor, plus inter-line gap
    lh <- fs / 72 * 25.4 * 1.15
    gap <- 1.1
    total <- sum(lh) + gap * (length(lh) - 1L)
    ## too tall for the cell (keeping a 1.5 mm margin top and bottom against
    ## sheet misalignment): drop trailing extra lines, the core lines stay
    while (total > label_height - 3 && utils::tail(ln$crop, 1L)) {
      keep <- seq_len(length(lh) - 1L)
      ln <- lapply(ln, `[`, keep)
      fs <- fs[keep]
      lh <- lh[keep]
      total <- sum(lh) + gap * (length(lh) - 1L)
    }
    cursor <- (label_height - total) / 2   # distance from top of cell

    for (j in seq_along(ln$txt)) {
      y_center <- label_height - (cursor + lh[j] / 2)
      left <- ln$align[j] == "left"
      grid::grid.text(
        ln$txt[j],
        x = grid::unit(if (left) pad_x else label_width - pad_x, "mm"),
        y = grid::unit(y_center, "mm"),
        just = c(if (left) "left" else "right", "centre"),
        gp = grid::gpar(fontsize = fs[j], fontface = ln$face[j],
                        fontfamily = family)
      )
      cursor <- cursor + lh[j] + gap
    }
  }

  ## --- open the PDF device ---
  w_in <- page_width / 25.4
  h_in <- page_height / 25.4
  cairo_ok <- isTRUE(unname(capabilities("cairo")))
  if (device == "cairo_pdf" && !cairo_ok) {
    warning("cairo is not available; falling back to pdf(). Non-standard ",
            "fonts (e.g. 'Calibri') may be substituted.", call. = FALSE)
  }
  if (device == "cairo_pdf" && cairo_ok) {
    grDevices::cairo_pdf(filename = output_file, width = w_in, height = h_in,
                         onefile = TRUE)
  } else {
    ## Base pdf() only knows its registered PostScript families; a system font
    ## name such as "Calibri" would raise an error, so fall back to "sans".
    if (!family %in% names(grDevices::pdfFonts())) {
      message("Font '", family, "' is not available on the pdf() device; ",
              "using 'sans' instead. Use device = \"cairo_pdf\" to keep '",
              family, "'.")
      family <- "sans"
    }
    grDevices::pdf(file = output_file, width = w_in, height = h_in,
                   onefile = TRUE, paper = "special")
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  ## --- draw all pages ---
  n_pages <- ceiling(n / per_page)
  for (p in seq_len(n_pages)) {
    grid::grid.newpage()
    start <- (p - 1L) * per_page + 1L
    end   <- min(p * per_page, n)
    for (i in start:end) {
      slot <- (i - 1L) %% per_page   # 0-based position on the page
      draw_label(i, col = slot %% n_cols, row = slot %/% n_cols)
    }
  }

  invisible(output_file)
}

#' Format a collection date for a specimen label
#'
#' Vectorised. Builds \code{"12 mar 2024"}-style strings from separate day,
#' month and year values, any of which may be \code{NA}. Non-numeric or
#' out-of-range values are treated as missing, and a day is dropped when the
#' month is missing (a bare day number is meaningless). Rows with nothing to
#' show give \code{""}.
#'
#' @param day,month,year Vectors of the same length (numeric or coercible).
#' @return A character vector.
#' @keywords internal
#' @noRd
.format_label_date <- function(day, month, year) {
  as_int <- function(x) suppressWarnings(as.integer(as.character(x)))
  d <- as_int(day)
  m <- as_int(month)
  y <- as_int(year)
  m[!is.na(m) & (m < 1L | m > 12L)] <- NA_integer_
  d[!is.na(d) & (d < 1L | d > 31L)] <- NA_integer_
  d[is.na(m)] <- NA_integer_
  y[!is.na(y) & y <= 0L] <- NA_integer_
  mon <- tolower(month.abb[m])
  parts <- cbind(as.character(d), mon, as.character(y))
  apply(parts, 1L, function(p) paste(p[!is.na(p)], collapse = " "))
}
