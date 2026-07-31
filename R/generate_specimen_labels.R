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
#' @param determination_col Character or \code{NULL}. Optional column holding a
#'   determination / revised identification. Only used when
#'   \code{include_determination = TRUE}.
#' @param include_determination Logical. If \code{TRUE}, an extra line with the
#'   determination is inserted below the scientific name. Default \code{FALSE}
#'   (matches the original template, which leaves this field blank).
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
  determ   <- if (include_determination) get_col(determination_col) else NULL

  ## --- grid geometry ---
  per_page <- n_cols * n_rows
  if (is.null(offset_x)) offset_x <- (page_width  - n_cols * label_width)  / 2
  if (is.null(offset_y)) offset_y <- (page_height - n_rows * label_height) / 2
  max_text_w <- label_width - 2 * pad_x

  ## --- per-label line specification (text, size, fontface, alignment) ---
  ## fontface codes: 1 = plain, 2 = bold, 3 = italic
  build_lines <- function(i) {
    txt   <- c(specimen[i], family_v[i], name_v[i])
    size  <- c(size_specimen, size_family, size_name)
    face  <- c(1L, 2L, 3L)
    align <- c("left", "right", "left")
    if (!is.null(determ)) {
      txt   <- c(txt, determ[i])
      size  <- c(size, size_locality)
      face  <- c(face, 1L)
      align <- c(align, "left")
    }
    txt   <- c(txt, locality[i])
    size  <- c(size, size_locality)
    face  <- c(face, 1L)
    align <- c(align, "left")
    list(txt = txt, size = size, face = face, align = align)
  }

  ## --- shrink a line's font until it fits the label width ---
  fit_size <- function(txt, size, face) {
    if (!shrink_to_fit || !nzchar(txt)) return(size)
    fs <- size
    while (fs > 5) {
      w <- grid::convertWidth(
        grid::grobWidth(grid::textGrob(
          txt,
          gp = grid::gpar(fontsize = fs, fontface = face, fontfamily = family)
        )),
        "mm", valueOnly = TRUE
      )
      if (w <= max_text_w) break
      fs <- fs - 0.5
    }
    fs
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
    fs <- vapply(
      seq_along(ln$txt),
      function(j) fit_size(ln$txt[j], ln$size[j], ln$face[j]),
      numeric(1)
    )
    ## line heights (mm) with a 1.15 leading factor, plus inter-line gap
    lh <- fs / 72 * 25.4 * 1.15
    gap <- 1.1
    total <- sum(lh) + gap * (length(lh) - 1L)
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
