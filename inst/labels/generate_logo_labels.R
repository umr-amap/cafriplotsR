## generate_logo_labels.R -------------------------------------------------
##
## Draft / standalone script - NOT part of the CafriplotsR package yet.
##
## This is a companion to R/generate_specimen_labels.R, reusing the same
## base-graphics (grid + grDevices) approach, but for a foldable, cuttable
## label sheet. The A4 page (kept in vertical / portrait orientation) is
## divided into 4 equal quarters stacked top to bottom: quarters 1 and 3
## are left blank, quarters 2 and 4 carry the printed label. The sheet is
## meant to first be cut in half (a solid guide line at the page's
## mid-height, between quarters 2 and 3) into two A5 halves - each half
## being a blank quarter over a filled quarter, exactly like a single-unit
## sheet - which the user then folds in two (a dashed guide line at the
## blank/filled boundary within each half) so only the filled quarter
## remains visible. One A4 page therefore yields two box labels. Each
## filled quarter shows a logo on the left and, to its right, a bold title,
## then a rule, then a bold "Family: " prompt followed by four more
## prompts - "Collection: ", "Country: ", "Responsable: " and "Action: " -
## stacked one per line below it, left blank for hand-writing after
## printing.
##
## It deliberately lives in inst/labels/ rather than R/, so it is installed
## with the package but is NOT sourced, collated or exported - it is not
## yet on the package's search path. To try it out:
##
##   source(system.file("labels", "generate_logo_labels.R",
##                       package = "CafriplotsR"))
##   # or, from a local checkout:
##   source("inst/labels/generate_logo_labels.R")
##
## Embedding the logo requires the 'png' package for a .png file, the
## 'jpeg' package for a .jpg/.jpeg file, or the 'magick' package for any
## other raster format. None of these are listed in DESCRIPTION since the
## function is not (yet) part of the package.

#' @title Generate foldable, cuttable logo + title labels (two per A4 sheet)
#'
#' @description
#' Produce a print-ready, multi-page PDF with two labels per A4 page, kept
#' in vertical (portrait) orientation. Each page is divided into 4 equal
#' quarters, stacked top to bottom: quarters 1 and 3 are left blank,
#' quarters 2 and 4 carry the printed label. A solid guide line at the
#' page's mid-height marks where to cut the sheet in half first; a dashed
#' guide line within each half then marks where to fold it in two, so only
#' the filled quarter remains visible - e.g. wrapped around a specimen
#' packet. One A4 page therefore yields two box labels.
#'
#' Each filled quarter shows a logo on the left and, to its right, a bold
#' title (from \code{data}), then a rule, then a bold \code{family_label}
#' prompt followed by four more static prompts - \code{collection_label},
#' \code{country_label}, \code{responsable_label} and \code{action_label}
#' - stacked one per line, left blank after the colon for hand-writing once
#' printed.
#'
#' @param data A data.frame (or tibble) with one row per label.
#' @param logo_path Character. Path to a logo image (.png, .jpg/.jpeg, or
#'   any format readable by \pkg{magick} if installed).
#' @param output_file Character. Path to the PDF file to create.
#' @param title_col Character. Column name in \code{data} holding the bold
#'   title text for each label. A column that is absent, wholly \code{NA},
#'   or a literal \code{"NA"} / \code{NaN} text value is rendered blank.
#' @param family_label Character. Static prompt printed in bold, directly
#'   below the rule that follows the title (default \code{"Family: "});
#'   same on every label and left blank for hand-writing after printing.
#' @param collection_label,country_label,responsable_label,action_label
#'   Character. Static prompts printed as-is, below \code{family_label},
#'   in this order (default \code{"Collection: "}, \code{"Country: "},
#'   \code{"Responsable: "} and \code{"Action: "}); these are the same on
#'   every label and are left blank for hand-writing after printing.
#' @param page_width,page_height Numeric. Page size in millimetres. Default
#'   A4 portrait (210 x 297); \code{page_height} should exceed
#'   \code{page_width} to keep the sheet in vertical orientation.
#' @param n_per_page Integer. Number of label units stacked per A4 page
#'   (default 2, i.e. 4 quarters: blank / filled / blank / filled). The
#'   page is cut into \code{n_per_page} equal-height units first, each of
#'   which is in turn blank on top and filled at the bottom.
#' @param content_frac Numeric in (0, 1). Fraction of each unit's height,
#'   at the bottom of the unit, that carries the printed label (default
#'   \code{0.5}, i.e. one quarter of the page). The remaining fraction, at
#'   the top of the unit, is left blank.
#' @param margin_x Numeric. Left/right margin within the visible (filled)
#'   part of a unit, in millimetres (default 8).
#' @param margin_y Numeric. Fixed gap between the fold line and the top of
#'   the logo/text block, in millimetres (default 5); the block hangs down
#'   from there, and any leftover room collects at the bottom of the unit
#'   (it is not a symmetric top+bottom inset).
#' @param logo_width_frac Numeric in (0, 1). Fraction of the visible part's
#'   width reserved for the logo (default 0.22). The logo is scaled to fit
#'   this box, preserving its aspect ratio, and centred both ways.
#' @param pad_x Numeric. Inner horizontal padding in millimetres, applied
#'   around the logo and around the text block (default 4).
#' @param line_gap Numeric. Vertical gap between stacked lines (title and
#'   the five prompts), in millimetres (default 2).
#' @param family Character. Font family, as in
#'   \code{generate_specimen_labels()} (default \code{"Calibri"}; requires
#'   \code{device = "cairo_pdf"} and the font installed).
#' @param size_title Numeric. Base font size (points) for the title
#'   (default 24).
#' @param size_family Numeric. Base font size (points) for the bold
#'   \code{family_label} prompt below the title (default 20).
#' @param size_field Numeric. Base font size (points) for the four
#'   remaining static field prompts (default 18).
#' @param shrink_to_fit Logical. If \code{TRUE} (default), the title's and
#'   field prompts' fonts are shrunk together (down to a 5 pt floor) if a
#'   line is still too wide, or the wrapped block too tall, after
#'   word-wrapping (see Details).
#' @param draw_fold_line Logical. If \code{TRUE} (default), a dashed guide
#'   line is drawn within each unit, at the blank/filled boundary.
#' @param draw_cut_line Logical. If \code{TRUE} (default), a solid guide
#'   line is drawn at each boundary between stacked units (e.g. the page's
#'   mid-height when \code{n_per_page = 2}), marking where to cut.
#' @param draw_field_lines Logical. If \code{TRUE} (default), a short
#'   writing line is drawn after each of the "Family: " / "Collection: " /
#'   "Country: " / "Responsable: " / "Action: " prompts, to hand-write the
#'   value once printed.
#' @param draw_title_rule Logical. If \code{TRUE} (default), a horizontal
#'   rule is drawn across the text column, separating the (bold) title
#'   from \code{family_label} and the field prompts stacked below it.
#' @param draw_guides Logical. If \code{TRUE}, thin rectangles are drawn
#'   around each unit's visible part and its logo box (useful for
#'   calibration). Default \code{FALSE}.
#' @param device Character. Graphics device: \code{"cairo_pdf"} (default)
#'   or \code{"pdf"}. Falls back to \code{"pdf"} automatically if cairo is
#'   unavailable.
#'
#' @return (Invisibly) the path to the created PDF file.
#'
#' @details
#' One page is produced per row of \code{data}, with that same label
#' duplicated into every unit on the page (e.g. both the top and bottom
#' quarter, for the default \code{n_per_page = 2}) - cutting the sheet
#' therefore yields two identical copies of the label. Cut each printed
#' sheet along the solid guide line(s) first, then fold each resulting
#' piece at its dashed guide line so the blank quarter tucks away and only
#' the printed quarter remains visible. Within that printed quarter, the
#' bold title, the bold "Family: " prompt and each of the "Collection: " /
#' "Country: " / "Responsable: " / "Action: " prompts are individually
#' word-wrapped onto as many lines as needed to fit the text column's
#' width (never splitting a word), then stacked title first, one line per
#' line, with a rule drawn right after the title and "Family: " stacked
#' immediately below that rule, followed by the remaining field prompts.
#' If \code{shrink_to_fit} is \code{TRUE} and the wrapped block still
#' overflows - a single overly long word, or too many wrapped lines in
#' total - the title, family and field font sizes are all reduced together
#' (down to a 5 pt floor) and everything is re-wrapped.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Title = c("IRD plot 2508", "IRD plot 2509"))
#' out <- tempfile(fileext = ".pdf")
#' generate_logo_labels(df, "vignettes/images/logo_ird.png", out)
#' }
generate_logo_labels <- function(
    data,
    logo_path,
    output_file,
    title_col = "Title",
    collection_label = "Collection: ",
    country_label = "Country: ",
    responsable_label = "Responsable: ",
    action_label = "Action: ",
    family_label = "Family: ",
    page_width = 210,
    page_height = 297,
    n_per_page = 2L,
    content_frac = 0.5,
    margin_x = 3,
    margin_y = 2,
    logo_width_frac = 0.22,
    pad_x = 4,
    line_gap = 2,
    family = "Calibri",
    size_title = 24,
    size_family = 20,
    size_field = 18,
    shrink_to_fit = TRUE,
    draw_fold_line = TRUE,
    draw_cut_line = TRUE,
    draw_field_lines = TRUE,
    draw_title_rule = TRUE,
    draw_guides = FALSE,
    device = c("cairo_pdf", "pdf")) {

  device <- match.arg(device)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (missing(logo_path) || !is.character(logo_path) ||
      length(logo_path) != 1L || !file.exists(logo_path)) {
    stop("`logo_path` must point to an existing image file.", call. = FALSE)
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
  if (page_height < page_width) {
    warning("`page_height` is smaller than `page_width`; this template is ",
            "designed for a page in vertical (portrait) orientation.",
            call. = FALSE)
  }
  if (content_frac <= 0 || content_frac >= 1) {
    stop("`content_frac` must be strictly between 0 and 1.", call. = FALSE)
  }
  if (logo_width_frac <= 0 || logo_width_frac >= 1) {
    stop("`logo_width_frac` must be strictly between 0 and 1.", call. = FALSE)
  }
  if (length(n_per_page) != 1L || n_per_page < 1L || n_per_page != round(n_per_page)) {
    stop("`n_per_page` must be a single positive integer.", call. = FALSE)
  }
  n_per_page <- as.integer(n_per_page)

  n <- nrow(data)
  if (n == 0L) {
    stop("`data` has no rows.", call. = FALSE)
  }

  ## --- pull a column as clean character, blanks for missing / NA ---
  ## Same NA-stripping logic as generate_specimen_labels(): drop stray
  ## "NA" / "NaN" tokens produced by upstream paste()-ing without touching
  ## words that merely contain "NA" (e.g. "Annonaceae").
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

  title_v <- get_col(title_col)

  ## --- load the logo as a raster, whatever its format ---
  read_logo_raster <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext == "png") {
      if (!requireNamespace("png", quietly = TRUE)) {
        stop("Reading a .png logo requires the 'png' package.", call. = FALSE)
      }
      img <- png::readPNG(path)
      dims <- dim(img)
      return(list(raster = img, w_px = dims[2], h_px = dims[1]))
    }
    if (ext %in% c("jpg", "jpeg")) {
      if (!requireNamespace("jpeg", quietly = TRUE)) {
        stop("Reading a .jpg/.jpeg logo requires the 'jpeg' package.",
             call. = FALSE)
      }
      img <- jpeg::readJPEG(path)
      dims <- dim(img)
      return(list(raster = img, w_px = dims[2], h_px = dims[1]))
    }
    if (!requireNamespace("magick", quietly = TRUE)) {
      stop("Reading a '.", ext, "' logo requires the 'magick' package ",
           "(or convert it to .png / .jpg first).", call. = FALSE)
    }
    img <- magick::image_read(path)
    ras <- as.raster(img)
    dims <- dim(ras)
    list(raster = ras, w_px = dims[2], h_px = dims[1])
  }

  logo <- read_logo_raster(logo_path)
  logo_aspect <- logo$w_px / logo$h_px

  ## --- fit a box of size (max_w, max_h) around an image, preserving AR ---
  fit_in_box <- function(aspect, max_w, max_h) {
    if (max_w / max_h > aspect) {
      h <- max_h
      w <- h * aspect
    } else {
      w <- max_w
      h <- w / aspect
    }
    c(w = w, h = h)
  }

  ## --- geometry: the page is split into `n_per_page` stacked units; the
  ## bottom `content_frac` of each unit is its visible (filled) part ---
  unit_height    <- page_height / n_per_page
  content_height <- unit_height * content_frac
  band_width     <- page_width - 2 * margin_x
  ## `margin_y` is a top-only pad below the fold line; content hangs down
  ## from there and any leftover room collects at the bottom of the unit.
  band_height    <- content_height - margin_y
  logo_box_w     <- band_width * logo_width_frac
  text_zone_w    <- band_width - logo_box_w
  text_x_left    <- margin_x + logo_box_w + pad_x
  text_max_w     <- text_zone_w - 2 * pad_x

  ## --- measure a string's rendered width, in mm ---
  text_width_mm <- function(txt, size, face = 1L) {
    grid::convertWidth(
      grid::grobWidth(grid::textGrob(
        txt, gp = grid::gpar(fontsize = size, fontface = face,
                              fontfamily = family)
      )),
      "mm", valueOnly = TRUE
    )
  }

  ## --- break `txt` into as few word-wrapped lines as fit `max_w`, at font
  ## size `size` (never splits a single word) ---
  wrap_to_width <- function(txt, size, face, max_w) {
    words <- strsplit(txt, "\\s+")[[1]]
    words <- words[nzchar(words)]
    if (length(words) == 0L) return("")
    lines <- character(0)
    current <- words[1]
    for (w in words[-1]) {
      candidate <- paste(current, w)
      if (text_width_mm(candidate, size, face) <= max_w) {
        current <- candidate
      } else {
        lines <- c(lines, current)
        current <- w
      }
    }
    c(lines, current)
  }

  ## --- wrap the title, the bold family prompt and the field prompts to
  ## fit `max_w`, shrinking all three font sizes together (if allowed) when
  ## a wrapped line is still too wide, or the whole wrapped block is too
  ## tall to fit `avail_h` ---
  fit_and_wrap_block <- function(title_txt, family_txt, field_txts,
                                  title_size, family_size, field_size,
                                  max_w, avail_h) {
    fs_title <- title_size
    fs_family <- family_size
    fs_field <- field_size
    repeat {
      title_lines <- if (nzchar(title_txt)) {
        wrap_to_width(title_txt, fs_title, 2L, max_w)
      } else {
        ""
      }
      family_lines <- if (nzchar(family_txt)) {
        wrap_to_width(family_txt, fs_family, 2L, max_w)
      } else {
        ""
      }
      field_lines <- lapply(field_txts, function(txt) {
        if (nzchar(txt)) wrap_to_width(txt, fs_field, 1L, max_w) else ""
      })

      n_title <- length(title_lines)
      n_family <- length(family_lines)
      n_field <- sum(lengths(field_lines))
      lh_title <- fs_title / 72 * 25.4 * 1.15
      lh_family <- fs_family / 72 * 25.4 * 1.15
      lh_field <- fs_field / 72 * 25.4 * 1.15
      total_h <- n_title * lh_title + n_family * lh_family +
        n_field * lh_field +
        line_gap * (n_title + n_family + n_field - 1L)

      max_w_title <- if (n_title) {
        max(vapply(title_lines, text_width_mm, numeric(1),
                    size = fs_title, face = 2L))
      } else {
        0
      }
      max_w_family <- if (n_family) {
        max(vapply(family_lines, text_width_mm, numeric(1),
                    size = fs_family, face = 2L))
      } else {
        0
      }
      max_w_field <- if (n_field) {
        max(vapply(unlist(field_lines), text_width_mm, numeric(1),
                    size = fs_field, face = 1L))
      } else {
        0
      }
      fits <- max(max_w_title, max_w_family, max_w_field) <= max_w &&
        total_h <= avail_h

      if (!shrink_to_fit || fits || fs_title <= 5 || fs_family <= 5 ||
          fs_field <= 5) {
        break
      }
      fs_title <- fs_title - 0.5
      fs_family <- fs_family - 0.5
      fs_field <- fs_field - 0.5
    }
    list(title_lines = title_lines, family_lines = family_lines,
         field_lines = field_lines, fs_title = fs_title,
         fs_family = fs_family, fs_field = fs_field)
  }

  ## --- draw one label, in unit `row` (0-based, top to bottom) of the page ---
  draw_label <- function(i, row) {
    ## visible band, anchored at the bottom of this unit
    unit_y_bottom <- page_height - (row + 1L) * unit_height
    vp <- grid::viewport(
      x = grid::unit(0, "mm"), y = grid::unit(unit_y_bottom, "mm"),
      width = grid::unit(page_width, "mm"),
      height = grid::unit(content_height, "mm"),
      just = c("left", "bottom"), clip = "off"
    )
    grid::pushViewport(vp)
    on.exit(grid::popViewport(), add = TRUE)

    if (draw_fold_line) {
      grid::grid.lines(
        x = grid::unit(c(0, page_width), "mm"),
        y = grid::unit(c(content_height, content_height), "mm"),
        gp = grid::gpar(col = "grey50", lty = "dashed", lwd = 0.5)
      )
    }
    if (draw_guides) {
      grid::grid.rect(
        x = grid::unit(margin_x, "mm"), y = grid::unit(0, "mm"),
        width = grid::unit(band_width, "mm"),
        height = grid::unit(band_height, "mm"),
        just = c("left", "bottom"),
        gp = grid::gpar(col = "grey70", fill = NA, lwd = 0.3)
      )
    }

    ## title, the bold family prompt just below it, and the remaining
    ## static prompts are each word-wrapped onto as many lines as needed
    ## (never splitting a word), then all stacked one per line, hanging
    ## down from `margin_y` below the fold line (same cursor technique as
    ## generate_specimen_labels()'s stacked lines) - any leftover room
    ## collects at the bottom of the unit
    fields <- c(collection_label, country_label, responsable_label,
                action_label)
    fit <- fit_and_wrap_block(title_v[i], family_label, fields,
                               size_title, size_family, size_field,
                               text_max_w, band_height)

    ## flatten into one draw order: title line(s), then the bold family
    ## prompt's line(s), then each remaining field's line(s) in turn - the
    ## writing line is only appended after the last line of a given prompt
    texts <- character(0); sizes <- numeric(0); faces <- integer(0)
    draw_after <- logical(0)
    n_title <- length(fit$title_lines)
    if (n_title) {
      texts <- fit$title_lines
      sizes <- rep(fit$fs_title, n_title)
      faces <- rep(2L, n_title)
      draw_after <- rep(FALSE, n_title)
    }
    n_family <- length(fit$family_lines)
    if (n_family) {
      texts <- c(texts, fit$family_lines)
      sizes <- c(sizes, rep(fit$fs_family, n_family))
      faces <- c(faces, rep(2L, n_family))
      draw_after <- c(draw_after, rep(FALSE, n_family - 1L), draw_field_lines)
    }
    for (fl in fit$field_lines) {
      n_fl <- length(fl)
      texts <- c(texts, fl)
      sizes <- c(sizes, rep(fit$fs_field, n_fl))
      faces <- c(faces, rep(1L, n_fl))
      draw_after <- c(draw_after, rep(FALSE, n_fl - 1L), draw_field_lines)
    }

    lh <- sizes / 72 * 25.4 * 1.15
    total_h <- sum(lh) + line_gap * (length(lh) - 1L)
    area_top <- band_height   # top of the text block area = margin_y below the fold line
    cursor <- 0

    ## logo, vertically centred against the text block's own height,
    ## aspect-ratio preserved
    logo_dims <- fit_in_box(logo_aspect, logo_box_w - 2 * pad_x, band_height)
    grid::grid.raster(
      logo$raster,
      x = grid::unit(margin_x + logo_box_w / 2, "mm"),
      y = grid::unit(area_top - total_h / 2, "mm"),
      width = grid::unit(logo_dims["w"], "mm"),
      height = grid::unit(logo_dims["h"], "mm")
    )

    ## a rule separating the title from the bold family prompt and the
    ## field prompts stacked below it, drawn in the gap right after the
    ## title's last line
    divider_idx <- if (draw_title_rule && n_title > 0L && length(texts) > n_title) {
      n_title + 1L
    } else {
      NA_integer_
    }

    for (k in seq_along(texts)) {
      if (!is.na(divider_idx) && k == divider_idx) {
        divider_y <- area_top - cursor + line_gap / 2
        grid::grid.lines(
          x = grid::unit(c(text_x_left, margin_x + band_width - pad_x), "mm"),
          y = grid::unit(c(divider_y, divider_y), "mm"),
          gp = grid::gpar(col = "grey40", lwd = 0.8)
        )
      }
      y_center <- area_top - (cursor + lh[k] / 2)
      grid::grid.text(
        texts[k],
        x = grid::unit(text_x_left, "mm"), y = grid::unit(y_center, "mm"),
        just = c("left", "centre"),
        gp = grid::gpar(fontsize = sizes[k], fontface = faces[k],
                         fontfamily = family)
      )
      ## static "Collection: " / "Country: " / "Family: " prompts are left
      ## blank to fill in by hand, each followed by a short writing line
      if (draw_after[k]) {
        label_w <- text_width_mm(texts[k], sizes[k])
        line_x0 <- text_x_left + label_w + 1
        line_x1 <- margin_x + band_width - pad_x
        if (line_x0 < line_x1) {
          ## sit just below the text baseline, on the same line as the prompt
          line_y <- y_center - sizes[k] / 72 * 25.4 * 0.32
          grid::grid.lines(
            x = grid::unit(c(line_x0, line_x1), "mm"),
            y = grid::unit(c(line_y, line_y), "mm"),
            gp = grid::gpar(col = "grey30", lwd = 0.5)
          )
        }
      }
      cursor <- cursor + lh[k] + line_gap
    }
  }

  ## --- open the PDF device (same fallback logic as generate_specimen_labels) ---
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

  ## --- one page per row of `data`, the same label duplicated into every
  ## unit on that page (e.g. top and bottom quarter, for n_per_page = 2) ---
  for (i in seq_len(n)) {
    grid::grid.newpage()
    for (row in seq_len(n_per_page) - 1L) {
      draw_label(i, row)
    }
    if (draw_cut_line && n_per_page > 1L) {
      for (u in seq_len(n_per_page - 1L)) {
        y_cut <- page_height - u * unit_height
        grid::grid.lines(
          x = grid::unit(c(0, page_width), "mm"),
          y = grid::unit(c(y_cut, y_cut), "mm"),
          gp = grid::gpar(col = "black", lwd = 0.75)
        )
      }
    }
  }

  invisible(output_file)
}
