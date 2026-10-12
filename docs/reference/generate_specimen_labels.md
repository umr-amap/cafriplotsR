# Generate printable specimen labels (A4 sticker sheets)

Produce a print-ready, multi-page PDF of specimen labels laid out on a 3
x 8 grid of 70 mm x 37 mm cells (24 labels per A4 page), reproducing the
formatting of the herbarium label template used for IRD forest-plot
vouchers. Each label shows, on vertically-centred lines, the specimen
number, the family (bold, right-aligned), the scientific name (italic)
and the locality (small font).

The function relies only on base R graphics devices (grDevices and
grid), which ship with every R installation, so it adds **no external
dependency**. Geometry is fully parameterised so the grid can be
calibrated against a specific physical sticker sheet.

## Usage

``` r
generate_specimen_labels(
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
  device = c("cairo_pdf", "pdf")
)
```

## Arguments

- data:

  A data.frame (or tibble) with one row per specimen / label.

- output_file:

  Character. Path to the PDF file to create.

- specimen_col, family_col, name_col, locality_col:

  Character. Column names in `data` holding, respectively, the specimen
  number, family, scientific name and locality. A column that is absent,
  wholly `NA`, or a literal `"NA"` / `NaN` text value is rendered as a
  blank line, preserving the vertical layout. A stray `NA` token
  embedded in otherwise valid text (e.g. `"Xylopia NA"`, from an
  upstream `paste(genus, species)` where `species` was `NA`) is removed
  and the surrounding whitespace collapsed; words that merely contain
  "NA" (e.g. "Annonaceae") are left untouched.

- determination_col:

  Character or `NULL`. Optional column holding ready-made determination
  text. When it is present in `data`, its non-empty values are printed
  as-is and take precedence over `det_cols`. Only used when
  `include_determination = TRUE`.

- include_determination:

  Logical. If `TRUE`, an extra line with the determination is inserted
  below the scientific name, built from `det_cols` as
  `"det. <determiner> (<date>)"`, e.g. `"det. G. Dauby (mar 2024)"`,
  with the date formatted as for `include_date`. Missing parts are
  dropped and the line is left blank when there is neither a determiner
  nor a date. Default `FALSE` (matches the original template, which
  leaves this field blank).

- det_cols:

  Named character vector giving the `by` (determiner), `day`, `month`
  and `year` columns of the determination. Defaults to the `detby`,
  `detd`, `detm`, `dety` columns returned by
  [`query_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/query_specimens.md).
  A missing column is treated as all `NA`.

- include_date:

  Logical. If `TRUE`, the collection date is appended to the specimen
  line, in parentheses with a lower-case month abbreviation, e.g.
  `"IRD plot 2508 (12 mar 2024)"`. Missing parts are dropped:
  `"(mar 2024)"`, `"(2024)"`; a day without a month is dropped too, and
  nothing is appended when all three are missing. Default `FALSE`.

- date_cols:

  Named character vector giving the `day`, `month` and `year` columns of
  the collection date. Defaults to the `cold`, `colm`, `coly` columns
  returned by
  [`query_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/query_specimens.md).
  A missing column is treated as all `NA`; out-of-range days or months
  are ignored.

- extra_cols:

  Character vector or `NULL`. Additional columns (e.g. `"description"`)
  each printed in a small font below the locality. Long text is
  word-wrapped onto at most `extra_max_lines` lines and cropped with
  `"..."` where it no longer fits. Default `NULL` (no extra lines).

- extra_max_lines:

  Integer. Maximum number of lines per extra column (default 2).

- size_extra:

  Numeric. Font size in points of the extra lines (default 7). Unlike
  the other lines, extra lines are cropped rather than shrunk to fit.

- page_width, page_height:

  Numeric. Page size in millimetres (default A4: 210 x 297).

- n_cols, n_rows:

  Integer. Number of label columns and rows per page (default 3 x 8 = 24
  labels per page).

- label_width, label_height:

  Numeric. Label (cell) size in millimetres (default 70 x 37).

- offset_x, offset_y:

  Numeric or `NULL`. Millimetre offset of the label grid from the
  top-left corner of the page, for fine calibration against a physical
  sticker sheet. When `NULL` (default) the grid is centred on the page.

- pad_x:

  Numeric. Horizontal text inset inside each label, in millimetres
  (default 3.7, matching the template's paragraph indent).

- family:

  Character. Font family. Default `"Calibri"`, matching the original
  template (requires `device = "cairo_pdf"`, the default, and that the
  font be installed). On the base `"pdf"` device, which only knows its
  built-in PostScript families, an unavailable font is replaced by
  `"sans"` with a message.

- size_specimen, size_family, size_name, size_locality:

  Numeric. Font sizes in points for the four lines (defaults 11, 12, 12,
  8).

- shrink_to_fit:

  Logical. If `TRUE` (default), the font size of any line whose text is
  wider than the label is reduced until it fits, preventing overflow
  onto neighbouring stickers.

- draw_guides:

  Logical. If `TRUE`, thin rectangles are drawn around each label
  (useful for calibration or when printing on plain paper). Default
  `FALSE` (borderless, like the template).

- device:

  Character. Graphics device: `"cairo_pdf"` (default, better system-font
  support) or `"pdf"`. Falls back to `"pdf"` automatically if cairo is
  unavailable.

## Value

(Invisibly) the path to the created PDF file.

## Details

Labels are filled left-to-right then top-to-bottom, matching the
original Word mail-merge output. The number of pages is
`ceiling(nrow(data) / (n_cols * n_rows))`; the last page is padded with
blank cells as needed.

To reproduce the original template's font on Windows, use
`device = "cairo_pdf", family = "Calibri"`.

## Examples

``` r
df <- data.frame(
  Specimen = c("IRD plot 2508", "IRD plot 2509"),
  Family   = c("Fabaceae", "Annonaceae"),
  Name     = c("Amphimas pterocarpoides", "Uvariastrum pierreanum"),
  Locality = c("mpem008", "mpem008")
)
out <- tempfile(fileext = ".pdf")
generate_specimen_labels(df, out)

# With the collection date and a (cropped) description, using the
# column names returned by query_specimens()
df$cold <- c(12, NA)
df$colm <- c(3, 11)
df$coly <- c(2024, 2023)
df$description <- c(
  "Tree 25 m, bark smooth, slash red with abundant white latex, fruits green",
  NA
)
df$detby <- c("G. Dauby", NA)
df$detm <- c(5, NA)
df$dety <- c(2025, NA)
generate_specimen_labels(df, out, include_date = TRUE,
                         include_determination = TRUE,
                         extra_cols = "description")
```
