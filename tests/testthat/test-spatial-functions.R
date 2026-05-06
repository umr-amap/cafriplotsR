# Tests for pure spatial/coordinate functions in R/functions_divid_plot.R
# These functions have no database dependency and can be tested in isolation.

# =============================================================================
# procrust()
# Internal Procrustes alignment: finds rotation + translation from X to Y
# =============================================================================

test_that("procrust() returns a list with rotation (2x2 matrix) and translation (length-2 vector)", {
  X <- matrix(c(0, 1, 1, 0,
                0, 0, 1, 1), ncol = 2)
  Y <- matrix(c(10, 11, 11, 10,
                20, 20, 21, 21), ncol = 2)

  result <- procrust(X, Y)

  expect_named(result, c("rotation", "translation"))
  expect_equal(dim(result$rotation), c(2L, 2L))
  expect_length(result$translation, 2L)
})

test_that("procrust() returns identity rotation when X and Y are identical", {
  X <- matrix(c(0, 1, 1, 0,
                0, 0, 1, 1), ncol = 2)

  result <- procrust(X, X)

  expect_equal(round(result$rotation, 8), diag(2))
})

test_that("procrust() recovers a known translation (pure shift, no rotation)", {
  X <- matrix(c(0, 1, 0,
                0, 0, 1), ncol = 2)
  # Y = X shifted by (+5, +10)
  Y <- X + matrix(rep(c(5, 10), each = 3), ncol = 2)

  result <- procrust(X, Y)

  # With a pure translation and identical shapes, rotation should be identity
  expect_equal(round(result$rotation, 6), diag(2))
  # Translation should recover the shift (within numerical precision)
  expect_equal(round(as.numeric(result$translation), 4), c(-5, -10))
})

test_that("procrust() maps Y back to X when Y is a rotated version of X", {
  # procrust(X, Y) finds A such that Y %*% A + b ≈ X
  X <- matrix(c(0, 1, 1, 0,
                0, 0, 1, 1), ncol = 2)
  # Y is X shifted by (5, 10)
  Y <- X + matrix(rep(c(5, 10), each = 4), ncol = 2)

  result <- procrust(X, Y)

  # Reconstruct X from Y: Y %*% rotation + translation (broadcast)
  reconstructed <- Y %*% result$rotation +
    matrix(rep(result$translation, nrow(Y)), ncol = 2, byrow = TRUE)

  expect_equal(round(reconstructed, 6), round(X, 6))
})

# =============================================================================
# bilinear_interpolation()
# Maps points from a relative coordinate space to an absolute space
# using 4 corner anchor points.
# =============================================================================

test_that("bilinear_interpolation() maps centre of unit square to centre of target square", {
  from <- matrix(c(0, 0,
                   1, 0,
                   1, 1,
                   0, 1), ncol = 2, byrow = TRUE)
  to   <- matrix(c(0,   0,
                   100, 0,
                   100, 100,
                   0,   100), ncol = 2, byrow = TRUE)
  colnames(to) <- c("x_interp", "y_interp")

  centre <- matrix(c(0.5, 0.5), ncol = 2)
  result <- bilinear_interpolation(centre, from, to)

  expect_equal(as.numeric(result[1, ]), c(50, 50), tolerance = 1e-6)
})

test_that("bilinear_interpolation() maps corner points exactly", {
  from <- matrix(c(0, 0,
                   1, 0,
                   1, 1,
                   0, 1), ncol = 2, byrow = TRUE)
  to   <- matrix(c(0,   0,
                   200, 0,
                   200, 200,
                   0,   200), ncol = 2, byrow = TRUE)
  colnames(to) <- c("x_interp", "y_interp")

  # The corners of the from-space should map exactly to corners of to-space
  result <- bilinear_interpolation(from, from, to)

  # After sorting CCW, row order may differ; check all target corners appear
  rounded <- round(result, 4)
  expected_corners <- round(to, 4)
  for (i in seq_len(nrow(expected_corners))) {
    expect_true(
      any(apply(rounded, 1, function(r) all(r == expected_corners[i, ]))),
      info = paste("Corner", i, "not found in output")
    )
  }
})

test_that("bilinear_interpolation() raises error when from_corner_coord lacks 4 rows", {
  from3 <- matrix(c(0, 0, 1, 0, 1, 1), ncol = 2, byrow = TRUE)
  to4   <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol = 2, byrow = TRUE)
  pt    <- matrix(c(0.5, 0.5), ncol = 2)

  expect_error(bilinear_interpolation(pt, from3, to4))
})

test_that("bilinear_interpolation() raises error for non-rectangular from_corner_coord", {
  # A trapezoid is not a rectangle
  from_trap <- matrix(c(0, 0,
                        1, 0,
                        0.8, 1,
                        0.2, 1), ncol = 2, byrow = TRUE)
  to4 <- matrix(c(0, 0, 100, 0, 100, 100, 0, 100), ncol = 2, byrow = TRUE)
  pt  <- matrix(c(0.5, 0.5), ncol = 2)

  expect_error(bilinear_interpolation(pt, from_trap, to4))
})

test_that("bilinear_interpolation() accepts data.frame and data.table inputs", {
  from <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol = 2, byrow = TRUE)
  to   <- matrix(c(0, 0, 100, 0, 100, 100, 0, 100), ncol = 2, byrow = TRUE)
  colnames(to) <- c("x_interp", "y_interp")

  pt_df <- data.frame(x = 0.5, y = 0.5)
  pt_dt <- data.table::data.table(x = 0.5, y = 0.5)

  result_df <- bilinear_interpolation(pt_df, from, to)
  result_dt <- bilinear_interpolation(pt_dt, from, to)

  expect_equal(as.numeric(result_df[1, ]), c(50, 50), tolerance = 1e-6)
  expect_equal(as.numeric(result_dt[1, ]), c(50, 50), tolerance = 1e-6)
})

# =============================================================================
# get_plot_rel_xy()
# Parses quadrat label "x_y" to add absolute plot coordinates x_100, y_100
# =============================================================================

test_that("get_plot_rel_xy() adds x_100 and y_100 columns", {
  df <- data.frame(
    quadrat          = c("0_20", "20_40"),
    position_x_iphone = c(5, 10),
    position_y_iphone = c(3,  7)
  )

  result <- get_plot_rel_xy(df)

  expect_true(all(c("x_100", "y_100") %in% names(result)))
})

test_that("get_plot_rel_xy() computes x_100 = position_x + x_quadrat", {
  df <- data.frame(
    quadrat           = c("0_20", "20_40", "40_60"),
    position_x_iphone = c(5,      10,      15),
    position_y_iphone = c(3,       7,       2)
  )

  result <- get_plot_rel_xy(df)

  expect_equal(result$x_100, c(5, 30, 55))   # 0+5, 20+10, 40+15
  expect_equal(result$y_100, c(23, 47, 62))  # 20+3, 40+7, 60+2
})

test_that("get_plot_rel_xy() works with custom column names", {
  df <- data.frame(
    subplot = c("0_0", "20_0"),
    pos_x   = c(8, 12),
    pos_y   = c(4,  6)
  )

  result <- get_plot_rel_xy(df,
                             col_subplot = "subplot",
                             col_pos_x   = "pos_x",
                             col_pos_y   = "pos_y")

  expect_equal(result$x_100, c(8, 32))
  expect_equal(result$y_100, c(4,  6))
})

test_that("get_plot_rel_xy() preserves all original columns", {
  df <- data.frame(
    quadrat           = "0_20",
    position_x_iphone = 5,
    position_y_iphone = 3,
    tag               = 42L,
    species           = "Gilbertiodendron dewevrei"
  )

  result <- get_plot_rel_xy(df)

  expect_true("tag"     %in% names(result))
  expect_true("species" %in% names(result))
})

test_that("get_plot_rel_xy() handles zero quadrat offsets (corner quadrat 0_0)", {
  df <- data.frame(
    quadrat           = "0_0",
    position_x_iphone = 12,
    position_y_iphone = 8
  )

  result <- get_plot_rel_xy(df)

  expect_equal(result$x_100, 12)
  expect_equal(result$y_100,  8)
})

# =============================================================================
# approximate_isolated_xy()
# Fills isolated NA positions by linear interpolation within quadrat groups
# =============================================================================

test_that("approximate_isolated_xy() fills a single NA by linear interpolation", {
  df <- data.frame(
    plot_name         = "Plot1",
    quadrat           = "0_20",
    tag               = c(1, 2, 3),
    position_x_iphone = c(5, NA, 15),
    position_y_iphone = c(10, NA, 10)
  )

  result <- approximate_isolated_xy(df)

  expect_false(any(is.na(result$position_x_iphone)))
  expect_false(any(is.na(result$position_y_iphone)))
  expect_equal(result$position_x_iphone[2], 10)  # midpoint between 5 and 15
  expect_equal(result$position_y_iphone[2], 10)  # constant
})

test_that("approximate_isolated_xy() does not extrapolate beyond known neighbours (rule = 1)", {
  # tag=1 has NA, only tag=2 and 3 have values; rule=1 means no extrapolation left of tag=2
  df <- data.frame(
    plot_name         = "Plot1",
    quadrat           = "0_0",
    tag               = c(1, 2, 3),
    position_x_iphone = c(NA, 10, 20),
    position_y_iphone = c(NA,  5, 15)
  )

  result <- approximate_isolated_xy(df)

  # Rule=1: NA should remain NA for extrapolation outside range
  expect_true(is.na(result$position_x_iphone[1]))
})

test_that("approximate_isolated_xy() handles multiple plots independently", {
  df <- data.frame(
    plot_name         = c("P1", "P1", "P1", "P2", "P2", "P2"),
    quadrat           = c("0_0", "0_0", "0_0", "0_0", "0_0", "0_0"),
    tag               = c(1, 2, 3, 1, 2, 3),
    position_x_iphone = c(0, NA, 20, 100, NA, 300),
    position_y_iphone = c(0,  5, 10,   0,  5,  10)
  )

  result <- approximate_isolated_xy(df)

  p1 <- result[result$plot_name == "P1", ]
  p2 <- result[result$plot_name == "P2", ]

  expect_equal(p1$position_x_iphone[2], 10)   # between 0 and 20
  expect_equal(p2$position_x_iphone[2], 200)  # between 100 and 300
})

test_that("approximate_isolated_xy() interpolates per quadrat independently", {
  # Each quadrat has 2 known values and 1 NA; verify interpolation is group-local
  df <- data.frame(
    plot_name         = c("P1", "P1", "P1", "P1", "P1", "P1"),
    quadrat           = c("0_0", "0_0", "0_0", "20_0", "20_0", "20_0"),
    tag               = c(1,  2,  3,  4,   5,   6),
    position_x_iphone = c(5, NA, 15, 100,  NA, 300),
    position_y_iphone = c(5,  5,  5,  10,  10,  10)
  )

  result <- approximate_isolated_xy(df)

  # Quadrat "0_0": NA at tag=2 should interpolate from tag=1 (x=5) and tag=3 (x=15)
  expect_equal(result$position_x_iphone[2], 10)
  # Quadrat "20_0": NA at tag=5 should interpolate from tag=4 (x=100) and tag=6 (x=300)
  expect_equal(result$position_x_iphone[5], 200)
  # Confirm groups did not bleed into each other
  expect_false(result$position_x_iphone[2] == 200)
})

test_that("approximate_isolated_xy() leaves data unchanged when no NAs", {
  df <- data.frame(
    plot_name         = "P1",
    quadrat           = c("0_0", "0_0", "0_0"),
    tag               = c(1, 2, 3),
    position_x_iphone = c(5, 10, 15),
    position_y_iphone = c(1,  2,  3)
  )

  result <- approximate_isolated_xy(df)

  expect_equal(result$position_x_iphone, c(5, 10, 15))
  expect_equal(result$position_y_iphone, c(1,  2,  3))
})

# =============================================================================
# latlong2UTM()
# Converts geographic (long/lat) to UTM coordinates
# Requires the 'proj4' package.
# =============================================================================

test_that("latlong2UTM() adds X, Y, and codeUTM columns to input", {
  skip_if_not_installed("proj4")

  coord <- data.frame(long = 9.0, lat = 1.0)
  result <- latlong2UTM(coord)

  expect_true(all(c("X", "Y", "codeUTM") %in% names(result)))
})

test_that("latlong2UTM() returns finite positive coordinates for a central African point", {
  skip_if_not_installed("proj4")

  coord <- data.frame(long = 11.5, lat = 0.5)  # near Libreville, Gabon
  result <- latlong2UTM(coord)

  expect_true(is.finite(result$X))
  expect_true(is.finite(result$Y))
  expect_gt(result$X, 0)
  expect_gt(result$Y, 0)
})

test_that("latlong2UTM() assigns +north suffix for positive latitude", {
  skip_if_not_installed("proj4")

  coord <- data.frame(long = 9.0, lat = 2.0)
  result <- latlong2UTM(coord)

  expect_match(result$codeUTM, "north")
})

test_that("latlong2UTM() assigns +south suffix for negative latitude", {
  skip_if_not_installed("proj4")

  coord <- data.frame(long = 15.0, lat = -3.0)
  result <- latlong2UTM(coord)

  expect_match(result$codeUTM, "south")
})

test_that("latlong2UTM() processes multiple rows and returns same number of rows", {
  skip_if_not_installed("proj4")

  coord <- data.frame(
    long = c(9.0, 15.0, 11.5),
    lat  = c(1.0, -3.0,  0.5)
  )
  result <- latlong2UTM(coord)

  expect_equal(nrow(result), 3L)
  expect_true(all(is.finite(result$X)))
  expect_true(all(is.finite(result$Y)))
})

test_that("latlong2UTM() assigns different UTM zones for well-separated longitudes", {
  skip_if_not_installed("proj4")

  coord <- data.frame(long = c(9.0, 21.0), lat = c(1.0, 1.0))
  result <- latlong2UTM(coord)

  # Zone 32 for long=9, zone 36 for long=21 (approx)
  expect_false(result$codeUTM[1] == result$codeUTM[2])
})
