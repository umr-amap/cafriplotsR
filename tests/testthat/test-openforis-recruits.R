# process_openforis_census(): recruits carry identity only
#
# Recruits go through the Import Wizard and their measurements through the
# Feature Wizard. When `recruits` also carried the measurements, the Import
# Wizard stored them (with no census link) and `measurements` stored them a
# second time.

test_that("recruits leave measurements to `measurements`", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(
    plot_plot_name          = "mbalmayo001",
    arbre                   = c(22, NA),
    label_recrut            = c(NA, "22.1"),
    state                   = c("alive", "recruted"),
    species_scientific_name = c("Sp a", "Sp b"),
    quadrat                 = c("1", "1"),
    position_x              = c(3, 4),
    position_y              = c(5, 6),
    stem_diameter           = c(30, 10),
    height_of_stem_diameter = c(1.3, 1.3),
    multi_stem              = c("no", "no"),
    herbarium_nbe_char      = NA_character_,
    stringsAsFactors = FALSE
  ), path)

  res <- suppressWarnings(suppressMessages(
    process_openforis_census(tree_file = path)
  ))

  expect_equal(res$recruits$tag, "22.1")
  expect_true(all(c("plot_name", "tag", "original_tax_name", "quadrat") %in%
                    names(res$recruits)))
  expect_length(intersect(
    names(res$recruits),
    c("stem_diameter", "height_of_stem_diameter", "position_x", "position_y",
      "multi_stem", "number_multi_stem", "observation", "light", "stem_status")
  ), 0)

  # ... which still holds the recruit's values, once
  rec_meas <- res$measurements[res$measurements$tag == "22.1", ]
  expect_equal(sum(rec_meas$trait_name == "stem_diameter"), 1)
  expect_equal(rec_meas$traitvalue[rec_meas$trait_name == "stem_diameter"], 10)
  expect_true("position_x" %in% rec_meas$trait_name)

  # all_stems feeds the full census import, which needs the wide traits
  expect_true(all(c("stem_diameter", "position_x", "multi_stem") %in%
                    names(res$all_stems)))
})
