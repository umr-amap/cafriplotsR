# Backend of launch_data_update_app(): the layer that decides where a value
# shown in an extracted table actually lives, and whether it is an aggregate.
# Everything tested here is pure - no database required.

# ── .upd_same: what counts as an edit ────────────────────────────────────────

test_that(".upd_same() treats every flavour of 'absent' as one value", {
  expect_true(CafriplotsR:::.upd_same(NA, ""))
  expect_true(CafriplotsR:::.upd_same(NULL, NA))
  expect_true(CafriplotsR:::.upd_same("", NULL))
  expect_true(CafriplotsR:::.upd_same("   ", NA))
  expect_true(CafriplotsR:::.upd_same(character(0), NA))
})

test_that(".upd_same() does not report a formatting difference as an edit", {
  # A numeric input round-trips through the browser as text; 12.50 and 12.5 are
  # the same stored number and must not be written back as a change.
  expect_true(CafriplotsR:::.upd_same("12.50", 12.5))
  expect_true(CafriplotsR:::.upd_same(12, "12"))
  expect_true(CafriplotsR:::.upd_same("  abc ", "abc"))
})

test_that(".upd_same() reports genuine edits", {
  expect_false(CafriplotsR:::.upd_same("a", "b"))
  expect_false(CafriplotsR:::.upd_same(NA, 5))
  expect_false(CafriplotsR:::.upd_same(5, NA))
  expect_false(CafriplotsR:::.upd_same(12.5, 12.6))
})

# ── .upd_coerce / .upd_input_kind ────────────────────────────────────────────

test_that(".upd_input_kind() maps PostgreSQL types to form inputs", {
  expect_equal(CafriplotsR:::.upd_input_kind("integer"), "integer")
  expect_equal(CafriplotsR:::.upd_input_kind("bigint"), "integer")
  expect_equal(CafriplotsR:::.upd_input_kind("numeric"), "numeric")
  expect_equal(CafriplotsR:::.upd_input_kind("double precision"), "numeric")
  expect_equal(CafriplotsR:::.upd_input_kind("boolean"), "boolean")
  expect_equal(CafriplotsR:::.upd_input_kind("character varying"), "text")
  expect_equal(CafriplotsR:::.upd_input_kind(NA), "text")
})

test_that(".upd_coerce() turns an empty input into NA, not an empty string", {
  # NA is what clears a column; "" would write an empty string instead.
  expect_true(is.na(CafriplotsR:::.upd_coerce("", "text")))
  expect_true(is.na(CafriplotsR:::.upd_coerce("   ", "text")))
  expect_true(is.na(CafriplotsR:::.upd_coerce(NULL, "numeric")))
  expect_true(is.na(CafriplotsR:::.upd_coerce(NA, "integer")))
})

test_that(".upd_coerce() casts to the column's R type", {
  expect_identical(CafriplotsR:::.upd_coerce("3.5", "numeric"), 3.5)
  expect_identical(CafriplotsR:::.upd_coerce("3", "integer"), 3L)
  expect_identical(CafriplotsR:::.upd_coerce(3.5, "text"), "3.5")
  expect_identical(CafriplotsR:::.upd_coerce("TRUE", "boolean"), TRUE)
})

# ── .upd_value_column: which column a feature value is stored in ─────────────

test_that(".upd_value_column() routes plot features to data_liste_sub_plots", {
  expect_equal(CafriplotsR:::.upd_value_column("numeric", "plot"), "typevalue")
  expect_equal(CafriplotsR:::.upd_value_column("integer", "plot"), "typevalue")
  expect_equal(CafriplotsR:::.upd_value_column("character", "plot"), "typevalue_char")
  expect_equal(CafriplotsR:::.upd_value_column("categorical", "plot"), "typevalue_char")
})

test_that(".upd_value_column() routes individual features to data_traits_measures", {
  expect_equal(CafriplotsR:::.upd_value_column("numeric", "individual"), "traitvalue")
  expect_equal(CafriplotsR:::.upd_value_column("character", "individual"), "traitvalue_char")
})

test_that(".upd_value_column() stores a table reference as its numeric id", {
  # A table_colnam feature is a numeric feature whose value is the
  # id_table_colnam, so it uses the numeric column. The character column is
  # never used for one, and neither is data_liste_sub_plots.id_colnam:
  # `typevalue` is populated on 100% of the people-feature rows in production
  # (additional_people 3095/3095, data_manager 858/858,
  # principal_investigator 1088/1088, team_leader 2169/2169), while id_colnam
  # is set on a negligible number of rows and only in error.
  expect_equal(CafriplotsR:::.upd_value_column("table_colnam", "plot"), "typevalue")
  expect_equal(CafriplotsR:::.upd_value_column("table_colnam", "individual"), "traitvalue")
  expect_false(
    grepl("_char$", CafriplotsR:::.upd_value_column("table_colnam", "plot"))
  )
})

test_that(".upd_value_column() falls back to the character column for unknown types", {
  expect_equal(CafriplotsR:::.upd_value_column(NA, "individual"), "traitvalue_char")
  expect_equal(CafriplotsR:::.upd_value_column(NA, "plot"), "typevalue_char")
})

# ── .upd_entity_spec ─────────────────────────────────────────────────────────

test_that(".upd_entity_spec() names the tables a section writes to", {
  plot_spec <- CafriplotsR:::.upd_entity_spec("plot")
  expect_equal(plot_spec$table, "data_liste_plots")
  expect_equal(plot_spec$id_column, "id_liste_plots")
  expect_equal(plot_spec$feature_table, "data_liste_sub_plots")
  expect_equal(plot_spec$feature_id, "id_sub_plots")

  ind_spec <- CafriplotsR:::.upd_entity_spec("individual")
  expect_equal(ind_spec$table, "data_individuals")
  expect_equal(ind_spec$id_column, "id_n")
  expect_equal(ind_spec$feature_table, "data_traits_measures")
  expect_equal(ind_spec$feature_id, "id_trait_measures")
})

test_that(".upd_entity_spec() keeps plot membership out of the editable columns", {
  # Re-parenting an individual is not a value correction.
  expect_true("id_table_liste_plots_n" %in%
                CafriplotsR:::.upd_entity_spec("individual")$exclude)
})

# ── aggregation annotation: the point of the whole exercise ──────────────────

make_records <- function() {
  dplyr::tibble(
    record_id     = 1:4,
    feature       = c("stem_diameter", "stem_diameter", "additional_people", "forest_type"),
    valuetype     = c("numeric", "numeric", "table_colnam", "character"),
    unit          = c("cm", "cm", NA, NA),
    min_allowed   = NA_real_,
    max_allowed   = NA_real_,
    value_num     = c(10, 20, 7, NA),
    value_char    = c(NA, NA, NA, "terra firme"),
    lookup_id     = c(NA, NA, 7L, NA),
    value_display = c("10", "20", "Dauby", "terra firme"),
    year = 2020L, month = 1L, day = 1L,
    issue = NA_character_, context = NA_character_
  )
}

test_that(".upd_annotate_aggregation() counts the records behind each column", {
  ann <- CafriplotsR:::.upd_annotate_aggregation(make_records())
  expect_equal(ann$n_records, c(2L, 2L, 1L, 1L))
})

test_that(".upd_annotate_aggregation() reproduces the extraction path's aggregate", {
  ann <- CafriplotsR:::.upd_annotate_aggregation(make_records())

  # Numeric traits are averaged by aggregate_numeric_features_dt() and
  # aggregate_numeric_plot_features(); the app must show the same number the
  # user saw in their extracted table.
  diam <- ann[ann$feature == "stem_diameter", ]
  expect_equal(unique(diam$agg_rule), "mean")
  expect_equal(unique(diam$aggregate_display), "15")

  # Character and table-referenced features are concatenated over unique values.
  ft <- ann[ann$feature == "forest_type", ]
  expect_equal(unique(ft$agg_rule), "concat")
  expect_equal(unique(ft$aggregate_display), "terra firme")
})

test_that(".upd_annotate_aggregation() concatenates a repeated character feature", {
  recs <- make_records()
  recs <- dplyr::bind_rows(recs, dplyr::tibble(
    record_id = 5L, feature = "forest_type", valuetype = "character",
    unit = NA_character_, min_allowed = NA_real_, max_allowed = NA_real_,
    value_num = NA_real_, value_char = "swamp", lookup_id = NA_integer_,
    value_display = "swamp", year = 2020L, month = 1L, day = 1L,
    issue = NA_character_, context = NA_character_
  ))
  ann <- CafriplotsR:::.upd_annotate_aggregation(recs)
  ft <- ann[ann$feature == "forest_type", ]
  expect_equal(unique(ft$n_records), 2L)
  expect_equal(unique(ft$aggregate_display), "terra firme, swamp")
})

test_that(".upd_annotate_aggregation() survives a feature with no values at all", {
  recs <- make_records()
  recs$value_num[recs$feature == "stem_diameter"] <- NA_real_
  ann <- CafriplotsR:::.upd_annotate_aggregation(recs)
  expect_true(all(is.na(ann$aggregate_display[ann$feature == "stem_diameter"])))
})

test_that(".upd_annotate_aggregation() returns the documented shape when empty", {
  ann <- CafriplotsR:::.upd_annotate_aggregation(CafriplotsR:::.upd_empty_features())
  expect_equal(nrow(ann), 0)
  expect_true(all(CafriplotsR:::.UPD_FEATURE_COLS %in% names(ann)))
})

# ── .upd_feature_summary ─────────────────────────────────────────────────────

test_that(".upd_feature_summary() flags exactly the aggregated features", {
  s <- CafriplotsR:::.upd_feature_summary(
    CafriplotsR:::.upd_annotate_aggregation(make_records())
  )
  expect_equal(nrow(s), 3)
  expect_true(s$is_aggregated[s$feature == "stem_diameter"])
  expect_false(s$is_aggregated[s$feature == "forest_type"])
  expect_false(s$is_aggregated[s$feature == "additional_people"])
})

test_that(".upd_feature_summary() lists aggregated features first", {
  s <- CafriplotsR:::.upd_feature_summary(
    CafriplotsR:::.upd_annotate_aggregation(make_records())
  )
  expect_equal(s$feature[1], "stem_diameter")
})

test_that(".upd_feature_summary() handles a record with no features", {
  s <- CafriplotsR:::.upd_feature_summary(CafriplotsR:::.upd_empty_features())
  expect_equal(nrow(s), 0)
  expect_true(all(c("feature", "n_records", "is_aggregated") %in% names(s)))
})

# ── .upd_display_value ───────────────────────────────────────────────────────

test_that(".upd_display_value() resolves a table reference to its label", {
  lookup <- dplyr::tibble(id_table_colnam = 7L, colnam = "Dauby")
  out <- CafriplotsR:::.upd_display_value(
    valuetype  = c("numeric", "table_colnam", "character"),
    value_num  = c(3.2, NA, NA),
    value_char = c(NA, NA, "swamp"),
    lookup_id  = c(NA, 7L, NA),
    colnam_lookup = lookup
  )
  expect_equal(out, c("3.2", "Dauby", "swamp"))
})

test_that(".upd_display_value() falls back to the raw id when unresolvable", {
  lookup <- dplyr::tibble(id_table_colnam = integer(), colnam = character())
  out <- CafriplotsR:::.upd_display_value(
    "table_colnam", NA_real_, NA_character_, 99L, lookup
  )
  expect_equal(out, "99")
})

test_that(".upd_display_value() reports a missing value as NA, not a string", {
  lookup <- dplyr::tibble(id_table_colnam = integer(), colnam = character())
  out <- CafriplotsR:::.upd_display_value(
    "numeric", NA_real_, NA_character_, NA_integer_, lookup
  )
  expect_true(is.na(out))
})

# ── .upd_fmt ─────────────────────────────────────────────────────────────────

test_that(".upd_fmt() renders every absent value as a dash", {
  expect_equal(CafriplotsR:::.upd_fmt(NULL), "-")
  expect_equal(CafriplotsR:::.upd_fmt(NA), "-")
  expect_equal(CafriplotsR:::.upd_fmt(""), "-")
  expect_equal(CafriplotsR:::.upd_fmt(character(0)), "-")
  expect_equal(CafriplotsR:::.upd_fmt(5), "5")
})

# ── module UI ────────────────────────────────────────────────────────────────

test_that("mod_update_record_ui() builds both sections", {
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file("translations/translation.json",
                                        package = "CafriplotsR")
  )
  i18n$set_translation_language("fr")

  expect_no_warning(ui_plot <- mod_update_record_ui("p", "plot", i18n))
  expect_no_warning(ui_ind  <- mod_update_record_ui("i", "individual", i18n))
  expect_s3_class(ui_plot, "shiny.tag.list")
  expect_s3_class(ui_ind, "shiny.tag.list")

  # The taxonomic picker belongs to individuals only.
  expect_true(grepl("taxa_pick", as.character(ui_ind)))
  expect_false(grepl("taxa_pick", as.character(ui_plot)))
})

test_that("mod_update_record_ui() rejects an unknown entity", {
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file("translations/translation.json",
                                        package = "CafriplotsR")
  )
  expect_error(mod_update_record_ui("x", "specimen", i18n), "should be one of")
})

# ── features the extraction does not simply aggregate ────────────────────────

test_that(".upd_annotate_aggregation() never averages the census numbers", {
  # A plot with censuses 1 and 2 must not be described as census 1.5: the
  # extracted table carries n_census and the dates, not the numbers.
  recs <- dplyr::tibble(
    record_id = 1:2, feature = "census", valuetype = "numeric",
    unit = NA_character_, min_allowed = NA_real_, max_allowed = NA_real_,
    value_num = c(1, 2), value_char = NA_character_, lookup_id = NA_integer_,
    value_display = c("1", "2"),
    year = c(2015L, 2021L), month = c(3L, 6L), day = c(4L, NA),
    issue = NA_character_, context = NA_character_
  )
  ann <- CafriplotsR:::.upd_annotate_aggregation(recs, "plot")

  expect_equal(unique(ann$agg_rule), "census")
  expect_false(grepl("1.5", unique(ann$aggregate_display), fixed = TRUE))
  expect_equal(unique(ann$aggregate_display), "n_census = 2 (2015-03-04, 2021-06)")
})

test_that(".upd_annotate_aggregation() keeps censuses apart for an individual", {
  # aggregate_numeric_features_dt() averages within a census and pivots to one
  # column per census, so a single mean over both would be a number the user
  # never sees.
  recs <- dplyr::tibble(
    record_id = 1:3, feature = "stem_diameter", valuetype = "numeric",
    unit = "cm", min_allowed = NA_real_, max_allowed = NA_real_,
    value_num = c(12.4, 12.6, 13.1), value_char = NA_character_,
    lookup_id = NA_integer_, value_display = c("12.4", "12.6", "13.1"),
    year = 2020L, month = 1L, day = 1L, issue = NA_character_,
    context = c("census_1", "census_1", "census_2")
  )
  ann <- CafriplotsR:::.upd_annotate_aggregation(recs, "individual")

  expect_equal(unique(ann$agg_rule), "per_census")
  expect_equal(unique(ann$aggregate_display), "census_1: 12.5 | census_2: 13.1")
})

test_that(".upd_annotate_aggregation() says so when a plot feature is not extracted", {
  # aggregate_plot_features() only handles numeric, character and table_*.
  recs <- dplyr::tibble(
    record_id = 1L, feature = "some_ordinal", valuetype = "ordinal",
    unit = NA_character_, min_allowed = NA_real_, max_allowed = NA_real_,
    value_num = NA_real_, value_char = "high", lookup_id = NA_integer_,
    value_display = "high", year = 2020L, month = 1L, day = 1L,
    issue = NA_character_, context = NA_character_
  )
  ann <- CafriplotsR:::.upd_annotate_aggregation(recs, "plot")

  expect_equal(ann$agg_rule, "not_extracted")
  expect_true(is.na(ann$aggregate_display))
})

test_that(".upd_record_dates() degrades from day to month to year", {
  grp <- dplyr::tibble(year = c(2020L, 2020L, 2020L, NA_integer_),
                       month = c(5L, 5L, NA, NA),
                       day = c(4L, NA, NA, NA))
  expect_equal(CafriplotsR:::.upd_record_dates(grp),
               c("2020-05-04", "2020-05", "2020", ""))
})

# ── identification cascade ───────────────────────────────────────────────────

test_that(".upd_accepted_idtax() maps a synonym to its accepted taxon", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "table_idtax", data.frame(
    idtax_n = c(13127L, 500L, 900L),
    idtax_good_n = c(NA_integer_, 600L, NA_integer_)
  ))

  acc <- CafriplotsR:::.upd_accepted_idtax(c(13127L, 500L), con)
  expect_equal(unname(acc["13127"]), 13127L)  # accepted already
  expect_equal(unname(acc["500"]), 600L)      # synonym resolved
})

test_that(".upd_accepted_idtax() leaves an unknown id alone", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "table_idtax", data.frame(
    idtax_n = 1L, idtax_good_n = NA_integer_
  ))

  expect_equal(unname(CafriplotsR:::.upd_accepted_idtax(42L, con)["42"]), 42L)
})

test_that(".upd_identification() lets a linked specimen govern the identification", {
  # This is the rule merge_individuals_taxa() applies:
  # idtax_individual_f = coalesce(idtax_specimen_f, idtax_f).
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "data_individuals", data.frame(
    id_n = 1L, idtax_n = 500L, original_tax_name = "Afrostyrax kamerunensis"
  ))
  DBI::dbWriteTable(con, "table_idtax", data.frame(
    idtax_n = c(500L, 700L), idtax_good_n = c(600L, NA_integer_)
  ))
  DBI::dbWriteTable(con, "data_link_specimens", data.frame(
    id_n = 1L, id_specimen = 9L, id_linktype = 1L
  ))
  DBI::dbWriteTable(con, "specimens", data.frame(
    id_specimen = 9L, idtax_n = 700L, id_colnam = 3L, colnbr = "1234",
    suffix = NA_character_, dety = 2019L, detm = 5L, detd = 2L
  ))
  DBI::dbWriteTable(con, "table_colnam", data.frame(
    id_table_colnam = 3L, colnam = "Dauby"
  ))
  DBI::dbWriteTable(con, "linktypelist", data.frame(
    id_linktype = 1L, priority = 100L
  ))

  ident <- CafriplotsR:::.upd_identification(1L, con, con_taxa = NULL)

  expect_equal(ident$idtax_n, 500L)
  expect_equal(ident$idtax_f, 600L)           # the individual's own synonymy
  expect_true(ident$is_synonym)
  expect_equal(ident$idtax_specimen_f, 700L)  # the specimen's identification
  expect_equal(ident$idtax_individual_f, 700L)
  expect_equal(ident$governed_by, "specimen")
  expect_equal(CafriplotsR:::.upd_specimen_label(ident$specimen), "Dauby 1234")
})

test_that(".upd_identification() falls back to the individual when nothing is linked", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "data_individuals", data.frame(
    id_n = 1L, idtax_n = 500L, original_tax_name = "Afrostyrax sp."
  ))
  DBI::dbWriteTable(con, "table_idtax", data.frame(
    idtax_n = 500L, idtax_good_n = 600L
  ))
  DBI::dbWriteTable(con, "data_link_specimens", data.frame(
    id_n = integer(0), id_specimen = integer(0), id_linktype = integer(0)
  ))
  DBI::dbWriteTable(con, "specimens", data.frame(
    id_specimen = integer(0), idtax_n = integer(0), id_colnam = integer(0),
    colnbr = character(0), suffix = character(0),
    dety = integer(0), detm = integer(0), detd = integer(0)
  ))
  DBI::dbWriteTable(con, "table_colnam", data.frame(
    id_table_colnam = integer(0), colnam = character(0)
  ))
  DBI::dbWriteTable(con, "linktypelist", data.frame(
    id_linktype = integer(0), priority = integer(0)
  ))

  ident <- CafriplotsR:::.upd_identification(1L, con, con_taxa = NULL)

  expect_null(ident$specimen)
  expect_true(is.na(ident$idtax_specimen_f))
  expect_equal(ident$idtax_individual_f, 600L)
  expect_equal(ident$governed_by, "individual")
})

test_that(".upd_identification() prefers the higher-priority link", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "data_individuals", data.frame(
    id_n = 1L, idtax_n = 500L, original_tax_name = NA_character_
  ))
  DBI::dbWriteTable(con, "table_idtax", data.frame(
    idtax_n = c(500L, 700L, 800L), idtax_good_n = NA_integer_
  ))
  DBI::dbWriteTable(con, "data_link_specimens", data.frame(
    id_n = c(1L, 1L), id_specimen = c(9L, 10L), id_linktype = c(2L, 1L)
  ))
  DBI::dbWriteTable(con, "specimens", data.frame(
    id_specimen = c(9L, 10L), idtax_n = c(700L, 800L), id_colnam = c(3L, 3L),
    colnbr = c("1", "2"), suffix = NA_character_,
    dety = c(2021L, 2005L), detm = 1L, detd = 1L
  ))
  DBI::dbWriteTable(con, "table_colnam", data.frame(
    id_table_colnam = 3L, colnam = "Dauby"
  ))
  # Link type 1 outranks 2, even though specimen 9 was determined more recently.
  DBI::dbWriteTable(con, "linktypelist", data.frame(
    id_linktype = c(1L, 2L), priority = c(100L, 50L)
  ))

  ident <- CafriplotsR:::.upd_identification(1L, con, con_taxa = NULL)
  expect_equal(ident$specimen$id_specimen, 10L)
  expect_equal(ident$idtax_individual_f, 800L)
})

# ── several plots at once ────────────────────────────────────────────────────

.mk_plot_feature_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "data_liste_plots", data.frame(
    id_liste_plots = c(1L, 2L),
    plot_name = c("BEL-01", "BEL-02"),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "subplotype_list", data.frame(
    id_subplotype = c(10L, 20L),
    type = c("soil_depth", "principal_investigator"),
    valuetype = c("numeric", "table_colnam"),
    expectedunit = c("cm", NA_character_),
    minallowedvalue = NA_real_, maxallowedvalue = NA_real_,
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "table_colnam", data.frame(
    id_table_colnam = 7L, colnam = "Dauby G.", stringsAsFactors = FALSE
  ))
  con
}

test_that(".upd_plot_feature_records() keeps each plot's records apart", {
  con <- .mk_plot_feature_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # One soil_depth record in each plot: an aggregate in neither.
  DBI::dbWriteTable(con, "data_liste_sub_plots", data.frame(
    id_sub_plots = 1:3,
    id_table_liste_plots = c(1L, 2L, 2L),
    id_type_sub_plot = c(10L, 10L, 20L),
    year = 2020L, month = NA_integer_, day = NA_integer_,
    typevalue = c(30, 50, 7), typevalue_char = NA_character_,
    original_subplot_name = NA_character_, issue = NA_character_,
    stringsAsFactors = FALSE
  ))

  recs <- CafriplotsR:::.upd_plot_feature_records(c(1L, 2L), con)

  expect_equal(nrow(recs), 3L)
  expect_true(all(c("id_plot", "plot_name") %in% names(recs)))
  soil <- recs[recs$feature == "soil_depth", ]
  expect_equal(unique(soil$n_records), 1L)
  expect_equal(sort(soil$aggregate_display), c("30", "50"))
  # A table_colnam value is the id held in typevalue, shown as the name.
  expect_equal(recs$value_display[recs$feature == "principal_investigator"],
               "Dauby G.")
})

test_that(".upd_plot_feature_records() aggregates only within one plot", {
  con <- .mk_plot_feature_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Two soil_depth records in plot 1, one in plot 2.
  DBI::dbWriteTable(con, "data_liste_sub_plots", data.frame(
    id_sub_plots = 1:3,
    id_table_liste_plots = c(1L, 1L, 2L),
    id_type_sub_plot = 10L,
    year = 2020L, month = NA_integer_, day = NA_integer_,
    typevalue = c(30, 40, 50), typevalue_char = NA_character_,
    original_subplot_name = NA_character_, issue = NA_character_,
    stringsAsFactors = FALSE
  ))

  recs <- CafriplotsR:::.upd_plot_feature_records(c(1L, 2L), con)
  s <- CafriplotsR:::.upd_feature_summary(recs)

  expect_equal(s$n_records[s$plot_name == "BEL-01"], 2L)
  expect_equal(s$n_records[s$plot_name == "BEL-02"], 1L)
  expect_true(s$is_aggregated[s$plot_name == "BEL-01"])
  expect_false(s$is_aggregated[s$plot_name == "BEL-02"])
  expect_equal(s$aggregate_display[s$plot_name == "BEL-01"], "35")
})

test_that(".upd_plot_feature_records() returns the plot shape when there is nothing", {
  con <- .mk_plot_feature_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "data_liste_sub_plots", data.frame(
    id_sub_plots = integer(), id_table_liste_plots = integer(),
    id_type_sub_plot = integer(), year = integer(), month = integer(),
    day = integer(), typevalue = numeric(), typevalue_char = character(),
    original_subplot_name = character(), issue = character(),
    stringsAsFactors = FALSE
  ))

  recs <- CafriplotsR:::.upd_plot_feature_records(c(1L, 2L), con)
  expect_equal(nrow(recs), 0L)
  expect_true(all(c("id_plot", "plot_name") %in% names(recs)))
  expect_equal(nrow(CafriplotsR:::.upd_feature_summary(recs)), 0L)
  expect_true("plot_name" %in% names(CafriplotsR:::.upd_feature_summary(recs)))

  # No plot at all: same shape, no query.
  expect_equal(nrow(CafriplotsR:::.upd_plot_feature_records(integer(0), con)), 0L)
})

test_that(".upd_feature_summary() still groups by feature alone without plots", {
  recs <- dplyr::tibble(
    record_id = 1:2, feature = "wood_density", valuetype = "numeric",
    unit = NA_character_, min_allowed = NA_real_, max_allowed = NA_real_,
    value_num = c(0.5, 0.7), value_char = NA_character_,
    lookup_id = NA_integer_, value_display = c("0.5", "0.7"),
    year = 2020L, month = NA_integer_, day = NA_integer_,
    issue = NA_character_, context = NA_character_
  )
  s <- CafriplotsR:::.upd_feature_summary(
    CafriplotsR:::.upd_annotate_aggregation(recs, "plot")
  )

  expect_false("plot_name" %in% names(s))
  expect_equal(nrow(s), 1L)
  expect_equal(s$n_records, 2L)
})
