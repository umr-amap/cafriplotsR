# =============================================================================
# BACKEND FOR THE DATA UPDATE APP
# =============================================================================
#
# `update_records()` only ever writes *direct* columns. Its feature branch
# (`detect_feature_changes()` / `execute_feature_updates()`) exists to refuse
# the update and tell the user where the value really lives: a plot feature is
# a row of `data_liste_sub_plots`, an individual feature is a row of
# `data_traits_measures`, and the value shown in an extracted table may be the
# *aggregate* of several such rows.
#
# The functions below make that resolution explicit so a UI can edit the real
# records instead of the aggregate:
#
#   .upd_direct_fields()             - which flat columns are editable, and how
#   .upd_plot_feature_records()      - one row per data_liste_sub_plots record
#   .upd_individual_feature_records()- one row per data_traits_measures record
#   .upd_apply_direct()              - write flat columns (with backup)
#   .upd_apply_feature()             - write feature records (with backup)
#
# Everything here takes a plain DBI connection. Callers holding a pool should
# wrap with `.upd_with_con()`.

# Creation / modification bookkeeping, never user-editable.
.UPD_SYSTEM_COLS <- c(
  "date_creation_d", "date_creation_m", "date_creation_y",
  "date_modif_d", "date_modif_m", "date_modif_y",
  "user_modif"
)

# Column types we refuse to render as a form input.
.UPD_OPAQUE_TYPES <- c("USER-DEFINED", "bytea", "json", "jsonb", "ARRAY")

#' Run a function against a real DBI connection
#'
#' Accepts a pool or a plain connection and always hands `fun` a plain one,
#' returning a checked-out connection to the pool afterwards.
#'
#' @param con A `Pool` or a DBI connection.
#' @param fun Function of one argument (the connection).
#' @return Whatever `fun` returns.
#' @keywords internal
.upd_with_con <- function(con, fun) {
  if (inherits(con, "Pool")) {
    actual <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(actual), add = TRUE)
    fun(actual)
  } else {
    fun(con)
  }
}

#' Column types of a table, as a named character vector
#' @keywords internal
.upd_column_types <- function(table, con) {
  types <- tryCatch(
    DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT column_name, data_type
         FROM information_schema.columns
        WHERE table_name = {table}",
      .con = con
    )),
    error = function(e) {
      message("Note: could not read column types for ", table, " (", e$message, ")")
      data.frame(column_name = character(), data_type = character())
    }
  )
  stats::setNames(types$data_type, types$column_name)
}

#' Map a PostgreSQL type to the input kind the app renders
#' @keywords internal
.upd_input_kind <- function(pg_type) {
  if (is.na(pg_type) || length(pg_type) == 0) return("text")
  if (pg_type %in% c("integer", "bigint", "smallint")) return("integer")
  if (pg_type %in% c("numeric", "decimal", "real", "double precision")) return("numeric")
  if (pg_type == "boolean") return("boolean")
  "text"
}

#' Coerce a form value to the R type matching a PostgreSQL column
#'
#' Comparison against the stored value happens in R, so a numeric column must
#' be compared as a number - otherwise `12.50` and `12.5` read as a change.
#'
#' @keywords internal
.upd_coerce <- function(value, pg_type) {
  kind <- .upd_input_kind(pg_type)
  if (is.null(value)) return(NA)
  if (is.character(value) && !nzchar(trimws(value))) return(NA)
  if (length(value) == 1 && is.na(value)) return(NA)

  switch(
    kind,
    integer = suppressWarnings(as.integer(value)),
    numeric = suppressWarnings(as.numeric(value)),
    boolean = as.logical(value),
    as.character(value)
  )
}

# -----------------------------------------------------------------------------
# DIRECT (FLAT) COLUMNS
# -----------------------------------------------------------------------------

#' Editable flat columns of the plots or individuals table
#'
#' Deliberately an allow-list, not everything the schema happens to hold.
#' `data_individuals` and `data_liste_plots` both carry deprecated columns
#' (`dbh`, `code_individu`, `sous_plot_name` and others) that nothing writes any
#' more; offering them for editing would invite corrections that change no
#' behaviour, and in `dbh`'s case would silently disagree with the
#' `stem_diameter` measurements in `data_traits_measures`.
#'
#' `get_table_columns()` is the package's maintained answer to "which columns of
#' this table does a user actually set", already used by the import wizard and
#' by `update_records()`. This intersects it with the live schema, so a name it
#' lists that is not a real column (`plot_name` on individuals, which is reached
#' through `id_table_liste_plots_n`) drops out rather than producing a form
#' field that cannot be written.
#'
#' The friendly lookup names `method` and `country` are swapped for the id
#' columns that store them, and flagged `"lookup"` so the UI offers a dropdown.
#' `idtax_n` is flagged `"taxon"` so it gets a taxonomic search instead of a
#' raw id box.
#'
#' Columns the schema has but the allow-list omits are recorded in the
#' `hidden` attribute, so the UI can say what it is not showing.
#'
#' @param entity `"plot"` or `"individual"`.
#' @param con A DBI connection.
#'
#' @return A tibble with one row per editable column: `field`, `pg_type`,
#'   `kind` (`"text"`, `"numeric"`, `"integer"`, `"boolean"`, `"lookup"`,
#'   `"taxon"`), and for lookups `lookup_table`, `lookup_key`, `lookup_value`.
#'   Carries a `hidden` attribute listing the omitted schema columns.
#' @keywords internal
.upd_direct_fields <- function(entity = c("plot", "individual"), con) {
  entity <- match.arg(entity)

  spec <- .upd_entity_spec(entity)
  types <- .upd_column_types(spec$table, con)

  in_table <- tryCatch(DBI::dbListFields(con, spec$table), error = function(e) character(0))
  if (length(in_table) == 0) return(dplyr::tibble())

  allowed <- tryCatch(get_table_columns(spec$table, con), error = function(e) character(0))
  # A lookup is listed under its readable name but stored as an id.
  for (friendly in names(.UPD_LOOKUPS)) {
    if (friendly %in% allowed) {
      allowed <- union(setdiff(allowed, friendly), .UPD_LOOKUPS[[friendly]]$id_col)
    }
  }

  cols <- intersect(allowed, in_table)
  cols <- setdiff(cols, c(spec$id_column, .UPD_SYSTEM_COLS, spec$exclude))

  # Drop anything we cannot render honestly (geometry, blobs, arrays).
  opaque <- names(types)[types %in% .UPD_OPAQUE_TYPES]
  cols <- setdiff(cols, opaque)

  hidden <- setdiff(in_table, c(cols, spec$id_column, .UPD_SYSTEM_COLS))

  if (length(cols) == 0) {
    out <- dplyr::tibble()
    attr(out, "hidden") <- hidden
    return(out)
  }

  fields <- dplyr::tibble(
    field   = cols,
    pg_type = unname(types[cols]),
    kind    = vapply(unname(types[cols]), .upd_input_kind, character(1)),
    lookup_table = NA_character_,
    lookup_key   = NA_character_,
    lookup_value = NA_character_
  )

  for (friendly in names(.UPD_LOOKUPS)) {
    lk <- .UPD_LOOKUPS[[friendly]]
    i <- which(fields$field == lk$id_col)
    if (length(i) == 1) {
      fields$kind[i]         <- "lookup"
      fields$lookup_table[i] <- lk$lookup_table
      fields$lookup_key[i]   <- lk$lookup_key
      fields$lookup_value[i] <- lk$lookup_value
    }
  }

  i <- which(fields$field == "idtax_n")
  if (length(i) == 1) fields$kind[i] <- "taxon"

  attr(fields, "hidden") <- hidden
  fields
}

# The two plot columns stored as a foreign key but set by a readable value.
# Mirrors get_metadata_mappings_plots(); people features are not here because
# they are subplot features, not columns of data_liste_plots.
.UPD_LOOKUPS <- list(
  method = list(
    id_col = "id_method", lookup_table = "methodslist",
    lookup_key = "id_method", lookup_value = "method"
  ),
  country = list(
    id_col = "id_country", lookup_table = "table_countries",
    lookup_key = "id_country", lookup_value = "country"
  )
)

#' Table / id-column / update_records table_type for an entity
#' @keywords internal
.upd_entity_spec <- function(entity = c("plot", "individual")) {
  entity <- match.arg(entity)
  if (entity == "plot") {
    list(
      table       = "data_liste_plots",
      id_column   = "id_liste_plots",
      table_type  = "plots",
      # `plot_name` identifies the plot everywhere else in the schema; renaming
      # it from here would silently orphan file-based references.
      exclude     = character(0),
      feature_table   = "data_liste_sub_plots",
      feature_id      = "id_sub_plots",
      feature_type    = "subplot_features"
    )
  } else {
    list(
      table       = "data_individuals",
      id_column   = "id_n",
      table_type  = "individuals",
      # Plot membership is structural, not a value to edit.
      exclude     = "id_table_liste_plots_n",
      feature_table   = "data_traits_measures",
      feature_id      = "id_trait_measures",
      feature_type    = "individual_features"
    )
  }
}

#' Fetch the current row of a plot or an individual
#' @keywords internal
.upd_fetch_record <- function(entity, id, con) {
  spec <- .upd_entity_spec(entity)
  sql <- glue::glue_sql(
    "SELECT * FROM {`spec$table`} WHERE {`spec$id_column`} = {id}",
    .con = con
  )
  res <- DBI::dbGetQuery(con, sql)
  if (nrow(res) == 0) return(NULL)
  dplyr::as_tibble(res)[1, , drop = FALSE]
}

# -----------------------------------------------------------------------------
# FULL RECORD VIEW
# -----------------------------------------------------------------------------

#' Run a console-oriented query function without letting it reach the UI
#'
#' `query_plots()` is written for a console session: it prints, and some of its
#' branches print an htmlwidget. Inside an app that takes over the RStudio pane
#' the app itself is running in, which reads as a freeze. Printed output is
#' swallowed and the viewer is pointed at nothing for the duration of the call;
#' messages are left alone, so the console still says what the query did.
#'
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
.upd_quiet_query <- function(expr) {
  old <- options(viewer = function(url, ...) invisible(NULL))
  on.exit(options(old), add = TRUE)
  out <- NULL
  utils::capture.output(out <- expr)
  out
}

#' The record as `query_plots(output_style = "full")` returns it
#'
#' The edit form only carries the columns the app can write. To review what is
#' stored, the app shows the record the way an extraction shows it - plot
#' metadata for a plot, the individual row for an individual - with every
#' column the "full" style keeps, features included. An individual is asked for
#' with every census kept apart, so nothing measured is left out of the review.
#'
#' @param entity Either `"plot"` or `"individual"`.
#' @param id Integer record id (`id_liste_plots` or `id_n`).
#' @param con A pool or DBI connection to the main database.
#' @param con_taxa Optional pool or connection to the taxa database.
#' @return A tibble with a `field` column and one value column per returned
#'   row, or `NULL` when the query came back with nothing to show.
#' @keywords internal
.upd_full_record_view <- function(entity = c("plot", "individual"), id, con,
                                  con_taxa = NULL) {
  entity <- match.arg(entity)
  id <- as.integer(id)
  if (is.na(id)) return(NULL)

  res <- .upd_quiet_query(
    if (entity == "plot") {
      query_plots(
        id_plot = id, extract_individuals = FALSE, extract_traits = FALSE,
        map = FALSE, extract_coordinates = FALSE,
        remove_ids = FALSE, output_style = "full",
        con = con, con.taxa = con_taxa
      )
    } else {
      # show_multiple_census keeps every census in its own column. Without it
      # the extraction collapses them with census_strategy and the review panel
      # would hide measurements the record actually holds.
      query_plots(
        id_individual = id, extract_individuals = TRUE,
        show_multiple_census = TRUE,
        map = FALSE, extract_coordinates = FALSE,
        remove_ids = FALSE, output_style = "full",
        con = con, con.taxa = con_taxa
      )
    }
  )

  .upd_transpose_record(.upd_full_record_table(res, entity))
}

#' Pick the table holding the record out of a query_plots() result
#'
#' `query_plots()` returns a list of tables, or a bare data frame when only one
#' table came back.
#'
#' @keywords internal
.upd_full_record_table <- function(res, entity) {
  if (is.null(res)) return(NULL)
  if (is.data.frame(res)) return(res)
  if (!is.list(res)) return(NULL)

  wanted <- if (entity == "plot") {
    c("metadata", "meta_data")
  } else {
    c("individuals", "extract", "metadata", "meta_data")
  }
  for (nm in wanted) {
    if (is.data.frame(res[[nm]])) return(res[[nm]])
  }
  NULL
}

#' One row per column of a record, values rendered as text
#'
#' A record read across is unreadable once it has a hundred columns, so it is
#' turned on its side: one row per column, one value column per record row (an
#' individual can come back as several rows, one per stem or census).
#'
#' @keywords internal
.upd_transpose_record <- function(tbl) {
  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)

  # Drop the sf class first: subsetting an sf object keeps its geometry column
  # whatever the selection says, and geometry has no honest one-line rendering.
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
  keep <- vapply(tbl, function(x) !inherits(x, c("sfc", "sfg")), logical(1))
  tbl <- tbl[, keep, drop = FALSE]
  if (ncol(tbl) == 0) return(NULL)

  out <- dplyr::tibble(field = names(tbl))
  single <- nrow(tbl) == 1
  for (i in seq_len(nrow(tbl))) {
    values <- vapply(names(tbl), function(cl) .upd_fmt_value(tbl[[cl]][i]),
                     character(1), USE.NAMES = FALSE)
    out[[if (single) "value" else paste0("value_", i)]] <- values
  }
  out
}

#' Render one stored value as a single string, empty when there is none
#' @keywords internal
.upd_fmt_value <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (is.null(x) || length(x) == 0) return("")
  x <- x[!is.na(x)]
  if (length(x) == 0) return("")
  paste(trimws(format(x, trim = TRUE)), collapse = ", ")
}

#' Choices for a lookup column, as a named vector (label -> id)
#' @keywords internal
.upd_lookup_choices <- function(lookup_table, lookup_key, lookup_value, con) {
  tryCatch({
    sql <- glue::glue_sql(
      "SELECT {`lookup_key`} AS key, {`lookup_value`} AS value
         FROM {`lookup_table`}
        ORDER BY {`lookup_value`}",
      .con = con
    )
    res <- DBI::dbGetQuery(con, sql)
    res <- res[!is.na(res$value) & nzchar(as.character(res$value)), , drop = FALSE]
    stats::setNames(as.character(res$key), as.character(res$value))
  }, error = function(e) {
    message("Note: could not read lookup ", lookup_table, " (", e$message, ")")
    character(0)
  })
}

# -----------------------------------------------------------------------------
# FEATURE RECORDS
# -----------------------------------------------------------------------------

# Shared shape returned by both feature resolvers, so one renderer serves both.
.UPD_FEATURE_COLS <- c(
  "record_id", "feature", "valuetype", "unit",
  "min_allowed", "max_allowed",
  "value_num", "value_char", "lookup_id", "value_display",
  "year", "month", "day", "issue", "context",
  "n_records", "agg_rule", "aggregate_display"
)

#' Plot feature records behind the columns of an extracted plot table
#'
#' Each row of `data_liste_sub_plots` for the plot, annotated with how many
#' records feed the same extracted column (`n_records`) and what that column
#' would show (`aggregate_display`). When `n_records > 1` the extracted value is
#' an aggregate and only these records can be edited.
#'
#' @param id_plot Integer, `data_liste_plots.id_liste_plots`.
#' @param con A DBI connection.
#' @return A tibble, one row per subplot record; zero rows if the plot has none.
#' @keywords internal
.upd_plot_feature_records <- function(id_plot, con) {

  sql <- glue::glue_sql(
    "SELECT
       sp.id_sub_plots,
       sp.year, sp.month, sp.day,
       sp.typevalue, sp.typevalue_char,
       sp.original_subplot_name, sp.issue,
       spt.type, spt.valuetype, spt.expectedunit,
       spt.minallowedvalue, spt.maxallowedvalue
     FROM data_liste_sub_plots sp
     LEFT JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
     WHERE sp.id_table_liste_plots = {id_plot}
     ORDER BY spt.type, sp.year, sp.id_sub_plots",
    .con = con
  )
  raw <- dplyr::as_tibble(DBI::dbGetQuery(con, sql))
  if (nrow(raw) == 0) return(.upd_empty_features())

  # A table_colnam feature is a numeric feature whose value is the
  # table_colnam id, held in `typevalue`. `typevalue_char` is never used for
  # these, and `id_colnam` is not their store either - it is populated on a
  # negligible number of rows and only by mistake.
  colnam_lookup <- if (any(!is.na(raw$valuetype) & grepl("^table_", raw$valuetype))) {
    tryCatch(
      dplyr::as_tibble(DBI::dbGetQuery(
        con, "SELECT id_table_colnam, colnam FROM table_colnam"
      )),
      error = function(e) dplyr::tibble(id_table_colnam = integer(), colnam = character())
    )
  } else {
    dplyr::tibble(id_table_colnam = integer(), colnam = character())
  }

  out <- raw %>%
    dplyr::mutate(
      record_id   = .data$id_sub_plots,
      feature     = .data$type,
      valuetype   = .data$valuetype,
      unit        = .data$expectedunit,
      min_allowed = suppressWarnings(as.numeric(.data$minallowedvalue)),
      max_allowed = suppressWarnings(as.numeric(.data$maxallowedvalue)),
      value_num   = suppressWarnings(as.numeric(.data$typevalue)),
      value_char  = stringr::str_squish(.data$typevalue_char),
      lookup_id   = suppressWarnings(as.integer(.data$typevalue)),
      context     = .data$original_subplot_name
    )

  out$value_display <- .upd_display_value(
    out$valuetype, out$value_num, out$value_char, out$lookup_id, colnam_lookup
  )

  out <- out %>%
    dplyr::select(dplyr::any_of(c(
      "record_id", "feature", "valuetype", "unit", "min_allowed", "max_allowed",
      "value_num", "value_char", "lookup_id", "value_display",
      "year", "month", "day", "issue", "context"
    )))

  .upd_annotate_aggregation(out, "plot")
}

#' Individual feature records behind the columns of an extracted individual table
#'
#' Each row of `data_traits_measures` for the individual, with the census it
#' belongs to and the same aggregation annotation as the plot resolver. A trait
#' measured at several censuses has `n_records > 1`, which is exactly the case
#' `detect_feature_changes()` refuses.
#'
#' @param id_ind Integer, `data_individuals.id_n`.
#' @param con A DBI connection.
#' @return A tibble, one row per measurement; zero rows if there are none.
#' @keywords internal
.upd_individual_feature_records <- function(id_ind, con) {

  sql <- glue::glue_sql(
    "SELECT
       tm.id_trait_measures,
       tm.id_sub_plots,
       tm.traitvalue, tm.traitvalue_char, tm.issue,
       tm.year, tm.month, tm.day,
       tl.trait, tl.valuetype, tl.expectedunit,
       tl.minallowedvalue, tl.maxallowedvalue,
       CONCAT(spt.type, '_', sp.typevalue) AS census_name
     FROM data_traits_measures tm
     LEFT JOIN traitlist tl ON tm.traitid = tl.id_trait
     LEFT JOIN data_liste_sub_plots sp ON tm.id_sub_plots = sp.id_sub_plots
     LEFT JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
     WHERE tm.id_data_individuals = {id_ind}
     ORDER BY tl.trait, sp.typevalue, tm.id_trait_measures",
    .con = con
  )
  raw <- dplyr::as_tibble(DBI::dbGetQuery(con, sql))
  if (nrow(raw) == 0) return(.upd_empty_features())

  out <- raw %>%
    dplyr::mutate(
      record_id   = .data$id_trait_measures,
      feature     = .data$trait,
      unit        = .data$expectedunit,
      min_allowed = suppressWarnings(as.numeric(.data$minallowedvalue)),
      max_allowed = suppressWarnings(as.numeric(.data$maxallowedvalue)),
      value_num   = suppressWarnings(as.numeric(.data$traitvalue)),
      value_char  = stringr::str_squish(.data$traitvalue_char),
      lookup_id   = NA_integer_,
      context     = .data$census_name
    )

  out$value_display <- .upd_display_value(
    out$valuetype, out$value_num, out$value_char, out$lookup_id,
    dplyr::tibble(id_table_colnam = integer(), colnam = character())
  )

  out <- out %>%
    dplyr::select(dplyr::any_of(c(
      "record_id", "feature", "valuetype", "unit", "min_allowed", "max_allowed",
      "value_num", "value_char", "lookup_id", "value_display",
      "year", "month", "day", "issue", "context"
    )))

  .upd_annotate_aggregation(out, "individual")
}

#' Empty feature table with the documented shape
#' @keywords internal
.upd_empty_features <- function() {
  empty <- dplyr::tibble(
    record_id = integer(), feature = character(), valuetype = character(),
    unit = character(), min_allowed = numeric(), max_allowed = numeric(),
    value_num = numeric(), value_char = character(), lookup_id = integer(),
    value_display = character(), year = integer(), month = integer(),
    day = integer(), issue = character(), context = character(),
    n_records = integer(), agg_rule = character(), aggregate_display = character()
  )
  empty
}

#' Human-readable value of a feature record
#' @keywords internal
.upd_display_value <- function(valuetype, value_num, value_char, lookup_id,
                               colnam_lookup) {
  n <- length(valuetype)
  out <- character(n)
  for (i in seq_len(n)) {
    vt <- valuetype[i]
    out[i] <- if (!is.na(vt) && grepl("^table_", vt)) {
      id <- lookup_id[i]
      hit <- colnam_lookup$colnam[colnam_lookup$id_table_colnam == id]
      if (!is.na(id) && length(hit) == 1) hit else as.character(id)
    } else if (!is.na(vt) && vt == "numeric") {
      if (is.na(value_num[i])) NA_character_ else as.character(value_num[i])
    } else {
      if (!is.na(value_char[i])) value_char[i]
      else if (!is.na(value_num[i])) as.character(value_num[i])
      else NA_character_
    }
  }
  out
}

#' Annotate feature records with what the extracted table shows for them
#'
#' The extraction does not treat every feature the same way, so neither can
#' this. `aggregate_plot_features()` averages numeric subplot features, joins
#' character and table-referenced ones, and hands `census` rows to
#' `extract_census_dates()` instead - a plot table carries `n_census`,
#' `first_census`, `last_census` and one `date_census_N` column per census, never
#' the mean of the census numbers. On the individual side
#' `aggregate_numeric_features_dt()` / `aggregate_character_features_dt()`
#' aggregate *within* each census, so a census-linked trait becomes one column
#' per census rather than one value.
#'
#' `agg_rule` records which of those happens; `aggregate_display` is what the
#' extracted table would show, and is `NA` when there is no single value to show.
#'
#' @param records Feature records from one of the two resolvers.
#' @param entity Either `"plot"` or `"individual"`.
#' @return `records` with `n_records`, `agg_rule` and `aggregate_display`.
#' @keywords internal
.upd_annotate_aggregation <- function(records, entity = c("plot", "individual")) {
  entity <- match.arg(entity)
  if (nrow(records) == 0) return(.upd_empty_features())

  records$n_records <- NA_integer_
  records$agg_rule <- NA_character_
  records$aggregate_display <- NA_character_

  for (idx in split(seq_len(nrow(records)), records$feature)) {
    grp <- records[idx, , drop = FALSE]
    rule <- .upd_agg_rule(entity, grp)
    records$n_records[idx] <- nrow(grp)
    records$agg_rule[idx] <- rule
    records$aggregate_display[idx] <- .upd_agg_display(rule, grp)
  }

  records
}

#' How the extraction treats one feature's records
#'
#' The rule is what the extraction *does*, not how many records happen to be
#' there: a single record still goes through the same mean or join, and the
#' caller has `n_records` when it wants to say "one record".
#'
#' @return One of `"census"`, `"per_census"`, `"mean"`, `"concat"`,
#'   `"not_extracted"` or `"other"`.
#' @keywords internal
.upd_agg_rule <- function(entity, grp) {
  vt <- grp$valuetype[1]

  # The census subplot type is the plot's census list, not a measurement.
  if (entity == "plot" && identical(grp$feature[1], "census")) return("census")

  # A measurement carrying a census belongs to a moment in time, and the
  # extraction keeps the censuses apart.
  if (entity == "individual" && any(!is.na(grp$context))) return("per_census")

  numeric_types <- if (entity == "plot") "numeric" else c("numeric", "integer")
  text_types <- if (entity == "plot") {
    "character"
  } else {
    c("character", "ordinal", "categorical")
  }

  if (!is.na(vt) && vt %in% numeric_types) return("mean")
  if (!is.na(vt) && vt %in% text_types) return("concat")
  # Table references are resolved and joined on the plot side only.
  if (!is.na(vt) && grepl("^table_", vt) && entity == "plot") return("concat")

  # Nothing in aggregate_plot_features() picks these up.
  if (entity == "plot") "not_extracted" else "other"
}

#' The value an extracted table would show for one feature
#' @keywords internal
.upd_agg_display <- function(rule, grp) {
  switch(
    rule,
    census     = .upd_census_display(grp),
    per_census = .upd_per_census_display(grp),
    mean       = .upd_mean_display(grp),
    concat = ,
    other  = {
      vals <- unique(stats::na.omit(grp$value_display))
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
    },
    NA_character_
  )
}

#' What a plot table shows for the census feature: how many, and when
#' @keywords internal
.upd_census_display <- function(grp) {
  dates <- .upd_record_dates(grp)[order(grp$value_num, na.last = TRUE)]
  dates <- dates[nzchar(dates)]
  if (length(dates) == 0) return(sprintf("n_census = %d", nrow(grp)))
  sprintf("n_census = %d (%s)", nrow(grp), paste(dates, collapse = ", "))
}

#' One value per census, the way the extraction keeps them apart
#' @keywords internal
.upd_per_census_display <- function(grp) {
  census <- ifelse(is.na(grp$context), "-", as.character(grp$context))
  groups <- split(seq_len(nrow(grp)), census)
  parts <- vapply(names(groups), function(nm) {
    rows <- grp[groups[[nm]], , drop = FALSE]
    value <- if (!is.na(rows$valuetype[1]) &&
                 rows$valuetype[1] %in% c("numeric", "integer")) {
      .upd_mean_display(rows)
    } else {
      vals <- unique(stats::na.omit(rows$value_display))
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
    }
    sprintf("%s: %s", nm, if (is.na(value)) "-" else value)
  }, character(1), USE.NAMES = FALSE)
  paste(parts, collapse = " | ")
}

#' Mean of a feature's numeric records, rounded the way the extraction rounds
#' @keywords internal
.upd_mean_display <- function(grp) {
  if (all(is.na(grp$value_num))) return(NA_character_)
  format(round(mean(grp$value_num, na.rm = TRUE), 2), trim = TRUE)
}

#' Dates of feature records as `YYYY`, `YYYY-MM` or `YYYY-MM-DD`
#' @keywords internal
.upd_record_dates <- function(grp) {
  out <- rep("", nrow(grp))
  y <- suppressWarnings(as.integer(grp$year))
  m <- suppressWarnings(as.integer(grp$month))
  d <- suppressWarnings(as.integer(grp$day))

  has_y <- !is.na(y)
  out[has_y] <- as.character(y[has_y])
  has_m <- has_y & !is.na(m)
  out[has_m] <- sprintf("%s-%02d", out[has_m], m[has_m])
  has_d <- has_m & !is.na(d)
  out[has_d] <- sprintf("%s-%02d", out[has_d], d[has_d])
  out
}

#' One row per feature: what the extracted table shows, and from how many records
#' @keywords internal
.upd_feature_summary <- function(records) {
  if (nrow(records) == 0) {
    return(dplyr::tibble(
      feature = character(), valuetype = character(), unit = character(),
      n_records = integer(), agg_rule = character(),
      aggregate_display = character(), is_aggregated = logical()
    ))
  }
  records %>%
    dplyr::group_by(.data$feature) %>%
    dplyr::summarise(
      valuetype         = dplyr::first(.data$valuetype),
      unit              = dplyr::first(.data$unit),
      n_records         = dplyr::n(),
      agg_rule          = dplyr::first(.data$agg_rule),
      aggregate_display = dplyr::first(.data$aggregate_display),
      .groups = "drop"
    ) %>%
    dplyr::mutate(is_aggregated = .data$n_records > 1) %>%
    dplyr::arrange(dplyr::desc(.data$is_aggregated), .data$feature)
}

#' Which database column a feature record's value lives in
#'
#' A `table_*` feature is a numeric feature whose value is the referenced
#' table's id, so it shares the numeric column with plain numeric features. The
#' character column is never used for one.
#'
#' @param valuetype The feature's `valuetype`.
#' @param entity `"plot"` or `"individual"`.
#' @return `"typevalue"` / `"typevalue_char"` for plots,
#'   `"traitvalue"` / `"traitvalue_char"` for individuals.
#' @keywords internal
.upd_value_column <- function(valuetype, entity = c("plot", "individual")) {
  entity <- match.arg(entity)
  numeric_like <- !is.na(valuetype) &&
    (valuetype %in% c("numeric", "integer") || grepl("^table_", valuetype))
  if (entity == "plot") {
    if (numeric_like) "typevalue" else "typevalue_char"
  } else {
    if (numeric_like) "traitvalue" else "traitvalue_char"
  }
}

# -----------------------------------------------------------------------------
# APPLYING UPDATES
# -----------------------------------------------------------------------------

#' Config for `detect_direct_changes()` / `execute_direct_updates()`
#'
#' `get_column_routing()` is the single source for table, id column and backup
#' table. Its `direct_columns` are tuned for the import wizard (friendly names
#' such as `method`, and `plot_name` on individuals where no such column
#' exists), so the app supplies its own list, derived from the live schema.
#'
#' @keywords internal
.upd_routing <- function(table_type, columns, con) {
  config <- get_column_routing(table_type, con)
  config$direct_columns <- columns
  config$metadata_mappings <- NULL
  config
}

#' Write flat columns of a plot or an individual
#'
#' Re-reads the stored values and writes only what actually differs, backing the
#' records up first via `execute_direct_updates()`.
#'
#' @param entity `"plot"` or `"individual"`.
#' @param id The record id.
#' @param values Named list of database column -> new value. `NA` clears.
#' @param con A DBI connection.
#' @return The change tibble that was applied (invisibly `NULL` if nothing did).
#' @keywords internal
.upd_apply_direct <- function(entity, id, values, con) {
  spec <- .upd_entity_spec(entity)
  values <- values[!vapply(values, is.null, logical(1))]
  if (length(values) == 0) return(NULL)

  data <- dplyr::as_tibble(c(
    stats::setNames(list(id), spec$id_column),
    values
  ))

  config <- .upd_routing(spec$table_type, names(values), con)
  changes <- detect_direct_changes(data, columns = names(values), config, con)
  if (is.null(changes) || nrow(changes) == 0) return(NULL)

  execute_direct_updates(changes, config, method = "single", con = con)
  changes
}

#' Write a record's flat columns and its feature records as one unit
#'
#' Flat columns and features live in different tables, so applying them
#' separately can leave a record half-updated if the second write fails. Both
#' go inside one transaction; anything raised rolls the whole edit back.
#'
#' @param entity `"plot"` or `"individual"`.
#' @param id The record id.
#' @param values Named list of database column -> new value for the flat table.
#' @param features Named list keyed by feature record id (see
#'   [.upd_apply_feature()]).
#' @param con A DBI connection.
#' @return A list with `n_direct` and `n_feature`: how many values were written.
#' @keywords internal
.upd_apply_all <- function(entity, id, values, features, con) {
  DBI::dbBegin(con)
  result <- tryCatch({
    direct_applied  <- .upd_apply_direct(entity, id, values, con)
    feature_applied <- .upd_apply_feature(entity, features, con)
    DBI::dbCommit(con)
    list(
      n_direct  = if (is.null(direct_applied)) 0L else nrow(direct_applied),
      n_feature = if (is.null(feature_applied)) 0L else nrow(feature_applied)
    )
  }, error = function(e) {
    tryCatch(DBI::dbRollback(con), error = function(e2) {
      cli::cli_alert_warning("Rollback failed: {e2$message}")
    })
    stop(e)
  })
  result
}

#' Write feature records
#'
#' @param entity `"plot"` or `"individual"`.
#' @param values A named list keyed by record id; each element a named list of
#'   feature-table column -> new value.
#' @param con A DBI connection.
#' @return The change tibble that was applied, or `NULL`.
#' @keywords internal
.upd_apply_feature <- function(entity, values, con) {
  spec <- .upd_entity_spec(entity)
  if (length(values) == 0) return(NULL)

  columns <- unique(unlist(lapply(values, names), use.names = FALSE))
  if (length(columns) == 0) return(NULL)

  rows <- lapply(names(values), function(rid) {
    row <- values[[rid]]
    filled <- stats::setNames(
      lapply(columns, function(cl) if (cl %in% names(row)) row[[cl]] else NULL),
      columns
    )
    # A column this record does not touch must stay untouched: keeping it out of
    # the comparison is the only way to say "not changed" rather than "cleared".
    filled <- filled[!vapply(filled, is.null, logical(1))]
    dplyr::as_tibble(c(
      stats::setNames(list(as.integer(rid)), spec$feature_id),
      filled
    ))
  })

  applied <- NULL
  # Records touching different column sets are written separately so an absent
  # column is never read as a request to set NULL.
  groups <- split(rows, vapply(rows, function(r) paste(sort(names(r)), collapse = "|"), character(1)))
  for (grp in groups) {
    data <- dplyr::bind_rows(grp)
    cols <- setdiff(names(data), spec$feature_id)
    if (length(cols) == 0) next

    config <- .upd_routing(spec$feature_type, cols, con)
    changes <- detect_direct_changes(data, columns = cols, config, con)
    if (is.null(changes) || nrow(changes) == 0) next

    execute_direct_updates(changes, config, method = "single", con = con)
    applied <- dplyr::bind_rows(applied, changes)
  }

  applied
}

# -----------------------------------------------------------------------------
# IDENTIFICATION CASCADE
# -----------------------------------------------------------------------------

#' The identification an extraction will actually use for an individual
#'
#' `merge_individuals_taxa()` does not read `data_individuals.idtax_n` and stop
#' there. It resolves that id through `table_idtax` synonymy into `idtax_f`,
#' resolves the identification of the specimen linked to the individual the same
#' way into `idtax_specimen_f`, and then takes
#' `idtax_individual_f = coalesce(idtax_specimen_f, idtax_f)`. Everything
#' downstream - taxonomy, traits, the name in an extracted table - hangs off
#' `idtax_individual_f`.
#'
#' The consequence matters to anyone editing a record: while a specimen is
#' linked, the specimen's identification wins, and correcting `idtax_n` here
#' changes nothing an extraction will show.
#'
#' The linked specimen is picked the way `merge_individuals_taxa()` picks it:
#' highest `linktypelist.priority` first, then the most recent determination
#' date.
#'
#' @param id_ind Integer, `data_individuals.id_n`.
#' @param con A DBI connection to the main database.
#' @param con_taxa Optional connection or pool to the taxa database, used for
#'   names only; ids are shown alone without it.
#' @return A list with `idtax_n`, `original_tax_name`, `idtax_f`, `specimen`
#'   (`NULL` or a one-row data frame), `idtax_specimen_f`,
#'   `idtax_individual_f`, `governed_by` (`"specimen"` or `"individual"`),
#'   `is_synonym`, and `names` (idtax as character -> taxon name).
#'   `NULL` when there is no such individual.
#' @keywords internal
.upd_identification <- function(id_ind, con, con_taxa = NULL) {
  id_ind <- suppressWarnings(as.integer(id_ind))
  if (is.na(id_ind)) return(NULL)

  ind <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT id_n, idtax_n, original_tax_name
       FROM data_individuals WHERE id_n = {id_ind}",
    .con = con
  ))
  if (nrow(ind) == 0) return(NULL)

  idtax_n <- suppressWarnings(as.integer(ind$idtax_n[1]))
  specimen <- .upd_specimen_link(id_ind, con)
  idtax_spec_n <- if (is.null(specimen)) {
    NA_integer_
  } else {
    suppressWarnings(as.integer(specimen$idtax_specimen_n[1]))
  }

  accepted <- .upd_accepted_idtax(c(idtax_n, idtax_spec_n), con)
  idtax_f <- unname(accepted[as.character(idtax_n)])
  if (length(idtax_f) == 0 || is.na(idtax_f)) idtax_f <- idtax_n
  idtax_specimen_f <- if (is.na(idtax_spec_n)) {
    NA_integer_
  } else {
    v <- unname(accepted[as.character(idtax_spec_n)])
    if (length(v) == 0 || is.na(v)) idtax_spec_n else v
  }

  idtax_individual_f <- if (!is.na(idtax_specimen_f)) idtax_specimen_f else idtax_f

  names_tbl <- .fetch_taxon_names(
    c(idtax_n, idtax_f, idtax_spec_n, idtax_specimen_f, idtax_individual_f),
    con_taxa
  )
  taxon_names <- stats::setNames(
    as.character(names_tbl$taxon_name), as.character(names_tbl$idtax_n)
  )

  list(
    id_n               = id_ind,
    idtax_n            = idtax_n,
    original_tax_name  = ind$original_tax_name[1],
    idtax_f            = idtax_f,
    is_synonym         = !is.na(idtax_f) && !is.na(idtax_n) && idtax_f != idtax_n,
    specimen           = specimen,
    idtax_specimen_n   = idtax_spec_n,
    idtax_specimen_f   = idtax_specimen_f,
    idtax_individual_f = idtax_individual_f,
    governed_by        = if (!is.na(idtax_specimen_f)) "specimen" else "individual",
    names              = taxon_names
  )
}

#' The specimen whose identification governs an individual
#'
#' Highest link priority first, then the most recent determination date - the
#' order `merge_individuals_taxa()` sorts by before taking one link per
#' individual.
#'
#' @return A one-row data frame, or `NULL` when nothing is linked.
#' @keywords internal
.upd_specimen_link <- function(id_ind, con) {
  base_select <-
    "SELECT ls.id_specimen,
            s.idtax_n AS idtax_specimen_n,
            s.colnbr, s.suffix, s.dety, s.detm, s.detd,
            cn.colnam"
  base_from <-
    "FROM data_link_specimens ls
       LEFT JOIN specimens s ON ls.id_specimen = s.id_specimen
       LEFT JOIN table_colnam cn ON s.id_colnam = cn.id_table_colnam"
  det_order <-
    "COALESCE(s.dety, 1900) DESC, COALESCE(s.detm, 1) DESC, COALESCE(s.detd, 1) DESC"

  # linktypelist may not exist on every database; without it every link has the
  # same priority and only the determination date orders them.
  res <- tryCatch(
    DBI::dbGetQuery(con, glue::glue_sql(
      paste(base_select, ", COALESCE(lt.priority, 0) AS priority",
            base_from,
            "LEFT JOIN linktypelist lt ON ls.id_linktype = lt.id_linktype",
            "WHERE ls.id_n = {id_ind}",
            "ORDER BY COALESCE(lt.priority, 0) DESC,", det_order,
            "LIMIT 1"),
      .con = con
    )),
    error = function(e) {
      tryCatch(
        DBI::dbGetQuery(con, glue::glue_sql(
          paste(base_select, ", 0 AS priority", base_from,
                "WHERE ls.id_n = {id_ind}", "ORDER BY", det_order, "LIMIT 1"),
          .con = con
        )),
        error = function(e2) {
          message("Note: could not read specimen links (", conditionMessage(e2), ")")
          NULL
        }
      )
    }
  )

  if (is.null(res) || nrow(res) == 0) return(NULL)
  res[1, , drop = FALSE]
}

#' Accepted id for each taxon id, through `table_idtax` synonymy
#'
#' @return Named integer vector, idtax as character -> accepted idtax. Ids the
#'   table does not know map to themselves.
#' @keywords internal
.upd_accepted_idtax <- function(idtax, con) {
  idtax <- unique(suppressWarnings(as.integer(idtax)))
  idtax <- idtax[!is.na(idtax)]
  if (length(idtax) == 0) return(stats::setNames(integer(0), character(0)))

  res <- tryCatch(
    DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT idtax_n, idtax_good_n FROM table_idtax WHERE idtax_n IN ({idtax*})",
      idtax = idtax, .con = con
    )),
    error = function(e) {
      message("Note: could not read table_idtax (", conditionMessage(e), ")")
      data.frame(idtax_n = integer(0), idtax_good_n = integer(0))
    }
  )

  out <- stats::setNames(idtax, as.character(idtax))
  if (nrow(res) > 0) {
    good <- suppressWarnings(as.integer(res$idtax_good_n))
    known <- suppressWarnings(as.integer(res$idtax_n))
    good[is.na(good)] <- known[is.na(good)]
    out[as.character(known)] <- good
  }
  out
}

#' A collector's label for a specimen: "Dauby 1234b"
#' @keywords internal
.upd_specimen_label <- function(specimen) {
  if (is.null(specimen) || nrow(specimen) == 0) return(NA_character_)
  number <- paste0(
    if (!is.na(specimen$colnbr[1])) as.character(specimen$colnbr[1]) else "",
    if (!is.na(specimen$suffix[1])) as.character(specimen$suffix[1]) else ""
  )
  parts <- c(
    if (!is.na(specimen$colnam[1])) as.character(specimen$colnam[1]),
    if (nzchar(number)) number
  )
  if (length(parts) == 0) return(paste0("id_specimen ", specimen$id_specimen[1]))
  paste(parts, collapse = " ")
}
