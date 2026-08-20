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

  .upd_annotate_aggregation(out)
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

  .upd_annotate_aggregation(out)
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

#' Annotate feature records with how the extracted column aggregates them
#'
#' Mirrors the extraction path: numeric features are averaged
#' (`aggregate_numeric_plot_features()`, `aggregate_numeric_features_dt()`),
#' character and table-referenced features are concatenated over unique values.
#'
#' @keywords internal
.upd_annotate_aggregation <- function(records) {
  if (nrow(records) == 0) return(.upd_empty_features())

  records %>%
    dplyr::group_by(.data$feature) %>%
    dplyr::mutate(
      n_records = dplyr::n(),
      agg_rule  = if (isTRUE(dplyr::first(.data$valuetype) == "numeric")) "mean" else "concat",
      aggregate_display = if (isTRUE(dplyr::first(.data$valuetype) == "numeric")) {
        if (all(is.na(.data$value_num))) NA_character_
        else format(mean(.data$value_num, na.rm = TRUE), trim = TRUE)
      } else {
        vals <- unique(stats::na.omit(.data$value_display))
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
      }
    ) %>%
    dplyr::ungroup()
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
