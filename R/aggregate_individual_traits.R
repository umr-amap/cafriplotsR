# =============================================================================
# Build taxa-level traits by aggregating individual-level measurements
#
# Public entry point: rebuild_aggregated_taxa_traits()
#
# Workflow on each rebuild:
#   1. Read aggregation rules from `trait_aggregation_config`.
#   2. Resolve / create the citation row that tags aggregated outputs (default
#      key "CafriplotsR_aggregated", is_public = FALSE).
#   3. For each rule, fetch individual values for `source_trait_id` from
#      `data_traits_measures`, join individuals -> idtax, replace idtax with
#      the accepted-name id (`idtax_good`), aggregate per accepted taxon, and
#      build rows for `taxa_traits_measures` tagged with the citation.
#   4. In a single transaction: delete prior rows tagged with this citation
#      (optionally restricted to the touched output trait ids) and insert the
#      newly computed rows.
#
# Aggregated rows are also tagged with:
#   basisofrecord       = "AggregatedFromIndividual"
#   measurementremarks  = "<method>(n=<n>) [param=<x>]"
#
# Access control: see R/migrate_add_aggregated_traits.R — the citation row is
# created with is_public = FALSE, and the RLS policy on taxa_traits_measures
# hides matching rows from the public role.
# =============================================================================


# -----------------------------------------------------------------------------
# Pure aggregation kernel (no DB) — exposed for unit testing.
# -----------------------------------------------------------------------------

#' Compute one aggregate value
#'
#' Pure function that turns a numeric (or character) vector into a single
#' aggregated value. NA values are removed before computation.
#'
#' @param values Numeric vector (or character for `mode`/`concat`/`count`).
#' @param method One of `"mean"`, `"median"`, `"min"`, `"max"`, `"sum"`,
#'   `"sd"`, `"percentile"`, `"mode"`, `"concat"`, `"count"`.
#' @param method_param Numeric parameter required by `"percentile"`
#'   (e.g. 95 for the 95th percentile). Ignored for other methods.
#' @return Length-1 list with components `value_num` (numeric) and
#'   `value_char` (character) — exactly one is non-NA depending on the
#'   method's output type. For `"count"` and `"n"`, returns `value_num`.
#' @keywords internal
#' @export
.compute_aggregate <- function(values, method, method_param = NULL) {

  out <- list(value_num = NA_real_, value_char = NA_character_, n = 0L)

  numeric_methods <- c("mean", "median", "min", "max", "sum", "sd",
                       "percentile", "count")
  char_methods    <- c("mode", "concat")

  if (!method %in% c(numeric_methods, char_methods)) {
    stop("Unknown aggregation method: ", method)
  }

  if (method %in% numeric_methods) {
    v <- suppressWarnings(as.numeric(values))
    v <- v[!is.na(v)]
    out$n <- length(v)
    if (length(v) == 0L) return(out)

    out$value_num <- switch(
      method,
      mean       = mean(v),
      median     = stats::median(v),
      min        = min(v),
      max        = max(v),
      sum        = sum(v),
      sd         = if (length(v) >= 2L) stats::sd(v) else NA_real_,
      percentile = {
        if (is.null(method_param) || !is.finite(method_param)) {
          stop("method_param required for 'percentile'")
        }
        if (method_param < 0 || method_param > 100) {
          stop("method_param for 'percentile' must be in [0, 100]")
        }
        unname(stats::quantile(v, probs = method_param / 100,
                               names = FALSE, na.rm = TRUE))
      },
      count      = length(v)
    )
    return(out)
  }

  # Character methods
  v <- as.character(values)
  v <- v[!is.na(v) & nzchar(trimws(v))]
  out$n <- length(v)
  if (length(v) == 0L) return(out)

  if (method == "mode") {
    tab <- sort(table(v), decreasing = TRUE)
    out$value_char <- names(tab)[1]
  } else { # concat
    out$value_char <- paste(unique(v), collapse = "; ")
  }
  out
}


# -----------------------------------------------------------------------------
# Config CRUD
# -----------------------------------------------------------------------------

#' Assert that `trait_aggregation_config` exists with the expected schema.
#'
#' Throws a clear error pointing at `migrate_aggregated_traits_all()` if the
#' table is missing or has been re-created with a wrong shape (e.g. by an
#' accidental `dbWriteTable(append = TRUE)` against a dropped table).
#'
#' @keywords internal
.assert_aggregation_config_schema <- function(con) {
  required <- c("id_aggregation", "source_trait_id", "target_trait_id",
                "method", "method_param", "min_n", "is_active")

  cols <- tryCatch(
    DBI::dbGetQuery(con,
      "SELECT column_name FROM information_schema.columns
        WHERE table_name = 'trait_aggregation_config'")$column_name,
    error = function(e) character()
  )

  if (length(cols) == 0L) {
    cli::cli_abort(c(
      "Table {.field trait_aggregation_config} is missing.",
      i = "Run {.run migrate_aggregated_traits_all(con, dry_run = FALSE)} to (re)create it."
    ))
  }

  missing_cols <- setdiff(required, cols)
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "Table {.field trait_aggregation_config} is malformed \\
       (missing column{?s}: {.field {missing_cols}}).",
      i = "This usually means the table was dropped and silently re-created \\
           by {.fn dbWriteTable}. Drop it and re-run \\
           {.run migrate_aggregated_traits_all(con, dry_run = FALSE)}."
    ))
  }

  invisible(TRUE)
}


#' List active trait aggregation rules
#'
#' Returns one row per rule with both `source_trait_id` / `source_trait` and
#' `target_trait_id` / `target_trait`. When `target_trait_id` is NULL in the
#' config (meaning "same as source"), the returned `effective_target_trait_id`
#' is filled with `source_trait_id` to make downstream filtering trivial.
#'
#' @param con Connection to `plots_transects` (or NULL to use `call.mydb()`).
#' @param include_inactive Logical. If TRUE, also return rules with
#'   `is_active = FALSE`.
#' @return Tibble with one row per rule, joined to trait names.
#' @export
list_trait_aggregations <- function(con = NULL, include_inactive = FALSE) {

  if (is.null(con)) con <- call.mydb()
  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  where <- if (include_inactive) "" else "WHERE c.is_active"

  sql <- paste0("
    SELECT c.id_aggregation,
           c.source_trait_id,
           ts.trait AS source_trait,
           c.target_trait_id,
           tt.trait AS target_trait,
           COALESCE(c.target_trait_id, c.source_trait_id) AS effective_target_trait_id,
           c.method, c.method_param, c.min_n,
           c.is_active, c.notes
    FROM trait_aggregation_config c
    LEFT JOIN traitlist ts ON c.source_trait_id = ts.id_trait
    LEFT JOIN traitlist tt ON c.target_trait_id = tt.id_trait
    ", where, "
    ORDER BY c.id_aggregation
  ")

  tibble::as_tibble(DBI::dbGetQuery(actual_con, sql))
}


#' Add an aggregation rule
#'
#' @param source_trait_id Integer. Trait id whose individual-level rows
#'   (in `data_traits_measures.traitid`) feed the aggregate.
#' @param method Character. One of the supported methods (see
#'   `.compute_aggregate()`).
#' @param target_trait_id Integer or NULL. Trait id stamped on the aggregated
#'   output rows in `taxa_traits_measures.fk_id_trait`. Three usage modes:
#'
#'   * **`NULL` (default) — auto-derive.** Creates (or reuses) a derived
#'     `traitlist` entry named `<source_trait>_<suffix>` (e.g.
#'     `stem_diameter_p95`, `stem_diameter_max`) with the source's
#'     `valuetype`, `expectedunit`, and value bounds copied over. The
#'     derived trait's id is stored as `target_trait_id`. The method is
#'     preserved in the trait name and multiple aggregations of the same
#'     source coexist as distinct columns in `query_taxa_traits()`.
#'   * **`source_trait_id` — keep the original trait name.** The aggregated
#'     row is written under the source trait id, so wide pivots show it as
#'     `taxa_<source_trait>` and there is no name suffix. Trade-offs:
#'     (a) the aggregation method is no longer visible at the trait-id
#'     level — it survives only in `measurementremarks` (e.g.
#'     `"percentile(n=12) param=95"`) and `basisofrecord =
#'     "AggregatedFromIndividual"`; (b) you can only have one such rule per
#'     source trait — two rules pointing to the same id collide in
#'     `(idtax, fk_id_trait)` and the wide pivot averages them; (c) any
#'     directly-measured taxa-level rows for that trait are mixed in.
#'   * **An explicit integer** — use that trait id (e.g. a pre-existing
#'     custom entry in `traitlist`) without auto-creation.
#' @param method_param Numeric or NULL. Required for `"percentile"`.
#' @param min_n Integer. Minimum non-NA values per taxon to emit a row
#'   (default 1).
#' @param notes Character. Free-text description.
#' @param con Connection (admin/write privileges).
#' @return Invisible integer: the new `id_aggregation`.
#' @export
add_trait_aggregation <- function(source_trait_id,
                                  method,
                                  target_trait_id = NULL,
                                  method_param = NULL,
                                  min_n = 1L,
                                  notes = NULL,
                                  con = NULL) {

  if (is.null(con)) con <- call.mydb()
  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  # Sanity-check method by running on a tiny sample
  .compute_aggregate(c(1, 2, 3), method = method, method_param = method_param)

  # Guard: ensure trait_aggregation_config exists with the expected schema
  # before any side effects (e.g. .ensure_derived_trait creating a row in
  # traitlist). dbWriteTable(append = TRUE) will silently auto-create a
  # malformed table from the data.frame columns if the real one was dropped,
  # which then breaks the currval() lookup below — fail clearly here instead.
  .assert_aggregation_config_schema(actual_con)

  # Auto-create derived trait when no explicit target is provided so the
  # aggregation method survives at the trait-id level (see help).
  if (is.null(target_trait_id)) {
    target_trait_id <- .ensure_derived_trait(
      source_trait_id = source_trait_id,
      method          = method,
      method_param    = if (is.null(method_param)) NA_real_ else method_param,
      con             = actual_con
    )
  }

  today <- Sys.Date()

  row <- data.frame(
    source_trait_id = as.integer(source_trait_id),
    target_trait_id = as.integer(target_trait_id),
    method          = method,
    method_param    = if (is.null(method_param)) NA_real_ else as.numeric(method_param),
    min_n           = as.integer(min_n),
    is_active       = TRUE,
    notes           = if (is.null(notes)) NA_character_ else notes,
    date_modif_d    = as.integer(format(today, "%d")),
    date_modif_m    = as.integer(format(today, "%m")),
    date_modif_y    = as.integer(format(today, "%Y")),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(actual_con, "trait_aggregation_config", row,
                    append = TRUE, row.names = FALSE)

  new_id <- DBI::dbGetQuery(actual_con,
    "SELECT currval(pg_get_serial_sequence('trait_aggregation_config',
                                           'id_aggregation')) AS id"
  )$id

  cli::cli_alert_success("Aggregation rule {new_id} added ({method})")
  invisible(as.integer(new_id))
}


#' Deactivate an aggregation rule
#'
#' Soft-delete: sets `is_active = FALSE`. Use `hard = TRUE` to actually delete
#' the row.
#'
#' @param id Integer. `id_aggregation` to deactivate.
#' @param hard If TRUE, DELETE the row instead of flipping `is_active`.
#' @param con Connection.
#' @return Invisible TRUE on success.
#' @export
remove_trait_aggregation <- function(id, hard = FALSE, con = NULL) {

  if (is.null(con)) con <- call.mydb()
  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  if (hard) {
    n <- DBI::dbExecute(actual_con,
      "DELETE FROM trait_aggregation_config WHERE id_aggregation = $1",
      params = list(as.integer(id))
    )
  } else {
    n <- DBI::dbExecute(actual_con,
      "UPDATE trait_aggregation_config SET is_active = FALSE
       WHERE id_aggregation = $1",
      params = list(as.integer(id))
    )
  }

  if (n == 0L) {
    cli::cli_alert_warning("No aggregation rule with id_aggregation = {id}")
  } else {
    cli::cli_alert_success("Rule {id} {if (hard) 'deleted' else 'deactivated'}")
  }
  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# Citation helper
# -----------------------------------------------------------------------------

#' Resolve (or create) the citation row used to tag aggregated outputs
#'
#' Looks up `citation_key` in `table_citations`. If absent, inserts a row with
#' `is_public = FALSE` and the supplied bibliographic fields. Returns the
#' resulting `id_citation`.
#'
#' @param citation_key Character. Stable key (default `"CafriplotsR_aggregated"`).
#' @param dataset_name Character. Used when the row has to be created.
#' @param notes Character. Used when the row has to be created.
#' @param con Connection.
#' @return Integer id_citation.
#' @keywords internal
.ensure_aggregated_citation <- function(citation_key = "CafriplotsR_aggregated",
                                        dataset_name = "CafriplotsR aggregated taxa traits",
                                        notes = "Auto-generated by rebuild_aggregated_taxa_traits()",
                                        con) {

  existing <- DBI::dbGetQuery(con,
    "SELECT id_citation, is_public FROM table_citations WHERE citation_key = $1",
    params = list(citation_key)
  )

  if (nrow(existing) == 1L) {
    if (isTRUE(existing$is_public)) {
      # The whole point of this citation row is to gate aggregated outputs
      # behind the public-read RLS policy. If is_public has drifted to TRUE
      # (e.g. because a rollback dropped the column and a subsequent
      # migration recreated it with the column-default of TRUE), repair it
      # before returning the id, otherwise the next rebuild would tag rows
      # with a citation that the public role can read.
      DBI::dbExecute(con,
        "UPDATE table_citations SET is_public = FALSE WHERE id_citation = $1",
        params = list(as.integer(existing$id_citation))
      )
      cli::cli_alert_warning(
        "Citation {.val {citation_key}} had is_public = TRUE — reset to \\
         FALSE so aggregated rows stay hidden from the public role."
      )
    }
    return(as.integer(existing$id_citation))
  }

  today <- Sys.Date()
  row <- data.frame(
    citation_key = citation_key,
    dataset_name = dataset_name,
    notes        = notes,
    is_public    = FALSE,
    date_modif_d = as.integer(format(today, "%d")),
    date_modif_m = as.integer(format(today, "%m")),
    date_modif_y = as.integer(format(today, "%Y")),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "table_citations", row, append = TRUE, row.names = FALSE)
  new_id <- DBI::dbGetQuery(con,
    "SELECT id_citation FROM table_citations WHERE citation_key = $1",
    params = list(citation_key)
  )$id_citation
  cli::cli_alert_success("Created citation {.val {citation_key}} (id={new_id}, is_public=FALSE)")
  as.integer(new_id)
}


# -----------------------------------------------------------------------------
# Derived trait helpers
# -----------------------------------------------------------------------------

#' Build the suffix appended to a source trait name to form the derived name.
#'
#' Encodes the aggregation method (and parameter where relevant) into a short
#' tag. Example: `"percentile"` with `method_param = 95` -> `"p95"`. All other
#' methods use the method name as the suffix (e.g. `"mean"` -> `"mean"`).
#'
#' @keywords internal
.derived_trait_suffix <- function(method, method_param = NULL) {
  if (method == "percentile") {
    if (is.null(method_param) || !is.finite(method_param)) {
      stop("method_param required for 'percentile'")
    }
    return(paste0("p", as.integer(method_param)))
  }
  method
}


#' Look up (or create) the derived `traitlist` row for an aggregation rule.
#'
#' For a `(source_trait_id, method, method_param)` tuple, returns the
#' `id_trait` of a `traitlist` row whose name is `<source_trait>_<suffix>` (see
#' [`.derived_trait_suffix()`]). Creates the row on the fly if it does not
#' exist, copying `valuetype`, `expectedunit`, `minallowedvalue` and
#' `maxallowedvalue` from the source trait.
#'
#' Note: the lookup is by name only, so if a `traitlist` row with the same
#' computed name already exists for a different reason it will be reused.
#'
#' @keywords internal
.ensure_derived_trait <- function(source_trait_id, method, method_param,
                                  con) {

  source_row <- DBI::dbGetQuery(con,
    "SELECT trait, valuetype, expectedunit, minallowedvalue, maxallowedvalue
       FROM traitlist WHERE id_trait = $1",
    params = list(as.integer(source_trait_id))
  )
  if (nrow(source_row) == 0L) {
    stop("source_trait_id ", source_trait_id, " not found in traitlist")
  }

  param_for_suffix <- if (is.na(method_param)) NULL else method_param
  suffix       <- .derived_trait_suffix(method, param_for_suffix)
  derived_name <- paste0(source_row$trait, "_", suffix)

  existing <- DBI::dbGetQuery(con,
    "SELECT id_trait FROM traitlist WHERE trait = $1",
    params = list(derived_name)
  )
  if (nrow(existing) >= 1L) return(as.integer(existing$id_trait[1]))

  param_text <- if (is.null(param_for_suffix)) "" else paste0("(param=", param_for_suffix, ")")
  description <- paste0("Aggregated from ", source_row$trait,
                        " via ", method, param_text)

  today <- Sys.Date()
  DBI::dbExecute(con,
    "INSERT INTO traitlist
       (trait, valuetype, expectedunit, minallowedvalue, maxallowedvalue,
        traitdescription, date_modif_d, date_modif_m, date_modif_y)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)",
    params = list(
      derived_name,
      source_row$valuetype,
      source_row$expectedunit,
      source_row$minallowedvalue,
      source_row$maxallowedvalue,
      description,
      as.integer(format(today, "%d")),
      as.integer(format(today, "%m")),
      as.integer(format(today, "%Y"))
    )
  )

  new_id <- DBI::dbGetQuery(con,
    "SELECT id_trait FROM traitlist WHERE trait = $1",
    params = list(derived_name)
  )$id_trait

  cli::cli_alert_success(
    "Created derived trait {.val {derived_name}} (id={new_id})"
  )
  as.integer(new_id[1])
}


# -----------------------------------------------------------------------------
# Fetch + aggregate one rule
# -----------------------------------------------------------------------------

#' Restrict raw individual values to taxa identified at the requested ranks.
#'
#' Aggregating to a taxa-level trait only makes sense when the underlying
#' identification is at species or finer rank — a measurement on an
#' individual identified to "Combretum sp." should not feed a species-level
#' aggregate. This filter is applied to the *source* idtax (the one attached
#' to `data_individuals`, before synonym resolution), since that is the rank
#' the field identification was made at.
#'
#' Cross-database: `tax_level` lives in `table_taxa` on the taxa DB, so this
#' issues one extra SELECT against `con_taxa`.
#'
#' @param raw Tibble returned by [`.fetch_individual_values()`].
#' @param con_taxa Connection to the taxa DB.
#' @param allowed_tax_levels Character vector of accepted `tax_level` values.
#' @keywords internal
.filter_to_allowed_tax_levels <- function(raw, con_taxa,
                                          allowed_tax_levels) {
  if (nrow(raw) == 0L) return(raw)
  ids <- unique(raw$idtax)

  levels <- DBI::dbGetQuery(con_taxa, glue::glue_sql(
    "SELECT idtax_n AS idtax, tax_level
       FROM table_taxa
      WHERE idtax_n IN ({ids*})",
    .con = con_taxa))

  keep_ids <- levels$idtax[levels$tax_level %in% allowed_tax_levels]
  raw[raw$idtax %in% keep_ids, , drop = FALSE]
}


#' Fetch individual measurements for a single trait, joined to taxon ids.
#'
#' Returns columns `idtax`, `traitvalue`, `traitvalue_char`, `valuetype`.
#' Rows whose `issue` is non-null are excluded.
#'
#' @keywords internal
.fetch_individual_values <- function(source_trait_id, con) {

  sql <- glue::glue_sql("
    SELECT i.idtax_n      AS idtax,
           tm.traitvalue,
           tm.traitvalue_char,
           tl.valuetype
      FROM data_traits_measures tm
      JOIN data_individuals i  ON tm.id_data_individuals = i.id_n
      JOIN traitlist tl        ON tm.traitid             = tl.id_trait
     WHERE tm.traitid = {source_trait_id}
       AND tm.issue IS NULL
       AND i.idtax_n IS NOT NULL
  ", .con = con)

  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}


#' Insert aggregated rows into `taxa_traits_measures` via plain INSERT.
#'
#' `DBI::dbWriteTable()` falls back to `COPY`, which PostgreSQL refuses on
#' tables protected by row-level security (it raises *"Use INSERT statements
#' instead."*). This helper runs one parametrised INSERT per row instead.
#'
#' @keywords internal
.insert_aggregated_rows <- function(con, rows) {
  if (nrow(rows) == 0L) return(invisible(0L))

  cols <- c("idtax", "fk_id_trait", "traitvalue", "traitvalue_char",
            "basisofrecord", "measurementremarks", "id_citation",
            "date_modif_d", "date_modif_m", "date_modif_y")
  stopifnot(all(cols %in% names(rows)))
  rows <- rows[, cols, drop = FALSE]

  placeholders <- paste0("$", seq_along(cols), collapse = ", ")
  sql <- paste0(
    "INSERT INTO taxa_traits_measures (",
    paste(cols, collapse = ", "), ") VALUES (", placeholders, ")"
  )

  stmt <- DBI::dbSendStatement(con, sql)
  on.exit(DBI::dbClearResult(stmt), add = TRUE)
  DBI::dbBind(stmt, params = unname(as.list(rows)))
  invisible(DBI::dbGetRowsAffected(stmt))
}


#' Aggregate a single rule, return rows ready to insert into taxa_traits_measures
#'
#' @keywords internal
.aggregate_one_rule <- function(rule, citation_id, con, con_taxa,
                                allowed_tax_levels = c("species", "infraspecific")) {

  effective_target <- rule$effective_target_trait_id

  cli::cli_alert_info(
    "Rule {rule$id_aggregation}: source trait {rule$source_trait_id} -> target trait {effective_target} via {rule$method}"
  )

  raw <- .fetch_individual_values(rule$source_trait_id, con = con)
  if (nrow(raw) == 0L) {
    cli::cli_alert_warning("  No individual values found")
    return(NULL)
  }

  # Restrict to source identifications at allowed ranks (default: species or
  # finer). A measurement on an individual identified only to genus must not
  # contribute to a species-level aggregate.
  n_before <- nrow(raw)
  raw <- .filter_to_allowed_tax_levels(raw, con_taxa = con_taxa,
                                       allowed_tax_levels = allowed_tax_levels)
  cli::cli_alert_info(
    "  Kept {nrow(raw)}/{n_before} measurement(s) at tax_level in \\
     {.val {allowed_tax_levels}}"
  )
  if (nrow(raw) == 0L) {
    cli::cli_alert_warning("  No measurements at allowed tax_level")
    return(NULL)
  }

  # Resolve synonyms via the internal backbone (taxa_traits_measures lives in
  # internal-backbone space; WCVP linkage is handled separately downstream).
  mapping <- resolve_taxon_synonyms(
    idtax            = unique(raw$idtax),
    include_synonyms = TRUE,
    con_taxa         = con_taxa
  )
  if (nrow(mapping) == 0L) {
    cli::cli_alert_warning("  No taxa survived synonym resolution")
    return(NULL)
  }

  raw <- merge(raw, mapping, by = "idtax")
  raw$idtax <- raw$idtax_good
  raw$idtax_good <- NULL

  # Pick the column that feeds the aggregate
  is_char_method <- rule$method %in% c("mode", "concat")
  raw$.x <- if (is_char_method) raw$traitvalue_char else raw$traitvalue

  # Compute one row per accepted taxon
  by_tax <- split(raw$.x, raw$idtax)

  computed <- lapply(by_tax, function(v) {
    .compute_aggregate(v, method = rule$method,
                       method_param = if (is.na(rule$method_param)) NULL else rule$method_param)
  })

  ns      <- vapply(computed, function(x) x$n,         integer(1))
  vnums   <- vapply(computed, function(x) x$value_num, numeric(1))
  vchars  <- vapply(computed, function(x) x$value_char, character(1))

  idtax_v <- as.integer(names(by_tax))
  keep    <- ns >= rule$min_n & (!is.na(vnums) | !is.na(vchars))
  if (!any(keep)) {
    cli::cli_alert_warning("  No taxon met min_n = {rule$min_n}")
    return(NULL)
  }

  remarks_param <- if (is.na(rule$method_param)) "" else paste0(" param=", rule$method_param)
  remarks <- paste0(rule$method, "(n=", ns[keep], ")", remarks_param)

  today <- Sys.Date()

  out <- data.frame(
    idtax              = idtax_v[keep],
    fk_id_trait        = as.integer(effective_target),
    traitvalue         = vnums[keep],
    traitvalue_char    = vchars[keep],
    basisofrecord      = "AggregatedFromIndividual",
    measurementremarks = remarks,
    id_citation        = as.integer(citation_id),
    date_modif_d       = as.integer(format(today, "%d")),
    date_modif_m       = as.integer(format(today, "%m")),
    date_modif_y       = as.integer(format(today, "%Y")),
    stringsAsFactors   = FALSE
  )
  cli::cli_alert_success("  {nrow(out)} aggregated row(s) ready")
  out
}


# -----------------------------------------------------------------------------
# Main entry
# -----------------------------------------------------------------------------

#' Rebuild aggregated taxa-level trait rows from individual measurements
#'
#' Reads the active rules in `trait_aggregation_config`, computes per-taxon
#' aggregates from individual-level measurements, and writes them back to
#' `taxa_traits_measures` tagged with the supplied (or default) citation key.
#'
#' Old aggregated rows tied to the same citation **and** to one of the touched
#' output trait ids are deleted in the same transaction before the new rows
#' are inserted, so the table always reflects the latest rebuild.
#'
#' @param con Connection to `plots_transects` (write privileges).
#' @param con_taxa Connection to `rainbio` (for synonym resolution).
#' @param rule_ids Optional integer vector. Restrict the rebuild to these
#'   `id_aggregation` rows; otherwise all `is_active` rules are processed.
#' @param citation_key Stable key under which aggregated rows are tagged.
#'   Created on first use with `is_public = FALSE`.
#' @param allowed_tax_levels Character vector of `tax_level` values
#'   (`"species"`, `"infraspecific"`, `"genus"`, `"family"`, `"order"`,
#'   `"class"`, `"higher"`) for which the source individual identification is
#'   allowed to feed the aggregate. Defaults to `c("species", "infraspecific")`
#'   so that, for example, a measurement on a "Combretum sp." stem does not
#'   contribute to a species-level aggregate.
#' @param execute If FALSE (default), a DRY RUN: rows are computed and shown
#'   but no DELETE/INSERT runs. Set to TRUE to apply.
#' @return Invisible tibble of all rows that were (or would be) inserted.
#' @export
rebuild_aggregated_taxa_traits <- function(con                 = NULL,
                                           con_taxa            = NULL,
                                           rule_ids            = NULL,
                                           citation_key        = "CafriplotsR_aggregated",
                                           allowed_tax_levels  = c("species", "infraspecific"),
                                           execute             = FALSE) {

  if (is.null(con))      con      <- call.mydb()
  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()

  is_pool    <- inherits(con, "Pool")
  actual_con <- if (is_pool) pool::poolCheckout(con) else con
  on.exit({
    if (is_pool && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  cli::cli_h1("Rebuild aggregated taxa-level traits")

  rules <- list_trait_aggregations(con = actual_con, include_inactive = FALSE)
  if (!is.null(rule_ids)) {
    rules <- rules[rules$id_aggregation %in% as.integer(rule_ids), , drop = FALSE]
  }
  if (nrow(rules) == 0L) {
    cli::cli_alert_warning("No active aggregation rules to process.")
    return(invisible(tibble::tibble()))
  }
  cli::cli_alert_info("{nrow(rules)} active rule(s) to process")

  # Backfill any rule whose target_trait_id is NULL (legacy "same as source"
  # behaviour). Auto-derive a method-specific target trait, persist it in the
  # config, and remember the source-trait id so that prior aggregated rows
  # written under the source id are also wiped by the DELETE below.
  legacy_targets <- integer()
  for (i in seq_len(nrow(rules))) {
    if (is.na(rules$target_trait_id[i])) {
      new_target <- .ensure_derived_trait(
        source_trait_id = rules$source_trait_id[i],
        method          = rules$method[i],
        method_param    = rules$method_param[i],
        con             = actual_con
      )
      DBI::dbExecute(actual_con,
        "UPDATE trait_aggregation_config SET target_trait_id = $1
           WHERE id_aggregation = $2",
        params = list(as.integer(new_target),
                      as.integer(rules$id_aggregation[i]))
      )
      cli::cli_alert_info(
        "Rule {rules$id_aggregation[i]}: backfilled target_trait_id -> {new_target}"
      )
      legacy_targets <- c(legacy_targets, as.integer(rules$source_trait_id[i]))
      rules$target_trait_id[i]           <- new_target
      rules$effective_target_trait_id[i] <- new_target
    }
  }

  # Resolve citation up front so we can both tag rows and target the DELETE.
  citation_id <- .ensure_aggregated_citation(citation_key = citation_key,
                                             con = actual_con)

  per_rule_rows <- lapply(seq_len(nrow(rules)), function(i) {
    .aggregate_one_rule(rule = rules[i, , drop = FALSE], citation_id = citation_id,
                        con = actual_con, con_taxa = con_taxa,
                        allowed_tax_levels = allowed_tax_levels)
  })
  per_rule_rows <- per_rule_rows[!vapply(per_rule_rows, is.null, logical(1))]

  if (length(per_rule_rows) == 0L) {
    cli::cli_alert_warning("No rows produced — nothing to write.")
    return(invisible(tibble::tibble()))
  }

  all_rows <- do.call(rbind, per_rule_rows)
  # Include legacy source-trait ids so prior aggregated rows written under the
  # source id (before backfill) are wiped on this rebuild.
  touched_traits <- unique(c(all_rows$fk_id_trait, legacy_targets))

  cli::cli_alert_info(
    "{nrow(all_rows)} row(s) ready across {length(touched_traits)} output trait(s)"
  )

  if (!execute) {
    cli::cli_alert_warning("DRY RUN - rerun with execute = TRUE to apply")
    return(invisible(tibble::as_tibble(all_rows)))
  }

  DBI::dbBegin(actual_con)
  tryCatch({
    n_deleted <- DBI::dbExecute(actual_con, glue::glue_sql("
      DELETE FROM taxa_traits_measures
       WHERE id_citation = {citation_id}
         AND fk_id_trait IN ({touched_traits*})
    ", .con = actual_con))

    cli::cli_alert_info("{n_deleted} stale aggregated row(s) deleted")

    # Use INSERT (not COPY) — taxa_traits_measures is protected by an
    # RLS policy and PostgreSQL refuses COPY against such tables.
    .insert_aggregated_rows(actual_con, all_rows)

    DBI::dbCommit(actual_con)
    cli::cli_alert_success("{nrow(all_rows)} aggregated row(s) written")
  }, error = function(e) {
    tryCatch(DBI::dbRollback(actual_con), error = function(x) NULL)
    cli::cli_alert_danger("Insert failed, rolled back: {e$message}")
    stop(e)
  })

  invisible(tibble::as_tibble(all_rows))
}
