# =============================================================================
# Taxonomic backbones - generic layer
#
# The internal backbone is table_taxa. Every other backbone is a mirror table
# in the taxa database, exposed through a view with the same canonical columns
# (v_backbone_names_<code>), registered in backbone_list and linked to
# table_taxa through taxa_backbone_link. Nothing here names a particular
# backbone: adding one is a migration and an importer, not a code change.
#
# Schema: inst/migrations/multi_backbone.R
# Design: inst/docs/migration_plan_multi_backbone.md
# =============================================================================


# ---- Internal helpers ----------------------------------------------------------

#' Run a read query on the taxa database
#'
#' Checks a connection out of a pool and returns it, so callers can wrap the
#' call in `tryCatch()` without handling the pool themselves.
#'
#' @param con_taxa Connection or pool to the taxa database; `NULL` calls
#'   [call.mydb.taxa()].
#' @param sql Query.
#' @param params Optional list of parameters for `$1`, `$2`, ...
#' @return A data frame.
#' @noRd
.backbone_query <- function(con_taxa, sql, params = NULL) {
  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()
  if (inherits(con_taxa, "Pool")) {
    con <- pool::poolCheckout(con_taxa)
    on.exit(pool::poolReturn(con), add = TRUE)
  } else {
    con <- con_taxa
  }
  if (is.null(params)) {
    DBI::dbGetQuery(con, sql)
  } else {
    DBI::dbGetQuery(con, sql, params = params)
  }
}


#' PostgreSQL array literal from a vector
#'
#' RPostgres binds a vector parameter as one value per row, not as an array, so
#' a set of IDs is passed as a single literal and cast in SQL
#' (`= ANY($1::int[])`). `NA` values are dropped; quotes and backslashes are
#' escaped.
#'
#' @param x Vector.
#' @return A single string such as `{"1","2"}`.
#' @noRd
.pg_array_literal <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (length(x) == 0L) return("{}")
  escaped <- gsub("([\"\\\\])", "\\\\\\1", x)
  paste0("{", paste0("\"", escaped, "\"", collapse = ","), "}")
}


#' Quote a view name taken from backbone_list
#' @noRd
.quote_backbone_view <- function(names_view) {
  as.character(DBI::dbQuoteIdentifier(DBI::ANSI(), names_view))
}


#' Look up one backbone in backbone_list
#'
#' Unlike [.validate_backbone()], a backbone not yet offered to users
#' (`is_name_source = false`) is accepted: it can be matched, linked and
#' checked before it is switched on.
#'
#' @return One-row data frame: `id_backbone`, `code`, `name`, `names_view`,
#'   `is_name_source`.
#' @noRd
.backbone_info <- function(backbone, con_taxa = NULL) {
  info <- tryCatch(
    .backbone_query(
      con_taxa,
      "SELECT id_backbone, code, name, names_view, is_name_source
         FROM backbone_list WHERE code = $1",
      params = list(backbone)
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      cli::cli_abort(c(
        "Could not read {.field backbone_list}: {msg}",
        "i" = "The taxa database needs {.file inst/migrations/multi_backbone.R}."
      ))
    }
  )
  if (nrow(info) != 1L) {
    cli::cli_abort("No backbone with code {.val {backbone}} in {.field backbone_list}.")
  }
  info
}


#' SQL for following synonym pointers through a backbone view
#'
#' Returns the `chain` and `ends` common table expressions. They expect a CTE
#' named `starts` with an `external_id` column, and must follow `WITH
#' RECURSIVE`. `ends` has one row per starting ID: `end_id` (the last name
#' reached), `resolved` (`TRUE` when that name has no pointer, `FALSE` when the
#' chain stopped on a missing ID, a cycle or `max_depth`) and `depth`.
#'
#' A pointer is followed whatever the name's status says: each view decides
#' where names point.
#'
#' @noRd
.backbone_chain_cte <- function(view, max_depth = 5L) {
  sprintf("
  chain AS (
    SELECT v.external_id AS start_id, v.external_id AS cur_id,
           v.accepted_external_id AS next_id, 0 AS depth,
           ARRAY[v.external_id] AS visited
      FROM %1$s v
     WHERE v.external_id IN (SELECT external_id FROM starts)
    UNION ALL
    SELECT c.start_id, v.external_id, v.accepted_external_id, c.depth + 1,
           c.visited || v.external_id
      FROM chain c
      JOIN %1$s v ON v.external_id = c.next_id
     WHERE c.depth < %2$d
       AND NOT (v.external_id = ANY(c.visited))
  ),
  ends AS (
    SELECT DISTINCT ON (start_id) start_id, cur_id AS end_id,
           next_id IS NULL AS resolved, depth
      FROM chain
     ORDER BY start_id, depth DESC
  )", view, as.integer(max_depth))
}


# ---- Available backbones -------------------------------------------------------

#' List the taxonomic backbones available in the taxa database
#'
#' @description
#' The internal backbone (\code{table_taxa}) is always available and is not
#' listed. Other backbones are registered in \code{backbone_list}, each with a
#' mirror of its names exposed through a view.
#'
#' If \code{backbone_list} cannot be read (database not migrated, no
#' permission), an empty tibble is returned and only the internal backbone is
#' usable.
#'
#' @param con_taxa Connection or pool to the taxa database. If \code{NULL},
#'   calls \code{call.mydb.taxa()}.
#' @param name_sources_only Logical. If \code{TRUE} (default), only backbones
#'   offered to users as a source of names (\code{is_name_source}).
#'
#' @return A tibble with columns \code{id_backbone}, \code{code}, \code{name},
#'   \code{publisher}, \code{names_view}, \code{url_template},
#'   \code{is_name_source}.
#'
#' @examples
#' \dontrun{
#' list_backbones()
#' }
#'
#' @export
list_backbones <- function(con_taxa = NULL, name_sources_only = TRUE) {
  empty <- dplyr::tibble(
    id_backbone = integer(), code = character(), name = character(),
    publisher = character(), names_view = character(),
    url_template = character(), is_name_source = logical()
  )
  sql <- paste(
    "SELECT id_backbone, code, name, publisher, names_view, url_template,",
    "is_name_source FROM backbone_list",
    if (name_sources_only) "WHERE is_name_source",
    "ORDER BY code"
  )
  tryCatch(
    dplyr::as_tibble(.backbone_query(con_taxa, sql)),
    error = function(e) {
      message("Note: could not read backbone_list (", conditionMessage(e),
              "). Only the internal backbone is available.")
      empty
    }
  )
}


#' Validate a backbone argument
#'
#' \code{"internal"} is accepted without touching the database. Any other code
#' must be a backbone offered to users (see [list_backbones()]).
#'
#' @param backbone Character scalar.
#' @param con_taxa Connection or pool to the taxa database, used only for a
#'   backbone other than \code{"internal"}.
#' @return \code{backbone}, unchanged.
#' @keywords internal
.validate_backbone <- function(backbone = "internal", con_taxa = NULL) {
  if (!is.character(backbone) || length(backbone) != 1L ||
      is.na(backbone) || !nzchar(backbone)) {
    cli::cli_abort(
      "{.arg backbone} must be a single backbone code, such as {.val internal} or {.val wcvp}."
    )
  }
  if (identical(backbone, "internal")) return(backbone)

  registered <- list_backbones(con_taxa, name_sources_only = FALSE)
  offered <- registered$code[registered$is_name_source %in% TRUE]
  if (!backbone %in% offered) {
    cli::cli_abort(c(
      if (backbone %in% registered$code) {
        "Backbone {.val {backbone}} is registered but not offered to users yet."
      } else {
        "Backbone {.val {backbone}} is unknown or not available."
      },
      "i" = "Available: {.val {c('internal', offered)}}.",
      if (backbone %in% registered$code) {
        c("i" = "It is offered once its names are imported and its links reviewed (see {.fn check_backbone_links}).")
      }
    ))
  }
  backbone
}


# ---- Names -----------------------------------------------------------------------

#' Get a backbone's names for internal taxa
#'
#' @description
#' Looks up, for each internal taxon, the name its **preferred** link points to
#' in another backbone. With \code{resolve_synonyms = TRUE}, synonym pointers
#' are followed until a name without a pointer is reached, whatever the names'
#' status says.
#'
#' A taxon keeps \code{name_source = "internal"} when it has no preferred link
#' or when the linked ID is absent from the backbone (e.g. removed by a newer
#' import). When a chain cannot be completed (missing target, cycle, more than
#' \code{max_depth} steps), the matched name is returned; see
#' [check_backbone_links()].
#'
#' @param idtax_n Integer vector of internal taxon IDs.
#' @param backbone Character. Code of a backbone registered in the taxa
#'   database, e.g. \code{"wcvp"}.
#' @param con_taxa Connection or pool to the taxa database. If \code{NULL},
#'   calls \code{call.mydb.taxa()}.
#' @param resolve_synonyms Logical. Follow synonym pointers to the accepted
#'   name. Default \code{TRUE}.
#' @param max_depth Integer. Maximum number of synonym steps followed.
#'   Default 5.
#'
#' @return A tibble with one row per unique \code{idtax_n}:
#'   \describe{
#'     \item{\code{backbone_name_id}}{ID of the linked name in the backbone.}
#'     \item{\code{backbone_accepted_id}}{ID of the accepted name when the
#'       linked name was followed to it, \code{NA} otherwise. With
#'       \code{resolve_synonyms = FALSE}, the raw pointer.}
#'     \item{\code{backbone_taxon_name}, \code{backbone_family},
#'       \code{backbone_genus}, \code{backbone_species},
#'       \code{backbone_authors}}{Name parts of the returned name.}
#'     \item{\code{backbone_status}}{\code{"accepted"}, \code{"synonym"} or
#'       \code{"other"}.}
#'     \item{\code{backbone_status_raw}}{The backbone's own status value.}
#'     \item{\code{name_source}}{\code{backbone} or \code{"internal"}.}
#'   }
#'
#' @examples
#' \dontrun{
#' get_backbone_names(c(123, 456), "wcvp")
#' }
#'
#' @export
get_backbone_names <- function(idtax_n,
                               backbone,
                               con_taxa = NULL,
                               resolve_synonyms = TRUE,
                               max_depth = 5L) {

  ids <- unique(stats::na.omit(suppressWarnings(as.integer(idtax_n))))
  if (length(ids) == 0L) {
    return(.shape_backbone_names(NULL, integer(0), backbone, resolve_synonyms))
  }

  info <- .backbone_info(backbone, con_taxa)
  view <- .quote_backbone_view(info$names_view)

  sql <- paste0(
    "WITH RECURSIVE starts AS (
       SELECT l.idtax_n, l.external_id
         FROM taxa_backbone_link l
        WHERE l.id_backbone = $1
          AND l.is_preferred
          AND l.idtax_n = ANY($2::int[])
     ),",
    .backbone_chain_cte(view, max_depth), "
     SELECT s.idtax_n, s.external_id,
            m.external_id IS NOT NULL AS in_view,
            m.accepted_external_id AS raw_accepted_id,
            e.end_id,
            COALESCE(e.resolved, false) AS resolved,
            COALESCE(e.depth, 0) AS depth,
            m.taxon_name AS m_taxon_name, m.family AS m_family,
            m.genus AS m_genus, m.species AS m_species,
            m.authors AS m_authors, m.status AS m_status,
            m.status_raw AS m_status_raw,
            x.taxon_name AS x_taxon_name, x.family AS x_family,
            x.genus AS x_genus, x.species AS x_species,
            x.authors AS x_authors, x.status AS x_status,
            x.status_raw AS x_status_raw
       FROM starts s
       LEFT JOIN ", view, " m ON m.external_id = s.external_id
       LEFT JOIN ends e ON e.start_id = s.external_id
       LEFT JOIN ", view, " x ON x.external_id = e.end_id"
  )

  raw <- .backbone_query(
    con_taxa, sql,
    params = list(info$id_backbone, .pg_array_literal(ids))
  )

  .shape_backbone_names(raw, ids, backbone, resolve_synonyms)
}


#' Shape the rows fetched by get_backbone_names()
#'
#' Kept apart from the query so the rules can be tested without a database.
#'
#' @param raw Data frame from the query in [get_backbone_names()], or `NULL`.
#' @param ids Integer vector of requested `idtax_n`.
#' @param backbone Backbone code.
#' @param resolve_synonyms Logical.
#' @return See [get_backbone_names()].
#' @noRd
.shape_backbone_names <- function(raw, ids, backbone, resolve_synonyms = TRUE) {
  value_cols <- c("backbone_name_id", "backbone_accepted_id",
                  "backbone_taxon_name", "backbone_family", "backbone_genus",
                  "backbone_species", "backbone_authors", "backbone_status",
                  "backbone_status_raw")
  out <- dplyr::tibble(idtax_n = as.integer(ids))

  if (is.null(raw) || nrow(raw) == 0L) {
    for (col in value_cols) out[[col]] <- rep(NA_character_, nrow(out))
    out$name_source <- rep("internal", nrow(out))
    return(out)
  }

  in_view <- as.logical(raw$in_view)
  # The accepted name replaces the linked one only when the chain completed
  # and actually moved
  follow <- resolve_synonyms & in_view & as.logical(raw$resolved) & raw$depth > 0
  pick <- function(field) {
    as.character(ifelse(follow, raw[[paste0("x_", field)]], raw[[paste0("m_", field)]]))
  }

  shaped <- dplyr::tibble(
    idtax_n              = as.integer(raw$idtax_n),
    backbone_name_id     = as.character(raw$external_id),
    backbone_accepted_id = if (resolve_synonyms) {
      as.character(ifelse(follow, raw$end_id, NA_character_))
    } else {
      as.character(raw$raw_accepted_id)
    },
    backbone_taxon_name  = pick("taxon_name"),
    backbone_family      = pick("family"),
    backbone_genus       = pick("genus"),
    backbone_species     = pick("species"),
    backbone_authors     = pick("authors"),
    backbone_status      = pick("status"),
    backbone_status_raw  = pick("status_raw"),
    name_source          = ifelse(in_view, backbone, "internal")
  )
  shaped <- shaped[!duplicated(shaped$idtax_n), , drop = FALSE]

  out <- dplyr::left_join(out, shaped, by = "idtax_n")
  out$name_source[is.na(out$name_source)] <- "internal"
  out
}


#' Replace internal name columns with a backbone's names
#'
#' Overwrites \code{tax_fam}, \code{tax_gen}, \code{tax_esp},
#' \code{tax_sp_level}, \code{tax_infra_level} and \code{tax_infra_level_auth}
#' (those present in \code{data}) for rows linked to \code{backbone}. The
#' internal name is kept in \code{alt_taxon_name}. Adds
#' \code{backbone_name_id}, \code{backbone_accepted_id} and \code{name_source};
#' for \code{"wcvp"}, also \code{wcvp_plant_name_id} and
#' \code{wcvp_accepted_plant_name_id}, the column names used before any other
#' backbone existed.
#'
#' @param data Data frame with internal taxonomy columns.
#' @param info Tibble returned by [get_backbone_names()].
#' @param backbone Backbone code.
#' @param id_col Name of the column of \code{data} holding \code{idtax_n}.
#' @return \code{data}, same number of rows.
#' @keywords internal
.apply_backbone <- function(data, info, backbone, id_col = "idtax_n") {

  name_cols <- c("backbone_taxon_name", "backbone_family", "backbone_genus",
                 "backbone_species", "backbone_authors", "backbone_status",
                 "backbone_status_raw")
  id_cols <- c("backbone_name_id", "backbone_accepted_id", "name_source")
  alias_cols <- c("wcvp_plant_name_id", "wcvp_accepted_plant_name_id")

  # Applying twice must not duplicate columns
  data <- dplyr::select(data, -dplyr::any_of(c(name_cols, id_cols, alias_cols)))

  internal_name_col <- if ("tax_infra_level" %in% names(data)) {
    "tax_infra_level"
  } else if ("tax_sp_level" %in% names(data)) {
    "tax_sp_level"
  } else {
    NA_character_
  }
  if (!is.na(internal_name_col)) {
    data$alt_taxon_name <- data[[internal_name_col]]
  }

  info <- info[, intersect(names(info), c("idtax_n", id_cols, name_cols)), drop = FALSE]
  info <- info[!duplicated(info$idtax_n), , drop = FALSE]

  data <- dplyr::left_join(data, info, by = stats::setNames("idtax_n", id_col))
  data$name_source[is.na(data$name_source)] <- "internal"

  linked <- data$name_source == backbone
  replace_where <- function(current, new) {
    ifelse(linked & !is.na(new), new, current)
  }

  if ("tax_fam" %in% names(data)) {
    data$tax_fam <- replace_where(data$tax_fam, data$backbone_family)
  }
  if ("tax_gen" %in% names(data)) {
    data$tax_gen <- replace_where(data$tax_gen, data$backbone_genus)
  }
  if ("tax_esp" %in% names(data)) {
    data$tax_esp <- replace_where(data$tax_esp, data$backbone_species)
  }
  if ("tax_sp_level" %in% names(data)) {
    sp <- ifelse(!is.na(data$backbone_genus) & !is.na(data$backbone_species),
                 paste(data$backbone_genus, data$backbone_species),
                 NA_character_)
    data$tax_sp_level <- replace_where(data$tax_sp_level, sp)
  }
  if ("tax_infra_level" %in% names(data)) {
    data$tax_infra_level <- replace_where(data$tax_infra_level, data$backbone_taxon_name)
  }
  if ("tax_infra_level_auth" %in% names(data)) {
    with_auth <- ifelse(!is.na(data$backbone_authors),
                        paste(data$backbone_taxon_name, data$backbone_authors),
                        data$backbone_taxon_name)
    data$tax_infra_level_auth <- replace_where(data$tax_infra_level_auth, with_auth)
  }

  if (identical(backbone, "wcvp")) {
    data$wcvp_plant_name_id <- suppressWarnings(as.integer(data$backbone_name_id))
    data$wcvp_accepted_plant_name_id <- suppressWarnings(as.integer(data$backbone_accepted_id))
  }

  dplyr::select(data, -dplyr::any_of(name_cols))
}


# ---- Synonym resolution ----------------------------------------------------------

#' Resolve synonyms through a backbone
#'
#' For each internal taxon with a preferred link, follows the backbone's
#' synonym chain and maps the accepted name back to an internal taxon linked to
#' it (an internally accepted one first). Taxa without a link, or whose
#' accepted name is linked to no internal taxon, fall back to the internal
#' backbone.
#'
#' @param idtax Vector of taxon IDs, or `NULL` for all.
#' @param include_synonyms Logical.
#' @param con_taxa Connection to the taxa database.
#' @param backbone Backbone code.
#' @param max_depth Maximum number of synonym steps followed.
#' @return Tibble with columns `idtax`, `idtax_good`.
#' @keywords internal
.resolve_synonyms_backbone <- function(idtax, include_synonyms, con_taxa,
                                       backbone, max_depth = 5L) {

  info <- tryCatch(.backbone_info(backbone, con_taxa), error = function(e) NULL)
  mapping <- data.frame()

  if (!is.null(info)) {
    view <- .quote_backbone_view(info$names_view)
    sql <- paste0(
      "WITH RECURSIVE starts AS (
         SELECT idtax_n, external_id FROM taxa_backbone_link
          WHERE id_backbone = $1 AND is_preferred
       ),",
      .backbone_chain_cte(view, max_depth), ",
       target AS (
         SELECT DISTINCT ON (s.external_id) s.external_id, s.idtax_n
           FROM starts s
           JOIN table_taxa t ON t.idtax_n = s.idtax_n
          ORDER BY s.external_id, (t.idtax_good_n IS NOT NULL), s.idtax_n
       )
       SELECT s.idtax_n,
              CASE WHEN e.resolved AND e.depth = 0 THEN s.idtax_n
                   WHEN e.resolved THEN tg.idtax_n
              END AS idtax_resolved
         FROM starts s
         LEFT JOIN ends e ON e.start_id = s.external_id
         LEFT JOIN target tg ON tg.external_id = e.end_id"
    )
    mapping <- tryCatch(
      .backbone_query(con_taxa, sql, params = list(info$id_backbone)),
      error = function(e) {
        message("Backbone ", backbone, " links not available, falling back to internal backbone: ",
                conditionMessage(e))
        data.frame()
      }
    )
  } else {
    message("Backbone ", backbone, " not registered, falling back to internal backbone.")
  }

  if (nrow(mapping) == 0L) {
    return(resolve_taxon_synonyms(idtax, include_synonyms, con_taxa, backbone = "internal"))
  }

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()
  internal_mapping <- dplyr::tbl(con_taxa, "table_taxa") %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    dplyr::collect() %>%
    dplyr::mutate(idtax_resolved = ifelse(is.na(idtax_good_n), idtax_n, idtax_good_n))

  all_ids <- dplyr::tibble(idtax_n = internal_mapping$idtax_n) %>%
    dplyr::left_join(
      dplyr::tibble(idtax_n = as.integer(mapping$idtax_n),
                    backbone_resolved = as.integer(mapping$idtax_resolved)),
      by = "idtax_n"
    ) %>%
    dplyr::mutate(
      idtax_resolved = dplyr::coalesce(
        backbone_resolved,
        as.integer(internal_mapping$idtax_resolved[match(idtax_n, internal_mapping$idtax_n)])
      )
    ) %>%
    dplyr::select(idtax_n, idtax_resolved)

  if (is.null(idtax)) {
    return(all_ids %>% dplyr::select(idtax = idtax_n, idtax_good = idtax_resolved))
  }

  result <- all_ids %>% dplyr::filter(idtax_n %in% !!idtax)

  if (include_synonyms) {
    resolved_ids <- unique(result$idtax_resolved)
    all_related <- all_ids %>% dplyr::filter(idtax_resolved %in% !!resolved_ids)
    cli::cli_alert_info(
      "{backbone} synonyms: {nrow(result)} taxa expanded to {nrow(all_related)} taxa"
    )
    result <- all_related
  }

  result %>%
    dplyr::select(idtax = idtax_n, idtax_good = idtax_resolved) %>%
    dplyr::distinct()
}


# ---- Status and checks -------------------------------------------------------------

#' Get a backbone's import status
#'
#' @param backbone Character. Backbone code.
#' @param con_taxa Connection or pool to the taxa database. If \code{NULL},
#'   calls \code{call.mydb.taxa()}.
#' @param verbose Logical. Print the status. Default \code{TRUE}.
#'
#' @return Invisibly, a list with \code{version}, \code{import_date},
#'   \code{record_count}, \code{link_count}, \code{imported_by} and
#'   \code{source_version}; \code{NULL} when there is no current import.
#'
#' @examples
#' \dontrun{
#' get_backbone_status("wcvp")
#' }
#'
#' @export
get_backbone_status <- function(backbone, con_taxa = NULL, verbose = TRUE) {

  meta <- tryCatch(
    .backbone_query(
      con_taxa,
      "SELECT b.name, i.version, i.import_date, i.imported_by, i.record_count,
              i.source_version,
              (SELECT count(*)::int FROM taxa_backbone_link l
                WHERE l.id_backbone = b.id_backbone) AS link_count
         FROM backbone_list b
         JOIN backbone_import i ON i.id_backbone = b.id_backbone AND i.is_current
        WHERE b.code = $1",
      params = list(backbone)
    ),
    error = function(e) {
      message("Note: could not read the import status of ", backbone, " (",
              conditionMessage(e), ").")
      data.frame()
    }
  )

  if (nrow(meta) == 0) {
    if (verbose) cli::cli_alert_info("No {backbone} data imported yet")
    return(NULL)
  }

  status <- list(
    version        = meta$version[1],
    import_date    = meta$import_date[1],
    record_count   = meta$record_count[1],
    link_count     = meta$link_count[1],
    imported_by    = meta$imported_by[1],
    source_version = meta$source_version[1]
  )

  if (verbose) {
    cli::cli_h2("{meta$name[1]} import status")
    cli::cli_alert_info("Version: {status$version}")
    cli::cli_alert_info("Imported: {status$import_date}")
    cli::cli_alert_info("Records: {status$record_count}")
    cli::cli_alert_info("Links: {status$link_count}")
    cli::cli_alert_info("Imported by: {status$imported_by}")
  }

  invisible(status)
}


#' Check the links between internal taxa and a backbone
#'
#' @description
#' Read-only. Counts what keeps taxa from getting a backbone's names:
#' \itemize{
#'   \item taxa with several links and none preferred (they keep their
#'     internal name);
#'   \item links to an ID absent from the backbone, e.g. after an import
#'     removed it;
#'   \item preferred links whose synonym chain cannot be completed (missing
#'     target, cycle, more than \code{max_depth} steps);
#'   \item fuzzy links not yet verified.
#' }
#'
#' @param backbone Character. Backbone code.
#' @param con_taxa Connection or pool to the taxa database. If \code{NULL},
#'   calls \code{call.mydb.taxa()}.
#' @param max_depth Integer. Maximum number of synonym steps followed.
#'   Default 5.
#' @param verbose Logical. Print the counts. Default \code{TRUE}.
#'
#' @return Invisibly, a tibble with columns \code{check} and \code{n}.
#'
#' @examples
#' \dontrun{
#' check_backbone_links("wcvp")
#' }
#'
#' @export
check_backbone_links <- function(backbone, con_taxa = NULL, max_depth = 5L,
                                 verbose = TRUE) {

  info <- .backbone_info(backbone, con_taxa)
  view <- .quote_backbone_view(info$names_view)
  id <- list(info$id_backbone)

  counts <- .backbone_query(con_taxa, "
    SELECT count(*)::int AS n_links,
           (count(*) FILTER (WHERE is_preferred))::int AS n_preferred,
           (count(*) FILTER (WHERE match_type IN ('fuzzy', 'author_mismatch') AND NOT verified))::int AS n_fuzzy_unverified
      FROM taxa_backbone_link WHERE id_backbone = $1", params = id)

  n_no_preferred <- .backbone_query(con_taxa, "
    SELECT count(*)::int AS n FROM (
      SELECT idtax_n FROM taxa_backbone_link
       WHERE id_backbone = $1
       GROUP BY idtax_n
      HAVING count(*) > 1 AND NOT bool_or(is_preferred)) h", params = id)$n

  n_dangling <- .backbone_query(con_taxa, paste0("
    SELECT count(*)::int AS n FROM taxa_backbone_link l
     WHERE l.id_backbone = $1
       AND NOT EXISTS (SELECT 1 FROM ", view, " v
                        WHERE v.external_id = l.external_id)"), params = id)$n

  n_unresolved <- .backbone_query(con_taxa, paste0(
    "WITH RECURSIVE starts AS (
       SELECT idtax_n, external_id FROM taxa_backbone_link
        WHERE id_backbone = $1 AND is_preferred
     ),",
    .backbone_chain_cte(view, max_depth), "
     SELECT count(*)::int AS n
       FROM starts s JOIN ends e ON e.start_id = s.external_id
      WHERE NOT e.resolved"), params = id)$n

  res <- dplyr::tibble(
    check = c("links", "preferred links",
              "taxa with several links and none preferred",
              "links to an ID absent from the backbone",
              "preferred links with an unresolved synonym chain",
              "fuzzy or author-mismatch links not verified"),
    n = c(counts$n_links, counts$n_preferred, n_no_preferred, n_dangling,
          n_unresolved, counts$n_fuzzy_unverified)
  )

  if (verbose) {
    cli::cli_h2("{info$name} links")
    for (i in seq_len(nrow(res))) {
      line <- paste0(res$check[i], ": ", res$n[i])
      if (i > 2 && res$n[i] > 0) cli::cli_alert_warning("{line}") else cli::cli_alert_info("{line}")
    }
  }

  invisible(res)
}
