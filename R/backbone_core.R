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

  available <- list_backbones(con_taxa, name_sources_only = TRUE)
  if (!backbone %in% available$code) {
    cli::cli_abort(c(
      "Backbone {.val {backbone}} is unknown or not available.",
      "i" = "Available: {.val {c('internal', available$code)}}."
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


# ---- Matching and links ------------------------------------------------------------

#' Match internal taxa to a backbone's names
#'
#' @description
#' Matches taxa from the internal \code{table_taxa} to the names of a backbone
#' already imported into the taxa database, using exact and optionally fuzzy
#' matching. Returns a tibble for review; nothing is written. Save reviewed
#' matches with [save_backbone_links()].
#'
#' A name that matches several backbone names (homonyms) returns one row per
#' candidate. Saved as such, none of them is preferred and the taxon keeps its
#' internal name until one is chosen.
#'
#' @param backbone Character. Backbone code, e.g. \code{"wcvp"}. A backbone not
#'   yet offered to users can be matched.
#' @param con_taxa Connection to the taxa database. If \code{NULL}, calls
#'   \code{call.mydb.taxa()}.
#' @param tax_ids Optional integer vector of \code{idtax_n} to match. If
#'   \code{NULL}, matches all taxa except morphospecies, mosses, lichens and
#'   fungi.
#' @param methods Character vector of matching methods. Default
#'   \code{c("exact", "fuzzy")}; fuzzy matching only runs on names without an
#'   exact match.
#' @param fuzzy_threshold Numeric (0-1). Minimum similarity for fuzzy matches.
#'   Default 0.9.
#' @param author_match Character. How author strings are used during exact
#'   matching: \code{"none"} (default) ignores them, \code{"exact"} requires
#'   identical strings, \code{"fuzzy"} keeps the best Jaro-Winkler author match
#'   among homonyms and drops those below \code{author_threshold}. Authors are
#'   taken from \code{author1}/\code{author2}/\code{author3} of
#'   \code{table_taxa}.
#' @param author_threshold Numeric (0-1). Minimum author similarity when
#'   \code{author_match = "fuzzy"}. Default 0.6.
#' @param n_cores Integer. Parallel workers for fuzzy matching. Default 1.
#' @param verbose Logical. Show progress. Default \code{TRUE}.
#'
#' @return A tibble with columns \code{idtax_n}, \code{taxon_name_internal},
#'   \code{external_id}, \code{backbone_taxon_name}, \code{match_type},
#'   \code{match_score}.
#'
#' @examples
#' \dontrun{
#' con_taxa <- call.mydb.taxa()
#' matches <- match_taxa_to_backbone("apd", con_taxa, author_match = "fuzzy")
#' save_backbone_links(matches, "apd", con_taxa)
#' }
#'
#' @export
match_taxa_to_backbone <- function(backbone,
                                   con_taxa = NULL,
                                   tax_ids = NULL,
                                   methods = c("exact", "fuzzy"),
                                   fuzzy_threshold = 0.9,
                                   author_match = c("none", "exact", "fuzzy"),
                                   author_threshold = 0.6,
                                   n_cores = 1L,
                                   verbose = TRUE) {

  author_match <- match.arg(author_match)
  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()

  info <- .backbone_info(backbone, con_taxa)
  view <- .quote_backbone_view(info$names_view)

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }
  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  empty <- dplyr::tibble(
    idtax_n = integer(), taxon_name_internal = character(),
    external_id = character(), backbone_taxon_name = character(),
    match_type = character(), match_score = numeric()
  )

  if (verbose) cli::cli_alert_info("Fetching internal taxa...")

  taxa_query <- dplyr::tbl(actual_con, "table_taxa") %>%
    dplyr::filter(morpho_species == "false",
                  !grepl("Musci-", tax_fam),
                  !grepl("Lichenes", tax_fam),
                  tax_fam != "Fungi")

  if (!is.null(tax_ids)) {
    taxa_query <- taxa_query %>% dplyr::filter(idtax_n %in% !!tax_ids)
  }

  use_authors <- author_match != "none"
  sel_cols <- c("idtax_n", "tax_fam", "tax_gen", "tax_esp",
                "tax_rank01", "tax_nam01", "tax_rank02", "tax_nam02")
  if (use_authors) sel_cols <- c(sel_cols, "author1", "author2", "author3")

  internal_taxa <- taxa_query %>%
    dplyr::select(dplyr::all_of(sel_cols)) %>%
    dplyr::collect()

  if (nrow(internal_taxa) == 0) {
    cli::cli_alert_warning("No taxa to match")
    return(empty)
  }

  # Name strings from the atomic columns; every part only when non-empty
  internal_taxa <- internal_taxa %>%
    dplyr::mutate(
      taxon_name_internal = dplyr::case_when(
        !is.na(tax_nam02) & nzchar(tax_nam02) & !is.na(tax_rank02) & nzchar(tax_rank02) ~
          paste(tax_gen, tax_esp, tax_rank01, tax_nam01, tax_rank02, tax_nam02),
        !is.na(tax_nam01) & nzchar(tax_nam01) & !is.na(tax_rank01) & nzchar(tax_rank01) ~
          paste(tax_gen, tax_esp, tax_rank01, tax_nam01),
        !is.na(tax_esp) & nzchar(tax_esp) ~ paste(tax_gen, tax_esp),
        TRUE ~ tax_gen
      )
    )

  # Authors of the deepest rank present
  if (use_authors) {
    internal_taxa <- internal_taxa %>%
      dplyr::mutate(
        taxon_authors_internal = dplyr::case_when(
          !is.na(tax_nam02) & nzchar(tax_nam02) & !is.na(author3) & nzchar(author3) ~ author3,
          !is.na(tax_nam02) & nzchar(tax_nam02) & !is.na(author2) & nzchar(author2) ~ author2,
          !is.na(tax_nam01) & nzchar(tax_nam01) & !is.na(author2) & nzchar(author2) ~ author2,
          !is.na(tax_esp)   & nzchar(tax_esp)   & !is.na(author1) & nzchar(author1) ~ author1,
          TRUE ~ NA_character_
        )
      )
  }

  if (verbose) cli::cli_alert_info("Fetching {info$name} names from the database...")

  # The matching helpers were written for WCVP and read its column names
  backbone_names <- DBI::dbGetQuery(actual_con, paste0(
    "SELECT external_id AS plant_name_id, taxon_name,
            authors AS taxon_authors, rank AS taxon_rank,
            status_raw AS taxon_status,
            accepted_external_id AS accepted_plant_name_id,
            genus, NULL::text AS ipni_id
       FROM ", view,
    " WHERE taxon_name IS NOT NULL"
  ))

  if (nrow(backbone_names) == 0) {
    cli::cli_abort("No names in backbone {.val {backbone}}. Import them first.")
  }

  if (verbose) cli::cli_alert_info("Matching {nrow(internal_taxa)} taxa against {info$name}...")

  all_matches <- empty

  # Deduplicate names before matching, then re-expand to every idtax_n
  if (use_authors) {
    unique_names <- internal_taxa %>%
      dplyr::distinct(taxon_name_internal, taxon_authors_internal) %>%
      dplyr::mutate(.match_id = dplyr::row_number())
  } else {
    unique_names <- internal_taxa %>%
      dplyr::distinct(taxon_name_internal) %>%
      dplyr::mutate(.match_id = dplyr::row_number())
  }

  if ("exact" %in% methods) {
    if (verbose) {
      author_note <- switch(author_match,
        exact = " (exact author filter)",
        fuzzy = glue::glue(" (fuzzy author, threshold {author_threshold})"),
        ""
      )
      cli::cli_alert_info("Running exact name matching on {nrow(unique_names)} unique names{author_note}...")
    }

    names_df <- data.frame(
      .match_id = unique_names$.match_id,
      name      = unique_names$taxon_name_internal,
      stringsAsFactors = FALSE
    )
    if (use_authors) names_df$author <- unique_names$taxon_authors_internal

    exact_result <- tryCatch({
      if (author_match == "fuzzy") {
        .wcvp_match_fuzzy_author(
          names_df         = names_df,
          wcvp_names       = backbone_names,
          name_col         = "name",
          author_col       = "author",
          id_col           = ".match_id",
          author_threshold = author_threshold
        )
      } else {
        ._wcvp_match_exact_db(
          names_df   = names_df,
          wcvp_names = backbone_names,
          name_col   = "name",
          author_col = if (author_match == "exact") "author" else NULL,
          id_col     = ".match_id"
        )
      }
    }, error = function(e) {
      cli::cli_alert_warning("Exact matching failed: {conditionMessage(e)}")
      NULL
    })

    if (!is.null(exact_result) && nrow(exact_result) > 0) {
      matched_unique <- exact_result %>%
        dplyr::filter(!is.na(wcvp_id)) %>%
        dplyr::transmute(
          taxon_name_internal = name,
          external_id         = as.character(wcvp_id),
          backbone_taxon_name = wcvp_name,
          match_type          = "exact",
          match_score         = as.numeric(match_similarity)
        ) %>%
        dplyr::distinct(taxon_name_internal, external_id, .keep_all = TRUE)

      # many-to-many: several idtax_n can share a name, and homonyms give
      # several external_id per name
      matched <- internal_taxa %>%
        dplyr::select(idtax_n, taxon_name_internal) %>%
        dplyr::inner_join(matched_unique, by = "taxon_name_internal",
                          relationship = "many-to-many")

      all_matches <- dplyr::bind_rows(all_matches, matched)

      if (verbose) cli::cli_alert_success("Exact: {nrow(matched_unique)} unique names matched ({nrow(matched)} taxa total)")
    }
  }

  if ("fuzzy" %in% methods) {
    matched_ids <- unique(all_matches$idtax_n)
    unmatched_taxa <- internal_taxa %>% dplyr::filter(!idtax_n %in% matched_ids)

    unmatched_unique <- unmatched_taxa %>%
      dplyr::distinct(taxon_name_internal) %>%
      dplyr::mutate(.match_id = dplyr::row_number())

    if (nrow(unmatched_unique) > 0) {
      if (verbose) cli::cli_alert_info("Running fuzzy matching on {nrow(unmatched_unique)} unique unmatched names...")

      fuzzy_result <- tryCatch(
        .wcvp_match_fuzzy_fast(
          names_df        = data.frame(name = unmatched_unique$taxon_name_internal,
                                       stringsAsFactors = FALSE),
          wcvp_names      = backbone_names,
          name_col        = "name",
          fuzzy_threshold = fuzzy_threshold,
          n_cores         = n_cores,
          verbose         = verbose
        ),
        error = function(e) {
          cli::cli_alert_warning("Fuzzy matching failed: {conditionMessage(e)}")
          NULL
        }
      )

      if (!is.null(fuzzy_result) && nrow(fuzzy_result) > 0) {
        fuzzy_unique <- fuzzy_result %>%
          dplyr::filter(!is.na(wcvp_id)) %>%
          dplyr::transmute(
            taxon_name_internal = name,
            external_id         = as.character(wcvp_id),
            backbone_taxon_name = wcvp_name,
            match_type          = "fuzzy",
            match_score         = as.numeric(match_similarity)
          ) %>%
          dplyr::filter(match_score >= fuzzy_threshold) %>%
          dplyr::distinct(taxon_name_internal, external_id, .keep_all = TRUE)

        fuzzy_matched <- unmatched_taxa %>%
          dplyr::select(idtax_n, taxon_name_internal) %>%
          dplyr::inner_join(fuzzy_unique, by = "taxon_name_internal",
                            relationship = "many-to-many")

        all_matches <- dplyr::bind_rows(all_matches, fuzzy_matched)

        if (verbose) cli::cli_alert_success("Fuzzy: {nrow(fuzzy_unique)} unique names matched ({nrow(fuzzy_matched)} taxa total, threshold >= {fuzzy_threshold})")
      }
    }
  }

  if (verbose) {
    n_matched <- length(unique(all_matches$idtax_n))
    cli::cli_alert_info("Summary: {n_matched} matched, {nrow(internal_taxa) - n_matched} unmatched out of {nrow(internal_taxa)} taxa")
  }

  all_matches %>%
    dplyr::select(idtax_n, taxon_name_internal, external_id,
                  backbone_taxon_name, match_type, match_score)
}


#' Save links between internal taxa and a backbone
#'
#' @description
#' Writes matches to \code{taxa_backbone_link}. An existing link (same taxon,
#' backbone and external ID) has its match details updated; its
#' \code{verified} and \code{is_preferred} flags are kept.
#'
#' A taxon left with a single link to the backbone gets that link marked
#' preferred. A taxon with several links gets none: it keeps its internal name
#' until one is chosen.
#'
#' @param matches Data frame with \code{idtax_n}, \code{external_id},
#'   \code{match_type} and optionally \code{match_score}, e.g. from
#'   [match_taxa_to_backbone()].
#' @param backbone Character. Backbone code.
#' @param con_taxa Connection or pool to the taxa database, with write access.
#' @param replace Logical. If \code{TRUE} (default), the backbone's existing
#'   links of the taxa in \code{matches} are deleted first.
#' @param verbose Logical. Show progress. Default \code{TRUE}.
#'
#' @return Invisible integer: number of links written.
#'
#' @examples
#' \dontrun{
#' matches <- match_taxa_to_backbone("wcvp", con_taxa)
#' save_backbone_links(matches, "wcvp", con_taxa)
#' }
#'
#' @export
save_backbone_links <- function(matches,
                                backbone,
                                con_taxa = NULL,
                                replace = TRUE,
                                verbose = TRUE) {

  if (nrow(matches) == 0) {
    if (verbose) cli::cli_alert_info("No matches to save")
    return(invisible(0L))
  }

  missing_cols <- setdiff(c("idtax_n", "external_id", "match_type"), names(matches))
  if (length(missing_cols) > 0) {
    cli::cli_abort("{.arg matches} lacks column{?s} {.field {missing_cols}}.")
  }

  link_data <- data.frame(
    idtax_n     = as.integer(matches$idtax_n),
    external_id = as.character(matches$external_id),
    match_type  = as.character(matches$match_type),
    match_score = if ("match_score" %in% names(matches)) as.numeric(matches$match_score) else NA_real_,
    matched_by  = unname(Sys.info()["user"]),
    stringsAsFactors = FALSE
  )
  if (anyNA(link_data$idtax_n) || anyNA(link_data$external_id) || anyNA(link_data$match_type)) {
    cli::cli_abort("{.field idtax_n}, {.field external_id} and {.field match_type} must not be missing.")
  }
  link_data <- link_data[!duplicated(link_data[, c("idtax_n", "external_id")]), , drop = FALSE]

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()
  info <- .backbone_info(backbone, con_taxa)

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }
  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  ids_literal <- .pg_array_literal(unique(link_data$idtax_n))

  DBI::dbBegin(actual_con)
  tryCatch({
    if (replace) {
      DBI::dbExecute(
        actual_con,
        "DELETE FROM taxa_backbone_link WHERE id_backbone = $1 AND idtax_n = ANY($2::int[])",
        params = list(info$id_backbone, ids_literal)
      )
    }

    DBI::dbWriteTable(actual_con, "tmp_backbone_links", link_data,
                      temporary = TRUE, overwrite = TRUE)

    DBI::dbExecute(actual_con, sprintf(
      "INSERT INTO taxa_backbone_link
              (idtax_n, id_backbone, external_id, match_type, match_score, matched_by)
       SELECT idtax_n, %d, external_id, match_type, match_score, matched_by
         FROM tmp_backbone_links
       ON CONFLICT (idtax_n, id_backbone, external_id) DO UPDATE
          SET match_type  = EXCLUDED.match_type,
              match_score = EXCLUDED.match_score,
              matched_by  = EXCLUDED.matched_by,
              matched_on  = CURRENT_TIMESTAMP",
      info$id_backbone
    ))

    DBI::dbExecute(
      actual_con,
      "UPDATE taxa_backbone_link t
          SET is_preferred = true
        WHERE t.id_backbone = $1
          AND t.idtax_n = ANY($2::int[])
          AND NOT t.is_preferred
          AND (SELECT count(*) FROM taxa_backbone_link u
                WHERE u.id_backbone = t.id_backbone
                  AND u.idtax_n = t.idtax_n) = 1",
      params = list(info$id_backbone, ids_literal)
    )

    DBI::dbExecute(actual_con, "DROP TABLE tmp_backbone_links")
    DBI::dbCommit(actual_con)
  }, error = function(e) {
    DBI::dbRollback(actual_con)
    cli::cli_abort("Failed to save {backbone} links: {conditionMessage(e)}")
  })

  if (verbose) {
    cli::cli_alert_success("Saved {nrow(link_data)} {backbone} link{?s} to taxa_backbone_link")
  }
  invisible(nrow(link_data))
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
           (count(*) FILTER (WHERE match_type = 'fuzzy' AND NOT verified))::int AS n_fuzzy_unverified
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
              "fuzzy links not verified"),
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
