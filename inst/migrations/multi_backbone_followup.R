# ARCHIVED MIGRATION - rewrite_legacy_wcvp_links() applied 2026-09-18, kept for
# the record. report_links_without_preferred() reads only, so it stays usable.
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what is done to the database stays readable.
# It follows up multi_backbone.R (applied 2026-09-15); see
# inst/docs/migration_plan_multi_backbone.md, section 11.
#
# A read-only report, and one repair of the legacy WCVP link table:
#
#   report_links_without_preferred()  which taxa have several links and none
#                                     preferred, sorted by what could settle them
#   rewrite_legacy_wcvp_links()       replaces wcvp_idtax_link with the preferred
#                                     WCVP links of taxa_backbone_link, so that
#                                     package versions still reading the legacy
#                                     table get the corrected names
#
# Why no rule-based choice and no legacy-to-new sync any more (2026-09-17):
# the links in both tables came from a matcher that gave every taxon sharing a
# name the matches chosen for all its homonyms' authors, and marked fuzzy
# matches preferred without review. Choosing "the accepted candidate" would
# pick the wrong one for internal auct. taxa, and copying wcvp_idtax_link into
# taxa_backbone_link would bring the faulty links back. The links are rebuilt
# instead: match_taxa_to_backbone(), review_backbone_matches(),
# replace_backbone_links(); then rewrite_legacy_wcvp_links().
#
# To run (taxa database):
#   source(system.file("migrations", "multi_backbone_followup.R", package = "CafriplotsR"))
#   con_taxa <- CafriplotsR::call.mydb.taxa()
#   report_links_without_preferred(con_taxa, "wcvp")
#   rewrite_legacy_wcvp_links(con_taxa)                   # rehearsal
#   rewrite_legacy_wcvp_links(con_taxa, dry_run = FALSE)  # apply


# ---- Shared ------------------------------------------------------------------

# Connection from a connection or a pool; the caller returns it with on.exit
.followup_con <- function(con_taxa) {
  if (inherits(con_taxa, "Pool")) pool::poolCheckout(con_taxa) else con_taxa
}

.followup_backbone <- function(con, backbone) {
  if (is.na(DBI::dbGetQuery(con, "SELECT to_regclass('public.backbone_list')::text AS r")$r)) {
    cli::cli_abort(c("{.field backbone_list} does not exist.",
                     "i" = "Apply {.file inst/migrations/multi_backbone.R} first."))
  }
  info <- DBI::dbGetQuery(con,
    "SELECT id_backbone, names_view FROM backbone_list WHERE code = $1",
    params = list(backbone))
  if (nrow(info) != 1) {
    cli::cli_abort("No backbone with code {.val {backbone}} in {.field backbone_list}.")
  }
  info
}

# One row per candidate link of every taxon that has several links for the
# backbone and none preferred, with where each candidate's synonym chain ends.
.followup_candidates <- function(con, id_backbone, names_view, max_depth) {
  view <- as.character(DBI::dbQuoteIdentifier(con, names_view))
  sql <- sprintf("
    WITH RECURSIVE cand AS (
      SELECT l.idtax_n, l.external_id, l.match_type, l.match_score
        FROM taxa_backbone_link l
       WHERE l.id_backbone = %1$d
         AND l.idtax_n IN (SELECT idtax_n FROM taxa_backbone_link
                            WHERE id_backbone = %1$d
                            GROUP BY idtax_n
                           HAVING count(*) > 1 AND NOT bool_or(is_preferred))
    ),
    chain AS (
      SELECT v.external_id AS start_id, v.external_id AS cur_id,
             v.accepted_external_id AS next_id, 0 AS depth,
             ARRAY[v.external_id] AS path
        FROM %2$s v
       WHERE v.external_id IN (SELECT external_id FROM cand)
      UNION ALL
      SELECT c.start_id, v.external_id, v.accepted_external_id, c.depth + 1,
             c.path || v.external_id
        FROM chain c
        JOIN %2$s v ON v.external_id = c.next_id
       WHERE c.depth < %3$d
         AND NOT (v.external_id = ANY(c.path))
    ),
    ends AS (
      SELECT DISTINCT ON (start_id) start_id, cur_id AS end_id,
             next_id IS NULL AS resolved
        FROM chain
       ORDER BY start_id, depth DESC
    )
    SELECT c.idtax_n, c.external_id, c.match_type, c.match_score::float8 AS match_score,
           (s.external_id IS NOT NULL AND s.accepted_external_id IS NULL) AS is_end_point,
           e.end_id, COALESCE(e.resolved, false) AS resolved,
           s.taxon_name, s.authors, s.status_raw
      FROM cand c
      LEFT JOIN %2$s s ON s.external_id = c.external_id
      LEFT JOIN ends e ON e.start_id = c.external_id
     ORDER BY c.idtax_n, c.external_id",
    id_backbone, view, max_depth)
  DBI::dbGetQuery(con, sql)
}

# Sort each taxon into the first kind that fits
.followup_classify <- function(cand) {
  if (nrow(cand) == 0) {
    return(data.frame(idtax_n = integer(0), kind = character(0),
                      n_links = integer(0), stringsAsFactors = FALSE))
  }
  by_taxon <- split(cand, cand$idtax_n)
  rows <- lapply(by_taxon, function(d) {
    same_target <- all(d$resolved) && length(unique(d$end_id)) == 1
    n_end <- sum(d$is_end_point)
    kind <- if (same_target) "same_target"
            else if (n_end == 1) "one_accepted"
            else if (length(unique(stats::na.omit(d$taxon_name))) > 1) "different_names"
            else "other"
    data.frame(idtax_n = d$idtax_n[1], kind = kind, n_links = nrow(d),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.followup_kind_labels <- c(
  same_target     = "all links resolve to the same accepted name",
  one_accepted    = "exactly one candidate is an accepted name (no pointer)",
  different_names = "different name strings",
  other           = "same name, several accepted or unresolved"
)


# ---- Report ------------------------------------------------------------------

#' Report taxa with several links to a backbone and none preferred
#'
#' Read-only. Only a preferred link supplies names, so these taxa keep their
#' internal name until one is chosen, in `review_backbone_matches()`. Each
#' taxon is sorted into the first kind that fits:
#'
#' - `same_target`: every link, once its synonym chain is followed, ends on the
#'   same name;
#' - `one_accepted`: exactly one candidate is an accepted name, i.e. has no
#'   pointer in the backbone view. Not necessarily the right one: for an
#'   internal `auct.` taxon it is the wrong one;
#' - `different_names`: the candidates are different name strings;
#' - `other`: the same name, several accepted or unresolved.
#'
#' @param con_taxa Connection (or pool) to the taxa database.
#' @param backbone Backbone code. Default `"wcvp"`.
#' @param max_depth Maximum number of synonym steps followed. Default 5.
#' @return Invisibly, a list: `summary` (kind, n_taxa), `taxa` (one row per
#'   taxon) and `candidates` (one row per link).
#' @keywords internal
report_links_without_preferred <- function(con_taxa, backbone = "wcvp", max_depth = 5L) {

  cli::cli_h1("Taxa with several {backbone} links and none preferred")
  con <- .followup_con(con_taxa)
  on.exit(if (inherits(con_taxa, "Pool")) pool::poolReturn(con), add = TRUE)

  info <- .followup_backbone(con, backbone)
  cand <- .followup_candidates(con, info$id_backbone, info$names_view, as.integer(max_depth))
  taxa <- .followup_classify(cand)

  if (nrow(taxa) == 0) {
    cli::cli_alert_success("None")
    return(invisible(list(summary = data.frame(), taxa = taxa, candidates = cand)))
  }

  summary <- as.data.frame(table(kind = factor(taxa$kind, levels = names(.followup_kind_labels))),
                           responseName = "n_taxa", stringsAsFactors = FALSE)
  summary$meaning <- .followup_kind_labels[summary$kind]
  cli::cli_alert_info("{nrow(taxa)} taxa, {nrow(cand)} links")
  print(summary, row.names = FALSE)

  invisible(list(summary = summary, taxa = taxa, candidates = cand))
}


# ---- Rewrite the legacy link table ------------------------------------------------

#' Replace wcvp_idtax_link with the preferred WCVP links
#'
#' Package versions from before the generic backbone layer read WCVP names
#' from `wcvp_idtax_link`, taking every link of a taxon. This empties it and
#' writes, for each taxon, only its preferred link in `taxa_backbone_link`, so
#' those versions show the same names as the current one. Taxa without a
#' preferred link (several candidates not reviewed, fuzzy matches not
#' verified) get none, and keep their internal names.
#'
#' Run it after `replace_backbone_links(..., "wcvp", dry_run = FALSE)`, and
#' again after later WCVP link changes for as long as a deployed app reads the
#' legacy table.
#'
#' Links a legacy app saves to `wcvp_idtax_link` after this runs are not
#' copied back; they are reported by the next rehearsal as links that would be
#' removed.
#'
#' @param con_taxa Connection (or pool) to the taxa database.
#' @param dry_run If `TRUE` (the default), compare and report without writing.
#' @return Invisibly, a data frame comparing the legacy and preferred link of
#'   each taxon (`idtax_n`, `legacy_ids`, `preferred_id`, `change`).
#' @keywords internal
rewrite_legacy_wcvp_links <- function(con_taxa, dry_run = TRUE) {

  cli::cli_h1("Rewrite wcvp_idtax_link from the preferred WCVP links")
  con <- .followup_con(con_taxa)
  on.exit(if (inherits(con_taxa, "Pool")) pool::poolReturn(con), add = TRUE)
  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  info <- .followup_backbone(con, "wcvp")
  if (is.na(DBI::dbGetQuery(con, "SELECT to_regclass('public.wcvp_idtax_link')::text AS r")$r)) {
    cli::cli_alert_success("wcvp_idtax_link no longer exists; nothing to rewrite.")
    return(invisible(NULL))
  }
  idb <- info$id_backbone

  legacy <- DBI::dbGetQuery(con, "
    SELECT idtax_n, string_agg(plant_name_id::text, ',' ORDER BY plant_name_id) AS legacy_ids,
           count(*)::int AS n_legacy
      FROM wcvp_idtax_link GROUP BY idtax_n")
  preferred <- DBI::dbGetQuery(con, sprintf("
    SELECT l.idtax_n, l.external_id AS preferred_id,
           EXISTS (SELECT 1 FROM wcvp_names w
                    WHERE w.plant_name_id::text = l.external_id) AS in_wcvp_names
      FROM taxa_backbone_link l
     WHERE l.id_backbone = %d AND l.is_preferred", idb))

  missing <- preferred[!preferred$in_wcvp_names, , drop = FALSE]
  preferred <- preferred[preferred$in_wcvp_names, , drop = FALSE]

  ids <- sort(unique(c(legacy$idtax_n, preferred$idtax_n)))
  cmp <- data.frame(
    idtax_n      = ids,
    legacy_ids   = legacy$legacy_ids[match(ids, legacy$idtax_n)],
    n_legacy     = legacy$n_legacy[match(ids, legacy$idtax_n)],
    preferred_id = preferred$preferred_id[match(ids, preferred$idtax_n)],
    stringsAsFactors = FALSE
  )
  cmp$n_legacy[is.na(cmp$n_legacy)] <- 0L
  among_legacy <- mapply(function(id, legacy_ids) {
    !is.na(id) && !is.na(legacy_ids) && id %in% strsplit(legacy_ids, ",", fixed = TRUE)[[1]]
  }, cmp$preferred_id, cmp$legacy_ids, USE.NAMES = FALSE)
  cmp$change <- ifelse(
    is.na(cmp$preferred_id), "removed",
    ifelse(is.na(cmp$legacy_ids), "added",
           ifelse(cmp$n_legacy == 1 & among_legacy, "unchanged",
                  ifelse(among_legacy, "several reduced to the preferred one", "changed"))))

  cli::cli_alert_info("Legacy links: {sum(legacy$n_legacy)} for {nrow(legacy)} taxa")
  cli::cli_alert_info("Preferred WCVP links: {nrow(preferred)}")
  if (nrow(missing) > 0) {
    cli::cli_alert_warning("Preferred links to an ID absent from wcvp_names, not written: {nrow(missing)}")
  }
  print(as.data.frame(table(change = cmp$change), responseName = "n_taxa"), row.names = FALSE)

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed. Re-run with {.code dry_run = FALSE}.")
    return(invisible(cmp))
  }

  DBI::dbBegin(con)
  ok <- tryCatch({
    n_del <- DBI::dbExecute(con, "DELETE FROM wcvp_idtax_link")
    n_ins <- DBI::dbExecute(con, sprintf("
      INSERT INTO wcvp_idtax_link
             (idtax_n, plant_name_id, match_type, match_score, matched_on,
              matched_by, verified, notes)
      SELECT l.idtax_n, w.plant_name_id, l.match_type, l.match_score,
             l.matched_on, l.matched_by, l.verified, l.notes
        FROM taxa_backbone_link l
        JOIN wcvp_names w ON w.plant_name_id::text = l.external_id
       WHERE l.id_backbone = %d AND l.is_preferred", idb))
    DBI::dbExecute(con, sprintf(
      "UPDATE wcvp_import_metadata SET link_count = %d WHERE is_current", n_ins))
    DBI::dbCommit(con)
    c(deleted = n_del, inserted = n_ins)
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Rolled back: {conditionMessage(e)}")
    NULL
  })
  if (is.null(ok)) stop("No change was committed.", call. = FALSE)

  cli::cli_alert_success("wcvp_idtax_link: deleted {ok[['deleted']]}, inserted {ok[['inserted']]}")
  cli::cli_alert_info("Record it in inst/migrations/README.md.")
  invisible(cmp)
}
