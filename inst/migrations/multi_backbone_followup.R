# PENDING MIGRATION - written for later, not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what is done to the database stays readable.
# It follows up multi_backbone.R (applied 2026-09-15); see
# inst/docs/migration_plan_multi_backbone.md, section 11.
#
# Two one-shot repairs, plus a read-only report:
#
#   report_links_without_preferred()  which taxa have several links and none
#                                     preferred, sorted by what could settle them
#   choose_preferred_links()          marks a preferred link where a rule can
#                                     decide it
#   sync_legacy_wcvp_links()          copies links that the pre-Phase-2 package
#                                     saved to wcvp_idtax_link after the
#                                     migration; run it just before Phase 2
#                                     goes live
#
# To run (taxa database):
#   source(system.file("migrations", "multi_backbone_followup.R", package = "CafriplotsR"))
#   con_taxa <- CafriplotsR::call.mydb.taxa()
#   report_links_without_preferred(con_taxa)
#   choose_preferred_links(con_taxa)                   # rehearsal
#   choose_preferred_links(con_taxa, dry_run = FALSE)  # apply
#   sync_legacy_wcvp_links(con_taxa)                   # rehearsal
#   sync_legacy_wcvp_links(con_taxa, dry_run = FALSE)  # apply


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

# Sort each taxon into the first kind that fits, and pick a link where a rule can
.followup_classify <- function(cand) {
  if (nrow(cand) == 0) {
    return(data.frame(idtax_n = integer(0), kind = character(0),
                      chosen = character(0), n_links = integer(0),
                      stringsAsFactors = FALSE))
  }
  by_taxon <- split(cand, cand$idtax_n)
  rows <- lapply(by_taxon, function(d) {
    same_target <- all(d$resolved) && length(unique(d$end_id)) == 1
    n_end <- sum(d$is_end_point)
    kind <- if (same_target) "same_target"
            else if (n_end == 1) "one_accepted"
            else if (length(unique(stats::na.omit(d$taxon_name))) > 1) "different_names"
            else "other"
    chosen <- NA_character_
    if (kind %in% c("same_target", "one_accepted")) {
      # Any link gives the same final name for same_target; prefer the one
      # that is itself the end point, then the best match
      ord <- order(!d$is_end_point,
                   -ifelse(is.na(d$match_score), -Inf, d$match_score),
                   d$match_type != "exact",
                   d$external_id)
      chosen <- if (kind == "one_accepted") d$external_id[d$is_end_point]
                else d$external_id[ord[1]]
    }
    data.frame(idtax_n = d$idtax_n[1], kind = kind, chosen = chosen,
               n_links = nrow(d), stringsAsFactors = FALSE)
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
#' internal name until one is chosen. Each taxon is sorted into the first kind
#' that fits:
#'
#' - `same_target`: every link, once its synonym chain is followed, ends on the
#'   same name — any of them gives the same result;
#' - `one_accepted`: exactly one candidate is an accepted name, i.e. has no
#'   pointer in the backbone view;
#' - `different_names`: the candidates are different name strings (typically a
#'   fuzzy match beside an exact one);
#' - `other`: the same name, several accepted or unresolved.
#'
#' @param con_taxa Connection (or pool) to the taxa database.
#' @param backbone Backbone code. Default `"wcvp"`.
#' @param max_depth Maximum number of synonym steps followed. Default 5.
#' @return Invisibly, a list: `summary` (kind, n_taxa), `taxa` (one row per
#'   taxon, with the link a rule would choose) and `candidates` (one row per
#'   link).
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
  cli::cli_alert_info("{.fn choose_preferred_links} can settle {.val same_target} and {.val one_accepted}.")

  invisible(list(summary = summary, taxa = taxa, candidates = cand))
}


# ---- Choose preferred links ----------------------------------------------------

#' Mark a preferred link where a rule can decide it
#'
#' For taxa with several links to a backbone and none preferred (see
#' [report_links_without_preferred()]), marks one link preferred when the taxon
#' falls under one of `rules`:
#'
#' - `"same_target"`: every link ends on the same name. The chosen link is the
#'   one that is itself that name if there is one, otherwise the best match
#'   (highest score, exact before fuzzy). Names do not depend on the choice.
#' - `"one_accepted"`: the only candidate that is an accepted name. Names
#'   **do** depend on this choice; review the rehearsal output before applying.
#'
#' Chosen links keep `verified = false` and get a note naming the rule. Taxa
#' that gained a preferred link since the report are left alone.
#'
#' @param con_taxa Connection (or pool) to the taxa database.
#' @param backbone Backbone code. Default `"wcvp"`.
#' @param rules Rules to apply, among `"same_target"` and `"one_accepted"`.
#' @param dry_run If `TRUE` (the default), report the choices without writing.
#' @param max_depth Maximum number of synonym steps followed. Default 5.
#' @return Invisibly, a data frame of the choices (`idtax_n`, `kind`, `chosen`).
#' @keywords internal
choose_preferred_links <- function(con_taxa,
                                   backbone = "wcvp",
                                   rules = c("same_target", "one_accepted"),
                                   dry_run = TRUE,
                                   max_depth = 5L) {

  rules <- match.arg(rules, several.ok = TRUE)
  cli::cli_h1("Choose preferred {backbone} links")
  con <- .followup_con(con_taxa)
  on.exit(if (inherits(con_taxa, "Pool")) pool::poolReturn(con), add = TRUE)
  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  info <- .followup_backbone(con, backbone)
  cand <- .followup_candidates(con, info$id_backbone, info$names_view, as.integer(max_depth))
  taxa <- .followup_classify(cand)
  choices <- taxa[taxa$kind %in% rules & !is.na(taxa$chosen), c("idtax_n", "kind", "chosen")]

  cli::cli_alert_info("Taxa without a preferred link: {nrow(taxa)}")
  for (r in rules) {
    cli::cli_alert_info("Rule {.val {r}}: {sum(choices$kind == r)} taxa")
  }
  cli::cli_alert_info("Left for review: {nrow(taxa) - nrow(choices)} taxa")

  if (nrow(choices) > 0) {
    cli::cli_h2("Sample of choices")
    sample_ids <- utils::head(choices$idtax_n, 10)
    show <- cand[cand$idtax_n %in% sample_ids,
                 c("idtax_n", "external_id", "taxon_name", "authors", "status_raw",
                   "match_type", "match_score", "end_id")]
    show$chosen <- show$external_id == choices$chosen[match(show$idtax_n, choices$idtax_n)]
    print(show, row.names = FALSE)
  }

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed. Re-run with {.code dry_run = FALSE}.")
    return(invisible(choices))
  }
  if (nrow(choices) == 0) {
    cli::cli_alert_success("Nothing to do")
    return(invisible(choices))
  }

  note <- sprintf("preferred by rule %%s, %s", format(Sys.Date()))
  DBI::dbBegin(con)
  ok <- tryCatch({
    DBI::dbWriteTable(con, "tmp_preferred_choice",
                      data.frame(idtax_n = as.integer(choices$idtax_n),
                                 external_id = as.character(choices$chosen),
                                 note = sprintf(note, choices$kind),
                                 stringsAsFactors = FALSE),
                      temporary = TRUE, overwrite = TRUE)
    n <- DBI::dbExecute(con, sprintf("
      UPDATE taxa_backbone_link t
         SET is_preferred = true,
             notes = concat_ws('; ', t.notes, c.note)
        FROM tmp_preferred_choice c
       WHERE t.id_backbone = %d
         AND t.idtax_n = c.idtax_n
         AND t.external_id = c.external_id
         AND NOT EXISTS (SELECT 1 FROM taxa_backbone_link p
                          WHERE p.id_backbone = t.id_backbone
                            AND p.idtax_n = t.idtax_n
                            AND p.is_preferred)", info$id_backbone))
    DBI::dbExecute(con, "DROP TABLE tmp_preferred_choice")
    DBI::dbCommit(con)
    n
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Rolled back: {conditionMessage(e)}")
    NA
  })
  if (is.na(ok)) stop("No change was committed.", call. = FALSE)

  cli::cli_alert_success("Preferred links set: {ok}")
  cli::cli_alert_info("Record it in inst/migrations/README.md.")
  invisible(choices)
}


# ---- Sync legacy links ---------------------------------------------------------

#' Copy links saved to wcvp_idtax_link after the migration
#'
#' Until Phase 2 is deployed, the package saves WCVP links to
#' `wcvp_idtax_link` only. This copies the ones missing from
#' `taxa_backbone_link`. A copied link is marked preferred when it is the
#' taxon's only WCVP link.
#'
#' Links present in `taxa_backbone_link` but no longer in `wcvp_idtax_link`
#' are reported. Before Phase 2 is live they can only be links the old package
#' replaced, and `mirror_deletions = TRUE` removes them. **Once Phase 2 is live,
#' leave it `FALSE`**: links saved by the new code exist only in
#' `taxa_backbone_link` and would be deleted.
#'
#' @param con_taxa Connection (or pool) to the taxa database.
#' @param dry_run If `TRUE` (the default), report without writing.
#' @param mirror_deletions Also delete WCVP links absent from
#'   `wcvp_idtax_link`. Default `FALSE`.
#' @return Invisibly `TRUE`.
#' @keywords internal
sync_legacy_wcvp_links <- function(con_taxa, dry_run = TRUE, mirror_deletions = FALSE) {

  cli::cli_h1("Sync WCVP links from wcvp_idtax_link")
  con <- .followup_con(con_taxa)
  on.exit(if (inherits(con_taxa, "Pool")) pool::poolReturn(con), add = TRUE)
  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  info <- .followup_backbone(con, "wcvp")
  if (is.na(DBI::dbGetQuery(con, "SELECT to_regclass('public.wcvp_idtax_link')::text AS r")$r)) {
    cli::cli_alert_success("wcvp_idtax_link no longer exists; nothing to sync.")
    return(invisible(TRUE))
  }
  idb <- info$id_backbone

  to_copy <- DBI::dbGetQuery(con, sprintf("
    SELECT l.idtax_n, l.plant_name_id::text AS external_id, l.match_type,
           l.matched_on, l.matched_by,
           EXISTS (SELECT 1 FROM table_taxa t WHERE t.idtax_n = l.idtax_n) AS taxon_exists
      FROM wcvp_idtax_link l
     WHERE NOT EXISTS (SELECT 1 FROM taxa_backbone_link n
                        WHERE n.id_backbone = %d
                          AND n.idtax_n = l.idtax_n
                          AND n.external_id = l.plant_name_id::text)
     ORDER BY l.matched_on", idb))
  orphans <- to_copy[!to_copy$taxon_exists, , drop = FALSE]
  to_copy <- to_copy[to_copy$taxon_exists, , drop = FALSE]

  gone <- DBI::dbGetQuery(con, sprintf("
    SELECT n.idtax_n, n.external_id, n.is_preferred, n.verified, n.matched_on
      FROM taxa_backbone_link n
     WHERE n.id_backbone = %d
       AND NOT EXISTS (SELECT 1 FROM wcvp_idtax_link l
                        WHERE l.idtax_n = n.idtax_n
                          AND l.plant_name_id::text = n.external_id)
     ORDER BY n.idtax_n", idb))

  cli::cli_alert_info("Links to copy: {nrow(to_copy)}")
  if (nrow(to_copy) > 0) print(utils::head(to_copy[, 1:5], 10), row.names = FALSE)
  if (nrow(orphans) > 0) {
    cli::cli_alert_warning("Skipped, taxon no longer in table_taxa: {nrow(orphans)}")
  }
  cli::cli_alert_info("WCVP links absent from wcvp_idtax_link: {nrow(gone)}")
  if (nrow(gone) > 0) {
    print(utils::head(gone, 10), row.names = FALSE)
    if (mirror_deletions) {
      cli::cli_alert_warning("They will be deleted ({.code mirror_deletions = TRUE}).")
    } else {
      cli::cli_alert_info("Kept ({.code mirror_deletions = FALSE}).")
    }
  }

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed. Re-run with {.code dry_run = FALSE}.")
    return(invisible(TRUE))
  }

  DBI::dbBegin(con)
  ok <- tryCatch({
    n_del <- 0L
    if (mirror_deletions && nrow(gone) > 0) {
      n_del <- DBI::dbExecute(con, sprintf("
        DELETE FROM taxa_backbone_link n
         WHERE n.id_backbone = %d
           AND NOT EXISTS (SELECT 1 FROM wcvp_idtax_link l
                            WHERE l.idtax_n = n.idtax_n
                              AND l.plant_name_id::text = n.external_id)", idb))
    }
    n_ins <- DBI::dbExecute(con, sprintf("
      INSERT INTO taxa_backbone_link
             (idtax_n, id_backbone, external_id, is_preferred, match_type,
              match_score, matched_on, matched_by, verified, notes)
      SELECT l.idtax_n, %1$d, l.plant_name_id::text, false, l.match_type,
             l.match_score, l.matched_on, l.matched_by,
             COALESCE(l.verified, false), l.notes
        FROM wcvp_idtax_link l
        JOIN table_taxa t ON t.idtax_n = l.idtax_n
      ON CONFLICT (idtax_n, id_backbone, external_id) DO NOTHING", idb))
    # A taxon whose only link is not preferred gets it preferred
    n_pref <- DBI::dbExecute(con, sprintf("
      UPDATE taxa_backbone_link t
         SET is_preferred = true
       WHERE t.id_backbone = %1$d
         AND NOT t.is_preferred
         AND (SELECT count(*) FROM taxa_backbone_link u
               WHERE u.id_backbone = t.id_backbone AND u.idtax_n = t.idtax_n) = 1", idb))
    DBI::dbCommit(con)
    c(deleted = n_del, inserted = n_ins, preferred = n_pref)
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Rolled back: {conditionMessage(e)}")
    NULL
  })
  if (is.null(ok)) stop("No change was committed.", call. = FALSE)

  cli::cli_alert_success(
    "Deleted {ok[['deleted']]}, copied {ok[['inserted']]}, newly preferred {ok[['preferred']]}")
  cli::cli_alert_info("Verify with {.fn check_multi_backbone_migration}.")
  invisible(TRUE)
}
