# Link taxa that were added without a place in the hierarchy
#
# Until 1.9.8, `.add_taxa_noninteractive()` - the "Add taxon" step of
# launch_taxo_backbone_app() - inserted rows into `table_taxa` with neither
# `tax_level` nor `id_parent`. Checked 2026-09-14: every recent
# `tax_source = 'NEW'` row inspected (idtax_n 367173 to 367191) had both NULL.
#
# Such a taxon is outside the tree. The hierarchy view shows it alone, its
# genus does not list it among its children, and check_hierarchy_consistency()
# cannot report it, because each of its checks filters on `tax_level`.
#
# This fills both columns with the rules the package now applies on insert:
# `.taxon_level()` names the rank from the flat columns, and
# `.find_or_create_parent_entry()` finds the parent, creating it when missing
# (a genus new to the backbone, say). A parent it creates carries
# `tax_source = 'H_AUT'` and is itself linked. (`tax_source` is varchar(5): a
# first version wrote 'AUTO_HIERARCHY', and the rehearsal failed on the first
# parent it had to create, rolling back as designed.)
#
# Taxa database (rainbio), not the main one:
#
#   source(system.file("migrations", "taxa_hierarchy_backfill.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb.taxa()
#   check_unlinked_taxa(con)                          # how many
#   migrate_link_unlinked_taxa(con)                   # rehearsal
#   migrate_link_unlinked_taxa(con, dry_run = FALSE)  # apply
#
# The rehearsal does the real work inside a transaction and rolls it back, so
# what it prints is exactly what applying would do. The one trace it leaves:
# if it had to create parents, the ids they took are not reused.


#' Count taxa outside the hierarchy
#'
#' @param con Connection to the taxa database.
#'
#' @return One-row data frame: `no_level` (rows with `tax_level` NULL),
#'   `no_parent` (rows below the top rank with `id_parent` NULL, whatever their
#'   `tax_level`), and `no_parent_labelled` (the subset that has a `tax_level`,
#'   i.e. not added through the app).
check_unlinked_taxa <- function(con = NULL) {
  if (is.null(con)) con <- CafriplotsR::call.mydb.taxa()

  DBI::dbGetQuery(con, "
    SELECT
      COUNT(*) FILTER (WHERE tax_level IS NULL) AS no_level,
      COUNT(*) FILTER (WHERE id_parent IS NULL
                         AND COALESCE(tax_level, '') NOT IN ('class', 'higher'))
        AS no_parent,
      COUNT(*) FILTER (WHERE id_parent IS NULL
                         AND tax_level IS NOT NULL
                         AND tax_level NOT IN ('class', 'higher'))
        AS no_parent_labelled
    FROM table_taxa
  ")
}


#' Fill tax_level and id_parent for taxa added without them
#'
#' @param con Connection to the taxa database.
#' @param dry_run If `TRUE` (the default), roll everything back at the end.
#' @param include_orphans If `FALSE` (the default), only rows with no
#'   `tax_level` - the ones the app added - are touched. `TRUE` also links rows
#'   that have a `tax_level` but no `id_parent`: taxa the original hierarchy
#'   migration could not link, or parents created by the update module, which
#'   does not link them either. Rehearse before applying: that set was never
#'   inspected.
#'
#' @return Invisibly, one row per taxon considered, with `new_level`,
#'   `new_parent` and `outcome`.
migrate_link_unlinked_taxa <- function(con = NULL, dry_run = TRUE,
                                       include_orphans = FALSE) {
  if (is.null(con)) con <- CafriplotsR::call.mydb.taxa()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool")) pool::poolReturn(actual_con)
  }, add = TRUE)

  CafriplotsR:::.require_taxa_hierarchy(actual_con, "migrate_link_unlinked_taxa")

  cli::cli_h1(paste0("Link taxa missing from the hierarchy",
                     if (dry_run) " (rehearsal)" else ""))

  where <- "tax_level IS NULL"
  if (include_orphans) {
    where <- paste(where, "OR (id_parent IS NULL AND tax_level NOT IN ('class', 'higher'))")
  }

  taxa <- DBI::dbGetQuery(actual_con, paste(
    "SELECT idtax_n, tax_famclass, tax_order, tax_fam, tax_gen, tax_esp,",
    "       tax_nam01, tax_level, id_parent, idtax_good_n, tax_source",
    "FROM table_taxa WHERE", where, "ORDER BY idtax_n"
  ))

  if (nrow(taxa) == 0) {
    cli::cli_alert_success("Every taxon has a rank and a parent. Nothing to do.")
    return(invisible(taxa))
  }

  taxa$new_level <- ifelse(
    is.na(taxa$tax_level),
    CafriplotsR:::.taxon_level(taxa$tax_famclass, taxa$tax_order, taxa$tax_fam,
                               taxa$tax_gen, taxa$tax_esp, taxa$tax_nam01),
    taxa$tax_level
  )
  taxa$new_parent <- NA_integer_
  taxa$outcome <- NA_character_

  cli::cli_alert_info("{nrow(taxa)} taxa to place:")
  print(as.data.frame(table(level = taxa$new_level, tax_source = taxa$tax_source,
                            useNA = "ifany")))

  max_id_before <- DBI::dbGetQuery(actual_con,
                                   "SELECT MAX(idtax_n) AS m FROM table_taxa")$m[1]

  DBI::dbBegin(actual_con)
  finished <- FALSE
  on.exit({
    if (!finished) {
      cli::cli_alert_danger("Stopped part-way: rolling back, nothing changed.")
      try(DBI::dbRollback(actual_con), silent = TRUE)
    }
  }, add = TRUE, after = FALSE)

  # 1. Rank first: a genus that is itself missing its tax_level must be
  # labelled before its species can find it as their parent
  to_label <- taxa[is.na(taxa$tax_level) & !is.na(taxa$new_level), , drop = FALSE]
  for (lvl in unique(to_label$new_level)) {
    ids <- to_label$idtax_n[to_label$new_level == lvl]
    DBI::dbExecute(
      actual_con,
      "UPDATE table_taxa SET tax_level = $1 WHERE idtax_n = ANY($2::int[])",
      params = list(lvl, paste0("{", paste(ids, collapse = ","), "}"))
    )
  }
  cli::cli_alert_success("Set tax_level on {nrow(to_label)} taxa")

  # 2. Then parents, top rank first, so a parent created or linked for one
  # taxon is found by the next
  rank_order <- c("higher", "class", "order", "family", "genus", "species",
                  "infraspecific")
  for (i in order(match(taxa$new_level, rank_order), taxa$idtax_n)) {
    row <- taxa[i, ]

    if (is.na(row$new_level)) {
      taxa$outcome[i] <- "rank unknown: every flat column empty"
      next
    }
    if (!is.na(row$id_parent)) {
      taxa$outcome[i] <- "already linked"
      next
    }
    if (row$new_level %in% c("higher", "class")) {
      taxa$outcome[i] <- "top of the tree"
      next
    }

    parent <- CafriplotsR:::.find_or_create_parent_entry(
      actual_con,
      tax_gen = row$tax_gen, tax_fam = row$tax_fam, tax_order = row$tax_order,
      tax_famclass = row$tax_famclass, tax_esp = row$tax_esp,
      level = row$new_level
    )

    if (is.null(parent)) {
      taxa$outcome[i] <- "not linked: a column naming the parent is empty"
      next
    }

    DBI::dbExecute(actual_con,
                   "UPDATE table_taxa SET id_parent = $1 WHERE idtax_n = $2",
                   params = list(as.integer(parent), as.integer(row$idtax_n)))
    taxa$new_parent[i] <- as.integer(parent)
    taxa$outcome[i] <- "linked"
  }

  created <- DBI::dbGetQuery(actual_con, "
    SELECT idtax_n, tax_level, tax_famclass, tax_order, tax_fam, tax_gen,
           tax_esp, id_parent
    FROM table_taxa
    WHERE tax_source = $1 AND idtax_n > $2
    ORDER BY idtax_n
  ", params = list(CafriplotsR:::.auto_parent_source(), max_id_before))

  cli::cli_h2("Outcome")
  print(as.data.frame(table(outcome = taxa$outcome)))

  if (nrow(created) > 0) {
    cli::cli_alert_warning("{nrow(created)} missing parent entr{?y/ies} created:")
    print(created)
  } else {
    cli::cli_alert_info("No parent entry had to be created.")
  }

  not_linked <- taxa[!taxa$outcome %in% c("linked", "already linked", "top of the tree"), ,
                     drop = FALSE]
  if (nrow(not_linked) > 0) {
    cli::cli_alert_warning("{nrow(not_linked)} taxa left unlinked:")
    print(not_linked[, c("idtax_n", "tax_famclass", "tax_order", "tax_fam",
                         "tax_gen", "tax_esp", "new_level", "outcome")])
  }

  if (dry_run) {
    DBI::dbRollback(actual_con)
    finished <- TRUE
    cli::cli_alert_info("Rehearsal: rolled back, nothing changed. Run again with dry_run = FALSE to apply.")
  } else {
    DBI::dbCommit(actual_con)
    finished <- TRUE
    cli::cli_alert_success("Committed.")
  }

  invisible(taxa)
}
