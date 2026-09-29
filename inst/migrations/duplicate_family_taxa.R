# NOT YET APPLIED. Written 2026-09-24.
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what is done to the database stays readable.
#
# The problem (taxa database)
# ---------------------------
# Some family-level names exist several times in `table_taxa`. Fabaceae is the
# worst case seen so far: five rows at family level, one accepted (5769) and
# four synonyms of it (11458, 14046, 16016, 16051), all with the same
# tax_order, tax_famclass and tax_source. They are not different taxa; they are
# the same family entered more than once.
#
# The visible symptom is an appariement that resolves "Fabaceae" to an accepted
# name that is also "Fabaceae", which reads as a bug in the matcher and is not:
# the matcher is reporting the duplication faithfully.
#
# Why this is not a plain DELETE
# ------------------------------
# Things reference those rows, and losing any of them would be worse than the
# duplication. The foreign keys are read from `pg_constraint` at run time
# rather than listed here, because they change: `table_taxa.id_parent` came
# with taxa_hierarchy.R, `taxa_backbone_link.idtax_n` with multi_backbone.R,
# and `table_traits_measures.idtax` was already there. Two of them matter in
# opposite ways:
#
#   table_taxa.id_parent        the hierarchy, ON DELETE NO ACTION. A delete
#                               fails loudly if a genus still hangs off a
#                               duplicate.
#   taxa_backbone_link.idtax_n  WCVP/APD links, ON DELETE CASCADE. A delete
#                               removes them silently. This is the one to fear.
#
# So every reference is repointed at the surviving row first, and only then are
# the duplicates deleted. Nothing is merged across names: rows are grouped by
# the exact same `tax_fam`, so Leguminosae and Fabaceae - a genuine synonymy
# between two different names - are never touched by this.
#
# What it refuses to do
# ---------------------
# A group is skipped, and reported, rather than guessed at, when:
#   - it has no accepted row;
#   - it has several accepted rows and no `tie_break` was asked for;
#   - a duplicate points at an accepted row outside its own group;
#   - the rows disagree on tax_famclass (then they are not obviously the same
#     family and the decision is not mine to make).
#
# To run (taxa database, plus the main one for the reference sweep):
#   source(system.file("migrations", "duplicate_family_taxa.R", package = "CafriplotsR"))
#   con_taxa <- CafriplotsR::call.mydb.taxa()
#   con_main <- CafriplotsR::call.mydb()
#   report_duplicate_family_taxa(con_taxa, con_main)
#   merge_duplicate_family_taxa(con_taxa, con_main, family = "Fabaceae")
#   merge_duplicate_family_taxa(con_taxa, con_main, family = "Fabaceae", dry_run = FALSE)
#   check_duplicate_family_taxa(con_taxa)
#
# Take a dump of `table_taxa`, `taxa_backbone_link`, `table_traits_measures`
# and the main database's idtax columns before the first run with
# dry_run = FALSE. The repointing is reversible only from a backup.
#
# What the main database's sweep touches, and why
# -----------------------------------------------
# `specimens` and `data_individuals` are live: the package reads and writes
# both, so they are repointed.
#
# `rainbio_records` (1,415 rows on the Fabaceae duplicates) and
# `followup_updates_rainbio_records` (308) are repointed too. No R file
# mentions either, and `git log -S"rainbio_records"` finds nothing across the
# 1,316 commits - the table lives in the database alone, created outside the
# repository. That absence is not evidence of a dead table: Gilles confirmed on
# 2026-09-24 that it is recent and part of work in progress, and that its
# `idtax_n` is to be repointed like the rest. Do not infer "unused" from
# "unreferenced" here.
#
# The follow-up table is an audit trail, so repointing it does rewrite recorded
# history: a row saying "changed to 16016" will say "changed to 5769". That is
# the lesser evil - the two ids are the same family, and the alternative is a
# history pointing at rows that no longer exist. Pass
# `never_repoint = "followup_updates_rainbio_records"` to keep the trail
# verbatim and accept the dangling ids.


# ---- Shared ------------------------------------------------------------------

# Connection from a connection or a pool; the caller returns it with on.exit
.dupfam_con <- function(con) {
  if (inherits(con, "Pool")) pool::poolCheckout(con) else con
}

# Integer ids as a SQL list. Values come from the database and are coerced
# again here, so nothing but integers ever reaches a statement.
.dupfam_ids <- function(x) paste(as.integer(x), collapse = ", ")

# Every foreign key pointing at table_taxa(idtax_n). Read at run time: a
# hardcoded list would be wrong the next time a table is added, and being
# wrong here means deleting something quietly.
.dupfam_fks <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT c.conrelid::regclass::text AS tbl, a.attname AS col
    FROM pg_constraint c
    JOIN unnest(c.conkey) WITH ORDINALITY AS k(attnum, ord) ON true
    JOIN pg_attribute a ON a.attrelid = c.conrelid AND a.attnum = k.attnum
    WHERE c.contype = 'f' AND c.confrelid = 'table_taxa'::regclass
    ORDER BY 1, 2")
}

# Duplicate family groups, one row per table_taxa row involved.
.dupfam_groups <- function(con, family = NULL) {
  sql <- "
    WITH fam AS (
      SELECT idtax_n, idtax_good_n, tax_fam, tax_famclass, tax_order,
             tax_source, id_parent
      FROM table_taxa
      WHERE tax_level = 'family' AND tax_fam IS NOT NULL
    )
    SELECT * FROM fam
    WHERE tax_fam IN (SELECT tax_fam FROM fam GROUP BY tax_fam HAVING count(*) > 1)
    ORDER BY tax_fam, (idtax_good_n IS NOT NULL), idtax_n"
  out <- DBI::dbGetQuery(con, sql)
  if (!is.null(family) && nrow(out) > 0) {
    out <- out[tolower(out$tax_fam) %in% tolower(family), , drop = FALSE]
  }
  out
}

# How many rows point at each of these ids, across both databases. Used to
# settle a group with several accepted rows, when the caller asks for that.
#
# The main database counts as much as the taxa one. A row can be a pure orphan
# on con_taxa and still be the id hundreds of individuals and specimens were
# recorded under - and on this installation the old low ids are usually the
# ones the main database knows, while the 35xxxx ones carry the hierarchy.
# Weighing only the taxa side would elect the id nobody records under and
# rewrite every main-database reference for nothing.
#
# Counts per id in one query per column rather than one per id: a family with
# five candidates would otherwise mean five passes over rainbio_records.
# `main_cols` is the repointable column list, computed once by the caller.
.dupfam_weight <- function(con, ids, fks, cmain = NULL, main_cols = NULL) {
  ids <- as.integer(ids)
  n   <- stats::setNames(integer(length(ids)), as.character(ids))
  if (length(ids) == 0) return(n)
  idl <- paste(ids, collapse = ", ")

  tally <- function(cn, tbl, col) {
    q <- DBI::dbGetQuery(cn, sprintf(
      "SELECT %s AS id, count(*) AS n FROM %s WHERE %s IN (%s) GROUP BY 1",
      DBI::dbQuoteIdentifier(cn, col), DBI::dbQuoteIdentifier(cn, tbl),
      DBI::dbQuoteIdentifier(cn, col), idl))
    if (nrow(q) == 0) return(invisible(NULL))
    hit <- as.character(q$id)
    n[hit] <<- n[hit] + as.integer(q$n)
    invisible(NULL)
  }

  tally(con, "table_taxa", "idtax_good_n")
  for (i in seq_len(nrow(fks))) tally(con, fks$tbl[i], fks$col[i])

  if (!is.null(cmain) && !is.null(main_cols) && nrow(main_cols) > 0) {
    for (i in seq_len(nrow(main_cols))) {
      tally(cmain, main_cols$table_name[i], main_cols$column_name[i])
    }
  }
  n
}

# The weighing function handed to .dupfam_plan_one. Only the columns the sweep
# would actually repoint are weighed: a hit in table_idtax_backup is not a
# reason to keep an id, since the merge will not touch that table anyway.
.dupfam_weigher <- function(con, fks, cmain = NULL, main_cols = NULL) {
  repointable <- if (is.null(main_cols)) NULL else
    main_cols[main_cols$why == "", , drop = FALSE]
  function(ids) .dupfam_weight(con, ids, fks, cmain, repointable)
}

# Split one group into keeper and duplicates, or say why it cannot be split.
#
# tie_break decides what to do with a group holding several accepted rows:
#   "none"            skip it, the default
#   "most_referenced" keep the one most rows point at, lowest id on a tie
#   "lowest_id"       keep the lowest id
# The losing accepted rows become duplicates like any other: same name, same
# class, same order, so keeping either is a naming choice, not a taxonomic one.
.dupfam_plan_one <- function(rows, tie_break = "none", weigh = NULL) {
  fam      <- rows$tax_fam[1]
  accepted <- rows[is.na(rows$idtax_good_n), , drop = FALSE]

  if (nrow(accepted) == 0) {
    return(list(family = fam, skip = "no accepted row in the group"))
  }
  classes <- unique(rows$tax_famclass[!is.na(rows$tax_famclass)])
  if (length(classes) > 1) {
    # Say which classes, not just that they differ: one of them is usually a
    # miskeyed row, and the reader can only see that if the values are shown.
    detail <- paste(vapply(classes, function(cl) sprintf(
      "%s: %s", cl,
      .dupfam_ids(rows$idtax_n[!is.na(rows$tax_famclass) &
                               rows$tax_famclass == cl])),
      character(1)), collapse = " | ")
    return(list(family = fam,
                skip = sprintf("rows disagree on tax_famclass - %s", detail)))
  }

  if (nrow(accepted) == 1) {
    keeper <- accepted$idtax_n[1]
    how    <- "only accepted row"
  } else if (identical(tie_break, "none")) {
    # Show the weights when they can be computed: the choice is far easier to
    # make with "3 references against 41" than with two bare ids.
    w <- if (!is.null(weigh)) {
      sprintf(" [references, both databases: %s]", paste(sprintf(
        "%d:%d", accepted$idtax_n, weigh(accepted$idtax_n)), collapse = ", "))
    } else ""
    return(list(family = fam, skip = sprintf(
      "%d accepted rows (%s)%s - pass tie_break to settle it",
      nrow(accepted), .dupfam_ids(accepted$idtax_n), w)))
  } else if (identical(tie_break, "lowest_id")) {
    keeper <- min(accepted$idtax_n)
    how    <- "lowest id among the accepted rows"
  } else if (identical(tie_break, "most_referenced")) {
    if (is.null(weigh)) {
      return(list(family = fam, skip = "most_referenced needs a connection"))
    }
    w      <- weigh(accepted$idtax_n)
    keeper <- accepted$idtax_n[order(-w, accepted$idtax_n)][1]
    how    <- sprintf("most referenced accepted row (%s)",
                      paste(sprintf("%d:%d", accepted$idtax_n, w), collapse = ", "))
  } else {
    return(list(family = fam, skip = sprintf("unknown tie_break %s", tie_break)))
  }

  dupes <- rows[rows$idtax_n != keeper, , drop = FALSE]

  # A synonym in the group may point at any row of the group, including an
  # accepted one that is about to be merged away. Pointing outside the group
  # means something else is going on, and that is not mine to resolve.
  astray <- dupes$idtax_n[!is.na(dupes$idtax_good_n) &
                          !(dupes$idtax_good_n %in% rows$idtax_n)]
  if (length(astray) > 0) {
    return(list(family = fam, skip = sprintf(
      "%s point(s) at an accepted row outside the group", .dupfam_ids(astray))))
  }

  list(family = fam, keeper = keeper, dupes = dupes$idtax_n, how = how,
       skip = NULL)
}

# What points at these ids, in the taxa database: one row per foreign key,
# plus the synonymy column, which is not declared as one.
.dupfam_refs_taxa <- function(con, dupes, fks) {
  ids <- .dupfam_ids(dupes)
  out <- data.frame(what = "table_taxa.idtax_good_n",
                    n = as.integer(DBI::dbGetQuery(con, sprintf(
                      "SELECT count(*) AS n FROM table_taxa
                       WHERE idtax_good_n IN (%s)", ids))$n[1]),
                    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(fks))) {
    n <- as.integer(DBI::dbGetQuery(con, sprintf(
      "SELECT count(*) AS n FROM %s WHERE %s IN (%s)",
      DBI::dbQuoteIdentifier(con, fks$tbl[i]),
      DBI::dbQuoteIdentifier(con, fks$col[i]), ids))$n[1])
    out <- rbind(out, data.frame(
      what = paste(fks$tbl[i], fks$col[i], sep = "."), n = n,
      stringsAsFactors = FALSE))
  }
  out
}

# Columns of the main database that hold a taxon id. No foreign key crosses the
# two databases, so they are found by name, over base tables only - which also
# keeps the table_idtax materialized view out of the way.
.dupfam_main_columns <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT c.table_name, c.column_name
    FROM information_schema.columns c
    JOIN information_schema.tables t
      ON t.table_schema = c.table_schema AND t.table_name = c.table_name
    WHERE c.table_schema = 'public'
      AND t.table_type = 'BASE TABLE'
      AND c.column_name ~ '^idtax'
    ORDER BY c.table_name, c.column_name")
}

# Is this column part of a primary or unique key? Such a column is the row's
# identity, not a reference to somebody else's. Repointing it would either fail
# on the constraint or, on a table that lost its constraint along the way,
# quietly create duplicate rows. table_taxa.idtax_n is exactly this case.
.dupfam_is_key_column <- function(con, tbl, col) {
  n <- DBI::dbGetQuery(con, sprintf("
    SELECT count(*) AS n
    FROM pg_constraint c
    JOIN unnest(c.conkey) WITH ORDINALITY AS u(attnum, ord) ON true
    JOIN pg_attribute a ON a.attrelid = c.conrelid AND a.attnum = u.attnum
    WHERE c.contype IN ('p', 'u') AND c.conrelid = %s::regclass
      AND a.attname = %s",
    DBI::dbQuoteString(con, tbl), DBI::dbQuoteString(con, col)))$n[1]
  n > 0
}

# Tables the name sweep must never write to.
#
# What each exclusion is, in the database as it stands on 2026-09-24:
#   table_taxa          on the main database, a stale copy no longer used. The
#                       live taxonomy is the one on con_taxa. Confirmed by the
#                       maintainer; do not "fix" this by removing the exclusion.
#   table_idtax         the materialized view, rebuilt from table_taxa.
#   table_idtax_backup  a backup. One that follows the live data is not a backup.
#   table_idtax_temp    the staging table that taxonomic_update_functions.R
#                       fills when taxa are added. Transient by design, and
#                       writing into someone else's staging area mid-flight is
#                       not this migration's business.
#
# `never` adds to the list without editing this file.
.dupfam_main_offlimits <- function(tbl, never = character()) {
  grepl("(_backup|_bak|_temp|_tmp|_old|_copy|_archive)$", tbl) ||
    tbl %in% c("table_taxa", "table_idtax", "table_traits_measures",
               "table_tax_famclass", "table_traits", never)
}

# Are these two connections looking at the same database? If they are, the
# name sweep would go over the tables the foreign keys already cover.
.dupfam_same_db <- function(a, b) {
  q <- "SELECT current_database() AS db,
               coalesce(inet_server_addr()::text, 'local') AS host,
               coalesce(inet_server_port()::text, '')      AS port"
  ra <- DBI::dbGetQuery(a, q)
  rb <- DBI::dbGetQuery(b, q)
  identical(as.list(ra), as.list(rb))
}

# Should the name sweep run at all? Not without a second connection, and not
# when that connection turns out to be the same database as the first.
.dupfam_sweep_main <- function(con, cmain) {
  if (is.null(cmain)) return(FALSE)
  if (.dupfam_same_db(con, cmain)) {
    cli::cli_alert_warning(
      "{.code con_main} and {.code con_taxa} are the same database. The name \\
       sweep is skipped: the foreign keys already cover every table here.")
    return(FALSE)
  }
  TRUE
}

# Rows of the main database pointing at these ids, per column. Returns what may
# be repointed and what was deliberately left alone, so the caller can print
# both: a silent exclusion is how a migration loses data without anyone noticing.
.dupfam_refs_main <- function(con, dupes, never = character(), cols = NULL) {
  empty <- data.frame(table_name = character(0), column_name = character(0),
                      n = integer(0), why = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(cols)) cols <- .dupfam_main_classified(con, never)
  if (nrow(cols) == 0) return(list(repoint = empty, left = empty))

  ids  <- .dupfam_ids(dupes)
  cols$n <- vapply(seq_len(nrow(cols)), function(i) {
    as.integer(DBI::dbGetQuery(con, sprintf(
      "SELECT count(*) AS n FROM %s WHERE %s IN (%s)",
      DBI::dbQuoteIdentifier(con, cols$table_name[i]),
      DBI::dbQuoteIdentifier(con, cols$column_name[i]), ids))$n[1])
  }, integer(1))
  cols <- cols[cols$n > 0, , drop = FALSE]
  if (nrow(cols) == 0) return(list(repoint = empty, left = empty))

  list(repoint = cols[cols$why == "", , drop = FALSE],
       left    = cols[cols$why != "", , drop = FALSE])
}

# Every idtax column of the main database, each marked with the reason it is
# off limits, or "" when it may be repointed. Computed once per run: the
# classification asks pg_constraint about every column and does not change
# between families.
.dupfam_main_classified <- function(con, never = character()) {
  cols <- .dupfam_main_columns(con)
  if (nrow(cols) == 0) return(cols)
  cols$why <- vapply(seq_len(nrow(cols)), function(i) {
    if (.dupfam_main_offlimits(cols$table_name[i], never)) {
      "off limits to the name sweep"
    } else if (.dupfam_is_key_column(con, cols$table_name[i], cols$column_name[i])) {
      "part of a primary or unique key"
    } else {
      ""
    }
  }, character(1))
  cols
}


# ---- Read-only report --------------------------------------------------------

#' What duplicate family entries exist, and what points at them
#'
#' Reads only. Run it before and after the merge.
report_duplicate_family_taxa <- function(con_taxa, con_main = NULL,
                                         family = NULL, tie_break = "none",
                                         never_repoint = character()) {
  con <- .dupfam_con(con_taxa)
  if (inherits(con_taxa, "Pool")) on.exit(pool::poolReturn(con), add = TRUE)

  groups <- .dupfam_groups(con, family)
  if (nrow(groups) == 0) {
    cli::cli_alert_success("No duplicated family-level name.")
    return(invisible(NULL))
  }

  fks <- .dupfam_fks(con)
  cli::cli_alert_info("Foreign keys to be repointed: \\
                      {.field {paste(fks$tbl, fks$col, sep = '.')}}")

  cmain <- NULL
  if (!is.null(con_main)) {
    cmain <- .dupfam_con(con_main)
    if (inherits(con_main, "Pool")) on.exit(pool::poolReturn(cmain), add = TRUE)
  }
  sweep_main <- .dupfam_sweep_main(con, cmain)
  main_cols  <- if (sweep_main) .dupfam_main_classified(cmain, never_repoint) else NULL
  weigh      <- .dupfam_weigher(con, fks, if (sweep_main) cmain else NULL, main_cols)

  cli::cli_h1("Duplicated family-level names")
  out <- list()

  for (fam in unique(groups$tax_fam)) {
    rows <- groups[groups$tax_fam == fam, , drop = FALSE]
    plan <- .dupfam_plan_one(rows, tie_break, weigh)

    cli::cli_h2("{fam} - {nrow(rows)} rows")

    if (!is.null(plan$skip)) {
      all_ids <- .dupfam_ids(rows$idtax_n)
      cli::cli_alert_danger("Skipped: {plan$skip}")
      cli::cli_alert_info("ids: {all_ids}")
      out[[fam]] <- plan
      next
    }

    dupe_ids <- .dupfam_ids(plan$dupes)
    cli::cli_alert_info("keeper {plan$keeper} ({plan$how}); \\
                        duplicates {dupe_ids}")

    refs <- .dupfam_refs_taxa(con, plan$dupes, fks)
    for (i in seq_len(nrow(refs))) {
      if (refs$n[i] > 0) {
        cli::cli_alert_warning("{refs$n[i]} row{?s} in {.field {refs$what[i]}}")
      }
    }
    if (all(refs$n == 0)) cli::cli_alert_info("taxa database: nothing points at a duplicate")

    if (sweep_main) {
      mrefs <- .dupfam_refs_main(cmain, plan$dupes, never_repoint, main_cols)
      if (nrow(mrefs$repoint) == 0 && nrow(mrefs$left) == 0) {
        cli::cli_alert_info("main database: nothing points at a duplicate")
      }
      for (i in seq_len(nrow(mrefs$repoint))) {
        cli::cli_alert_warning("main database: {mrefs$repoint$n[i]} row{?s} in \\
                               {.field {mrefs$repoint$table_name[i]}}.\\
                               {.field {mrefs$repoint$column_name[i]}}")
      }
      for (i in seq_len(nrow(mrefs$left))) {
        why <- mrefs$left$why[i]
        cli::cli_alert_info("main database, left alone: {mrefs$left$n[i]} row{?s} \\
                            in {.field {mrefs$left$table_name[i]}}.\\
                            {.field {mrefs$left$column_name[i]}} - {why}")
      }
      plan$main <- mrefs
    }

    plan$refs <- refs
    out[[fam]] <- plan
  }

  n_skip <- sum(vapply(out, function(p) !is.null(p$skip), logical(1)))
  cli::cli_h2("Summary")
  cli::cli_alert_info("{length(out)} duplicated famil{?y/ies}, \\
                      {length(out) - n_skip} mergeable, {n_skip} skipped")
  if (n_skip > 0 && identical(tie_break, "none")) {
    cli::cli_alert_info("Most skips are groups with several accepted rows. \\
                        Re-run with {.code tie_break = \"most_referenced\"} to see \\
                        which row would survive.")
    if (!sweep_main) {
      cli::cli_alert_warning("Those counts cover the taxa database only. Pass \\
                             {.code con_main} so an id the main database records \\
                             under is not weighed as an orphan.")
    }
  }

  invisible(out)
}


# ---- The merge ---------------------------------------------------------------

#' Repoint everything at the surviving row, then delete the duplicates
#'
#' @param con_taxa Taxa database connection or pool.
#' @param con_main Main database connection or pool. Required unless
#'   `skip_main = TRUE`: with no main connection there is no way to know
#'   whether an individual or a trait measure points at a duplicate.
#' @param family Character vector of family names, or NULL for every duplicated
#'   family. Start with one.
#' @param dry_run TRUE (the default) prints what it would do and changes
#'   nothing.
#' @param tie_break How to settle a group with several accepted rows: "none"
#'   (skip it), "most_referenced" or "lowest_id". "most_referenced" counts the
#'   rows pointing at each candidate across both databases, and so needs
#'   `con_main` to see the whole picture: without it the count covers the taxa
#'   database alone and may elect an id nothing was ever recorded under.
#' @param skip_main Proceed without checking the main database. Only for a
#'   restored copy that has no main database beside it.
merge_duplicate_family_taxa <- function(con_taxa, con_main = NULL,
                                        family = NULL, dry_run = TRUE,
                                        tie_break = "none", skip_main = FALSE,
                                        never_repoint = character()) {

  if (is.null(con_main) && !isTRUE(skip_main)) {
    cli::cli_abort(c(
      "No main database connection.",
      "i" = "Individuals and trait measures carry {.field idtax} columns too; \\
             deleting a taxon they point at would leave them dangling.",
      "i" = "Pass {.code con_main}, or {.code skip_main = TRUE} if there is \\
             genuinely no main database."))
  }

  con <- .dupfam_con(con_taxa)
  if (inherits(con_taxa, "Pool")) on.exit(pool::poolReturn(con), add = TRUE)

  cmain <- NULL
  if (!is.null(con_main)) {
    cmain <- .dupfam_con(con_main)
    if (inherits(con_main, "Pool")) on.exit(pool::poolReturn(cmain), add = TRUE)
  }

  sweep_main <- .dupfam_sweep_main(con, cmain)

  fks    <- .dupfam_fks(con)
  groups <- .dupfam_groups(con, family)
  if (nrow(groups) == 0) {
    cli::cli_alert_success("Nothing to merge.")
    return(invisible(NULL))
  }

  main_cols <- if (sweep_main) .dupfam_main_classified(cmain, never_repoint) else NULL
  weigh     <- .dupfam_weigher(con, fks, if (sweep_main) cmain else NULL, main_cols)

  cli::cli_h1("Merge duplicated family entries{if (dry_run) ' (rehearsal)' else ''}")
  cli::cli_alert_info("Repointing: {.field {paste(fks$tbl, fks$col, sep = '.')}} \\
                      and {.field table_taxa.idtax_good_n}")
  done <- list()

  for (fam in unique(groups$tax_fam)) {
    rows <- groups[groups$tax_fam == fam, , drop = FALSE]
    plan <- .dupfam_plan_one(rows, tie_break, weigh)

    cli::cli_h2(fam)
    if (!is.null(plan$skip)) {
      cli::cli_alert_danger("Skipped: {plan$skip}")
      next
    }

    keeper   <- as.integer(plan$keeper)
    ids      <- .dupfam_ids(plan$dupes)
    dupe_ids <- ids
    refs     <- .dupfam_refs_taxa(con, plan$dupes, fks)
    cli::cli_alert_info("keeper {keeper} ({plan$how}); duplicates {dupe_ids}")

    # --- main database first -------------------------------------------------
    # Two databases, so two transactions: there is no single one to hold them.
    # Doing the main one first means that if the taxa side then fails, its rows
    # point at the surviving family - which is correct either way.
    if (sweep_main) {
      mrefs <- .dupfam_refs_main(cmain, plan$dupes, never_repoint, main_cols)
      for (i in seq_len(nrow(mrefs$left))) {
        why <- mrefs$left$why[i]
        cli::cli_alert_info("left alone: {mrefs$left$n[i]} row{?s} in \\
                            {.field {mrefs$left$table_name[i]}}.\\
                            {.field {mrefs$left$column_name[i]}} - {why}")
      }
      for (i in seq_len(nrow(mrefs$repoint))) {
        tbl <- DBI::dbQuoteIdentifier(cmain, mrefs$repoint$table_name[i])
        col <- DBI::dbQuoteIdentifier(cmain, mrefs$repoint$column_name[i])
        if (dry_run) {
          cli::cli_alert_info("would repoint {mrefs$repoint$n[i]} row{?s} in \\
                              {.field {mrefs$repoint$table_name[i]}}.\\
                              {.field {mrefs$repoint$column_name[i]}} to {keeper}")
        } else {
          n <- DBI::dbExecute(cmain, sprintf(
            "UPDATE %s SET %s = %d WHERE %s IN (%s)", tbl, col, keeper, col, ids))
          cli::cli_alert_success("repointed {n} row{?s} in \\
                                 {.field {mrefs$repoint$table_name[i]}}.\\
                                 {.field {mrefs$repoint$column_name[i]}}")
        }
      }
    }

    # --- taxa database -------------------------------------------------------
    if (dry_run) {
      for (i in seq_len(nrow(refs))) {
        if (refs$n[i] > 0) {
          cli::cli_alert_info("would repoint {refs$n[i]} row{?s} in \\
                              {.field {refs$what[i]}} to {keeper}")
        }
      }
      cli::cli_alert_info("would delete {length(plan$dupes)} row{?s}: {dupe_ids}")
      done[[fam]] <- plan
      next
    }

    DBI::dbBegin(con)
    ok <- tryCatch({
      n_syn <- DBI::dbExecute(con, sprintf(
        "UPDATE table_taxa SET idtax_good_n = %d
         WHERE idtax_good_n IN (%s) AND idtax_n <> %d", keeper, ids, keeper))

      n_fk <- 0L
      for (i in seq_len(nrow(fks))) {
        tbl <- DBI::dbQuoteIdentifier(con, fks$tbl[i])
        col <- DBI::dbQuoteIdentifier(con, fks$col[i])

        # A composite key means a link the keeper already holds cannot simply
        # be moved onto it. taxa_backbone_link is the case today; the same
        # treatment is harmless on any table, so it is applied by key rather
        # than by name.
        keycols <- .dupfam_unique_key(con, fks$tbl[i], fks$col[i])
        if (length(keycols) > 0) {
          same <- paste(sprintf("k.%s = d.%s",
                                DBI::dbQuoteIdentifier(con, keycols),
                                DBI::dbQuoteIdentifier(con, keycols)),
                        collapse = " AND ")
          n_drop <- DBI::dbExecute(con, sprintf(
            "DELETE FROM %s d WHERE d.%s IN (%s)
               AND EXISTS (SELECT 1 FROM %s k WHERE k.%s = %d AND %s)",
            tbl, col, ids, tbl, col, keeper, same))
          if (n_drop > 0) {
            cli::cli_alert_info("dropped {n_drop} row{?s} of \\
                                {.field {fks$tbl[i]}} the keeper already had")
          }
        }

        n_fk <- n_fk + DBI::dbExecute(con, sprintf(
          "UPDATE %s SET %s = %d WHERE %s IN (%s)", tbl, col, keeper, col, ids))
      }

      n_del <- DBI::dbExecute(con, sprintf(
        "DELETE FROM table_taxa WHERE idtax_n IN (%s)", ids))

      DBI::dbCommit(con)
      cli::cli_alert_success(
        "{fam}: {n_syn} synonym link{?s} and {n_fk} referencing row{?s} \\
         repointed to {keeper}; {n_del} row{?s} deleted")
      TRUE
    }, error = function(e) {
      DBI::dbRollback(con)
      cli::cli_alert_danger("{fam}: rolled back - {conditionMessage(e)}")
      FALSE
    })

    if (ok) done[[fam]] <- plan
  }

  if (!dry_run && length(done) > 0) .dupfam_refresh_link_table(con, cmain)

  invisible(done)
}

# The other columns of a unique key that `col` takes part in, if any. Two rows
# differing only by `col` would collide once both carry the keeper's id.
.dupfam_unique_key <- function(con, tbl, col) {
  k <- DBI::dbGetQuery(con, sprintf("
    SELECT c.conname, a.attname AS col
    FROM pg_constraint c
    JOIN unnest(c.conkey) WITH ORDINALITY AS u(attnum, ord) ON true
    JOIN pg_attribute a ON a.attrelid = c.conrelid AND a.attnum = u.attnum
    WHERE c.contype IN ('p', 'u') AND c.conrelid = %s::regclass",
    DBI::dbQuoteString(con, tbl)))
  if (nrow(k) == 0) return(character(0))

  # Only a key that `col` takes part in can collide; the others are untouched
  # by the repointing.
  holding <- unique(k$conname[k$col == col])
  if (length(holding) == 0) return(character(0))
  setdiff(unique(k$col[k$conname %in% holding]), col)
}

# table_idtax is the synonymy link table on the MAIN database - two columns,
# idtax_n and idtax_good_n, ~367,000 rows - rebuilt from the taxa database. It
# still holds the deleted ids until it is rebuilt, so this runs on the main
# connection even though the merge happened on the other side.
#
# Refreshing goes through the package's own update_taxa_link_table(), not
# through refresh_table_idtax() directly. That distinction matters here:
# when the staging table `table_idtax_temp` holds rows - which it does on this
# installation - update_taxa_link_table() deliberately takes the legacy path
# instead, because the SQL function does not refresh the staging table. Calling
# the SQL function by hand would report success and leave the staging table on
# the old taxonomy.
.dupfam_refresh_link_table <- function(con_taxa, cmain) {
  if (is.null(cmain)) {
    cli::cli_alert_warning(
      "No main connection: {.field table_idtax} still lists the deleted ids. \\
       Run {.code update_taxa_link_table(force = TRUE)} before anything reads \\
       the taxonomy again.")
    return(invisible(NULL))
  }
  if (is.na(DBI::dbGetQuery(cmain,
        "SELECT to_regclass('public.table_idtax')::text AS r")$r)) {
    return(invisible(NULL))
  }

  tryCatch({
    res <- CafriplotsR::update_taxa_link_table(con = cmain, con_taxa = con_taxa,
                                              force = TRUE)
    if (isTRUE(res$success)) {
      cli::cli_alert_success("Rebuilt {.field table_idtax} ({res$method}, \\
                             {res$record_count} rows)")
    } else {
      cli::cli_alert_warning("{.field table_idtax} not rebuilt: {res$message}")
    }
  }, error = function(e) {
    cli::cli_alert_warning(
      "Could not rebuild {.field table_idtax}: {conditionMessage(e)}. \\
       Run {.code update_taxa_link_table(force = TRUE)} before anything reads \\
       the taxonomy again.")
  })
}


# ---- Evidence ----------------------------------------------------------------

#' What remains duplicated, and whether anything was orphaned
#'
#' Reads only. The expected state after a successful run is: no group left for
#' the families that were merged, and zero dangling references.
check_duplicate_family_taxa <- function(con_taxa) {
  con <- .dupfam_con(con_taxa)
  if (inherits(con_taxa, "Pool")) on.exit(pool::poolReturn(con), add = TRUE)

  groups <- .dupfam_groups(con)
  fams   <- unique(groups$tax_fam)

  dangling_parent <- DBI::dbGetQuery(con, "
    SELECT count(*) AS n FROM table_taxa c
    WHERE c.id_parent IS NOT NULL
      AND NOT EXISTS (SELECT 1 FROM table_taxa p WHERE p.idtax_n = c.id_parent)")$n[1]

  dangling_good <- DBI::dbGetQuery(con, "
    SELECT count(*) AS n FROM table_taxa c
    WHERE c.idtax_good_n IS NOT NULL
      AND NOT EXISTS (SELECT 1 FROM table_taxa p WHERE p.idtax_n = c.idtax_good_n)")$n[1]

  self_syn <- DBI::dbGetQuery(con, "
    SELECT count(*) AS n FROM table_taxa WHERE idtax_good_n = idtax_n")$n[1]

  cli::cli_h1("Duplicate family entries: state")
  if (length(fams) == 0) {
    cli::cli_alert_success("No duplicated family-level name left.")
  } else {
    cli::cli_alert_warning("Still duplicated: {.val {fams}}")
  }
  cli::cli_alert_info("dangling {.field id_parent}: {dangling_parent}")
  cli::cli_alert_info("dangling {.field idtax_good_n}: {dangling_good}")
  cli::cli_alert_info("rows that are their own synonym: {self_syn}")

  invisible(list(duplicated = fams,
                 dangling_parent = dangling_parent,
                 dangling_good = dangling_good,
                 self_synonym = self_syn))
}
