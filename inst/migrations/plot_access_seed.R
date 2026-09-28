# =============================================================================
# Seed plot_access from what currently grants access (step 4c)
#
# There are exactly two things granting access to a plot today, and both have
# to land in plot_access or step 5 locks someone out of their own data:
#
#   1. the ~125 per-account policies on data_liste_plots, whose USING
#      expressions carry literal plot ID lists
#   2. data_liste_plots.created_by, read by the global creator_access_* policies
#
# The owner is excluded from both. It bypasses row-level security by ownership,
# and add_created_by.R's backfill attributed every pre-existing plot to it, so
# seeding those would add ~2,000 rows saying the owner can see what the owner
# can already see, and put the owner's name in every who-can-see-this answer.
#
# WHAT MAKES THIS SAFE TO APPLY BEFORE STEP 5
#
# Nothing reads plot_access yet. No child table has row-level security, and the
# 125 policies are untouched. Seeding is therefore a data-only change that can
# be re-run, corrected, or emptied with a DELETE. The point of doing it now is
# to find out whether the grant set is recoverable at all, while a mistake
# still costs nothing.
#
# THE GATE
#
# After writing, every account's plot set is compared against
# get_user_accessible_plots() -- the package's existing reader, which parses the
# same policies with a deliberately loose digit regex
# (R/connections_db.R:1182). Where the two agree, both are right. Where they
# disagree, either that reader is scraping digits out of an expression that is
# not a plot list, or this seed dropped a grant. Applying aborts on any
# disagreement rather than leaving one to be discovered at step 5.
#
# WHAT IS NOT RECOVERABLE
#
# When each grant was originally made. A policy records no date, so every
# seeded row gets today's granted_on. Rows written from here on are accurate.
#
# PREREQUISITES
#   inst/migrations/created_by_server_asserted.R
#   inst/migrations/plot_access_table.R
#
# TO UNDO
#   DELETE FROM plot_access;   -- nothing reads it yet
# =============================================================================


#' Grants implied by the per-account policies on data_liste_plots
#'
#' @param con A connection to plots_transects, as the owner.
#' @return A list with `grants` (data.frame of db_user, id_liste_plots,
#'   can_write), `policies` (one row per policy, with how it was read), and
#'   `unparseable` (the policies that could not be read).
.seed_policy_grants <- function(con) {

  pol <- DBI::dbGetQuery(con, "
    SELECT p.policyname, p.cmd, p.permissive, p.qual, u.role_name
      FROM pg_policies p,
           LATERAL unnest(p.roles) AS u(role_name)
     WHERE p.schemaname = 'public' AND p.tablename = 'data_liste_plots'
     ORDER BY u.role_name, p.policyname")

  owner <- DBI::dbGetQuery(con, "
    SELECT pg_get_userbyid(relowner) AS n
      FROM pg_class WHERE oid = 'public.data_liste_plots'::regclass")$n

  pol$kind <- NA_character_
  pol$n_ids <- 0L
  pol$capability <- NA_character_
  pol$used <- FALSE
  pol$reason <- NA_character_

  rows <- list()

  for (i in seq_len(nrow(pol))) {

    parsed <- CafriplotsR:::.parse_policy_plot_ids(pol$qual[i])
    pol$kind[i]  <- parsed$kind
    pol$n_ids[i] <- length(parsed$ids)

    # A policy TO PUBLIC is a global rule (the creator policies, insert_own),
    # not a per-account grant. Its effect arrives through created_by instead.
    if (identical(tolower(pol$role_name[i]), "public")) {
      pol$reason[i] <- "global policy (TO PUBLIC) - not a per-account grant"
      next
    }
    if (identical(pol$role_name[i], owner)) {
      pol$reason[i] <- "owner - bypasses row-level security by ownership"
      next
    }

    capability <- CafriplotsR:::.policy_cmd_capability(pol$cmd[i])
    pol$capability[i] <- capability

    if (is.na(capability)) {
      pol$reason[i] <- "INSERT - governed globally, carries no plot list"
      next
    }
    if (identical(parsed$kind, "creator")) {
      pol$reason[i] <- "created_by comparison - arrives through the creator route"
      next
    }
    if (identical(parsed$kind, "none")) {
      pol$reason[i] <- "no USING expression"
      next
    }
    if (!identical(parsed$kind, "ids")) {
      pol$reason[i] <- "UNPARSEABLE - refused rather than guessed at"
      next
    }
    if (!identical(pol$permissive[i], "PERMISSIVE")) {
      pol$reason[i] <- "RESTRICTIVE policy - narrows access, cannot be read as a grant"
      pol$kind[i] <- "unparseable"
      next
    }

    pol$used[i] <- TRUE
    pol$reason[i] <- "read as a grant"
    rows[[length(rows) + 1L]] <- data.frame(
      db_user        = pol$role_name[i],
      id_liste_plots = parsed$ids,
      can_write      = capability %in% c("write", "all"),
      can_delete     = capability %in% c("delete", "all"),
      stringsAsFactors = FALSE
    )
  }

  grants <- if (length(rows) > 0) do.call(rbind, rows) else
    data.frame(db_user = character(0), id_liste_plots = integer(0),
               can_write = logical(0), can_delete = logical(0),
               stringsAsFactors = FALSE)

  # One row per (account, plot); the strongest capability wins.
  if (nrow(grants) > 0) {
    key <- paste(grants$db_user, grants$id_liste_plots, sep = "\r")
    grants <- do.call(rbind, lapply(split(grants, key), function(g) {
      data.frame(db_user = g$db_user[1], id_liste_plots = g$id_liste_plots[1],
                 can_write = any(g$can_write), can_delete = any(g$can_delete),
                 stringsAsFactors = FALSE)
    }))
    grants <- grants[order(grants$db_user, grants$id_liste_plots), ]
    rownames(grants) <- NULL
  }

  list(grants = grants, policies = pol,
       unparseable = pol[pol$kind == "unparseable" &
                         !is.na(pol$kind), , drop = FALSE])
}


#' Grants implied by created_by
#'
#' @param con A connection to plots_transects, as the owner.
#' @return A list with `grants` and `not_a_role` (values naming no database
#'   role, which cannot be turned into a grant).
.seed_creator_grants <- function(con) {

  g <- DBI::dbGetQuery(con, "
    SELECT p.created_by AS db_user,
           p.id_liste_plots,
           EXISTS (SELECT 1 FROM pg_roles r WHERE r.rolname = p.created_by)
             AS is_a_role
      FROM data_liste_plots p
     WHERE p.created_by IS NOT NULL
       AND p.created_by <> (SELECT pg_get_userbyid(relowner) FROM pg_class
                             WHERE oid = 'public.data_liste_plots'::regclass)
     ORDER BY 1, 2")

  bad <- g[!g$is_a_role, , drop = FALSE]
  ok  <- g[g$is_a_role, c("db_user", "id_liste_plots"), drop = FALSE]
  if (nrow(ok) > 0) {
    ok$can_write  <- TRUE
    # FALSE, like everyone else. "Remove DELETE from all users and let me give
    # it back when needed" applies to importers too, so a mis-imported plot is
    # deleted by the owner or after grant_delete_right(). To change that:
    #   UPDATE plot_access SET can_delete = TRUE WHERE origin = 'creator';
    ok$can_delete <- FALSE
    ok$can_grant  <- TRUE
    rownames(ok) <- NULL
  } else {
    ok <- data.frame(db_user = character(0), id_liste_plots = integer(0),
                     can_write = logical(0), can_delete = logical(0),
                     can_grant = logical(0), stringsAsFactors = FALSE)
  }

  list(grants = ok, not_a_role = bad)
}


#' What the seed would write, read-only
#'
#' Run this first. It touches nothing and answers the two questions that decide
#' whether the seed is trustworthy: can every policy be read, and does every
#' `created_by` value name a real role?
#'
#' @param con A connection to plots_transects, as the owner.
#' @return Invisibly a list with everything gathered.
report_plot_access_seed <- function(con) {

  stopifnot("Invalid connection" = DBI::dbIsValid(con))

  cli::cli_h1("What the plot_access seed would write")

  pol <- .seed_policy_grants(con)
  cre <- .seed_creator_grants(con)

  # --- how each policy was read --------------------------------------------
  cli::cli_h2("Policies on data_liste_plots: {nrow(pol$policies)} (role x policy)")
  summ <- as.data.frame(table(kind = pol$policies$kind,
                              used = pol$policies$used))
  print(summ[summ$Freq > 0, ], row.names = FALSE)

  cli::cli_h2("Why each policy was or was not read as a grant")
  print(as.data.frame(table(reason = pol$policies$reason)), row.names = FALSE)

  if (nrow(pol$unparseable) > 0) {
    cli::cli_alert_danger(
      "{nrow(pol$unparseable)} policy/policies could not be read:")
    print(pol$unparseable[, c("role_name", "policyname", "cmd", "qual")],
          row.names = FALSE)
    cli::cli_alert_info(
      "Applying will refuse while any of these remain. Either they are not plot
       grants - in which case say so and they can be skipped explicitly - or the
       parser needs a new shape.")
  } else {
    cli::cli_alert_success("Every policy was read, or explained")
  }

  # --- created_by ----------------------------------------------------------
  cli::cli_h2("Creator route")
  if (nrow(cre$not_a_role) > 0) {
    cli::cli_alert_danger(
      "{nrow(cre$not_a_role)} plot{?s} {?is/are} attributed to a name that is not
       a database role:")
    print(utils::head(unique(cre$not_a_role$db_user), 20))
    cli::cli_alert_info("No grant can be written for those.")
  } else {
    cli::cli_alert_success("Every created_by value names a real database role")
  }
  cli::cli_alert_info(
    "{nrow(cre$grants)} creator grant{?s} across
     {length(unique(cre$grants$db_user))} account{?s}")

  # --- what the DELETE policies say, before the default overrides them -----
  cli::cli_h2("DELETE, which is not carried over by default")
  faithful <- .seed_combine(pol$grants, cre$grants, preserve_delete = TRUE)
  del <- faithful[faithful$can_delete, , drop = FALSE]
  if (nrow(del) == 0) {
    cli::cli_alert_success("No policy grants DELETE")
  } else {
    per_del <- as.data.frame(table(db_user = del$db_user))
    per_del <- per_del[per_del$Freq > 0, ]
    per_del <- per_del[order(-per_del$Freq), ]
    names(per_del)[2] <- "n_delete_in_policies"
    print(utils::head(per_del, 40), row.names = FALSE)
    cli::cli_alert_warning(
      "{nrow(del)} DELETE grant{?s} across {nrow(per_del)} account{?s} will be
       recorded as {.code can_delete = FALSE}.")
    cli::cli_alert_info(
      "Nothing is lost: the DELETE policies stay in pg_policies untouched
       through step 4, and {.code migrate_plot_access_seed(preserve_delete = TRUE)}
       carries them over instead.")
  }

  # --- combined ------------------------------------------------------------
  combined <- .seed_combine(pol$grants, cre$grants)

  cli::cli_h2("Rows that would be written: {nrow(combined)}")
  if (nrow(combined) > 0) {
    per_user <- do.call(rbind, lapply(split(combined, combined$db_user), function(u) {
      data.frame(db_user = u$db_user[1], n_plots = nrow(u),
                 n_write = sum(u$can_write), n_delete = sum(u$can_delete),
                 n_grant = sum(u$can_grant),
                 origin_creator = sum(u$origin == "creator"),
                 stringsAsFactors = FALSE)
    }))
    per_user <- per_user[order(-per_user$n_plots), ]
    print(per_user, row.names = FALSE)
    cli::cli_alert_info(
      "Largest grant: {max(per_user$n_plots)} plots. Smallest: {min(per_user$n_plots)}.")
  }

  # --- the gate, previewed -------------------------------------------------
  gate <- .seed_gate(con, combined)
  .seed_print_gate(gate)

  invisible(list(policies = pol, creator = cre, combined = combined, gate = gate))
}


#' Combine the two grant sources into rows for plot_access
#'
#' @param policy_grants From `.seed_policy_grants()`.
#' @param creator_grants From `.seed_creator_grants()`.
#' @return A data.frame ready to insert.
.seed_combine <- function(policy_grants, creator_grants, preserve_delete = FALSE) {

  empty <- data.frame(db_user = character(0), id_liste_plots = integer(0),
                      can_write = logical(0), can_delete = logical(0),
                      can_grant = logical(0),
                      origin = character(0), stringsAsFactors = FALSE)

  a <- if (nrow(policy_grants) > 0) {
    data.frame(policy_grants, can_grant = FALSE, origin = "admin",
               stringsAsFactors = FALSE)
  } else empty

  b <- if (nrow(creator_grants) > 0) {
    data.frame(creator_grants, origin = "creator", stringsAsFactors = FALSE)
  } else empty

  all_rows <- rbind(a[, names(empty)], b[, names(empty)])
  if (nrow(all_rows) == 0) return(empty)

  key <- paste(all_rows$db_user, all_rows$id_liste_plots, sep = "\r")
  out <- do.call(rbind, lapply(split(all_rows, key), function(g) {
    data.frame(
      db_user        = g$db_user[1],
      id_liste_plots = as.integer(g$id_liste_plots[1]),
      can_write      = any(g$can_write),
      # The one capability the seed does not carry over by default. The old
      # DELETE policies stay in pg_policies untouched through step 4, so this is
      # a decision recorded in data, not information destroyed.
      can_delete     = if (preserve_delete) any(g$can_delete) else FALSE,
      can_grant      = any(g$can_grant),
      # Creator is the stronger statement: it is why can_grant is set.
      origin         = if (any(g$origin == "creator")) "creator" else "admin",
      stringsAsFactors = FALSE)
  }))
  out <- out[order(out$db_user, out$id_liste_plots), ]
  rownames(out) <- NULL
  out
}


#' Compare a grant set against the package's own reader
#'
#' @param con A connection to plots_transects, as the owner.
#' @param combined The rows the seed holds or would write.
#' @return A data.frame, one row per account, with the two set sizes and the
#'   symmetric difference.
.seed_gate <- function(con, combined) {

  owner <- DBI::dbGetQuery(con, "
    SELECT pg_get_userbyid(relowner) AS n
      FROM pg_class WHERE oid = 'public.data_liste_plots'::regclass")$n

  users <- sort(unique(setdiff(combined$db_user, owner)))
  if (length(users) == 0) {
    return(data.frame(db_user = character(0), n_seed = integer(0),
                      n_reader = integer(0), missing_from_seed = character(0),
                      extra_in_seed = character(0), agrees = logical(0),
                      stringsAsFactors = FALSE))
  }

  rows <- lapply(users, function(u) {

    seed_ids <- sort(unique(combined$id_liste_plots[combined$db_user == u]))

    reader_ids <- tryCatch({
      res <- suppressMessages(
        CafriplotsR:::get_user_accessible_plots(con, u, "data_liste_plots"))
      if (is.null(res) || nrow(res) == 0) integer(0)
      else sort(unique(as.integer(unlist(res$plot_ids))))
    }, error = function(e) {
      cli::cli_alert_warning("get_user_accessible_plots('{u}') failed: {e$message}")
      NA_integer_
    })

    if (length(reader_ids) == 1 && is.na(reader_ids[1])) {
      return(data.frame(db_user = u, n_seed = length(seed_ids),
                        n_reader = NA_integer_,
                        missing_from_seed = "(reader failed)",
                        extra_in_seed = "", agrees = FALSE,
                        stringsAsFactors = FALSE))
    }

    miss  <- setdiff(reader_ids, seed_ids)
    extra <- setdiff(seed_ids, reader_ids)
    abbrev <- function(x) {
      if (length(x) == 0) return("")
      if (length(x) <= 8) paste(x, collapse = ",")
      paste0(paste(x[1:8], collapse = ","), " (+", length(x) - 8, " more)")
    }

    data.frame(db_user = u, n_seed = length(seed_ids),
               n_reader = length(reader_ids),
               missing_from_seed = abbrev(miss),
               extra_in_seed = abbrev(extra),
               agrees = length(miss) == 0 && length(extra) == 0,
               stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' @noRd
.seed_print_gate <- function(gate) {
  cli::cli_h2("Gate: seed vs get_user_accessible_plots()")
  if (nrow(gate) == 0) {
    cli::cli_alert_warning("No accounts to compare")
    return(invisible(NULL))
  }
  bad <- gate[!gate$agrees, , drop = FALSE]
  if (nrow(bad) == 0) {
    cli::cli_alert_success(
      "All {nrow(gate)} account{?s} agree, plot for plot, with the reader the
       package already uses")
  } else {
    cli::cli_alert_danger("{nrow(bad)} account{?s} disagree{?s/}:")
    print(bad, row.names = FALSE)
  }
  invisible(NULL)
}


#' Write the seed rows into plot_access
#'
#' @param con A connection to plots_transects, as the owner.
#' @param include_creator Logical. Include grants derived from `created_by`.
#'   Default `TRUE`. `FALSE` seeds only the explicit policies, which would leave
#'   importers without access to the plots they imported - and the gate will
#'   then fail and roll back, because `get_user_accessible_plots()` counts
#'   creator access too. It is here to make the two sources separable while
#'   reading the code, not because `FALSE` is a usable setting.
#' @param preserve_delete Logical. Carry the existing DELETE policies over as
#'   `can_delete = TRUE`. Default `FALSE`: DELETE is off for every account and
#'   handed out per plot with `grant_delete_right()`. Nothing is destroyed by the
#'   default - the DELETE policies stay in `pg_policies` untouched through step
#'   4, so `TRUE` recovers the original state at any time.
#' @param skip_unparseable Logical. Proceed even though some policy could not
#'   be read. Default `FALSE`, i.e. refuse. Only set this after looking at
#'   `report_plot_access_seed()` and concluding those policies are not plot
#'   grants.
#' @param dry_run Logical. `TRUE` (the default) reports and changes nothing.
#' @return Invisibly the number of rows written.
migrate_plot_access_seed <- function(con, include_creator = TRUE,
                                     preserve_delete = FALSE,
                                     skip_unparseable = FALSE, dry_run = TRUE) {

  stopifnot("Invalid connection" = DBI::dbIsValid(con))

  if (!isTRUE(DBI::dbGetQuery(con, "
        SELECT to_regclass('public.plot_access') IS NOT NULL AS ok")$ok)) {
    cli::cli_abort(c(
      "plot_access does not exist.",
      i = "Apply {.file inst/migrations/plot_access_table.R} first."))
  }

  pol <- .seed_policy_grants(con)
  cre <- .seed_creator_grants(con)

  if (nrow(pol$unparseable) > 0 && !skip_unparseable) {
    print(pol$unparseable[, c("role_name", "policyname", "cmd", "qual")],
          row.names = FALSE)
    cli::cli_abort(c(
      "{nrow(pol$unparseable)} policy/policies could not be read.",
      x = "Seeding a partial grant set would lock someone out at step 5.",
      i = "Inspect with {.code report_plot_access_seed(con)}.",
      i = "If they are genuinely not plot grants, re-run with
           {.code skip_unparseable = TRUE}."))
  }

  if (nrow(cre$not_a_role) > 0) {
    cli::cli_abort(c(
      "{nrow(cre$not_a_role)} plot{?s} {?is/are} attributed to a name that is not
       a database role.",
      i = "Those creators cannot be granted anything. Fix created_by, or accept
           the loss knowingly - there is no flag for it because it should not
           happen once P4.3 is applied."))
  }

  combined <- .seed_combine(
    pol$grants,
    if (include_creator) cre$grants else cre$grants[0, ],
    preserve_delete = preserve_delete)

  n_del_policy <- sum(.seed_combine(pol$grants, cre$grants,
                                    preserve_delete = TRUE)$can_delete)
  if (!preserve_delete && n_del_policy > 0) {
    cli::cli_alert_warning(
      "{n_del_policy} DELETE grant{?s} in the policies {?is/are} being recorded as
       {.code can_delete = FALSE}. The policies themselves are untouched.")
  }

  if (nrow(combined) == 0) {
    cli::cli_alert_warning("Nothing to seed.")
    return(invisible(0L))
  }

  existing <- DBI::dbGetQuery(con,
    "SELECT count(*)::int AS n FROM public.plot_access")$n
  cli::cli_alert_info(
    "plot_access holds {existing} row{?s}; the seed has {nrow(combined)} row{?s}
     across {length(unique(combined$db_user))} account{?s}")

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was written.")
    .seed_print_gate(.seed_gate(con, combined))
    cli::cli_alert_info("Re-run with {.code dry_run = FALSE} to write.")
    return(invisible(0L))
  }

  # --- write ---------------------------------------------------------------
  # ON CONFLICT with OR semantics, so re-running is idempotent and never
  # narrows an existing grant.
  chunk_size <- 1000L
  chunks <- split(combined, ceiling(seq_len(nrow(combined)) / chunk_size))

  DBI::dbBegin(con)
  ok <- FALSE
  on.exit({
    if (!ok) {
      try(DBI::dbRollback(con), silent = TRUE)
      cli::cli_alert_danger("Rolled back - nothing was written.")
    }
  }, add = TRUE)

  written <- 0L
  for (ch in chunks) {
    values <- paste0(
      "(", DBI::dbQuoteString(con, ch$db_user), ", ",
      as.integer(ch$id_liste_plots), ", ",
      ifelse(ch$can_write, "TRUE", "FALSE"), ", ",
      ifelse(ch$can_delete, "TRUE", "FALSE"), ", ",
      ifelse(ch$can_grant, "TRUE", "FALSE"), ", ",
      DBI::dbQuoteString(con, ch$origin), ", ",
      # A creator row is attributed to the creator, exactly as
      # trg_plot_access_creator writes it, so the two are indistinguishable
      # afterwards and "origin = 'creator' implies granted_by = db_user" holds.
      ifelse(ch$origin == "creator",
             as.character(DBI::dbQuoteString(con, ch$db_user)),
             "current_user"), ", ",
      DBI::dbQuoteString(con, paste0("seeded from the ", ch$origin,
                                     " route; original grant date not recoverable")),
      ")",
      collapse = ", ")

    sql <- paste0(
      "INSERT INTO public.plot_access
         (db_user, id_liste_plots, can_write, can_delete, can_grant,
          origin, granted_by, note)
       VALUES ", values, "
       ON CONFLICT (db_user, id_liste_plots) DO UPDATE
         SET can_write = public.plot_access.can_write OR EXCLUDED.can_write,
             can_grant = public.plot_access.can_grant OR EXCLUDED.can_grant,
             origin    = CASE WHEN EXCLUDED.origin = 'creator' THEN 'creator'
                              ELSE public.plot_access.origin END")
    # can_delete is deliberately absent from DO UPDATE. Re-running the seed must
    # never hand DELETE back to an account the owner has since taken it from -
    # the OR semantics that make the other flags safely idempotent would do
    # exactly that.

    written <- written + DBI::dbExecute(con, sql)
  }

  # --- the gate, before committing -----------------------------------------
  held <- DBI::dbGetQuery(con, "
    SELECT db_user, id_liste_plots, can_write, can_delete, can_grant, origin
      FROM public.plot_access ORDER BY 1, 2")
  gate <- .seed_gate(con, held)
  .seed_print_gate(gate)

  if (any(!gate$agrees)) {
    cli::cli_abort(c(
      "The gate failed for {sum(!gate$agrees)} account{?s} - rolling back.",
      i = "Nothing was written. Inspect with {.code report_plot_access_seed(con)}."))
  }

  DBI::dbCommit(con)
  ok <- TRUE

  cli::cli_alert_success("{written} row{?s} written to plot_access")
  cli::cli_alert_info(
    "Nothing reads it yet. The 125 policies still enforce access; no child
     table has row-level security.")

  invisible(written)
}


#' Re-check the seed against the policies at any time
#'
#' @param con A connection to plots_transects, as the owner.
#' @return Invisibly `TRUE` if the seed still matches.
check_plot_access_seed <- function(con) {

  cli::cli_h1("Verifying the plot_access seed")

  held <- DBI::dbGetQuery(con, "
    SELECT db_user, id_liste_plots, can_write, can_delete, can_grant, origin
      FROM public.plot_access ORDER BY 1, 2")

  cli::cli_alert_info("{nrow(held)} row{?s} across
                       {length(unique(held$db_user))} account{?s}")

  by_origin <- as.data.frame(table(origin = held$origin))
  print(by_origin, row.names = FALSE)

  cli::cli_alert_info(
    "{sum(held$can_write)} row{?s} with can_write, {sum(held$can_delete)} with
     can_delete, {sum(held$can_grant)} with can_grant")
  if (sum(held$can_delete) > 0) {
    print(as.data.frame(table(db_user = held$db_user[held$can_delete])),
          row.names = FALSE)
  }

  gate <- .seed_gate(con, held)
  .seed_print_gate(gate)

  # Creator rows must still match created_by in both directions. Stored can now
  # diverge from derived, which was impossible while access was derived on read.
  cli::cli_h2("Creator rows vs created_by")
  drift <- DBI::dbGetQuery(con, "
    WITH derived AS (
      SELECT created_by AS db_user, id_liste_plots
        FROM data_liste_plots
       WHERE created_by IS NOT NULL
         AND created_by <> (SELECT pg_get_userbyid(relowner) FROM pg_class
                             WHERE oid = 'public.data_liste_plots'::regclass)
    ), stored AS (
      SELECT db_user, id_liste_plots FROM plot_access WHERE origin = 'creator'
    )
    SELECT 'in created_by, no grant row' AS problem, d.db_user,
           count(*)::int AS n
      FROM derived d LEFT JOIN stored s
        ON s.db_user = d.db_user AND s.id_liste_plots = d.id_liste_plots
     WHERE s.db_user IS NULL
     GROUP BY 1, 2
    UNION ALL
    SELECT 'grant row, not in created_by', s.db_user, count(*)::int
      FROM stored s LEFT JOIN derived d
        ON d.db_user = s.db_user AND d.id_liste_plots = s.id_liste_plots
     WHERE d.db_user IS NULL
     GROUP BY 1, 2
     ORDER BY 1, 2")

  if (nrow(drift) == 0) {
    cli::cli_alert_success("Creator rows match created_by exactly")
  } else {
    cli::cli_alert_danger("Creator rows have drifted from created_by:")
    print(drift, row.names = FALSE)
  }

  pass <- all(gate$agrees) && nrow(drift) == 0
  if (pass) cli::cli_alert_success("The seed is consistent with both sources")
  else      cli::cli_alert_danger("Verification failed - see above")

  invisible(pass)
}
