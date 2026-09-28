# =============================================================================
# Helpers for the plot_access grant table
#
# The table itself is created by inst/migrations/plot_access_table.R and seeded
# by inst/migrations/plot_access_seed.R. What lives here is the part that has
# to be *right* rather than merely run once: reading an existing row-level
# security policy and deciding what grant it represents.
#
# The seed refuses to guess. If a policy's USING expression is not one of the
# shapes define_user_policy() produces, .parse_policy_plot_ids() says so and
# the migration aborts naming it, rather than seeding a partial grant set. A
# grant silently lost here is a colleague locked out of their own plots later,
# and it would look like a policy bug rather than a seeding bug.
# =============================================================================

#' @title Parse the plot IDs out of a row-level security policy expression
#' @description
#' Reads the `qual` (USING expression) of a policy on `data_liste_plots` and
#' returns the plot IDs it grants access to.
#'
#' `define_user_policy()` writes `USING (id_liste_plots IN (...))`, which
#' PostgreSQL stores back in one of three deparsed forms depending on how many
#' IDs were given. The creator policies from `inst/migrations/add_created_by.R`
#' instead compare `created_by` to `current_user` and carry no ID list at all.
#'
#' Anything else is reported as `"unparseable"`. This is deliberate: a loose
#' parser that scraped digits out of an unexpected expression would produce a
#' plausible-looking, wrong grant set.
#'
#' @param qual Character of length 1, or `NA`. The `qual` column of
#'   `pg_policies`.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{kind}{One of `"ids"` (an explicit plot list), `"creator"` (access
#'       derived from `created_by`), `"none"` (no USING expression, e.g. an
#'       INSERT policy), or `"unparseable"`.}
#'     \item{ids}{Integer vector of plot IDs, sorted and unique. Empty unless
#'       `kind` is `"ids"`.}
#'   }
#'
#' @examples
#' .parse_policy_plot_ids("(id_liste_plots = ANY (ARRAY[179, 180]))")
#' .parse_policy_plot_ids("(created_by = (CURRENT_USER)::text)")
#' .parse_policy_plot_ids("(ddlat > (0)::double precision)")
#'
#' @keywords internal
#' @export
.parse_policy_plot_ids <- function(qual) {

  if (length(qual) != 1L) {
    stop(".parse_policy_plot_ids() takes one qual at a time, got ",
         length(qual), call. = FALSE)
  }

  no_ids <- function(kind) list(kind = kind, ids = integer(0))

  if (is.na(qual) || !nzchar(trimws(qual))) return(no_ids("none"))

  # Collapse the whitespace PostgreSQL may have introduced, then peel one layer
  # of outer parentheses. A compound expression such as "(a) AND (b)" peels to
  # "a) AND (b", matches nothing below, and is correctly reported unparseable.
  q <- gsub("[[:space:]]+", " ", trimws(qual))
  q <- sub("^[(](.*)[)]$", "\\1", q)

  if (grepl("^created_by = [(]?CURRENT_USER[)]?(::text)?$", q,
            ignore.case = TRUE)) {
    return(no_ids("creator"))
  }

  inner <- NULL

  # (a) id_liste_plots = ANY (ARRAY[179, 180])   -- the usual form
  #     The inner group excludes "]" so an "::integer[]" cast after the bracket
  #     cannot be swallowed into the ID list.
  m <- regmatches(q, regexec(
    "^id_liste_plots = ANY [(]ARRAY[[]([^]]*)[]](::integer[[][]])?[)]$", q))[[1]]
  if (length(m) >= 2L) inner <- m[2]

  # (b) id_liste_plots = ANY ('{179,180}'::integer[])
  if (is.null(inner)) {
    m <- regmatches(q, regexec(
      "^id_liste_plots = ANY [(]'[{]([^}]*)[}]'::integer[[][]][)]$", q))[[1]]
    if (length(m) >= 2L) inner <- m[2]
  }

  # (c) id_liste_plots = 179   -- what a single-ID grant deparses to
  if (is.null(inner)) {
    m <- regmatches(q, regexec("^id_liste_plots = ([0-9]+)$", q))[[1]]
    if (length(m) >= 2L) inner <- m[2]
  }

  if (is.null(inner)) return(no_ids("unparseable"))
  inner <- trimws(inner)

  # Whatever the shape, the payload must be nothing but positive integers.
  if (!grepl("^[0-9]+( *, *[0-9]+)*$", inner)) return(no_ids("unparseable"))

  ids <- suppressWarnings(as.integer(strsplit(inner, " *, *")[[1]]))
  if (anyNA(ids) || any(ids <= 0L)) return(no_ids("unparseable"))

  list(kind = "ids", ids = sort(unique(ids)))
}


#' @title What capability does a policy command confer?
#' @description
#' Maps the `cmd` column of `pg_policies` onto the capability flags of
#' `plot_access`.
#'
#' DELETE is kept separate from UPDATE rather than folded into one `can_write`
#' flag. It is the destructive one, it cascades through six child tables, and
#' `safe_delete_plot()` is not atomic, so it is the one capability that should
#' be handed out deliberately rather than inherited.
#'
#' Every command implies read, because a policy's USING expression is evaluated
#' against rows the account must already be able to see - which is why
#' `plot_access` has no `can_read` column: a row in it *is* read access, and
#' `can_write` and `can_delete` are the escalations.
#'
#' INSERT returns `NA`: insertion on `data_liste_plots` is governed by a single
#' global policy with no plot list, so it contributes nothing to a per-plot
#' grant and the caller skips it.
#'
#' @param cmd Character of length 1. One of `"SELECT"`, `"INSERT"`,
#'   `"UPDATE"`, `"DELETE"`, `"ALL"`.
#'
#' @return `"read"`, `"write"`, `"delete"`, `"all"`, or `NA_character_` for
#'   INSERT. Errors on anything else.
#'
#' @examples
#' .policy_cmd_capability("SELECT")
#' .policy_cmd_capability("DELETE")
#' .policy_cmd_capability("ALL")
#'
#' @keywords internal
#' @export
.policy_cmd_capability <- function(cmd) {

  if (length(cmd) != 1L || is.na(cmd)) {
    stop(".policy_cmd_capability() takes one command at a time", call. = FALSE)
  }

  switch(
    toupper(trimws(cmd)),
    "SELECT" = "read",
    "UPDATE" = "write",
    "DELETE" = "delete",
    "ALL"    = "all",
    "INSERT" = NA_character_,
    stop("Unknown policy command: ", cmd, call. = FALSE)
  )
}


# =============================================================================
# Handing DELETE out, and taking it back
#
# DELETE is off by default for every account, on both layers:
#
#   - the table privilege, swept by inst/migrations/revoke_delete_rights.R
#   - plot_access.can_delete, which defaults to FALSE
#
# Both have to be on for an account to delete a plot once step 5 enforces
# row-level security on the child tables. Until then only the table privilege
# matters, because only data_liste_plots has any policy at all - which is why
# grant_delete_right() says so out loud rather than implying a plot-scoped
# grant it cannot yet deliver.
# =============================================================================

#' The seven tables a plot deletion reaches
#' @keywords internal
#' @noRd
.plot_scope_tables <- function() {
  c("data_liste_plots", "data_liste_sub_plots", "data_subplot_feat",
    "data_individuals", "data_traits_measures", "data_ind_measures_feat",
    "data_link_specimens")
}


#' @title Give an account the right to delete plots
#' @description
#' Grants DELETE on the seven tables a plot deletion reaches, and sets
#' `can_delete` on that account's `plot_access` rows for the named plots.
#'
#' Both layers are needed, and they are not equally precise. `can_delete` is
#' per plot. The table privilege is not - PostgreSQL has no per-row GRANT - so
#' until row-level security reaches the child tables, the table privilege is the
#' only gate on individuals, measurements and specimens, and it is account-wide.
#' This function says so on every call rather than leaving the caller to infer a
#' plot-scoped delete that does not yet exist.
#'
#' @param con A connection to plots_transects, as the owner of the tables.
#' @param user Character. The database role to grant to.
#' @param ids Integer vector of plot IDs, or `NULL` for every plot the account
#'   already has access to.
#' @param tables Character vector of tables to grant DELETE on. Defaults to the
#'   seven a plot deletion reaches.
#' @param dry_run Logical. `TRUE` (the default) reports and changes nothing.
#'
#' @return Invisibly the number of `plot_access` rows updated.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' grant_delete_right(con, "arthur", ids = c(2101, 2102))
#' grant_delete_right(con, "arthur", ids = c(2101, 2102), dry_run = FALSE)
#' }
#' @export
grant_delete_right <- function(con, user, ids = NULL,
                               tables = .plot_scope_tables(),
                               dry_run = TRUE) {

  .assert_plot_access(con)
  stopifnot(
    "user must be a single role name" = length(user) == 1L && nchar(user) > 0,
    "ids must be finite" = is.null(ids) || all(is.finite(ids))
  )

  held <- .plot_access_rows(con, user, ids)
  if (nrow(held) == 0) {
    cli::cli_abort(c(
      "{.val {user}} has no plot_access row for those plots.",
      i = "Grant access first - can_delete is an escalation of an existing grant."))
  }

  cli::cli_alert_info(
    "{.val {user}}: {nrow(held)} plot{?s} would get can_delete = TRUE")
  cli::cli_alert_warning(c(
    "The table privilege cannot be plot-scoped. Until row-level security reaches
     the child tables, this lets {.val {user}} delete individuals, measurements
     and specimens on {.strong any} plot it can read, not only these
     {nrow(held)}."))

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed.")
    return(invisible(0L))
  }

  n <- .plot_access_set_delete(con, user, ids, value = TRUE, tables = tables,
                               grant_table = TRUE)
  cli::cli_alert_success(
    "{n} plot_access row{?s} updated; DELETE granted on {length(tables)} tables")
  invisible(n)
}


#' @title Take the right to delete plots back
#' @description
#' Clears `can_delete` and, unless the account keeps it on some other plot,
#' revokes DELETE on the tables as well.
#'
#' @inheritParams grant_delete_right
#' @param revoke_table Logical. Also revoke the table privilege once the account
#'   holds `can_delete` on no plot. Default `TRUE`.
#'
#' @return Invisibly the number of `plot_access` rows updated.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' revoke_delete_right(con, "arthur", dry_run = FALSE)
#' }
#' @export
revoke_delete_right <- function(con, user, ids = NULL,
                                tables = .plot_scope_tables(),
                                revoke_table = TRUE, dry_run = TRUE) {

  .assert_plot_access(con)
  stopifnot(
    "user must be a single role name" = length(user) == 1L && nchar(user) > 0,
    "ids must be finite" = is.null(ids) || all(is.finite(ids))
  )

  held <- .plot_access_rows(con, user, ids)
  held <- held[held$can_delete, , drop = FALSE]

  if (nrow(held) == 0) {
    cli::cli_alert_info("{.val {user}} holds can_delete on no matching plot.")
  } else {
    cli::cli_alert_info(
      "{.val {user}}: can_delete would be cleared on {nrow(held)} plot{?s}")
  }

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed.")
    return(invisible(0L))
  }

  n <- .plot_access_set_delete(con, user, ids, value = FALSE, tables = tables,
                               grant_table = FALSE)

  if (revoke_table) {
    remaining <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT count(*)::int AS n FROM plot_access
        WHERE db_user = {user} AND can_delete", .con = con))$n
    if (remaining == 0) {
      for (tb in tables) {
        DBI::dbExecute(con, paste0(
          "REVOKE DELETE ON ", DBI::dbQuoteIdentifier(con, tb),
          " FROM ", DBI::dbQuoteIdentifier(con, user), ";"))
      }
      cli::cli_alert_success("DELETE revoked on {length(tables)} tables")
    } else {
      cli::cli_alert_info(
        "Table privilege kept: {.val {user}} still holds can_delete on
         {remaining} plot{?s}")
    }
  }

  cli::cli_alert_success("{n} plot_access row{?s} updated")
  invisible(n)
}


#' @title Who can delete what
#' @description
#' Reports both layers side by side: the `can_delete` rows in `plot_access`, and
#' who holds the DELETE table privilege. A mismatch is the interesting case - a
#' table privilege with no `can_delete` row is an account that can delete child
#' rows but no plot.
#'
#' @param con A connection to plots_transects.
#' @param tables Character vector of tables to check the privilege on.
#'
#' @return Invisibly a list of two data frames.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' plot_access_delete_rights(con)
#' }
#' @export
plot_access_delete_rights <- function(con, tables = .plot_scope_tables()) {

  .assert_plot_access(con)

  per_user <- DBI::dbGetQuery(con, "
    SELECT db_user,
           count(*)::int                             AS n_plots,
           (count(*) FILTER (WHERE can_delete))::int AS n_delete
      FROM plot_access
     GROUP BY db_user
     HAVING count(*) FILTER (WHERE can_delete) > 0
     ORDER BY 3 DESC, 1")

  cli::cli_h2("plot_access rows with can_delete")
  if (nrow(per_user) == 0) cli::cli_alert_success("No account holds can_delete")
  else print(per_user, row.names = FALSE)

  privs <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT c.relname AS table_name,
           pg_get_userbyid(a.grantee) AS grantee
      FROM pg_class c
           CROSS JOIN LATERAL
             aclexplode(COALESCE(c.relacl, acldefault('r', c.relowner))) a
      JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public'
       AND c.relname IN ({tables*})
       AND a.privilege_type = 'DELETE'
       AND a.grantee <> 0
       AND a.grantee <> c.relowner
     ORDER BY 2, 1", .con = con))

  cli::cli_h2("Accounts holding the DELETE table privilege")
  if (nrow(privs) == 0) cli::cli_alert_success("None but the owner")
  else print(table(grantee = privs$grantee, table_name = privs$table_name))

  invisible(list(can_delete = per_user, table_privilege = privs))
}


#' @keywords internal
#' @noRd
.assert_plot_access <- function(con) {
  stopifnot("Invalid connection" = DBI::dbIsValid(con))
  ok <- DBI::dbGetQuery(con,
    "SELECT to_regclass('public.plot_access') IS NOT NULL AS ok")$ok
  if (!isTRUE(ok)) {
    cli::cli_abort(c(
      "plot_access does not exist on this database.",
      i = "Apply {.file inst/migrations/plot_access_table.R} first."))
  }
  invisible(TRUE)
}


#' @keywords internal
#' @noRd
.plot_access_rows <- function(con, user, ids) {
  if (is.null(ids)) {
    DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT id_liste_plots, can_write, can_delete FROM plot_access
        WHERE db_user = {user} ORDER BY 1", .con = con))
  } else {
    DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT id_liste_plots, can_write, can_delete FROM plot_access
        WHERE db_user = {user}
          AND id_liste_plots = ANY({as.integer(ids)}::integer[])
        ORDER BY 1", .con = con))
  }
}


#' @keywords internal
#' @noRd
.plot_access_set_delete <- function(con, user, ids, value, tables, grant_table) {

  DBI::dbBegin(con)
  ok <- FALSE
  on.exit({
    if (!ok) try(DBI::dbRollback(con), silent = TRUE)
  }, add = TRUE)

  sql <- if (is.null(ids)) {
    glue::glue_sql("UPDATE plot_access SET can_delete = {value}
                     WHERE db_user = {user} AND can_delete <> {value}",
                   .con = con)
  } else {
    glue::glue_sql("UPDATE plot_access SET can_delete = {value}
                     WHERE db_user = {user}
                       AND id_liste_plots = ANY({as.integer(ids)}::integer[])
                       AND can_delete <> {value}", .con = con)
  }
  n <- DBI::dbExecute(con, sql)

  if (grant_table) {
    for (tb in tables) {
      DBI::dbExecute(con, paste0(
        "GRANT DELETE ON ", DBI::dbQuoteIdentifier(con, tb),
        " TO ", DBI::dbQuoteIdentifier(con, user), ";"))
    }
  }

  DBI::dbCommit(con)
  ok <- TRUE
  n
}
