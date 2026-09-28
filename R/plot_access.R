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


#' @title Does a policy command imply write access?
#' @description
#' Maps the `cmd` column of `pg_policies` onto the `can_write` flag of
#' `plot_access`.
#'
#' UPDATE and DELETE imply write. They also imply read, because a policy's
#' USING expression is evaluated against rows the account must be able to see -
#' which is why `plot_access` has no `can_read` column: a row in it *is* read
#' access, and `can_write` is the escalation.
#'
#' INSERT returns `NA`: insertion on `data_liste_plots` is governed by a single
#' global policy with no plot list, so it contributes nothing to a per-plot
#' grant and the caller skips it.
#'
#' @param cmd Character of length 1. One of `"SELECT"`, `"INSERT"`,
#'   `"UPDATE"`, `"DELETE"`, `"ALL"`.
#'
#' @return `TRUE`, `FALSE`, or `NA` for INSERT. Errors on anything else.
#'
#' @examples
#' .policy_cmd_grants_write("SELECT")
#' .policy_cmd_grants_write("ALL")
#'
#' @keywords internal
#' @export
.policy_cmd_grants_write <- function(cmd) {

  if (length(cmd) != 1L || is.na(cmd)) {
    stop(".policy_cmd_grants_write() takes one command at a time", call. = FALSE)
  }

  switch(
    toupper(trimws(cmd)),
    "SELECT" = FALSE,
    "UPDATE" = TRUE,
    "DELETE" = TRUE,
    "ALL"    = TRUE,
    "INSERT" = NA,
    stop("Unknown policy command: ", cmd, call. = FALSE)
  )
}
