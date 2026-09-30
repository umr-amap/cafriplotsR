# =============================================================================
# AUTHORSHIP / CO-AUTHOR LISTS FOR A SET OF PLOTS
# =============================================================================

#' People attached to a set of plots, as a co-author list
#'
#' @description
#' Collects every person recorded against a set of plots and returns them as
#' co-author candidates. People reach a plot by two routes, and both are
#' followed:
#'
#' \itemize{
#'   \item **Plot-level features** - rows of `data_liste_sub_plots` whose
#'     subplot type has `valuetype == "table_colnam"`
#'     (`principal_investigator`, `data_manager`, `team_leader`,
#'     `additional_people`). These describe the plot as a whole.
#'   \item **Subplot-observation features** - rows of `data_subplot_feat`
#'     carrying the same people types, attached to a subplot of the plot.
#'     In practice these are the people of a given census, so the census
#'     number and year come back with them.
#' }
#'
#' A person recorded twice (say principal investigator of the plot and team
#' leader of its second census) appears once per plot/role/source combination
#' in `by_plot`, and once overall in `authors_all`.
#'
#' The person id is read from `typevalue` for both routes. `typevalue_char`
#' and `id_colnam` are never read: for `table_colnam` features they are empty
#' or wrong.
#'
#' @param plot_name,country,locality_name,method,feature_filters Plot filters,
#'   with the meaning they have in [query_plots()] - they are handed to the
#'   same query builder. Matching is exact by default here, unlike
#'   [query_plots()]; see `exact_match`.
#' @param id_plot Integer vector of `data_liste_plots.id_liste_plots`, when the
#'   plots are already resolved.
#' @param plots A [query_plots()] result to take the plots from, in any of its
#'   shapes: the styled list (its `metadata` table, whose id column is
#'   `plot_id`), the `"full"` list (its `extract` table), or a bare data frame.
#' @param exact_match Logical. Match filter values exactly rather than as
#'   substrings. **Default `TRUE`, unlike [query_plots()]**, where it is
#'   `FALSE`. An author list is a claim about a named set of plots, so a
#'   `plot_name` that quietly pulls in the neighbouring plots adds people who
#'   do not belong on it. Pass `exact_match = FALSE` for the substring
#'   matching [query_plots()] does by default - `plot_name = "mbalmayo01"`
#'   then takes every plot whose name contains it.
#' @param interactive Logical. Resolve `country` and `method` through fuzzy
#'   matching prompts when they do not match a lookup value. Default `TRUE`,
#'   as in [query_plots()].
#' @param core_sources Character vector of the `source` values that make up
#'   `authors_core`, or `NULL` for no restriction. Defaults to `"plot"` and
#'   `"census"` - the plot itself and the events that produced its data.
#'   People recorded against an ancillary observation, a soil sample say,
#'   stay in `authors_all` and in `by_plot` but do not reach the short list.
#'
#'   This is a default rather than something read from the database because
#'   there is nothing in the database to read. `subplotype_list` says what a
#'   subplot type *is* (`type`, `valuetype`, `category`), not whether running
#'   one earns a place on a paper, and that judgement differs by study. It
#'   sits in the signature, next to `core_roles`, so changing it is an
#'   argument rather than an edit.
#' @param core_roles Character vector of the roles that make up the `core`
#'   output. Defaults to `principal_investigator`, `data_manager` and
#'   `team_leader` - the roles that normally carry an authorship claim,
#'   leaving out `additional_people`.
#' @param include_plot_features Logical. Follow the plot-level route.
#'   Default `TRUE`.
#' @param include_subplot_features Logical. Follow the subplot-observation
#'   (census) route. Default `TRUE`.
#' @param subplot_type Character or `NULL`. When set, keeps only
#'   subplot-observation people whose parent subplot is of this type, matched
#'   as a regular expression. Use `"census"` to ignore people attached to any
#'   other kind of subplot. Default `NULL` (keep all).
#' @param require_contact Logical. Drop people with no `contact` (no e-mail
#'   address), who cannot be invited. Default `FALSE`, which keeps them and
#'   flags them with `has_contact = FALSE`.
#' @param con A DBI connection or pool to the main database. Opened with
#'   [call.mydb()] if `NULL`.
#' @param verbose Logical. Report what was found. Default `TRUE`.
#'
#' @return A list of five elements:
#' \describe{
#'   \item{`authors_core`}{One row per person, restricted to `core_roles` and
#'     `core_sources`. This is the short invitation list.}
#'   \item{`authors_all`}{One row per person, all roles. Same columns.}
#'   \item{`by_plot`}{The detail behind both: one row per record, with
#'     `plot_name`, `role`, `source`, `id_sub_plots`, `census_number` and
#'     `census_year`. This is what to save alongside a dataset as authorship
#'     metadata.
#'
#'     `id_sub_plots` is the `data_liste_sub_plots` row the person hangs off,
#'     so it feeds straight back into [query_subplots()] for the raw record.
#'     What it points at depends on `source`: **the census** when
#'     `source == "census"` (or any other subplot observation), and **the
#'     people feature row itself** when `source == "plot"`, since a plot-level
#'     feature is its own row of that table. Filter on `source` before
#'     chaining if only one of the two is wanted.
#'
#'     One row per record, not per person x plot x role: the same person
#'     recorded twice against one plot in one role is two rows of
#'     `data_liste_sub_plots` with two ids, and both are shown rather than
#'     collapsed, so a duplicated entry is visible. The author tables are
#'     unaffected - they are one row per person either way.}
#'   \item{`roles_found`}{Count of people and plots per role, so it is visible
#'     which roles actually carry data.}
#'   \item{`plots_without_people`}{Ids and names of queried plots that
#'     returned nobody - usually a gap in the metadata rather than an empty
#'     plot.}
#' }
#'
#' The two author tables carry `id_table_colnam`, `colnam`, `surname`,
#' `family_name`, `contact`, `institute`, `nationality`, plus `roles`,
#' `sources`, `n_plots`, `plot_names` and `has_contact`.
#'
#' @seealso [query_plots()] to resolve the plots, [query_colnam()] to look up
#'   a person, [subplot_list()] for the full list of feature types.
#'
#' @examples
#' \dontrun{
#'   # Filter the plots here, as in query_plots(). Matching is exact by
#'   # default, so this is the one plot named mbalmayo010:
#'   authors <- query_plot_authors(plot_name = "mbalmayo010")
#'   authors <- query_plot_authors(method = "1ha-IRD")
#'   authors <- query_plot_authors(country = "Cameroon", method = "1ha-IRD")
#'
#'   # ... and this is every plot whose name contains "mbalmayo01"
#'   query_plot_authors(plot_name = "mbalmayo01", exact_match = FALSE)
#'
#'   authors$authors_core   # PI / data manager / team leader only
#'   authors$authors_all    # everyone, additional_people included
#'   authors$by_plot        # who, on which plot, in which role
#'
#'   # Back to the raw subplot records behind the census people
#'   census_ids <- unique(
#'     authors$by_plot$id_sub_plots[authors$by_plot$source == "census"]
#'   )
#'   query_subplots(ids_subplots = census_ids)
#'
#'   # Or reuse a query_plots() result you already have, in any of its shapes
#'   extract <- query_plots(method = "1ha-IRD")
#'   query_plot_authors(plots = extract)
#'
#'   # Only the people of the censuses, and only those reachable by e-mail
#'   query_plot_authors(
#'     method = "1ha-IRD",
#'     include_plot_features = FALSE,
#'     subplot_type = "census",
#'     require_contact = TRUE
#'   )
#' }
#'
#' @export
query_plot_authors <- function(plot_name = NULL,
                               country = NULL,
                               locality_name = NULL,
                               method = NULL,
                               feature_filters = NULL,
                               id_plot = NULL,
                               plots = NULL,
                               exact_match = TRUE,
                               interactive = TRUE,
                               core_roles = c("principal_investigator",
                                              "data_manager",
                                              "team_leader"),
                               core_sources = c("plot", "census"),
                               include_plot_features = TRUE,
                               include_subplot_features = TRUE,
                               subplot_type = NULL,
                               require_contact = FALSE,
                               con = NULL,
                               verbose = TRUE) {

  if (is.null(con)) con <- call.mydb()

  if (!include_plot_features && !include_subplot_features) {
    cli::cli_abort(
      "Nothing to collect: {.arg include_plot_features} and {.arg include_subplot_features} are both {.code FALSE}."
    )
  }

  plot_ids <- .resolve_author_plot_ids(
    id_plot       = id_plot,
    plots         = plots,
    plot_name     = plot_name,
    country       = country,
    locality_name = locality_name,
    method        = method,
    feature_filters = feature_filters,
    exact_match   = exact_match,
    interactive   = interactive,
    con           = con,
    verbose       = verbose
  )

  if (verbose) cli::cli_h2("Collecting people attached to {length(plot_ids)} plot{?s}")

  people_types <- .fetch_people_feature_types(con)

  if (nrow(people_types) == 0) {
    cli::cli_abort(
      "No people feature type found: no row of {.field subplotype_list} has {.code valuetype = 'table_colnam'}."
    )
  }

  unknown_roles <- setdiff(core_roles, people_types$type)
  if (length(unknown_roles) > 0) {
    cli::cli_alert_warning(
      "{.arg core_roles} value{?s} not a people feature type and ignored: {.val {unknown_roles}}."
    )
    cli::cli_alert_info("Available: {.val {people_types$type}}")
  }

  # Both routes give the same columns, so they stack
  collected <- list()

  if (include_plot_features) {
    collected$plot <- .fetch_plot_level_people(plot_ids, con)
  }

  if (include_subplot_features) {
    collected$subplot <- .fetch_subplot_level_people(plot_ids, subplot_type, con)
  }

  by_plot <- bind_rows(collected)

  plot_names <- .fetch_author_plot_names(plot_ids, con)

  if (nrow(by_plot) == 0) {
    if (verbose) cli::cli_alert_warning("No people recorded on any of the queried plots")
    return(.empty_plot_authors(plot_names))
  }

  by_plot <- by_plot %>%
    left_join(.fetch_author_people(unique(by_plot$id_table_colnam), con),
              by = "id_table_colnam") %>%
    left_join(plot_names, by = "id_liste_plots") %>%
    mutate(has_contact = !is.na(contact) & nzchar(trimws(contact)))

  # A person id pointing at no row of table_colnam is a broken reference, not a
  # person: keep it out of the lists but say so, because it is a data problem.
  orphans <- by_plot %>% filter(is.na(colnam))
  if (nrow(orphans) > 0) {
    orphan_ids <- unique(orphans$id_table_colnam)
    cli::cli_alert_warning(
      "Dropped {nrow(orphans)} feature row{?s} whose person id is absent from {.field table_colnam}: {.val {orphan_ids}}"
    )
    by_plot <- by_plot %>% filter(!is.na(colnam))
  }

  if (require_contact) {
    no_contact <- by_plot %>% filter(!has_contact) %>% distinct(colnam)
    if (nrow(no_contact) > 0) {
      cli::cli_alert_info(
        "Dropped {nrow(no_contact)} people with no contact: {.val {no_contact$colnam}}"
      )
    }
    by_plot <- by_plot %>% filter(has_contact)
  }

  by_plot <- by_plot %>%
    distinct() %>%
    select(
      id_table_colnam, colnam, surname, family_name, contact, institute,
      nationality, role, source, id_liste_plots, plot_name, id_sub_plots,
      census_number, census_year, has_contact
    ) %>%
    arrange(family_name, surname, plot_name, role)

  authors_all <- .summarise_authors(by_plot)

  core_rows <- by_plot %>% filter(role %in% core_roles)
  if (!is.null(core_sources)) {
    core_rows <- core_rows %>% filter(source %in% core_sources)
  }
  authors_core <- .summarise_authors(core_rows)

  roles_found <- by_plot %>%
    group_by(role, source) %>%
    summarise(
      n_people = n_distinct(id_table_colnam),
      n_plots  = n_distinct(id_liste_plots),
      .groups  = "drop"
    ) %>%
    arrange(desc(n_people))

  plots_without_people <- plot_names %>%
    filter(!id_liste_plots %in% unique(by_plot$id_liste_plots))

  if (verbose) {
    n_people <- nrow(authors_all)
    n_plots_with <- n_distinct(by_plot$id_liste_plots)
    cli::cli_alert_success(
      "{n_people} people found over {n_plots_with} plot{?s}"
    )
    cli::cli_alert_info(
      "{nrow(authors_core)} of them in the core roles: {.val {intersect(core_roles, people_types$type)}}"
    )

    # An author excluded only by where they were recorded is worth naming:
    # nothing in authors_core says a person was dropped for the route they
    # arrived on rather than for the role they hold
    if (!is.null(core_sources)) {
      dropped <- setdiff(
        by_plot$colnam[by_plot$role %in% core_roles],
        core_rows$colnam
      )
      other_sources <- setdiff(unique(by_plot$source), core_sources)
      if (length(dropped) > 0) {
        cli::cli_alert_info(
          "{length(dropped)} more hold{?s/} a core role only through {.val {other_sources}}, excluded by {.arg core_sources}: {.val {dropped}}"
        )
      }
    }

    if (nrow(plots_without_people) > 0) {
      cli::cli_alert_warning(
        "{nrow(plots_without_people)} queried plot{?s} with nobody recorded: {.val {plots_without_people$plot_name}}"
      )
    }
    n_missing <- sum(!authors_all$has_contact)
    if (n_missing > 0) {
      cli::cli_alert_warning("{n_missing} of them have no contact and cannot be invited")
    }
  }

  list(
    authors_core         = authors_core,
    authors_all          = authors_all,
    by_plot              = by_plot,
    roles_found          = roles_found,
    plots_without_people = plots_without_people
  )
}

# -----------------------------------------------------------------------------
# HELPERS
# -----------------------------------------------------------------------------

#' Resolve the plot ids of query_plot_authors()
#'
#' Three ways in, in this order of precedence: ids given outright, a
#' [query_plots()] result to read them off, or filters to run the same query
#' [query_plots()] would have run.
#'
#' @keywords internal
#' @noRd
.resolve_author_plot_ids <- function(id_plot = NULL,
                                     plots = NULL,
                                     plot_name = NULL,
                                     country = NULL,
                                     locality_name = NULL,
                                     method = NULL,
                                     feature_filters = NULL,
                                     exact_match = FALSE,
                                     interactive = TRUE,
                                     con = NULL,
                                     verbose = TRUE) {

  has_filters <- !is.null(plot_name) || !is.null(country) ||
    !is.null(locality_name) || !is.null(method) || !is.null(feature_filters)

  given <- c(id_plot = !is.null(id_plot), plots = !is.null(plots),
             filters = has_filters)

  if (sum(given) == 0) {
    cli::cli_abort(c(
      "No plots named.",
      i = 'Filter them here, e.g. {.code query_plot_authors(plot_name = "mbalmayo01")} or {.code query_plot_authors(method = "1ha-IRD")}.',
      i = "Or pass {.arg id_plot}, or a {.fn query_plots} result as {.arg plots}."
    ))
  }

  if (sum(given) > 1 && verbose) {
    cli::cli_alert_info(
      "{.arg {names(given)[given]}} all given; {.arg {names(given)[given][1]}} is used"
    )
  }

  plot_ids <- if (!is.null(id_plot)) {
    id_plot
  } else if (!is.null(plots)) {
    .plot_ids_from_query_result(plots)
  } else {
    .plot_ids_from_filters(
      plot_name = plot_name, country = country,
      locality_name = locality_name, method = method,
      feature_filters = feature_filters, exact_match = exact_match,
      interactive = interactive, con = con
    )
  }

  plot_ids <- suppressWarnings(as.integer(plot_ids))
  plot_ids <- unique(plot_ids[!is.na(plot_ids)])

  if (length(plot_ids) == 0) {
    cli::cli_abort("No plot matched.")
  }

  plot_ids
}

#' Read plot ids off any shape of a query_plots() result
#'
#' `query_plots()` returns a styled list whose plot table is `metadata` and
#' whose id column has been renamed `plot_id`; the `"full"` style keeps the
#' internal names, `extract` and `id_liste_plots`; and a result with a single
#' component is returned as that component. All three arrive here.
#'
#' @keywords internal
#' @noRd
.plot_ids_from_query_result <- function(plots) {

  id_cols <- c("plot_id", "id_liste_plots", "id_table_liste_plots")

  # A data frame is either the plot table itself or a single-component result
  frames <- if (is.data.frame(plots)) {
    list(plots)
  } else if (is.list(plots)) {
    # Named tables first, in the order they are likely to hold plots, then
    # anything else the list carries - a style not seen here still resolves.
    # Built as two subscripts rather than one: mixing names and positions in a
    # single `[` coerces the positions to strings, which match nothing.
    named <- intersect(c("metadata", "extract", "meta_data"), names(plots))
    rest  <- setdiff(seq_along(plots), match(named, names(plots)))
    c(plots[named], plots[rest])
  } else {
    cli::cli_abort(c(
      "{.arg plots} is neither a data frame nor a {.fn query_plots} result.",
      i = "Pass what {.fn query_plots} returned, or give {.arg id_plot} instead."
    ))
  }

  for (frame in frames) {
    if (!is.data.frame(frame)) next
    hit <- intersect(id_cols, names(frame))
    if (length(hit) > 0) return(frame[[hit[1]]])
  }

  cli::cli_abort(c(
    "{.arg plots} carries no plot id column.",
    i = "Looked for {.field {id_cols}} in {.field metadata}, {.field extract}, and every other table it holds.",
    i = "Filter the plots here instead, e.g. {.code query_plot_authors(plot_name = ...)}."
  ))
}

#' Run the query_plots() filters to get plot ids
#'
#' Goes through the same query builder as [query_plots()], so `plot_name` and
#' the rest match exactly as they do there, without paying for the individuals,
#' the traits, the taxa connection or the output styling.
#'
#' @keywords internal
#' @noRd
.plot_ids_from_filters <- function(plot_name, country, locality_name, method,
                                   feature_filters, exact_match, interactive,
                                   con) {

  sql <- .plot_filter_query(
    con             = con,
    country         = country,
    plot_name       = plot_name,
    method          = method,
    locality_name   = locality_name,
    feature_filters = feature_filters,
    interactive     = interactive,
    exact_match     = exact_match
  )

  res <- DBI::dbGetQuery(con, sql)

  if (!"id_liste_plots" %in% names(res)) {
    cli::cli_abort("{.field data_liste_plots} returned no {.field id_liste_plots} column.")
  }

  res$id_liste_plots
}

#' Feature types whose value is a person
#' @keywords internal
#' @noRd
.fetch_people_feature_types <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT id_subplotype, type
    FROM subplotype_list
    WHERE valuetype = 'table_colnam'
    ORDER BY type
  ") %>%
    as_tibble()
}

#' People recorded against the plot itself
#' @keywords internal
#' @noRd
.fetch_plot_level_people <- function(plot_ids, con) {

  # plot_ids has been through as.integer(), so pasting them in is safe and
  # keeps the helper callable with a mock connection - the same reason
  # .plot_link_edges() does it this way
  sql <- sprintf("
    SELECT
      sp.id_table_liste_plots AS id_liste_plots,
      sp.id_sub_plots        AS id_sub_plots,
      sp.typevalue           AS id_table_colnam,
      spt.type               AS role
    FROM data_liste_sub_plots sp
    JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
    WHERE spt.valuetype = 'table_colnam'
      AND sp.typevalue IS NOT NULL
      AND sp.id_table_liste_plots IN (%s)
  ", paste(plot_ids, collapse = ","))

  DBI::dbGetQuery(con, sql) %>%
    as_tibble() %>%
    mutate(
      id_table_colnam = as.integer(id_table_colnam),
      id_sub_plots    = as.integer(id_sub_plots),
      source          = "plot",
      census_number   = NA_integer_,
      census_year     = NA_integer_
    )
}

#' People recorded against a subplot observation, typically a census
#' @keywords internal
#' @noRd
.fetch_subplot_level_people <- function(plot_ids, subplot_type, con) {

  sql <- sprintf("
    SELECT
      sp.id_table_liste_plots AS id_liste_plots,
      sp.id_sub_plots         AS id_sub_plots,
      sf.typevalue            AS id_table_colnam,
      spt.type                AS role,
      psp.type                AS source,
      sp.typevalue            AS census_number,
      sp.year                 AS census_year
    FROM data_subplot_feat sf
    JOIN subplotype_list spt ON sf.id_type_sub_plot = spt.id_subplotype
    JOIN data_liste_sub_plots sp ON sf.id_sub_plots = sp.id_sub_plots
    JOIN subplotype_list psp ON sp.id_type_sub_plot = psp.id_subplotype
    WHERE spt.valuetype = 'table_colnam'
      AND sf.typevalue IS NOT NULL
      AND sp.id_table_liste_plots IN (%s)
  ", paste(plot_ids, collapse = ","))

  res <- DBI::dbGetQuery(con, sql) %>% as_tibble()

  if (!is.null(subplot_type) && nrow(res) > 0) {
    res <- res %>% filter(grepl(subplot_type, source, ignore.case = TRUE))
  }

  # census_number and census_year describe a census; on any other parent
  # subplot type they would be that type's own value, which is not a census
  res %>%
    mutate(
      id_table_colnam = as.integer(id_table_colnam),
      id_sub_plots    = as.integer(id_sub_plots),
      census_number   = if_else(source == "census", as.integer(census_number), NA_integer_),
      census_year     = if_else(source == "census", as.integer(census_year), NA_integer_)
    )
}

#' Person details for a set of ids
#' @keywords internal
#' @noRd
.fetch_author_people <- function(ids, con) {

  ids <- as.integer(ids)
  ids <- unique(ids[!is.na(ids)])

  if (length(ids) == 0) {
    return(tibble(
      id_table_colnam = integer(),
      colnam          = character(),
      surname         = character(),
      family_name     = character(),
      contact         = character(),
      institute       = character(),
      nationality     = character()
    ))
  }

  sql <- sprintf("
    SELECT id_table_colnam, colnam, surname, family_name,
           contact, institute, nationality
    FROM table_colnam
    WHERE id_table_colnam IN (%s)
  ", paste(ids, collapse = ","))

  DBI::dbGetQuery(con, sql) %>%
    as_tibble() %>%
    mutate(id_table_colnam = as.integer(id_table_colnam))
}

#' Names of the queried plots
#' @keywords internal
#' @noRd
.fetch_author_plot_names <- function(plot_ids, con) {

  sql <- sprintf("
    SELECT id_liste_plots, plot_name
    FROM data_liste_plots
    WHERE id_liste_plots IN (%s)
  ", paste(plot_ids, collapse = ","))

  DBI::dbGetQuery(con, sql) %>%
    as_tibble() %>%
    mutate(id_liste_plots = as.integer(id_liste_plots))
}

#' Collapse the per-plot detail into one row per person
#' @keywords internal
#' @noRd
.summarise_authors <- function(by_plot) {

  if (nrow(by_plot) == 0) return(.empty_author_table())

  by_plot %>%
    group_by(
      id_table_colnam, colnam, surname, family_name, contact, institute,
      nationality, has_contact
    ) %>%
    summarise(
      roles      = paste(sort(unique(role)), collapse = ", "),
      sources    = paste(sort(unique(source)), collapse = ", "),
      n_plots    = n_distinct(id_liste_plots),
      plot_names = paste(sort(unique(plot_name)), collapse = ", "),
      .groups    = "drop"
    ) %>%
    relocate(has_contact, .after = last_col()) %>%
    arrange(desc(n_plots), family_name, surname)
}

#' Shape of an author table with no rows
#' @keywords internal
#' @noRd
.empty_author_table <- function() {
  tibble(
    id_table_colnam = integer(),
    colnam          = character(),
    surname         = character(),
    family_name     = character(),
    contact         = character(),
    institute       = character(),
    nationality     = character(),
    roles           = character(),
    sources         = character(),
    n_plots         = integer(),
    plot_names      = character(),
    has_contact     = logical()
  )
}

#' Result of query_plot_authors() when nobody is recorded
#' @keywords internal
#' @noRd
.empty_plot_authors <- function(plot_names) {
  list(
    authors_core = .empty_author_table(),
    authors_all  = .empty_author_table(),
    by_plot      = tibble(
      id_table_colnam = integer(),
      colnam          = character(),
      surname         = character(),
      family_name     = character(),
      contact         = character(),
      institute       = character(),
      nationality     = character(),
      role            = character(),
      source          = character(),
      id_liste_plots  = integer(),
      plot_name       = character(),
      id_sub_plots    = integer(),
      census_number   = integer(),
      census_year     = integer(),
      has_contact     = logical()
    ),
    roles_found = tibble(
      role     = character(),
      source   = character(),
      n_people = integer(),
      n_plots  = integer()
    ),
    plots_without_people = plot_names
  )
}
