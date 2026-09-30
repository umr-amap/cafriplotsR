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
#' @param plot_ids Integer vector of `data_liste_plots.id_liste_plots`. Either
#'   this or `plots` is required.
#' @param plots Alternative to `plot_ids`: the result of [query_plots()] (the
#'   whole list, or its `extract` element). Requires `remove_ids = FALSE` in
#'   the [query_plots()] call, since the plot ids are what is needed here.
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
#'   \item{`authors_core`}{One row per person, restricted to `core_roles`.
#'     This is the short invitation list.}
#'   \item{`authors_all`}{One row per person, all roles. Same columns.}
#'   \item{`by_plot`}{The detail behind both: one row per person x plot x role
#'     x source, with `plot_name`, `role`, `source`, `census_number` and
#'     `census_year`. This is what to save alongside a dataset as authorship
#'     metadata.}
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
#'   # All plots of one method, then their co-authors
#'   extract <- query_plots(method = "1ha-IRD", remove_ids = FALSE)
#'   authors <- query_plot_authors(plots = extract)
#'
#'   authors$authors_core   # PI / data manager / team leader only
#'   authors$authors_all    # everyone, additional_people included
#'   authors$by_plot        # who, on which plot, in which role
#'
#'   # Only the people of the censuses, and only those reachable by e-mail
#'   query_plot_authors(
#'     plot_ids = extract$extract$id_liste_plots,
#'     include_plot_features = FALSE,
#'     subplot_type = "census",
#'     require_contact = TRUE
#'   )
#' }
#'
#' @export
query_plot_authors <- function(plot_ids = NULL,
                               plots = NULL,
                               core_roles = c("principal_investigator",
                                              "data_manager",
                                              "team_leader"),
                               include_plot_features = TRUE,
                               include_subplot_features = TRUE,
                               subplot_type = NULL,
                               require_contact = FALSE,
                               con = NULL,
                               verbose = TRUE) {

  plot_ids <- .resolve_author_plot_ids(plot_ids = plot_ids, plots = plots)

  if (!include_plot_features && !include_subplot_features) {
    cli::cli_abort(
      "Nothing to collect: {.arg include_plot_features} and {.arg include_subplot_features} are both {.code FALSE}."
    )
  }

  if (is.null(con)) con <- call.mydb()

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
      nationality, role, source, id_liste_plots, plot_name, census_number,
      census_year, has_contact
    ) %>%
    arrange(family_name, surname, plot_name, role)

  authors_all  <- .summarise_authors(by_plot)
  authors_core <- .summarise_authors(by_plot %>% filter(role %in% core_roles))

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
#' @keywords internal
#' @noRd
.resolve_author_plot_ids <- function(plot_ids, plots) {

  if (!is.null(plot_ids) && !is.null(plots)) {
    cli::cli_alert_info("Both {.arg plot_ids} and {.arg plots} given; {.arg plot_ids} is used")
    plots <- NULL
  }

  if (!is.null(plots)) {

    # query_plots() returns either the list or, when a single component is
    # available, that component directly
    extract <- if (is.data.frame(plots)) {
      plots
    } else if (is.list(plots) && is.data.frame(plots$extract)) {
      plots$extract
    } else {
      cli::cli_abort(c(
        "{.arg plots} is neither a data frame nor a {.fn query_plots} result.",
        i = "Pass the result of {.fn query_plots}, or give {.arg plot_ids} instead."
      ))
    }

    id_col <- intersect(c("id_liste_plots", "id_table_liste_plots"), names(extract))

    if (length(id_col) == 0) {
      cli::cli_abort(c(
        "{.arg plots} carries no plot id column.",
        i = "Call {.fn query_plots} with {.code remove_ids = FALSE} so the ids survive."
      ))
    }

    plot_ids <- extract[[id_col[1]]]
  }

  if (is.null(plot_ids)) {
    cli::cli_abort(c(
      "{.arg plot_ids} or {.arg plots} is required.",
      i = 'Resolve the plots first, e.g. {.code query_plots(method = "1ha-IRD", remove_ids = FALSE)}.'
    ))
  }

  plot_ids <- as.integer(plot_ids)
  plot_ids <- unique(plot_ids[!is.na(plot_ids)])

  if (length(plot_ids) == 0) {
    cli::cli_abort("{.arg plot_ids} resolved to no plot.")
  }

  plot_ids
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
