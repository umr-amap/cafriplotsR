# People attached to a set of plots, as a co-author list

Collects every person recorded against a set of plots and returns them
as co-author candidates. People reach a plot by two routes, and both are
followed:

- \*\*Plot-level features\*\* - rows of \`data_liste_sub_plots\` whose
  subplot type has \`valuetype == "table_colnam"\`
  (\`principal_investigator\`, \`data_manager\`, \`team_leader\`,
  \`additional_people\`). These describe the plot as a whole.

- \*\*Subplot-observation features\*\* - rows of \`data_subplot_feat\`
  carrying the same people types, attached to a subplot of the plot. In
  practice these are the people of a given census, so the census number
  and year come back with them.

A person recorded twice (say principal investigator of the plot and team
leader of its second census) appears once per record in \`by_plot\`, and
once overall in either \`authors_core\` or \`authors_other\` - never
both.

The person id is read from \`typevalue\` for both routes.
\`typevalue_char\` and \`id_colnam\` are never read: for
\`table_colnam\` features they are empty or wrong.

## Usage

``` r
query_plot_authors(
  plot_name = NULL,
  country = NULL,
  locality_name = NULL,
  method = NULL,
  feature_filters = NULL,
  id_plot = NULL,
  plots = NULL,
  exact_match = TRUE,
  interactive = TRUE,
  core_roles = c("principal_investigator", "data_manager", "team_leader"),
  core_sources = c("plot", "census"),
  include_plot_features = TRUE,
  include_subplot_features = TRUE,
  subplot_type = NULL,
  require_contact = FALSE,
  con = NULL,
  verbose = TRUE
)
```

## Arguments

- plot_name, country, locality_name, method, feature_filters:

  Plot filters, with the meaning they have in \[query_plots()\] - they
  are handed to the same query builder. Matching is exact by default
  here, unlike \[query_plots()\]; see \`exact_match\`.

- id_plot:

  Integer vector of \`data_liste_plots.id_liste_plots\`, when the plots
  are already resolved.

- plots:

  A \[query_plots()\] result to take the plots from, in any of its
  shapes: the styled list (its \`metadata\` table, whose id column is
  \`plot_id\`), the \`"full"\` list (its \`extract\` table), or a bare
  data frame.

- exact_match:

  Logical. Match filter values exactly rather than as substrings.
  \*\*Default \`TRUE\`, unlike \[query_plots()\]\*\*, where it is
  \`FALSE\`. An author list is a claim about a named set of plots, so a
  \`plot_name\` that quietly pulls in the neighbouring plots adds people
  who do not belong on it. Pass \`exact_match = FALSE\` for the
  substring matching \[query_plots()\] does by default - \`plot_name =
  "mbalmayo01"\` then takes every plot whose name contains it.

- interactive:

  Logical. Resolve \`country\` and \`method\` through fuzzy matching
  prompts when they do not match a lookup value. Default \`TRUE\`, as in
  \[query_plots()\].

- core_roles:

  Character vector of the roles that make up the \`core\` output.
  Defaults to \`principal_investigator\`, \`data_manager\` and
  \`team_leader\` - the roles that normally carry an authorship claim,
  leaving out \`additional_people\`.

- core_sources:

  Character vector of the \`source\` values that make up
  \`authors_core\`, or \`NULL\` for no restriction. Defaults to
  \`"plot"\` and \`"census"\` - the plot itself and the events that
  produced its data. People recorded against an ancillary observation, a
  soil sample say, go to \`authors_other\` instead, and stay in
  \`by_plot\`.

  This is a default rather than something read from the database because
  there is nothing in the database to read. \`subplotype_list\` says
  what a subplot type \*is\* (\`type\`, \`valuetype\`, \`category\`),
  not whether running one earns a place on a paper, and that judgement
  differs by study. It sits in the signature, next to \`core_roles\`, so
  changing it is an argument rather than an edit.

- include_plot_features:

  Logical. Follow the plot-level route. Default \`TRUE\`.

- include_subplot_features:

  Logical. Follow the subplot-observation (census) route. Default
  \`TRUE\`.

- subplot_type:

  Character or \`NULL\`. When set, keeps only subplot-observation people
  whose parent subplot is of this type, matched as a regular expression.
  Use \`"census"\` to ignore people attached to any other kind of
  subplot. Default \`NULL\` (keep all).

- require_contact:

  Logical. Drop people with no \`contact\` (no e-mail address), who
  cannot be invited. Default \`FALSE\`, which keeps them and flags them
  with \`has_contact = FALSE\`.

- con:

  A DBI connection or pool to the main database. Opened with
  \[call.mydb()\] if \`NULL\`.

- verbose:

  Logical. Report what was found. Default \`TRUE\`.

## Value

A list of five elements:

- \`authors_core\`:

  One row per person, restricted to \`core_roles\` and \`core_sources\`.
  This is the short invitation list.

- \`authors_other\`:

  One row per person \*\*not\*\* in \`authors_core\`: everyone else
  recorded on the plots - \`additional_people\`, and anyone holding a
  core role only through a source outside \`core_sources\`. Same
  columns. The two tables partition the people, so a person is in
  exactly one of them; someone in \`authors_core\` does not reappear
  here for the non-core roles they also hold. Bind the two for the
  complete list.

- \`by_plot\`:

  The detail behind both: one row per record, with \`plot_name\`,
  \`role\`, \`source\`, \`id_sub_plots\`, \`census_number\` and
  \`census_year\`. This is what to save alongside a dataset as
  authorship metadata.

  \`id_sub_plots\` is the \`data_liste_sub_plots\` row the person hangs
  off, so it feeds straight back into \[query_subplots()\] for the raw
  record. What it points at depends on \`source\`: \*\*the census\*\*
  when \`source == "census"\` (or any other subplot observation), and
  \*\*the people feature row itself\*\* when \`source == "plot"\`, since
  a plot-level feature is its own row of that table. Filter on
  \`source\` before chaining if only one of the two is wanted.

  One row per record, not per person x plot x role: the same person
  recorded twice against one plot in one role is two rows of
  \`data_liste_sub_plots\` with two ids, and both are shown rather than
  collapsed, so a duplicated entry is visible. The author tables are
  unaffected - they are one row per person either way.

- \`roles_found\`:

  Count of people and plots per role, so it is visible which roles
  actually carry data.

- \`plots_without_people\`:

  Ids and names of queried plots that returned nobody - usually a gap in
  the metadata rather than an empty plot.

The two author tables carry \`id_table_colnam\`, \`colnam\`,
\`surname\`, \`family_name\`, \`contact\`, \`institute\`,
\`nationality\`, plus \`roles\`, \`sources\`, \`n_plots\`,
\`plot_names\` and \`has_contact\`.

## See also

\[query_plots()\] to resolve the plots, \[query_colnam()\] to look up a
person, \[subplot_list()\] for the full list of feature types.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Filter the plots here, as in query_plots(). Matching is exact by
  # default, so this is the one plot named mbalmayo010:
  authors <- query_plot_authors(plot_name = "mbalmayo010")
  authors <- query_plot_authors(method = "1ha-IRD")
  authors <- query_plot_authors(country = "Cameroon", method = "1ha-IRD")

  # ... and this is every plot whose name contains "mbalmayo01"
  query_plot_authors(plot_name = "mbalmayo01", exact_match = FALSE)

  authors$authors_core   # PI / data manager / team leader only
  authors$authors_other  # everyone not in authors_core
  dplyr::bind_rows(authors$authors_core, authors$authors_other)  # everyone
  authors$by_plot        # who, on which plot, in which role

  # Back to the raw subplot records behind the census people
  census_ids <- unique(
    authors$by_plot$id_sub_plots[authors$by_plot$source == "census"]
  )
  query_subplots(ids_subplots = census_ids)

  # Or reuse a query_plots() result you already have, in any of its shapes
  extract <- query_plots(method = "1ha-IRD")
  query_plot_authors(plots = extract)

  # Only the people of the censuses, and only those reachable by e-mail
  query_plot_authors(
    method = "1ha-IRD",
    include_plot_features = FALSE,
    subplot_type = "census",
    require_contact = TRUE
  )
} # }
```
