# Readable plot labels for a set of plot ids

A message about a tag that could not be matched is of little use without
the plot it was looked for in. Names are taken from the prepared data
when it carries them and from the database otherwise; an id that
resolves to no name is labelled \`#\<id\>\` rather than left as \`NA\`.

## Usage

``` r
.plot_labels_for_ids(con, plot_ids, data = NULL)
```

## Arguments

- con:

  Database connection, or NULL to skip the database lookup.

- plot_ids:

  Vector of \`id_liste_plots\` values.

- data:

  Optional prepared data frame, used first when it holds both
  \`plot_name\` and \`id_liste_plots\`.

## Value

Named character vector of labels, indexed by plot id as character.
