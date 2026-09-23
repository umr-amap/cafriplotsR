# Everything the app needs to show and edit one plot's parent link

One call, because the section is rendered as a unit: where this plot
sits, what sits under it, and which plots it may be attached to.

## Usage

``` r
.upd_plot_link(id, con)
```

## Arguments

- id:

  Integer, \`data_liste_plots.id_liste_plots\`.

- con:

  A DBI connection to the main database.

## Value

A list with:

- available:

  \`FALSE\` when the hierarchy migration has not run, in which case
  every other element is empty and the app hides the section.

- id_parent_plot, parent_name, parent_relation:

  The current link, \`NA\` when the plot has no parent.

- chain:

  The ancestor chain, this plot first: \`id_liste_plots\`,
  \`plot_name\`, \`parent_relation\` (how that plot sits in \*its\*
  parent), \`depth\`.

- children:

  Direct children: \`id_liste_plots\`, \`plot_name\`,
  \`parent_relation\`.

- candidates:

  Named character vector, plot name -\> id, of the plots this one may be
  attached to.

## Details

The candidate parents deliberately exclude the plot itself and
everything below it. That is the cycle prevention:
\`chk_plot_not_own_parent\` stops A -\> A, but nothing in the schema
stops A -\> B -\> A, so the only reliable moment to refuse one is before
it is offered.
