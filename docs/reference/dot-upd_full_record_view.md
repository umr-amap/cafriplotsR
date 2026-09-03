# The record as \`query_plots(output_style = "full")\` returns it

The edit form only carries the columns the app can write. To review what
is stored, the app shows the record the way an extraction shows it -
plot metadata for a plot, the individual row for an individual - with
every column the "full" style keeps, features included. An individual is
asked for with every census kept apart, so nothing measured is left out
of the review.

## Usage

``` r
.upd_full_record_view(
  entity = c("plot", "individual"),
  id,
  con,
  con_taxa = NULL
)
```

## Arguments

- entity:

  Either \`"plot"\` or \`"individual"\`.

- id:

  Integer record id (\`id_liste_plots\` or \`id_n\`).

- con:

  A pool or DBI connection to the main database.

- con_taxa:

  Optional pool or connection to the taxa database.

## Value

A tibble with a \`field\` column and one value column per returned row,
or \`NULL\` when the query came back with nothing to show.
