# Attach an individual id to every measurement row

Remeasures resolve against the database, recruits against the ids just
returned by the insert. Both are keyed on plot name plus tag.

## Usage

``` r
.resolve_census_individuals(data, plot_ids, new_individuals, con)
```

## Arguments

- data:

  Measurement rows with \`plot_name\` and \`tag\`.

- plot_ids:

  Integer vector of plot ids in play.

- new_individuals:

  Result of \[.insert_census_recruits()\].

- con:

  Database connection.

## Value

\`data\` with an \`id_data_individuals\` column.
