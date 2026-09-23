# Plots a given plot may be attached to

Every plot except this one and its descendants. Excluding the subtree is
what makes a cycle unreachable from the app.

## Usage

``` r
.upd_plot_parent_choices(id, con)
```

## Arguments

- id:

  Integer plot id.

- con:

  A DBI connection.

## Value

Named character vector, plot name -\> id as character.
