# Resolve parent plot values to id_liste_plots

Step 4 of the import wizard replaces each parent plot name it matched
with its `id_liste_plots`, and leaves a name it was not asked to match
as typed. One column can therefore hold both, so each value is resolved
on its own: a whole number that is an existing `id_liste_plots` is taken
as that id, anything else is looked up by `plot_name`, ignoring case and
surrounding spaces.

## Usage

``` r
.resolve_parent_plot_ids(values, parent_lookup)
```

## Arguments

- values:

  Vector of parent plot values (ids or plot names).

- parent_lookup:

  Data frame with `id_liste_plots` and `plot_name`.

## Value

Integer vector of ids, NA where a value is blank or unknown.
