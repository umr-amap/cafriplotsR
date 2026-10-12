# Pool the per-quadrat metadata of each small-tree plot into one row

`plot.xlsx` describes every quadrat, a plot row holds one value per
column. The rules:

- `plot_area` is the number of quadrats times `quadrat_area`;

- `quadrats` lists the quadrat labels in firsttag order;

- the date is the earliest of the quadrats;

- `team_leader`, `additional_people` and `identified_by` pool the people
  of every quadrat;

- `forest_type` and `forest_state` pool their values;

- any other column keeps its first non-missing value.

A plot whose quadrats disagree on its date or on a column of the last
kind is announced, naming the values found.

## Usage

``` r
.collapse_small_tree_plots(
  per_quadrat,
  quadrats,
  quadrat_area = NULL,
  relation = "nested_subsample"
)
```

## Arguments

- per_quadrat:

  Plot metadata with one row per quadrat, keyed by `quadrat_unit`.

- quadrats:

  Quadrat units (quadrat_unit, plot_name, parent_plot_name, quadrat,
  firsttag).

- quadrat_area:

  Area of one quadrat in hectares, or NULL.

- relation:

  Value written to `parent_relation`.

## Value

Data frame with one row per small-tree plot: plot_name,
parent_plot_name, parent_relation, quadrats, plot_area (when
`quadrat_area` is given), then the pooled metadata columns.
