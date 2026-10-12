# Join the 20 x 20 m quadrat and the 10 x 10 m sub-quadrat of each stem

Join the 20 x 20 m quadrat and the 10 x 10 m sub-quadrat of each stem

## Usage

``` r
.combine_small_tree_quadrat(quadrat, subquadrat = NULL, sep = "_")
```

## Arguments

- quadrat:

  Character. Quadrat label (`"20_20"`).

- subquadrat:

  Character. Sub-quadrat label (`"A"`), or NULL.

- sep:

  Separator.

## Value

Character vector such as `"20_20_A"`; the quadrat alone when the
sub-quadrat is missing, NA when the quadrat is.
