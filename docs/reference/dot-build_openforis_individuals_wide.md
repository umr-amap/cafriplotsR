# Flatten the long measurement table onto the individuals table

The Import Wizard reads a single sheet holding one column per trait, so
the long `measurements` table is pivoted wide and joined to the
individuals. Traits already carried by `individuals` (position_x,
position_y) are skipped rather than duplicated. Character traits
recorded several times for one stem — `observation` and
`observation_flag` — are joined with "; "; a numeric trait with repeated
values keeps the first and warns.

## Usage

``` r
.build_openforis_individuals_wide(individuals, measurements, census = NULL)
```

## Arguments

- individuals:

  Individuals table.

- measurements:

  Long measurement table.

- census:

  Census number written to `census_id`. NULL to omit.

## Value

Data frame with one row per stem and one column per trait.
