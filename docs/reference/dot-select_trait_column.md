# Select Trait Column Interactively - Internal

Uses .find_cat() to let user select which trait/feature this represents
from available traits in the database.

## Usage

``` r
.select_trait_column(column_name, con)
```

## Arguments

- column_name:

  Name of the column being mapped

- con:

  Database connection

## Value

Character: selected trait name, or NA if skipped
