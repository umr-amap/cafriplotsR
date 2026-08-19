# Check for Duplicate Plots in Database

Detects potential duplicate plots by matching method, country, and
coordinates (rounded to 3 decimal places ~111m). Helps prevent
re-importing existing plots with different names (e.g., "FND32" vs
"Releve32").

## Usage

``` r
.check_duplicate_plots(data, con)
```

## Arguments

- data:

  Data frame with plot data (must have method, country, ddlat, ddlon)

- con:

  Database connection

## Value

List with warnings and errors
