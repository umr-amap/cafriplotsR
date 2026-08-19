# Compute growth rates for permanent plots

Calculates tree growth rates between successive censuses for plots with
multiple census records. The function automatically fetches data from
the database and computes growth metrics.

## Usage

``` r
compute_growth(
  plot_ids = NULL,
  plot_names = NULL,
  mindbh = 100,
  err.limit = 4,
  maxgrow = 75,
  method = c("I", "E"),
  return_individual = TRUE,
  con = NULL
)
```

## Arguments

- plot_ids:

  Integer vector. Plot IDs to analyze.

- plot_names:

  Character vector. Plot names to analyze.

- mindbh:

  Numeric. Minimum diameter (mm) for including measurements. Default
  100.

- err.limit:

  Numeric. Error limit for negative growth detection. Default 4.

- maxgrow:

  Numeric. Maximum valid growth rate (mm/year). Default 75.

- method:

  Character. Growth calculation method: "I" for incremental or "E" for
  exponential.

- return_individual:

  Logical. Whether to return individual-level growth data.

- con:

  Database connection. If NULL, connects automatically.

## Value

A list with: - \`summary\`: Data frame with growth summary per plot and
census interval - \`individuals\`: Data frame with individual growth
rates (if return_individual = TRUE)

## Examples

``` r
if (FALSE) { # \dontrun{
# Compute growth for specific plots by ID
growth <- compute_growth(plot_ids = c(1, 2, 3))

# Compute growth for plots by name
growth <- compute_growth(plot_names = c("plot001", "plot002"))

# Get only summary statistics
growth <- compute_growth(plot_ids = 1, return_individual = FALSE)
} # }
```
