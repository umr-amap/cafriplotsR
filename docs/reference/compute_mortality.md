# Compute mortality and recruitment rates

Calculates mortality and recruitment rates between successive censuses
for plots with multiple census records.

## Usage

``` r
compute_mortality(plot_ids = NULL, plot_names = NULL, mindbh = 100, con = NULL)
```

## Arguments

- plot_ids:

  Integer vector. Plot IDs to analyze.

- plot_names:

  Character vector. Plot names to analyze.

- mindbh:

  Numeric. Minimum diameter (mm) for including measurements. Default
  100.

- con:

  Database connection. If NULL, connects automatically.

## Value

A list with: - \`summary\`: Data frame with mortality/recruitment
summary per plot and census interval - \`dead_individuals\`: Data frame
with individuals that died - \`recruits\`: Data frame with newly
recruited individuals

## Examples

``` r
if (FALSE) { # \dontrun{
# Compute mortality for specific plots
mort <- compute_mortality(plot_ids = c(1, 2, 3))

# View summary
mort$summary

# View dead individuals
mort$dead_individuals
} # }
```
