# Link Infraspecific Taxa to Species (Step 5 only)

Standalone function to run only Step 5 of the hierarchy linking. Useful
if the full migration timed out on this step.

## Usage

``` r
migration_link_infraspecific(
  con = NULL,
  batch_size = 500,
  dry_run = FALSE,
  verbose = TRUE
)
```

## Arguments

- con:

  Database connection to taxa database

- batch_size:

  Number of records to update per batch (default: 500)

- dry_run:

  If TRUE, only count what would be updated

- verbose:

  If TRUE, show progress

## Value

Number of taxa linked
