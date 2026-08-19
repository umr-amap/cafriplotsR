# Safely delete plot(s) with all related data

\*\*DANGER: This permanently deletes data from the database!\*\*

This function safely deletes one or more plots and all related data in
the correct order to respect foreign key constraints: 1. Individual
measurement features 2. Trait measurements 3. Individual measurements
(if exists) 4. Individuals 5. Subplot features 6. Subplots 7. Plot
(skipped if `delete_plot = FALSE`)

\*\*Safety features:\*\* - Dry-run mode to preview what will be
deleted - Shows counts of all related data - Requires explicit
confirmation (unless force = TRUE) - Processes plots one-by-one (or in
small batches) to avoid memory crashes with large datasets - Uses
per-batch transactions (rolls back each batch on error) - Detailed
logging of each step

## Usage

``` r
safe_delete_plot(
  plot_ids,
  con = NULL,
  dry_run = TRUE,
  force = FALSE,
  delete_individuals = TRUE,
  delete_subplots = TRUE,
  delete_plot = TRUE,
  plot_batch_size = 1L,
  row_batch_size = 2000L,
  verbose = TRUE
)
```

## Arguments

- plot_ids:

  Integer vector. Plot ID(s) to delete (id_liste_plots)

- con:

  Database connection. If NULL, will connect automatically.

- dry_run:

  Logical. If TRUE, shows what would be deleted without deleting.
  Default TRUE for safety.

- force:

  Logical. If TRUE, skips confirmation prompts. Default FALSE. \*\*USE
  WITH EXTREME CAUTION!\*\*

- delete_individuals:

  Logical. Delete individuals? Default TRUE.

- delete_subplots:

  Logical. Delete subplot features? Default TRUE.

- delete_plot:

  Logical. Delete the plot record itself? Default TRUE. Set to FALSE
  combined with `delete_subplots = FALSE` to remove only individuals and
  their features while preserving all plot metadata (same as
  [`safe_delete_individuals`](https://umr-amap.github.io/cafriplotsR/reference/safe_delete_individuals.md)).

- plot_batch_size:

  Integer. Number of plots processed per iteration. Reduce to 1
  (default) for very large plots to avoid memory/query size issues.

- row_batch_size:

  Integer. Number of rows deleted per SQL statement within each plot
  batch. Default 2000.

- verbose:

  Logical. Show detailed progress? Default TRUE.

## Value

List with deletion summary (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# STEP 1: Always do dry-run first!
safe_delete_plot(plot_ids = 123, dry_run = TRUE)

# STEP 2: Review the output, then delete if sure
safe_delete_plot(plot_ids = 123, dry_run = FALSE)

# Delete multiple plots (processed one by one by default)
safe_delete_plot(plot_ids = c(123, 124, 125))

# Process 5 plots at a time (faster for many small plots)
safe_delete_plot(plot_ids = c(123, 124, 125), plot_batch_size = 5)

# Delete plot but keep individuals (rare)
safe_delete_plot(plot_ids = 123, delete_individuals = FALSE)

# Delete ONLY individuals and their features, keep plot metadata
safe_delete_plot(plot_ids = 123, delete_plot = FALSE, delete_subplots = FALSE)
} # }
```
