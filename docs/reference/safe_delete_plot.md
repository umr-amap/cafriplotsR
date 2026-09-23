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
logging of each step - Refuses to orphan a child plot (see
`child_plots`)

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
  child_plots = c("stop", "detach", "delete"),
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

- child_plots:

  Character. What to do about plots whose parent is being deleted:
  `"stop"` (default, refuse), `"detach"` (keep them, clear the link) or
  `"delete"` (delete the whole subtree). See the Child plots section.
  Ignored when `delete_plot = FALSE`, and on a database without the plot
  hierarchy columns.

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

## Child plots

A plot can be the parent of another plot - a nested regeneration
inventory inside its 1 ha host, or the block a set of plots tiles. See
`inst/migrations/plot_hierarchy.R`.

The foreign key is `ON DELETE SET NULL`, so deleting a parent clears the
child's `id_parent_plot` and leaves its `parent_relation` behind - which
`chk_plot_parent_relation_paired` then rejects. Without the handling
below, deleting a parent aborts on a constraint nobody has heard of
instead of saying "this plot has a child". `child_plots` decides what
happens instead:

- `"stop"`:

  (default) Refuse while any child exists outside the deletion set, and
  name the children.

- `"detach"`:

  Keep the children, clearing both `id_parent_plot` and
  `parent_relation`. The link is lost; the plots and their data are not.

- `"delete"`:

  Add every descendant to the deletion set and delete the whole subtree.
  Shown in the dry-run and in the confirmation prompt before anything
  happens.

Children that are already in `plot_ids` are never a problem: their
parent link is cleared before the plots are removed, in every mode.

This section is a no-op on a database where the hierarchy migration has
not been applied.

## See also

\[check_plot_hierarchy_consistency()\] for the state of the parent links
themselves.

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

# A plot with a nested regeneration inventory attached: keep the child,
# drop the link
safe_delete_plot(plot_ids = 123, child_plots = "detach")

# ... or take the whole subtree with it
safe_delete_plot(plot_ids = 123, child_plots = "delete")
} # }
```
