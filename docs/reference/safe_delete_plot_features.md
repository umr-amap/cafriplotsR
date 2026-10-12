# Safely delete plot features

\*\*DANGER: This permanently deletes data from the database!\*\*

Deletes records of \`data_liste_sub_plots\`: the features of a plot,
such as a census, a soil sample or a \`principal_investigator\` (see
\[query_plot_features()\]). \*\*The plot itself is preserved\*\*, as are
its individuals.

A plot feature can have things attached to it, and they are dealt with
before the feature is removed: - its observation features
(\`data_subplot_feat\`, e.g. the people of a census) are deleted with
it - they describe nothing else; - measurements linked to it
(\`data_traits_measures.id_sub_plots\` and
\`data_ind_measures_feat.id_sub_plots\`, i.e. the measurements taken
during a census) are \*\*not\*\* deleted. See \`linked_measurements\`.

\*\*Safety features:\*\* - Dry-run mode to preview what will be
deleted - Refuses to strand census-linked measurements unless told what
to do - Requires explicit confirmation (unless force = TRUE) - Uses a
single database transaction (rollback on error)

## Usage

``` r
safe_delete_plot_features(
  subplot_ids = NULL,
  plot_ids = NULL,
  types = NULL,
  linked_measurements = c("stop", "unlink"),
  con = NULL,
  dry_run = TRUE,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- subplot_ids:

  Integer vector. Specific plot feature record(s) to delete
  (\`id_sub_plots\` from \`data_liste_sub_plots\`). Takes precedence
  over \`plot_ids\`. Typically obtained from
  `query_plot_features(plot_ids = ..., format = "long")$features_raw`.
  Default NULL.

- plot_ids:

  Integer vector. Delete the features of these plot(s)
  (\`id_liste_plots\`). Best combined with \`types\`. Default NULL.

- types:

  Character vector. Restrict to these feature types (\`type\` from
  \`subplotype_list\`, e.g. \`"soil_sample"\`). Default NULL (all
  types).

- linked_measurements:

  Character. What to do when individual measurements are linked to a
  feature being deleted (typically the diameters of a census):
  \`"stop"\` (default) refuses and lists them; \`"unlink"\` keeps the
  measurements and sets their \`id_sub_plots\` to NULL, so they no
  longer belong to any census. To delete them instead, use
  \[safe_delete_individual_features()\] first.

- con:

  Database connection. If NULL, will connect automatically.

- dry_run:

  Logical. If TRUE, shows what would be deleted without deleting.
  Default TRUE for safety.

- force:

  Logical. If TRUE, skips confirmation prompts. Default FALSE. \*\*USE
  WITH EXTREME CAUTION!\*\*

- verbose:

  Logical. Show detailed progress? Default TRUE.

## Value

List with deletion summary (invisible). Includes \`feat_info\`, a data
frame with one row per plot feature to be deleted (\`id_sub_plots\`,
\`id_table_liste_plots\`, \`plot_name\`, \`type\`, \`year\`,
\`typevalue\`, \`typevalue_char\`, \`colnam\`, and the counts
\`n_obs_features\`, \`n_measures\`, \`n_measure_features\` of what is
attached to it), \`blocked\` (TRUE when linked measurements stopped the
deletion), and \`error\`, the database message, when the deletion
failed.

## See also

\[safe_delete_subplot_obs_features()\] to delete only the observation
features of a plot feature, \[safe_delete_plot()\] to delete the whole
plot.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# STEP 1: Extract the features and pick the ones to remove
feats <- query_plot_features(plot_ids = 123, format = "long",
                             con = con)$features_raw
ids_to_remove <- feats$id_sub_plots[feats$type == "soil_sample"]

# STEP 2: Always do a dry-run first!
safe_delete_plot_features(subplot_ids = ids_to_remove, con = con)

# STEP 3: Review output, then delete if sure
safe_delete_plot_features(subplot_ids = ids_to_remove,
                          con = con, dry_run = FALSE)

# Alternative: every soil_sample feature of a plot
safe_delete_plot_features(plot_ids = 123, types = "soil_sample", con = con)

# A census whose measurements must be kept, detached from it
safe_delete_plot_features(subplot_ids = 15063,
                          linked_measurements = "unlink", con = con)
} # }
```
