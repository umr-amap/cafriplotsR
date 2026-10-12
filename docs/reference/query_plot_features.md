# Query subplot features with improved architecture

Retrieves subplot features (measurements at plot level) with optional
aggregation and enrichment with subplot observation features.

## Usage

``` r
query_plot_features(
  plot_ids = NULL,
  subplot_ids = NULL,
  subplot_type = NULL,
  format = c("wide", "long"),
  include_subplot_obs_features = FALSE,
  con = NULL
)
```

## Arguments

- plot_ids:

  Vector of plot IDs

- subplot_ids:

  Vector of subplot IDs (optional)

- subplot_type:

  Filter by subplot type (e.g., "census")

- format:

  Output format: "wide" (pivoted) or "long" (raw)

- include_subplot_obs_features:

  Include subplot observation features (rows of \`data_subplot_feat\`,
  e.g. the people of a census). With \`format = "wide"\` they are
  pivoted into \`obs\_\*\` columns of \`features_raw\`, one cell per
  subplot and type (text pasted, numbers averaged). With \`format =
  "long"\` they are not pivoted: they come back one row per database
  record in \`subplot_obs_features\`, with \`id_subplot_feat\` so a
  record can be updated or deleted.

- con:

  Database connection (optional)

## Value

List with components: - features_raw: Raw subplot features -
features_aggregated: Aggregated features by plot (if format="wide") -
census_info: Census-specific information (if census data present) -
subplot_obs_features: Only with \`format = "long"\` and
\`include_subplot_obs_features = TRUE\`. One row per
\`data_subplot_feat\` record, all its columns as stored, plus the
feature \`type\` and \`valuetype\`, the parent subplot's plot, type and
year, and \`colnam\` (the person's name) for people features.

## Examples

``` r
if (FALSE) { # \dontrun{
# Observation features of one census, one row per record
res <- query_plot_features(subplot_ids = 15063, format = "long",
                           include_subplot_obs_features = TRUE)
res$subplot_obs_features
} # }
```
