# Annotate feature records with how the extracted column aggregates them

Mirrors the extraction path: numeric features are averaged
(\`aggregate_numeric_plot_features()\`,
\`aggregate_numeric_features_dt()\`), character and table-referenced
features are concatenated over unique values.

## Usage

``` r
.upd_annotate_aggregation(records)
```
