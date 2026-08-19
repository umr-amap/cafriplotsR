# Map Subplot Feature Columns (Internal Helper)

Maps user column names to database subplot feature types using: 1. Exact
matching 2. Synonym matching (reusing existing .get_column_synonyms())
3. Fuzzy string matching 4. Interactive selection (if enabled)

## Usage

``` r
.map_subplot_feature_columns(
  data,
  plot_id_column,
  column_mapping,
  con,
  interactive,
  similarity_threshold,
  verbose
)
```
