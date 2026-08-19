# Extract and Process ALL Subplot Features

Identifies ALL subplot feature columns from the imported data by: 1.
Querying subplot_list() to get all defined subplot features 2. Filtering
to columns present in data that aren't flat table columns 3. Separating
into: - People features (valuetype == "table_colnam") - need linking -
Other features (valuetype != "table_colnam") - direct values 4.
Processing each type appropriately

## Usage

``` r
.extract_and_process_subplot_features(
  data,
  config,
  con,
  interactive,
  dry_run,
  progress
)
```
