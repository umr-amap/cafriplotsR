# Specimen Import Configuration for Column Mapping (Internal)

Builds a minimal `config` object compatible with
[`map_user_columns`](https://umr-amap.github.io/cafriplotsR/reference/map_user_columns.md)
for the fixed set of specimen database fields. Unlike the
plot/individual import wizard, specimens have a fixed target schema (no
dynamic features), so no database connection is required.

## Usage

``` r
.get_specimen_import_config()
```

## Value

List with `direct_columns` and `import_config` (`column_synonyms`,
`required_columns`).
