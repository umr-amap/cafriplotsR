# Update records with optional single-record comparison display

Update records with optional single-record comparison display

## Usage

``` r
update_records(
  data,
  table_type = c("individuals", "plots", "individual_features", "subplot_features",
    "individual_features_metadata", "methodslist", "table_colnam", "traitlist",
    "taxa_traits_measures", "subplotype_list", "specimens"),
  execute = FALSE,
  method = c("single", "batch"),
  con = NULL,
  interactive = TRUE,
  similarity_threshold = 0.6,
  show_comparison = TRUE
)
```

## Arguments

- data:

  Tibble with records to update. Must include the ID column for the
  table type.

- table_type:

  Character: type of table. One of "individuals", "plots", "specimens",
  "individual_features", "subplot_features",
  "individual_features_metadata", "methodslist", "table_colnam",
  "traitlist", or "subplotype_list"

- execute:

  Logical: if FALSE (default), dry run only - shows what would change

- method:

  Character: "single" (row-by-row) or "batch" (bulk update via temp
  table)

- con:

  Database connection. If NULL, creates new connection

- interactive:

  Logical: enable interactive prompts for metadata matching

- similarity_threshold:

  Numeric: threshold (0-1) for fuzzy matching in metadata mapping

- show_comparison:

  Logical: if TRUE and method="single", display HTML comparison
