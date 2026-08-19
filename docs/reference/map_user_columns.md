# Map User Columns to Database Schema

Automatically maps user column names to database column names using: 1.
Exact matching 2. Synonym dictionary (including domain-specific like dbh
= stem_diameter) 3. Fuzzy string matching

## Usage

``` r
map_user_columns(
  user_data,
  config,
  similarity_threshold = 0.6,
  interactive = FALSE
)
```

## Arguments

- user_data:

  Data frame with user columns to map

- config:

  Import configuration from get_import_column_routing()

- similarity_threshold:

  Numeric: minimum similarity for fuzzy matching (0-1). Default: 0.6

- interactive:

  Logical: allow user to review mappings? Default: FALSE

## Value

Named character vector: user_col_name = database_col_name

## Examples

``` r
if (FALSE) { # \dontrun{
# Get config
config <- get_import_column_routing("plots")

# Map columns
user_data <- read.csv("messy_data.csv")
mapping <- map_user_columns(user_data, config)

# Result: c("Plot ID" = "plot_name", "Latitude" = "ddlat", ...)
} # }
```
