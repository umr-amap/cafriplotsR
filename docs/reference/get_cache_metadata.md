# Get cache metadata with formatted displays

Retrieves cache metadata including download date, file size, and record
count. Formats age display (e.g., "Today", "3 days ago", "2 weeks ago")
and size display (e.g., "650 KB") for user-friendly presentation.

## Usage

``` r
get_cache_metadata()
```

## Value

List with metadata fields, or NULL if cache unavailable or invalid: -
download_date: Date cache was created - n_records: Number of taxa
records - file_size_bytes: File size in bytes - age_days: Numeric age in
days - age_display: Human-readable age string - size_display:
Human-readable size string - columns: Character vector of column names -
cache_version: Cache format version
