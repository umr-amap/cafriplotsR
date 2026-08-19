# Get backbone cache directory path

Returns the path to the cache directory for taxonomic backbone storage.
Creates the directory if it doesn't exist. Uses platform-appropriate
cache location via \`tools::R_user_dir()\`.

## Usage

``` r
get_backbone_cache_path()
```

## Value

Character string, full path to cache directory
