# Clear backbone cache

Deletes the cached taxonomic backbone data and metadata files. This
function is exported for user convenience when they want to force a
fresh download or clear disk space.

## Usage

``` r
delete_backbone_cache()
```

## Value

Logical (invisibly), TRUE if files were deleted, FALSE if no cache
existed

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear the taxonomic backbone cache
delete_backbone_cache()
} # }
```
