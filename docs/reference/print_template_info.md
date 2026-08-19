# Print Template Column Information

Display detailed information about template columns including
descriptions and validation rules pulled from the database

## Usage

``` r
print_template_info(
  template_type = c("permanent_plot", "transect", "minimal", "full")
)
```

## Arguments

- template_type:

  Character: Type of template (see \[get_plot_metadata_template()\])

## Value

Invisibly returns a data frame with column information

## Examples

``` r
if (FALSE) { # \dontrun{
# Show information about permanent plot template
print_template_info("permanent_plot")

# Show minimal template info
print_template_info("minimal")
} # }
```
