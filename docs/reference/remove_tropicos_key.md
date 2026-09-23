# Remove the stored Tropicos API key

Deletes \`TROPICOS_API_KEY\` from \`~/.Renviron\` and drops it from the
session cache.

## Usage

``` r
remove_tropicos_key()
```

## Value

\`TRUE\` invisibly if a key was removed, \`FALSE\` otherwise.

## See also

\[setup_tropicos_key()\]

## Examples

``` r
if (FALSE) { # \dontrun{
remove_tropicos_key()
} # }
```
