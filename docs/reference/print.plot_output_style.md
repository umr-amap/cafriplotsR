# Print an output style configuration

Pretty-prints a \`plot_output_style\` object as returned by
\[get_output_style()\] or built by \[output_style()\]. The output groups
fields by purpose: column selection, pattern filters, renames,
additional tables, and flags. Custom styles built with
\[output_style()\] show the parent they were derived from, when any.

## Usage

``` r
# S3 method for class 'plot_output_style'
print(x, ...)
```

## Arguments

- x:

  A \`plot_output_style\` object.

- ...:

  Currently ignored.

## Value

\`x\`, invisibly.
