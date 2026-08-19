# Validate an output style configuration

Checks that the fields of an output style configuration have the
expected types and shapes. Used internally by \[output_style()\] and by
\[query_plots()\] when a user passes a raw list. Exported so power users
can pre-validate a config before passing it on.

## Usage

``` r
validate_output_style(config)
```

## Arguments

- config:

  A list (or \`plot_output_style\` object) holding the configuration
  fields documented in \[output_style()\].

## Value

The validated \`config\`, invisibly. Throws an error if any field is
malformed and a warning when \`additional_tables\` contains values that
are not currently recognised by the package.
