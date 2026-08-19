# Generate Sequential Tags Per Plot (Internal)

Automatically generates sequential tag numbers (1 to n) for each plot
when tags are missing. Each plot gets its own sequence starting from 1.

## Usage

``` r
.generate_sequential_tags(data)
```

## Arguments

- data:

  Data frame with plot_name column and optionally a tag column

## Value

Data frame with tag column populated
