# Extract Collector and Number from Herbarium String (Internal Helper)

Parses a string like "Dauby 1234" into collector name and number.

## Usage

``` r
.extract_collector_and_number(herbarium_string)
```

## Arguments

- herbarium_string:

  Character string with collector and number

## Value

List with collector and number, or NULL if parsing fails
