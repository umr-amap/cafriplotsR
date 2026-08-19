# Prepare recruit data from OpenForis export

Builds a wide-format tibble with identity columns plus decoded
measurement/observation columns for each recruit.

## Usage

``` r
.prepare_openforis_recruits(
  data,
  specimen_prefix = NULL,
  observation_codes = NULL,
  pom_codes = NULL,
  light_codes = NULL,
  status_codes = NULL
)
```

## Arguments

- data:

  Data frame of recruit rows (already filtered).

- specimen_prefix:

  Prefix for specimen numbers. NULL to skip.

- observation_codes:

  Parsed observation code list data frame, or NULL.

- pom_codes:

  Parsed POM code list data frame, or NULL.

- light_codes:

  Parsed light code list data frame, or NULL.

- status_codes:

  Parsed status code list data frame, or NULL.
