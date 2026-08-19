# Decode OpenForis observation columns and derive flags

Takes the wide observation\_\* columns, pivots to long, joins with the
code list, and maps decoded labels to single-letter flag codes.

## Usage

``` r
.decode_openforis_observations(data, code_list, flag_mapping = NULL)
```
