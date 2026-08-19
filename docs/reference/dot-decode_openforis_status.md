# Decode OpenForis stem status columns and derive flags

Handles the two-level status system: stem_status + stem_status2 are
concatenated into a combined code, looked up, and mapped to flag1/flag2.

## Usage

``` r
.decode_openforis_status(data, code_list, flag_mapping = NULL)
```
