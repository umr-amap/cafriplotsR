# Decode flag1_rainfor (trait 19) into mortality_risk_flag rows

Fetches the flag1_rainfor measurements for the given individuals and
turns each single-letter code into a mortality_risk_flag row. Returned
rows have the same column structure as the text-derived ones, with
`source_phrases` prefixed by `"flag1_rainfor: "`.

## Usage

``` r
.derive_mortality_from_flag1(individual_ids, flag_trait_id, con)
```
