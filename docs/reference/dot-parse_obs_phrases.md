# Split free-text observations into atomic phrases

Splits on `;` or `, ` (comma followed by whitespace) to keep decimals
such as `2,5` intact.

## Usage

``` r
.parse_obs_phrases(obs_long)
```
