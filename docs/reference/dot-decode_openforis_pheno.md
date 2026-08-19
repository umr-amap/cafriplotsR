# Decode OpenForis phenology columns into observation rows

Phenology codes are emitted as `observation` rows, alongside the
`observations[]` codes and free-text comments.

## Usage

``` r
.decode_openforis_pheno(data, code_list)
```

## Arguments

- data:

  Normalised tree data frame (pheno columns renamed `pheno_*`).

- code_list:

  Parsed phenology code list.

## Value

Long-format data frame, or NULL.
