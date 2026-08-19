# Add Growth Forms to Database (Non-Interactive)

Directly inserts growth form measurements without interactive prompts

## Usage

``` r
.add_growth_forms_noninteractive(
  idtax,
  growth_form_selections,
  basisofrecord,
  measurementremarks,
  pool
)
```

## Arguments

- idtax:

  Taxon ID

- growth_form_selections:

  List of growth form paths

- basisofrecord:

  Basis of record

- measurementremarks:

  Measurement remarks

- pool:

  Database connection pool

## Value

NULL (silently adds data)
