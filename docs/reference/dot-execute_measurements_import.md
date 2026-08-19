# Execute individual measurements import

Execute individual measurements import

## Usage

``` r
.execute_measurements_import(data, config, con, dry_run = TRUE, i18n = NULL)
```

## Arguments

- data:

  Data frame with measurement rows (plot_name, tag, traitid, traitvalue,
  traitvalue_char, id_liste_plots, id_sub_plots)

- config:

  Measurement configuration list

- con:

  Database connection

- dry_run:

  Logical

- i18n:

  Translator object

## Value

List with import results
