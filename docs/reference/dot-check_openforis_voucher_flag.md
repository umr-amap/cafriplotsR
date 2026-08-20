# Warn when the voucher flag and the recorded voucher numbers disagree

The form asks twice whether a stem was collected: `any_voucher` (code 1
= yes) and the voucher number itself. A stem flagged as collected with
no number, or a number with no flag, means one of the two was missed.

## Usage

``` r
.check_openforis_voucher_flag(trees, specimens)
```

## Arguments

- trees:

  Normalised tree data frame.

- specimens:

  Prepared specimen table, or NULL.

## Value

NULL, invisibly. Called for its messages.
