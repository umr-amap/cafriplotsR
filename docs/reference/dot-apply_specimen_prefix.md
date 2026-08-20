# Prefix a voucher number, once

Field teams are inconsistent about whether they type the herbarium
prefix into the form: one campaign records `107`, the next `"Pird 107"`
for the same kind of voucher. Pasting the prefix on unconditionally
turns the second into `"PIRD Pird 107"`, which is a different — and
wrong — herbarium number.

## Usage

``` r
.apply_specimen_prefix(x, prefix)
```

## Arguments

- x:

  Vector of voucher numbers as recorded.

- prefix:

  Prefix to apply, e.g. `"PIRD"`. NULL or empty returns `x` as
  character, untouched.

## Value

Character vector, NA where `x` was NA or blank.

## Details

A prefix already present is therefore stripped before the canonical one
is applied, whatever its case and whatever separator followed it. The
result is the same string either way, spelled the way `prefix` spells
it.
