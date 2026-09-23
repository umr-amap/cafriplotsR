# Relation choices for the parent-plot select

The stored values are the closed vocabulary the CHECK constraint allows;
the labels say what each one means for arithmetic, which is the whole
reason the column exists and the only thing that makes the choice
decidable.

## Usage

``` r
.upd_relation_choices(i18n)
```

## Arguments

- i18n:

  A resolved \`shiny.i18n\` translator.

## Value

Named character vector, label -\> stored value, blank entry first.
