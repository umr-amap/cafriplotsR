# Shape a freshly downloaded taxon table into the matching backbone

Drops the \`"ZZ auct."\` placeholder rows and derives the level keys the
matching stages join on. Split out from the download so the rule that
decides which taxa exist for the app is one testable thing rather than a
line buried in a Shiny observer.

## Usage

``` r
.shape_backbone(taxa)
```

## Arguments

- taxa:

  Data frame, the collected \`table_taxa\` columns.

## Value

The same data frame, filtered, with \`tax_sp_level\`, \`tax_gen_level\`,
\`tax_fam_level\` and \`tax_class_level\` added.
