# Which unknown tags follow a recognised recruit-tagging convention?

Field teams tag recruits in one of two ways, sometimes both within a
plot: by continuing the plot's numbering (\`600, 601, 602\`), or by
hanging a decimal off the nearest already-tagged neighbour (\`45\` gets
\`45.1\`, then \`45.2\`). Either way the new tag lands a character or
two from a tag already in use, so the typo guard would flag nearly every
genuine recruit. This recognises the conventions so only the tags that
fit neither are held back.

## Usage

``` r
.recruit_tag_exempt(
  value,
  pool_num,
  file_num = numeric(0),
  schemes = c("sequential", "decimal")
)
```

## Arguments

- value:

  Numeric values of the candidate tags.

- pool_num:

  Numeric values of the tags already recorded in that plot.

- file_num:

  Numeric values of the tags for the same plot in the file; the
  whole-numbered ones count as parents too, so a recruit hung off
  another recruit is recognised.

- schemes:

  Character vector of conventions to honour: \`"sequential"\`,
  \`"decimal"\`, or neither.

## Value

Data frame with one row per candidate: \`exempt\` and, where it is
\`TRUE\`, the \`reason\` to record on the row.
