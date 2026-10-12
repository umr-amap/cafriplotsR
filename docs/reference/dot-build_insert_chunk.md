# Build one multi-row INSERT and its bound parameters

Split out of \[.db_append_table()\] so the placeholder layout can be
tested without a database connection. Takes identifiers already quoted,
and so does no quoting of its own.

## Usage

``` r
.build_insert_chunk(quoted_table, quoted_cols, chunk)
```

## Arguments

- quoted_table:

  Character(1). Already-quoted table identifier.

- quoted_cols:

  Character(1). Already-quoted, comma-separated column list, in the same
  order as the columns of \`chunk\`.

- chunk:

  A data frame of at least one row.

## Value

A list with \`sql\` and \`params\`.

## Details

Placeholders are laid out \*\*column-major\*\*: row \`i\` of column
\`j\` is \`\$((j-1)\*k + i)\` for a chunk of \`k\` rows. That is what
allows \`params\` to be the columns concatenated — one \`as.list()\` per
column — rather than a loop over every cell, which matters when a chunk
holds tens of thousands of values.
