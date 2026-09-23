# Read an uploaded delimited text file (CSV, TSV, TXT)

Guesses what a spreadsheet export actually contains rather than assuming
\`read_csv()\` defaults. French-locale Excel writes "CSV" with \`;\`
between fields, \`,\` as decimal mark and Windows-1252 encoding; read as
a plain CSV that came out as a single garbled column.

## Usage

``` r
.read_delimited_upload(path)
```

## Arguments

- path:

  Character path to the file.

## Value

A tibble.
