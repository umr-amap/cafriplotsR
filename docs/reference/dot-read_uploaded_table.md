# Read an uploaded table, honouring the chosen sheet

Falls back to the first sheet when \`sheet\` is missing or does not
belong to the workbook, so a stale selection left over from a previous
upload cannot error out.

## Usage

``` r
.read_uploaded_table(file_info, sheet = NULL, guess_max = 5000)
```

## Arguments

- file_info:

  One row of a \`shiny::fileInput()\` value (\`name\`, \`datapath\`).

- sheet:

  Sheet name to read. Ignored for csv files.

- guess_max:

  Rows readxl uses to guess column types.

## Value

A data.frame.
