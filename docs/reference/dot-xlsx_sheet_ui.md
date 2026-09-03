# Placeholder for the sheet selector attached to a file input

Put this straight after the \`shiny::fileInput()\` it belongs to, then
wire it up with \[.xlsx_sheet_server()\] using the same \`file_id\`.

## Usage

``` r
.xlsx_sheet_ui(ns, file_id = "xlsx_file")
```

## Arguments

- ns:

  Module namespace function.

- file_id:

  Input id of the file input, without namespace.

## Value

A \`shiny::uiOutput()\`.
