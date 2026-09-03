# Wire a sheet selector to a file input and read the selected sheet

Renders the selector as soon as an Excel file is uploaded and returns a
reactive holding the parsed table. The reactive waits until the selector
has caught up with the newly uploaded file, so switching files reads the
table once, not twice.

## Usage

``` r
.xlsx_sheet_server(
  input,
  output,
  session,
  file_id = "xlsx_file",
  i18n = NULL,
  guess_max = 5000
)
```

## Arguments

- input, output, session:

  Module server arguments.

- file_id:

  Input id of the file input, without namespace.

- i18n:

  Reactive returning a translator object, or NULL for no translation.

- guess_max:

  Rows readxl uses to guess column types.

## Value

A reactive returning a data.frame.
