# print table as html in viewer

print table as html in viewer reordered

## Usage

``` r
print_table(res_print)
```

## Arguments

- res_print:

  tibble

## Value

print html in viewer; invisible NULL inside a Shiny session

## Details

Printing an HTML widget navigates the RStudio Viewer pane away from
whatever it is showing. When a Shiny app is running there, that closes
its websocket and kills the session mid-query. The preview is therefore
skipped when this is called from inside a Shiny session – the caller
still gets its return value, it just does not steal the Viewer.

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
