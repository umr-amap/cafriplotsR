# Export plots for citation backfill

Exports \`data_liste_plots\` to a data frame (optionally saved as Excel)
with a blank \`id_citation\` column ready to be filled in manually. Once
filled, pass the result to \`apply_plot_citation_backfill()\`.

## Usage

``` r
export_plots_for_citation_backfill(
  con = NULL,
  file = NULL,
  only_missing = TRUE
)
```

## Arguments

- con:

  Database connection to \`plots_transects\`. If NULL, calls
  \`call.mydb()\`.

- file:

  Path to an \`.xlsx\` file to write. If NULL (default), returns the
  data frame without writing.

- only_missing:

  Logical. If TRUE (default), export only rows where \`id_citation IS
  NULL\`.

## Value

A data frame with columns \`id_liste_plots\`, \`plot_name\`,
\`country\`, \`method\`, \`id_citation\`.

## Details

The export contains only the columns needed to identify each plot and
assign a citation: \`id_liste_plots\`, \`plot_name\`, \`country\`,
\`method\`, and the current \`id_citation\` (NA where unset).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Return as data frame
df <- export_plots_for_citation_backfill(con)

# Write to Excel for manual editing
export_plots_for_citation_backfill(con, file = "plots_to_cite.xlsx")
} # }
```
