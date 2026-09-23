# Link Parent Plot for Import

Resolves the \`parent_plot\` column (a plot_name, or an id_liste_plots
once step 4 of the wizard has matched it) to \`id_parent_plot\`.

## Usage

``` r
.link_parent_plot_for_import(data, con, interactive, dry_run, progress)
```

## Details

Unlike method and country this does not fall back to \`.link_table()\`:
an unmatched parent is an error, not something to resolve interactively.
The parent is an existing plot the user knows by name, so a miss means
either a typo or that the parent has not been imported yet - and
offering a fuzzy menu of 2,000 plot names would invite attaching a plot
to the wrong parent.

No-ops when the plot hierarchy migration has not been applied.
