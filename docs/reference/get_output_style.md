# Retrieve the configuration of an output style

Returns the configuration list backing a built-in output style. The
returned object has class \`"plot_output_style"\` and a dedicated print
method (\[print.plot_output_style()\]) that summarises the fields in a
readable form. Use \[unclass()\] on the result to see the raw list.

## Usage

``` r
get_output_style(name)
```

## Arguments

- name:

  Character scalar. The name of the style to retrieve. Must be one of
  the names returned by \[list_output_styles()\].

## Value

An object of class \`"plot_output_style"\`. Internally a named list
whose fields are documented in \[output_style()\].

## See also

\[list_output_styles()\] for the list of available styles;
\[output_style()\] for the meaning of each field.

## Examples

``` r
# Inspect the default standard style
get_output_style("standard")
#> 
#> ── Output style: standard ──────────────────────────────────────────────────────
#> Standard output for general analysis
#> 
#> 
#> ── Column selection ──
#> 
#> • metadata_columns (15): plot_name, country, locality_name, method, ddlat,
#> ddlon, elevation, first_census, last_census, n_census, id_liste_plots,
#> data_provider, principal_investigator, data_manager, team_leader
#> • individuals_columns (11): id_n, plot_name, tag, quadrat, subplot_name,
#> tax_fam, tax_gen, tax_sp_level, dbh, height, census_date
#> 
#> ── Pattern filters (Perl regex) ──
#> 
#> • remove_patterns (2):
#> `^id_(?!n|liste_plots)`
#> `^date_modif`
#> 
#> ── Column renames ──
#> 
#> • metadata (2):
#> `ddlat` → `latitude`
#> `ddlon` → `longitude`
#> • individuals (3):
#> `tax_fam` → `family`
#> `tax_gen` → `genus`
#> `tax_sp_level` → `species`
#> 
#> ── Flags ──
#> 
#> • keep_common_features: TRUE
#> 
#> Use unclass() to see the raw configuration list.

# Programmatic access to the underlying list
cfg <- unclass(get_output_style("permanent_plot"))
cfg$remove_patterns
#> [1] "^id_(?!n|liste_plots)" "^date_modif"           "_census_\\d+$"        
```
