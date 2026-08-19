# Build a custom output style

Constructs a \`plot_output_style\` object that can be passed directly to
the \`output_style\` argument of \[query_plots()\]. The returned object
lives only in the current R session – it is \*not\* registered or
cached. To reuse it, assign it to a variable, save it with
\[saveRDS()\], or put the constructor call in \`.Rprofile\` / a project
script.

Provide \`based_on\` to inherit from an existing style and override only
the fields you care about. \*\*Override semantics are "replace, not
append"\*\*: any field you pass replaces the parent's value entirely. To
clear a vector field while inheriting the rest, pass an empty vector
(e.g. \`remove_patterns = character()\`); to inherit unchanged, leave
the argument unspecified.

## Usage

``` r
output_style(
  description = NULL,
  metadata_columns = NULL,
  individuals_columns = NULL,
  keep_patterns = NULL,
  remove_patterns = NULL,
  rename_columns = NULL,
  additional_tables = NULL,
  keep_common_features = NULL,
  keep_census_columns = NULL,
  keep_all_features = NULL,
  census_column_renames = NULL,
  based_on = NULL
)
```

## Arguments

- description:

  Character scalar. Short human-readable label.

- metadata_columns:

  Character vector of plot-level columns to keep in the \`\$metadata\`
  table, or the literal string \`"all"\` to keep every plot-level
  column. Required when \`based_on\` is not provided.

- individuals_columns:

  Character vector of columns to keep in the \`\$individuals\` table, or
  \`"all"\`. Required when \`based_on\` is not provided.

- keep_patterns:

  Character vector of Perl-compatible regexes. Any column matching any
  of these is added to the keep list.

- remove_patterns:

  Character vector of Perl-compatible regexes applied \*\*after\*\*
  \`keep_patterns\` to drop columns.

- rename_columns:

  Named list with optional elements \`metadata\` and \`individuals\`,
  each a named character vector \`c(old_name = "new_name")\`.

- additional_tables:

  Character vector of extra tables to attach. Recognised values:
  \`"censuses"\`, \`"height_diameter"\`.

- keep_common_features:

  Logical scalar. If \`TRUE\`, columns starting with \`feat\_\` that are
  non-NA in more than 10 added to the metadata table.

- keep_census_columns:

  Logical scalar. If \`TRUE\` and \`show_multiple_census = TRUE\` in the
  query, every column ending in \`\_census\_\<N\>\` is kept on the
  individuals table.

- keep_all_features:

  Logical scalar. Reserved flag indicating that features should remain
  on the main table rather than being moved to the census table.

- census_column_renames:

  Named character vector mapping \`c(old_prefix = "new_prefix")\`.
  Applied to columns matching \`^\<old_prefix\>\_census\_\d+\$\` when
  census columns are kept.

- based_on:

  Optional. Either the name of a built-in style (e.g.
  \`"permanent_plot"\`) or another \`plot_output_style\` object to
  inherit from. When supplied, missing arguments are inherited from the
  parent.

## Value

A \`plot_output_style\` object: a named list with class
\`"plot_output_style"\` and an attached print method. Use \[unclass()\]
to see the raw list, or pass it straight to \[query_plots()\].

## See also

\[list_output_styles()\], \[get_output_style()\].

## Examples

``` r
# Inherit from "permanent_plot" and drop trait_ columns
my_style <- output_style(
  based_on        = "permanent_plot",
  remove_patterns = c("^id_(?!n|liste_plots)", "^date_modif",
                      "_census_\\d+$", "^trait_")
)
my_style
#> 
#> ── Output style: <custom> ──────────────────────────────────────────────────────
#> Based on: permanent_plot
#> Organized output for permanent plot monitoring (single or most recent census)
#> 
#> 
#> ── Column selection ──
#> 
#> • metadata_columns (18): id_liste_plots, plot_name, country, locality_name,
#> method, ddlat, ddlon, elevation, data_provider, plot_area, forest_description,
#> first_census, last_census, n_census, data_provider, principal_investigator,
#> data_manager, team_leader
#> • individuals_columns (17): id_n, plot_name, tag, quadrat, tax_fam, tax_gen,
#> tax_sp_level, stem_diameter, tree_height, census_date, number_of_stem,
#> traitvalue, traitvalue_char, trait, traitdescription, valuetype, census_date
#> 
#> ── Pattern filters (Perl regex) ──
#> 
#> • keep_patterns (10):
#> `wood_density`
#> `stem_diameter`
#> `observations`
#> `light`
#> `position_`
#> `phenology`
#> `succession_guild`
#> `census_date`
#> `stem_status`
#> `mortality_risk_flag`
#> • remove_patterns (4):
#> `^id_(?!n|liste_plots)`
#> `^date_modif`
#> `_census_\d+$`
#> `^trait_`
#> 
#> ── Column renames ──
#> 
#> • metadata (2):
#> `ddlat` → `latitude`
#> `ddlon` → `longitude`
#> • individuals (6):
#> `tax_fam` → `family`
#> `tax_gen` → `genus`
#> `tax_sp_level` → `species`
#> `stem_diameter` → `dbh`
#> `tree_height` → `height`
#> `height_of_stem_diameter` → `pom`
#> 
#> ── Additional tables ──
#> 
#>   • censuses
#>   • height_diameter
#> 
#> Use unclass() to see the raw configuration list.

# Build a style from scratch
tiny <- output_style(
  description         = "Just IDs and species",
  metadata_columns    = c("plot_name", "country", "id_liste_plots"),
  individuals_columns = c("id_n", "tag", "tax_fam", "tax_gen", "tax_sp_level")
)
```
