# Launch Taxonomic Backbone Management App

Launches an interactive Shiny application for managing the taxonomic
backbone database. This app provides tools for adding new taxa, updating
existing records, and managing synonymy relationships.

## Usage

``` r
launch_taxo_backbone_app(pool_taxa = NULL, language = c("fr", "en"), ...)
```

## Arguments

- pool_taxa:

  Optional database connection pool for the taxa database. If not
  provided, the app will prompt for login credentials.

- language:

  Character string for UI language. Options:

  - "fr" (French, default)

  - "en" (English)

- ...:

  Additional arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.,
  `launch.browser = TRUE`, `port = 3838`)

## Value

NULL (launches the Shiny app)

## Details

The app provides an intuitive interface for:

**Browse & Search**:

- Search by genus, species, family, order, or taxon ID

- Filter by synonymy status (all/accepted/synonyms)

- View database statistics

- Select taxa for editing

**Add New Taxa**:

- Search Tropicos for taxonomic information

- Manual entry for all taxonomic ranks

- Duplicate checking and validation

- Set synonymy relationships

- Growth form selection

**Update Existing Taxa**:

- Edit all taxonomic fields

- View modification history

- Automatic backup creation

- Change validation

**Synonymy Management**:

- Set taxa as synonyms

- Cancel existing synonymy

- View synonym networks

- Handle synonym cascades

**Hierarchy Viewer**:

- Interactive taxonomic tree

- Statistics by rank

- Export capabilities

## Database Permissions

The app checks user permissions on startup:

- Read Access:

  All authenticated users can browse and search

- Write Access:

  Only users with INSERT privileges on table_taxa can modify data

Users without write permissions will see a "Read-Only Mode" badge and
modification features will be disabled.

## Core Functions

The app wraps existing package functions:

- [`add_entry_taxa`](https://umr-amap.github.io/cafriplotsR/reference/add_entry_taxa.md) -
  Add new taxonomic entries

- [`update_dico_name`](https://umr-amap.github.io/cafriplotsR/reference/update_dico_name.md) -
  Update existing records and manage synonymy

- [`query_taxa`](https://umr-amap.github.io/cafriplotsR/reference/query_taxa.md) -
  Search taxonomic database

## Security

All modifications are:

- Logged in `followup_updates_diconames` table

- Backed up before changes

- Validated for taxonomic consistency

- Subject to database permissions

## See also

[`add_entry_taxa`](https://umr-amap.github.io/cafriplotsR/reference/add_entry_taxa.md)
for adding new taxa programmatically
[`update_dico_name`](https://umr-amap.github.io/cafriplotsR/reference/update_dico_name.md)
for updating taxa programmatically
[`query_taxa`](https://umr-amap.github.io/cafriplotsR/reference/query_taxa.md)
for searching the taxonomic database
[`launch_query_plots_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_query_plots_app.md)
for the plot query app
[`launch_taxonomic_match_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxonomic_match_app.md)
for the taxonomic matching app

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch app with default settings
launch_taxo_backbone_app()

# Launch app in English
launch_taxo_backbone_app(language = "en")

# Launch app in browser on specific port
launch_taxo_backbone_app(launch.browser = TRUE, port = 8080)

# Use existing connection pool
pool <- call.mydb.taxa()
launch_taxo_backbone_app(pool_taxa = pool)
} # }
```
