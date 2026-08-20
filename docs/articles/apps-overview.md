# The CafriplotsR Apps at a Glance

## Introduction

CafriplotsR ships ten interactive applications. They cover the whole
life cycle of an inventory dataset — exploring it, standardizing its
taxonomy, importing it, correcting it, and linking it to herbarium
specimens — without writing any R code beyond the one line that launches
the app.

Each app opens on the same login screen. What differs is **who may use
it**: three apps can be opened without any credentials, the other seven
need your own account because they write to the database.

## Which app do I need?

| App | Launch with | Access |
|----|----|----|
| Taxonomic name standardization | [`launch_taxonomic_match_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxonomic_match_app.md) | public or account |
| Taxonomic backbone | [`launch_taxo_backbone_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxo_backbone_app.md) | public to browse, account to edit |
| Plot querying | [`launch_query_plots_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_query_plots_app.md) | public or account |
| Plot data import | [`launch_import_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/launch_import_wizard.md) | account |
| Plot features and censuses | [`launch_feature_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/launch_feature_wizard.md) | account |
| Record-by-record corrections | [`launch_data_update_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_data_update_app.md) | account |
| Taxa-level trait import | [`launch_taxa_traits_import()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxa_traits_import.md) | account |
| Herbarium specimen import | [`launch_specimen_import_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/launch_specimen_import_wizard.md) | account |
| Specimen identifications | [`launch_specimen_identification_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_specimen_identification_app.md) | account |
| Individual ↔︎ specimen linking | [`launch_individual_specimen_linking_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_individual_specimen_linking_app.md) | account |

## Working without an account

The login screen of the three read-only apps offers a **Connect as
public user** button. It signs you in through a shared, read-only
account that reaches the taxonomy and the species-level traits. Nothing
you do in that mode can change the database, and the editing controls
are hidden rather than disabled.

This is enough to:

- standardize your own species list against the Central African
  backbone,
- browse the backbone, its synonymy and the traits attached to a taxon,
- explore the inventories that their owners have opened to everyone.

It is **not** enough to import data, correct records, or manage
specimens. Those apps do not show the public button at all, because a
read-only account cannot complete a single one of their workflows.

## Explore and standardize

### Taxonomic name standardization

``` r

launch_taxonomic_match_app()
```

Matches your own list of species names against the Central African plant
taxonomic backbone: automatic matching with fuzzy search, manual review
of whatever the matcher could not resolve on its own, and export of the
standardized list. The result gives each of your names a stable taxon
identifier (`idtax_n`), which is what lets you join traits and
inventories later on.

Start here if you arrive with a species list from your own fieldwork.

### Taxonomic backbone

``` r

launch_taxo_backbone_app()
```

Browses and manages the taxonomic backbone itself: searching taxa,
inspecting synonymy relationships, and generating the R code for the
query you just built by hand. With an account, it also adds new taxa,
updates existing records and maintains synonymy. As a public user you
get the browsing and code-generation half; the editing controls do not
appear.

### Plot querying

``` r

launch_query_plots_app()
```

An interactive front end to
[`query_plots()`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md):
filter plots by country, method or other criteria, see them on a map,
drill down to the individual trees, and export the result. Which plots
you see depends on the account you used — row-level security policies
decide, and a public user sees only the inventories their owners have
opened to everyone.

## Import and update

These apps all require your own account, and they only ever touch the
plots your account is entitled to.

### Plot data import

``` r

launch_import_wizard()
```

The complete import workflow for plots and individual measurements:
upload a file, map its columns to the database fields, validate, preview
what will be written, and execute. It wraps the package’s import
functions, so the checks you would otherwise run by hand are applied for
you.

### Plot features and censuses

``` r

launch_feature_wizard()
```

Adds features to plots that already exist — either a new census, with
its dates and the people involved, or arbitrary plot-level features.

### Record-by-record corrections

``` r

launch_data_update_app()
```

Corrects plot metadata and individual records one at a time. It is the
friendly counterpart to
[`update_records()`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md),
which is more powerful but expects you to already know which table a
value lives in.

### Taxa-level trait import

``` r

launch_taxa_traits_import()
```

Imports trait measurements attached to taxa rather than to individual
stems: upload, map your columns onto the trait list, preview, execute.

## Herbarium specimens

### Specimen import

``` r

launch_specimen_import_wizard()
```

Imports new herbarium specimens from Excel or CSV files.

### Specimen identifications

``` r

launch_specimen_identification_app()
```

Updates the identifications recorded for specimens, wrapping
[`update_ident_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/update_ident_specimens.md).

### Individual ↔︎ specimen linking

``` r

launch_individual_specimen_linking_app()
```

Creates the links between individual trees and herbarium specimens,
using the herbarium information carried in your individuals dataset.

## Getting an account

Public access covers taxonomy and traits. Working with inventory data —
your own or a colleague’s — needs an account, which also determines
which plots you may query and update. Access is granted per user, so get
in touch with the maintainers to have one created.
