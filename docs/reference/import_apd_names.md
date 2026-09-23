# Import an African Plant Database export

Replaces the names in `apd_names` with those of an APD export and
records the import in `backbone_import`. Run it at every new export.

Links in `taxa_backbone_link` are kept. Links whose APD ID is absent
from the new export are reported by `check_backbone_links("apd")`, run
at the end.

The export is read as it is sent (tab-separated, Latin-1). APD's
`idtax_good_n` is its own accepted-name pointer, stored as
`apd_accepted_id` and followed whatever `taxon_status` says. Family
names are capitalised as in `table_taxa`, rank prefixes above genus are
removed from `taxon_name`, and the canonical fields (`family`,
`species`, `infra_rank`, `infra_epithet`, `authors`) are derived; the
raw columns are kept.

Requires `inst/migrations/apd_backbone.R` on the taxa database.

## Usage

``` r
import_apd_names(
  file,
  version = NULL,
  source_version = NULL,
  con_taxa = NULL,
  encoding = c("Latin-1", "UTF-8"),
  dry_run = TRUE,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- file:

  Path to the export. Keep it outside the package.

- version:

  Character. Export version, the date the file was created, as
  `"YYYY-MM-DD"`. Default: the file's last-modified date, printed so it
  can be checked; give it explicitly if the file has been edited.

- source_version:

  Character. APD's own release version, as the Conservatoire et Jardin
  botaniques publishes it (`"4.0.0"`), used by \[backbone_citation()\].
  The export carries no such tag, so it has to be read off the APD site
  at download time. Default: the file name, which is provenance rather
  than a version and is not cited.

- con_taxa:

  Connection or pool to the taxa database, with write access. If `NULL`,
  calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- encoding:

  Character. Encoding of the export: `"Latin-1"` (default) or `"UTF-8"`.
  A mismatch is detected and refused.

- dry_run:

  Logical. If `TRUE` (default), read and check the export and report
  what would be imported, without writing.

- force:

  Logical. If `TRUE`, import even if this version is already the current
  one.

- verbose:

  Logical. Show progress. Default `TRUE`.

## Value

Invisibly, a list with `version`, `record_count`, `skipped`, `dry_run`
and `names` (the rows prepared for `apd_names`).

## Examples

``` r
if (FALSE) { # \dontrun{
con_taxa <- call.mydb.taxa()
import_apd_names("path/to/apd_export.txt", con_taxa = con_taxa)  # rehearsal
import_apd_names("path/to/apd_export.txt", version = "2026-07-29",
                 con_taxa = con_taxa, dry_run = FALSE)
} # }
```
