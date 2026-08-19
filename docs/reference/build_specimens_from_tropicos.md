# Build a specimens-shaped table from a Tropicos specimen export

Converts a Tropicos specimen export/search-results CSV (e.g. as
downloaded from a Tropicos search – see
\`inst/docs/example_tropicos.csv\` for the expected layout; this is a
\*\*different\*\* column set from the bulk upload template used by
\[build_tropicos_upload_table()\]) into a tibble shaped like
\`query_specimens()\` output, ready for review and taxon/collector ID
resolution before \[add_specimens()\].

## Usage

``` r
build_specimens_from_tropicos(tropicos_data, con = NULL)
```

## Arguments

- tropicos_data:

  Data frame/tibble already read from a Tropicos export CSV (e.g. via
  \`readr::read_csv()\` or \`read.csv()\`), with the same columns as
  \`inst/docs/example_tropicos.csv\`.

- con:

  Database connection to the main database, used to resolve
  \`id_colnam\` via \`SeniorCollectorPersonID\`. If NULL, calls
  \[call.mydb()\].

## Value

A tibble with one row per input record: \`colnam\`, \`id_colnam\`,
\`colnbr\`, \`suffix\`, \`ddlat\`, \`ddlon\`, \`country\`, \`locality\`,
\`detby\`, \`detd\`, \`detm\`, \`dety\`, \`add_col\`, \`cold\`,
\`colm\`, \`coly\`, \`detvalue\`, \`description\`, \`idtax_n\`,
\`idtax_f\`, \`tax_gen\`, \`tax_esp\`, \`tax_fam\`, \`tax_infra_level\`,
\`tax_infra\`, \`id_tropicos_name\`, \`id_tropicos_specimen\`.

## Column mapping and known gaps

\`colnbr\`/\`suffix\` are parsed from \`CollectionNumber\` (leading
digits / trailing letters); if a value doesn't match that pattern,
\`colnbr\` keeps the raw string and \`suffix\` is \`NA\`.
\`colnam\`/\`add_col\` are parsed from \`CollectorString\` by splitting
on the first comma – the senior collector before it, the rest (if any)
after – which is the exact inverse of how
\[build_tropicos_upload_table()\] builds \`CollectorString\` from
\`colnam\` + \`add_col\`. If \`CollectorString\` isn't present,
\`SeniorCollector\` (\`"Lastname, Firstname"\`) is reformatted to
\`"Firstname Lastname"\` instead, and \`add_col\` is left \`NA\`.

\`id_colnam\` is resolved precisely via \`SeniorCollectorPersonID\` -\>
\`table_colnam.id_tropicos_person\` when \`con\` is supplied – the exact
inverse of how \`build_tropicos_upload_table()\` fills
\`SeniorCollectorPersonID\` from that same column. Collectors not yet
backfilled with an \`id_tropicos_person\` are left with \`id_colnam =
NA\` and \`colnam\` holds the raw parsed name for you to resolve
manually (e.g. via \`.link_colnam()\`) or by running
\`match_tropicos_person_ids()\` / \`apply_tropicos_person_ids()\`
(\`inst/scripts/tropicos_collector_matching_and_export.R\`) against an
updated MBG collector spreadsheet first, then re-running this function.

\`tax_gen\`/\`tax_esp\`/\`tax_fam\`/\`tax_infra\` map directly from
\`Genus\`/\`Species\`/\`FamilyName\`/\`Subspecific\`.
\`tax_infra_level\` uses Tropicos' own pre-built \`NameNoAuthors\` when
\`Species\` is present (\`NA\` for genus-only records), matching this
package's own convention (see \`.format_taxa_names()\`).

\`id_tropicos_name\` is the taxon's Tropicos Name ID, mapped directly
from \`NameID\`. \*\*It is not the \`specimens.id_tropicos\` column\*\*,
which holds the Tropicos \*collection\* ID (one per gathering event) – a
different namespace. Do not map it onto \`id_tropicos\` when calling
\[add_specimens()\]; use it to resolve the name against this database's
backbone instead.

\*\*\`idtax_n\`/\`idtax_f\` are always \`NA\`\*\* – resolving a Tropicos
name to this database's taxon backbone (\`table_taxa\`) is a separate
step; use the matching tools in \`R/taxonomic_query_functions.R\` (e.g.
\`match_tax()\`) against
\`tax_gen\`/\`tax_esp\`/\`tax_fam\`/\`tax_infra_level\`, and
\`add_entry_taxa()\` for names genuinely not yet in the database.
\`id_specimen\` is always \`NA\` (these are new records). \`detvalue\`
has no Tropicos equivalent and is always \`NA\`. \`detd\`/\`detm\` are
only populated if the export includes
\`DeterminationDay\`/\`DeterminationMonth\` columns –
\`example_tropicos.csv\` doesn't, only \`DeterminationYear\`.

An extra \`id_tropicos_specimen\` column (Tropicos' own \`SpecimenID\`,
not a \`specimens\` table column) is included for your own
traceability/audit – \[add_specimens()\] only keeps whatever columns you
explicitly map, so it's safe to leave in and simply not map. Note that
\`SpecimenID\` identifies one physical herbarium sheet, so several rows
of a Tropicos export – one per institution holding a duplicate – can
correspond to a single row of \`specimens\`; deduplicate on
\`colnbr\`/\`suffix\` before comparing with the database (see
\`inst/scripts/pird_collection_status.R\`).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
tropicos_data <- readr::read_csv("inst/docs/example_tropicos.csv")
specimens_staging <- build_specimens_from_tropicos(tropicos_data, con)
} # }
```
