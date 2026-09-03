# Build a Tropicos bulk-upload table from query_specimens() output

Converts a specimens tibble (as returned by
\`query_specimens(subset_columns = TRUE)\`) into the 31-column layout of
the Tropicos bulk upload "Data" sheet.

## Usage

``` r
build_tropicos_upload_table(
  specimens,
  con = NULL,
  authority = "Madagascar",
  coordinate_method = "GPS",
  elevation_unit = "m",
  elevation_method = "GPS",
  date_language = "French",
  keyword_columns = NULL,
  keyword_sep = "; "
)
```

## Arguments

- specimens:

  Tibble from \`query_specimens(subset_columns = TRUE)\` (or containing
  at least the same columns).

- con:

  Database connection to the main database, used to look up
  \`id_tropicos_person\`. If NULL, calls \[call.mydb()\].

- authority:

  Character or \`NA\` (default \`"Madagascar"\`). Value for the
  \`Authority\` column (a fixed per-submission code in Tropicos' terms,
  not the specimens' actual collection country – override per batch as
  needed).

- coordinate_method:

  Character, recycled to all rows (default \`"GPS"\`, matching every
  example row in the template).

- elevation_unit:

  Character, recycled to all rows (default \`"m"\`, matching every
  example row in the template).

- elevation_method:

  Character, recycled to all rows (default \`"GPS"\`, matching every
  example row in the template).

- date_language:

  Character or \`NA\` (default \`"French"\`), recycled to all rows. The
  template uses this to flag the language of \`DescriptionNote\`;
  there's no way to infer it from the database, so override it per batch
  as needed.

- keyword_columns:

  Character vector of column names in \`specimens\` (default \`NULL\`),
  concatenated row-wise into \`GeneralKeywords\`. Each value is trimmed
  and blank/\`NA\` entries are dropped before joining, so a row missing
  some of the listed columns still gets whatever the others provide (or
  \`NA\` if none do). \`NULL\` leaves \`GeneralKeywords\` blank, as
  before.

- keyword_sep:

  Character, default \`"; "\`. Separator used to join
  \`keyword_columns\` values within a row.

## Value

A tibble with the 31 Tropicos template columns, in template column
order, one row per input specimen.

## Column mapping and known gaps

Most columns map directly or through a light transform (dates split into
day/month/year, \`CollectionNumber\` built as \`colnbr\` + \`suffix\`
and always coerced to character, taxon name built from
\`tax_infra_level\`/\`tax_gen\`, senior collector's Tropicos Person ID
joined from \`table_colnam.id_tropicos_person\`).

If \`SeniorCollectorPersonID\` comes back \`NA\` for collectors you'd
expect to have a Tropicos Person ID, \`table_colnam.id_tropicos_person\`
hasn't been backfilled for them yet – see
\`inst/scripts/tropicos_collector_matching_and_export.R\` for
\`match_tropicos_person_ids()\` (fuzzy-matches an MBG collector
spreadsheet against \`table_colnam\`) and
\`apply_tropicos_person_ids()\` (writes confirmed matches back to the
database). That same script also has \`write_tropicos_upload_table()\`
for saving this function's output to xlsx.

A few columns have \*\*no source in the database at all\*\* and are
always \`NA\`, to be filled in manually: \`DeterminationQualifier\`,
\`DeterminedByPersonID\` (\`detby\` is free text, not linked to
\`table_colnam\`), \`DeterminationInstitution\`, \`LocationID\`,
\`MinimumElevation\` (specimens aren't tied to a plot with elevation),
\`VegetationDescription\`, \`Duplicates\`, \`Institutions\`,
\`OtherCollectorIDs\`. \`GeneralKeywords\` is the same by default, but
can be built from \`specimens\` via \`keyword_columns\` – see below.

\`AuthorityKey\` is also left blank by default for now – the template's
example rows suggest it follows a convention (first initial of the
collector's first name + first three letters of their surname,
lowercase, + collection number + suffix, e.g. "Ehoarn Bidault" + 6362 +
"A" -\> \`"Ebid6362A"\`), but this hasn't been confirmed as the actual
rule yet, so nothing is auto-generated until it is.

\`CollectorString\` is the senior collector's name (\`colnam\`), with
the free-text \`add_col\` ("additional collectors") field from
\`specimens\` appended verbatim if present; it is not parsed into
individual names.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
specimens <- query_specimens(id_colnam = 123, subset_columns = TRUE,
                             show_html = FALSE, con = con)
tropicos_tbl <- build_tropicos_upload_table(specimens, con, authority = "Madagascar")

# Fold locality and free-text description into GeneralKeywords
tropicos_tbl <- build_tropicos_upload_table(
  specimens, con,
  keyword_columns = c("locality", "description")
)
} # }
```
