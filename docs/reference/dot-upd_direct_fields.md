# Editable flat columns of the plots or individuals table

Deliberately an allow-list, not everything the schema happens to hold.
\`data_individuals\` and \`data_liste_plots\` both carry deprecated
columns (\`dbh\`, \`code_individu\`, \`sous_plot_name\` and others) that
nothing writes any more; offering them for editing would invite
corrections that change no behaviour, and in \`dbh\`'s case would
silently disagree with the \`stem_diameter\` measurements in
\`data_traits_measures\`.

## Usage

``` r
.upd_direct_fields(entity = c("plot", "individual"), con)
```

## Arguments

- entity:

  \`"plot"\` or \`"individual"\`.

- con:

  A DBI connection.

## Value

A tibble with one row per editable column: \`field\`, \`pg_type\`,
\`kind\` (\`"text"\`, \`"numeric"\`, \`"integer"\`, \`"boolean"\`,
\`"lookup"\`, \`"taxon"\`), and for lookups \`lookup_table\`,
\`lookup_key\`, \`lookup_value\`. Carries a \`hidden\` attribute listing
the omitted schema columns.

## Details

\`get_table_columns()\` is the package's maintained answer to "which
columns of this table does a user actually set", already used by the
import wizard and by \`update_records()\`. This intersects it with the
live schema, so a name it lists that is not a real column (\`plot_name\`
on individuals, which is reached through \`id_table_liste_plots_n\`)
drops out rather than producing a form field that cannot be written.

The friendly lookup names \`method\` and \`country\` are swapped for the
id columns that store them, and flagged \`"lookup"\` so the UI offers a
dropdown. \`idtax_n\` is flagged \`"taxon"\` so it gets a taxonomic
search instead of a raw id box.

Columns the schema has but the allow-list omits are recorded in the
\`hidden\` attribute, so the UI can say what it is not showing.
