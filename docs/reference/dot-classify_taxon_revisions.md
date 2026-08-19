# Classify the identification revisions carried by a census table

Takes the \`taxon_drift\` a \[split_census_table()\] found and works
out, for each revised stem, what evidence stands behind the
identification it would overwrite — and therefore whether accepting it
should be the default.

## Usage

``` r
.classify_taxon_revisions(
  drift,
  data = NULL,
  evidence = NULL,
  taxa = NULL,
  unidentified_idtax = 351190L
)
```

## Arguments

- drift:

  The \`taxon_drift\` frame from a \`census_split\`.

- data:

  The classified census table (\`split\$data\`), used to find a
  herbarium number recorded against a revised stem in this census.

- evidence:

  Output of \[.fetch_specimen_evidence()\], or \`NULL\`.

- taxa:

  Output of \[.fetch_taxon_names()\], or \`NULL\`.

- unidentified_idtax:

  Taxon id standing for "not identified".

## Value

Data frame, one row per revision, with \`row_id\`, \`id_n\`,
\`plot_name\`, \`tag\`, \`idtax_db\`, \`idtax_file\`, the resolved names
and precisions, \`n_voucher\`, \`n_reference\`, \`herbarium_nbe_char\`,
\`evidence\`, \`category\` and \`decision\` (\`"keep_db"\` or
\`"accept_file"\`).

## Details

Four evidence states, in decreasing weight:

- \`voucher\`:

  a specimen was collected \*\*from this tree\*\*. The revision
  contradicts physical material, so the default is to keep the database
  value and make the user override it deliberately.

- \`collected_this_census\`:

  the uploaded row carries a herbarium number but no link exists yet —
  the specimen is in a press, not in the database. Detectable only from
  the file, which is why this belongs in the wizard and not in a later
  taxonomic pass.

- \`reference\`:

  the determination rested on comparison with another specimen. Revising
  it is ordinary.

- \`field_only\`:

  no herbarium material at all.

Two cases override the evidence entirely, because they are not really
revisions:

- a stem recorded as unidentified that now has a determination is pure
  gain — accepted by default whatever the evidence, and reported as
  \`identification_gained\`;

- a file naming a \*less\* precise taxon than the database — a genus
  where there was a species — is almost always a data entry regression.
  It defaults to keeping the database value even with no evidence at
  all, and is reported as \`precision_lost\`.

## See also

\[split_census_table()\], which produces the drift this consumes.
