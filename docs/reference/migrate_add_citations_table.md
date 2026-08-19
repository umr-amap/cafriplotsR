# Migration: Add table_citations and id_citation FK to taxa_traits_measures

Creates the \`table_citations\` lookup table in the \`plots_transects\`
database and adds an \`id_citation\` foreign key column to
\`taxa_traits_measures\`.

## Usage

``` r
migrate_add_citations_table(con, dry_run = TRUE)
```

## Arguments

- con:

  Database connection to \`plots_transects\` (must have admin
  privileges)

- dry_run:

  If TRUE, only print SQL without executing (default: TRUE)

## Value

Invisible TRUE on success

## Details

The \`table_citations\` table stores structured bibliographic
information for the datasets and databases that contributed trait
measurements. This is distinct from the \`references\` flat-text column
in \`taxa_traits_measures\`, which records the original source of an
individual measurement (e.g. a herbarium label or field survey). A
citation describes the compiled dataset/database itself (e.g. TRY v6,
BIEN, a curated species list) and is what users should cite when using
trait data.

This migration: 1. Creates \`table_citations\` with structured
bibliographic fields 2. Grants INSERT/UPDATE/SELECT on
\`table_citations\` to PUBLIC 3. Adds \`id_citation\` (FK to
\`table_citations\`) to \`taxa_traits_measures\`

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb(user = "admin", password = "xxx")

# Preview the migration
migrate_add_citations_table(con, dry_run = TRUE)

# Run the migration
migrate_add_citations_table(con, dry_run = FALSE)
} # }
```
