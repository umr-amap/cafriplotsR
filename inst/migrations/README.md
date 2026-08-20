# Schema migrations (archive)

One-shot changes to the `plots_transects` and `rainbio` databases. Each was run
once against production and is kept here for the record — what was changed, and
how to tell that it happened.

**These files are not part of the package.** They are installed under
`inst/migrations/` but never sourced into the namespace, so nothing here can be
called by accident. That is deliberate: a migration is a thing that happened,
not a function the package offers.

## Status

Verified against production on 2026-08-12 by inspecting the schema, not by
trusting the code.

| Migration | What it changed | Evidence it ran |
|---|---|---|
| `tag_to_numeric.R` | `data_individuals.tag` and its follow-up table from `real` to `numeric` | `information_schema` reports `numeric` |
| `followup_idtax.R` | added `idtax_n`, `idtax_n_new`, `created_by` to `followup_updates_individuals` | `idtax_n_new` present |
| `traitlist_census_link.R` | added `traitlist.census_link`, seeded `'never'` for 28 features | 28 rows `never`, 80 `NULL` |
| `add_citations_table.R` | created `table_citations`, added `id_citation` to `taxa_traits_measures` | both present |
| `add_created_by.R` | added `created_by` to the tables that needed provenance | `data_link_specimens.created_by` present |
| `taxa_hierarchy.R` | added `table_taxa.id_parent` and populated the taxonomic hierarchy | `id_parent` present (**taxa** database) |
| `specimen_links.R` | created `linktypelist`, added `id_linktype` and audit columns to `data_link_specimens` | both present |
| `table_idtax_materialized_view.R` | converted `table_idtax` from a table to a materialized view | `pg_class.relkind = 'm'` |
| `rename_data_d_to_date_d.R` | renamed `data_d` to `date_d` on `data_liste_plots` and `followup_updates_liste_plots` | no `data_d` column remains anywhere in `public` |

## `data_d` → `date_d`: the one that was not backward compatible

Applied 2026-08-20. Both tables renamed in one transaction; 1,252 of 2,166 plot
rows and 1,767 of 2,298 audit rows carried a day, and every one survived
unchanged. No view referenced the column.

The day column had been `data_d` since the database was built — a typo beside
`date_y` and `date_m` — and the codebase had already split around it: the
import templates hand users a `date_d` column and the validation rules are
keyed on `date_d`, while the database, the synonym table, the column
descriptions and `get_table_columns()` said `data_d`. A day crossing from one
side to the other had nowhere to land. Renaming beat adding `date_d` as an
alias, which is how such a split becomes permanent.

Unlike every other migration here it had **no compatible intermediate state**,
so all five package references were renamed in the same commit. Applying it
without that code — or that code without it — breaks the plot import path and
`R/mod_census_information.R`, which names the column in raw SQL. That matters
only for a restored backup now, but it is why the two must move together.

`followup_updates_liste_plots` was included because `backup_direct_records()`
copies by column name: leaving the mirror as `data_d` would have broken every
plot backup insert.

## Why `real` → `numeric` mattered

`tag_to_numeric.R` is the one worth reading if you read only one. PostgreSQL
`real` is single precision: exact for integers only up to 2^24 = 16,777,216.
Tags were nowhere near that ceiling, but the type was also rounding fractional
multi-stem tags. The migration proved the cast lossless over all 400,517 tagged
rows before running, and re-fingerprinted afterwards.

`numeric` was chosen over `double precision` because the cast is clean:
`(22.1::real)::numeric` gives `22.1`, while `::double precision` gives
`22.100000381469727`.

## If one ever has to be run again

It should not — they are one-shot. But a restored backup or a fresh database
might need one:

```r
source(system.file("migrations", "tag_to_numeric.R", package = "CafriplotsR"))
con <- CafriplotsR::call.mydb()

migrate_tag_to_numeric(con)                   # rehearsal: prints, changes nothing
migrate_tag_to_numeric(con, dry_run = FALSE)  # apply
```

Every migration here takes `dry_run` and **defaults it to `TRUE`**. Two of them
(`add_created_by.R` and `table_idtax_materialized_view.R`) used to default to
`FALSE`, so a bare call applied immediately; that was normalised when they were
archived. The materialized-view one drops `table_idtax` and recreates it, so it
takes a backup first and ships with `rollback_table_idtax_migration()`.

A few of these call package internals — `taxa_hierarchy.R` uses
`CafriplotsR:::.add_modif_field()`, `traitlist_census_link.R` uses
`CafriplotsR:::.default_census_link_policy()`. The `:::` is required now that
these files sit outside the namespace.

## What stayed in `R/`

Not everything that touches these schema changes is archived. Still live, and
still exported:

- `check_hierarchy_consistency()` — ongoing consistency of the taxon hierarchy,
  with a `fix` argument. Not a migration check.
- `check_table_idtax_staleness()`, `get_table_idtax_metadata()` — day-to-day
  operation of the materialized view, which needs refreshing.
- `.feature_census_link()`, `.default_census_link_policy()` — the census link
  policy is consulted on every census import, so it belongs in the package. The
  migration only wrote the same rule into `traitlist.census_link`, where the
  database has the last word.

The checks that existed *only* to answer "has this migration run?"
(`check_citations_migration()`, `check_created_by_migration()`,
`verify_hierarchy_integrity()`, `verify_specimen_links_migration()`,
`test_table_idtax_migration()`) came here with the migrations they check.
