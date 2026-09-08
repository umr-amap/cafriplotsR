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
| `reference_plot_linktype.R` | added `linktypelist.scope`, seeded the `reference_plot` type, backfilled `id_linktype`, added `fk_id_liste_plots` | `scope` present, `reference_plot` at priority 10 / scope plot, `fk_id_liste_plots` in `pg_constraint` |
| `reference_plot_mistyped_links.R` | retyped as `referenced_individual` the 443 links the previous migration mistyped | no `reference_plot` row carries an `id_n`; 74 remain, every one with a plot; `type` and `id_linktype` agree on every link |
| `add_plot_citations.R` | added `id_citation` (FK to `table_citations`) to `data_liste_plots` | `check_plot_citations_migration()` reports `id_citation` present, migration complete |
| `plot_hierarchy.R` | added `data_liste_plots.id_parent_plot` and `parent_relation`, with four constraints | `check_plot_hierarchy_migration()` reports both columns, all four constraints, migration complete |

## `plot_hierarchy.R`: the parent link

Applied 2026-09-08, both phases, verified by
`check_plot_hierarchy_migration()`: both columns present, all four constraints
(`fk_data_liste_plots_id_parent_plot`, `chk_plot_not_own_parent`,
`chk_plot_parent_relation`, `chk_plot_parent_relation_paired`) in
`pg_constraint`, and no plot carrying a parent yet — which is the expected
state, since the columns are populated by import, not by the migration.

It exists for nested regeneration inventories: 3 quadrats of an existing 1 ha
plot in which all stems 2-10 cm are monitored. Those are imported as their own
`data_liste_plots` records with their own method, because a plot must stay
protocol-homogeneous — every aggregation in the package assumes it — and
because the small-stem tag series is separate from the parent's and would
collide with it. `id_parent_plot` is what then records that the two are the
same piece of ground.

The second column is the point. `table_taxa.id_parent` needs no companion
because `tax_level` already says what each node is; plots have no such ladder,
and the arithmetic inverts between the two relations a plot edge can mean:
children of a `block_member` parent tile it and may be summed, children of a
`nested_subsample` parent overlap it and may not. A bare parent column would
invite traversal while withholding that. Hence
`chk_plot_parent_relation_paired`, which refuses a parent without a relation.

`linktypelist` was the obvious host for the vocabulary and was rejected: its
`scope` carries `CHECK (scope IN ('individual', 'plot'))`, so a third value
means constraint surgery on the table governing specimen links, and its
`priority` column means "which specimen governs a determination", which is
meaningless here. A closed VARCHAR with a CHECK is the same shape
`reference_plot_linktype.R` used for `scope` itself.

Two nullable columns, no backfill. All ~2,166 plots keep `id_parent_plot IS
NULL`, and the package works either way: `.has_plot_hierarchy()` gates the
import wizard's parent-plot column on the columns being present, so an
unmigrated database simply never offers it. Migration and code can be applied
in either order.

Two things the migration could not carry, both since added to `R/` and both
no-ops on an unmigrated database:

- `check_plot_hierarchy_consistency()` (`R/plot_hierarchy_consistency.R`) — the
  cycle probe. `chk_plot_not_own_parent` stops A → A; nothing in the schema
  stops A → B → A, because no constraint can see a chain. It also checks the
  things a restored copy might have lost — a dangling parent, a broken pairing,
  a relation outside the vocabulary — and repairs, under `fix = TRUE`, only the
  three with a single sensible outcome. A parent with no relation is not one of
  them: whether the child tiles its parent or overlaps it is not recoverable
  from the data, and guessing corrupts every aggregation over the pair.
- `safe_delete_plot(child_plots = )`. `ON DELETE SET NULL` mirrors
  `fk_table_taxa_id_parent`, but here it cannot even orphan cleanly: nulling
  `id_parent_plot` leaves `parent_relation` behind, which
  `chk_plot_parent_relation_paired` rejects, so deleting a parent used to abort
  on a constraint name instead of a sentence. It now defaults to `"stop"` and
  names the children; `"detach"` keeps them and clears both columns; `"delete"`
  takes the subtree. The detach runs as an explicit step before the plot
  delete, in every mode, which also removes any need to order children before
  parents when both are in the same call.

## `reference_plot`: a convention that was never written down

`data_link_specimens` has always had an `id_liste_plots` column beside `id_n`,
and no package code ever wrote it. 74 rows use it. They were written straight
to the table on 2026-01-06 — `id_n` NULL, `id_liste_plots` set, `id_linktype`
NULL, and the legacy free-text `type` column reading `reference_plot`. They
record a specimen collected somewhere inside a plot, where the individual tree
is unknown. Every one is an IRD plot collection.

**Those 74 were not the whole population of the label, and assuming they were
is the mistake this pair of migrations records.** 517 rows carried the string
`reference_plot`, all written the same day in the same session, under one label
meaning two different things. The other 443 have an `id_n` and no plot: one
specimen serving as the identification reference for several trees of one plot
— specimen 39793 for four trees of somalomo002, specimen 39789 for seven of
somalomo004, and so on. That is individual-level data, and it is what
`referenced_individual` already means. `reference_plot_linktype.R` gave them a
plot-scope type while they held an `id_n`, the exact combination
`.check_link_scope()` rejects; `reference_plot_mistyped_links.R` retypes them.
The backfill phase had reported them as anomalies and stamped them anyway — it
now refuses instead, so a restored backup cannot repeat it.

Both migrations ran on 2026-09-01, the correction directly after the mistake.

The mistyping itself moved no determination: priority 10 only outranks a link
whose `id_linktype` is NULL, and no individual holding a mistyped link also held
one of those. The retype to 50 is the step that *can* move a winner — it turns
a loss against an existing `referenced_individual` link into a tie broken by
determination date — which is why `report_reference_plot_mistyped_impact()`
exists, why it runs as the first phase, and why the correction is a separate
migration rather than a quiet `UPDATE`. Read its output before applying on any
restored copy.

Nothing else in the codebase knew about them: `query_all_specimen_links()`
returned the column but no caller read it, and every consumer reached a plot
the long way round, through `id_n → data_individuals.id_table_liste_plots_n`.
A specimen linked only to a plot therefore looked unlinked.

The migration turns the convention into schema:

- `linktypelist.scope` (`'individual'` or `'plot'`) says which of `id_n` and
  `id_liste_plots` a link type fills. Every pre-existing type is
  `'individual'`, which is what they are.
- `reference_plot` becomes a real row, priority 10.
- the 74 rows get their `id_linktype`.
- `id_liste_plots` gets the foreign key it never had. Only `fk_id_n` and
  `fk_linktype` existed, so nothing guaranteed those 74 values pointed at a
  real plot. The phase refuses to add the key while orphans exist rather than
  letting `ALTER TABLE` fail.

Priority 10 is below `referenced_individual` (50) deliberately. Priority orders
the specimen that governs an individual's determination, and every one of those
sorts filters on `id_n`, which a plot link has not got — so the number is inert
there. It is not inert in `mod_link_preview.R`, which preselects the
highest-priority type: a plot-level type must never become the default for
pairing a specimen with a tree.

Package code moved with it, in the same commit: `get_linktypes()` gained a
`scope` argument (and reconstructs the column when the migration has not run,
so the package works either way), the two linking Shiny modules ask for
individual-level types only, `.add_link_specimens()` validates each link
against its type's scope instead of demanding an `id_n` from every one, and
`safe_delete_plot()` now clears plot-level links — under the new foreign key
they would otherwise block the plot deletion.

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
