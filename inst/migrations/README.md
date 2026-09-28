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
| `taxa_hierarchy_backfill.R` | set `tax_level` and `id_parent` on the 25 taxa added from `launch_taxo_backbone_app()` without them; created 2 missing genera (`Kuloa` 367194, `Conchograecum` 367195, `tax_source = 'H_AUT'`) | applied 2026-09-14 (**taxa** database): all 25 `linked`, none left unlinked; `check_unlinked_taxa()` should report `no_level = 0` |
| `multi_backbone.R` | created `backbone_list`, `backbone_import`, `taxa_backbone_link` and `v_backbone_names_wcvp`; copied the WCVP links and import history; legacy WCVP tables untouched apart from two expression indexes on `wcvp_names` | verified 2026-09-15 (**taxa** database): `check_multi_backbone_migration()` passes every check; 313,476 WCVP links copied, none missing, none dangling; foreign key to `table_taxa` on delete cascade; 1,550 taxa with several links and none preferred, left for review |
| `apd_backbone.R` | created the empty `apd_names` and `v_backbone_names_apd`; registered APD in `backbone_list` without offering it to users | verified 2026-09-15 (**taxa** database): `check_apd_backbone_migration()` passes every check; view columns and types identical to WCVP's; 0 names, no import, no links, `is_name_source = false` — the expected state before `import_apd_names()` runs |
| `multi_backbone_followup.R` | no schema change: replaced the legacy `wcvp_idtax_link` with the preferred WCVP links of `taxa_backbone_link` (**taxa** database); keeps the read-only `report_links_without_preferred()` | applied 2026-09-18 after the link rebuild: 313,476 legacy rows deleted, 285,037 inserted — 282,168 taxa unchanged, 1,484 reduced from several links to the preferred one, 1,381 changed, 4 added, 26,861 removed (links the old matcher had guessed, now awaiting review in `taxa_backbone_link`) |
| `fk_indexes_plot_scope.R` | built six indexes, one per table, on the column that reaches a plot: `data_individuals.id_table_liste_plots_n`, `data_liste_sub_plots.id_table_liste_plots`, `data_link_specimens.id_n`, `data_subplot_feat.id_sub_plots`, `data_traits_measures.id_data_individuals`, `data_ind_measures_feat.id_trait_measures`. Four are declared foreign keys — PostgreSQL indexes the referenced side, never the referencing side, so every "rows belonging to these plots" lookup was a seq scan | applied 2026-09-28: `check_fk_indexes_plot_scope(con)` reports all six present and valid. On a 100-plot grant, `data_individuals` filters in **20 ms** (bitmap index scan) and `data_traits_measures` through the individual in **33 ms** (nested loop over an index-only scan), against a pre-index baseline of 75 ms for the seq scan on the denormalised column and 116 ms two-hop. The correct RLS key is now cheaper than the wrong one used to be, which removes the trade-off P4.4 assumed. After `VACUUM (ANALYZE)`, `Heap Fetches` is 0 on both scans. Measured at both ends of the grant range: a 100-plot grant (4.6% of the network) costs 2 ms for the `data_individuals` predicate, 21 ms for the `data_traits_measures` predicate and 159 ms to extract the rows; a 2,000-plot grant (91.2%) costs 46 ms, 306 ms and 1,234 ms. Scaling is linear in rows visible — 20x the plots for ~14x the predicate — with no plan collapse at low selectivity, and the predicate is 12-25% of total query cost. No real account occupies the upper row: `dauby` created 1,988 of the 2,194 plots and owns the tables, so RLS never applies to it |
| `revoke_stray_dml_grants.R` | revokes the direct `INSERT, UPDATE, DELETE` grants held by `CafriP_public`, `user_test3` and `user_test4` and drops their write policies, keeping `SELECT` and the SELECT policies so the public apps are unaffected. P0.2 revoked write `FROM PUBLIC`, which never touched these separate direct grants | run 2026-09-28: **found the target state already reached** — no direct write grant and no write policy for any of the three, where the audit of 2026-09-26 had recorded `DELETE, INSERT, SELECT, UPDATE` on `data_liste_plots` for `CafriP_public` together with `policy_CafriP_public_update` and `policy_CafriP_public_delete` over 83 plots. The end state matches exactly what the fixed `define_read_only_policy()` produces, so it was most likely closed by that; grant changes leave no audit trail, so this cannot be confirmed from the database. Verified independently: `has_table_privilege` false for INSERT/UPDATE/DELETE, only `policy_*_select` remaining. **32 other login roles still hold the same direct write grant on `data_liste_plots`** and were deliberately not touched |
| `delete_orphan_plot_rows.R` | exported to CSV then deleted the rows nothing could attribute to a plot: 890 `data_liste_sub_plots` (23 with `id_table_liste_plots` NULL, 867 naming a plot id absent from `data_liste_plots`), the 139 `data_subplot_feat` rows on them, and 3 `data_traits_measures` rows with `id_data_individuals` NULL. Under RLS all of them would have matched no policy and been invisible to everyone but the owner | applied 2026-09-28. Confirmed indirectly and decisively: `fk_subplot_plot_integrity.R` validated both foreign keys and set `NOT NULL`, neither of which PostgreSQL permits while a violating row remains. CSVs under `%LOCALAPPDATA%\cafri_deleted_rows\<timestamp>\` with a `README.txt`. **The 867 were from 2025, in contiguous plot-id blocks (1998-2000, 2453-2473) of 10 or 44 subplots each — an import that wrote its subplots and lost its plots. The producer was never identified in the code; the foreign key now makes it moot** |
| `fk_subplot_plot_integrity.R` | added the missing foreign keys on `data_liste_sub_plots.id_table_liste_plots` and `data_traits_measures.id_table_liste_plots`, both to `data_liste_plots (id_liste_plots)` `ON DELETE NO ACTION`, and made the first `NOT NULL` | applied 2026-09-28: both constraints report `convalidated = TRUE` with `confdeltype = 'a'`, and `is_nullable = NO`. Proven to bite by `inst/scripts/verify_write_paths.R`, which inserts through the real code inside a rolled-back transaction: a subplot with no plot is refused with a not-null violation, and a subplot or measurement naming a missing plot with a foreign-key violation — each matched on the error text, not on any error |

## Written, not yet applied

Move a row to the table above, with its evidence, once it has run.

| Migration | What it will change | Plan |
|---|---|---|
| `created_by_server_asserted.R` | **apply first — the two below depend on it.** Drops **every** permissive INSERT policy on `data_liste_plots` whose check is only `true` and replaces them with one `insert_own WITH CHECK (created_by = current_user)`. On this database that is `insert_open` **plus a per-account `policy_<user>_insert` for thirteen accounts**, left over from a version of `define_user_policy()` that created them — permissive policies are ORed, so dropping only `insert_open` would achieve nothing for those thirteen. They lose nothing they should have: `insert_own` is `TO PUBLIC`, so they can still insert plots, just not attribute one to another account. Also re-asserts the `DEFAULT current_user` that `add_created_by.R:67` already set, and makes `created_by NOT NULL`. `created_by` is correct today for the import wizard, which omits the column and takes the default — but any client that *names* it may write anything, and once `plot_access` exists a trigger reads `NEW.created_by` to write a grant, so that stops being a lie in an audit column and becomes **one account granting another access to a plot** | `report_created_by_state(con)` first — read-only; prints the column, every INSERT policy, and whether each distinct `created_by` value names a real database role. Then `migrate_created_by_server_asserted(con)` to rehearse, `dry_run = FALSE` to apply, in one transaction. No write path in the package is affected: `created_by` is written in exactly one place in `R/` (`specimen_linking_functions.R:548`) and that targets `data_link_specimens`. Evidence: `check_created_by_server_asserted(con)` reports `insert_open` gone, `insert_own` checking `created_by` against `current_user`, and the default intact |
| `plot_access_table.R` | creates `plot_access` — one row per (account, plot), with `can_write`, `can_delete`, `can_grant`, `origin`, `granted_by`, `granted_on`; `can_delete` is separate from `can_write` because it is the destructive one, it cascades through six child tables, and `safe_delete_plot()` is not atomic; the creator trigger sets it `TRUE`, and it defaults `FALSE` for every admin grant — on `data_liste_plots (id_liste_plots) ON DELETE CASCADE`, one row per pair. Enables RLS **on `plot_access` only**, with `plot_access_self USING (db_user = current_user)` so an account reads its own grants and no others; grants `SELECT` directly to whoever already holds `SELECT` on `data_liste_plots`, never to `PUBLIC`. Adds `accessible_plots(mode)` (`STABLE`, `SECURITY INVOKER`, `search_path` pinned) for the R layer, and `plot_access_creator()` + `trg_plot_access_creator` to give a plot's importer read, write and grant rights over it. **No `can_read` column**: a row *is* read access, because write implies read. **Drops no existing policy, enables RLS on no other table, changes no existing grant** — the ~125 policies still enforce access, so behaviour is identical the moment it finishes | refuses to run while any `INSERT` policy on `data_liste_plots` is `WITH CHECK (true)`. `migrate_plot_access_table(con)` to rehearse — it prints all ~25 statements and the derived grant list — then `dry_run = FALSE`. Evidence: `check_plot_access_table(con)` reports the table, the cascade, the single policy, `search_path` pinned on both functions, `accessible_plots()` as `SECURITY INVOKER` and `plot_access_creator()` as `SECURITY DEFINER` (it must be: the inserting account has no `INSERT` on `plot_access`), the trigger enabled, no `INSERT`/`UPDATE`/`DELETE` for anyone but the owner, nothing to `PUBLIC`, and **`rls_enabled = FALSE` on all six child tables** |
| `plot_access_seed.R` | fills `plot_access` from the two things granting access today: the ~125 per-account policies on `data_liste_plots`, and `created_by`. The owner is excluded from both — it bypasses RLS by ownership, and `add_created_by.R`'s backfill attributed every pre-existing plot to it. Data only; nothing reads the table yet, so it can be re-run, corrected, or emptied with a `DELETE` | `report_plot_access_seed(con)` first — read-only, and it answers the two questions that decide whether the grant set is recoverable at all: can every policy be read, and does every `created_by` value name a real role. Parsing is anchored (`.parse_policy_plot_ids()`, 37 offline assertions) and **refuses rather than guesses**: applying aborts while any policy is unparseable, because a partial seed is a colleague locked out at step 5. Then a gate — every account's plot set is compared against `get_user_accessible_plots()`, the package's own reader, which parses the same policies with a deliberately loose digit regex (`R/connections_db.R:1182`); a disagreement means one of the two is wrong and applying rolls back. `ON CONFLICT` with `OR` semantics, so re-running never narrows a grant — except `can_delete`, which is left out of the `DO UPDATE` on purpose, so a re-run cannot hand `DELETE` back to an account the owner has since taken it from. **Admin `DELETE` grants are not carried over**: the old per-account `DELETE` policies say one thing and `can_delete` is recorded `FALSE` anyway. Creator rows keep `can_delete = TRUE` regardless of the flag - deleting a plot you imported is what `creator_access_delete` already enforces. Nothing is destroyed by that — those policies stay in `pg_policies` untouched through step 4, and `preserve_delete = TRUE` recovers the original state at any time. Not recoverable: when each grant was originally made — a policy records no date, so seeded rows carry today's `granted_on`. Evidence: `check_plot_access_seed(con)` re-runs the gate and checks creator rows against `created_by` in both directions |
| `revoke_delete_rights.R` | target state: **an account may delete the plots it created, and nothing else.** `report_plot_access_seed()` showed 13,913 of 14,018 plot grants carrying write, nearly all of them `DELETE`, because `define_user_policy(operations = "ALL")` wrote `_select`, `_update` and `_delete` together — nine accounts on more than 1,100 plots, three on more than 1,900 of 2,194; 32 accounts also hold the `DELETE` table privilege directly. Moves **both** layers, because either alone leaves the right useless or the restriction cosmetic: (1) drops the per-account `policy_<user>_delete` policies, leaving `creator_access_delete` — a global `USING (created_by = current_user)` from `add_created_by.R` — as the only `DELETE` policy, so deletion is creator-scoped by the database's own rule rather than by a plot list (without this, `alexmass` could still delete 2,036 plots rather than the 68 it imported); (2) revokes the `DELETE` table privilege from every non-owner account **except** those appearing in `created_by`, since without it the creator policy grants nothing. **Residual until step 5:** the creator accounts keep `DELETE` on all seven tables, and the six child tables have no policy, so they can delete child rows of plots they did not create. Revoking from them is not an option — they could delete the plot row but not its children, and `fk_subplot_plot_integrity.R`'s keys are `ON DELETE NO ACTION`, so the deletion would fail outright. **Nothing else breaks:** every `DELETE FROM` against these seven tables in `R/` lives in `safe_delete_plot()`, `safe_delete_individuals()`, `safe_delete_individual_features()` or `safe_delete_specimen_links()`. Paired with a change in `R/`: `define_user_policy()` no longer writes a `DELETE` policy or grants `DELETE` for `operations = "ALL"`, without which the next `define_full_access_policy()` call would reopen all of this | `report_delete_rights(con)` first — read-only, and it covers **every** table in the schema, not just the seven, so `DELETE` held on an audit trail (`followup_*`) is visible before a narrower sweep hides it. Then `migrate_revoke_delete_rights(con)` to rehearse: it prints every `DROP POLICY` and `REVOKE` **and the inverse of each**. Refuses to drop anything unless a `TO PUBLIC` `DELETE` policy is present to catch what those policies were doing. `keep = NULL` derives the creator set from `created_by`; pass a vector to override. On apply it writes `restore_delete_rights.sql` **and reads it back to count the statements** before dropping anything. Handing delete to a non-creator afterwards is `grant_delete_right(con, user, ids)` in `R/plot_access.R`, which sets `can_delete`, grants the table privilege, and warns on every call that the table privilege cannot be plot-scoped until step 5. Evidence: `check_delete_rights(con)` reports a `TO PUBLIC` policy surviving, no per-account `DELETE` policy left, the privilege held only by the owner and the creators, **and that each creator holds it on all seven tables** — one short would make deleting their own plot fail partway |
| `plot_scope_orphans.R` | **superseded as a repair by `delete_orphan_plot_rows.R`; keep it for `report_plot_scope_orphans()`, which is what characterises the problem.** The report proved neither inference route reaches the 23 NULL rows — they carry no measurement and their names match no plot — so the repair path now finds nothing to do. Was: gives a plot to the `data_liste_sub_plots` rows that have none, where the measurements attached to the subplot all agree on one plot. An RLS policy follows a key, so a NULL key matches no policy and the row goes invisible to everyone but the table owner — 23 subplots and 3 measurements are in that state | `report_plot_scope_orphans(con)` first — read-only, and also answers whether `data_ind_measures_feat.id_sub_plots` is usable as a one-hop key, and whether the two unconstrained plot columns could take a foreign key. Then `migrate_plot_scope_orphans(con)` to rehearse, `dry_run = FALSE` to apply. Repairs only the unambiguous cases; subplots spanning several plots, subplots with no measurements, and the 3 measurements with no individual are reported and left for a decision. Evidence: step 4 reports exactly the un-inferable rows remaining |
| `duplicate_family_taxa.R` | merges family-level rows that repeat the same `tax_fam` (**taxa** database): repoints `table_taxa.idtax_good_n` and every foreign key `pg_constraint` reports against `table_taxa` — today `table_taxa.id_parent`, `taxa_backbone_link.idtax_n` and `table_traits_measures.idtax` — plus the main database's `idtax*` columns, onto the surviving accepted row; then deletes the duplicates and refreshes `table_idtax`. Fabaceae is the known case: 5769 accepted, 11458/14046/16016/16051 duplicates | `report_duplicate_family_taxa(con_taxa, con_main)` first, then `merge_duplicate_family_taxa(con_taxa, con_main, family = "Fabaceae")` as a rehearsal and `dry_run = FALSE` to apply, one family at a time. Groups with several accepted rows are skipped unless `tie_break` is given (`"most_referenced"` or `"lowest_id"`); the rehearsal prints which row would survive and why. On the main database the sweep repoints `specimens`, `data_individuals`, `rainbio_records` and `followup_updates_rainbio_records`, and never writes to a key column, to `table_taxa`/`table_idtax`, or to a `*_backup`/`*_temp` table, and is skipped entirely when both connections turn out to be the same database; what it leaves alone is printed. Evidence: `check_duplicate_family_taxa()` reports the family gone and no dangling `id_parent` or `idtax_good_n` |
| `backbone_citation_metadata.R` | adds `backbone_list.homepage` and `backbone_list.citation_template`; writes the citation formula each publisher asks for (APD's, and Kew's for WCVP followed by the rWCVP reference), the publishers, the sites, and `4.0.0` as the `source_version` of the current APD import (**taxa** database) | run once, before APD is offered to users: `migrate_backbone_citation_metadata(con_taxa)` then `dry_run = FALSE`. The rehearsal prints the citation each formula produces. `backbone_citation()` builds citations from these values and falls back to a plain sentence until it has run |

## Applied from another project: the RAINBIO transfer

Not ours, but it changed `plots_transects`, so it belongs in this record.

A local RAINBIO database (`rainbio_n`) was transferred into
`plots_transects.public` by the **georefapp** project, in five phases, during
August 2026. Everything it created carries a `rainbio_` prefix. Roughly
1.06 M rows.

**The scripts are not in this repository, and not in that one either.** They
live on the maintainer's machine at
`georeferencing_app/inst/migrations/` (`phase0_inspect_target.R` through
`phase5_gazetteer.R`, plus `_helpers.R` and `migration_sql.R`), with the plan
in `inst/docs/PLAN_RAINBIO_MIGRATION.md` and the deferred items in
`inst/docs/OUTSTANDING_DECISIONS.md`. That project gitignores both directories,
so this section may be the only versioned trace of the work.

### What it put in `plots_transects`

| Object | Note |
|---|---|
| `rainbio_records` | the occurrence records; carries `idtax_n` |
| `followup_updates_rainbio_records` | 132,038 rows, from `followup_updates_table_records`; carries `idtax_n` |
| `rainbio_loc_notes`, `rainbio_colnam`, `rainbio_maj_areas`, `rainbio_country_map` | supporting tables |
| `rainbio_gazetteer_localities`, `rainbio_gazetteer_occurrences` | the gazetteer georefapp consumes |
| `table_countries` | extended from the ForestPlots template, plus an `iso3` column. Additive: existing ids unchanged |

Rollback, should it ever be needed, is in that project's `inst/migrations/README.md`:
every object carries the prefix, so dropping them is enough. `rainbio_n` stayed
authoritative throughout — nothing was dropped from the source.

### Status

Phases 1–3 recorded as applied on 2026-08-26. Phases 4 and 5 are not confirmed
from here. Evidence seen from this side on 2026-09-24: `rainbio_records` and
`followup_updates_rainbio_records` exist and hold data (1,415 and 308 rows
respectively pointing at the duplicate Fabaceae entries).

### Why this matters to CafriplotsR

Both tables carry `idtax_n` and no package code names them, so a search of `R/`
suggests they are dead. They are not. Anything that merges, renumbers or
deletes taxa must maintain them — `duplicate_family_taxa.R` does.

### The stale copies it did *not* create

Phase 0 found these already sitting in `plots_transects.public`, beside the
live mirror `table_idtax` (367,171 rows):

| Table | rows | reading |
|---|---:|---|
| `table_taxa` | 351,561 | the 2020 snapshot, identical to local RAINBIO |
| `table_traits_measures` | 84,911 | near-identical |
| `table_tax_famclass` | 15 | identical |
| `table_traits` | 10 | identical |

**Anything joining `plots_transects.public.table_taxa` is reading 2020 taxonomy
while `table_idtax` next to it carries 2026.** The live taxonomy is `table_taxa`
in the **rainbio** database. `table_idtax` is not a copy of it: it is the
synonymy link table, two columns (`idtax_n`, `idtax_good_n`) over ~367,000 rows,
rebuilt from rainbio by `update_taxa_link_table()`. That function is the way to
refresh it — depending on the installation it goes through the
`refresh_table_idtax()` SQL function or, when the staging table
`table_idtax_temp` holds rows, through the legacy path that also refreshes the
staging table. Calling the SQL function by hand reports success and can leave
the staging table on the old taxonomy.

These four are excluded from anything this repository's migrations write, and
cleaning them up is its own piece of work, not yet done.

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
