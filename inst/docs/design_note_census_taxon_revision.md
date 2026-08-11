# Design note — validating revised identifications during a census import (IMPLEMENTED)

**Status:** implemented 2026-08-11. `R/census_taxon_revision.R`,
`R/mod_feat_step3b_taxon_revision.R`, `R/migration_followup_idtax.R`.
**Related:** `R/mod_feat_step3_census_import.R`, `R/census_split.R`
(`taxon_drift`), `R/specimen_linking_functions.R`, `R/census_import_transaction.R`.

---

## 1. The gap

A census import matches stems on plot + tag and writes measurements. It never
touches `data_individuals.idtax_n`. So when a field team corrects an
identification between two censuses — which is normal, not exceptional — the
correction is *reported* as `taxon_drift` and then discarded. The UI is at least
honest about it: "The database value is kept."

That is the right default for an automated import, and the wrong end state. The
correction is real information and there is currently no path for it other than
the taxonomic tools, used separately, after the fact, against a list the user
has to reconstruct by hand.

## 2. Why it cannot just be applied

A taxon revision is not symmetric with a measurement. A measurement is a new
fact about a new moment; a revision *overwrites* a determination that may rest
on physical evidence. Whether it should be accepted depends on what evidence
stands behind the existing identification — which is exactly why this needs a
validation step and not a checkbox.

## 3. The evidence that decides it

Verified against the production database, 2026-08-11.

Individuals link to herbarium material through `data_link_specimens`
(`id_n` ↔ `id_specimen`, plus `id_linktype`, and a copy of the field
identifiers). Two link types are in use:

| `id_linktype` | `linktype` | links | meaning |
|---|---|---|---|
| 1 | `type_individual` | 26,408 | the specimen was collected **from this tree** |
| 2 | `referenced_individual` | 127,687 | the specimen was used as a **comparison** for the determination |

151,231 of 439,567 individuals carry at least one formal link.

The distinction is the whole point. Revising a stem that has a
`type_individual` voucher contradicts a physical specimen sitting in a
herbarium — and arguably the specimen's determination should move with it.
Revising a stem whose identification rested on comparison, or on nothing but a
field call, is routine.

**And the state the user asked about is already the common one.** Field
collection numbers live on `data_individuals` (`herbarium_nbe_char`, 156,230
rows) well before any formal link exists:

| has field number | has formal link | individuals |
|---|---|---|
| no | no | 280,589 |
| no | yes | 2,748 |
| **yes** | **no** | **7,747** ← collected, not yet linked |
| yes | yes | 148,483 |

So "a specimen was collected but the link does not exist yet" is not an edge
case to handle defensively — it is 7,747 individuals of steady state. During a
*new* census it is the norm: the specimen is in a press, the number is in the
field table, and nothing has reached the herbarium database.

## 4. Four evidence states

For each stem whose file taxon differs from the database:

| State | Source | Reading |
|---|---|---|
| **Voucher** | `data_link_specimens`, `type_individual` | A specimen of this tree exists. Revision contradicts it — highest scrutiny, and the specimen determination should probably follow. |
| **Reference** | `data_link_specimens`, `referenced_individual` | Determination by comparison. Revision is ordinary. |
| **Collected this census** | herbarium number in the uploaded row, no link in the database | A voucher is coming but is not filed. Accept provisionally and flag for the specimen linking app. |
| **Field only** | nothing | Cheapest to accept, least evidenced. |

The third state is the one the current code has no concept of, and it can only
be detected by reading the *uploaded file*, not the database — which is why it
belongs in the wizard rather than in a later taxonomic pass.

## 5. Proposed shape

**A step of its own, between classification and validation.** Not a panel inside
step 3: the existing drift alert is read-only reporting, whereas this asks for a
decision per stem, and mixing the two invites clicking through it.

The table, one row per revised stem:

```
plot_name │ tag │ id_n │ current taxon │ proposed taxon │ evidence │ herbarium n° │ decision
```

`current taxon` and `proposed taxon` must be resolved from `idtax_n` to names
via the **taxa database** — a second connection (`call.mydb.taxa()`), so the
module needs both. Showing bare integers would make the step useless.

Decisions default by evidence state, and the defaults are the design:

- **Voucher** → default *keep database*, require an explicit override, and warn
  that the specimen determination will not be updated by this import.
- **Reference** / **Field only** → default *accept file*.
- **Collected this census** → default *accept file*, marked pending voucher.

Bulk actions per evidence group, because a whole-plot recensus can produce
hundreds of revisions and a per-row UI would not be used.

**Writing.** Accepted revisions become `UPDATE data_individuals SET idtax_n`
inside the existing census transaction, alongside the census record, recruits
and measurements — one commit or none. `original_tax_name` is **not** touched:
it exists to preserve what was originally written.

## 6. Questions raised while designing, and how they were settled

1. **Provenance — resolved.** An earlier draft of this note called
   `followup_updates_individuals` legacy because it records `id_diconame_n`, the
   pre-`idtax_n` taxon column. **That was wrong.** The table is live and already
   carries 4,440 rows with `modif_type = 'idtax_n'`, the most recent from
   2026-06-19. What it has never held is *which taxa a revision moved between* —
   it is a mirror of the old `data_individuals` schema and has no column able to
   store a taxon id, so those 4,440 rows say a determination changed without
   saying to what.

   [migrate_followup_idtax()] adds `idtax_n` (before), `idtax_n_new` (after) and
   `created_by`. A pure mirror can express a state but not a transition, which is
   precisely why the existing rows are unreadable. Existing rows keep `NULL`;
   their information is not recoverable and inventing it would be worse than
   leaving the gap visible. `.apply_taxon_revisions()` refuses to run until the
   columns exist, rather than adding to the pile.

2. **Specimen determinations — deliberately excluded.** Revising a stem does not
   revise the determination of a specimen collected from it. That belongs to the
   specimen tools. The step says so in a warning rather than doing it silently
   or leaving it unsaid.

3. **Unidentified → identified — its own category.** `identification_gained`,
   accepted by default whatever the evidence, including against a voucher.
   `351190` ranks as `higher` on the precision ladder, so it can never be
   read as a precision loss.

4. **Reverse direction — its own category.** `precision_lost`, defaulting to
   *keep database* even with no evidence at all, and marked in the table so the
   user sees why.

## 7. What was built

| Piece | Where |
|---|---|
| precision ladder, name assembly | `.taxon_precision()`, `.assemble_taxon_name()` |
| the two database reads | `.fetch_taxon_names()` (taxa DB), `.fetch_specimen_evidence()` (main DB) |
| the pure classifier | `.classify_taxon_revisions()` |
| one-call wrapper | `collect_taxon_revisions()` |
| the step | `mod_feat_step3b_taxon_revision_ui/server()` |
| the write | `.apply_taxon_revisions()`, inside the census transaction |
| audit columns | `migrate_followup_idtax()` |

**One deviation from §5.** The step renders inside wizard step 3, below the
classification review, rather than as a seventh step. Renumbering 1–6 would
have meant touching the navigation and skip logic for every mode. It keeps its
own header, evidence cards, bulk actions and decision counter, so it remains a
distinct decision surface rather than another alert.

Both database reads degrade to a message and an empty frame on failure: missing
evidence shows the revision without it, a failed taxa lookup shows `idtax` ids.
Neither blocks the step.
