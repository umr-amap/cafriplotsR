# Design note — validating revised identifications during a census import (PROPOSED / NOT IMPLEMENTED)

**Status:** proposed. Discussion held 2026-08-11, no code written.
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

## 6. Open questions, to settle before building

1. **Provenance.** Overwriting a determination should leave a trace.
   `followup_updates_individuals` exists (84,463 rows) but records
   `id_diconame_n` — the legacy taxon column — not `idtax_n`, so it appears to
   be legacy itself. Decide whether to extend it, add audit columns, or accept
   that `created_by`/timestamps on the census record are enough.
2. **Specimen determinations.** When a stem with a `type_individual` voucher is
   revised, should the specimen's own determination follow? That crosses into
   the specimen tools and is probably out of scope for a census import — but it
   should be a deliberate exclusion, not an oversight.
3. **Unidentified → identified.** A stem recorded as `351190` (Magnoliopsida)
   that now has a real determination is technically drift but is pure gain. It
   probably deserves its own group with *accept* as the default and no warning.
4. **Reverse direction.** A file that identifies a stem *less* precisely than the
   database — genus where there was a species — is almost always a data entry
   regression, not a revision. It should default to *keep database* regardless
   of evidence.

## 7. Effort

A new wizard module, a cross-database name resolution, one query joining
`data_link_specimens` and `linktypelist`, a field-number check against the
uploaded table, and an `UPDATE` branch in `.execute_census_import()`.
Comparable in size to the census import mode itself — not a small addition.
