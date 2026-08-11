# Design note — tag identity and alphanumeric tags (PARTLY IMPLEMENTED / `tag_label` PARKED)

**Status:** the storage and matching defects found during this work are fixed and
merged. The `tag_label` proposal in §6 is parked, no code written.
**Discussion held:** 2026-08-10 / 2026-08-11.
**Trigger to revisit:** when a user actually arrives with alphanumeric tags
(`A12`, `12B`, `PLOT1-034`), or when a field team asks to record tags exactly as
written rather than as numbers.

**Related:** `R/census_split.R`, `R/import_individuals_validation.R`,
`R/migration_tag_to_numeric.R`.

---

## 1. The question that started it

`launch_feature_wizard()` and the import wizard both treat `tag` as a number.
What happens to a user whose tags contain letters?

Answering it required reading the live schema rather than the code, because the
two disagree. The code declares tag as `"numeric"`
(`import_individuals_templates.R`) or coerces it with `as.numeric()` in a dozen
places. The column was something else.

## 2. What the database actually held

Verified against the production database on 2026-08-11, 439,567 individuals:

| Fact | Value |
|---|---|
| `data_individuals.tag` type | **`real`** — single precision float, not integer |
| exact integer range of a `real` | 2^24 = 16,777,216 |
| max tag in use | 702,910 (21,115 tags ≥ 100,000) |
| non-integer tags | **3,651** across 82 plots — the `22.1, 22.2, …` multi-stem convention |
| duplicate (plot, tag) | **44,025 groups, 183,672 rows, 989 of 2,166 plots** |
| unique constraint on (plot, tag) | none — only `PRIMARY KEY (id_n)` |
| `sous_plot_name` | superseded: **empty for the most recent ~70,000 individuals** |
| `code_individu` | superseded: text, 162,072 rows, values like `LAM_2-8-1` (plot-quadrat-tag) |
| `multi_tiges_id` | `text` in the database, holds `"no"` on 92 rows, yet parsed with `as.numeric()` everywhere |

Two findings mattered more than the alphanumeric question itself.

**`real` was a latent corruption bug.** `20250001::real` returns `20250000`.
Eight- and nine-digit barcode and RFID tags are ordinary in current inventories,
and nothing downstream would have noticed the difference. Nothing had crossed
the ceiling yet — the migration in §5 confirmed that before running.

**A tag did not identify one stem.** Legacy datasets numbered tags per quadrat,
so `KPWA` tag `1` names 24 different trees. Identity was carried by
`sous_plot_name` and `code_individu`, both of which have since been abandoned
with no replacement — the affected individuals have no `id_sub_plots` link
either, and `subplotype_list` contains no spatial subplot type at all.

## 3. What the code did with a letter in a tag

The worst path was silent. `.generate_sequential_tags()` fired whenever *any*
row had a blank tag and opened with `as.numeric()`. One blank cell in a
character column turned every `"A12"` into `NA`, and the fill then replaced all
of them with row numbers — logged as though only the blank row had changed.

Everything else either failed loudly or degraded quietly:

| Site | Behaviour |
|---|---|
| `add_functions.R:1425`, `:2456` | `stop()` — fine |
| `mod_feat_step3_multi_stems.R:319`, `:749` | stems vanish from the multi-stem UI |
| `import_individuals_with_transactions.R:485` | `stem_grouping` silently not set |
| `functions_divid_plot.R:700` | `stats::approx(x = tag, …)` — interpolates positions along the tag axis |
| `openforis_processing.R:1080` | `seq(from = group_tag, …)` — infers stem groups from consecutive tags |
| `census_split.R` | every alphanumeric recruit flagged as a possible typo |

The last three are the reason §6 is not simply "make `tag` text": several
consumers treat the tag as a position on a number line, not as a label.

## 4. What was fixed

Committed as `980a058` and `c81ed46`.

- Tag values are validated **before** auto-generation can rewrite them, and
  `.generate_sequential_tags()` refuses to coerce rather than discarding what it
  cannot read.
- The zero and negative checks used to sit behind `is.numeric(data$tag)` and so
  never ran on a tag column read from a spreadsheet as text. They now run on the
  parsed values.
- `split_census_table()` counts matches instead of trusting `match()`. More than
  one match is held for review with the candidates listed, never bound to the
  first. Those rows stay out of the import even when reviewed rows are confirmed
  as recruits.
- `idtax_n` is explicitly *not* part of the match key, with a test saying so — an
  identification revised between censuses must not turn a stem into a recruit.

## 5. The migration that ran

`migrate_tag_to_numeric()` — `real` → `numeric` on `data_individuals` and
`followup_updates_individuals`, applied 2026-08-11.

`numeric` rather than `double precision`, because it also clears the float noise
from the fractional tags:

```
(22.1::real)::numeric           -> 22.1
(22.1::real)::double precision  -> 22.100000381469727
```

Proven lossless before altering anything —
`SELECT count(*) WHERE tag::text <> tag::numeric::text` returned 0 over all
400,517 tagged rows — and re-fingerprinted afterwards. R is unaffected:
RPostgres returns `real`, `double precision` and `numeric` alike as R doubles.

`.validate_tag_values()` takes its ceiling from `.tag_precision_limit()`, which
reads the column type, so it was correct both before and after with no edit in
between.

## 6. `tag_label` — parked

**The proposal.** Add `data_individuals.tag_label text`, nullable, carrying the
literal string the field team wrote. `tag` keeps its numeric value where one
exists. Census matching prefers `tag_label` when present and falls back to
`tag`. The numeric consumers listed in §3 keep working on plots that have
numbers and simply do not apply to plots that do not.

**Explicitly not `code_individu`.** It already holds 162,072 alphanumeric
identifiers and looked like a natural home, but it is superseded and its
provenance is unclear (49 rows share `TS051B1`). Reusing a dead column would
inherit its ambiguity.

**Why additive rather than migrating `tag` to text.** A one-column addition
needs no backfill and cannot disturb 400k existing rows. Migrating `tag` to text
would force a canonical string for the 3,651 fractional tags, where `22.1` and
`22.10` are genuinely ambiguous in the source data, and would require a natural
sort or numeric-cast fallback in every consumer. Worth doing eventually; not
worth doing at the same time as anything else.

**What it would touch.** The column, the import template and its validation, the
column-mapping UI in both wizards, `.normalize_tag()` and the keying in
`split_census_table()`, and `.insert_census_recruits()`.

## 7. Left open

- Alphanumeric tags remain unsupported — reported clearly, never destroyed.
- `.validate_tag_conflicts_database()` (`import_individuals_validation.R:863`)
  queries `WHERE plot_name = …` against `data_individuals`, which has no such
  column. The error is swallowed by a `tryCatch`, so the check has been silently
  doing nothing. Not fixed here — separate concern, flagged twice.
- `.generate_sequential_tags()` numbers from `row_number()` within a plot, so a
  partially-filled tag column can generate a tag that already exists in that
  plot. Pre-existing; only reachable when some rows have tags and others do not.
