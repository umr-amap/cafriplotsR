# CafriplotsR 1.9.8 (Development)

### Breaking Changes

* **`query_specimens()` no longer accepts `genus`, `species` and `family`** (`R/functions_manip_db.R`, `R/specimen_query_builder.R`) — the three arguments were accepted and never applied. `filter_taxonomy()` stored them in R6 private fields that were never declared and never read, so the query ran with no taxonomic condition; worse, supplying one satisfied the `needs_filtering` test and sent the call down the filter branch, which then built a condition-free query. `query_specimens(genus = "Cola")` returned every specimen in the database and looked like a successful search
  - Passing one now fails with R's own `unused argument`, which is the point: a name filter that cannot be honoured must not be silently absorbed
  - `idtax_n` is the only taxonomic column of `specimens`, so filtering by name means resolving it first — `query_specimens(idtax_n = query_taxa("Cola nitida")$idtax_n)`
  - `.specimen_condition_taxonomy()` was three quarters apology and is now `.specimen_condition_idtax()`. No caller in the package, in `inst/scripts/` or in the Shiny modules passed any of the three

* **`query_subplots()` no longer accepts `plot_name`, `country`, `locality_name` and `method`** (`R/subsplots_features_function.R`, `R/utils.R`) — the wrapper's filter branch called `.build_plot_query()`, a helper that stopped existing in 1.9.0. The October 2025 rewrite of `query_plots()` replaced it with `PlotFilterBuilder` and commented the definition out; the reorganisation two days later deleted the comment. `query_plots()` was migrated, `query_subplots()` was not, so every filtered call has raised `could not find function ".build_plot_query"` since. Passing IDs skipped the branch, which is why it went unnoticed
  - Supplying one now raises `lifecycle::deprecate_stop()` naming `query_plots()`. There is no behaviour to preserve and so nothing to warn about gently — resolve the plots first and pass their ids: `query_subplots(ids_plots = query_plots(country = "Gabon")$id_liste_plots)`
  - **Calling with neither `ids_plots` nor `ids_subplots` is now an error.** The removed branch treated "nothing supplied" as every plot in the database; with nothing left to fail first, that would have become a live full-table scan. `query_plot_features()` is where asking for everything belongs
  - `".build_plot_query"` is gone from `globalVariables()` in `R/utils.R`, where it had been added to quiet `R CMD check` — which is what kept the missing function invisible
  - `test-specimens-subplots.R` had a test asserting the `could not find function` error. It now asserts the deprecation error for each of the four arguments, and the new abort


* **`PlotFilterBuilder`, `PlotFetcher`, `SpecimenFilterBuilder` and `SpecimenFetcher` are no longer exported** — the four R6 classes were removed along with the `R6` dependency (see *Code Refactoring*). They were query plumbing for `query_plots()` and `query_specimens()`, with no caller anywhere else in the package; code that built a query through one of them should call `query_plots()` / `query_specimens()` instead. Their unused methods — `build_with_or()`, `add_custom_condition()`, `print_conditions()`, `filter_by_ids()`, `fetch_with_filter()`, `fetch_by_collector_and_number()` — have no replacement

* **`update_specimen_fields()` and `update_specimens_batch()` deprecated** in favour of `update_records(table_type = "specimens")` (`R/updates_tables_functions.R`). Both wrote plain columns of `specimens` that `update_records()` already covers; keeping three write paths to one table meant three places to keep in sync, and they had already drifted apart (see the `description` fix below). Both still work, and emit a `lifecycle::deprecate_warn()` naming the replacement
  - `update_records()` is the superset: it takes several specimens at once, defaults to a dry run (`execute = FALSE`), and backs up to the same `followup_updates_specimens` table
  - Migration — `update_specimen_fields(id_speci = 12345, new_values = list(locality = "Mont Bela"))` becomes `update_records(data.frame(id_specimen = 12345, locality = "Mont Bela"), table_type = "specimens", execute = TRUE)`. For `update_specimens_batch()`, rename the columns of `new_data` to their database names first — `col_names_select` / `col_names_corresp` has no equivalent — then add `method = "batch"`
  - **`update_ident_specimens()` is not deprecated.** It resolves genus/species/family to `idtax_n` and can locate a specimen by collector + number, neither of which `update_records()` does — `get_metadata_mappings_specimens()` states that `idtax_n` is expected to be pre-resolved. It remains the identification path
  - The single in-package caller, the *"4b. Collection, locality & notes"* pane of `launch_specimen_identification_app()` (`R/mod_specid_manual.R`), was migrated to `update_records()` so the app does not trigger its own deprecation warning
* **`data_liste_plots.data_d` renamed to `date_d`** — the day-of-survey column has carried a typo since the database was built, beside `date_y` and `date_m`, and the codebase had already split around it: `import_templates.R` hands users a `date_d` column and the validation rules in `import_column_mapping.R` are keyed on `date_d`, while the database, the synonym table, the column descriptions and `get_table_columns()` said `data_d`. A day crossing from one side to the other had nowhere to land
  - Migration script: `inst/migrations/rename_data_d_to_date_d.R`, `dry_run = TRUE` by default. **Applied 2026-08-20.** `data_liste_plots` and `followup_updates_liste_plots` renamed in one transaction; 1,252 of 2,166 plot rows and 1,767 of 2,298 audit rows carried a day and every one survived unchanged, no view referenced the column, and no `data_d` column remains
  - **There is no version that accepts both spellings.** Applying the migration without deploying this code, or deploying this code without applying the migration, breaks the plot import path and `R/mod_census_information.R`, which names the column in raw SQL. They are in the same commit and must move together — relevant now only for a restored backup
  - The audit mirror was included because `backup_direct_records()` copies by column name; leaving it as `data_d` would have broken every plot backup insert
  - Renamed in `R/add_functions.R`, `R/import_column_mapping.R` (synonyms, descriptions, recommended columns), `R/mod_census_information.R` and `R/updates_tables_functions.R`. No `data_d` reference remains outside the migration itself

### Code Refactoring

* **The four R6 classes are gone, and with them the `R6` dependency** (`R/plot_query_builder.R`, `R/specimen_query_builder.R`, `R/plot_feature_filters.R`) — `PlotFilterBuilder`, `PlotFetcher`, `SpecimenFilterBuilder` and `SpecimenFetcher` were the package's only use of `R6`. None of them held state between calls: a builder was constructed inside `query_plots()` or `query_specimens()`, fed the filter arguments, `build()`-ed and discarded, its private field accumulating SQL condition strings. Functions returning those strings do the same work
  - Each filter argument is now translated by its own `.plot_condition_*()` / `.specimen_condition_*()`, which returns that argument's condition(s) as a character vector: none when the argument is `NULL`, and the unsatisfiable `FALSE` when its value matched nothing. `.assemble_plot_query()` and `.assemble_specimen_query()` join the set into one SELECT, and `query_plots()` reaches all of it through a single `.plot_filter_query()` call
  - The feature clauses moved to `R/plot_feature_filters.R`, beside the validation and lookup-resolution they belong with. `.plot_ids_matching_features()` no longer instantiates a builder to reach them
  - **No cost to the query.** The SQL text is unchanged and so is the number of round trips — the lookups that resolve names to ids, then one SELECT. What disappears is an R6 environment per query
  - `.assemble_plot_query()` is deliberately not named `.build_plot_query()`: that name is called by the legacy `query_subplots()` filter path without ever having been defined, and `test-specimens-subplots.R` pins the resulting error. Naming the assembler that would have shadowed a known gap with a silent signature mismatch
  - Behaviour is unchanged with one exception: `query_specimens(genus =, species =, family =)` now says out loud that it is not applying them. `filter_taxonomy()` stored them in R6 private fields that were never declared and never read, so the query ran with no taxonomic condition and returned every specimen. Resolve the names with `query_taxa()` and pass `idtax_n`, which does filter
  - Tests: `test-query-plots-feature-filters.R` rewritten against the new functions, and a new `test-specimen-query-builder.R` covers the specimen path, which had none

* **Growth form traits consolidated** — collapsed 7 branching `growth_form_level_*` traits (ids 42–47) into 3 flat traits: `growth_form_level_1` (unchanged, id=41), `growth_form_level_2` (id=120), `growth_form_level_3` (id=121). The previous design encoded hierarchy branches in trait names, unnecessarily multiplying trait definitions. Migration script: `migrate_growth_form_traits.R`. Affected rows: 31,664 + 14,700 in `taxa_traits_measures`; 410 + 748 in `data_traits_measures`.

### New Features

* **Specimen links can attach to a plot, not only to an individual** (`R/specimen_linking_functions.R`, `inst/migrations/reference_plot_linktype.R`) — `data_link_specimens` has always carried an `id_liste_plots` column beside `id_n`, and no package code ever wrote or read it. 74 rows used it, written directly to the table on 2026-01-06: `id_n` NULL, `id_liste_plots` set, and the free-text `type` reading `reference_plot`. They record a specimen collected somewhere inside a plot where the tree is unknown. Every consumer reached a plot the long way round, through `id_n` → `data_individuals.id_table_liste_plots_n`, so a specimen linked only to a plot looked unlinked
  - **`linktypelist.scope`** — `'individual'` or `'plot'`, saying which of `id_n` and `id_liste_plots` a link type fills. Every pre-existing type is `'individual'`, which is what they are. `reference_plot` is seeded at priority 10, scope `'plot'`
  - **Priority 10 is deliberately below `referenced_individual` (50).** Priority orders the specimen that governs an individual's determination (`idtax_individual_f = coalesce(idtax_specimen_f, idtax_f)`), and every one of those sorts filters on `id_n`, which a plot link has not got — so the value is inert there. It is not inert in `mod_link_preview.R`, which preselects the highest-priority type: a plot-level type must never become the default for pairing a specimen with a tree
  - `get_linktypes()` gains a `scope` argument and reconstructs the column when the migration has not run, so the package works against either schema. Both linking Shiny modules now ask for individual-level types only
  - `.add_link_specimens()` validates each link against its type's scope instead of demanding an `id_n` from every one: NAs are dropped before `setdiff()` (which previously reported the `NA` itself as a missing individual ID and aborted), plot IDs are checked against `data_liste_plots`, and the free-text `type` is written alongside `id_linktype` so the two cannot drift
  - **The duplicate key gained `id_liste_plots`.** dplyr matches `NA` to `NA` by default, so with the old three-column key two plot links to *different* plots collapsed into one
  - **`safe_delete_plot()` and the new `fk_id_liste_plots` must move together.** The existing sweep deletes specimen links by `id_n` only, which never reaches a plot-level link; under the foreign key those links would block the plot deletion. A new step 5.0b clears them
  - Migration `inst/migrations/reference_plot_linktype.R`, `dry_run = TRUE` by default. **Applied 2026-09-01.** The foreign-key phase refuses while orphan plot references exist rather than letting `ALTER TABLE` fail; none were found

* **The login screen can introduce the app it belongs to** (`R/mod_database_login.R`, `R/shiny_app_taxonomic_match.R`) — all ten apps share one login module, whose header reads *"Connect to the CafriplotsR database to access forest plot data."* For an app launched from an R console that is harmless: the user typed the launch call and knows what they asked for. For an app reachable by URL it is the landing page, and the only thing a first-time visitor reads — so the hosted taxonomic matching app at <https://cafri-taxomatch.lab.sspcloud.fr> introduced itself as a plot database, while its own title, subtitle and *About this app* panel sat inside the authenticated panel, invisible until the visitor had already decided to connect
  - `mod_database_login_server()` gains `intro`, defaulting to `NULL`, which keeps the generic header. Given a list of `title` and `body`, it renders those in its place
  - Both are **English translation keys, not final text**, and go through the module's own translator, so the intro follows the language toggle like the rest of the screen
  - Server-side only, unlike `allow_public` and `allow_offline`: the header is a `uiOutput` placeholder, so `mod_database_login_ui()` needs no matching argument and the other nine apps are untouched

* **`query_plots(feature_filters = ...)` filters plots on their features** (`R/plot_feature_filters.R`, `R/functions_manip_db.R`) — a plot carries three kinds of value stored three different ways, and the query function could filter on only two of them. Flat columns (`plot_name`, `locality_name`) and lookup ids (`country` → `id_country`, `method` → `id_method`) were filterable; features were not, although they are what the extracted table shows under names like `data_provider` and `principal_investigator`
  - A feature is not a column of `data_liste_plots` but a row of `data_liste_sub_plots` typed by `subplotype_list`, so it is matched with a subquery rather than a `WHERE` clause on the plots table: `query_plots(feature_filters = list(data_provider = "IRD", principal_investigator = c("Dauby", "Sonke")))`
  - **Values of one feature are combined with OR, different features with AND.** A plot must satisfy every named feature but may do so through different subplot records, which is the only reading that makes sense when each feature is a separate row
  - **Only features whose value reads as text can be used** — `character` features, held in `typevalue_char`, and lookup features such as the `table_colnam` people features, whose value is an `id_table_colnam` held in `typevalue` and whose names are resolved for you. A numeric feature (`census`, `ddlat`) is refused with an error naming its valuetype, rather than being matched as a string and returning nothing
  - Matching follows the existing `exact_match` argument: substring by default, equality when `TRUE`, exactly as `plot_name` and `locality_name` already behave
  - **Explicit ids narrow rather than override.** When `query_plots()` is given `id_plot` (or `id_individual`, `id_tax`, `id_specimen`) it never builds a filter query, so the feature filter would have been silently dropped. It is applied to the fetched ids instead, and reports how many plots it removed
  - `PlotFilterBuilder` gains `filter_features()`; `R6` is now declared in `Imports`, which it was not despite four `R6::R6Class()` calls
  - 37 assertions in `tests/testthat/test-query-plots-feature-filters.R`, run against an in-memory SQLite database of the same shape, covering the generated SQL, the plots actually selected, and every validation error
  - **`launch_query_plots_app()` exposes this under Advanced Filters** (`R/mod_plot_filters.R`) — a feature filter cannot be a fixed input the way country and method are, since the user picks a feature before there are any values to offer. Each filter is therefore a row of its own: a feature dropdown built from `plot_feature_filters()`, then a multi-select of that feature's values, loaded on demand with `plot_feature_values()` and cached per feature. Typing a value that is not in the list is allowed, and is matched as a substring
    - Rows can be added and removed freely. Two rows naming the same feature are merged into one filter holding both sets of values, because `query_plots()` refuses a repeated name and the user plainly meant "either of these"; a row left empty is ignored rather than turned into a filter that matches nothing
    - The panel is a `renderUI`, rebuilt whenever a row is added or the language is changed, so each row's state is held outside its inputs and restored on rebuild — otherwise switching language silently emptied every filter the user had set
    - The generated R code carries `feature_filters = list(...)` rather than the plot ids it resolves to, so the script the user copies out of the app is the same query they built, and runs on its own
    - Failing to read the feature list costs only the feature section, not the rest of the filters
    - Eight EN/FR pairs added to `inst/translations/translation.json`, and the shared `"Feature"` entry — which had `fr` set to `"Feature"` — is now translated. 24 assertions in `tests/testthat/test-app-feature-filters.R`, including that a row survives a panel rebuild and that the generated code parses

* **`plot_feature_filters()` and `plot_feature_values()`** (`R/plot_feature_filters.R`) — two helpers for finding out what can be filtered before filtering on it. `plot_feature_filters()` lists the features accepted by `feature_filters`, with their valuetype, category and description; `plot_feature_values()` returns the distinct values one feature actually holds, with the number of plots carrying each, resolving `table_colnam` ids to readable names. `subplot_list()` remains the way to see every feature type, filterable or not

* **Public login is now opt-in per app** (`R/mod_database_login.R`) — the "Connect as public user" button sat in the shared login UI, so all ten apps offered it, including the import wizards, the record editor and the specimen apps, where a read-only account cannot carry a single workflow to the end
  - `mod_database_login_ui()` and `mod_database_login_server()` gain `allow_public`, defaulting to `FALSE`. The separator, the button and the read-only notice are built only when it is `TRUE`, and the `connect_public` observer is guarded with `req(isTRUE(allow_public))`, so the public connection cannot be driven from a crafted client either
  - Three apps opt in: `launch_taxonomic_match_app()`, `launch_taxo_backbone_app()` — which already hid its editing controls from public users through `is_public` — and `launch_query_plots_app()`, where the row-level security policies decide what a public user actually sees
  - The seven apps that write to the database now show the credentials form alone

* **Offline mode is now opt-in per app** (`R/mod_database_login.R`) — the "Use offline (cached backbone)" button sat in the shared login UI beside the public one, so any app showed it as soon as a cache existed on disk, although only `launch_taxonomic_match_app()` does anything with the result
  - Offline mode leaves the session authenticated with `pool_main` and `pool_taxa` set to `NULL`. In the nine other apps that means an app with no connection and nothing to query, import or correct — the button led straight into a dead end
  - `mod_database_login_ui()` and `mod_database_login_server()` gain `allow_offline`, defaulting to `FALSE`; the button and its notice are built only when it is `TRUE` *and* a cache exists, and the `connect_offline` observer is guarded as well
  - `launch_taxonomic_match_app()` is the only app that opts in, and it already hides the traits enrichment tab in that mode
  - `vignettes/apps-overview.Rmd` and `-fr.Rmd` document what offline mode gives (auto matching, fuzzy suggestions, manual review), what it does not (traits enrichment, which has no local substitute), the prerequisite that the cache is only written by an *online* run of the matching app, and that no other app offers it
  - 10 assertions in `tests/testthat/test-login-gating.R`, including a source scan asserting no second app opts in

* **`launch_data_update_app()`** — new Shiny app (`R/shiny_app_data_update.R`) for correcting plot metadata and individual data one record at a time. `update_records()` can already do this, but it expects the caller to know which table a value lives in, which is the part that is not obvious
  - **Two sections.** *Plot metadata* loads a plot, edits the columns of `data_liste_plots` (with `method` and `country` as dropdowns rather than foreign keys), and edits its features. *Individual data* finds an individual by plot and tag or by `id_n`, edits `data_individuals`, changes the identification through the embedded `mod_taxa_search` picker, and edits its trait measurements
  - **Aggregated columns are resolved, never written.** Many columns of an extracted table are not columns of the record: plot features are rows of `data_liste_sub_plots`, individual features are rows of `data_traits_measures`, and one extracted column can be the aggregate of several such rows — which is why `detect_feature_changes()` refuses them. The app shows the aggregate read-only and offers the underlying records as the editable inputs, each labelled with its own id and its census or subplot context
  - **Each feature is described the way its own extraction summarises it**, not with one blanket rule. A plot's `census` feature is not a value at all — it becomes `n_census`, `first_census`, `last_census` and `date_census_N` — so reporting the mean of censuses 1 and 2 as `1.5` was nonsense. An individual trait measured at several censuses is kept per census by `aggregate_numeric_features_dt()`, and is shown as `census_1: 12.5 | census_2: 13.1`. Numeric plot features are averaged, text and `table_*` features are joined, and a feature `aggregate_plot_features()` does not pick up at all is named as such rather than given an invented value
  - **The whole record, on demand.** "Current stored values" showed a hand-picked set of columns; it now shows the record as `output_style = "full"` returns it, features included, in a `DT` table. It is a full extraction, so it runs only when asked for — and `query_plots()` is console-oriented, printing progress and returning widgets that RStudio would render into the pane the app is running in, so the call is made with the viewer disabled and its output captured
  - **An identification is not just `idtax_n`.** The panel used to show the stored `idtax_n` and its original name, which is not what an extraction reports: `merge_individuals_taxa()` resolves synonymy (`idtax_f`), then lets a linked specimen's determination override it (`idtax_specimen_f`), and `idtax_individual_f` is what extracted tables carry. The app now shows all four steps, says which one governs, and warns plainly when a specimen governs — editing `idtax_n` there changes nothing an extraction will show, and the specimen is the thing to correct, in `launch_specimen_identification_app()`
  - **Reference features are edited by name.** A `table_colnam` feature is a numeric feature holding an `id_table_colnam`, stored in `typevalue`; the app resolves it to the collector's name for display, offers a dropdown of names, and writes the id back to `typevalue` only. `typevalue_char` is never used for these, and neither is `data_liste_sub_plots.id_colnam` — it is populated on a negligible number of rows and only in error (`typevalue` is set on 100% of them: additional_people 3095/3095, data_manager 858/858, principal_investigator 1088/1088, team_leader 2169/2169)
  - **The flat-column form is an allow-list, not the schema.** `data_individuals` and `data_liste_plots` both carry deprecated columns (`dbh`, `code_individu`, `sous_plot_name`) that nothing writes any more; offering them would invite corrections that change no behaviour, and editing `dbh` would silently disagree with the `stem_diameter` measurements in `data_traits_measures`. `.upd_direct_fields()` intersects `get_table_columns()` with the live schema instead, and reports what it omitted rather than hiding it silently
  - **Writes reuse the existing machinery.** `detect_direct_changes()` and `execute_direct_updates()` re-read stored values immediately before writing, write only genuine differences, and back records up to their follow-up table where one exists. Flat columns and features go in one transaction, so a failure cannot leave a record half-updated
  - Manual editing of existing records only — adding and deleting measurements remain the job of `launch_feature_wizard()` and the `safe_delete_*` functions
  - New backend in `R/update_app_resolver.R`, one module in `R/mod_update_record.R` serving both sections, 121 assertions in `tests/testthat/test-update-app-resolver.R`, and the EN/FR pairs it needs in `inst/translations/translation.json`. The read-only column notice ("N columns of `data_liste_plots` cannot be edited here") was dropped: the form is an allow-list of what *can* be corrected, and counting the rest told the user nothing they could act on

* **Feature Wizard, Add Plot Features — what the selected plots already hold is now on screen** (`R/mod_feat_step3_plot_features.R`) — step 3 let a user pick a feature type and type a value with no view of what the plot already carries. That is how a plot ends up with two principal investigators, or with the same value recorded twice for the same year
  - A panel at the top of the step lists every feature the selected plots already have, one row per plot and feature, with what an extraction would show for it and how many records back it. A checkbox narrows the list to the feature types being added. In *New Census* mode the same panel is where the plot's existing censuses appear, as `n_census = 2 (2015-03, 2021-06)`
  - Under each feature's input, a line naming the plots that already have a value for that very feature — amber, because a second record turns the extracted value into an aggregate that `launch_data_update_app()` can then only edit record by record
  - Step 5 warns about the same thing at validation time, per row and column: a record for the same year is named as such, otherwise the warning says how many records the plot already carries. Reported as warnings rather than through the "drop these rows" checkbox — one row of an `add_features` import is a plot carrying several feature columns, and dropping the row would discard the columns that were fine. Nothing is blocked; a second record is sometimes exactly what is wanted
  - The overview is the update app's, not a second implementation of it. `.upd_plot_feature_records()` now takes several plot ids and annotates each plot separately — a feature backed by one record in each of three plots is an aggregate in none of them — and the wording of the extraction rules and the table itself moved to `R/feature_overview.R`, taking a resolved translator so a non-Shiny caller can use them. 15 assertions in `tests/testthat/test-feature-overview.R`, 11 EN/FR pairs added to `inst/translations/translation.json`

* **Feature Wizard, Validation — the measurements the database already holds can be dropped from the import** (`R/mod_feat_step5_validation.R`) — validation has long reported *"N measurement(s) already exist in the database for the same individual, trait and census"*, but the only way to act on it was to go back and edit the file. A checkbox now offers to remove exactly those rows
  - **Unticked by default, and nothing is removed unless it is ticked.** Recording a second measurement of the same individual, feature and census is sometimes intentional, so the duplicate report stays a warning rather than becoming a rule
  - The rows are recorded as row numbers rather than counted, so they can be dropped precisely, and each is named in the preview's `issue` column ("already recorded in the database for this census") — the count can be traced back to the individuals it came from
  - Ticking the box updates the row count, the preview table and the "Issues by Trait" summary together, and the filtered data is what step 6 imports
  - Dropping every row fails validation instead of running an import of nothing and reporting success
  - Three EN/FR pairs added to `inst/translations/translation.json`

* **`split_census_table()`** — new exported function (`R/census_split.R`) that classifies a flat census table against the individuals already recorded for the selected plots, removing the need to hand-split field data into recruits and remeasures before importing. Which stems are already in the database is something only the database knows, so splitting by hand is guesswork
  - Each row is labelled `remeasure` (existing stem, `id_n` attached), `recruit` (new individual), `review` or `invalid`, and every original column and row is preserved
  - **Typo guard**: an unknown tag within one edit of an existing tag — or one adjacent-character swap away from it — is held as `review` rather than becoming a new individual, since creating a duplicate tree from a mistyped tag is silent and hard to undo. `assume_new_block = TRUE` exempts numeric tags that continue the plot's numbering, without which nearly every genuine recruit would be flagged
  - Also reports taxon drift on remeasured stems, recorded stems with no row in the table (excluding those already recorded dead), repeated plot + tag combinations, and per-plot counts
  - Pure when `existing` is supplied — no connection needed. `.fetch_plot_individuals()` is the thin database layer, reaching `plot_name` through `data_liste_plots` (`data_individuals` has no such column)

* **Feature Wizard — "Import a Full Census" mode** (`R/mod_feat_step3_census_import.R`, `R/census_import_transaction.R`) — a new operation mode that takes the single flat table a field team actually produces and does the whole campaign in one pass, replacing the previous sequence of New Census → hand-split → Import Wizard → Add Measurements → Define Multi-Stems
  - Step 3 uploads one file, creates or reuses the census record, maps columns in a single pass (keys, individual attributes, traits), and calls `split_census_table()` to classify every row against the database. The split review panel shows the counts and surfaces possible typos, taxon drift, repeated stems and stems absent from the file; rows held for review require an explicit confirmation before they can be imported as recruits
  - Step 5 validates the census identity, the recruits (including `.validate_multi_stem_grouping()` on `multi_tiges_id`), and what the split flagged. Tags absent from the database are no longer errors in this mode — the split already accounted for them
  - Step 6 runs `.execute_census_import()`: census record, recruits (`INSERT ... RETURNING id_n`), `.apply_stem_grouping()`, then measurements for every stem, in one transaction that rolls back completely on failure. Previously a failure part-way through the multi-app sequence left recruits inserted with no measurements
  - Recruits with no `idtax_n` are recorded as unidentified (Magnoliopsida, 351190), matching the Import Wizard, and step 5 warns with the count
  - Team members are not collected in this mode; the UI directs users to create the census with the New Census mode first and select it here

* **`export_census_split()`** — writes a `census_split` into the files the existing wizards already accept: recruits for `launch_import_wizard()`, measurements for `launch_feature_wizard()`. Review rows are written separately and deliberately kept out of the recruit file

* **`update_specimen_fields()`** — new exported function (`R/updates_tables_functions.R`) for updating the non-identification columns of a single specimen: `colnbr`, `suffix`, `coly`, `colm`, `cold`, `add_col`, `locality`, `country`, `ddlat`, `ddlon`, `description`, `original_tax_name`. Until now only `update_ident_specimens()` could write to `specimens`, and its `UPDATE` is limited to `idtax_n`, the `det*` columns, `colnbr` and `suffix`, leaving the other columns with no update path
  - Fields are whitelisted with a declared type (`.specimen_editable_fields()`) and coerced accordingly; only values that actually differ from the stored ones are written
  - `NA` or an empty string clears a field (sets it to `NULL`); a field absent from `new_values`, or set to `NULL`, is left untouched
  - The backup row in `followup_updates_specimens` and the `UPDATE` run in a single transaction, checking out from a pool when one is supplied via `con`

* **Specimen identification app — edit specimen fields manually** (`R/mod_specid_manual.R`) — the manual pane of `launch_specimen_identification_app()` gains a *"4b. Collection, locality & notes"* section for `coly`/`colm`/`cold`, `add_col`, `locality`, `country`, `ddlat`, `ddlon` and `description`, applied through `update_specimen_fields()`
  - Unlike the determination section (where an empty field means *keep the current value*), these inputs are pre-filled with the current values and are absolute: clearing one erases it in the database. Both sections now state their rule in the UI, and a "Reset to current values" link restores the stored values
  - The current-values card and the preview diff cover the new fields, and the specimen is re-queried after a successful apply so the pane reflects what is stored
  - Batch mode is unchanged and still updates identification only

* **Tropicos bulk upload conversion** (`R/tropicos_export_functions.R`)
  - **`build_tropicos_upload_table()`** — converts `query_specimens()` output into the 31-column Tropicos bulk upload template layout (dates split into day/month/year, collection number + suffix, taxon name, `SeniorCollectorPersonID` joined from `table_colnam.id_tropicos_person`). Columns with no reliable database source (e.g. `Duplicates`, `DeterminationQualifier`, `AuthorityKey`) are always left blank rather than guessed
  - **`build_specimens_from_tropicos()`** — the reverse: converts an already-imported Tropicos specimen export/search-results table (a different column layout from the upload template, see `inst/docs/example_tropicos.csv`) into a `query_specimens()`-shaped tibble, ready for taxon/collector ID resolution and review before `add_specimens()`. Resolves `id_colnam` precisely via `SeniorCollectorPersonID` → `table_colnam.id_tropicos_person` when a connection is supplied (calls `call.mydb()` if `con` is `NULL`)
  - New `table_colnam.id_tropicos_person` column (migration in `inst/scripts/migrate_add_tropicos_person_id.R`) links collectors to their Tropicos Person ID; `match_tropicos_person_ids()` / `apply_tropicos_person_ids()` (fuzzy-match and backfill from an MBG collector spreadsheet, `inst/scripts/tropicos_collector_matching_and_export.R`) are archived, ad-hoc, unexported tools for maintaining that mapping, along with `write_tropicos_upload_table()` for writing the upload table to xlsx

* **`build_data_sources_table()`** — new exported helper (`R/citations_functions.R`) that pivots long-format trait data (with citation metadata) into a wide **citations × traits** table, one row per source and one column per trait containing the measurement count, plus a `n_taxa` column
  - Used by `query_plots()` (returned as `$data_sources` when `extract_traits = TRUE`) and all Shiny apps that display the Data Sources panel

* **`query_plots()` returns `$data_sources`** (`R/functions_manip_db.R`, `R/output_styles_helpers.R`) — when `extract_traits = TRUE`, the returned list now includes a `data_sources` element containing the citations × traits pivot table; this works for all output styles

* **`mod_citation_panel`** — Shiny module (`R/mod_citation_panel.R`) updated to display the citations × traits pivot as a searchable, sortable **DT datatable** (replacing per-citation cards); stats row now derives totals from the pivot columns
  - All callers (`mod_traits_enrichment`, `mod_taxa_traits_table`, `mod_results_display`) now call `build_data_sources_table()` instead of building a manual summary, eliminating the duplicate `group_by/summarise` blocks
  - `launch_query_plots_app()` extracts `data_sources` directly from the `query_plots()` result instead of making a second `query_taxa_traits()` DB call

* **Data Sources export** (`R/mod_results_display.R`) — the "Data Sources" table is now included in Excel, CSV, and RDS downloads when `extract_traits = TRUE`

* **Connection failures are now diagnosed instead of echoed** (`R/connection_diagnostics.R`) — connecting from an institutional network used to fail with nothing but `timeout expired`, which says nothing about the cause and reads like a database or password problem. It is almost always neither: the database listens on port 35699, and many institutional, campus and corporate networks (and some VPNs) allow only ports 80 and 443 outbound
  - `.classify_connect_error()` maps the libpq message to one of ten causes (`auth`, `dns`, `timeout`, `refused`, `unreachable`, `too_many_clients`, `no_database`, `ssl`, `server_closed`, `unknown`). `SSL SYSCALL error: EOF detected` is classified as a dropped connection rather than a certificate problem, since the remedy differs
  - `connect_database()` now says why each retry failed, and prints the cause, the target, the raw server message and a numbered list of remedies before giving up. The thrown error carries the diagnosis too, so it survives into bug reports
  - Applies to `create_pool_main()` / `create_pool_taxa()` as well

* **`check_db_network()`** — new exported function that answers "is it me, my network, or the server?" without credentials. It opens a raw TCP connection to the database host and port and, if that fails, a control connection to `cran.r-project.org:443`; that second probe is what separates a filtered network from no connectivity at all. Returns one of three verdicts — `reachable`, `port_blocked`, `no_connectivity` — each with the corresponding action. `db_diagnostic()` runs it automatically when both databases fail to connect

* **Shiny login reports the same diagnosis** (`R/mod_database_login.R`) — all four connection paths (main, taxa, and both public-user connections) print the full report to the console and add one translated sentence to the error box, so app users get an actionable message rather than raw driver text. Four EN/FR pairs added to `inst/translations/translation.json`, with a test asserting every hint string is present


* **`query_plots(verbose = ...)` cuts the console log down to what the caller acts on** (`R/verbosity.R`, `R/functions_manip_db.R`) — a full extraction narrates every internal step, and a query over sixteen plots scrolled roughly fifty lines past: connection notices, section headers, retry attempts and four separate "Query completed". The lines that matter — what came back, and what was silently dropped on the way — were buried among them
  - Three levels. `"normal"` (the new default) reports warnings, then closes with a summary of what was found, what was excluded and which tables the result holds. `"quiet"` reports warnings and failures only. `"debug"` prints the full log, exactly what earlier versions always printed. `TRUE` and `FALSE` are accepted as `"debug"` and `"quiet"`, and `options(CafriplotsR.verbose = "debug")` sets a session default
  - **No message was rewritten to achieve this.** Every `cli` call signals a `cli_message` condition carrying its alert type, so a calling handler dismisses whole severity classes with the `cli_message_handled` restart. The internals still report their work exactly as before; only what reaches the console changes, which is why the `"debug"` output is unchanged rather than reconstructed
  - **Every exclusion names the argument that controls it.** A count of removed rows is only actionable if you know which knob puts them back, so the summary reads `130 dead individuals, absent from the last census: census_strategy, or show_multiple_census = TRUE to keep every census` and `656 measurements flagged with an issue: issues = "remove", or issues = "include" to keep them`. `traits_to_genera = TRUE` and `wd_fam_level = TRUE` each add a line stating the consequence rather than the setting — that trait values are genus aggregates rather than the taxon's own, with provenance in the `source_*` columns
  - Counts computed deep in the pipeline reach the summary through a small internal tally, so muting the step log does not lose them. The census strategy is reported as its resolved value: `match.arg()` runs inside the implementation, so the wrapper still holds the raw `c("last", "first", "mean")` default until then
  - **Interactive prompts are never muted.** `choose_prompt()` and the four `.link_table()` call sites run inside `.verbose_output()`, which lifts the filter — an invisible question above a live `readline()` prompt is unanswerable
  - The chunking progress bar follows the same rule and appears at `"debug"` only; it is a step trace like the messages around it and leaves a full-width line in the scrollback
  - `query_plots()` is now a thin wrapper forwarding its 33 arguments to `.query_plots_impl()`, which is the previous function unchanged — a calling handler has to enclose an expression, and the body is some five hundred lines. A test asserts the two signatures match and that every formal is actually forwarded, so they cannot drift apart
  - `"ids removed - remove_ids = TRUE"` dropped from warning to info: it announces a documented default on every single call rather than reporting an anomaly. It is still there at `"debug"`
  - 103 assertions across 23 tests in `tests/testthat/test-verbosity.R`, covering which severities survive at each level, that base `message()` and `warning()` are left alone, that the level is restored after an error, and the end-to-end wrapper behaviour with the implementation mocked

### Bug Fixes

* **An unmatched collector made `query_specimens()` return every specimen** (`R/specimen_query_builder.R`) — a collector name that matched no row of `table_colnam` produced no SQL condition rather than an unsatisfiable one, so `query_specimens(collector = "Dauy")` fell through to a bare `SELECT * FROM specimens` and reported the whole table as a successful result. A typo in a name quietly meant "give me everything"
  - Both no-match branches, the exact lookup and the interactive `.link_table()` one, now return the unsatisfiable `FALSE` that the plot conditions have always used. The warning naming the unmatched collector is unchanged; what follows it is `No specimens found matching the criteria`
  - It stays unsatisfiable when combined with the other filters, so `collector = "Nobody", number_min = 10` no longer leaks the results of the number filter

* **192 interface strings had no French translation** (`inst/translations/translation.json`, `R/mod_step3_mapping.R`) — `shiny.i18n` falls back to the English key and warns `'...' translation does not exist` for anything absent from the translation file, so a French user met untranslated labels sitting in the middle of otherwise translated panels. Sweeping every `i18n$t()` and `i18n()$t()` call in `R/` against the file found 192 missing keys, not the two the warnings happened to name
  - French added for all of them — the backbone-selection prompt and the fuzzy-matching progress notice that raised the warnings, and whole panes that had been skipped: the observation standardisation step and its ontology messages, specimen link creation and preview, citation creation in the trait import, taxon synonymy and cascade updates, and the DataTables labels (`Show ... entries`, `Showing _START_ to _END_`) shared by the individual and specimen search tables
  - **One key was unmatchable rather than merely missing** — the notice at `R/mod_step3_mapping.R:924` carried the Latin-1 mojibake of `'%s' → '%s'`, three garbled characters where the arrow should be, left by a save through a Windows codepage. The translation file has held the correct arrow all along, so that lookup could never hit whatever was added. The source is repaired; the entry needed no change
  - Verified by re-running the sweep: every key the package asks for at runtime now resolves in both languages. The merge appended only, so no existing entry was reformatted or overwritten

* **An uploaded file already holding an `idtax_n` column crashed the matching app** (`R/mod_column_select.R`, `R/mod_auto_matching.R`) — `launch_taxonomic_match_app()` writes its results into the user's own table with a `left_join()`, so a file that already carried one of the output column names came back with `idtax_n.x` and `idtax_n.y` and no `idtax_n` at all. Every later step looks for the unsuffixed name, so the run died the moment automatic matching finished, with `object 'idtax_n' not found` raised from the Review tab. This is the ordinary case of re-standardising a table that a previous run had already annotated
  - The clash is now resolved once, in the column-selection module, before anything is joined: a user column named like a pipeline output is parked under an `_input` suffix (`idtax_n` → `idtax_n_input`, `idtax_n_input2` if that is taken too) and the user is told which columns were renamed. Their content is kept, not dropped
  - **The full set is protected, not just `idtax_n`** — `idtax_good_n`, `matched_name`, `match_method`, `match_score`, `is_synonym`, `accepted_name`, `corrected_name` and the five WCVP columns would each have failed the same way, some of them earlier and less legibly: a pre-existing `is_synonym` breaks the `corrected_name` mutation rather than the review step
  - **The rename happens upstream of the matching module, not inside it**, because the selected name column may itself be a reserved name. Renaming the data alone would leave `column_name` pointing at a column that no longer exists, and the traits enrichment module reads that same name; `mod_column_select_server()` returns both, so it is the one place they can be kept in step
  - Export follows the rename: `original_data` is now the post-selection table, so *exclude original columns* still drops a renamed column instead of leaving it behind. `taxonomic_name_combined` is excluded from that drop, which is what the previous wiring did by accident and is worth keeping — dropping it would leave the user with no trace of the name that was matched

* **443 links were typed `reference_plot` when they are `referenced_individual`** (`inst/migrations/reference_plot_mistyped_links.R`) — the migration above read the 74 rows carrying an `id_liste_plots` as the whole population of the `reference_plot` label. It was not. 517 rows carried the string, all written on 2026-01-06 in one session, under one label meaning two different things. The other 443 have an `id_n` and no plot: one specimen serving as the identification reference for several trees of one plot — specimen 39793 for four trees of somalomo002, specimen 39789 for seven of somalomo004. That is individual-level data, and it is what `referenced_individual` already means
  - The backfill phase reported them as anomalies and stamped them anyway, giving them a plot-scope type while they hold an `id_n` — the exact combination `.check_link_scope()` rejects. That phase now returns `FALSE` and stops instead, so a restored backup cannot repeat it
  - **The mistyping moved no determination.** Priority 10 only outranks a link whose `id_linktype` is NULL, and no individual holding a mistyped link also held one of those
  - **The correction is the step that could have moved one.** Retyping raises those links from 10 to 50, which cannot cost them a determination — `type_individual` still outranks at 100 — but turns a loss against an existing `referenced_individual` link into a tie broken by determination date. `report_reference_plot_mistyped_impact()` ranks every affected individual's links both ways and lists those whose winning specimen changes; it runs as phase 1 of the migration and reported none
  - Both `id_linktype` and `type` are set, since the free-text column is what the earlier backfill keyed on. The migration refuses outright if any row carries both an individual and a plot, where neither reading would apply
  - Migration `inst/migrations/reference_plot_mistyped_links.R`, `dry_run = TRUE` by default. **Applied 2026-09-01**, directly after the migration that caused it. Verified: no `reference_plot` row carries an `id_n`, 74 remain and every one has a plot, and `type` and `id_linktype` agree on every link in the table

* **The taxonomic matching vignettes documented columns and values the app has never produced** (`vignettes/taxonomic-app.Rmd`, `vignettes/taxonomic-app-fr.Rmd`) — nothing in the package misbehaved, but anyone writing code against the documented output was writing against a specification that did not exist. Each claim is now checked against `R/taxonomic_matching.R`, `R/mod_auto_matching.R`, `R/mod_name_review.R` and `R/mod_results_export.R`
  - **`match_method`** was documented as `exact_species`, `exact_genus`, `exact_family` and `exact_class`. The engine writes none of them: all four exact tiers record `exact`, and the rank that matched is held separately in `tax_level`. The values a user can actually see are `exact`, `genus_constrained`, `fuzzy` and `no_match` from matching, plus `manual` and `unresolved` set on the Review tab. A filter on `match_method == "exact_species"` silently returned nothing
  - **The WCVP columns were named wrongly.** The vignettes promised `wcvp_plant_name_id` and `wcvp_accepted_plant_name_id`; the app attaches `wcvp_taxon_name`, `wcvp_family`, `wcvp_taxon_authors` and `wcvp_taxon_status`. More consequentially, enabling WCVP **replaces `corrected_name`** with the WCVP name wherever WCVP holds the taxon, which nothing said — a reader could reasonably have believed `corrected_name` always came from the internal backbone. `name_source` records which reference supplied each value and is the column to check
  - **`family` and `genus` were listed as output columns.** They are not produced; the internal `tax_gen` and `tax_fam` never surface under those names
  - **The Export tab was said to offer a WCVP column group.** Three groups toggle — matched IDs, corrected names, match metadata. WCVP columns are appended when the option was enabled and travel with the export regardless
  - **The match-quality bands did not match the interface.** The prose gave 0.8 and 0.5 as the thresholds worth acting on, while the app colours from 90 % and 70 %, so a reader comparing the two saw different advice in each

* **The served app no longer offers an offline button that cannot work** (`R/shiny_app_taxonomic_match.R`) — `app_taxonomic_match()` passed `allow_offline = TRUE` unconditionally, but the button renders only when `cache_exists()`, which looks in `tools::R_user_dir("CafriplotsR", "cache")` on the machine running R. In a container that directory starts empty and `deployment/taxonomic_match/Dockerfile` never seeds it, so on a fresh pod the button was absent — then appeared, for everybody at once, as soon as any single visitor populated the cache in the shared single R process. The login screen therefore differed between visitors for no reason they could see
  - `allow_offline` is now `!.is_served()`. Nothing is lost by it: offline mode caches the backbone on the user's *own* machine to survive a bad link to the database, which a hosted app cannot do on a visitor's behalf, and working without an account is already covered there by the public read-only login

* **The public-access path is translated** (`inst/translations/translation.json`) — all eight of its strings were absent from the translation file, including the `"Connect as public user"` button itself, the `"Read-only access:"` heading and the notice saying what the account can and cannot do. `shiny.i18n` falls back to the key, so on the hosted app — which sets `CAFRI_LANGUAGE=fr` — the entire credential-free route rendered in English to a French-speaking visitor, on the one screen offering no other way in
  - The two offline strings whose French had been typed without accents (`referentiel`, `telecharger`) are repaired, and the sidebar's `"Output options"` heading, also untranslated, is added

* **The generated R snippet no longer prints the public account's credentials** (`R/mod_taxa_r_code.R`) — `.build_combined_taxa_code()` wrote `call.mydb(user = "CafriP_public", pass = "...")` into the copyable workflow script whenever the visitor had connected publicly, which handed the credential to every visitor of the hosted app and to anyone they passed the snippet on to. The snippet is a starting point for work in R, and work in R is done under one's own account, so both branches now show the interactive `call.mydb()` form; the public branch adds a line saying an account of one's own is needed to run it

* **`docs/PUBLIC_ACCESS_PLAN.html` removed from the published site**, along with its 12 entries in `docs/search.json`. It carried the public password in full and was served from the pkgdown site. The page also described an access model that the P0 remediation has since superseded

* **`description` could not be updated through `update_records()`** (`R/updates_tables_functions.R`) — `get_table_columns()` returns a hardcoded column list for `specimens`, and `description` was missing from it. `update_records(table_type = "specimens")` therefore reported it as an unrecognised column, dropped it, and — since it was usually the only column being written — aborted with "No updatable columns found in data". The column is now listed, bringing `update_records()` to full parity with `.specimen_editable_fields()`, the list the deprecated `update_specimen_fields()` accepted

* **The interactive matching prompt asked for a number without saying which one** (`R/link_table_functions.R`) — when a `query_plots()` filter value does not match exactly, `.find_cat()` prints the near misses and asks the user to pick one. The table it printed carried two bare integer columns: a row-number column called `ID`, and the lookup table's own key — `id_country`, `id_method`, `id_trait` — with nothing distinguishing them. `country = "Gab"` offered a list where both `ID` and `id_country` were plausible readings of "type a number"
  - The column to type is now called `Choice`, comes first, is bold, and the table is captioned "Type the number in the **Choice** column, not the id"
  - The searched column is shown under its own name (`country`, `method`, `trait`) instead of `comp_value`, `.find_cat()`'s internal name for it, and the `perfect_match` flag is no longer shown at all — it is always `FALSE` at this prompt, since an exact match never reaches it
  - **The prompt said "Type a number (1-10)" on every page**, but the numbering is continuous across pages: pressing ENTER on a 25-match list showed rows numbered 11-20, and 1-10 were no longer on screen. It now names the range actually displayed and says how many matches there are in total
  - The choice number still indexes `sorted_matches` exactly as before, so every caller that slices the returned table by it is unaffected. The display construction is split out into `.find_cat_display()`, which is testable without the `readline()` loop; 14 assertions in `tests/testthat/test-link-table.R`

* **A filter that matched nothing returned every plot instead of none** (`R/functions_manip_db.R`) — `PlotFilterBuilder$filter_country()` and `filter_method()` resolve a readable name to an id before filtering on it. When the name matched no row of `table_countries` or `methodslist`, both warned and then `return(self)` — dropping the condition entirely. `query_plots(country = "Atlantis")` therefore ran with no country condition at all and returned every plot the user was allowed to see, which reads as a successful query rather than as a mistake
  - Five paths were affected: the interactive and non-interactive branches of `filter_country()` and `filter_method()`, and the interactive branch of `filter_plot_name()`. All five now add an unsatisfiable condition through the new private `add_impossible()`, so an unmatched value returns no plots and the warning is borne out by the result
  - The non-interactive branches of `filter_plot_name()` and `filter_locality()` never had the bug: they match against `data_liste_plots` directly, so an unmatched name already yielded nothing

* **`try_open_postgres_table()` opened a second connection it never used** (`R/connections_db.R`) — the function took a connection as its `con` argument and then called `call.mydb()` on its first line, assigning the result to a local `mydb` that nothing read. On a session without cached credentials that meant a credential prompt (or a connection attempt) triggered by a function that had already been handed a working connection, including from inside `filter_country()`. The dead line is gone

* **`print_table()` no longer kills a Shiny app by taking over the RStudio Viewer** (`R/helpers.R`) — running any query in `launch_query_plots_app()` that returned fewer than 100 rows froze the app. The console showed the query succeeding — `Query completed`, `Found 1 plot(s)`, `Selected 1 plots: 1188` — and then `All connections closed and credentials cleared`, with the Results tab never rendering
  - `query_plots()` calls `print_table()` on its result whenever `nrow(res) < 100` (`R/functions_manip_db.R:688`). That builds a `kableExtra` HTML table and `print()`s it, which in RStudio navigates the Viewer pane — the pane the app is running in. The websocket closed, so the session died mid-flush: the observers already scheduled finished and logged, no output on the newly selected tab ever rendered, and `session$onSessionEnded()` ran `cleanup_connections()`. From the user's side the app simply stopped responding
  - `print_table()` now returns invisibly when `shiny::getDefaultReactiveDomain()` is non-`NULL`, so it prints from the console exactly as before and stays out of the way inside an app. The guard sits in `print_table()` rather than at the call site because the same trap was reachable from `query_taxa()` (`R/taxonomic_query_functions.R:702`, `:1034`) and the specimen queries (`R/functions_manip_db.R:3221`, `:3233`), which the taxonomic and specimen apps call

* **DT export buttons now actually render** (`R/mod_plot_metadata_viewer.R`, `R/mod_results_display.R`, `R/mod_taxa_search.R`, `R/mod_citation_panel.R`) — five tables set `dom = "Bfrtip"` and listed `buttons = c("copy", "csv", "excel")` without declaring `extensions = "Buttons"`. DataTables silently ignores an unregistered `B` in `dom`, so the copy/CSV/Excel buttons were never drawn and nobody got an error saying why
  - The column-documentation table also asked for `B` but configured no buttons; it gets the same copy/CSV/Excel set as its neighbours
  - `mod_citation_panel.R` spelled the argument `extension =`, which only worked through R's partial matching; corrected to `extensions =`

* **Feature Wizard — the same mode can be chosen again after changing the plot selection** (`R/mod_feat_step2_choose_mode.R`, `R/shiny_app_feature_wizard.R`) — selecting a plot, choosing a mode, going back to step 1, selecting another plot and choosing the *same* mode again left Next disabled with no way out. Choosing a different mode worked, which made it look arbitrary
  - The wizard clears its copy of the mode whenever the plot selection changes — rightly, since everything downstream was built for the old plots. Step 2 kept its own copy, so clicking the same card set a `reactiveVal` to the value it already held, which notifies nobody: the wizard never heard about the choice and `rv$operation_mode` stayed `NULL`, which is what `can_proceed()` reads. The step still displayed "Selected: Add Plot Features. Click Next to continue." beside a Next button that refused
  - Step 2 now takes a `reset` reactive and clears its selection, and its card highlighting, when the plot selection changes, so the next click is a real change. Step 3 clears prepared data on the same signal, where the same trap was waiting one step later: preparing identical data a second time (plots A → B → A) would not have reached the wizard either

* **Feature Wizard, Validation — the "already in the database" check now says which comparison it made** (`R/mod_feat_step5_validation.R`) — the check treats a row carrying a census and a row carrying none differently, but reported both under one message naming the census, which misdescribed half of what it found
  - A row with a census is a repeat only of a measurement recorded **during that same census**; a row with none — a position, a quadrat, anything the census link policy keeps off a campaign — is a repeat of **any** value recorded for that feature on that individual, there being no campaign to narrow it to. Both were already computed this way; only the reporting conflated them
  - The two are now counted and worded separately, and the preview's `issue` column says "already recorded in the database for this census" or "...for this individual" accordingly. The removal checkbox offers the union, as before
  - The matching moved out of the validation observer into `.existing_measurement_rows()`, which is vectorised and covered by 27 assertions. The per-row loop it replaces re-subset two data frames for every row: at the reported scale of 2,459 rows the whole comparison now takes 0.03 s
  - One EN/FR pair added to `inst/translations/translation.json`

* **Feature Wizard, Choose Mode — clicking an operation card now registers visibly** (`R/mod_feat_step2_choose_mode.R`, `R/shiny_app_feature_wizard.R`) — selecting a card set a 2px border and a pale tint, easy to miss among eight cards taller than the viewport, and the confirmation and the Next button both sat below the fold
  - The chosen card takes a 3px border, a tint, a drop shadow and a circled check mark in its corner; the other seven fade to 45% opacity with light grayscale, and restore on hover so re-choosing still reads normally. Card titles carry a permanent right padding so the badge never reflows the heading
  - The nav buttons are scrolled into view, but only when they are actually off screen, and after a short delay so the confirmation is on the page before the browser measures
  - The Next button pulses while the current step is satisfied — on every step, not only this one
  - `.mode_selection_js()` was extracted from the observer so the generated JavaScript can be asserted, including with `node --check`
  - No new translation strings: both confirmation strings already existed

* **Feature Wizard, Add Measurements — the "Link to census" box now reflects what was actually mapped** (`R/mod_feat_step3_measurements.R`) — the box pre-selected the latest census for every plot no matter what the file contained, so a position-only import arrived at the write step carrying a census it had no business carrying
  - Pre-selection is now driven by the policy: when none of the mapped features belong to a census nothing is pre-selected, and the box says so. The selection is only revised when the mapped set genuinely crosses that boundary, so a hand-picked census is never wiped by an unrelated dropdown change
  - Features the policy excludes are named under the selector — "Recorded for the tree itself, not attached to a census" — rather than being silently dropped later
  - `.apply_wide_mapping()` and `.apply_long_mapping()` clear the link on those rows before the data leaves the step, so what the screen shows and what is prepared agree
  - A collapsed "What linking to a census changes" panel states the three consequences, each traced to the code that causes it: one column per campaign from `aggregate_numeric_features_dt()`, the census date from `enrich_census_info()`, and pairing by `compute_growth()`, which greps `^stem_diameter_census_\d+$` and needs at least two
  - **Fixes a pre-existing blocker**: the Apply button lived inside the census selector, which rendered nothing when the selected plots had no census at all — measurements for those plots could not be prepared. It now always renders
  - Ten EN/FR pairs added to `inst/translations/translation.json`

* **Census links are now decided by policy on every write path** (`R/feature_census_link.R`, `R/mod_feat_step6_import.R`, `R/census_import_transaction.R`) — a measurement whose feature does not belong to a campaign is no longer stamped with `id_sub_plots`. Mapping a `quadrat` or a `position_x` in the Add Measurements step used to write a census-linked position, which `query_plots(show_multiple_census = TRUE)` then pivots into `quadrat_census_1`, `quadrat_census_2` ... repeating one unchanging value as though the stem had been re-located at each campaign
  - `.unlink_never_features()` clears the link on every row the policy calls `"never"` and names the features it left out; `.never_linked_features()` answers the same question for callers deciding what to *offer* rather than what to *write*
  - `.execute_measurements_import()` re-reads the policy from `traitlist.census_link` immediately before building its records, so the screen cannot decide it
  - `.execute_census_import()` now calls the shared helper instead of its own inline copy — the only path that already enforced this. Behaviour there is unchanged

* **`query_colnam()`** — fixed collector search in `launch_specimen_identification_app()` manual mode failing with `` `.con` is absent but must be supplied ``. The pattern-search branch built its SQL via `paste0()` and passed it to `glue::glue_sql()` without the required `.con` argument; it now uses proper `glue_sql()` parameter interpolation with `.con = mydb`, which also closes a SQL-injection gap (the collector name was previously spliced directly into the query string).

* **`update_ident_specimens()`** — applying an update in the specimen identification app (manual or batch mode) no longer appears to freeze the app. The function's internal `query_specimens()` calls used the default `show_html = TRUE`, which prints an HTML `kableExtra` table via the RStudio Viewer/browser; when the app itself runs in that same Viewer pane, this hijacked it away from the live Shiny session right after the database write succeeded, making the app look dead. Both calls now pass `show_html = FALSE`, matching the convention already used elsewhere in the app.

* **`output_styles_helpers.R` / `.extract_individuals_table()`** — `idtax_individual_f` is now always preserved in the individuals output table alongside `id_n`, regardless of output style. Previously this linking column was silently dropped during style processing, preventing downstream citation lookup in the Shiny app.

* **`mod_taxonomic_validator`** — clicking "Confirm Selection" no longer resets the Action column or misclassifies previously accepted rows as rejected. The fix separates the display data (`validated_data`) from the confirmed output (`final_validated_links`): confirming no longer overwrites the table's data, so row indices used by `user_decisions` remain stable.

* **`mod_link_executor` / `.add_link_specimens()`** — duplicate links (same `id_n` + `id_specimen` + `id_linktype`) are now prevented at two levels: internal duplicates within a new batch are removed before insertion, and the existing-DB check now matches on all three key columns instead of only `id_n` + `id_specimen`.


* **A trait not measured at the selected census vanished without a word** (`R/individual_features_function.R`) — `query_plots(show_multiple_census = FALSE)` keeps one census per plot, and `filter_to_census()` chooses it **per plot, across all traits at once**. Tree height is typically measured at the first census or two and not re-measured, so on a plot with four censuses every height row was dropped — and because the wide pivot builds columns from trait/census combinations that carry data, the result had no `tree_height` column at all rather than a column of `NA`. That is indistinguishable from a plot where height was never measured
  - The filtering itself is unchanged and deliberate: no value from another census is carried into a column labelled with this one. What was missing was any notice, so every dropped trait is now named, with the remedy on the next line: `Dropped 4 traits with no measurement at the selected (last) census: "crown_width", "flag5_rainfor", "height_of_first_branch", "tree_height"` / `Keep them with show_multiple_census = TRUE or census_strategy = "mean"`. At `"debug"` each trait also reports the censuses where it does exist
  - `filter_to_census()` now dates the census-linked measurements once and reuses that ordering for both the selection and the report, so the two cannot disagree. Row counts and the census chosen are identical to before
  - Height and diameter were never actually lost: `output_style = "full"` returns them for every census in the `height_diameter` table, which is built from an unfiltered fetch
  - 12 assertions across 7 tests in `tests/testthat/test-census-filter-dropped-traits.R`, including that the report stays silent when every trait survives and that non-census measurements pass through untouched

* **`update_records(table_type = "individuals")` now actually writes trait corrections instead of only reporting them** (`R/updates_tables_functions.R`) — `detect_feature_changes()` already resolved a trait-name column (e.g. `quadrat`) to the individual's current single measurement and reported how many rows would change, but `execute_feature_updates()` was a stub that printed "Feature updates not implemented" and did nothing. Correcting a trait therefore meant bypassing this path entirely: querying `query_individual_features(format = "long")` by hand, joining it back to find `id_trait_measures`, and calling `update_records(..., table_type = "individual_features")` on that instead
  - `detect_feature_changes()` now also resolves `id_trait_measures` and the trait's `valuetype` per individual, and returns the row-level comparison rather than a bare count. `execute_feature_updates()` feeds that straight into the existing `execute_direct_updates()` — the same backup-then-write path already used for `table_type = "individual_features"` — picking `traitvalue` vs `traitvalue_char` via `.upd_value_column()`
  - Scope is deliberately narrow: only an existing single measurement is corrected. An individual with no existing measurement for that trait is reported and left untouched rather than inserted, since that would need `id_sub_plots` and other context this path doesn't have — insert with `add_traits_measures()` instead. An individual with more than one measurement for the trait still aborts with the existing "value is an AGGREGATION" guidance, unchanged
  - Net effect: `update_records(data.frame(id_n = ..., quadrat = ...), table_type = "individuals", execute = TRUE)` now does the correction directly, with no manual `id_trait_measures` lookup required

### Infrastructure

* **The public account's password is no longer shipped in the package** (`R/public_credential.R`, `R/mod_database_login.R`) — `CafriP_public`'s credential was a literal at `R/mod_database_login.R:151-152` in a public repository, and was also rendered into the pkgdown site. The account is read-only and reaches data the project intends to publish, so the value was never a secret; what it could not be was *withdrawn*. The database runs on OVH Webcloud, where no per-role connection limit can be set (`inst/docs/PLAN_SECURITY_REMEDIATION.md`, P0.4), which leaves withdrawal as the only control over a published login exhausting `max_connections` — and a password compiled into the package stays valid in every installed copy until every user reinstalls
  - The package now ships a URL. `.public_credential()` resolves the credential at each app launch from `CAFRI_PUBLIC_USER` / `CAFRI_PUBLIC_PASS` if set, otherwise from a descriptor published at `https://umr-amap.github.io/cafriplotsR/public-access.json`. There is deliberately **no fallback value anywhere in the source**
  - **Rotating is now an edit to one file** — `pkgdown/assets/public-access.json`, mirrored into `docs/`, which GitHub Pages serves from `master` — picked up by every installation however old, rather than a release. Setting `"enabled": false` there is a kill switch that removes the button everywhere within five minutes; its `message` field is shown in its place. See `inst/public-access/README.md` for the descriptor, the publishing procedure and the rotation and withdrawal runbooks
  - **No exported signature changes and nothing to do for users.** `allow_public` still means the same thing. Public login is simply absent rather than broken whenever the credential cannot be resolved — no network, a blocked host, a malformed descriptor, or access switched off upstream. The separator and the "or" label moved into the same renderer, so an app that asked for public login is not left with a rule across an empty space
  - **The descriptor is world-readable, and that is the trade.** Serving it from the published site means the value sits in a public repository exactly as the old literal did; what changes is that it can now be rotated or withdrawn in one commit instead of one release. Keeping the value out of the repository entirely would mean hosting the descriptor off it — the URL is a single constant in `R/public_credential.R`, overridable with `options(CafriplotsR.public_access_url = ...)`
  - **Served deployments should set the environment variables.** They take precedence over the descriptor, so a hosted app never depends on GitHub Pages being reachable to let anyone in, and the credential never leaves the server. `deployment/taxonomic_match/` supplies them from a Kubernetes Secret named by `publicCredential.secretName`, injected by the existing post-install hook with `optional: true` — a missing Secret degrades to the descriptor rather than taking the pod down. It must not go in the image, which is world-pullable from ghcr.io
  - `curl` and `jsonlite` move to `Imports` (`jsonlite` was in `Suggests`); the descriptor is read on every launch of an app that offers public login
  - 26 assertions in `tests/testthat/test-public-credential.R`, including that the source tree contains no embedded credential, so a later edit cannot quietly restore one. None of them touch the network

### Documentation

* **`devtools::document()` runs clean again** (`R/functions_manip_db.R`, `R/add_functions.R`) — roxygen2 reported 44 problems on every run, from two causes. `add_method()` had a bare `@param new_description_method` with no description. The R6 classes carried a hand-written `@section Methods:` list on the class block, which roxygen2 does not read as documentation of the methods it generates topics for; the prose was moved into per-method `@description` / `@param` / `@return` tags, the pattern `PlotFetcher` already used and the reason it alone went unreported. Those classes have since been removed, but the same fix now applies to any R6 class added later

* **The taxonomic matching app explains itself** (`R/shiny_app_taxonomic_match.R`) — the *About this app* panel was a `<details>` element styled as a tinted info banner with an `info-circle` icon and collapsed by default, so it read as a static notice rather than a control. The workflow description it holds went unread by the first-time visitors it was written for
  - It now opens on arrival, and is restyled as a bordered card with a hover state and a chevron that rotates on open; the native disclosure triangle is suppressed in favour of the chevron. Collapsing it is still one click
  - The subtitle no longer paraphrases the title. It carries a sentence that had been sitting in `translation.json`, translated but referenced by nothing, since the panel was written — the one line that names both of the app's tasks: standardizing names, then enriching them with traits
  - An **Export** item joins the walkthrough, which had described every tab except that one. It points at the per-column descriptions the Export tab already renders rather than restating them. A lead-in names the reference the app matches against, and the panel closes with a link straight to this app's own vignette — to the French or the English copy, following the interface language, rather than to the site root a reader would then have to search from
  - `fluidPage()` gains a `title`, so the browser tab and any bookmark carry the app's name instead of the bare hostname. It is fixed at the app's initial language, because it lands in `<head>` when the UI is built and so cannot follow the in-app toggle

* **The taxonomic matching vignettes describe what the app actually does** (`vignettes/taxonomic-app.Rmd`, `vignettes/taxonomic-app-fr.Rmd`) — the walkthrough named every tab but described several of them at a level well below what the interface offers. Both languages gained, at parity:
  - **The backbone copy dialog**, which asks whether to match against the cached backbone or download a fresh one, and was documented nowhere. The guidance is the useful part: cached for ordinary use, fresh after taxa have been added or revised
  - **The two fuzzy stages, told apart.** The strategy was written as five tiers ending in one "fuzzy matching" step; there are in fact two, and they differ in how much they should be trusted. A genus-constrained match compares only against species in a genus already recognised, so at equal score it is worth more than a match drawn from the whole backbone
  - **How traits are aggregated.** A taxon carries many measurements of one trait, and the vignette described the categorical modes without ever saying what happens to numeric traits: they come back as mean, sd and **n**. Reading `n` first is the point — a mean over one measurement and a mean over forty are the same number carrying very different weight
  - **The Export tab's per-column descriptions**, which the app renders beside the preview and the vignette never mentioned, so readers had no idea the reference was already in front of them
  - **`id_data`**, the internal row identifier the app adds and strips again on export, and **when to enable WCVP** — including that the box must be ticked before matching, since the enrichment happens during that step

* **The vignettes tell public access apart from offline mode** (`vignettes/taxonomic-app.Rmd`, `vignettes/taxonomic-app-fr.Rmd`) — the *"Without credentials"* section was headed "public access mode" but described only offline mode, and never mentioned the public read-only account at all. It also told readers to click a button the hosted app does not show, under a French label that did not match the one in the interface
  - Both routes are now described separately: the public account as the one available everywhere, the hosted app included, and offline mode as local-only, with the reason it is local-only

* **The app catalogs name the hosted taxonomic app** (`vignettes/apps-overview.Rmd`, `vignettes/apps-overview-fr.Rmd`) — the taxonomic name standardization app has been deployed on SSP Cloud at <https://cafri-taxomatch.lab.sspcloud.fr> since the `deployment/taxonomic_match/` chart landed, and nothing outside that directory said so. A reader looking for a way to try the package without installing R had no reason to suspect one existed
  - Named twice per language: in the introduction, beside the note that three apps open without credentials, since "needs no account" and "needs no install" are the two questions a newcomer actually has; and in the app's own section, beside the offline note, where it matters that the hosted copy queries the same database and so returns the same `idtax_n` values as a local run
  - Left out of the *Which app do I need?* table, which answers a different question — which app, not where to run it — and has no column that would hold a URL without distorting it

* **`apps-overview` vignette, in English and French** (`vignettes/apps-overview.Rmd`, `vignettes/apps-overview-fr.Rmd`) — ten apps and no single page saying what each one is for. The overview groups them by what they do (explore and standardize, import and update, herbarium specimens), states for every app whether the public account suffices or your own is required, and spells out what public access covers and why the write apps do not offer it. Listed under Getting Started and Français - Démarrage

* **README — the access note now says what actually needs credentials** — the note claimed the database is restricted, full stop, which discourages newcomers and is not true: taxonomic standardization, backbone browsing and plot querying all run through the public account. Only inventory writes and per-account plot visibility are restricted. Reworded in `README.md`, `README.fr.md`, `README-fr.md` and `vignettes/readme-fr.Rmd`

* **pkgdown no longer publishes local working notes** — `pkgdown:::package_mds()` globs `*.md` at the package root and offers no way to exclude a file, so a mail draft, the workshop programme, a newsletter draft and the internal `CLAUDE.md` had all been rendered onto the public site; gitignoring them never helped, since pkgdown reads the working directory rather than the index. The notes moved to `inst/notes/`, which the glob does not reach, and `^inst/notes$` was added to `.Rbuildignore` so they are not installed with the package either. The pages already generated from them were deleted, along with their search-index and sitemap entries. `CLAUDE.md` must stay at the root to be read, so its generated pages are gitignored instead

* **Feature Wizard, Add Individual Measurements — "trait" reworded to "feature" throughout the step** (`R/mod_feat_step3_measurements.R`, `R/mod_feat_step2_choose_mode.R`) — what this step records is whatever is measured on a stem, and half of it is not a trait *sensu stricto*: `position_x` and `position_y`, a quadrat, a transect section. The rest of the wizard already says feature (Add Plot Features, the feature catalog, `query_individual_features()`), so the step now agrees with it
  - 26 user-facing strings reworded across the format chooser, the key-column panel, both mapping panels and the error notifications, plus the mode card in Choose Operation. 24 new EN/FR pairs added and the 21 strings left with no caller removed from `inst/translations/translation.json`
  - `features_field` keeps its own name: its radio label is now "Measured feature" vs "Measurement metadata", since the two roles sit in the same radio group and would otherwise be indistinguishable once the first was called a feature
  - Nothing changed but wording — trait names, `traits_field` / `features_field` arguments, element ids and the `traitlist` categories are untouched

* **`mod_step1_choose_type_ui()`** — reworded the taxonomic-standardization requirement checkbox to explain the `launch_taxonomic_match_app()` workflow more precisely (standardize the taxa list, keep original names, obtain `idtax_n`) and to correct the column-mapping guidance, which previously referred to a non-existent `idtax` column instead of `idtax_n`. French translation synced.

* **Newsletter vignettes** (`newsletter.Rmd`, `newsletter-fr.Rmd`) — copyedited the WCVP and aggregated-traits sections: dropped marketing language tied to the Barcelona presentation, fixed example calls, added a `get_wcvp_status()` snippet, and clarified the public-access/data-sovereignty note.

* **README — "Troubleshooting Connections"** — new section explaining the `timeout expired` failure that appears on institutional networks, why port 35699 is the reason, the three `check_db_network()` verdicts and the action for each, plus a table of the other common connection errors (rejected credentials, no free connection slots, DNS failure, dropped connection).


* **`census_strategy` documents that the census is chosen per plot, not per trait** (`R/functions_manip_db.R`) — the parameter said individuals recruited or dead outside the selected census "will have NA values", which describes a column that exists. It now states that one census is selected from all of a plot's measurements and every census-linked trait filtered to it, that a trait never measured there returns no column rather than a column of `NA`, that tree height is the common case, and that `height_diameter` under `output_style = "full"` still spans every census
  - `query_individual_features()` and `get_individual_aggregated_features()` had no `@param census_strategy` at all, despite both accepting it — an `R CMD check` warning waiting to happen. Both now document it

### New Features

* **`connect_cafri()` — single entry point for both databases** (`R/connections_db.R`)
  - Opens connections to the main (`plots_transects`) and taxa (`rainbio`) databases from one credential prompt instead of requiring separate `call.mydb()` and `call.mydb.taxa()` calls
  - Returns a list with `$main` and `$taxa` connections; both are also stashed in `.db_env` so existing package functions calling `call.mydb()` / `call.mydb.taxa()` later in the session reuse them transparently
  - Supports `taxa = FALSE` for sessions that only need the main database (taxa is opened lazily when a query needs it, still with cached credentials)
  - `call.mydb()` and `call.mydb.taxa()` remain available unchanged for backward compatibility

* **`.Renviron` credentials are now used by default** (`R/connections_db.R`)
  - `use_env_credentials = TRUE` is now the default on `connect_cafri()`, `connect_database()`, `call.mydb()`, `call.mydb.taxa()`, `create_pool_main()`, and `create_pool_taxa()`. Users who previously ran `setup_db_credentials()` no longer need to remember the flag — `MYDB_USER` / `MYDB_PASS` are picked up automatically when neither explicit nor cached credentials are available
  - The "Using stored credentials from environment" message is now emitted only once per session (cleared by `cleanup_connections()` and `remove_db_credentials()`)
  - Resolution priority is now: explicit `user`/`pass` arguments → cached session credentials → `.Renviron` → interactive prompt
  - Set `use_env_credentials = FALSE` to opt out and always prompt
  - New internal helpers `.get_env_credentials()` and `.resolve_credentials()` share the lookup logic between the connect and pool builders

* **Standardize free-text observations into mortality and dawkins traits** (`R/observations_standardization.R`, `R/mod_feat_step_observations.R`)
  - New `standardize_observations()` parses the free-text `observations` trait (id 13) and decodes `flag1_rainfor` (id 19) into two derived traits: `mortality_risk_flag` (multi-token; one DB row per matched token) and `dawkins_index` (id 15; never overwrites existing values)
  - Editable regex ontology shipped in `inst/ontology/observations_ontology.csv` (22 mortality tokens + 5 dawkins classes); `.gitignore` updated with `!inst/ontology/*.csv` exception so the file is tracked
  - PCRE word-boundary preprocessor `.fix_word_boundaries()` converts `\b` to unicode-safe `(?<![[:alpha:]])` / `(?![[:alpha:]])` so accented French tokens (cassé, déraciné) match correctly
  - `flag1_rainfor` letter codes are decoded via `.flag1_to_mortality_map()` and merged with text-derived tokens; source provenance is preserved in the comment (`"text: ..."` vs `"flag1_rainfor: b"`)
  - `bootstrap_mortality_risk_flag_trait()` inserts the new categorical trait into `traitlist` with 22 factor levels including the distinct `uprooted` category
  - Wizard integration: new "Standardize Observations" card in `launch_feature_wizard()` step 2 under a new **Derived / Computed Traits** section alongside Compute Stem Status; step 3 module (`mod_feat_step3_standardize_obs_*`) provides compute → review derived rows → review unresolved phrases → confirm flow; step 4 (lookup) is skipped; step 6 dispatches to `.execute_standardize_observations_import()`

* **Inspectable and customisable `output_style` for `query_plots()`** — built-in styles are now transparent and users can define their own
  - `list_output_styles()` returns a tibble summarising every built-in style (description, additional tables, column/pattern counts)
  - `get_output_style(name)` returns the configuration of a built-in style as a `plot_output_style` object with a dedicated print method that groups fields by purpose (column selection, pattern filters, renames, additional tables, flags); use `unclass()` to see the raw list
  - `output_style(...)` builds a custom style; pass `based_on = "<built-in>"` to inherit and override only the fields you care about (replace, not append, semantics — pass `character()` to clear a vector field while keeping the rest)
  - `validate_output_style()` checks the shape of a custom config; called automatically by the constructor and by `query_plots()` when a raw list is passed
  - `query_plots(output_style = ...)` accepts a built-in name string, a `plot_output_style` object, or a raw list. The `"permanent_plot" → "permanent_plot_multi_census"` auto-upgrade and the `census_pairs` override only apply to character input; custom objects are respected as-is
  - Custom styles live only in the current R session (no registry, no disk cache); to reuse, assign to a variable, `saveRDS()`, or put the constructor call in `.Rprofile`
  - Tests: 42 new unit tests in `tests/testthat/test-output-styles-custom.R`

* **Offline mode for `launch_taxonomic_match_app()`** — auto-matching and manual review now work without a live database connection
  - New "Use offline (cached backbone)" button on the login screen, shown automatically when a backbone cache is present on disk (`mod_database_login.R`)
  - `match_taxonomic_names()` gains a `backbone` parameter; when supplied (or when a cache exists and no `con` is given) all matching runs in R via `stringdist`'s trigram-Jaccard, mirroring PostgreSQL's `pg_trgm` `SIMILARITY()` — no network round-trips
  - Auto-matching, fuzzy suggestions, and the Review tab's custom search are all routed through the cached backbone in offline mode (`mod_auto_matching.R`, `mod_fuzzy_suggestions.R`, `mod_name_review.R`)
  - Traits enrichment and WCVP option are hidden when offline (require live DB)
  - Performance: R-side path is typically faster than the SQL-side per-name loop on slow connections (no network round-trips). For online users with a cache, matching also uses the R-side path by default
  - R-side trigram-Jaccard agrees with PostgreSQL `pg_trgm` `SIMILARITY()` at correlation ~0.99 (mean delta ~0.03) on representative names, so existing `min_similarity` thresholds carry over without retuning
  - Tests: 37 new unit tests in `tests/testthat/test-r-side-matching.R` against a synthetic backbone (no DB required)

* **Aggregated taxa traits from individual measurements** (`R/aggregate_individual_traits.R`)
  - New `rebuild_aggregated_taxa_traits()` aggregates individual-level measurements (`data_traits_measures`) into taxa-level rows in `taxa_traits_measures`, driven by declarative rules in a new `trait_aggregation_config` table
  - Config CRUD helpers: `add_trait_aggregation()`, `remove_trait_aggregation()`, `list_trait_aggregations()`
  - Aggregation kernel `.compute_aggregate()` supports `mean`, `median`, `min`, `max`, `sum`, `sd`, `percentile` (with `method_param`), `mode`, `concat`, `count`
  - Auto-derived target traits preserve the transformation method in the trait identity (e.g. `stem_diameter_p95`); pass `target_trait_id = source_trait_id` to keep the original trait name, or an explicit integer to write into a chosen trait
  - Aggregation is restricted to taxa identified at species or lower (configurable via `allowed_tax_levels`)
  - RLS-safe insert path via parametrised `INSERT` (replaces `dbWriteTable`/`COPY`, which PostgreSQL refuses on RLS-protected tables)
  - Citation `CafriplotsR_aggregated` (auto-managed, `is_public = FALSE`) tags all aggregated rows; `RESTRICTIVE` RLS policy hides them from the public role
  - Migration / rollback helpers in `inst/scripts/migrate_add_aggregated_traits.R`

* **`mod_extraction_config` — UI redesign with CSS-only tooltips and section cards**
  - Replaced dynamically-rendered UI with a static layout featuring coloured section cards (`.cfg-card`) and collapsible advanced options via native `<details>`
  - Added CSS-only question-mark tooltips (`.tip`) requiring no JavaScript

* **`mod_taxa_add` — WCVP backbone search integrated into Step 1**
  - `mod_taxa_add_server()` gains a `pool_main` parameter and new reactive values (`wcvp_results`, `wcvp_selected_id`, `wcvp_synonymy_candidates`)
  - Step 1 now searches both Tropicos and the WCVP backbone simultaneously; results are displayed in separate panels with distinct visual styling

* **WCVP backbone integration** — all major query functions now accept `backbone = "wcvp"` to use the World Checklist of Vascular Plants as an alternative to the internal taxonomy
  - New file `R/wcvp_integration.R` with schema setup (`setup_wcvp_schema()`), data import (`import_wcvp_names()`), taxon matching (`match_taxa_to_wcvp()`, `save_wcvp_links()`), lookup (`get_wcvp_names()`), and status utilities (`get_wcvp_status()`, `check_wcvp_update()`)
  - When `backbone = "wcvp"`, standard taxonomy columns (`tax_fam`, `tax_gen`, `tax_esp`, `tax_sp_level`, `tax_infra_level`, `tax_infra_level_auth`) are replaced in-place with WCVP values; two extra columns are added: `name_source` (`"wcvp"` or `"internal"`) and `alt_taxon_name` (the internal name preserved for reference)
  - WCVP ID columns `wcvp_plant_name_id` and `wcvp_accepted_plant_name_id` are added as analogs of `idtax_n` / `idtax_good_n`; internal IDs are never replaced
  - Taxa with no WCVP match fall back silently to the internal backbone with `name_source = "internal"`
  - `backbone` parameter propagated through: `query_taxa()`, `add_taxa_table_taxa()`, `resolve_taxon_synonyms()`, `merge_individuals_taxa()`, `query_plots()` / `process_individuals()`, `query_individual_features()`, `query_taxa_traits()`
  - `launch_taxonomic_match_app()`: WCVP option now appears in the backbone-selection modal when WCVP data is present in the taxa database; fixed availability check that was always returning `FALSE` due to a missing `is_current` field in `get_wcvp_status()` return value

* **`describe_columns()` — new function for documenting query result columns**
  - Reverse-maps every output column back to its database origin, accounting for output style renames (e.g. `ddlat` → `latitude`), census column renames (e.g. `stem_diameter_census_1` → `dbh_census_1`), and pivot suffixes (`_mean`, `_sd`, `char_`, `issue_agg_`, `_census_N`, `_0`/`_1` pairs)
  - Accepts a `plot_query_list` (from `query_plots()`) or a plain `data.frame`; returns a named list of documentation tables (one per result table) or a single table
  - Each documentation table has columns: `column_name`, `original_name`, `description`, `category`, `unit`, `notes`
  - `con` defaults to `NULL` and uses the active connection via `call.mydb()`, matching `query_plots()` behaviour
  - A dedicated **Column Documentation** tab is shown alongside the results tables in the `launch_query_plots()` Shiny app (replaces per-table collapsible panel); combines all tables' column docs into one filterable table with a leading *Table* column
  - Column documentation can be included in Excel, CSV (zip), and RDS exports via the *Select tables to include* checkbox (selected by default)

* **`query_plots()` new `census_pairs` output format for individual features**
  - New `individual_features_format = "census_pairs"` option produces one row per consecutive census pair per individual
  - Columns include `dbh_0`, `dbh_1`, `date_census_0`, `date_census_1`, `time` (days between censuses), and `stem_status` at the second census
  - Available in both the R function and the Query Plots Shiny app extraction config

* **`safe_delete_specimen_links()` — new function for removing individual–specimen links**
  - Dry-run mode (default) previews what would be deleted before any change is made
  - Selection by individual IDs, specimen ID, or direct link ID
  - Wrapped in a database transaction with rollback on error
  - Replaces the old internal `.delete_link_individual_specimen()`

* **`add_traits_measures()` — redesigned API for inserting individual-level trait measurements**
  - Clearer parameter names: `plot_name_col`, `tag_col`, `id_individual_col`
    (removed ambiguous `id_plot_name`, `id_tag_plot`, `individual_plot_field`)
  - Bulk insert via temp table + COPY protocol — tested on 112 000+ rows
  - `census_col` and `id_sub_plots_col` for flexible census linking

* **Feature Wizard Shiny app** (`launch_feature_wizard()`)
  - New 6-step guided wizard for adding features and census data to existing plots
  - Step 1: Login and multi-select plot selector with summary (reuses `mod_database_login`)
  - Step 2: Choose operation mode — New Census, Add Plot Features, Add Individual Measurements, Define Multi-Stems, or Add Recruits (redirects to Import Wizard)
  - Step 3 (plot features): Form or xlsx upload with column mapping for census metadata and arbitrary subplot features
  - Step 3 (measurements): xlsx upload with trait mapping grouped by category, showing description, unit, factor levels, and column content preview; supports wide and long formats
  - Step 3 (multi-stems): Upload or interactively define stem groups; enriches data by joining with DB to resolve `id_n`, `group_id_n`, and existing `stem_grouping`; shows all plot individuals alongside grouped ones for manual editing (remove, reassign, reset)
  - Step 4: Lookup matching for people columns (skipped for measurements and multi-stems)
  - Step 5: Validation with context-aware checks — duplicate detection (numeric traits only), previous census value comparison with `issue` column, issue summary table by trait; multi-stems uses pre-resolved IDs with "tag not found" as warning
  - Step 6: Import execution with dry-run support — bulk insert via single `dbAppendTable()` in explicit transaction for measurements; `update_records()` for multi-stem `stem_grouping` updates; context-aware labels (import vs update)
  - Full EN/FR internationalization

* **Taxonomic Matching app (`launch_taxonomic_match_app()`) — auto-matching checkpoint/resume**
  - Matching progress is saved to a temp file after each name; closing the browser mid-session no longer loses work
  - On next launch with the same dataset and column, the app detects the saved checkpoint and offers to resume from where it stopped or start fresh
  - Fuzzy matching loop exits immediately when the browser is closed (previously kept running in R until completion)

* **Taxonomic Matching app (`launch_taxonomic_match_app()`) WCVP integration refactored**
  - WCVP is no longer offered as a backbone selection option during auto-matching (removed from modal)
  - Only internal backbone is used for the matching process (automated via `mod_backbone_cache_selection`)
  - New sidebar checkbox "Use WCVP names in output" allows users to enrich matched results with WCVP taxonomic names when available
  - WCVP enrichment happens immediately after matching completes, so corrected names display WCVP values in Review and Export tabs
  - Gracefully falls back to internal names when WCVP data is unavailable
  - Checkbox appears conditionally only when WCVP tables exist in the taxa database

* **Taxonomic Backbone app (`launch_taxo_backbone_app()`) enhancements**
  - Browse & Search tab now displays **morphotaxon status** (Yes/No) in the selected taxon info panel
  - WCVP link information displayed when available: **WCVP ID**, **WCVP Status** (Accepted/Synonym), and **WCVP Name** from the WCVP database
  - Update Taxa tab now allows modifying the **morphotaxon status** via a checkbox in the "Other attributes" section
  - When toggling only morphotaxon status with no other field changes, the update now correctly bypasses the `update_dico_name()` call and applies the morpho_species change via direct SQL

* **`add_subplot_features()` people resolution**
  - For features with `valuetype == "table_colnam"` (team_leader, additional_people, etc.), comma-delimited person names are now split and matched to `table_colnam` IDs before insertion

* **`safe_delete_individuals()` specimen link cascade**
  - Now counts and deletes specimen links (`data_link_specimens`) in cascade when deleting individuals

* **`safe_delete_plot()` `delete_plot` parameter**
  - New `delete_plot = TRUE` parameter; set to `FALSE` to remove only individuals and their features while preserving all plot metadata and subplot features

### Bug Fixes

* **Authentication failures no longer cache a bad password** (`R/connections_db.R`)
  - Previously, a wrong password was cached in memory and every subsequent `call.mydb()` reused it, forcing users to discover the `reset = TRUE` flag
  - `connect_database()` now distinguishes authentication errors (`password authentication failed`, `role does not exist`, etc.) from network errors via the internal `.is_auth_error()` helper; on auth failure, cached credentials are cleared and the user is re-prompted automatically in interactive sessions
  - Network/transient errors retain the existing retry-with-backoff behaviour

* **Username is now prompted before password** in `connect_database()`, matching standard login-form ordering (previously password was asked first)

* **Import Wizard Step 3 — column auto-mapping with category-aware scoring**
  - Fixed "plot" column mapping to "plot_name" (direct) instead of feature "plot"
  - Fixed "subplot" column mapping to "quadrat" (individual feature) instead of no match
  - Replaced first-match-wins sequential strategy with category-aware scoring: all possible matches across exact/synonym/fuzzy are scored; direct/required columns get multiplier bonuses (2.0x for required direct, 1.5x for other direct, 1.0x for features)
  - Added word-boundary aware pattern matching in synonym resolution to prevent "plot" from matching within "subplot"
  - New function `.score_candidates()` evaluates all alternatives for each user column; alternatives stored in result for potential future UI enhancements
  - All schema columns remain available in Step 3 dropdown for user override (no columns hidden from choices)

* **Import Wizard synonym dictionary merging for individuals**
  - Fixed synonym dictionary merging to use `modifyList()` instead of `c()` when combining base column synonyms with trait-specific and feature-specific synonyms
  - This ensures trait-specific definitions (e.g., `stem_diameter` with "dbh" synonym) properly override base definitions
  - Fixes "dbh" column now correctly mapping to "stem_diameter" via synonym match

* **`query_plots()` dead/presumed_dead individual filtering at `census_strategy = "first"/"last"`**
  - When `show_multiple_census = FALSE` and `census_strategy` is `"first"` or `"last"`,
    individuals with `stem_status` of `"dead"` or `"presumed_dead"` at the selected census
    are now automatically removed from the result
  - A warning is emitted when no `stem_status` data is found (requires the stem_status
    workflow to have been run for the plot)
  - Applies to both wide and long `individual_features_format` output paths

* **`query_plots()` `stem_diameter = NA` with `show_multiple_census = FALSE`**
  - Older measurements stored without `id_table_liste_plots` were silently
    dropped during census filtering; fixed by coalescing the plot ID from the
    subplot table

* **`query_plots()` / `query_individual_features()` — consolidated issue-handling parameter**
  - Replaced the confusing pair `remove_obs_with_issue` + `include_issue` with a
    single `issues = c("remove", "include", "ignore")` parameter throughout the
    call stack (including Shiny apps)

* **`update_records()` — setting values to `NA`/`NULL` now detected and applied**
  - Change detection previously ignored rows where the new value is `NA`; now
    both "fill" and "clear" directions are handled
  - Single and batch execution paths both emit `SET col = NULL` correctly

* **`output_styles_config` — `census_date` added to permanent plot styles**
  - `census_date` now included in `individuals_columns` and `keep_patterns`
    for `permanent_plot` and `permanent_plot_multi_census` output styles

* **Import wizard `.row_idx` column leak**
  - Internal `.row_idx` column excluded from trait validation, data preview display, and xlsx/csv exports

* **`mod_taxa_add` unused `con` parameter**
  - Removed unused `con = pool()` argument from `query_taxa()` call that could cause errors

* **`register_user()` validation simplification**
  - Fixed registry table permissions (GRANT ALL to creator); removed redundant role existence check that could fail for users without `pg_roles` access

* **`output_styles_config` additional keep patterns**
  - Added `position_`, `strate`, `transect_part` to default keep_patterns for transect output

* **Import wizard subplot feature insertion type mismatches**
  - Fixed `add_subplot_features()` using bare `NA` in `ifelse()` statements, causing logical-to-numeric/character type errors
  - Now uses `NA_real_` and `NA_character_` for proper type matching with database schema

* **Import wizard map preview crash on non-numeric coordinates**
  - Fixed `mod_step6_preview.R` calling `abs()` on potentially non-numeric coordinate columns
  - Added coercion to numeric with proper fallback handling for invalid coordinates

* **Import transaction error messages unclear**
  - Improved error handling in import transactions to capture and display actual error messages instead of empty strings

* **`.link_table()` dynamic column selection failure**
  - Fixed `pull()` and `select()` using incorrect `{{}}` embrace syntax with string variable column names
  - Changed to `rlang::sym()` for proper symbol conversion in `pull()` and `select()` calls

* **Import wizard silent failure during subplot feature insertion**
  - Fixed `add_subplot_features()` missing `id_colnam` column in `data_to_add` tibble (required by `data_liste_sub_plots` schema)
  - Fixed `.link_colnam()` call using incorrect `id_field = "subplotype"` parameter (should be `id_field = "id_colnam"`)

* **Feature Wizard compute_stem_status mode column mismatches**
  - Fixed `compute_stem_vital_status()` using incorrect column name `p.id_table_liste_plots` in LEFT JOIN (correct: `p.id_liste_plots`)
  - Fixed step 1 plot selection module passing `id_liste_plots` but step 3 expecting `id_table_liste_plots`
  - Fixed step 3 result missing `tag` column in final output; now propagated through joins for display in review table
  - Fixed step 5 validation auto-validation for `compute_stem_status` mode missing `$summary` sub-list structure, causing "argument is of length zero" error
  - Added fallback: non-lookup features now get `id_colnam = NA_integer_` for proper database constraint handling

* **Import wizard `table_colnam` feature insertion with pre-matched IDs**
  - Fixed `add_subplot_features()` silent failure when Shiny import wizard Step 4 pre-matches person names to `id_table_colnam` IDs
  - When feature values are already numeric IDs (e.g., `"123, 456"` from wizard matching), bypass `.link_colnam()` to prevent it from trying to match ID strings as names and falling into an interactive `readline()` loop that hangs in Shiny
  - Added validation of pre-matched IDs against `table_colnam` before assignment
  - Non-numeric values still flow through `.link_colnam()` for interactive name-to-ID resolution

### Documentation

* Newsletter text refined (EN/FR): concise TWDD description, clarified citation tracking panel wording, added function names for interactive apps

* **Structured citation tracking for taxa-level trait measurements**
  - New `table_citations` table in the main database (`plots_transects`) with fields for authors, year, title, journal, DOI, URL, and dataset name
  - `migrate_add_citations_table()`: migration function to create the table and add `id_citation` FK column to `taxa_traits_measures`
  - `query_citations()`, `add_citation()`, `update_citation()`: CRUD functions for managing citation records
  - `export_taxa_traits_for_citation_backfill()` / `apply_citation_backfill()`: workflow for bulk-assigning citations to existing measurements via an exported Excel file
  - `grant_lookup_table_permissions()` now includes `table_citations` by default
  - `add_sp_traits_measures()` accepts `id_citation` to tag new measurements at import
  - `fetch_taxa_trait_measurements()` and `query_taxa_traits()` gain `include_citation = FALSE` parameter to join full citation metadata

* **Citation source panel in taxa traits enrichment module** (`shiny_app_taxo_match`)
  - New "Data Sources" tab in the trait enrichment results showing per-citation measurement/taxa/trait counts
  - Acknowledgement banner emphasising the importance of citing data sources
  - Excel downloads (wide and long format) include a `citations` sheet alongside trait data

* **Citation selector in taxa traits import app** (`shiny_app_taxa_traits_import`)
  - Dropdown to pick the citation for all rows being imported, with a "New citation" modal to create one on the fly

* **Multi-row selection in taxo backbone app** (`shiny_app_taxo_backbone`)
  - Search results table now supports multi-row selection (Ctrl/Cmd click)
  - Selected taxon panel lists all selected taxa when multiple are chosen; Update/Synonymy tabs act on the first selected
  - Tree view shows an explicit message instead of silently showing the first taxon when multiple are selected
  - Trait text panel redirects to "Extract as Table" when multiple taxa are selected

* **New module: taxa trait table extraction** (`mod_taxa_traits_table`)
  - "Extract as Table" button in the taxo backbone search panel fetches wide and long trait tables for selected taxa
  - Results include Wide Format, Long Format, and Data Sources tabs with per-citation cards
  - Excel downloads include a `citations` sheet

* **New module: equivalent R code preview** (`mod_taxa_r_code`)
  - Collapsible "Show Equivalent R Code" panel in the taxo backbone search section
  - Generates `query_taxa()` call matching current search filters (including `include_children`)
  - Generates `query_taxa_traits()` calls once "Extract as Table" is triggered
  - Produces a complete workflow script; when user is connected as public, includes the public connection credentials automatically
  - Notes for Shiny-only options (`include_synonyms`, `synonymy_filter`) that require post-processing in R

* **`include_children` parameter in `query_taxa()`**
  - New `include_children = FALSE` parameter recursively fetches all descendant taxa via the `id_parent` foreign key (up to 10 iterations)
  - New internal helper `.include_children()` used by both the name-based and ID-based query paths
  - The taxo backbone Shiny app now delegates child fetching to this parameter instead of a manual loop

### Bug Fixes

* **`pivot_numeric_traits()` namespace fix**
  - `str_remove()` qualified as `stringr::str_remove()` to avoid ambiguity when package is not attached

### Code Refactoring

* **`safe_delete_plot()` batch transactions**
  - Replaced single large transaction covering all deletions with per-batch transactions (batch size 2000)
  - Pre-resolves individual and trait-measure IDs before deletion to avoid expensive nested subqueries
  - Prevents lock timeouts on plots with large numbers of individuals

* **Output styles config**
  - Added `phenology` and `succession_guild` to default individual output columns

* **Public access login option in all Shiny apps**
  - `mod_database_login` now offers a "Connect as public user" button alongside the personal credentials form
  - Public connection uses a dedicated read-only database user (`CafriP_public`) with access restricted to taxonomy and taxa-level trait tables only — no plot data exposed
  - A yellow warning notice on the login panel informs users that public access is read-only and does not allow adding or modifying data
  - `mod_database_login_server()` returns a new `is_public` reactive so parent apps can adapt their UI accordingly
  - `shiny_app_taxo_backbone`: the existing write-permission check (`has_table_privilege`) automatically detects the public user and displays the amber "Read-Only Mode" badge — no additional changes required
  - Applies to both `launch_taxonomic_match_app()` and `launch_taxo_backbone_app()`

* **Taxa traits import Shiny app** (`shiny_app_taxa_traits_import()`)
  - New interactive app for importing taxa-level trait measurements into the database
  - Modules: column mapping (`mod_trait_column_mapping`), metadata mapping (`mod_trait_metadata_mapping`), validation (`mod_trait_validation`), preview & import (`mod_trait_preview_import`)
  - Supports dry run preview before committing data
  - Duplicate detection against existing database records
  - Transactional import: trait measures and features inserted atomically (single transaction)

* **Internationalization (i18n) on login module**
  - `mod_database_login` now includes an EN/FR language toggle at the login step
  - Language choice is synced to the main app language selector across all 7 Shiny apps
  - Checkbox "Use saved credentials" now correctly hidden when no saved credentials are detected

### Bug Fixes

* **`.traits_to_genera_aggreg()` incorrect `source` assignment**
  - Fixed hardcoded `source = "species"` for all non-NA trait values; traits assigned via a genus-level `idtax` were incorrectly labelled as species-level
  - `tax_level` (already present in `individuals` via `add_taxa_table_taxa`) is now carried through `dataset_subset` and used to set `source` correctly (`"species"` for species/infraspecific, the actual level otherwise)
  - Applies to both categorical and numeric trait paths
  - `tax_level` is dropped before `pivot_wider` to avoid column duplication on join-back

* **`register_user()` NULL parameter crash**
  - Fixed "Expected string vector of length 1" error when `institution` or `notes` are NULL; `glue_sql` requires length-1 values, so NULLs are now converted to `NA_character_` before SQL construction

* **`add_sp_traits_measures()` robustness improvements**
  - Added `con` parameter to accept an existing connection/pool instead of always calling `call.mydb.taxa()`
  - Fixed `else { new_data_renamed <- new_data }` branch that silently discarded the `idtax` column rename
  - Replaced deprecated `dplyr::filter_at()` / `dplyr::any_vars()` with `dplyr::if_any()`
  - Fixed `ifelse()` type coercion bug: numeric trait values were silently converted to character
  - Transaction now wraps both trait measures and features inserts (was committing before features)
  - `dbRollback()` errors no longer mask the original insertion error
  - Fixed `apply()` converting tibble rows to character vectors; replaced with `lapply()`

* **`add_sp_traits_measures_features()` fixes**
  - Added `in_transaction` parameter to prevent nested `dbBegin()` errors when called within an outer transaction
  - Added `interactive` parameter passthrough (was defaulting to `TRUE`, showing console prompts in Shiny)
  - Fixed `valuetype` variable name collision with `dplyr::select()`
  - Added `is.numeric()` guard on zero-value check to prevent NA crash with character trait columns

* **`.link_sp_trait()` range validation fix**
  - Added `!is.na()` guards on `minallowedvalue` / `maxallowedvalue` checks to prevent NA propagation crash when optional range limits are NULL

* **Trait table references migrated to main database**
  - `table_traits` → `traitlist`, `table_traits_measures` → `taxa_traits_measures` across all query, add, update, delete, and link functions
  - `mydb_taxa` → `mydb` for trait operations (traits now in `plots_transects` database)

### Code Refactoring

* Switched all trait operations from taxa database (`rainbio`) to main database (`plots_transects`)
* Updated `R/taxa_traits_function.R`, `R/delete_functions.R`, `R/updates_tables_functions.R`, `R/link_table_functions.R`, `R/individual_features_function.R`, `R/mod_taxa_add.R`, `R/mod_growth_form_selector.R`

---

# CafriplotsR 1.9.4 (Development)

### New Features

* **User management system** (`R/user_management.R`)
  - `create_user_registry()`: Create a `user_registry` table in the main database for tracking user metadata (email, institution, etc.)
  - `register_user()`: Add or update user metadata in the registry
  - `setup_user_permissions()`: Grant/configure user permissions on main and taxa databases
  - `get_registered_users()`: List registered users with their metadata
  - `get_user_emails()`: Retrieve user email addresses for communications
  - `deactivate_user()` / `reactivate_user()`: Manage user active status

* **Feature and trait categories**
  - New `category` parameter in `add_trait()` and `add_subplottype()` for grouping features in the UI
  - Import wizard (step 3) now shows a category selector when adding new traits or subplot features

* **Census summary columns in `query_plots()` metadata output**
  - New columns `n_census`, `first_census`, `last_census` in plot-level metadata
  - Exposed in all output styles

* **Grouped schema column dropdowns in import wizard**
  - `get_schema_choices_grouped()`: Builds optgroup-organized choices for column mapping dropdowns
  - Columns grouped by category, with most-similar matches sorted to the top per group

* **Apply fuzzy matches button in specimen lookup module**
  - Users can now review and confirm fuzzy collector/specimen matches before applying them

* **Long format output for individual features in `query_plots()`**
  - New `individual_features_format = c("wide", "long")` parameter (default: `"wide"`)
  - Wide format (existing behaviour): one row per individual with trait columns pivoted wide
  - Long format: one row per individual × measurement (`trait`, `traitvalue`, `traitvalue_char`, `valuetype`, `census_date`)
  - Census filtering (`census_strategy`, `show_multiple_census`) applies to both formats
  - `concatenate_stem = TRUE` is incompatible with long format and raises an informative error
  - Option exposed in the **Census Handling** section of the interactive query-plots Shiny app

* **Consistent `plot_id` column across all output styles**
  - `id_liste_plots` is now always renamed to `plot_id` in metadata output, regardless of output style
  - Enables reliable chaining: `query_plots(id_plot = metadata$metadata$plot_id, ...)`
  - `remove_patterns` regex updated to also preserve `id_liste_plots` (like `id_n`)

### Bug Fixes

* **Shiny app query-plots — results reset when query parameters change**
  - Going back to the query builder and changing filters, plot selection, or extraction options now clears the results section
  - Prevents stale extraction results from being displayed alongside a new metadata query

* **Shiny app query-plots — generated R code now reflects actual extraction**
  - Code preview captures plot IDs at extraction time (not live selection state)
  - Individuals code always uses `metadata$metadata$plot_id` when a metadata query preceded extraction, regardless of how many plots were selected
  - Fixed metadata viewer failing to find the `plot_id` column (was only checking legacy names `id_liste_plots` / `id_plot`)

### Documentation

* Updated vignettes (`using-query-plots.Rmd`, `using-query-plots-fr.Rmd`) with `individual_features_format` parameter in the **Census Handling** section

---

# CafriplotsR 1.9.3 (Development)

### New Features

* **Improved parameter naming in `query_plots()`**
  - New `extract_coordinates` parameter replaces `show_all_coordinates` for better clarity
  - More intuitive name better describes the action: extracting coordinate data from subplots
  - When TRUE, returns `coordinates` (raw data) and `coordinates_sf` (spatial features) in output list
  - Old parameter `show_all_coordinates` still works but shows deprecation warning
  - Will be removed in a future version (2.0.0)

* **Enhanced column mapping with pattern/substring synonym matching**
  - Column mapping now recognizes synonyms embedded in larger strings (e.g., "DBH [cm]" matches "dbh" → `stem_diameter`)
  - Normalizes both user columns and synonyms by removing special characters, brackets, spaces
  - Minimum 3-character synonym length to avoid false positives
  - Composite scoring: prioritizes longest synonym match (×100), then column similarity (×10) as tiebreaker
  - Dynamic confidence scoring (0.80-0.90) based on similarity for pattern matches
  - Works across all import types: plots, individuals, and traits
  - Dramatically improves auto-mapping success rate for datasets with unit annotations

* **Smart deduplication for duplicate column mappings**
  - Automatically detects when multiple user columns map to the same database column
  - Keeps only the best mapping based on priority: exact match > exact synonym > pattern synonym > fuzzy
  - Unmaps lower-quality duplicates (sets to skip) to prevent data conflicts
  - Console/log output shows which columns were kept and which were unmarked
  - Example: "original_tax_name", "Espece", "Espece N" all mapping to `original_tax_name` → keeps exact match, skips others
  - Prevents import errors from ambiguous column data sources

* **Similarity-based dropdown sorting in column mapping UI**
  - Import wizard dropdowns now show database columns sorted by similarity to user column name
  - Each user column gets its own relevance-ranked dropdown (not global alphabetical)
  - Most similar options appear first, making manual mapping intuitive
  - Uses same string similarity algorithm as fuzzy matching
  - Significantly improves UX for columns that weren't auto-mapped

* **Auto-fill missing taxonomy with Magnoliopsida**
  - Missing `idtax_n` values are automatically filled with 351190 (Magnoliopsida class)
  - Converts taxonomy validation errors to warnings for missing `idtax_n` and `original_tax_name`
  - Clear messaging: "Missing idtax_n are considered to be unidentified stems"
  - Allows import to proceed for unidentified individuals while providing placeholder taxonomy
  - Users can update taxonomy later when identification becomes available

* **Enhanced validation error messages with expected units**
  - Min/max range validation errors now include expected unit information
  - Example: "Trait 'height_of_stem_diameter' has 5930 value(s) above maximum allowed (30) (expected unit: m)"
  - Helps users quickly identify unit mismatches (cm vs m, mm vs cm, etc.)
  - Only appends unit info when trait has `expectedunit` defined in database
  - Reduces debugging time and prevents data import errors

* **Database backup and restore functions**
  - New `backup_database()` function creates timestamped PostgreSQL backups using pg_dump
  - Supports both main (`plots_transects`) and taxa (`rainbio`) databases
  - Backup files use format: `database_backup_YYYY-MM-DD_HH-MM-SS.dump`
  - Optional compression (enabled by default) for smaller file sizes
  - New `list_backups()` function shows all available backups with timestamps and sizes
  - New `restore_database()` function restores from backup with safety confirmations
  - New `cleanup_old_backups()` function removes backups older than specified days (with dry-run mode)
  - Proper Windows path handling using short path names (8.3 format) to avoid space issues
  - Secure password handling via PGPASSWORD environment variable
  - Requires PostgreSQL client tools (pg_dump/pg_restore) installed and in PATH

### Bug Fixes

* **Fixed missing `stringr::` namespace prefix in coordinate extraction**
  - Added `stringr::` prefix to `str_split()` calls in coordinate extraction code
  - Fixes "impossible de trouver la fonction 'str_split'" error in `query_plots()` with `show_all_coordinates = TRUE`
  - Affects `functions_manip_db.R` lines 351-354 in coordinate processing
  - `stringr` was already in package dependencies, just needed proper namespace usage

* **Fixed missing `purrr` dependency for coordinate extraction**
  - Added `purrr` to package Imports (required by `query_plots()` with `show_all_coordinates = TRUE`)
  - Fixed unnamespaced `map_chr()` calls to use `purrr::map_chr()` in coordinate processing
  - Resolves "dépendance 'tidytable' pas chargée (necessaire pour coordinates)" error message
  - Affects `functions_manip_db.R` coordinate extraction when querying subplot coordinates

### Code Refactoring

* **Refactored `generate_rmd_export_plot.R` script**
  - Better structure with clear configuration, validation, and processing sections
  - Comprehensive error handling for each quadrat and plot
  - Improved user feedback with `cli` package progress messages
  - Validates output directory and template existence before processing
  - Tracks results and errors for each operation
  - Final summary with counts of generated files and any errors
  - Optional cleanup of individual PDFs after merging
  - Remains as internal/non-exported script for user convenience

### Documentation

* **Added FUTURE_IMPROVEMENTS.md**
  - Documents enhancement idea for database-backed synonym system for traits
  - Currently trait synonyms are hardcoded in R; proposal to store in `table_traits` table
  - Includes implementation roadmap and related files for future development

# CafriplotsR 1.9.2 (Development)

### Bug Fixes

* **Fixed empty specimen handling in `merge_individuals_taxa()`**
  - When no specimens are linked to individuals, function was creating empty tibble without proper column structure
  - Caused "Column 'id_specimen' doesn't exist" error in `dplyr::select()`
  - Now creates empty tibble with correct column structure (id_specimen, idtax_specimen_f, colnam_specimen, colnbr, suffix)
  - Fixes error in `query_plots()` with `extract_individuals = TRUE` when plots have no linked specimens

* **Fixed connection retry logic for trait measurement features queries**
  - Replaced direct `DBI::dbGetQuery()` calls with `func_try_fetch()` in measurement features functions
  - Automatic retry (up to 10 attempts) when database connections are lost or timeout
  - Affected functions: `count_measurement_features()`, `fetch_measurement_features_raw()`, `fetch_taxa_trait_measurements()`, `pivot_table_references()`
  - Prevents "server closed the connection unexpectedly" errors in long-running Shiny sessions
  - Users no longer need to manually rerun queries after connection failures

* **Fixed duplicate rows when including measurement features**
  - `query_taxa_traits()` and `query_individual_features()` with `include_measurement_features = TRUE` were creating duplicate rows for same `id_trait_measures`
  - Root cause: Multiple feature records per measurement (e.g., separate records for `try_dataset_id` and `try_observation_id`) were being kept as separate rows
  - Modified `pivot_features_by_type()` and `pivot_table_references()` to aggregate features by `id_trait_measures` only
  - Multiple feature values are now properly concatenated with `|` separator in single row
  - Added safety check in `pivot_measurement_features()` to ensure one row per measurement

* **Fixed missing id_col in pivoted measurement features**
  - `id_ind_meas_feat` (individuals) and `id_taxa_trait_feat` (taxa) columns were lost during pivot operations
  - Modified `pivot_measurement_features()` to pre-aggregate feature IDs separately and join back after pivoting
  - Feature IDs are now properly preserved and concatenated when multiple features exist per measurement
  - Prevents "objet 'id_ind_meas_feat' introuvable" errors in downstream code

* **Fixed census-linked measurements exclusion in `query_individual_features()`**
  - When `include_multi_census = FALSE`, measurements linked to subplots/censuses (having `id_sub_plots`) were incorrectly excluded from results
  - Now properly aggregates census-linked measurements by individual when `include_multi_census = FALSE`
  - When `include_multi_census = TRUE`, still keeps separate rows for each subplot/census
  - Affects both numeric and character trait pivoting

* **Fixed R code generation bug in Shiny app for individual features**
  - `mod_code_preview` was always generating `individuals$extract$id_n` regardless of `output_style`
  - Now correctly generates `individuals$individuals$id_n` for standard output styles
  - Only uses `individuals$extract$id_n` when `output_style = "full"`
  - Prevents "Column 'id_n' not found" errors when copying generated code

* **Optimized `merge_individuals_taxa()` performance**
  - Was loading entire `table_idtax` synonym table (could be millions of rows) causing long delays
  - Now fetches individuals first, then loads only the synonyms needed for those specific taxa
  - Also optimized specimen and specimen-link queries to filter early
  - Added detailed progress indicators at each step
  - Dramatically improves query speed for large databases

* **Fixed timeout errors for large individual features queries**
  - Count query with huge IN clauses (>10,000 measurements) was causing "SSL SYSCALL error: EOF detected"
  - Now skips count query for very large datasets to avoid timeout
  - Reduced chunking threshold from 15,000 to 5,000 for more aggressive chunking
  - Added better progress indicators throughout the query process

### New Features

* **Comprehensive permission management for import wizard**
  - New `setup_import_wizard_permissions()` - one-command setup for all import permissions
  - New `grant_all_table_permissions()` - grant on ALL existing tables and sequences
  - New `grant_plot_insert_permissions()` - grant specific table permissions
  - New `diagnose_plot_permissions()` - diagnose permission issues
  - New `diagnose_add_person_setup()` - check if secure functions are available
  - Automatic sequence discovery - finds and grants permissions on all sequences for tables
  - Handles missing tables gracefully - skips non-existent tables without failing
  - Comprehensive error messages guide users through permission setup
  - Supports granting to specific users or PUBLIC (all users)
  - RLS policies remain intact - table permissions don't affect row-level security

* **Safe plot deletion with transaction support**
  - New `safe_delete_plot()` function for safely deleting plots and all related data
  - **Dry-run mode by default** - preview what will be deleted before actually deleting
  - Shows detailed counts: individuals, trait measurements, measurement features, subplots
  - Requires explicit confirmation (can be bypassed with `force = TRUE`)
  - Uses database transactions - rolls back all changes if any step fails
  - Correct cascade deletion order respects foreign key constraints:
    1. Measurement features → 2. Trait measurements → 3. Individuals → 4. Subplots → 5. Plot
  - Detailed progress logging at each step
  - Options to keep individuals or subplots if needed
  - Returns deletion summary for verification
  - See documentation: `?safe_delete_plot`

* **Secure function for adding people without INSERT permissions**
  - New `setup_add_person_function()` creates a PostgreSQL SECURITY DEFINER function
  - Database administrators run this once to enable all users to add people to `table_colnam`
  - Users without INSERT permission can now add people through the import wizard
  - Automatic fallback: tries secure function first, then direct INSERT if available
  - Improved error messages guide users to contact admin if permissions lacking
  - New functions: `add_person_to_db()`, `grant_lookup_table_permissions()`
  - See documentation: `?setup_add_person_function`

* **Improved user experience for individual features display**
  - Removed internal `id_ind_meas_feat` column from results when metadata is included
  - Added informative note in Shiny app explaining `id_data_individuals` corresponds to `id_n` in individuals table
  - Helps users understand how to join individual features with individuals data
  - Bilingual support (English/French)

* **Long format traits table in taxonomic match Shiny app**
  - Added tabbed interface to traits enrichment module in `launch_taxonomic_match_app()`
  - Two views now available after fetching traits:
    - **Wide Format (Aggregated)**: One row per taxon with trait columns (existing functionality preserved)
    - **Long Format (Detailed)**: One row per measurement with all metadata
  - Long format automatically includes:
    - Measurement remarks (`include_remarks = TRUE`)
    - Measurement features (`include_measurement_features = TRUE`)
    - Original input names, matched names, and corrected names for traceability
    - All measurement metadata (basisofrecord, traitdescription, expectedunit, etc.)
  - Separate download buttons for each format (`.xlsx` export)
  - Full bilingual support (English/French) with 9 new translations
  - Equivalent to calling `query_taxa_traits(format = "long", include_remarks = TRUE, include_measurement_features = TRUE)`

* **Individual Features Query in Plot Query App**
  - Added optional individual-level features extraction to `launch_query_plots_app()`
  - New collapsible configuration panel for querying individual features separately using `query_individual_features()`
  - Extracts features from already-loaded individuals data (uses `id_n` column)
  - Configurable parameters:
    - Trait selection (all traits or specific trait IDs)
    - Output format (wide with aggregation or long without aggregation)
    - Multi-census data inclusion
    - Measurement metadata inclusion
    - Census strategy
  - Results displayed in separate "Individual Features" tab
  - Included in all export formats (Excel, CSV, RDS)
  - Dynamic R code generation shows equivalent `query_individual_features()` call
  - Format explanations:
    - Wide format: one row per individual, measurements as columns (aggregated if multiple observations)
    - Long format: one row per measurement (complete representation, no aggregation)
  - Full bilingual support with 30+ new translations
  - Positioned logically after individual extraction button for intuitive workflow

# CafriplotsR 1.9.1 (2026-01-18)

### New Features

* **Census Information Module for Import Wizard**
  - New Step 8 in plot metadata import wizard for adding first census information
  - Automatically detects and displays people features (team_leader, principal_investigator, etc.) from imported plot metadata
  - Auto-prefills census date fields from plot database
  - Non-interactive census creation suitable for Shiny environment
  - Directly inserts people features into `data_subplot_feat` bypassing string-to-ID conversion issues
  - Shows read-only summary of people information that will be copied to census
  - Full bilingual support (English/French) with 42 new translations

* **Taxonomic Backbone Management App**
  - New interactive Shiny app `launch_taxo_backbone_app()` for managing the taxonomic reference database
  - Browse and search taxonomic entries at all hierarchical levels (family, genus, species, infraspecific)
  - Visualize full taxonomic hierarchy using the hybrid system (flat columns + id_parent tree structure)
  - Add new taxa with automatic parent linking and hierarchy validation
  - Modify existing taxa with automatic cascade updates to maintain consistency
  - Comprehensive synonymy management:
    - Set new synonymy relationships
    - Reverse synonymy (swap synonym with accepted name)
    - Cancel synonymy (make taxon independent)
  - Hierarchy consistency checking with automatic fixing of missing parent links
  - Full bilingual support (English/French) with shiny.i18n integration

* **Cascade Update System for Taxonomic Hierarchy**
  - When modifying upper taxonomic fields (tax_fam, tax_order, tax_famclass), changes automatically cascade to all descendants
  - Detects all affected taxa via id_parent relationships (recursive traversal)
  - Shows warning modal with list of affected descendants before execution
  - Automatically finds or creates upper taxon entries (e.g., creates "Asterales" order when referenced)
  - Updates both flat taxonomic columns AND id_parent relationships atomically
  - Maintains consistency between denormalized columns and hierarchical structure
  - Transaction-safe with automatic rollback on errors

* **Enhanced Hierarchy Consistency Checking**
  - `check_hierarchy_consistency()` now detects taxa with missing parent links
  - Six types of consistency checks:
    1. Species → genus mismatch
    2. Genus → family mismatch
    3. Family → order mismatch
    4. Order → class mismatch
    5. Infraspecific → species mismatch
    6. **NEW**: Missing parent links (taxa with upper fields but no id_parent)
  - `fix = TRUE` parameter automatically resolves all detected issues
  - Finds and links appropriate parent entries for orphaned taxa

* **Search with Child Taxa Inclusion**
  - New "Include child taxa" option in taxonomic search
  - Recursively retrieves all descendants via id_parent relationships
  - Example: Search "Fabaceae" → returns family + all genera + all species in that family
  - Supports up to 10 levels of recursion for safety
  - Automatically adds traits to all retrieved child taxa

* **Comprehensive Vignettes for Backbone Management**
  - Bilingual vignettes explaining the taxonomic backbone app:
    - English: `vignettes/taxonomic-backbone-app.Rmd`
    - French: `vignettes/taxonomic-backbone-app-fr.Rmd`
  - Detailed documentation of:
    - Hybrid taxonomic system architecture
    - All app features with step-by-step workflows
    - Cascade update examples with real scenarios
    - Best practices for taxonomy management
    - Troubleshooting common issues
  - Added to pkgdown site under "Tools" section

### Bug Fixes

* **Fixed Import Wizard Mapping Reset on New File Upload**
  - Column mappings now properly reset when user uploads a new file mid-workflow
  - Detects data changes by tracking column names between uploads
  - Resets user_modified_mappings and recreates observers when data structure changes
  - Prevents "indice hors limites" errors when old mappings reference non-existent columns

* **Fixed Census Module Input Initialization Crash**
  - Added proper NULL/length checks before comparing input values
  - Uses `shiny::req()` to wait for UI inputs to initialize before accessing them
  - Prevents "l'argument est de longueur nulle" (argument is of zero length) errors on module load

* **Fixed Exact Match Search Behavior**
  - Removed automatic fuzzy fallback when `exact_match = TRUE`
  - Now returns NULL when no exact match found instead of falling back to fuzzy matching
  - Changed default to fuzzy matching (exact match checkbox unchecked by default)

* **Fixed Synonym Priority in Search Results**
  - When duplicates exist (e.g., multiple "Fabaceae" entries), accepted taxa now appear first
  - SQL queries prioritize entries where `idtax_good_n IS NULL` (accepted names)
  - Fixed PostgreSQL `SELECT DISTINCT` + `ORDER BY` compatibility issue using subqueries
  - `check_synonymy = FALSE` now properly excludes synonyms instead of just skipping resolution

* **Fixed Pool Connection Errors**
  - Fixed "Not supported for pool objects" errors in multiple modules:
    - Cancel synonymy operation
    - Modify taxon operation
    - Search with include children option
  - Added proper `con` parameter passing to `update_dico_name()` calls
  - Added `tryCatch` wrappers for `poolReturn()` to handle "already returned" errors gracefully

* **Fixed NA Value Handling in Taxonomic Operations**
  - Created `na_to_empty()` helper function for safe NA/NULL handling
  - Fixed "valeur manquante là où TRUE / FALSE est requis" errors in:
    - Taxon modification form prefill
    - Taxon field change tracking
    - Taxon display rendering
  - Fixed hierarchy visualization crashes when viewing upper taxa (family, order, class)
  - Added safe NA checks in hierarchy tree building and breadcrumb path generation
  - Created `has_value()` helper for safe field validation with NA/NULL handling

### Code Refactoring

* **Enhanced `query_subplots()` Pool Connection Support**
  - Added `con` parameter to support pool connections in Shiny environments
  - Passes connection through to `query_plot_features()` for consistent connection handling
  - Prevents unnecessary connection creation when using connection pools

* **Non-Interactive Census Creation**
  - Refactored census addition to bypass `add_subplot_observations_feat()` string-to-ID conversion
  - Two-step process: creates census records first, then directly inserts people features into `data_subplot_feat`
  - Properly structures dataframe to match `data_subplot_feat` schema with all required columns
  - Uses `typevalue` column for census number storage (not a dedicated `census` column)

* **Improved Taxonomic Hierarchy Functions**
  - `get_taxon_hierarchy()` now uses safe NA handling throughout
  - `build_hierarchy_tree_html()` safely compares IDs with explicit NA checks
  - `build_breadcrumb_path()` validates current level before comparisons

* **Enhanced Synonym Management**
  - Reverse synonym feature with automatic cascade to other synonyms
  - Direct SQL updates for atomic synonym operations
  - Warns about affected taxa before executing changes

### Documentation

* **Updated Search UI Labels**
  - Changed "Binomial search" to "Name search (any taxonomic level)"
  - Updated placeholders to show family/genus/species examples
  - Clarified that search works for all taxonomic levels, not just binomials

* **Enhanced CLAUDE.md Guidelines**
  - Documented plot data storage architecture (flat columns vs lookup columns vs features)
  - Clarified `subplotype_list` feature type categories
  - Added examples for dynamic lookup feature identification

# CafriplotsR 1.9.0 (2026-01-09)

### New Features

* **Herbarium Specimen Linking System**
  - New interactive Shiny app `launch_individual_specimen_linking_app()` for linking herbarium specimens to individual trees
  - Creates formal database-level connections between inventory individuals and herbarium specimens
  - Enables automatic taxonomic updates: when specimens are revised by taxonomists, linked individuals inherit updated taxonomy
  - Supports two link types:
    - `type_individual`: Direct evidence (specimen collected FROM this specific tree)
    - `referenced_individual`: Indirect evidence (tree field-identified as same species as specimen tree)
  - Extends specimen utility: one specimen can provide taxonomic updates to multiple field-identified trees
  - Six-step workflow: Select individuals → Parse herbarium info → Match collectors → Retrieve specimens → Validate taxonomy → Create links
  - Comprehensive taxonomic validation with family/genus/species comparison and visual indicators
  - Full bilingual support (English/French) with shiny.i18n integration

* **Specimen Linking Documentation**
  - Comprehensive bilingual vignettes explaining specimen linking workflow and scientific rationale
  - English: `vignettes/specimen_linking_workflow.Rmd`
  - French: `vignettes/specimen_linking_workflow-fr.Rmd`
  - README.md section highlighting specimen linking as key feature for long-term data quality
  - French README vignette (`vignettes/readme-fr.Rmd`) updated with specimen linking section
  - Clear explanation of the two-column system (`herbarium_nbe_type` vs `herbarium_nbe_char`)
  - Rationale for extending specimen utility while acknowledging confidence trade-offs

* **Modular Specimen Linking Architecture**
  - New R6 classes for efficient batch querying:
    - `SpecimenFilterBuilder`: Build complex specimen queries with multiple filters
    - `SpecimenFetcher`: Execute batch specimen retrieval with connection pooling
  - New Shiny modules for specimen linking workflow:
    - `mod_herbarium_parser`: Parse herbarium references from text (collector names, specimen numbers)
    - `mod_specimen_retriever`: Batch-retrieve specimens by collector and number ranges
    - `mod_taxonomic_validator`: Validate taxonomic concordance with visual indicators
    - `mod_individual_search`: Search individuals with herbarium information
    - `mod_specimen_search`: Search specimens database
    - `mod_link_preview`: Preview and review proposed links before creation
    - `mod_link_executor`: Execute batch link creation with validation
  - Reusable components support both individual-specimen and specimen-only workflows

### Performance Improvements

* **Optimized Batch Validation**
  - Link validation now uses batch queries instead of row-by-row checks
  - Performance improvement: ~200x faster (3 queries vs 639 queries for 213 links)
  - Validates all specimen IDs, individual IDs, and link type IDs in parallel
  - Eliminates app hanging during validation step

* **Optimized Specimen Retrieval**
  - Batch retrieval by collector with min/max specimen number ranges
  - Instead of N individual queries (one per specimen), makes 1 query per collector
  - Example: 222 specimens from 3 collectors = 3 queries instead of 222

### Bug Fixes

* **Fixed Taxonomic Match Classification**
  - "Same Genus" and "Same Family" categories now correctly count links
  - Previously, links where genus AND species matched were incorrectly classified as "same_genus"
  - Now properly handles synonym cases: if genus+species both match even when idtax_n differs → classified as "exact"
  - Categories are now mutually exclusive: exact → same_genus (species differs) → same_family (genus differs) → different_family

* **Fixed Taxonomic Validation for Specimen Links**
  - Dynamically constructs full taxonomic names (`full_name_no_auth`) from base columns (`tax_gen`, `tax_esp`, `tax_nam01`)
  - Handles all taxonomic levels: infraspecific, species, genus
  - Fixes "Missing columns in taxa_info" error in validation step

### User Experience Improvements

* **Opt-Out Selection System for Link Validation**
  - All links now pre-selected by default (opt-out instead of opt-in)
  - Users can uncheck links to reject rather than checking 200+ links individually
  - New selection controls:
    - "Select All" - Re-select all links
    - "Deselect All" - Clear all selections
    - "Reject Different Family" - Auto-reject links with taxonomic family mismatches
  - Visual selection status column with ✓/✗ indicators and color coding
  - Interactive table: click rows to toggle selection

* **Prerequisites Information in Linking App**
  - Prominent yellow warning box explaining prerequisites before starting
  - Clear explanation of two column types (`herbarium_nbe_type` vs `herbarium_nbe_char`)
  - Distinguishes direct evidence (high confidence) from indirect evidence (lower confidence)
  - Explains rationale for extending specimen utility across multiple trees

### Database Schema

* **New Tables and Migrations**
  - `link_individual_specimen`: Stores specimen-individual links with audit trails
  - `linktypelist`: Lookup table for link types (type_individual, referenced_individual)
  - Migration functions for adding audit columns and link type tracking
  - Functions: `run_specimen_links_migration()`, `verify_specimen_links_migration()`

---

# CafriplotsR 1.8.2 (2026-01-05)

### Bug Fixes

* **Fixed multiple Import Wizard issues for individuals import**
  - Automatic column matching now includes trait/feature columns (stem_diameter, tree_height, etc.)
  - Fixed "objet de type 'closure' non indiçable" i18n error in lookup matching step when no lookups needed
  - Fixed "Not supported for pool objects" error in taxonomy validation by using dplyr instead of DBI::dbReadTable
  - Fixed "nombre de dimensions incorrect" error in dry run by detecting import type and calling correct import function
  - Fixed preview showing unmapped columns (like multi_tiges_id) - now only shows columns actually mapped by user
  - Preview module now correctly handles individuals import list structure (individuals + features data frames)
  - Import Step 7 now properly detects and handles both plots and individuals imports

* **Fixed validation issues for flexible data import**
  - Duplicate tags within plots now trigger warnings instead of errors (allows intentional duplicates)
  - Fixed taxa table name from incorrect "taxonomic_table" to correct "table_taxa"
  - Validation now only adds truly required columns, not all possible optional columns

* **Improved database connection resilience for laptop sleep/wake cycles**
  - Added connection validation every 60 seconds to detect stale connections
  - Added onActivate callback to validate connections before use
  - Prevents "SSL SYSCALL error: EOF detected" after laptop wakes from sleep
  - Applied to both main and taxa database connection pools

### New Features

* **Added French translations for Import Wizard validation section**
  - All validation messages, progress indicators, and UI labels now translated
  - Includes error messages, summary cards, and alert messages
  - Follows i18n best practices with i18n()$t() pattern in server functions

### Dependencies

* Added `plotly` to package Imports (required by plot statistics module in shiny_app_query_plots)

---

# CafriplotsR 1.8.1 (2025-12-12)

### New Features

* **Taxonomic backbone caching system for improved performance**
  - Local caching for taxonomic backbone dramatically improves performance with slow internet
  - After first download, subsequent app launches load backbone from cache (~1 second vs 5-30 seconds)
  - Performance improvement: 10-70x faster on subsequent runs
  - Interactive modal allows choosing between cached or fresh backbone
  - Cache displays age and file size for informed decision-making
  - Cache location: platform-appropriate user cache directory via `rappdirs`
  - New exported function `delete_backbone_cache()` for manual cache clearing
  - Added `rappdirs` package dependency

* **Row-level security improvements with `created_by` migration**
  - New `created_by` column tracks which user created each plot
  - Migration function `migrate_add_created_by()` safely adds column and updates policies
  - Simplified import function: automatic privilege management
  - Users automatically get access to plots they create
  - Function `check_created_by_migration()` checks migration status

* **Enhanced `define_user_policy()` with automatic privilege grants**
  - Automatically grants necessary table privileges (SELECT, INSERT, UPDATE) when creating policies
  - Eliminates manual privilege management for administrators
  - Covers all relevant tables: plots, subplots, individuals, features, traits

* **Improved plot ID query with multi-column matching**
  - Plot retrieval now matches on multiple identifying columns beyond just `id_table_liste_plots`
  - Handles `admin_code`, `plot_code`, and `plot_name` for flexible querying
  - Reduces need to know exact internal IDs

### Bug Fixes

* **Fixed `query_plots()` ignoring provided database connections**
  - Function now properly respects `con` parameter when provided
  - Prevents unnecessary connection creation when using connection pools
  - Critical for Shiny apps using reactive database connections

* **Fixed `.link_subplotype()` missing connection parameter**
  - Function now accepts and uses provided database connection
  - Ensures transaction consistency during imports
  - Prevents connection errors in multi-step workflows

* **Fixed multiple RLS policy issues preventing imports**
  - INSERT operations now work correctly for non-admin users
  - RETURNING clause properly returns inserted plot IDs despite RLS restrictions
  - SELECT policy adjusted to allow RETURNING without exposing other users' data
  - All users can now import plots regardless of RLS configuration

* **Fixed Import Wizard lookup matcher performance issues**
  - Eliminated UI freeze when matching large lookup tables
  - Enhanced name matching with better fuzzy algorithms
  - Improved responsiveness during interactive matching

* **Fixed Import Wizard validation and conversion issues**
  - Exact-matched lookup values now properly converted to IDs before validation
  - Prevents validation errors for values that were successfully matched
  - Duplicate column mapping now detected and prevented in Step 3

* **Fixed multiple i18n reactive call errors in Import Wizard**
  - Corrected reactive i18n calls in Step 1, Step 2, and other modules
  - Fixed missing i18n parameters in module calls
  - Removed duplicate translations from translation.json

* **Fixed graphics parameter error in mapview map creation**
  - Added error handling for graphics device issues
  - Required BIOMASS >= 2.2.4 to prevent `par()` parameter errors
  - Graceful fallback when map creation fails

### Documentation

* **Added critical security warnings for database credentials**
  - CLAUDE.md now includes prominent warnings about credential management
  - Clear guidelines for using placeholder credentials in examples
  - Instructions for credential leak response

* **Updated Import Wizard documentation**
  - Vignettes restructured to feature Shiny Import Wizard prominently
  - Added i18n and translation management guidelines to CLAUDE.md

* **Added package citation information**
  - Citation section added to README files (EN and FR)
  - Updated DESCRIPTION with proper author roles and ORCID
  - Package now citable via `citation("CafriplotsR")`

### Infrastructure

* **Cleaned up version control**
  - Removed xlsx, csv, gpkg data files from tracking
  - Removed geospatial temporary and KML files
  - Removed R Markdown cache and generated files
  - Removed RStudio Connect deployment files
  - Added working .Rmd files to .gitignore to prevent credential leaks
  - Added .Rprofile to .gitignore

* **GitHub Actions workflows**
  - Added Claude Code Review workflow for automated code review
  - Added Claude PR Assistant workflow for pull request automation

### Code Refactoring

* **Improved Step 7 messaging in Import Wizard**
  - Clearer explanations of row-level security and access models
  - Better guidance on admin contact for access grants
  - Updated UI based on user feedback

---

# CafriplotsR 1.8.0 (2025-01-15)

### New Features

* **Interactive Import Wizard Shiny App - Complete 7-Step Workflow**
  - New `launch_import_wizard()` function provides comprehensive plot metadata import interface
  - Full internationalization support (English/French)
  - Reuses existing validation and import functions for consistency
  - See version 1.7.2 entries below for detailed step-by-step feature descriptions

* **Plot Statistics & Visualizations module**
  - New "Statistics" tab in `launch_query_plots_app()` with comprehensive summaries
  - Summary statistics: number of plots, individuals, species, families
  - Diameter statistics: mean, median, min, max
  - Interactive visualizations with ggplot2 + plotly:
    - Diameter distribution histogram with hover tooltips
    - Top N species composition bar chart (adjustable slider: 5-30 species)
  - Download summary statistics as CSV
  - Fully bilingual (EN/FR) with i18n support
  - Smart column mapping adapts to different output styles

* **Code preview and export features**
  - Query plots app now includes code preview for reproducibility
  - Generated R code can be copied or downloaded
  - Helps users transition from GUI to programmatic workflows

### Bug Fixes

* **Fixed census date parsing error with missing/invalid data**
  - Gracefully handles missing year/month values in census dates
  - Prevents errors when date components are NA or invalid
  - Returns NA for unparseable dates instead of crashing

### Infrastructure

* **Added stringdist package dependency**
  - Required for fuzzy string matching in import wizard
  - Fixes import wizard initialization errors

* **Updated pkgdown website**
  - Documentation website updated with latest function references
  - Added logo to package site
  - All vignettes updated with new features

---

# CafriplotsR 1.7.2 (2024-12-15)

### New Features

* **Import Wizard: Duplicate plot detection during validation**
  - New validation step checks for potential duplicate plots in database
  - Matches on method, country, and coordinates (rounded to 3 decimals ≈ 111m precision)
  - Prevents re-importing existing plots with different names (e.g., "FND32" vs "Releve32")
  - Returns warnings (not errors) with existing plot names for user awareness
  - Respects row-level security (only checks plots user can access)

* **Import Wizard: UTM coordinate detection and conversion**
  - Two-stage detection system for UTM coordinates:
    - **Stage 1 (Validation)**: Detects coordinates > 1000 with specific error message
    - **Stage 2 (Preview)**: Interactive converter with zone and hemisphere inputs
  - Uses sf package for coordinate transformation (EPSG 326XX/327XX → 4326)
  - Converts in-place and updates preview map automatically
  - Guides users to convert rather than rejecting data

* **Import Wizard: Interactive map preview in Step 6**
  - Leaflet map displays plot locations with marker clustering
  - Auto-zoom to plot extent
  - Clickable popups show plot details (name, coordinates, method, country)
  - Warnings for invalid coordinates and unusual ranges
  - Info box guides users to fix reversed lat/lon by switching column mappings
  - Helps catch common errors: reversed coordinates, wrong coordinate system

* **Interactive Import Wizard Shiny App for Plot Metadata**
  - New `launch_import_wizard()` function provides comprehensive 7-step workflow for importing plot metadata
  - **Step 1: Choose Type** - Select import type (plots or individuals)
  - **Step 2: Upload Data** - Upload Excel/CSV files or download template
  - **Step 3: Map Columns** - Intelligent fuzzy column mapping with confidence scores and descriptions
  - **Step 4: Match Lookups** - Proactive lookup matching before validation
    - Analyzes all lookup columns (method, country, people fields)
    - Identifies exact matches vs. values needing matching
    - Interactive fuzzy matching with similarity scores for all possibilities
    - Displays method descriptions when selecting matches
    - Supports comma-separated people values (e.g., "Gilles Dauby, Hugo Leblanc")
    - Each person matched individually and aggregated back
  - **Step 5: Validate** - Comprehensive data validation using matched values
    - Validates both IDs (after matching) and names (before matching)
    - Clear error reporting with actionable messages
  - **Step 6: Preview** - Preview cleaned data with readable names
    - Displays lookup names instead of IDs for user-friendly preview
    - Handles comma-separated people fields (shows aggregated names)
    - Download enriched data as Excel or CSV with human-readable values
  - **Step 7: Execute Import** - Live import with transaction support
    - Dry run mode for testing without database changes
    - Uses existing `import_plot_metadata()` function
    - Displays admin code for row-level security access
    - Copy to clipboard and download as .R file options
  - New module files: `R/mod_step1_choose_type.R`, `R/mod_step2_upload.R`, `R/mod_step3_mapping.R`, `R/mod_step4_lookup_matching.R`, `R/mod_step5_validation.R`, `R/mod_step6_preview.R`, `R/mod_step7_import.R`, `R/mod_lookup_matcher.R`
  - Reuses existing validation and import functions for consistency
  - Full connection pool support with proper cleanup

* **Plot Statistics & Visualizations module for Query Plots Shiny App**
  - New "Statistics" tab in `launch_query_plots_app()` displays comprehensive plot statistics
  - Smart column mapping system automatically adapts to different output styles (`minimal`, `standard`, `permanent_plot`, etc.)
  - Detects column names regardless of renaming (e.g., `stem_diameter`/`dbh`/`D`, `tax_sp_level`/`species`)
  - Summary statistics cards: number of plots, individuals, species (richness), families
  - Diameter statistics: mean, median, min, max (when diameter data available)
  - Interactive visualizations using `ggplot2` + `plotly`:
    - Diameter distribution histogram with hover tooltips
    - Top N species composition bar chart (adjustable slider: 5-30 species)
  - Graceful degradation: sections automatically hide when data unavailable
  - Download summary statistics as CSV
  - Fully bilingual (EN/FR) with integrated i18n support
  - New module files: `R/mod_plot_statistics.R` (UI/server functions)
  - Added 24 translation strings to `inst/translations/translation.json`
  - Designed for easy expansion (guild analysis, height-diameter plots, basal area, etc.)

* **Bilingual support for Taxonomic Matching Shiny App**
  - Implemented full internationalization using shiny.i18n package
  - French and English interfaces with instant language switching
  - French is now the default language
  - Migrated all 7 modules to use centralized i18n translation system:
    - `mod_auto_matching` - Auto matching tab
    - `mod_column_select` - Column selection
    - `mod_data_input` - Data upload/input
    - `mod_fuzzy_suggestions` - Fuzzy match suggestions
    - `mod_name_review` - Manual review interface
    - `mod_results_export` - Results export
    - `mod_traits_enrichment` - Traits enrichment
  - Added `utils_i18n.R` with `init_translator()` and translation utilities
  - Created `inst/translations/translation.json` with comprehensive translations
  - Language toggle located in top-right corner of app
  - Set via `launch_taxonomic_match_app(language = "fr")` or `language = "en"`

* **Taxonomic Matching: Backbone caching system for improved performance**
  - Added local caching for taxonomic backbone to dramatically improve performance with slow internet connections
  - After first download, subsequent app uses load backbone from local cache (~1 second) instead of downloading from database (5-30 seconds)
  - Performance improvement: 10-70x faster on subsequent runs
  - Users can choose between cached or fresh backbone via modal dialog
  - Cache displays age (e.g., "3 days ago") and file size for informed decision-making
  - Cache location: `rappdirs::user_cache_dir('CafriplotsR')` (platform-appropriate)
  - New exported function `delete_backbone_cache()` allows manual cache clearing
  - Added `rappdirs` package dependency

* **Taxonomic Matching: Improved similarity threshold UI**
  - Similarity threshold now displays as percentage (0-100%) instead of decimal (0-1)
  - Default value: 60% (previously 0.6)
  - More explicit help text: "Minimum similarity percentage for fuzzy matching. Names with similarity below this threshold will not be matched. Higher values = more strict matching (fewer but more accurate matches)."
  - Label updated to "Minimum similarity (%)" for clarity
  - Internal calculations still use decimal format (0-1) for compatibility with matching algorithms

### Bug Fixes

* **Taxonomic Matching: Fixed progress bar not updating after manual review**
  - Progress bar calculation now correctly includes manually reviewed names in completion percentage
  - Previously only counted automatically matched names (exact/genus/fuzzy), causing progress to appear stuck at ~60% even after reviewing all remaining names
  - Connected progress tracker to review module results instead of auto-matching results
  - Added "Manually reviewed" line (displayed in green) to progress breakdown when manual reviews are performed
  - Progress now correctly shows 100% when all names are either auto-matched or manually reviewed

* **Import Wizard: Fixed mixed IDs and names in people column preview**
  - Preview was showing mix of numeric IDs (169, 85) and text names (Théophile Ayol)
  - Now processes each value individually in comma-separated lists
  - Converts numeric IDs to names, keeps text names as-is
  - Applies to all lookup columns (method, country, people fields)

* **Import Wizard: Fixed skipped columns still appearing in preview**
  - Columns set to "skip" in column mapping were still appearing in data preview
  - Now properly filters out NA mappings in both validation and preview steps
  - Removed skipped columns from data before processing

* **Import Wizard: Fixed mapping state not persisting on navigation**
  - Manual changes (e.g., skipping columns) were lost when navigating back
  - Added persistent `user_modified_mappings` ReactiveVal to track changes
  - Observers now track dropdown changes without cascade triggering
  - Returns both `mappings` (no NAs) and `mappings_with_skips` (includes NAs)

* **Fixed import failing with unmapped columns**
  - Import now filters out columns that were skipped during column mapping
  - Prevents "ERROR: column 'X' of relation 'data_liste_plots' does not exist"
  - Only mapped columns are included in database insertion

* **Fixed type mismatch error when joining subplot features**
  - Added explicit character type conversion for `plot_name` before joins
  - Prevents "Can't join due to incompatible types" errors
  - Applies to both people features and other subplot features

* **Fixed `poolWithTransaction()` warning in Shiny app imports**
  - Import now properly handles connection pools by checking out dedicated connections
  - Uses `pool::poolCheckout()` and `pool::poolReturn()` for proper pool management
  - Eliminates "Please use `poolWithTransaction()` instead" warnings

* **Fixed validation errors after lookup matching**
  - Validation now correctly handles both IDs (after Step 4 matching) and names (before matching)
  - Detects if values are numeric (IDs) or character (names) and validates accordingly
  - Applies to method, country, and people column validation

* **Fixed preview and downloads showing IDs instead of names**
  - Preview now enriches lookup columns by replacing IDs with readable names
  - Method IDs → Method names (e.g., 18 → "Plot_40x40")
  - Country IDs → Country names (e.g., 5 → "Cameroon")
  - People IDs → People names with comma-separated aggregation
  - Excel and CSV downloads export enriched data with names

* **Fixed `query_individual_features()` ignoring `trait_ids` parameter with large datasets**
  - When querying more than 1000 individuals, the chunking mechanism was not passing `trait_ids` filter
  - This caused all traits to be returned instead of only the requested ones
  - Fixed by passing `trait_ids` parameter through `fetch_with_chunking()` to `build_trait_query()`
  - Affects `individual_features_function.R:1319` and `individual_features_function.R:1270`

### Documentation

* **Updated Taxonomic App vignettes for bilingual support**
  - English vignette (`taxonomic-app.Rmd`) now documents language switching feature
  - French vignette (`taxonomic-app-fr.Rmd`) includes parallel documentation
  - Documented EN/FR toggle button usage
  - Documented programmatic language selection
  - Noted French as default language

### Code Refactoring

* **Removed obsolete `table_taxa_tb` dataset**
  - Deleted `data/table_taxa_tb.RData` and associated documentation
  - Data is no longer needed as taxonomy queries now use direct database connections
  - Reduced package size and maintenance burden

---

# CafriplotsR 1.7.1 (2025-11-18)

### New Features

* **New `get_user_accessible_plots()` function**
  - Extracts plot IDs that a user can access based on row-level security policies
  - Parses policy expressions to return clean vector of accessible plot IDs
  - Useful for checking user permissions and debugging access issues

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`

* **Fixed `list_user_policies()` returning empty results**
  - Added `::name` type cast for proper comparison with PostgreSQL `name[]` array
  - Function now correctly filters policies by username

### Documentation

* **New vignette: "Database Connections Guide"**
  - Complete guide to database connection workflows
  - Explains both `call.mydb()` and `call.mydb.taxa()` functions
  - Credential management options (interactive, .Renviron, direct)
  - Connection cleanup best practices with `cleanup_connections()`
  - Row-level security and checking plot accessibility
  - Troubleshooting common connection issues
  - Best practices summary for users

---

# CafriplotsR 1.7 (2025-01-13)

### New Features

* **Interactive Shiny app for plot querying and data extraction**
  - New `launch_query_plots_app()` function provides user-friendly interface for `query_plots()`
  - Two-stage workflow: (1) filter and discover plots, (2) select and extract individual data
  - Filter interface: query by country, plot name, locality, method, tags, and IDs (including comma-separated values)
  - Interactive leaflet map with multiple basemaps showing plot locations
  - Metadata table viewer with sortable/searchable columns
  - Plot selection: all plots selected by default, users can deselect specific plots
  - Configurable extraction options: output styles, census strategies, data organization, trait extraction
  - Results viewer with dynamic tabs for each data table
  - Multi-format download: Excel (.xlsx), CSV (zipped), R object (.rds), and shapefile (.zip) formats
  - Row-level security aware: filter options respect user's database access permissions
  - Modular architecture with dedicated UI/server modules for extensibility
  - Database login integration with support for saved credentials from .Renviron

* **Complete individual tree data import workflow**
  - New `import_individual_data()` function with transaction-based imports and automatic rollback on errors
  - Interactive column mapping with `map_individual_columns()` - automatically matches user columns to database schema
  - Comprehensive validation with `validate_individual_data()` - checks plots, taxonomy, tags, traits before import
  - Template generation with `get_individual_template()` - creates Excel templates with guidance
  - Dry-run mode to preview imports without committing changes
  - Support for both flat table and two-table (individuals + features) data structures
  - Auto-generates sequential tags when missing
  - Imports into `data_individuals` and `data_traits_measures` tables
  - See new vignette "Importing Plot Data into the Database" for complete workflow

* **Intelligent column mapping system**
  - Fuzzy matching of user column names to database columns and traits
  - Interactive classification: feature/trait vs individual identification columns
  - Manual selection with ranked suggestions based on similarity scores
  - Synonym support for common column name variations
  - Automatic detection of linking columns (plot_name, tag)
  - Mapping audit trail preserved for reproducibility

* **Comprehensive data validation before import**
  - Required columns validation (plot_name, idtax_n)
  - Plot existence verification with exact name matching
  - Taxonomy ID validation against database
  - Tag uniqueness within plots
  - Tag conflict detection with existing database records
  - Trait value validation (numeric vs categorical)
  - Feature-to-individual linkage verification
  - Method-specific requirements validation
  - Detailed error reporting with actionable messages

* **`query_plots()` exact name matching**
  - New `exact_match` parameter (default FALSE) for precise plot name filtering
  - Prevents unintended pattern matching (e.g., "41" matching "Plot-41", "4100")
  - Uses SQL IN clause for exact matching vs LIKE for pattern matching
  - Applied throughout PlotFilterBuilder pipeline

* **Taxonomic matching app: Class-level taxonomic support**
  - Now recognizes and matches class-level taxa (e.g., names ending in -opsida, -psida)
  - Searches in `tax_famclass` column for class names
  - Both exact and fuzzy matching supported for classes
  - Expands hierarchical matching beyond family/order/genus/species

* **Taxonomic matching app: Improved large dataset handling**
  - Excel file reading now uses `guess_max = 30000` for better column type detection
  - Prevents type mismatches when taxonomic names appear late in large datasets
  - Ensures consistent data type inference across entire dataset

### Documentation

* **New vignette: "Importing Plot Data into the Database"**
  - Complete workflow from plot metadata to individual tree data
  - Step-by-step examples with expected output
  - Interactive and programmatic workflows
  - Common issues and troubleshooting guide
  - Best practices for data import
  - Advanced topics: custom column synonyms

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`

* **Fixed `query_plots()` with `output_style` throwing errors on missing columns**
  - Changed column selection from `all_of()` to `any_of()` in output style transformations
  - Functions now gracefully handle missing columns instead of throwing errors
  - Applies to `.extract_metadata_table()`, `.extract_individuals_table()`, and `.extract_height_diameter_pairs()`
  - Dynamic column selection for `height_of_stem_diameter` (POM) when creating height-diameter pairs
  - Output styles (`permanent_plot`, `standard`, etc.) now work reliably with varying data structures

* **Fixed `.find_cat()` return value handling in column mapping**
  - Interactive column selection was returning wrong columns due to table reordering
  - Now correctly extracts selected value from `result$sorted_matches` instead of original table
  - Applies to both individual column and trait column selection

* **Fixed traits_list() column name**
  - Changed `description` to `traitdescription` to match actual column name
  - Prevents errors during trait column display

* **Fixed tag propagation from individuals to features**
  - Auto-generated tags now correctly synced to features sheet during validation
  - Ensures features can link to individuals via tag column

### Infrastructure

* **`query_plots()` improvements for Shiny integration**
  - New `con` parameter accepts optional database connection (defaults to `call.mydb()` if NULL)
  - Enables Shiny apps to pass reactive connection pools without triggering reactive context errors
  - Consistent `metadata` naming in return list regardless of `output_style` (previously `meta_data` for "full" style, `metadata` for others)
  - Ensures predictable list structure for programmatic access

* **Improved package dependency management**
  - Moved `getPass` and `dm` from Imports to Suggests
  - Reduces installation requirements - only needed for specific optional features
  - `getPass`: Used only for secure password prompts (has fallbacks to rstudioapi and readline)
  - `dm`: Used only for database structure visualization with `get_database_fk()`
  - Both packages now checked with `requireNamespace()` before use with helpful error messages
  - Fixes installation errors for users without these packages: "ERROR: dependencies 'getPass', 'dm' are not available"

### Breaking Changes

* **Taxonomic matching app: Stricter default similarity threshold**
  - Default `min_similarity` increased from 0.3 to 0.7 in `launch_taxonomic_match_app()`
  - Reduces false positive matches by requiring higher similarity scores
  - Previous behavior available by setting `min_similarity = 0.3` explicitly
  - **Action required**: Users relying on low-quality fuzzy matches may need to adjust threshold or improve input data quality
  - Rationale: Quality over quantity - fewer but more reliable matches improve data integrity

# CafriplotsR 1.5

### New Features

* **Interactive validation with fuzzy matching for plot metadata import**
  - `validate_plot_metadata()` now has `interactive = TRUE` and `fix_on_fly = TRUE` parameters (both default to TRUE)
  - Integrates with existing `resolve_multiple_values()` for on-the-fly fixing of lookup mismatches (Country, Method)
  - Returns enhanced structure with three data versions:
    - `original_data`: Unchanged user input
    - `cleaned_data`: Data with interactive fixes applied
    - `changes_made`: Complete audit trail of all corrections (column, row, original, corrected, method)
  - Eliminates tedious manual Excel editing - users interactively match mismatches (e.g., "Cameroun" → "CAMEROON") with fuzzy suggestions
  - Pattern search ("G" option) available for large lookup tables
  - Non-breaking: Old code works but gets enhanced behavior automatically

* **Complete subplot features import system**
  - Plot import now handles ALL subplot feature types, not just people features
  - New `.extract_and_process_subplot_features()` dynamically queries `subplot_list()` to identify all subplot features
  - Automatically separates into two categories:
    - People features (`valuetype == "table_colnam"`): Linked to `table_colnam` via `.link_colnam()`
    - Other features (numeric, character, etc.): Direct value insertion
  - No hardcoded feature lists - fully dynamic based on database schema
  - Identifies subplot features by excluding flat table columns (plot_name, ddlat, ddlon, elevation, etc.)
  - Both types inserted as subplot features in Step 6 of import workflow

* **Row-Level Security (RLS) safe plot import for non-admin users**
  - Uses PostgreSQL `INSERT ... RETURNING` clause to retrieve plot IDs during insertion
  - Bypasses RLS SELECT restrictions that would prevent non-admin users from reading their own inserted plots
  - Enables subplot features to be linked even when user doesn't have SELECT permission yet
  - More secure than alternative approaches (no exposure of other users' plot IDs)
  - Critical fix: Previously, non-admin imports would fail at Step 6 (subplot features) with empty plot_id_data

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`

* **Restored missing helper functions** accidentally commented out
  - `.rename_data()` (R/helpers.R:307) - Renames columns in datasets
  - `.add_modif_field()` (R/helpers.R:283) - Adds modification date fields (date_modif_d/m/y)
  - Both functions now properly exported and available
  - Fixes errors: "impossible de trouver la fonction .rename_data" and ".add_modif_field"

* **Fixed transaction connection management throughout import workflow**
  - `try_open_postgres_table()` now properly handles errors and maintains connection scope
  - `.link_table()` now uses passed `db_connection` parameter instead of creating new connection
  - `.link_colnam()` now uses passed `db_connection` parameter instead of creating new connection
  - `add_subplot_features()` added `con` parameter to accept transaction connection
  - All functions now respect transaction boundaries (no more "Invalid connection" errors)
  - Prevents connection invalidation during multi-step import process

* **Fixed invalid cli package parameter**
  - Removed unsupported `line = 2` parameter from `cli::cli_rule()` calls in import success messages
  - Fixes error: "argument inutilisé (line = 2)"

### Code Refactoring

* **Renamed and expanded subplot features processing**
  - `.extract_and_link_people()` → `.extract_and_process_subplot_features()`
  - Function now handles all subplot feature types, not just people features
  - Enhanced documentation reflects expanded scope and hierarchical processing logic

### Breaking Changes

* **`query_plots()` now returns a list by default instead of a flat data frame**
  - Output is automatically structured based on inventory method using the new output styles system
  - Different styles organize data into separate tables: metadata, individuals, censuses, height-diameter, etc.
  - **Action required**: To preserve old behavior (flat data frame), use `output_style = "full"`
  - Rationale: Structured output makes it easier to work with complex plot data without overwhelming column counts
  - See documentation for `?query_plots` for details on available output styles

### New Features

* **Census selection strategy for multi-census plots**
  - New `census_strategy` parameter in `query_plots()` with three options:
    - `"last"` (default): Extract data from most recent census only
    - `"first"`: Extract data from earliest census only
    - `"mean"`: Average across all censuses (previous default behavior)
  - When using "first" or "last" strategy:
    - Individuals recruited after first census show NA values (biologically correct)
    - Individuals dead before last census show NA values (biologically correct)
    - Single `census_date` column shows the date of the selected census (instead of `date_census_1`, `date_census_2`, etc.)
  - Census selection based on actual census dates using proper date computation
  - Applies to individual-level features (stem diameter, tree height, etc.)
  - When `show_multiple_census = TRUE`, all census data shown regardless of strategy

* **Configurable output styles system for `query_plots()`**
  - 6 predefined output styles: `minimal`, `standard`, `permanent_plot`, `permanent_plot_multi_census`, `transect`, `full`
  - Auto-detection of appropriate style based on `method` field (e.g., "1 ha plot" → `permanent_plot`)
  - Manual style selection via `output_style` parameter
  - Each style returns a structured list with relevant tables (e.g., `$metadata`, `$individuals`, `$censuses`)
  - Column renaming from database names to user-friendly names (e.g., `ddlat` → `latitude`, `tax_sp_level` → `species`)
  - New configuration files: `R/output_styles_config.R`, `R/output_styles_helpers.R`

* **Specialized output tables for permanent plots**
  - `$censuses` table: plot_name, census_number, census_date, team_leader, principal_investigator
  - `$height_diameter` table: Paired height-diameter measurements (id_n, D, H, POM) with issue filtering
  - Handles multiple censuses with automatic pivoting from wide to long format
  - Census-specific column renaming (e.g., `stem_diameter_census_1` → `dbh_census_1`)

* **Custom print method for query results**
  - New S3 class `plot_query_list` with informative print method
  - Shows table dimensions, column names, and geometry type for sf objects
  - Makes it easy to understand query result structure

* **Preservation of spatial data**
  - `coordinates_sf` table automatically included when `show_all_coordinates = TRUE`
  - Print method detects and displays sf geometry information

### Code Refactoring

* **Modular output style configuration**
  - Centralized style definitions in `.plot_output_styles` list
  - Method-to-style mapping in `.method_to_style_map`
  - Style auto-detection function `.detect_style_from_method()`
  - Easy to add new output styles by extending configuration

* **Improved metadata extraction**
  - Uses `res_meta_data` table (created before individual extraction) for metadata source
  - Ensures all plot-level columns available even when `extract_individuals = TRUE`
  - Consistent variable naming and error handling

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`

* **Fixed commented `@export` tag causing roxygen2 errors**
  - Removed `@export` from commented-out `subplot_list()` function in `R/subsplots_features_function.R`
  - Prevents documentation build failures

# CafriplotsR 1.4 (development version)

### New Features

* **Traits enrichment module in taxonomic matching Shiny app**
  - New tab "Enrich with Traits" allows enriching matched taxonomic names with trait data from the taxa database
  - Aggregates multiple input names that match to the same taxon into a single row
  - Concatenates all input names (e.g., "cola edulis | coula edrulis" → "Coula edulis")
  - Configurable options for categorical trait aggregation (mode vs concatenation)
  - User can select which columns to include (original names, corrected names, IDs, metadata)
  - Downloads enriched data as Excel file
  - Filters out `id_trait_measures` columns for cleaner output
  - Module: `mod_traits_enrichment_ui()` and `mod_traits_enrichment_server()`

* **Enhanced file upload in taxonomic matching Shiny app**
  - CSV file support added (in addition to Excel .xlsx and .xls)
  - Excel sheet selector allows choosing which sheet to import from multi-sheet workbooks
  - Sheet selector appears dynamically after Excel file upload
  - Default sheet selection is the first sheet
  - CSV files are loaded directly without sheet selection

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`

* **Fixed NA input names appearing in trait enrichment**
  - Enrichment module now filters out rows where the input taxonomic name is NA or empty
  - Prevents invalid NA entries from being matched to taxa or included in enriched output
  - Applied in both trait fetching and result aggregation steps

* **Fixed incorrect input names in enrichment output**
  - Enrichment now correctly uses the user-selected taxonomic name column (not first column of dataset)
  - `column_name` parameter now passed from main app to enrichment module
  - Ensures `input_names` column shows actual taxonomic names from the selected column

### Code Refactoring

* **Optimized taxonomic name cleaning for faster matching**
  - Name cleaning (removing "sp.", "cf.", "aff.", etc.) now happens **before** batch exact matching
  - Previously, cleaning only occurred during slow fuzzy matching phase
  - Names like "Coula edulis sp." now match exactly to "Coula edulis" in fast batch step
  - Significantly reduces number of names sent to slower fuzzy matching
  - Cleaning happens once at beginning, benefiting all matching steps (species, genus, family)
  - Both original and cleaned names preserved in matching pipeline
  - Added underscore replacement in `clean_taxonomic_name()` (e.g., "Coula_edulis" → "Coula edulis")

### Breaking Changes

* **`query_taxa()` default behavior changed**: `exact_match` parameter now defaults to `TRUE` (was `FALSE`)
  - Exact matching is now the default for family/genus/order queries to prevent unexpected fuzzy matching results
  - For species queries, if exact match fails, the function automatically falls back to intelligent fuzzy matching
  - **Action required**: Code relying on fuzzy matching by default should explicitly set `exact_match = FALSE`
  - Rationale: Higher taxonomic ranks are standardized names where fuzzy matching rarely helps and can introduce errors

### New Features

* **Intelligent taxonomic name matching** with genus-constrained fuzzy search
  - New `match_taxonomic_names()` function implements hierarchical matching strategy:
    1. Exact matching (fastest)
    2. Genus-constrained fuzzy matching (searches species only within matched genus)
    3. Full fuzzy matching (last resort)
  - Dramatically improves match quality by restricting fuzzy search space
  - Includes synonym detection and resolution
  - Supports scoring and ranking of multiple matches
  - New helper functions: `parse_taxonomic_name()`, `.match_exact_sql()`, `.match_genus_constrained_sql()`, `.match_fuzzy_sql()`

* **Auto fuzzy fallback for species queries**
  - `query_taxa()` automatically retries with fuzzy matching when exact species match fails
  - Transparent user feedback shows match quality (similarity score)
  - Handles typos and spelling variations automatically
  - Only applies to species queries; family/genus/order use exact matching only

* **Database enhancement: `tax_level` field added to `table_taxa`**
  - New column explicitly indicates taxonomic level: "species", "genus", "family", "order", "infraspecific", "higher"
  - Indexed for query performance
  - Eliminates ambiguity between missing data and genus/family-level taxa
  - Script provided: `add_tax_level_field.R` for database migration
  - All query functions updated to use new field for cleaner, more reliable filtering

### Code Refactoring

* **Complete rewrite of `query_taxa()`** to use new intelligent matching functions
  - Eliminated redundancy with `helpers.R` functions
  - 8 new modular helper functions replace complex inline logic
  - Cleaner separation of concerns: matching, filtering, synonym resolution, formatting, trait addition
  - ~160 lines of code removed through better abstraction
  - Better maintainability and extensibility
  - Deprecated `query_fuzzy_match()` and `query_exact_match()` in favor of `match_taxonomic_names()`

* **Simplified taxonomic level filtering** using `tax_level` field
  - Replaced complex multi-column checks (e.g., `is.na(tax_esp) & is.na(tax_gen)`) with simple `tax_level == "family"`
  - Applied in `query_taxa()` for clearer intent and better performance via index usage

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`

* **Fixed `query_taxa()` empty results with `only_family = TRUE`**
  - Previously, fuzzy matching by default caused empty results when filtering for family-level taxa
  - Now uses exact matching by default for higher taxonomic ranks

### Dependencies

* Added new package dependencies to DESCRIPTION:
  - `cli` - User-friendly command line interfaces (moved from Suggests to Imports)
  - `lifecycle` - Manage function lifecycle (deprecation warnings)
  - `data.table` - High-performance data manipulation
  - `glue` - String interpolation for SQL queries
  - `RecordLinkage` - String similarity calculations

# CafriplotsR 1.0

### Breaking Changes
* **Database schema change**: Renamed column `ind_num_sous_plot` to `tag` in `data_individuals` and `followup_updates_individuals` tables
  - All R package functions updated to use new column name
  - **Action required**: External scripts accessing `ind_num_sous_plot` must be updated to use `tag`
  - Updated files: `R/functions_manip_db.R`, `R/individual_features_function.R`, `R/functions_divid_plot.R`, `R/generate_plot_summary.Rmd`, `structure.yml`
  - Default parameter in `approximate_isolated_xy()` changed from `tag = "ind_num_sous_plot"` to `tag = "tag"`

### New Features
* Initial release of package structure with comprehensive database query functions
* **Enhanced `update_ident_specimens()`**: Now shows summary of linked individuals before updating specimen identification
  - Displays which plots and how many individuals will inherit the new identification
  - Shows current taxonomic identification of linked individuals
  - Provides better context for informed decision-making before confirmation
  - New helper function `.get_linked_individuals_summary()` queries and summarizes impact

### Bug Fixes

* **Fixed Shiny apps crashing RStudio when browser is closed**
  - Removed duplicate `onSessionEnded` callbacks that caused "Can't access reactive value outside of reactive consumer" errors
  - Updated `cleanup_connections()` to properly close pool connections used by Shiny apps
  - Removed `q("no")` calls that were quitting R entirely and crashing RStudio
  - Affects `launch_taxonomic_match_app()` and `launch_query_plots_app()`

* **Connection error with complex home paths**: Fixed `create_db_config()` function that failed when home directory path contained spaces or special characters (e.g., OneDrive paths like `C:/Users/NOBUS CAPITAL/OneDrive/Documents/`)
  - Added proper error handling with `tryCatch()` for file creation
  - Creates parent directories if they don't exist
  - Falls back to in-memory configuration if file cannot be written
  - Users now get informative warnings instead of connection failures

### Documentation
* Added comprehensive README.md with package overview, quick start guide, and function reference
* README includes prominent link to NEWS.md for tracking updates

### Infrastructure
* Added NEWS.md to track package changes and updates
* Established git branching workflow for all code modifications

### Code Refactoring
* **Major refactoring**: Reorganized `R/functions_manip_db.R` (previously 10,528 lines) into modular, domain-specific files
  - Created `R/growth_census_functions.R` (556 lines) - Growth computation and census analysis functions
  - Created `R/specimen_linking_functions.R` (406 lines) - Herbarium specimen linking and querying functions
  - Created `R/taxonomic_query_functions.R` (944 lines) - Taxonomic query functions with synonym resolution
  - Created `R/taxonomic_update_functions.R` (838 lines) - Taxonomic data update and entry functions
  - Expanded `R/connections_db.R` with database query utilities (`func_try_fetch`, `try_open_postgres_table`)
  - Removed ~6,800 lines from `R/functions_manip_db.R` through extraction to specialized modules
  - All functions verified as moved (not duplicated) to new locations
  - Improved code maintainability and discoverability

---

