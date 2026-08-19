# Shiny Test Strategy — Tier 2, 3, 4 Plan

**Date**: 2026-05-22
**Author**: Shiny testing initiative
**Status**: PLANNING (Tier 1 already merged)
**Owner**: TBD

---

## Context

Tier 1 (construction smoke tests) is implemented and merged. It lives in
`tests/testthat/test-app-construction.R` and covers all 8 exported app
builders with 39 assertions. Headline numbers:

| Layer | Coverage from Tier 1 |
|---|---|
| Whole package | 9.09% |
| Shiny app builder files | 22.41% (825 / 3681 lines) |
| Module files (`mod_*.R`) | 2.8% (757 / 27,398 lines) |

The ceiling for Tier 1 is reached: it only exercises UI assembly + the
outer function body. Everything inside the `server <- function(...) {...}`
closure stays uncovered until we drive an active session.

**Tier 2, 3, 4 target that gap.** This document is the parking lot — pick
it up when the next coverage push is on the table.

`shinytest2` and `chromote` are already in `Suggests:`; no further
DESCRIPTION changes needed to start Tier 2.

---

## Tier 2 — Login-screen / unauthenticated UI tests (shinytest2)

**Goal**: exercise the pre-authentication UI in a real headless browser.
Catches DOM rendering issues, language toggle bugs, i18n key drift, and
visual regressions in the login panel — none of which Tier 1 can see.

### Scope

All 8 apps share the `mod_database_login` pattern with a conditional
panel. Without credentials we can only test what's visible before login:

- Login form renders (`#login-user_input`, `#login-pass_input`, login button)
- Main UI is hidden (`output.authenticated` is `FALSE`)
- Language toggle changes visible labels FR ↔ EN
- Invalid login attempt produces an error message (validation only —
  no DB call needed if the message is client-side)
- No JavaScript console errors on initial load

### Code sketch

`tests/testthat/test-app-query-plots-login.R`:
```r
test_that("query_plots: login UI renders in FR and EN", {
  skip_if_no_chromote()

  app <- shinytest2::AppDriver$new(
    shiny_app_query_plots(language = "fr"),
    name = "query-plots-login-fr",
    height = 800, width = 1200,
    load_timeout = 15 * 1000,
    seed = 42
  )

  # Main UI hidden before auth
  app$expect_values(output = "authenticated")  # FALSE

  # Snapshot FR
  app$expect_screenshot(variant = "fr")

  # Toggle to EN
  app$set_inputs(selected_language = "en")
  app$wait_for_idle(500)
  app$expect_screenshot(variant = "en")
})
```

### Prerequisites

- Chrome / Chromium installed on the runner (already the case on
  GitHub-hosted Linux runners)
- `tests/testthat/_snaps/` will appear once tests run — commit it
- First run records the baseline; subsequent runs assert pixel
  equivalence. Use `shinytest2::record_test()` to re-record after an
  intentional UI change.

### Apps to cover (priority order)

1. `shiny_app_query_plots` — simplest auth gate, good for establishing the pattern
2. `app_taxonomic_match` (via `launch_taxonomic_match_app` builder) — also simple
3. `shiny_app_taxo_backbone`
4. `launch_feature_wizard`
5. `launch_import_wizard`
6. `launch_taxa_traits_import`
7. `launch_individual_specimen_linking_app`
8. `launch_specimen_import_wizard`

### CI strategy

- Run on Linux runners only (Chrome reliability)
- Mark snapshot tests with `testthat::skip_on_os("windows")` and
  `skip_on_os("mac")` if cross-platform snapshots prove flaky
- Use `skip_if_no_chromote()` helper (already in `helper-shiny.R`)
- Snapshot diffs surface in PR review automatically via testthat output

### Expected coverage gain

Marginal — Tier 2 only exercises the login module + outer scaffolding
of each app. Realistic ceiling: ~25-28% on builders (up from 22%).
**The value is regression detection, not coverage percentage.**

### Effort estimate

- Helper / first app: 0.5 day (build the recording pattern, commit baseline)
- Each remaining app: ~30 min
- **Total: 1.5 days** for all 8 apps

---

## Tier 3 — Authenticated end-to-end flows (shinytest2 + real DB)

**Goal**: drive full user journeys through authenticated apps — query
→ filter → export, import wizard step-through, taxonomic match
review. This is the only tier that exercises the server closure in
realistic conditions.

### Scope

Per-app, target the **golden path** (the single most-used flow):

| App | Golden path |
|---|---|
| `shiny_app_query_plots` | Login → filter by country → results render → export |
| `launch_individual_specimen_linking_app` | Login → search individual → select specimen → confirm link |
| `launch_import_wizard` | Login → upload sample file → mapping step → validation → dry-run import |
| `launch_taxonomic_match_app` | Login → load sample taxa → auto-match → manual review → export |
| `launch_feature_wizard` | Login → select plot → add census feature → validate → dry-run |
| `launch_specimen_import_wizard` | Login → upload sample file → mapping → preview → dry-run |
| `launch_taxa_traits_import` | Login → upload sample file → mapping → validation → dry-run |
| `shiny_app_taxo_backbone` | Login → search a name → view hierarchy |

### Prerequisites — the hard part

Tier 3 requires a **dedicated test database**. Options:

1. **Dockerized PostgreSQL with seeded fixtures** (recommended)
   - `docker-compose.yml` in `tests/fixtures/`
   - SQL dump with anonymized subset of `plots_transects` + `rainbio`
   - ~100 plots, ~1000 individuals, ~50 taxa — small enough to load in seconds
   - `setup-test-db.sh` script for local dev
2. **Live development DB** with read-only test user
   - No infra to maintain but tests pollute logs and need network access
3. **Mock the pool layer**
   - Reject: `pool::dbPool` returns proxy connections; mocking it deeply enough to fool the apps is a project of its own

### Code sketch

`tests/testthat/test-app-query-plots-flow.R`:
```r
test_that("query_plots: login → filter Gabon → results render", {
  skip_if_no_chromote()
  skip_if_no_db()

  pool <- pool::dbPool(
    RPostgres::Postgres(),
    dbname = "plots_transects_test",
    host = Sys.getenv("CAFRI_TEST_DB_HOST", "localhost"),
    user = Sys.getenv("CAFRI_TEST_DB_USER"),
    password = Sys.getenv("CAFRI_TEST_DB_PASS")
  )
  withr::defer(pool::poolClose(pool))

  app <- shinytest2::AppDriver$new(
    shiny_app_query_plots(pool_main = pool, language = "en"),
    name = "query-plots-flow"
  )

  # Pool passed in → skip login screen
  app$wait_for_value(output = "authenticated", value = TRUE,
                     timeout = 10 * 1000)

  app$set_inputs(`filters-country_select` = "Gabon")
  app$click("filters-apply_btn")
  app$wait_for_idle(3000)

  results <- app$get_value(output = "results-plot_count")
  expect_gt(as.numeric(results), 0)
})
```

### CI strategy

- **Do not run on PR builds.** Tier 3 is slow (per-test setup ~10s) and
  needs DB secrets.
- Run on `workflow_dispatch` (manual trigger) and nightly schedule
- Store DB credentials as GitHub Actions secrets:
  `CAFRI_TEST_DB_HOST`, `CAFRI_TEST_DB_USER`, `CAFRI_TEST_DB_PASS`
- Local dev: developers run via `Sys.setenv()` + `devtools::test()`

### Expected coverage gain

Significant — Tier 3 enters the server closure. Realistic numbers:

- Builders: 22% → **50-60%**
- Modules: 2.8% → **20-30%** (only modules on golden paths get touched)

### Effort estimate

- Test DB infra (Docker + seed): **3-5 days** one-time
- Helper to spin up pool / set env: 0.5 day
- Each app's golden path test: 0.5-1 day (depends on flow complexity)
- **Total: ~2 weeks** for full 8-app coverage including infra

### Risks

- **Flakiness**: server-driven UI updates are timing-sensitive.
  Mitigate with `app$wait_for_idle()` and explicit value waits, never
  fixed `Sys.sleep()`.
- **Seed data drift**: each schema change to `plots_transects` may
  break the SQL dump. Version the dump alongside migrations.
- **DB connection leaks** in failing tests — always wrap pool in
  `withr::defer(pool::poolClose(pool))`.

---

## Tier 4 — Module-level unit tests (testServer)

**Goal**: exercise individual `mod_*_server` functions in isolation
using `shiny::testServer()`. No browser, no DB (or minimal mocked
connections). Highest ROI per hour for raising module coverage.

### Scope

The package has **60 modules** in `R/mod_*.R`. Not all are worth unit
testing. Triage:

#### Easy wins (pure logic, no DB calls in server) — START HERE

These modules transform inputs to outputs without touching the
database. Quick to test, high coverage gain.

- `mod_language_toggle` — language state management
- `mod_column_select` — column picker reactives
- `mod_data_input` — file upload validation
- `mod_code_preview` — code snippet generation
- `mod_fuzzy_suggestions` — string-distance suggestion logic
- `mod_herbarium_parser` — string parsing for herbarium codes
- `mod_growth_form_selector` — selection state
- `mod_plot_filters` — filter state reactives (without DB lookup pieces)

#### Medium effort (DB-touching but mockable)

These query lookup tables. Pass a fake pool or use `DBI::dbConnect(RSQLite::SQLite(), ":memory:")` with a fixture schema.

- `mod_database_login` — auth validation
- `mod_lookup_matcher` — fuzzy lookup against `table_colnam`
- `mod_taxonomic_validator` — name standardization
- `mod_individual_search` — search reactives

#### Skip (too entangled or rarely changed)

The remaining ~45 modules — leave them to Tier 2/3 coverage.

### Code sketch

`tests/testthat/test-mod-language-toggle.R`:
```r
test_that("mod_language_toggle: clicking FR sets language to 'fr'", {
  shiny::testServer(
    mod_language_toggle_server,
    args = list(initial_lang = "en"),
    {
      # Initial state
      expect_equal(current_lang(), "en")

      # Simulate click
      session$setInputs(lang_button = "fr")
      expect_equal(current_lang(), "fr")
    }
  )
})

test_that("mod_language_toggle: invalid language is rejected", {
  shiny::testServer(
    mod_language_toggle_server,
    args = list(initial_lang = "en"),
    {
      session$setInputs(lang_button = "de")
      # Should remain at 'en' since 'de' is unsupported
      expect_equal(current_lang(), "en")
    }
  )
})
```

`tests/testthat/test-mod-lookup-matcher.R` (with mock pool):
```r
test_that("mod_lookup_matcher: exact match returns single result", {
  # In-memory SQLite mimicking table_colnam
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "table_colnam", data.frame(
    id_table_colnam = 1:3,
    colnam = c("Dauby G.", "Sosef M.", "Stévart T.")
  ))
  withr::defer(DBI::dbDisconnect(con))

  shiny::testServer(
    mod_lookup_matcher_server,
    args = list(con = con, table = "table_colnam", column = "colnam"),
    {
      session$setInputs(search_input = "Dauby G.")
      result <- matched_id()
      expect_equal(result, 1)
    }
  )
})
```

### Prerequisites

- No new dependencies (testServer is built into shiny)
- Optional: `RSQLite` for mock fixtures (already a likely transitive dep)

### CI strategy

- Run on every PR — fast, deterministic, no infrastructure needed
- Bundle into the main `devtools::test()` invocation

### Expected coverage gain

The 8 "easy win" modules average ~200 lines each = 1600 lines of
mostly server-body code. Covering them at 70% adds ~1100 lines to
module coverage, taking modules from 2.8% to **~7%**.

Adding the 4 medium-effort modules brings another ~800 lines: **~10%
module coverage total.**

### Effort estimate

- Helper / first module: 0.5 day
- Each easy module: 1-2 hours
- Each medium module (with mock pool): 0.5 day
- **Total: ~4-5 days** for all 12 prioritized modules

---

## Recommended rollout order

This ordering maximizes value per unit of effort:

1. **Tier 4 easy modules** (4-5 days) — biggest coverage ROI, no infra
2. **Tier 2** for `shiny_app_query_plots` + `app_taxonomic_match`
   (0.5 day) — establishes the snapshot pattern
3. **Tier 2** rollout to remaining 6 apps (1 day) — once the pattern
   is solid
4. **Tier 3 infra** (Docker test DB, 3-5 days) — only when there is
   appetite to commit to maintaining seed fixtures
5. **Tier 3** golden-path tests per app (1-2 weeks total) — last

**Stop after step 1+2+3** if budget is tight. That gives ~10% module
coverage and snapshot regression detection on the unauthenticated UI
of every app, for a total of ~6 days of work. The Tier 3 infra is the
expensive part and only justified if the apps are changing rapidly
enough that regressions in authenticated flows matter.

---

## Open questions to resolve before starting

- [ ] Who owns the test DB if we go for Tier 3? (Docker fixtures need
      a maintainer when schema changes.)
- [ ] Should snapshot tests run on all OS or Linux-only?
      (Recommendation: Linux-only — Windows/Mac browser font rendering
      differs.)
- [ ] Do we want a `covr::package_coverage()` GitHub Action workflow
      to track the trend over time? (Useful to see the impact of each
      tier landing.)
- [ ] For Tier 4, do we add `RSQLite` to `Suggests` for mock fixtures,
      or use a pure-R fake pool class?

---

## Related artifacts

- `tests/testthat/test-app-construction.R` — Tier 1 implementation
- `tests/testthat/helper-shiny.R` — already has `skip_if_no_chromote()`
  and `skip_if_no_db()` helpers ready for Tiers 2 & 3
- `DESCRIPTION` Suggests already lists `shinytest2` and `chromote`
- `inst/translations/translation.json` — must be kept in sync if Tier
  2 snapshot text changes
