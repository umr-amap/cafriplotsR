# Standardize free-text observations into mortality and dawkins flags

Parses the free-text `observations` trait (id_trait = 13) of the given
individuals, splits multi-observation entries into atomic phrases, and
matches each phrase against a regex ontology to derive standardized rows
for the `mortality_risk_flag` trait (multi-valued) and the
`dawkins_index` trait (single-valued). The original `observations` trait
is not modified.

## Usage

``` r
standardize_observations(
  individual_ids,
  ontology = NULL,
  add_data = FALSE,
  dry_run = TRUE,
  mortality_trait_name = "mortality_risk_flag",
  dawkins_trait_id = 15L,
  obs_trait_id = 13L,
  flag1_trait_id = 19L,
  con = NULL
)
```

## Arguments

- individual_ids:

  Integer vector of individual IDs.

- ontology:

  A data frame with columns `trait, std_value, pattern`, or a path to a
  CSV with those columns. Defaults to the package ontology.

- add_data:

  Logical. If `TRUE`, upsert the derived rows into the database. Default
  `FALSE`.

- dry_run:

  Logical. When `add_data = TRUE`, preview without committing changes.
  Default `TRUE`.

- mortality_trait_name:

  Name of the categorical trait that receives mortality risk tokens.
  Default `"mortality_risk_flag"`.

- dawkins_trait_id:

  Trait ID of the dawkins trait. Default `15L`.

- obs_trait_id:

  Trait ID of the free-text observations source. Default `13L`.

- flag1_trait_id:

  Trait ID of `flag1_rainfor` (single-letter alive-stem condition code).
  Default `19L`. Codes are decoded with the OpenForis mapping
  (`.default_observation_flags`) and appended to the mortality rows
  derived from free text. Rows are de-duplicated per (id_n,
  id_sub_plots, std_value); the `source_phrases` column records whether
  a row came from text, from a flag, or both.

- con:

  Database connection. Defaults to
  [`call.mydb()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md).

## Value

A tibble with one row per (`id_n`, `census_name`, `trait`, `std_value`):

- `id_n`:

  Individual ID.

- `id_table_liste_plots`:

  Plot ID.

- `id_sub_plots`:

  Census subplot ID (used for DB linking).

- `plot_name`, `tag`:

  Plot name and stem tag.

- `census_name`, `census_date`:

  Census label and date.

- `trait`:

  Target trait — `"mortality_risk_flag"` or `"dawkins_index"`.

- `std_value`:

  Standardized token.

- `source_phrases`:

  The raw phrase(s) that triggered the match.

- `full_observation`:

  The full original observations string.

- `skip_existing`:

  Logical — `TRUE` for dawkins rows whose individual x census already
  has a dawkins value in the DB (these rows are skipped on write).

Additionally, the attribute `"unresolved"` on the returned tibble holds
a tibble of phrases (with counts) that matched no pattern — useful for
growing the ontology.

## Details

Existing `dawkins_index` measurements are never overwritten; derived
dawkins values for individual x census combinations already present in
the DB are dropped from the output of the DB write (they remain in the
returned tibble flagged as `skip_existing = TRUE`).

## Ontology

By default the function reads
`system.file("ontology", "observations_ontology.csv", package = "CafriplotsR")`.
Columns expected: `trait`, `std_value`, `pattern`. Patterns are
case-insensitive Perl regexes. Provide `ontology` (a data frame or a
path) to override.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
plots <- query_plots(method = "1ha-IRD", extract_individuals = TRUE,
                     country = "CAMEROON")
res <- standardize_observations(individual_ids = plots$individuals$id_n,
                                con = con)
attr(res, "unresolved")

# Preview the upsert
standardize_observations(individual_ids = plots$individuals$id_n,
                         add_data = TRUE, dry_run = TRUE, con = con)

# Commit
standardize_observations(individual_ids = plots$individuals$id_n,
                         add_data = TRUE, dry_run = FALSE, con = con)
} # }
```
