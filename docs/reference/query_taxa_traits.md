# Query traits at the taxonomic level

Retrieves trait measurements associated with taxa, with automatic
resolution of taxonomic synonyms. Traits linked to synonyms are
consolidated under the accepted taxon name.

## Usage

``` r
query_taxa_traits(
  idtax = NULL,
  include_synonyms = TRUE,
  add_taxa_info = FALSE,
  trait_ids = NULL,
  categorical_mode = c("mode", "concat"),
  format = c("wide", "long"),
  include_remarks = FALSE,
  include_measurement_features = FALSE,
  include_citation = FALSE,
  con = NULL,
  con_taxa = NULL,
  backbone = c("internal", "wcvp")
)
```

## Arguments

- idtax:

  Vector of taxon IDs to query

- include_synonyms:

  If TRUE, includes traits from all synonyms

- add_taxa_info:

  Add taxonomic information (family, genus, species)

- trait_ids:

  Vector of trait IDs to filter (NULL = all traits)

- categorical_mode:

  How to aggregate categorical traits: "mode" (most frequent) or
  "concat" (all unique values)

- format:

  Output format: "wide" (pivoted) or "long" (raw measurements)

- include_remarks:

  Include measurement remarks

- include_measurement_features:

  Add measurement-level features/metadata

- con:

  Connection to main database (optional, defaults to call.mydb()). Used
  for trait measurements (taxa_traits_measures, traitlist).

- con_taxa:

  Connection to taxa database (optional, defaults to call.mydb.taxa()).
  Used for synonym resolution (table_taxa) and taxonomic info
  enrichment.

- backbone:

  Character. Which taxonomic backbone to use for synonym resolution.
  `"internal"` (default) uses the internal `table_taxa`. `"wcvp"` uses
  WCVP via `wcvp_idtax_link` and `wcvp_names`, falling back to internal
  for unlinked taxa.

## Value

List with components: - traits_raw: Raw trait measurements with resolved
taxonomy - traits_numeric: Numeric traits (aggregated if
format="wide") - traits_categorical: Categorical traits (aggregated if
format="wide")
