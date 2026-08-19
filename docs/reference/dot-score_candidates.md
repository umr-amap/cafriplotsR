# Score All Candidate Column Matches (Internal Helper)

Evaluates all possible matches across exact, synonym, and fuzzy
strategies for both direct and feature columns. Returns candidates
ranked by final score.

## Usage

``` r
.score_candidates(
  user_col_clean,
  direct_cols,
  feature_cols,
  synonyms,
  required_cols,
  similarity_threshold = 0.6
)
```

## Arguments

- user_col_clean:

  Cleaned user column name

- direct_cols:

  Direct database columns

- feature_cols:

  All feature columns (subplot + trait features)

- synonyms:

  Synonym dictionary (names are target columns, values are synonym
  lists)

- required_cols:

  Required column names

- similarity_threshold:

  Fuzzy match threshold

## Value

List of candidate matches, each with: match, method, category,
base_score, final_score. Sorted descending by final_score. Returns NULL
if no candidates found.
