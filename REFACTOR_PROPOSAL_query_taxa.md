# Refactoring Proposal: `query_taxa()` Function

## Problem Statement

The `query_taxa()` function in `R/taxonomic_query_functions.R` contains redundant fuzzy and exact matching logic that duplicates functionality now available in the newly developed `R/taxonomic_matching.R` file. Additionally, helper functions in `R/helpers.R` (`query_fuzzy_match()` and `query_exact_match()`) are now superseded by more sophisticated matching functions.

## Redundancy Analysis

### Functions to Deprecate in `helpers.R`

1. **`query_fuzzy_match()`** (lines 156-184)
   - Current: Basic SQL SIMILARITY query, returns top 1-5 matches
   - Replaced by: `.match_fuzzy_sql()` and `.match_genus_constrained_sql()` in `taxonomic_matching.R`
   - Why better: New functions provide hierarchical matching, scoring, and genus-constrained search

2. **`query_exact_match()`** (lines 204-239)
   - Current: Basic exact matching on single or concatenated fields
   - Replaced by: `.match_exact_sql()` in `taxonomic_matching.R`
   - Why better: New function handles infraspecific ranks, optional authors, and standardized output

### Redundant Code in `query_taxa()`

Lines 80-242 contain repetitive pattern for order/family/genus/species:
```r
# Pattern repeated 4 times:
q_res <- query_exact_match(...)
if (!exact_match & any(is.na(q_res$query_tb$id))) {
  # Loop through missing names
  fuz_list <- vector('list', nrow(query_tb_miss))
  for (i in 1:nrow(query_tb_miss)) {
    fuz_list[[i]] <- query_fuzzy_match(...)
  }
  fuz_res <- bind_rows(fuz_list)
}
# Combine exact and fuzzy results
```

This can be replaced with single calls to `match_taxonomic_names()`.

## Proposed Changes

### Option 1: Minimal Refactoring (Recommended)

**Approach**: Keep `query_taxa()` interface unchanged, but replace internal matching logic.

**Benefits**:
- Full backward compatibility
- No changes to existing user code
- Cleaner implementation
- Leverages new intelligent matching

**Changes**:

1. **For species matching** (lines 203-242):
   ```r
   # OLD (40 lines of code):
   q_res <- query_exact_match(tbl = "table_taxa", field = c("tax_gen", "tax_esp"), ...)
   if (!exact_match & any(is.na(q_res$query_tb$id))) {
     # Loop and fuzzy match each name...
   }

   # NEW (8 lines of code):
   if (exact_match) {
     # Keep existing SQL for exact match (fast)
     res_species <- query_exact_match(...)
   } else {
     # Use new intelligent matching
     matches <- match_taxonomic_names(
       names = species,
       method = "hierarchical",
       max_matches = 1,
       min_similarity = 0.3,
       include_synonyms = FALSE,
       con = mydb_taxa,
       verbose = verbose
     )
     res_species <- matches %>%
       filter(match_rank == 1) %>%
       select(idtax_n, idtax_good_n)
   }
   ```

2. **For genus/family/order matching** (similar pattern):
   - Use `match_taxonomic_names()` when `exact_match = FALSE`
   - Keep direct SQL for `exact_match = TRUE` (performance)

3. **Mark `query_exact_match()` and `query_fuzzy_match()` as superseded**:
   ```r
   #' @description
   #' \lifecycle{superseded} This function is superseded by the matching functions
   #' in `taxonomic_matching.R`. Use `match_taxonomic_names()` instead for more
   #' intelligent matching with genus-constrained search.
   ```

### Option 2: Full Rewrite with Breaking Changes

**Approach**: Completely rewrite `query_taxa()` to use new architecture.

**Benefits**:
- Simpler implementation
- Better matching quality
- Consistent with new functions

**Drawbacks**:
- Breaking changes for existing code
- More work to implement
- Need to update documentation and tests
- May require version bump

**Not recommended** for this refactoring phase.

## Implementation Plan

### Phase 1: Refactor `query_taxa()` (Current Task)

1. **Update species matching** (lines 203-242):
   - Replace with `match_taxonomic_names()` when `exact_match = FALSE`
   - Keep existing SQL for `exact_match = TRUE`

2. **Update genus matching** (lines 162-201):
   - Replace with `match_taxonomic_names()` when `exact_match = FALSE`
   - Keep existing SQL for `exact_match = TRUE`

3. **Update family matching** (lines 121-160):
   - Replace with `match_taxonomic_names()` when `exact_match = FALSE`
   - Keep existing SQL for `exact_match = TRUE`

4. **Update order matching** (lines 80-120):
   - Replace with `match_taxonomic_names()` when `exact_match = FALSE`
   - Keep existing SQL for `exact_match = TRUE`

5. **Test thoroughly**:
   - Ensure backward compatibility
   - Check performance
   - Verify synonym handling

### Phase 2: Mark Old Functions as Superseded

1. **Add lifecycle badges** to `query_fuzzy_match()` and `query_exact_match()`:
   ```r
   #' @description
   #' \lifecycle{superseded}
   ```

2. **Update documentation** to point users to new functions

3. **Add deprecation warnings** (optional, for next major version):
   ```r
   lifecycle::deprecate_soft("1.4.0", "query_fuzzy_match()", "match_taxonomic_names()")
   ```

### Phase 3: Future Cleanup (Next Major Version)

1. **Remove `query_fuzzy_match()` and `query_exact_match()`** from `helpers.R`
2. **Update all internal code** that uses these functions
3. **Version bump** to indicate breaking changes

## Estimated Code Reduction

**Before refactoring**:
- `query_taxa()`: ~240 lines of matching logic (lines 78-242)
- Total with helpers: ~320 lines

**After refactoring**:
- `query_taxa()`: ~80 lines of matching logic (estimate)
- Reduction: ~160 lines (50% reduction)
- Cleaner, more maintainable code

## Testing Strategy

1. **Unit tests** for refactored sections:
   - Test exact match still works
   - Test fuzzy match produces same/better results
   - Test with synonyms
   - Test with missing names

2. **Integration tests**:
   - Run existing code that uses `query_taxa()`
   - Compare results before/after
   - Check performance

3. **Edge cases**:
   - Empty inputs
   - NA values
   - Special characters
   - Very long names

## Backward Compatibility Guarantees

✅ **Guaranteed**:
- Function signature unchanged
- Return format unchanged
- Default behavior unchanged
- All existing code continues to work

⚠️ **May differ**:
- Exact matches returned when fuzzy matching (better quality)
- Order of results (now ranked by similarity score)
- Performance (should be faster, but different query patterns)

## Recommendation

**Proceed with Option 1 (Minimal Refactoring)** for the following reasons:

1. ✅ Maintains full backward compatibility
2. ✅ Reduces code duplication significantly
3. ✅ Improves match quality with genus-constrained search
4. ✅ Can be done incrementally (one matching level at a time)
5. ✅ Low risk of breaking existing user code
6. ✅ Provides foundation for future improvements

## Next Steps

1. Implement refactored `query_taxa()` function
2. Test thoroughly with existing workflows
3. Ask user for approval before committing
4. Update NEWS.md under "Code Refactoring" section
5. Mark old helper functions as superseded in documentation

---

**Created**: 2025-10-20
**Branch**: feature/taxonomic-name-standardization
**Related Files**:
- `R/taxonomic_query_functions.R` (to be modified)
- `R/taxonomic_matching.R` (new helpers)
- `R/helpers.R` (functions to be superseded)
