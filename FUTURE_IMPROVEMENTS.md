# Future Improvements for CafriplotsR

This file tracks enhancement ideas and architectural improvements for future development.

---

## 1. Database-Backed Synonym System for Traits

**Current Issue:**
Trait synonyms for column mapping are hardcoded in `.get_trait_column_synonyms()` (R/import_individuals_column_mapping.R). This means:
- Only ~20-30 common traits have synonyms defined (stem_diameter, tree_height, etc.)
- New traits created via the import wizard won't have synonyms unless R code is manually updated
- Maintenance burden for adding synonyms to new traits

**Proposed Solution:**
Add a `synonyms` column to `table_traits` table to store trait synonyms in the database.

**Implementation Steps:**
1. Add `synonyms` column to `table_traits` (TEXT or JSON type)
2. Modify `add_trait()` to accept a `synonyms` parameter
3. Update `.get_trait_column_synonyms()` to:
   - Load synonyms from database via `traits_list()`
   - Merge with hardcoded common synonyms (backward compatible)
   - Cache for performance
4. Update import wizard "Create New Feature" modal to allow synonym input
5. Consider same approach for plot features (`subplotype_list`)

**Benefits:**
- Scalable: No R code changes needed for new traits
- User-friendly: Users can define synonyms when creating traits
- Maintainable: Synonyms stored with trait definitions in database
- Backward compatible: Keep hardcoded synonyms for common traits

**Priority:** Medium (current hardcoded approach covers 80%+ of use cases)

**Related Files:**
- `R/import_individuals_column_mapping.R` - `.get_trait_column_synonyms()`
- `R/import_column_mapping.R` - `.get_subplot_feature_synonyms()` (plot features)
- `R/updates_tables_functions.R` - `add_trait()`, `add_subplottype()`
- `R/individual_features_function.R` - `traits_list()`
- `R/subsplots_features_function.R` - `subplot_list()`

---

## 2. Other Future Enhancements

(Add other improvement ideas here as they arise)

