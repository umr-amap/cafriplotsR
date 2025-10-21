# Adding tax_level Field to table_taxa

**Date**: 2025-10-20
**Status**: Proposal - Ready to Implement

## Problem Statement

The `table_taxa` table currently lacks an explicit field indicating the taxonomic level of each entry. This creates several issues:

### Current Workaround (Problematic)

To extract genus-level taxa:
```r
# Current approach - error-prone
genera <- tbl(mydb_taxa, "table_taxa") %>%
  filter(!is.na(tax_gen), is.na(tax_esp)) %>%  # Assumes empty species = genus level
  collect()
```

**Problems with this approach:**
1. **Ambiguous**: Empty `tax_esp` could mean missing data OR genus-level taxon
2. **Fragile**: Relies on data entry consistency
3. **Verbose**: Requires multiple conditions for each query
4. **Error-prone**: Easy to forget edge cases (infraspecific taxa, etc.)

### Examples of Current Issues

**Issue 1: Infraspecific taxa filtering**
```r
# Want species-level only, but hard to exclude infraspecific
species <- tbl(mydb_taxa, "table_taxa") %>%
  filter(!is.na(tax_esp)) %>%  # Gets BOTH species AND infraspecific!
  collect()
```

**Issue 2: Genus-level filtering**
```r
# Current: Need complex logic
genera <- tbl(mydb_taxa, "table_taxa") %>%
  filter(
    !is.na(tax_gen),
    is.na(tax_esp),           # No species
    is.na(tax_nam01),         # No infraspecific 1
    is.na(tax_nam02)          # No infraspecific 2
  ) %>%
  collect()
```

**Issue 3: Family-level filtering**
```r
# Current: Need complex logic
families <- tbl(mydb_taxa, "table_taxa") %>%
  filter(
    !is.na(tax_fam),
    is.na(tax_gen),           # No genus
    is.na(tax_esp),           # No species
    is.na(tax_nam01),         # No infraspecific 1
    is.na(tax_nam02)          # No infraspecific 2
  ) %>%
  collect()
```

---

## Proposed Solution

### Add Explicit `tax_level` Field

Add a new column to `table_taxa`:
```sql
ALTER TABLE table_taxa ADD COLUMN tax_level VARCHAR(50)
```

Populate with one of these values:
- `"infraspecific"` - Taxa with infraspecific components (variety, subspecies, etc.)
- `"species"` - Species-level taxa (has genus + species epithet)
- `"genus"` - Genus-level taxa (has genus only)
- `"family"` - Family-level taxa (has family only)
- `"higher"` - Higher taxonomic levels (order, class, phylum, etc.)

### Logic for Population

```sql
UPDATE table_taxa
SET tax_level = CASE
  WHEN tax_nam01 IS NOT NULL AND tax_nam01 != '' THEN 'infraspecific'
  WHEN tax_nam02 IS NOT NULL AND tax_nam02 != '' THEN 'infraspecific'
  WHEN tax_esp IS NOT NULL AND tax_esp != '' THEN 'species'
  WHEN tax_gen IS NOT NULL AND tax_gen != '' THEN 'genus'
  WHEN tax_fam IS NOT NULL AND tax_fam != '' THEN 'family'
  ELSE 'higher'
END
```

---

## Benefits

### 1. Cleaner Queries

**Before** (complex conditions):
```r
genera <- tbl(mydb_taxa, "table_taxa") %>%
  filter(!is.na(tax_gen), is.na(tax_esp), is.na(tax_nam01), is.na(tax_nam02)) %>%
  collect()
```

**After** (simple and explicit):
```r
genera <- tbl(mydb_taxa, "table_taxa") %>%
  filter(tax_level == "genus") %>%
  collect()
```

### 2. Unambiguous Intent

```r
# Species-level ONLY (not infraspecific)
species <- tbl(mydb_taxa, "table_taxa") %>%
  filter(tax_level == "species") %>%
  collect()

# Species AND infraspecific
species_all <- tbl(mydb_taxa, "table_taxa") %>%
  filter(tax_level %in% c("species", "infraspecific")) %>%
  collect()
```

### 3. Better Performance with Index

```sql
CREATE INDEX idx_table_taxa_tax_level ON table_taxa(tax_level)
```

Queries filtering by `tax_level` will be faster than multi-column checks.

### 4. Easier Data Validation

```sql
-- Find records with missing level
SELECT * FROM table_taxa WHERE tax_level IS NULL;

-- Distribution by level
SELECT tax_level, COUNT(*) FROM table_taxa GROUP BY tax_level;
```

### 5. Improved Batch Matching

In `mod_auto_matching.R`, the batch matching becomes cleaner:

**Before**:
```r
unique_genera <- backbone %>%
  filter(!is.na(tax_gen), is.na(tax_esp)) %>%  # Complex
  ...
```

**After**:
```r
unique_genera <- backbone %>%
  filter(tax_level == "genus") %>%  # Clear intent
  ...
```

---

## Implementation Steps

### Step 1: Run the Script

```r
source("add_tax_level_field.R")
```

This will:
1. Check if column exists
2. Add `tax_level` column to `table_taxa`
3. Populate values based on logic above
4. Create index for performance
5. Verify results with summary statistics
6. Show examples of each level
7. Test queries

### Step 2: Update Batch Matching Code

Replace the batch matching logic in `R/mod_auto_matching.R` with the updated version from `mod_auto_matching_UPDATED_with_tax_level.R`.

**Key changes**:
```r
# OLD (complex filtering)
unique_genera <- backbone %>%
  filter(!is.na(tax_gen_level), is.na(tax_esp)) %>%
  ...

# NEW (explicit level filtering)
unique_genera <- backbone %>%
  filter(tax_level == "genus", !is.na(tax_gen_level)) %>%
  ...
```

### Step 3: Update Other Functions

Search for similar patterns in other query functions:

```bash
# Find functions using the old pattern
grep -r "is.na(tax_esp)" R/
```

Update them to use `tax_level` field.

### Step 4: Add to Package Functions

Consider adding a helper function:

```r
#' Get Taxa by Level
#'
#' Retrieve taxa at a specific taxonomic level
#'
#' @param level Character, taxonomic level: "species", "genus", "family", "infraspecific", "higher"
#' @param unique_only Logical, return only unique names (no duplicates)
#' @param con Database connection, defaults to call.mydb.taxa()
#'
#' @return Data frame of taxa at specified level
#'
#' @export
get_taxa_by_level <- function(level = c("species", "genus", "family", "infraspecific", "higher"),
                              unique_only = FALSE,
                              con = NULL) {

  level <- match.arg(level)

  if (is.null(con)) {
    con <- call.mydb.taxa()
    on.exit(DBI::dbDisconnect(con))
  }

  result <- dplyr::tbl(con, "table_taxa") %>%
    dplyr::filter(tax_level == !!level) %>%
    dplyr::collect()

  if (unique_only) {
    name_col <- switch(level,
      "species" = "tax_esp",
      "genus" = "tax_gen",
      "family" = "tax_fam",
      "tax_gen"  # default for infraspecific/higher
    )

    result <- result %>%
      dplyr::group_by(!!rlang::sym(name_col)) %>%
      dplyr::filter(dplyr::n() == 1) %>%
      dplyr::ungroup()
  }

  return(result)
}
```

---

## Testing Checklist

After implementation:

### Data Integrity Tests

- [ ] All records have non-NULL `tax_level`
- [ ] Distribution of levels makes sense (most should be species/infraspecific)
- [ ] No unexpected values in `tax_level` column
- [ ] Records with `tax_nam01` are marked as "infraspecific"
- [ ] Records with only `tax_gen` are marked as "genus"
- [ ] Records with only `tax_fam` are marked as "family"

### Query Tests

- [ ] Filter by `tax_level == "species"` returns expected records
- [ ] Filter by `tax_level == "genus"` excludes species-level taxa
- [ ] Filter by `tax_level == "family"` works correctly
- [ ] Index improves query performance (compare with EXPLAIN ANALYZE)

### Integration Tests

- [ ] Batch matching in Shiny app works with updated code
- [ ] `query_taxa()` can use tax_level for filtering
- [ ] Other taxonomic functions benefit from clearer queries

### Edge Cases

- [ ] Records with missing taxonomy data handled correctly
- [ ] Infraspecific taxa with complex ranks correctly identified
- [ ] Higher-level taxa (order, class) marked as "higher"

---

## Expected Results

Running the script should show something like:

```
Taxonomic Level Distribution:
─────────────────────────────────────────
tax_level       n_records  percentage
infraspecific      12,543       8.5%
species           98,234      66.3%
genus             28,456      19.2%
family             6,234       4.2%
higher             2,678       1.8%
```

(Exact numbers depend on your database content)

---

## Files Created

1. **`add_tax_level_field.R`** - Script to add and populate the field
2. **`mod_auto_matching_UPDATED_with_tax_level.R`** - Updated batch matching code
3. **`TAX_LEVEL_FIELD_PROPOSAL.md`** - This document

---

## Migration Notes

### Before Running

1. **Backup the database** (especially table_taxa)
   ```bash
   pg_dump -U username -t table_taxa rainbio > table_taxa_backup.sql
   ```

2. **Ensure write permissions** on the taxa database
   ```r
   mydb_taxa <- call.mydb.taxa()
   # Should NOT show "READ-ONLY" warning
   ```

3. **Test on a copy first** if possible

### After Running

1. **Verify statistics** look reasonable
2. **Test a few manual queries** to confirm accuracy
3. **Update application code** to use the new field
4. **Remove old complex filtering patterns** from codebase

---

## Rollback Plan

If needed, remove the column:

```sql
ALTER TABLE table_taxa DROP COLUMN tax_level;
```

This is safe because the original data is not modified - we only added a new column.

---

## Future Enhancements

### Phase 2: Add Validation Constraints

```sql
ALTER TABLE table_taxa
ADD CONSTRAINT check_tax_level
CHECK (tax_level IN ('infraspecific', 'species', 'genus', 'family', 'higher'));
```

### Phase 3: Add Trigger for Automatic Updates

```sql
CREATE OR REPLACE FUNCTION update_tax_level()
RETURNS TRIGGER AS $$
BEGIN
  NEW.tax_level := CASE
    WHEN NEW.tax_nam01 IS NOT NULL AND NEW.tax_nam01 != '' THEN 'infraspecific'
    WHEN NEW.tax_nam02 IS NOT NULL AND NEW.tax_nam02 != '' THEN 'infraspecific'
    WHEN NEW.tax_esp IS NOT NULL AND NEW.tax_esp != '' THEN 'species'
    WHEN NEW.tax_gen IS NOT NULL AND NEW.tax_gen != '' THEN 'genus'
    WHEN NEW.tax_fam IS NOT NULL AND NEW.tax_fam != '' THEN 'family'
    ELSE 'higher'
  END;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_update_tax_level
BEFORE INSERT OR UPDATE ON table_taxa
FOR EACH ROW
EXECUTE FUNCTION update_tax_level();
```

This ensures `tax_level` is always correct when taxa are added/updated.

---

## Conclusion

Adding the `tax_level` field will:

✅ **Simplify queries** - No more complex multi-column filters
✅ **Improve clarity** - Explicit intent in code
✅ **Boost performance** - Indexed column for faster filtering
✅ **Reduce errors** - Less ambiguity about taxonomic levels
✅ **Enable validation** - Easy to spot data quality issues

**Recommendation**: Implement this change. The benefits far outweigh the small implementation effort.

---

**Next Steps**:
1. Review this proposal
2. Run `add_tax_level_field.R` script
3. Verify results
4. Update `mod_auto_matching.R` with cleaner filtering
5. Gradually update other functions to use `tax_level`

---

**Author**: Claude Code Assistant
**Date**: 2025-10-20
**Status**: Ready to implement
