# Hierarchy Consistency Guide

## Overview

The `table_taxa` database uses a **HYBRID system**:

- **Flat columns** (`tax_gen`, `tax_fam`, `tax_order`, `tax_famclass`): Used for fast querying
- **Hierarchical structure** (`id_parent`): Used for tree navigation and hierarchy operations

**Both systems must stay in sync** to maintain data integrity.

---

## Key Functions

### 1. `check_hierarchy_consistency()` - Validate Consistency

Checks if flat columns match the hierarchy defined by `id_parent`.

```r
library(CafriplotsR)
con <- call.mydb.taxa()

# Check for inconsistencies (read-only)
issues <- check_hierarchy_consistency(con)

# Check and automatically fix inconsistencies
check_hierarchy_consistency(con, fix = TRUE)
```

**What it checks:**
- Species: Does `tax_gen` match parent genus?
- Genus: Does `tax_fam` match parent family?
- Family: Does `tax_order` match parent order?
- Order: Does `tax_famclass` match parent class?
- Infraspecific: Do `tax_gen` and `tax_esp` match parent species?

**Returns:**
- `NULL` if all consistent
- List of inconsistencies if problems found

---

### 2. `update_taxon_parent()` - Safe Parent Update

**This is the SAFE way to modify hierarchy relationships.**

Updates both `id_parent` AND flat columns to maintain consistency.

```r
# Example: Move species to a different genus
update_taxon_parent(
  idtax_n = 12345,         # Species ID
  new_parent_id = 67890,   # New genus ID
  con = con
)
# This updates BOTH:
# - id_parent = 67890
# - tax_gen = (genus name from parent)
```

**Use cases:**
- Moving species to different genus
- Reassigning genus to different family
- Correcting misplaced taxa in hierarchy

**Features:**
- Validates parent-child relationship (e.g., species can only be child of genus)
- Updates flat columns automatically
- Transaction-safe with rollback on error

---

## When to Use These Functions

### ✅ DO Use `update_taxon_parent()` when:

```r
# Moving a species to a different genus
update_taxon_parent(
  idtax_n = species_id,
  new_parent_id = new_genus_id,
  con = con
)
```

### ❌ DON'T Update `id_parent` directly:

```r
# BAD - Creates inconsistency!
DBI::dbExecute(con, sprintf("
  UPDATE table_taxa
  SET id_parent = %d
  WHERE idtax_n = %d
", new_parent_id, taxon_id))
```

### 🔍 Check Consistency Regularly:

```r
# Weekly/monthly consistency check
issues <- check_hierarchy_consistency(con)

if (!is.null(issues)) {
  # Fix automatically
  check_hierarchy_consistency(con, fix = TRUE)
}
```

---

## Example Workflow

### Scenario: Correcting a species that was placed in wrong genus

```r
library(CafriplotsR)

con <- call.mydb.taxa()

# 1. Find the species that needs correction
species <- query_taxa(genus = "Pinus", species = "alba", con = con)
species_id <- species$idtax_n[1]
# Current: Pinus alba (id_parent → genus Pinus)

# 2. Find the correct genus
correct_genus <- query_taxa(genus = "Picea", con = con) %>%
  filter(tax_level == "genus")
correct_genus_id <- correct_genus$idtax_n[1]

# 3. Move species to correct genus (SAFE way)
update_taxon_parent(
  idtax_n = species_id,
  new_parent_id = correct_genus_id,
  con = con
)
# Now: Picea alba (id_parent → genus Picea, tax_gen = "Picea")

# 4. Verify the change
updated <- query_taxa(ids = species_id, con = con)
print(updated[, c("idtax_n", "tax_gen", "tax_esp", "id_parent")])

# 5. Check for any other inconsistencies
check_hierarchy_consistency(con)
```

---

## Validation & Monitoring

### Run consistency checks periodically:

```r
# Create a monitoring script
library(CafriplotsR)

con <- call.mydb.taxa()

# Check consistency
cli::cli_h1("Monthly Hierarchy Consistency Check")
issues <- check_hierarchy_consistency(con, limit = 1000)

if (!is.null(issues)) {
  cli::cli_alert_warning("Found inconsistencies - review before fixing")

  # Review issues
  print(issues)

  # Decide whether to auto-fix or manual review
  response <- readline("Fix automatically? (y/n): ")

  if (tolower(response) == "y") {
    check_hierarchy_consistency(con, fix = TRUE)
  }
} else {
  cli::cli_alert_success("Hierarchy is consistent!")
}

cleanup_connections()
```

---

## Best Practices

### 1. Always use `update_taxon_parent()` for hierarchy changes
```r
# ✅ GOOD
update_taxon_parent(idtax_n = id, new_parent_id = parent_id, con = con)

# ❌ BAD
DBI::dbExecute(con, "UPDATE table_taxa SET id_parent = ... WHERE ...")
```

### 2. Run consistency checks after bulk operations
```r
# After migration or bulk update
check_hierarchy_consistency(con, fix = TRUE)
```

### 3. Use flat columns for queries (fast)
```r
# ✅ Fast - uses index on tax_gen
query_taxa(genus = "Pinus", con = con)

# ❌ Slow - requires recursive CTE
get_taxon_children(genus_id, con = con, recursive = TRUE)
```

### 4. Use id_parent for hierarchy navigation
```r
# Get all species in a genus (hierarchy-based)
get_taxon_children(genus_id, con = con)

# Get full lineage
get_taxon_ancestors(species_id, con = con)
```

---

## Architecture Summary

```
┌─────────────────────────────────────────────────────────┐
│                    HYBRID SYSTEM                        │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  FLAT COLUMNS                  HIERARCHICAL             │
│  (for queries)                 (for navigation)         │
│                                                         │
│  tax_gen                       id_parent                │
│  tax_fam          ←→ SYNC ←→   ↓ ↑                     │
│  tax_order                     (recursive)              │
│  tax_famclass                                           │
│                                                         │
├─────────────────────────────────────────────────────────┤
│  Consistency Functions:                                 │
│  - check_hierarchy_consistency()                        │
│  - update_taxon_parent()                                │
└─────────────────────────────────────────────────────────┘
```

**Key principle:** Flat columns and `id_parent` must always agree!

---

## Troubleshooting

### Q: I found inconsistencies, what should I do?

```r
# 1. Review the issues
issues <- check_hierarchy_consistency(con)
print(issues)

# 2. Fix automatically (flat columns will match id_parent)
check_hierarchy_consistency(con, fix = TRUE)

# 3. Verify fix worked
check_hierarchy_consistency(con)
```

### Q: Can I update flat columns directly?

**Not recommended** unless you also update `id_parent`. Use `update_taxon_parent()` instead.

### Q: What if I need to update genus name globally?

```r
# This requires updating:
# 1. The genus entry itself
# 2. All children (species, subspecies) via flat columns

# Better to use a dedicated function (to be created if needed)
```

---

## See Also

- `get_taxon_children()` - Get descendants via hierarchy
- `get_taxon_ancestors()` - Get ancestors via hierarchy
- `get_taxon_hierarchy()` - Full hierarchy as nested list
- `verify_hierarchy_integrity()` - Overall hierarchy health check
