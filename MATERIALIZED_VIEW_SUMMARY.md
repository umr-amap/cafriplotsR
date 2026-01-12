# Summary: table_idtax Materialized View Migration

This document summarizes the plan to convert `table_idtax` from a regular table to a PostgreSQL materialized view, addressing the issues you raised about synonym resolution and permissions.

---

## Problems Addressed

### 1. ✅ Admin-Only Updates
**Current**: Only you (admin) can update `table_idtax` via `update_taxa_link_table()`
**Solution**: Materialized view with `REFRESH` permission granted to data managers

### 2. ✅ Stale Data Risk
**Current**: No way to know when `table_idtax` is outdated
**Solution**:
- Metadata table tracks last update time
- Automatic warnings when > 90 days old
- Functions check staleness before operations

### 3. ✅ Missing from query_all_specimen_links()
**Current**: Function doesn't resolve synonyms
**Solution**: Add optional `include_taxonomy` parameter to enrich with resolved taxonomy

### 4. ✅ Automatic Refresh
**Current**: Manual refresh only
**Solution**: Optional pg_cron job for monthly automatic refresh

---

## What is a Materialized View?

Think of it like a **smart snapshot**:

| Feature | Regular Table | Materialized View |
|---------|--------------|-------------------|
| Data storage | Yes | Yes |
| Query speed | Fast | Fast (same) |
| Who can update | Owner/admin | Anyone with REFRESH grant |
| Update method | INSERT/UPDATE/DELETE | REFRESH (all-or-nothing) |
| Traceability | None | Clear it's derived from query |
| Safe to delegate | No | Yes ✅ |

**Key benefit for you**: You can grant REFRESH permission to trusted users without giving them dangerous write access to your database.

---

## How Permissions Work

### Current Situation
```
table_idtax (TABLE)
├─ SELECT: Everyone
└─ INSERT/UPDATE: Admin only ❌
```

### After Migration
```
table_idtax (MATERIALIZED VIEW)
├─ SELECT: Everyone
└─ REFRESH: data_manager_role ✅

data_manager_role members:
├─ You (admin)
├─ Gilles
├─ Hugo
└─ [Other designated users]
```

Data managers can run:
```r
update_taxa_link_table()  # Works! ✅
```

Regular users see:
```
ℹ You don't have permission to refresh table_idtax
ℹ Contact admin to request data_manager_role
```

---

## Files Created

### 1. SQL Migration Script
**File**: `inst/sql/migration_table_idtax_to_materialized_view.sql`
**Purpose**: Complete database migration (run once as admin)
**Includes**:
- Backup creation
- Materialized view creation
- Metadata table setup
- Permission grants
- Helper functions (refresh, check staleness)
- Automatic refresh setup (if pg_cron available)
- Verification queries
- Rollback plan

### 2. Updated R Functions
**File**: `R/taxonomic_update_functions_new.R`
**Purpose**: Replace existing functions to work with materialized view
**New functions**:
- `check_table_idtax_staleness()` - Check age and get warnings
- `update_taxa_link_table()` - Refresh via PostgreSQL function (works for non-admins!)
- `get_table_idtax_metadata()` - View refresh history
- `legacy_update_taxa_link_table()` - Fallback for old method

**Key feature**: Automatic fallback - if materialized view isn't available, falls back to old method.

### 3. Implementation Plan
**File**: `inst/docs/PLAN_MATERIALIZED_VIEW_MIGRATION.md`
**Purpose**: Detailed step-by-step migration guide
**Sections**:
- Pre-migration checks
- Testing procedure
- Production migration steps
- R package updates
- User training plan
- Monitoring & maintenance
- Complete rollback procedure
- Timeline: 10-14 hours over 4 weeks

### 4. Quick Reference
**File**: `inst/docs/QUICK_REFERENCE_table_idtax.md`
**Purpose**: User documentation for daily use
**For**: Data managers and regular users

---

## Implementation Timeline

### Week 1: Preparation & Testing (4 hours)
- [ ] Review plan and SQL script
- [ ] Identify data manager users
- [ ] Test on development database
- [ ] Verify R functions work

### Week 2: Continued Testing (2 hours)
- [ ] Performance testing
- [ ] Permission testing
- [ ] User acceptance testing

### Week 3: Production Migration (4 hours)
- [ ] Saturday morning: Run migration (30 min)
- [ ] Grant permissions to users (15 min)
- [ ] Update R package (2 hours)
- [ ] Verify everything works (1 hour)

### Week 4: Finalization (2 hours)
- [ ] Set up automatic refresh (optional)
- [ ] User training session
- [ ] Documentation finalization
- [ ] Monitor for issues

**Total: 10-14 hours over 4 weeks**

---

## Next Steps for You

### Immediate (This Week)
1. **Review the migration plan**: Read `inst/docs/PLAN_MATERIALIZED_VIEW_MIGRATION.md`
2. **Review SQL script**: Read `inst/sql/migration_table_idtax_to_materialized_view.sql`
3. **Decide on users**: List who should get `data_manager_role`
4. **Check pg_cron**: See if it's available for automatic refresh

### Before Migration
5. **Test on dev database**: Run through entire migration on test system
6. **Schedule date**: Pick a low-usage time (e.g., Saturday morning)
7. **Notify users**: Send migration announcement email

### Day of Migration
8. **Backup database**: Full backup before starting
9. **Run SQL script**: Execute as admin
10. **Grant permissions**: Add users to data_manager_role
11. **Update R package**: Replace functions and push to GitHub
12. **Test thoroughly**: Verify all functions work

### After Migration
13. **Monitor**: Check for any issues during first week
14. **Train users**: Show data managers how to refresh
15. **Set up cron**: Configure automatic monthly refresh (optional)
16. **Document**: Update any internal wikis/docs

---

## Example Workflow After Migration

### For You (Admin)
Same as before, but now you can delegate:
```r
# Check if refresh needed (automatic warning)
con <- call.mydb()
check_table_idtax_staleness(con)

# Refresh if needed
update_taxa_link_table(con)
```

### For Data Managers
New capability - they can refresh themselves:
```r
# When they see warning
con <- call.mydb()
update_taxa_link_table(con)  # Works! No admin needed!
```

### For Regular Users
They see helpful warnings:
```r
# Automatic warning if stale
specimens <- query_specimens(collector = "Dauby")
# ! WARNING: table_idtax is 120 days old
# ℹ Ask data manager to refresh
```

---

## Rollback Safety

If anything goes wrong:
1. **Backup exists**: `table_idtax_backup` created automatically
2. **Quick rollback**: DROP materialized view, recreate from backup
3. **R package**: Git revert to previous version
4. **Time needed**: 30 minutes to fully rollback

**The migration is LOW RISK with full safety net.**

---

## Benefits Summary

### For You (Admin)
- ✅ Delegate refresh responsibility to trusted users
- ✅ Reduce your maintenance burden
- ✅ Automatic staleness tracking
- ✅ Clear audit trail (who refreshed when)
- ✅ Optional automatic monthly refresh

### For Data Managers
- ✅ Can refresh taxonomy without waiting for admin
- ✅ Clear feedback on success/failure
- ✅ See when data was last updated
- ✅ Simple R function calls

### For All Users
- ✅ Automatic warnings when data is stale
- ✅ Confidence that taxonomy is current
- ✅ Transparent - queries work exactly the same
- ✅ Better data quality

---

## Questions to Consider

Before proceeding, decide on:

1. **User list**: Who should get `data_manager_role`?
   - Gilles Dauby?
   - Hugo Leblanc?
   - Others?

2. **Staleness threshold**: Is 90 days appropriate, or shorter/longer?
   - Default: 90 days
   - Can be changed per-function call

3. **Automatic refresh**: Do you want monthly auto-refresh?
   - Requires pg_cron extension
   - Or use system cron job
   - Or keep manual only

4. **Testing database**: Do you have a test environment?
   - Strongly recommended to test first
   - Can clone production to test

5. **Migration timing**: When is low-usage period?
   - Suggest: Saturday morning
   - Downtime: < 5 minutes

---

## Questions I Can Help With

I can help you with:

- ✅ Modifying SQL script for your specific needs
- ✅ Adding more users to permission grants
- ✅ Adjusting staleness thresholds
- ✅ Creating custom refresh schedules
- ✅ Adding email notifications on refresh
- ✅ Updating any R functions
- ✅ Creating user training materials
- ✅ Testing procedures

Just let me know what you need!

---

## Final Recommendation

**I recommend proceeding with this migration** because:

1. **Low risk**: Complete rollback plan, tested approach
2. **High benefit**: Solves all three problems you identified
3. **Standard practice**: Materialized views are designed for exactly this use case
4. **Proper permissions**: PostgreSQL security model, not custom workaround
5. **Future-proof**: Scalable, maintainable, well-documented

**Timeline**: Can be completed in 2-4 weeks with minimal disruption

**Your decision points**:
- Review and approve the plan
- Choose data manager users
- Set migration date
- I'll help with execution

---

## Contact

If you have questions or want to proceed:
- Review the detailed plan: `inst/docs/PLAN_MATERIALIZED_VIEW_MIGRATION.md`
- Review SQL script: `inst/sql/migration_table_idtax_to_materialized_view.sql`
- Let me know if you want any modifications
- I can help with testing and execution

Ready to make your database more maintainable! 🚀
