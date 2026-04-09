# Migration Plan: table_idtax to PostgreSQL Materialized View

**Date**: 2026-01-12
**Author**: Database Administrator
**Version**: 1.0
**Status**: PLANNING

---

## Executive Summary

Convert `table_idtax` from a regular PostgreSQL table to a materialized view to:
- Allow designated non-admin users to refresh taxonomy mappings
- Add automatic staleness detection and warnings
- Set up automatic monthly refresh
- Maintain data integrity with proper PostgreSQL permission model

**Estimated Time**: 2-4 hours
**Risk Level**: LOW (full rollback plan included)
**User Impact**: None (transparent to queries)

---

## Current Architecture Issues

### Problem 1: Admin-Only Updates
- `table_idtax` is a regular table updated via `dbWriteTable()`
- Only database admin has write permissions
- Data managers cannot refresh even when taxonomy DB is updated

### Problem 2: No Staleness Tracking
- No automatic way to know when data is outdated
- Users don't know if synonym resolution is using stale data
- No warnings when cache is 3+ months old

### Problem 3: Missing in query_all_specimen_links()
- Function retrieves specimen data but doesn't resolve synonyms
- Users get raw `idtax_n` instead of resolved `idtax_good_n`
- Inconsistent with other query functions

---

## Proposed Solution: Materialized View

### What is a Materialized View?

A PostgreSQL materialized view is a database object that:
- Stores the result of a query physically (fast like a table)
- Can be refreshed on-demand to update data
- Has separate permissions for SELECT vs REFRESH
- Supports concurrent refresh (non-blocking)

### Architecture Changes

```
BEFORE:                          AFTER:
┌─────────────────┐             ┌─────────────────┐
│  table_idtax    │             │  table_idtax    │
│   (TABLE)       │             │ (MAT. VIEW)     │
│                 │             │                 │
│ Update: Admin   │    →        │ Refresh: Data   │
│ only via        │             │ Managers via    │
│ dbWriteTable()  │             │ SQL function    │
└─────────────────┘             └─────────────────┘
                                         ↓
                                ┌────────────────┐
                                │ Metadata Table │
                                │ - last_updated │
                                │ - updated_by   │
                                │ - record_count │
                                └────────────────┘
                                         ↓
                                ┌────────────────┐
                                │  Auto Warning  │
                                │  if > 90 days  │
                                └────────────────┘
```

---

## Implementation Plan

### Phase 1: Pre-Migration (Week 1)

#### Step 1.1: Database Assessment (30 min)
- [ ] Connect to production database as admin
- [ ] Check current `table_idtax` record count
- [ ] Verify admin access to taxa database
- [ ] Check if `pg_cron` extension is available (optional for auto-refresh)

**Commands:**
```sql
-- Check current data
SELECT COUNT(*) FROM table_idtax;
SELECT COUNT(DISTINCT idtax_n) FROM table_idtax;

-- Check extensions
SELECT * FROM pg_extension WHERE extname = 'pg_cron';
```

#### Step 1.2: User Role Planning (15 min)
- [ ] Identify users who need REFRESH permission (data managers)
- [ ] Document usernames for GRANT statements
- [ ] Confirm with users they will test after migration

**Example:**
```sql
-- List of users to grant permissions:
-- - dauby (Gilles)
-- - hugo
-- - marie
```

#### Step 1.3: Backup Strategy (30 min)
- [ ] Plan backup timing (low-usage period)
- [ ] Test backup restoration procedure
- [ ] Document rollback steps

**Backup Commands:**
```bash
# Full database backup (recommended)
pg_dump -h hostname -U admin -d plots_transects > backup_pre_migration_$(date +%Y%m%d).sql

# Just table_idtax
pg_dump -h hostname -U admin -d plots_transects -t table_idtax > backup_table_idtax.sql
```

---

### Phase 2: Development Environment Testing (Week 1-2)

#### Step 2.1: Create Test Database (1 hour)
- [ ] Clone production database to test environment
- [ ] Verify test data matches production structure
- [ ] Create test users with limited permissions

#### Step 2.2: Run Migration Script on Test (30 min)
- [ ] Execute `inst/sql/migration_table_idtax_to_materialized_view.sql`
- [ ] Verify all objects created successfully
- [ ] Check backup table `table_idtax_backup` exists

**Execution:**
```bash
psql -h test-hostname -U admin -d test_plots_transects -f inst/sql/migration_table_idtax_to_materialized_view.sql
```

#### Step 2.3: Test R Package Functions (1 hour)
- [ ] Install updated package on test machine
- [ ] Test `check_table_idtax_staleness()` with test user
- [ ] Test `update_taxa_link_table()` with test data manager user
- [ ] Test `update_taxa_link_table()` with test non-privileged user (should show permission message)
- [ ] Verify queries still work: `query_specimens()`, `query_all_specimen_links()`

**R Test Script:**
```r
library(CafriplotsR)

# Test as data manager
con <- call.mydb()  # Test credentials

# Check staleness
staleness <- check_table_idtax_staleness(con)
print(staleness)

# Refresh (should work for data managers)
result <- update_taxa_link_table(con)
print(result)

# Verify queries work
specimens <- query_specimens(collector = "Dauby", con = con)
print(head(specimens))
```

#### Step 2.4: Performance Testing (30 min)
- [ ] Measure query performance before/after migration
- [ ] Test concurrent refresh (doesn't block queries)
- [ ] Verify index usage

**Benchmark:**
```r
# Before migration
system.time({
  result <- DBI::dbGetQuery(con, "SELECT * FROM table_idtax WHERE idtax_n IN (1,2,3,...,1000);")
})

# After migration (should be same or faster)
```

---

### Phase 3: Production Migration (Week 3)

#### Step 3.1: Preparation (Day 1 - Before Migration)
- [ ] Announce migration to users (email/Slack)
- [ ] Schedule migration during low-usage period (e.g., Saturday morning)
- [ ] Prepare rollback plan document
- [ ] Have backup admin available for emergencies

**Migration Announcement Template:**
```
Subject: Database Maintenance - table_idtax Upgrade

Dear CafriplotsR Users,

We will be upgrading the table_idtax database object on Saturday, [DATE] at [TIME].

What's changing:
- You will be able to refresh taxonomy mappings without admin access
- Automatic warnings when data is outdated
- No changes to how you query data

Expected downtime: < 5 minutes
Your action required: None (transparent change)

If you have questions, contact [ADMIN EMAIL].
```

#### Step 3.2: Execute Migration (30 min - Day 2)
- [ ] Connect to production database as admin
- [ ] Run migration script
- [ ] Verify all steps completed successfully
- [ ] Check no errors in PostgreSQL logs

**Execution:**
```bash
# Connect to production
psql -h prod-hostname -U admin -d plots_transects

# Run migration (with transcript logging)
\o migration_log_$(date +%Y%m%d_%H%M%S).txt
\i inst/sql/migration_table_idtax_to_materialized_view.sql
\o
```

#### Step 3.3: Verification (15 min - Day 2)
- [ ] Check materialized view exists and is populated
- [ ] Verify metadata table has correct data
- [ ] Test staleness check function
- [ ] Verify backup table intact

**Verification Commands:**
```sql
-- 1. Check materialized view
SELECT schemaname, matviewname, ispopulated
FROM pg_matviews
WHERE matviewname = 'table_idtax';

-- 2. Check record count matches backup
SELECT COUNT(*) as mv_count FROM table_idtax;
SELECT COUNT(*) as backup_count FROM table_idtax_backup;

-- 3. Test staleness function
SELECT * FROM check_table_idtax_staleness(90);

-- 4. Check metadata
SELECT * FROM table_idtax_metadata;
```

#### Step 3.4: Grant User Permissions (15 min - Day 2)
- [ ] Grant `data_manager_role` to designated users
- [ ] Test with one user account
- [ ] Verify non-privileged users cannot refresh

**Grant Commands:**
```sql
-- Grant to specific users
GRANT data_manager_role TO dauby;
GRANT data_manager_role TO hugo;
GRANT data_manager_role TO marie;

-- Verify grants
SELECT r.rolname as role, m.rolname as member
FROM pg_roles r
JOIN pg_auth_members am ON r.oid = am.roleid
JOIN pg_roles m ON am.member = m.oid
WHERE r.rolname = 'data_manager_role';
```

---

### Phase 4: R Package Update (Week 3)

#### Step 4.1: Update Package Code (1 hour)
- [ ] Replace old `update_taxa_link_table()` function
- [ ] Add new functions: `check_table_idtax_staleness()`, `get_table_idtax_metadata()`
- [ ] Update `.enrich_specimens_with_taxonomy()` to check staleness
- [ ] Update `query_all_specimen_links()` to optionally enrich with taxonomy

**Files to update:**
- `R/taxonomic_update_functions.R` - Replace with new version
- `R/functions_manip_db.R` - Update `.enrich_specimens_with_taxonomy()`
- `R/specimen_linking_functions.R` - Add taxonomy enrichment to `query_all_specimen_links()`

#### Step 4.2: Update Documentation (30 min)
- [ ] Update function documentation
- [ ] Update `NEWS.md` with breaking changes (if any)
- [ ] Create migration guide for users
- [ ] Update `README.md` with new workflow

#### Step 4.3: Testing (1 hour)
- [ ] Run `devtools::check()`
- [ ] Test all affected functions
- [ ] Verify backward compatibility
- [ ] Test with both admin and non-admin accounts

#### Step 4.4: Package Release (30 min)
- [ ] Update version number
- [ ] Tag release in git
- [ ] Build and test installation from GitHub
- [ ] Notify users of update

**Release commands:**
```bash
# Update DESCRIPTION version
# Update NEWS.md

git add -A
git commit -m "Version X.Y: Add materialized view support for table_idtax"
git tag vX.Y
git push origin master --tags

# Test installation
Rscript -e "devtools::install_github('umr-amap/cafriplotsR')"
```

---

### Phase 5: Automatic Refresh Setup (Week 4 - Optional)

#### Step 5.1: Check pg_cron Availability
- [ ] Verify `pg_cron` extension is installed
- [ ] If not, request installation from DB admin/hosting provider
- [ ] Test cron functionality

**Commands:**
```sql
-- Check if installed
SELECT * FROM pg_extension WHERE extname = 'pg_cron';

-- If not installed (requires superuser)
CREATE EXTENSION pg_cron;
```

#### Step 5.2: Schedule Monthly Refresh
- [ ] Create cron job for first day of month at 2 AM
- [ ] Test cron job execution
- [ ] Verify metadata updates after cron run
- [ ] Set up notification for failures (optional)

**Schedule Commands:**
```sql
-- Schedule refresh (first day of month at 2 AM)
SELECT cron.schedule(
    'refresh-table-idtax',
    '0 2 1 * *',  -- Cron expression: minute hour day month weekday
    'SELECT refresh_table_idtax();'
);

-- View scheduled jobs
SELECT * FROM cron.job WHERE jobname = 'refresh-table-idtax';

-- Check job run history
SELECT * FROM cron.job_run_details
WHERE jobid = (SELECT jobid FROM cron.job WHERE jobname = 'refresh-table-idtax')
ORDER BY start_time DESC
LIMIT 10;
```

#### Step 5.3: Alternative - System Cron (if pg_cron not available)
If `pg_cron` extension is not available, set up system-level cron:

**Create script** (`/usr/local/bin/refresh_table_idtax.sh`):
```bash
#!/bin/bash
# Refresh table_idtax materialized view

PGHOST="your-host"
PGDATABASE="plots_transects"
PGUSER="data_manager_user"

psql -h $PGHOST -U $PGUSER -d $PGDATABASE -c "SELECT refresh_table_idtax();" \
    >> /var/log/table_idtax_refresh.log 2>&1

# Optional: Send notification email on failure
if [ $? -ne 0 ]; then
    echo "table_idtax refresh failed on $(date)" | mail -s "DB Alert" admin@example.com
fi
```

**Add to crontab**:
```bash
sudo crontab -e

# Add line (runs first day of month at 2 AM):
0 2 1 * * /usr/local/bin/refresh_table_idtax.sh
```

---

### Phase 6: User Training & Documentation (Week 4)

#### Step 6.1: User Documentation
- [ ] Create user guide: "How to Refresh Taxonomy Mappings"
- [ ] Document warning messages and what they mean
- [ ] Create troubleshooting guide
- [ ] Add FAQ section

**User Guide Outline:**
```markdown
# Refreshing Taxonomy Mappings

## When to Refresh
- You see a warning about stale data (>90 days old)
- After taxa database has been updated with new synonyms
- Before major analysis to ensure current taxonomy

## How to Refresh (R)
library(CafriplotsR)
con <- call.mydb()
update_taxa_link_table(con)

## How to Check Staleness
check_table_idtax_staleness(con)

## Permissions
- All users: Can check staleness and query data
- Data managers: Can refresh the view
- Contact admin if you need refresh permission
```

#### Step 6.2: Training Session (1 hour)
- [ ] Schedule online training for data managers
- [ ] Demonstrate new workflow
- [ ] Answer questions
- [ ] Provide written guide

#### Step 6.3: Support Period (1 month)
- [ ] Monitor for issues
- [ ] Be available for questions
- [ ] Collect feedback for improvements
- [ ] Document any additional edge cases

---

## Monitoring & Maintenance

### Daily (Automatic)
- [ ] Staleness warnings appear in R functions (automatic)
- [ ] Users see age when querying specimens

### Weekly (Manual Check)
- [ ] Check metadata: `SELECT * FROM table_idtax_metadata;`
- [ ] Review any refresh errors
- [ ] Verify cron job is running (if enabled)

### Monthly (After Refresh)
- [ ] Verify automatic refresh completed
- [ ] Check record count hasn't dropped unexpectedly
- [ ] Review PostgreSQL logs for any errors

**Monitoring Query:**
```sql
-- Quick health check
SELECT
    m.table_name,
    m.last_updated,
    EXTRACT(EPOCH FROM (CURRENT_TIMESTAMP - m.last_updated))/86400 as days_old,
    m.record_count,
    m.updated_by
FROM table_idtax_metadata m
WHERE m.table_name = 'table_idtax';
```

---

## Rollback Plan

If migration causes issues, follow these steps to revert:

### Step 1: Drop Materialized View (5 min)
```sql
-- As admin
DROP MATERIALIZED VIEW IF EXISTS table_idtax CASCADE;
DROP TABLE IF EXISTS table_idtax_metadata;
DROP FUNCTION IF EXISTS refresh_table_idtax();
DROP FUNCTION IF EXISTS check_table_idtax_staleness(INTEGER);
```

### Step 2: Restore from Backup (10 min)
```sql
-- Recreate as regular table
CREATE TABLE table_idtax AS
SELECT * FROM table_idtax_backup;

-- Create index
CREATE INDEX idx_table_idtax_idtax_n ON table_idtax(idtax_n);
CREATE INDEX idx_table_idtax_good_n ON table_idtax(idtax_good_n);

-- Grant permissions
GRANT SELECT ON table_idtax TO public;

-- Verify
SELECT COUNT(*) FROM table_idtax;
```

### Step 3: Revert R Package (15 min)
```bash
# In git
git revert <migration-commit-hash>
git push origin master

# Users reinstall
Rscript -e "devtools::install_github('umr-amap/cafriplotsR')"
```

### Step 4: Notify Users (5 min)
Send email explaining rollback and that old workflow is restored.

---

## Success Criteria

Migration is considered successful when:

- [ ] ✅ Materialized view exists and is populated
- [ ] ✅ Data managers can refresh without admin access
- [ ] ✅ All R package functions work correctly
- [ ] ✅ Staleness warnings appear appropriately
- [ ] ✅ Queries have same or better performance
- [ ] ✅ No data loss (record count matches)
- [ ] ✅ Automatic monthly refresh works (if enabled)
- [ ] ✅ No user-reported issues for 2 weeks post-migration

---

## Timeline Summary

| Phase | Duration | When |
|-------|----------|------|
| Phase 1: Pre-Migration | 1-2 hours | Week 1 |
| Phase 2: Testing | 3-4 hours | Week 1-2 |
| Phase 3: Production Migration | 1 hour | Week 3 (Saturday) |
| Phase 4: R Package Update | 3 hours | Week 3 |
| Phase 5: Auto-Refresh Setup | 1-2 hours | Week 4 (optional) |
| Phase 6: User Training | 2 hours | Week 4 |

**Total Time**: 10-14 hours over 4 weeks

---

## Risk Assessment

### Low Risk ✅
- **Data Loss**: Backup created before migration
- **Query Breakage**: Queries use same table name, transparent change
- **Permission Issues**: Can be fixed with GRANT statements

### Medium Risk ⚠️
- **R Package Compatibility**: Some old package versions may need update
- **User Confusion**: Mitigated by training and documentation

### High Risk ❌
- **None identified**: Full rollback plan available

---

## Contacts & Resources

### Key People
- **Database Admin**: [Name, Email, Phone]
- **R Package Maintainer**: Gilles Dauby, gilles.dauby@ird.fr
- **Backup Admin**: [Name, Email, Phone]

### Documentation
- PostgreSQL Materialized Views: https://www.postgresql.org/docs/current/sql-creatematerializedview.html
- pg_cron Extension: https://github.com/citusdata/pg_cron

### Files
- Migration SQL: `inst/sql/migration_table_idtax_to_materialized_view.sql`
- Updated R Functions: `R/taxonomic_update_functions_new.R`
- This Plan: `inst/docs/PLAN_MATERIALIZED_VIEW_MIGRATION.md`

---

## Post-Migration Notes

_Section to be filled in after migration_

### What Went Well


### Issues Encountered


### Lessons Learned


### Recommendations for Future


---

**Plan Version**: 1.0
**Last Updated**: 2026-01-12
**Status**: Ready for review and approval
