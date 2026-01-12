# Quick Reference: table_idtax Materialized View

## For Data Managers

### Check if refresh is needed
```r
library(CafriplotsR)
con <- call.mydb()

# Check staleness (default threshold: 90 days)
check_table_idtax_staleness(con)
```

**Output:**
```
✔ table_idtax is up to date (45.3 days old)
# OR
! WARNING: table_idtax is 120.5 days old (threshold: 90 days)
ℹ Run update_taxa_link_table() to refresh.
```

### Refresh taxonomy mappings
```r
# Simple refresh
update_taxa_link_table(con)

# Force refresh even if recently updated
update_taxa_link_table(con, force = TRUE)
```

**Output:**
```
ℹ Attempting to refresh table_idtax materialized view...
✔ Successfully refreshed table_idtax with 45,231 records
ℹ Refresh completed in 2.34 seconds
```

### View metadata
```r
get_table_idtax_metadata(con)
```

**Output:**
```
# A tibble: 1 × 6
  table_name   last_updated        updated_by record_count source_info  notes
  <chr>        <dttm>              <chr>             <int> <chr>        <chr>
1 table_idtax  2026-01-12 02:00:15 dauby             45231 Refreshed…   NA
```

---

## For Regular Users

### Automatic warnings
When you query data, you'll automatically see warnings if taxonomy data is stale:

```r
specimens <- query_specimens(collector = "Dauby")
```

**Output with warning:**
```
! WARNING: table_idtax is 120.5 days old (threshold: 90 days)
ℹ Consider asking a data manager to refresh
```

### Manual check
```r
con <- call.mydb()
check_table_idtax_staleness(con)
```

---

## SQL Direct Access (PostgreSQL)

### Check staleness
```sql
SELECT * FROM check_table_idtax_staleness(90);
```

### Refresh (requires data_manager_role)
```sql
SELECT * FROM refresh_table_idtax();
```

### View metadata
```sql
SELECT * FROM table_idtax_metadata WHERE table_name = 'table_idtax';
```

### Check permissions
```sql
-- Check if you have data_manager_role
SELECT rolname FROM pg_roles WHERE oid IN (
    SELECT member FROM pg_auth_members WHERE roleid = (
        SELECT oid FROM pg_roles WHERE rolname = 'data_manager_role'
    )
);
```

---

## Troubleshooting

### Error: "permission denied for materialized view"
**Problem**: You don't have REFRESH permission
**Solution**: Contact database admin to grant you `data_manager_role`

### Error: "function refresh_table_idtax() does not exist"
**Problem**: Migration not completed yet
**Solution**: Database admin needs to run migration script

### Warning: "table_idtax is X days old"
**Problem**: Data is stale
**Solution**:
- If you're a data manager: Run `update_taxa_link_table()`
- If you're a regular user: Ask a data manager to refresh

### No warning but I think data is wrong
**Problem**: Specific taxon may have been updated in taxa DB
**Solution**: Check when last refresh was: `get_table_idtax_metadata()`

---

## Scheduled Maintenance

### Automatic Monthly Refresh
If pg_cron is enabled, the view automatically refreshes on the 1st of each month at 2:00 AM.

### Check if automatic refresh is working
```sql
-- Check scheduled job
SELECT * FROM cron.job WHERE jobname = 'refresh-table-idtax';

-- Check recent runs
SELECT * FROM cron.job_run_details
WHERE jobid = (SELECT jobid FROM cron.job WHERE jobname = 'refresh-table-idtax')
ORDER BY start_time DESC
LIMIT 5;
```

---

## Contact

**For permission requests**: [Database Admin Email]
**For R package issues**: gilles.dauby@ird.fr
**For database issues**: [DBA Email]
