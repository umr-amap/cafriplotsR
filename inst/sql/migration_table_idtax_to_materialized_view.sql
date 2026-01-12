-- ============================================================================
-- Migration: Convert table_idtax to Materialized View
-- ============================================================================
--
-- Purpose: Convert the regular table_idtax table to a PostgreSQL materialized
--          view to allow designated users to refresh taxonomy mappings without
--          requiring full admin permissions.
--
-- Author: Database Admin
-- Date: 2026-01-12
--
-- IMPORTANT: Run this script as database admin/superuser
-- ============================================================================

-- Step 1: Backup existing table_idtax
-- ============================================================================
CREATE TABLE IF NOT EXISTS table_idtax_backup AS
SELECT * FROM table_idtax;

-- Verify backup
DO $$
DECLARE
    backup_count INTEGER;
    original_count INTEGER;
BEGIN
    SELECT COUNT(*) INTO backup_count FROM table_idtax_backup;
    SELECT COUNT(*) INTO original_count FROM table_idtax;

    RAISE NOTICE 'Backup created: % rows', backup_count;

    IF backup_count != original_count THEN
        RAISE EXCEPTION 'Backup verification failed: % rows backed up vs % original rows',
                        backup_count, original_count;
    END IF;
END $$;


-- Step 2: Drop existing table_idtax
-- ============================================================================
-- WARNING: This will drop the existing table. Ensure backup is complete!
DROP TABLE IF EXISTS table_idtax CASCADE;


-- Step 3: Create materialized view
-- ============================================================================
-- Note: Adjust the taxa database schema name if different (currently assumes 'public')
-- You may need to use: rainbio.public.table_taxa or specify the full connection

-- Option A: If table_taxa is in a different database accessible via foreign data wrapper
-- CREATE MATERIALIZED VIEW table_idtax AS
-- SELECT idtax_n, idtax_good_n
-- FROM foreign_taxa_db.table_taxa;

-- Option B: If table_taxa is in the same database but different schema
-- CREATE MATERIALIZED VIEW table_idtax AS
-- SELECT idtax_n, idtax_good_n
-- FROM rainbio_schema.table_taxa;

-- Option C: Standard approach (you'll populate this from R initially)
CREATE MATERIALIZED VIEW table_idtax AS
SELECT
    idtax_n::INTEGER,
    idtax_good_n::INTEGER
FROM table_idtax_backup;  -- Initial data from backup

-- Add comment
COMMENT ON MATERIALIZED VIEW table_idtax IS
'Materialized view containing taxonomy ID mappings for synonym resolution.
Refresh using: REFRESH MATERIALIZED VIEW table_idtax;
Last updated: check table_idtax_metadata';


-- Step 4: Create unique index (required for CONCURRENTLY refresh)
-- ============================================================================
CREATE UNIQUE INDEX idx_table_idtax_idtax_n ON table_idtax(idtax_n);

-- Additional index for performance
CREATE INDEX idx_table_idtax_good_n ON table_idtax(idtax_good_n);


-- Step 5: Create metadata table for tracking updates
-- ============================================================================
CREATE TABLE IF NOT EXISTS table_idtax_metadata (
    table_name VARCHAR(100) PRIMARY KEY,
    last_updated TIMESTAMP WITH TIME ZONE NOT NULL,
    updated_by VARCHAR(100),
    record_count INTEGER,
    source_info TEXT,
    notes TEXT
);

COMMENT ON TABLE table_idtax_metadata IS
'Tracks when table_idtax materialized view was last refreshed';

-- Insert initial metadata
INSERT INTO table_idtax_metadata
    (table_name, last_updated, updated_by, record_count, source_info)
VALUES
    ('table_idtax', CURRENT_TIMESTAMP, CURRENT_USER,
     (SELECT COUNT(*) FROM table_idtax),
     'Initial migration from table to materialized view')
ON CONFLICT (table_name) DO UPDATE
SET
    last_updated = CURRENT_TIMESTAMP,
    updated_by = CURRENT_USER,
    record_count = (SELECT COUNT(*) FROM table_idtax),
    source_info = 'Initial migration from table to materialized view';


-- Step 6: Create role for data managers (if not exists)
-- ============================================================================
DO $$
BEGIN
    IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'data_manager_role') THEN
        CREATE ROLE data_manager_role;
        RAISE NOTICE 'Created role: data_manager_role';
    ELSE
        RAISE NOTICE 'Role data_manager_role already exists';
    END IF;
END $$;


-- Step 7: Grant permissions
-- ============================================================================
-- Grant SELECT to all users
GRANT SELECT ON table_idtax TO public;

-- Grant REFRESH permission to data manager role
-- Note: In PostgreSQL, there's no direct "REFRESH" privilege, but users need
-- to be able to execute REFRESH MATERIALIZED VIEW command
GRANT SELECT ON table_idtax TO data_manager_role;

-- Grant access to metadata table
GRANT SELECT ON table_idtax_metadata TO public;
GRANT INSERT, UPDATE ON table_idtax_metadata TO data_manager_role;

-- To allow refresh, we need to grant ownership or create a function with SECURITY DEFINER
-- We'll create a function approach (safer)


-- Step 8: Create refresh function with security definer
-- ============================================================================
CREATE OR REPLACE FUNCTION refresh_table_idtax()
RETURNS TABLE(
    success BOOLEAN,
    message TEXT,
    record_count INTEGER,
    refresh_duration INTERVAL
)
SECURITY DEFINER
LANGUAGE plpgsql
AS $$
DECLARE
    start_time TIMESTAMP;
    end_time TIMESTAMP;
    row_count INTEGER;
BEGIN
    -- Record start time
    start_time := clock_timestamp();

    -- Perform the refresh (CONCURRENTLY to avoid locking)
    REFRESH MATERIALIZED VIEW CONCURRENTLY table_idtax;

    -- Record end time
    end_time := clock_timestamp();

    -- Get row count
    SELECT COUNT(*) INTO row_count FROM table_idtax;

    -- Update metadata
    INSERT INTO table_idtax_metadata
        (table_name, last_updated, updated_by, record_count, source_info)
    VALUES
        ('table_idtax', CURRENT_TIMESTAMP, CURRENT_USER, row_count,
         'Refreshed via refresh_table_idtax() function')
    ON CONFLICT (table_name) DO UPDATE
    SET
        last_updated = CURRENT_TIMESTAMP,
        updated_by = CURRENT_USER,
        record_count = row_count,
        source_info = 'Refreshed via refresh_table_idtax() function';

    -- Return success info
    RETURN QUERY SELECT
        TRUE::BOOLEAN as success,
        format('Successfully refreshed table_idtax with %s records', row_count)::TEXT as message,
        row_count as record_count,
        (end_time - start_time) as refresh_duration;

EXCEPTION WHEN OTHERS THEN
    -- Return error info
    RETURN QUERY SELECT
        FALSE::BOOLEAN as success,
        format('Error refreshing table_idtax: %s', SQLERRM)::TEXT as message,
        NULL::INTEGER as record_count,
        NULL::INTERVAL as refresh_duration;
END;
$$;

COMMENT ON FUNCTION refresh_table_idtax() IS
'Refreshes the table_idtax materialized view and updates metadata.
Can be called by users granted EXECUTE permission.';

-- Grant execute permission to data manager role
GRANT EXECUTE ON FUNCTION refresh_table_idtax() TO data_manager_role;


-- Step 9: Create function to check staleness
-- ============================================================================
CREATE OR REPLACE FUNCTION check_table_idtax_staleness(warn_days INTEGER DEFAULT 90)
RETURNS TABLE(
    is_stale BOOLEAN,
    days_old NUMERIC,
    last_updated TIMESTAMP WITH TIME ZONE,
    message TEXT
)
LANGUAGE plpgsql
AS $$
DECLARE
    meta_record RECORD;
    age_days NUMERIC;
BEGIN
    -- Get metadata
    SELECT * INTO meta_record
    FROM table_idtax_metadata
    WHERE table_name = 'table_idtax';

    IF NOT FOUND THEN
        RETURN QUERY SELECT
            TRUE::BOOLEAN,
            NULL::NUMERIC,
            NULL::TIMESTAMP WITH TIME ZONE,
            'No metadata found for table_idtax'::TEXT;
        RETURN;
    END IF;

    -- Calculate age in days
    age_days := EXTRACT(EPOCH FROM (CURRENT_TIMESTAMP - meta_record.last_updated)) / 86400;

    -- Return staleness info
    RETURN QUERY SELECT
        (age_days > warn_days)::BOOLEAN,
        ROUND(age_days, 1),
        meta_record.last_updated,
        CASE
            WHEN age_days > warn_days THEN
                format('WARNING: table_idtax is %.1f days old (threshold: %s days). Consider refreshing.',
                       age_days, warn_days)
            ELSE
                format('table_idtax is up to date (%.1f days old)', age_days)
        END::TEXT;
END;
$$;

COMMENT ON FUNCTION check_table_idtax_staleness(INTEGER) IS
'Checks if table_idtax needs refreshing based on age threshold (default 90 days)';

-- Grant execute to everyone
GRANT EXECUTE ON FUNCTION check_table_idtax_staleness(INTEGER) TO public;


-- Step 10: Add specific users to data_manager_role
-- ============================================================================
-- Replace these with actual usernames
-- GRANT data_manager_role TO gilles;
-- GRANT data_manager_role TO hugo;
-- GRANT data_manager_role TO your_username;

-- Example (uncomment and modify):
-- GRANT data_manager_role TO dauby;

RAISE NOTICE 'IMPORTANT: Grant data_manager_role to specific users by running:';
RAISE NOTICE 'GRANT data_manager_role TO username;';


-- Step 11: Set up automatic refresh (optional - requires pg_cron extension)
-- ============================================================================
-- Check if pg_cron is installed
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
        -- Schedule monthly refresh (first day of month at 2 AM)
        PERFORM cron.schedule(
            'refresh-table-idtax',
            '0 2 1 * *',  -- Cron: minute hour day month weekday
            'SELECT refresh_table_idtax();'
        );
        RAISE NOTICE 'Scheduled automatic monthly refresh at 2 AM on first day of month';
    ELSE
        RAISE NOTICE 'pg_cron extension not installed. Automatic refresh not scheduled.';
        RAISE NOTICE 'To enable: CREATE EXTENSION pg_cron;';
    END IF;
END $$;


-- ============================================================================
-- Verification queries
-- ============================================================================
-- Run these to verify the migration was successful

-- Check materialized view exists
SELECT
    schemaname,
    matviewname,
    matviewowner,
    ispopulated
FROM pg_matviews
WHERE matviewname = 'table_idtax';

-- Check record count
SELECT COUNT(*) as record_count FROM table_idtax;

-- Check metadata
SELECT * FROM table_idtax_metadata WHERE table_name = 'table_idtax';

-- Check staleness
SELECT * FROM check_table_idtax_staleness(90);

-- Check permissions on refresh function
SELECT
    routine_name,
    grantee,
    privilege_type
FROM information_schema.routine_privileges
WHERE routine_name = 'refresh_table_idtax';

-- List users in data_manager_role
SELECT
    r.rolname as role,
    m.rolname as member
FROM pg_roles r
JOIN pg_auth_members am ON r.oid = am.roleid
JOIN pg_roles m ON am.member = m.oid
WHERE r.rolname = 'data_manager_role';


-- ============================================================================
-- Rollback plan (if needed)
-- ============================================================================
-- If something goes wrong, you can rollback by:
-- 1. DROP MATERIALIZED VIEW table_idtax;
-- 2. Recreate regular table from backup:
--    CREATE TABLE table_idtax AS SELECT * FROM table_idtax_backup;
-- 3. CREATE INDEX idx_table_idtax_idtax_n ON table_idtax(idtax_n);
