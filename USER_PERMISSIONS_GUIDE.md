# User Permissions Guide

## Problem: "Permission Denied" When Adding People

When users try to add new people in the import wizard, they may encounter:
```
Error: permission denied for table table_colnam
```

This happens because they don't have INSERT permissions on the `table_colnam` table.

## Solution: Secure Function Approach (Recommended)

### For Database Administrators (One-Time Setup):

Run this **once** to enable all users to add people:

```r
library(CafriplotsR)

# Connect with admin credentials
con <- call.mydb()

# Create the secure function (SECURITY DEFINER)
setup_add_person_function(con)
```

**What this does:**
- Creates a PostgreSQL function `add_person()` that runs with admin privileges
- Grants EXECUTE permission to all users (PUBLIC)
- Validates inputs and prevents duplicates
- No direct INSERT permission needed for users

### For Regular Users:

After admin setup, adding people works automatically in the import wizard!

Or manually in R:

```r
# Add a new person
id <- add_person_to_db(
  con = con,
  first_name = "John",
  last_name = "Doe",
  nationality = "USA",
  institute = "University XYZ",
  contact = "john.doe@example.com"
)
```

## Alternative: Direct INSERT Permissions

If you prefer to grant direct table permissions:

```r
# As database administrator:
con <- call.mydb()

grant_lookup_table_permissions(
  con = con,
  user = "username",
  tables = "table_colnam",
  operations = c("SELECT", "INSERT", "UPDATE")
)
```

## How the Import Wizard Works

The import wizard automatically:

1. **Checks** if secure `add_person()` function exists
2. **Uses** secure function if available (no permissions needed)
3. **Falls back** to direct INSERT if user has permissions
4. **Shows helpful error** if neither option works

## Technical Details

### Security Model

The `add_person()` function uses PostgreSQL's `SECURITY DEFINER`:
- Function executes with **creator's privileges** (admin)
- Users can **execute** the function without table-level INSERT permission
- Input validation prevents SQL injection
- Duplicate checking prevents data issues

### Function Signature

```sql
add_person(
  p_first_name TEXT,
  p_last_name TEXT,
  p_nationality TEXT DEFAULT NULL,
  p_institute TEXT DEFAULT NULL,
  p_contact TEXT DEFAULT NULL
) RETURNS INTEGER
```

**Returns**: The `id_table_colnam` of the new (or existing) person

### Checking if Function Exists

```r
# Check if secure function is available
con <- call.mydb()
check_add_person_function_exists(con)
```

## Troubleshooting

### Error: "function add_person does not exist"

**Solution**: Database admin needs to run `setup_add_person_function(con)`

### Error: "permission denied for function add_person"

**Cause**: EXECUTE permission not granted (rare)

**Solution**:
```sql
GRANT EXECUTE ON FUNCTION add_person(TEXT, TEXT, TEXT, TEXT, TEXT) TO PUBLIC;
```

### Error: "must be owner of function add_person"

**Cause**: Trying to create/modify function without admin privileges

**Solution**: Contact your database administrator

## Best Practices

1. **Use secure function approach** - Benefits all users, more secure
2. **Validate data before adding** - The function does basic validation but check your data
3. **Handle duplicates** - Function returns existing ID if person already exists
4. **Log changes** - Consider adding audit logging for production databases

## See Also

- `?setup_add_person_function` - Detailed function documentation
- `?add_person_to_db` - Manual person addition
- `?grant_lookup_table_permissions` - Alternative permission approach
- `?define_user_policy` - Row-level security for plot data
