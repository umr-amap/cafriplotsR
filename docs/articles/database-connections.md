# Database Connections Guide

``` r

library(CafriplotsR)
```

## Introduction

This guide explains how to connect to the CafriplotsR databases, manage
credentials securely, and troubleshoot common connection issues.

CafriplotsR uses **two separate PostgreSQL databases**: 1. **Main
database** (`plots_transects`): Contains plot, subplot, and individual
tree data 2. **Taxa database** (`rainbio`): Contains taxonomic
information and species-level traits

## Quick Start

### First-time Connection

The recommended way to start a session is
[`connect_cafri()`](https://umr-amap.github.io/cafriplotsR/reference/connect_cafri.md).
It opens both databases from a single credential prompt (the same
username and password are valid for both):

``` r

library(CafriplotsR)

# Connect to both databases with a single credential prompt
cons <- connect_cafri()

cons$main   # main database connection
cons$taxa   # taxa database connection

# Test your connections
print_connection_status()
```

On first use, you’ll be prompted to enter:

- **Username**: Your database username
- **Password**: Your database password

Credentials are cached in memory for the duration of your R session, so
any subsequent call to
[`call.mydb()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md)
or
[`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md)
reuses the same connections silently.

## Connection Functions

### Recommended: `connect_cafri()`

[`connect_cafri()`](https://umr-amap.github.io/cafriplotsR/reference/connect_cafri.md)
is a single entry point that opens both databases at once and stashes
the connections internally, so functions that look up a connection
through
[`call.mydb()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md)
/
[`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md)
later in the session find them without re-prompting.

``` r

# Both databases in one prompt
cons <- connect_cafri()

# Only the main database (taxa is opened lazily when a query needs it,
# still reusing the cached credentials — no second prompt)
cons <- connect_cafri(taxa = FALSE)

# Force new credentials (e.g. to switch users)
cons <- connect_cafri(reset = TRUE)

# Always prompt, even if MYDB_USER/MYDB_PASS are set in .Renviron
cons <- connect_cafri(use_env_credentials = FALSE)
```

If `MYDB_USER` and `MYDB_PASS` are present in your `.Renviron` (see
“Managing Credentials” below),
[`connect_cafri()`](https://umr-amap.github.io/cafriplotsR/reference/connect_cafri.md)
uses them automatically without prompting — no flag required.

### Individual functions: `call.mydb()` and `call.mydb.taxa()`

These are still available, mostly for scripts that only need one of the
two databases. Calling them twice in a row is *not* required —
[`connect_cafri()`](https://umr-amap.github.io/cafriplotsR/reference/connect_cafri.md)
covers that case in one line.

``` r

# Main database only
con <- call.mydb()

# Force new credentials (if you need to switch users)
con <- call.mydb(reset = TRUE)

# Provide credentials directly (not recommended for security)
con <- call.mydb(user = "myuser", pass = "mypassword")
```

``` r

# Taxa database only (uses cached credentials from the main connect if any)
con_taxa <- call.mydb.taxa()
```

**Important**: The taxa database is **read-only** for most users. Write
operations are restricted to administrators.

## Managing Credentials

### Option 1: Interactive Prompts (Default)

The simplest approach - enter credentials when prompted:

``` r

cons <- connect_cafri()
# Enter username: [your_username]
# Enter password: [your_password]
```

**Pros**: Secure, no stored passwords **Cons**: Must enter credentials
each new R session

### Option 2: Environment Variables (Persistent)

Store credentials in your `.Renviron` file for automatic loading on
every session:

``` r

# Run once to set up
setup_db_credentials()
# Follow the prompts to enter username and password
```

That’s it — from the next R session onwards,
[`connect_cafri()`](https://umr-amap.github.io/cafriplotsR/reference/connect_cafri.md)
(and
[`call.mydb()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md)
/
[`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md))
will pick up `MYDB_USER` and `MYDB_PASS` from `.Renviron` automatically,
with no flag to remember:

``` r

cons <- connect_cafri()   # no prompt; credentials read from .Renviron
```

If you want to override this for a single call and be prompted instead,
pass `use_env_credentials = FALSE`:

``` r

cons <- connect_cafri(use_env_credentials = FALSE)
```

The resolution order is: explicit `user`/`pass` arguments → cached
session credentials → `.Renviron` (`MYDB_USER` / `MYDB_PASS`) →
interactive prompt.

**Warning**: Credentials are stored in **plain text** in `~/.Renviron`.
Only use this on your personal, secure computer.

To remove stored credentials:

``` r

remove_db_credentials()
```

### Option 3: Direct Parameters (Not Recommended)

You can pass credentials directly, but this is **not recommended** as
passwords may be visible in your code or history:

``` r

# Avoid this in shared code!
cons <- connect_cafri(user = "myuser", pass = "mypassword")
```

## Cleaning Up Connections

### Why Clean Connections?

Database connections are limited resources. Failing to close them can
cause:

- **Connection exhaustion**: Database refuses new connections
- **Memory leaks**: Unused connections consume resources
- **Stale connections**: Old connections may timeout and cause errors

### When to Clean Up

Try to clean up connections when:

1.  You’re done working with the database
2.  Before closing R/RStudio
3.  When switching between users

### How to Clean Up

``` r

# Close all connections and clear cached credentials
cleanup_connections()
```

This function:

- Closes the main database connection
- Closes the taxa database connection
- Clears cached credentials from memory

## Checking Connection Status

### Quick Status Check

``` r

# See current connection status
print_connection_status()
```

Example output:

    -- Database Connections Status --
    v Main DB: Connected to plots_transects as myuser
    v Taxa DB: Connected to rainbio as myuser

### Full Diagnostic

For troubleshooting, run a complete diagnostic:

``` r

db_diagnostic()
```

This shows:

- Connection status for both databases
- Configuration details (host, port, database names)
- Connectivity test results
- PostgreSQL version information

## Checking Data Access (Row-Level Security)

The database uses **row-level security (RLS)** to control which plots
each user can access.

### View Your Accessible Plots

``` r

con <- connect_cafri()$main

# See which plots you can access
result <- get_user_accessible_plots(con, "your_username")
print(result)

# Get just the plot IDs as a vector
plot_ids <- result$plot_ids[[1]]
print(plot_ids)
```

### View Your Policies

``` r

# See all policies for a user
list_user_policies(con, user = "your_username")

# See all policies on a table
list_user_policies(con, user = NULL, table = "data_liste_plots")
```

### Understanding Access Levels

Policies can grant different operations:

- **SELECT**: Read-only access
- **INSERT**: Can add new records
- **UPDATE**: Can modify existing records
- **DELETE**: Can remove records
- **ALL**: Full access (SELECT, INSERT, UPDATE, DELETE)

## Troubleshooting

### Common Issues and Solutions

#### “Connection refused” or “Could not connect”

**Causes**:

- Network issues
- Database server is down
- Firewall blocking connection

**Solutions**:

1.  Check your internet connection
2.  Try again in a few minutes
3.  Contact database administrator

#### “Authentication failed”

**Causes**:

- Wrong username or password
- Account doesn’t exist

**Solutions**:

``` r

# Reset credentials and try again
cons <- connect_cafri(reset = TRUE)
```

#### “SSL SYSCALL error: EOF detected”

**Causes**:

- Connection was closed but still being used
- Typically happens after closing a Shiny app

**Solutions**:

``` r

# Clean up and reconnect
cleanup_connections()
cons <- connect_cafri()
```

#### “Too many connections”

**Causes**:

- Multiple unclosed connections
- Other users consuming connections

**Solutions**:

``` r

# Clean up your connections
cleanup_connections()

# Wait and try again
Sys.sleep(5)
cons <- connect_cafri()
```

#### Query timeout or “Lost connection”

**Causes**:

- Very large query
- Network instability
- Server overload

**Solutions**:

The package includes automatic retry for transient failures:

``` r

# func_try_fetch automatically retries failed queries
result <- func_try_fetch(con, "SELECT * FROM large_table")
```

#### “Permission denied” or empty results

**Causes**:

- Row-level security restricting access
- User doesn’t have policy for requested plots

**Solutions**:

1.  Check your accessible plots:

``` r

get_user_accessible_plots(con, "your_username")
```

2.  Contact administrator to request access to additional plots

### Reset Everything

If you’re having persistent issues, do a complete reset:

``` r

# 1. Clean up all connections
cleanup_connections()

# 2. Restart R session
.rs.restartR()

# 3. Reconnect fresh
library(CafriplotsR)
cons <- connect_cafri(reset = TRUE)
```

## Best Practices Summary

### Do’s

- **Start sessions with
  [`connect_cafri()`](https://umr-amap.github.io/cafriplotsR/reference/connect_cafri.md)**
  rather than two separate calls
- **Always call
  [`cleanup_connections()`](https://umr-amap.github.io/cafriplotsR/reference/cleanup_connections.md)**
  before closing R
- **Use
  [`print_connection_status()`](https://umr-amap.github.io/cafriplotsR/reference/print_connection_status.md)**
  to verify connections
- **Check your plot access** with
  [`get_user_accessible_plots()`](https://umr-amap.github.io/cafriplotsR/reference/get_user_accessible_plots.md)
  if queries return empty
- **Use connection pools** in Shiny apps
- **Run
  [`db_diagnostic()`](https://umr-amap.github.io/cafriplotsR/reference/db_diagnostic.md)**
  when troubleshooting
- **Store credentials in `.Renviron`** only on personal secure computers

### Don’ts

- **Don’t hardcode passwords** in scripts
- **Don’t share credentials** with others
- **Don’t leave connections open** indefinitely
- **Don’t ignore SSL errors** - clean up and reconnect
- **Don’t create multiple connections** when one will do

### Recommended Workflow

``` r

library(CafriplotsR)

# 1. Connect to both databases in one step
cons <- connect_cafri()

# 2. Verify connection
print_connection_status()

# 3. Check your access if needed
get_user_accessible_plots(cons$main, "your_username")

# 4. Do your work
# ... queries, updates, etc. ...

# 5. Clean up when done
cleanup_connections()
```

## Getting Help

If you encounter issues not covered here:

1.  Run
    [`db_diagnostic()`](https://umr-amap.github.io/cafriplotsR/reference/db_diagnostic.md)
    and note the output
2.  Check your accessible plots with
    [`get_user_accessible_plots()`](https://umr-amap.github.io/cafriplotsR/reference/get_user_accessible_plots.md)
3.  Contact the database administrator with:
    - The diagnostic output
    - The exact error message
    - What you were trying to do
