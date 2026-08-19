# Run Full Hierarchy Migration

Runs all three phases of the hierarchy migration in order: 1. Add
id_parent column 2. Create missing hierarchy entries 3. Link existing
entries

## Usage

``` r
run_hierarchy_migration(con = NULL, dry_run = TRUE, create_backup = TRUE)
```

## Arguments

- con:

  Database connection to taxa database

- dry_run:

  If TRUE, only show what would happen

- create_backup:

  If TRUE, create backup table first

## Value

List with results from each phase
