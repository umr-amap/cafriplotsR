# Check if secure add_person function exists

Checks whether the add_person() PostgreSQL function has been created by
a database administrator.

## Usage

``` r
check_add_person_function_exists(con)
```

## Arguments

- con:

  Database connection

## Value

Logical. TRUE if function exists, FALSE otherwise
