# Create a connection pool for Shiny apps (main database)

Creates a connection pool using the pool package. This is the
recommended approach for Shiny applications as it automatically manages
connections, preventing connection leaks and exhaustion.

## Usage

``` r
create_pool_main(
  pass = NULL,
  user = NULL,
  minSize = 1,
  maxSize = 5,
  use_env_credentials = TRUE
)
```

## Arguments

- pass:

  Character, database password (optional)

- user:

  Character, database username (optional)

- minSize:

  Integer, minimum number of connections in pool (default: 1)

- maxSize:

  Integer, maximum number of connections in pool (default: 5)

- use_env_credentials:

  Logical, use credentials from .Renviron

## Value

A pool object

## Examples

``` r
if (FALSE) { # \dontrun{
# In your Shiny app server function:
pool_main <- create_pool_main()
onStop(function() {
  pool::poolClose(pool_main)
})

# Use the pool in queries:
data <- pool::poolWithTransaction(pool_main, function(conn) {
  DBI::dbGetQuery(conn, "SELECT * FROM table_name")
})
} # }
```
