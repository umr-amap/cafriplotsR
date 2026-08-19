# Create a connection pool for Shiny apps (taxa database)

Creates a connection pool for the taxa database. This is the recommended
approach for Shiny applications.

## Usage

``` r
create_pool_taxa(
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
pool_taxa <- create_pool_taxa()
onStop(function() {
  pool::poolClose(pool_taxa)
})
} # }
```
