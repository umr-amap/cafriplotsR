# Get primary database connection (wrapper)

Returns a database connection. In Shiny apps with connection pools, this
function automatically uses the pool if available in .db_env.

## Usage

``` r
call.mydb(
  pass = NULL,
  user = NULL,
  reset = FALSE,
  retry = TRUE,
  use_env_credentials = TRUE
)
```
