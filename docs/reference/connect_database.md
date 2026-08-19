# Connect to database

Generic function to connect to main or taxa database

## Usage

``` r
connect_database(
  db_type = c("main", "taxa"),
  pass = NULL,
  user = NULL,
  reset = FALSE,
  retry = TRUE,
  use_env_credentials = TRUE
)
```

## Arguments

- db_type:

  One of \`"main"\` or \`"taxa"\`.

- pass:

  Password. If \`NULL\`, falls back to the cached session credential,
  then to \`MYDB_PASS\` from \`.Renviron\`, then prompts.

- user:

  Username. If \`NULL\`, falls back to the cached session credential,
  then to \`MYDB_USER\` from \`.Renviron\`, then prompts.

- reset:

  If \`TRUE\`, forces new credential prompt.

- retry:

  If \`TRUE\`, retry on failure.

- use_env_credentials:

  If \`TRUE\` (the default), \`MYDB_USER\` and \`MYDB_PASS\` from
  \`.Renviron\` are used when no explicit or cached credentials are
  available. Set to \`FALSE\` to always prompt instead.

## Value

A database connection object.
