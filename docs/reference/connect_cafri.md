# Connect to both CafriplotsR databases in one step

Single entry point that opens connections to both the main
(\`plots_transects\`) and taxa (\`rainbio\`) databases using the same
set of credentials. This is the recommended way to start a session:
users only enter their username and password once, instead of going
through both \`call.mydb()\` and \`call.mydb.taxa()\`.

The returned connections are also stored internally so that package
functions calling \`call.mydb()\` or \`call.mydb.taxa()\` later in the
session reuse them transparently.

## Usage

``` r
connect_cafri(
  user = NULL,
  pass = NULL,
  reset = FALSE,
  retry = TRUE,
  use_env_credentials = TRUE,
  taxa = TRUE
)
```

## Arguments

- user:

  Username. If \`NULL\`, will check environment then prompt.

- pass:

  Password. If \`NULL\`, will check environment then prompt.

- reset:

  If \`TRUE\`, forces a new credential prompt and reopens both
  connections.

- retry:

  If \`TRUE\`, retry on transient failures.

- use_env_credentials:

  If \`TRUE\` (the default), \`MYDB_USER\` and \`MYDB_PASS\` from
  \`.Renviron\` are used when no cached or explicit credentials are
  available. Set to \`FALSE\` to always prompt instead (see
  \`setup_db_credentials()\` to persist credentials).

- taxa:

  If \`FALSE\`, only the main database is opened. The taxa connection is
  then created lazily by package functions that need it (still reusing
  the cached credentials, so no extra prompt).

## Value

Invisibly, a list with components: - \`main\`: connection to the main
database. - \`taxa\`: connection to the taxa database, or \`NULL\` if
\`taxa = FALSE\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# One prompt, both connections ready
cons <- connect_cafri()
cons$main
cons$taxa

# Open only the main database; taxa will be opened lazily when needed
cons <- connect_cafri(taxa = FALSE)
} # }
```
