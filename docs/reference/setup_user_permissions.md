# Setup user permissions on both databases

One-command setup to grant a user appropriate permissions on the main
database (\`plots_transects\`) and/or the taxa database (\`rainbio\`).
Also registers the user in the \`user_registry\` table.

The user must already exist as a PostgreSQL role (created via OVH
portal).

\*\*For database administrators only.\*\*

## Usage

``` r
setup_user_permissions(
  con_main,
  con_taxa = NULL,
  username,
  email = NULL,
  first_name = NULL,
  last_name = NULL,
  institution = NULL,
  main_db_access = c("read_write", "read_only", "none"),
  taxa_db_access = c("read_only", "read_write", "none"),
  plot_ids = NULL,
  notes = NULL
)
```

## Arguments

- con_main:

  Connection to main database.

- con_taxa:

  Connection to taxa database (optional).

- username:

  Character. PostgreSQL username.

- email:

  Character. User's email address (optional but recommended).

- first_name:

  Character. User's first name (optional).

- last_name:

  Character. User's last name (optional).

- institution:

  Character. User's institution (optional).

- main_db_access:

  Character. Access level for plots_transects: \`"read_only"\`,
  \`"read_write"\`, or \`"none"\`. Default \`"read_write"\`.

- taxa_db_access:

  Character. Access level for rainbio: \`"read_only"\`,
  \`"read_write"\`, or \`"none"\`. Default \`"read_only"\`.

- plot_ids:

  Integer vector. Plot IDs to grant access to (optional). If provided,
  RLS policies will be set for these plots.

- notes:

  Character. Additional notes about the user (optional).

## Value

A list with setup results.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
con_taxa <- call.mydb.taxa()

# Full setup for a new user
setup_user_permissions(
  con_main = con,
  con_taxa = con_taxa,
  username = "jdupont",
  email = "j.dupont@institution.org",
  first_name = "Jean",
  last_name = "Dupont",
  institution = "IRD",
  main_db_access = "read_write",
  taxa_db_access = "read_only",
  plot_ids = c(10, 15, 22)
)
} # }
```
