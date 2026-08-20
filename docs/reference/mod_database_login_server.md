# Database Login Module - Server

Server logic for database authentication with language selection

## Usage

``` r
mod_database_login_server(id, allow_public = FALSE)
```

## Arguments

- id:

  Module namespace ID

- allow_public:

  Logical. Allow connecting through the read-only public account?
  Defaults to \`FALSE\`. Must match the value given to
  \[mod_database_login_ui()\].

## Value

A reactive list containing: - authenticated: Reactive logical indicating
connection status - pool_main: Main database connection pool (NULL if
not connected) - pool_taxa: Taxa database connection pool (NULL if not
connected) - language: Reactive string returning selected language ("en"
or "fr")
