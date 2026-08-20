# Database Login Module - UI

UI component for database authentication with language selection

## Usage

``` r
mod_database_login_ui(id, allow_public = FALSE)
```

## Arguments

- id:

  Module namespace ID

- allow_public:

  Logical. Show the "Connect as public user" button? Defaults to
  \`FALSE\`. The public account is read-only, so only apps that are
  useful without write access should opt in (taxonomic matching,
  backbone browsing, plot querying).

## Value

A shiny tagList
