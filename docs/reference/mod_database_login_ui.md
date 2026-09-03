# Database Login Module - UI

UI component for database authentication with language selection

## Usage

``` r
mod_database_login_ui(id, allow_public = FALSE, allow_offline = FALSE)
```

## Arguments

- id:

  Module namespace ID

- allow_public:

  Logical. Show the "Connect as public user" button? Defaults to
  \`FALSE\`. The public account is read-only, so only apps that are
  useful without write access should opt in (taxonomic matching,
  backbone browsing, plot querying).

- allow_offline:

  Logical. Show the "Use offline (cached backbone)" button? Defaults to
  \`FALSE\`. Offline mode leaves the app with no database connection at
  all and only the cached taxonomic backbone, so only the taxonomic
  matching app — the one workflow that can be finished from the cache
  alone — should opt in. The button is shown only when a cache also
  exists on disk.

## Value

A shiny tagList
