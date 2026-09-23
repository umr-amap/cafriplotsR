# Get the Tropicos API key

Resolves the personal Tropicos API key used to query Tropicos through
taxize, in this order: the \`key\` argument, the key cached earlier in
this session, the \`TROPICOS_API_KEY\` environment variable (typically
set by \[setup_tropicos_key()\] or by \`.Renviron\`), and finally an
interactive prompt. A key found anywhere is cached for the rest of the
session.

The package ships no key: request one at
\<https://services.tropicos.org/help?requestkey\>.

## Usage

``` r
get_tropicos_key(key = NULL, prompt = interactive())
```

## Arguments

- key:

  Character. A key to use and cache. \`NULL\` (the default) to resolve
  one from the cache, the environment, or the user.

- prompt:

  Logical. Ask for the key if none was found. Defaults to
  \[interactive()\]; pass \`FALSE\` where a prompt would block, such as
  inside a Shiny app.

## Value

The key as a single string, or \`NULL\` if none is available.

## See also

\[setup_tropicos_key()\] to store the key permanently.

## Examples

``` r
if (FALSE) { # \dontrun{
key <- get_tropicos_key()
taxize::tp_search(sci = "Dacryodes edulis", key = key)
} # }
```
