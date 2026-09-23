# Store a Tropicos API key in \`.Renviron\`

Saves a Tropicos API key as \`TROPICOS_API_KEY\` in \`~/.Renviron\` so
that it is available in every future R session, and caches it for the
current one. The key is the personal credential obtained from
\<https://services.tropicos.org/help?requestkey\>; the package does not
ship one.

WARNING: the key is stored in plain text. Only use this on a personal,
secure computer.

## Usage

``` r
setup_tropicos_key(key = NULL)
```

## Arguments

- key:

  Character. The Tropicos API key. If \`NULL\` (the default), it is
  asked for interactively.

## Value

\`TRUE\` invisibly if the key was written, \`FALSE\` otherwise.

## See also

\[get_tropicos_key()\], \[remove_tropicos_key()\]

## Examples

``` r
if (FALSE) { # \dontrun{
setup_tropicos_key("your-tropicos-api-key")
} # }
```
