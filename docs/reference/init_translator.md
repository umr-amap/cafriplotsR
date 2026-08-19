# Initialize Translator for Shiny App

Creates a shiny.i18n Translator object loaded with translations from the
JSON file. This should be called once at app initialization.

## Usage

``` r
init_translator()
```

## Value

shiny.i18n::Translator object

## Details

The translator reads from \`inst/translations/translation.json\` which
contains translations for English (en) and French (fr).

## Examples

``` r
if (FALSE) { # \dontrun{
translator <- init_translator()
translator$set_translation_language("fr")
translator$t("app_title")
} # }
```
