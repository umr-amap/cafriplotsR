# Create Reactive Translator

Creates a reactive expression that returns the translator after setting
the language. This is the recommended pattern for using shiny.i18n with
reactive language switching.

## Usage

``` r
create_reactive_translator(translator, language_reactive)
```

## Arguments

- translator:

  shiny.i18n::Translator object (from init_translator())

- language_reactive:

  Reactive expression returning current language code ("en" or "fr")

## Value

Reactive expression returning the translator
