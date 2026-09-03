# Feature Wizard Step 2: Choose Mode - Server

Feature Wizard Step 2: Choose Mode - Server

## Usage

``` r
mod_feat_step2_choose_mode_server(id, i18n, reset = shiny::reactive(NULL))
```

## Arguments

- id:

  Module namespace ID

- i18n:

  Reactive returning translator object

- reset:

  Reactive whose change clears the chosen mode. The wizard clears its
  own copy of the mode whenever the plot selection changes, and this
  keeps the module from holding a stale one: a \`reactiveVal\` set to
  the value it already has notifies nobody, so without this, choosing
  the same mode again after a new plot selection would never reach the
  wizard.

## Value

Reactive containing selected mode string
