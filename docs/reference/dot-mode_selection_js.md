# The browser side of choosing an operation

The eight cards fill more than one screen, so a click at the top used to
be invisible from the bottom: the only confirmation was an alert below
the fold, and the Next button that goes with it further down still. This
marks the chosen card, fades the rest, and brings the confirmation and
the buttons into view when they are not already there.

## Usage

``` r
.mode_selection_js(card_ids, chosen_id)
```

## Arguments

- card_ids:

  Character vector of namespaced card element ids.

- chosen_id:

  The namespaced id of the card that was clicked.

## Value

A single string of JavaScript.

## Details

The scroll waits a moment so the confirmation alert is on the page
before the browser measures where to stop, and does nothing when the
buttons are already on screen — no motion the user did not need.
