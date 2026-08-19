# Choose from prompt

Show prompt in console to choose which directions to take

## Usage

``` r
choose_prompt(choices_vec = c("Yes", "No", "Cancel"), message = "")
```

## Arguments

- choices_vec:

  Optional. A character vector of choices.

- message:

  Optional. A single string for the prompt message.

## Value

A logical value: \`TRUE\` for the first choice, \`FALSE\` for the second
choice, and \`NA\` for the third choice. For invalid selections, the
function will print an error message but may return an unexpected value.
