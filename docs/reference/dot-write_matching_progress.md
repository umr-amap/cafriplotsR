# Publish progress for a polling parent process

Written to a temporary file and renamed into place, so a parent polling
mid-write reads the previous state rather than half a line of JSON.

## Usage

``` r
.write_matching_progress(
  progress_file,
  stage,
  i = NA_integer_,
  n = NA_integer_,
  name = NA_character_
)
```

## Arguments

- progress_file:

  Character path, or NULL to discard progress.

- stage:

  Character, one of "resume", "fuzzy_start", "fuzzy".

- i, n:

  Integer, position and total for the current stage.

- name:

  Character, the name being matched, if any.

## Value

Invisibly NULL.
