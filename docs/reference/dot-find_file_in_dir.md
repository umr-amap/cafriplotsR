# Find a file in a directory by glob pattern and extension

Find a file in a directory by glob pattern and extension

## Usage

``` r
.find_file_in_dir(dir, pattern, ext = NULL, label = "file", required = TRUE)
```

## Arguments

- dir:

  Directory path

- pattern:

  Glob-style pattern (e.g. "arbre\*", "code_light\*")

- ext:

  File extension without dot (e.g. "xlsx", "csv")

- label:

  Human-readable label for messages

- required:

  If TRUE, stop with an error when not found

## Value

File path, or NULL if not found and not required
