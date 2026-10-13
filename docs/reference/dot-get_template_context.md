# Live Database Context for Templates (Internal Helper)

Collects what the template needs from the database, with a static
fallback for every piece so a template can always be produced.

## Usage

``` r
.get_template_context(con = NULL)
```

## Arguments

- con:

  Database connection, pool, or NULL

## Value

List: people (character), features (character), hierarchy (logical),
methods (character), countries (character); the last two are NULL when
unknown.
