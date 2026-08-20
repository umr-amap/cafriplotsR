# Run a function against a real DBI connection

Accepts a pool or a plain connection and always hands \`fun\` a plain
one, returning a checked-out connection to the pool afterwards.

## Usage

``` r
.upd_with_con(con, fun)
```

## Arguments

- con:

  A \`Pool\` or a DBI connection.

- fun:

  Function of one argument (the connection).

## Value

Whatever \`fun\` returns.
