# Run a console-oriented query function without letting it reach the UI

\`query_plots()\` is written for a console session: it prints, and some
of its branches print an htmlwidget. Inside an app that takes over the
RStudio pane the app itself is running in, which reads as a freeze.
Printed output is swallowed and the viewer is pointed at nothing for the
duration of the call; messages are left alone, so the console still says
what the query did.

## Usage

``` r
.upd_quiet_query(expr)
```

## Arguments

- expr:

  Expression to evaluate.

## Value

The value of \`expr\`.
