# Is the app being served by a Shiny server?

Distinguishes a hosted deployment (shiny-server, ShinyProxy, SSP Cloud)
from a local interactive launch via one of the \`launch\_\*()\`
wrappers.

## Usage

``` r
.is_served()
```

## Value

Logical scalar. \`TRUE\` when running under a hosted server.

## Details

On a hosted server a single R process serves many concurrent user
sessions, so a session ending must NOT call \[shiny::stopApp()\] or the
global \[cleanup_connections()\] - doing so would tear the process (and
every other user's session) down. Locally the opposite is desired:
closing the browser tab should free the connection and return the R
console.

Detection is driven by the \`CAFRI_SERVED\` environment variable, which
the deployment entry point (\`inst/app/\*/app.R\`) and the Dockerfile
set to \`"true"\`. \`SHINY_SERVER_VERSION\` (exported by shiny-server)
is used as a fallback so the guard still holds if the variable is ever
missed.
