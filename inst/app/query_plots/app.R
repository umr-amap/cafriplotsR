# Entry point for serving the Query Plots app under a hosted Shiny server
# (SSP Cloud / shiny-server).
#
# This file is NOT used for local interactive use - run
# launch_query_plots_app() from R for that. shiny-server sources this file
# and expects the final expression to be a Shiny app object.
#
# The app authenticates each visitor through the in-app database login
# module (mod_database_login), so no database credentials are baked into the
# image. Visitors use their own credentials or the public read-only user.

# Mark this process as "served" so the app does not call stopApp() /
# cleanup_connections() when an individual user closes their browser tab.
Sys.setenv(CAFRI_SERVED = "true")

library(CafriplotsR)

# Initial UI language; override at deploy time with CAFRI_LANGUAGE=en.
language <- Sys.getenv("CAFRI_LANGUAGE", "fr")

# shiny_app_query_plots() builds and returns the shinyApp object (it does not
# call runApp()), which is exactly what shiny-server needs. Called with no
# pool so the login module drives authentication per visitor.
CafriplotsR::shiny_app_query_plots(
  pool_main = NULL,
  language  = if (language %in% c("en", "fr")) language else "fr"
)
