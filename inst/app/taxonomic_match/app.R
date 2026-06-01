# Entry point for serving the Taxonomic Name Standardization app under a
# hosted Shiny server (SSP Cloud / shiny-server).
#
# This file is NOT used for local interactive use - run
# launch_taxonomic_match_app() from R for that. shiny-server sources this
# file and expects the final expression to be a Shiny app object.
#
# The app authenticates each visitor through the in-app database login
# module (mod_database_login), so no database credentials are baked into the
# image. Visitors can use their own credentials, the public read-only user,
# or the offline cached backbone.

# Mark this process as "served" so the app does not call stopApp() /
# cleanup_connections() when an individual user closes their browser tab.
Sys.setenv(CAFRI_SERVED = "true")

library(CafriplotsR)

# Initial UI language; override at deploy time with CAFRI_LANGUAGE=en.
language <- Sys.getenv("CAFRI_LANGUAGE", "fr")

# app_taxonomic_match() builds and returns the shinyApp object (it does not
# call runApp()), which is exactly what shiny-server needs. It is an internal
# function, hence the ::: access.
CafriplotsR:::app_taxonomic_match(
  language = if (language %in% c("en", "fr")) language else "fr"
)
