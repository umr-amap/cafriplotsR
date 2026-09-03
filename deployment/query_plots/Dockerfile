# Dockerfile for the Query Plots Shiny app (SSP Cloud).
#
# Build context = repository root, e.g.:
#   docker build -f deployment/query_plots/Dockerfile -t <user>/cafri-queryplots .
#
# Base image bundles R + shiny-server (listens on 3838). Pin a recent tag
# rather than :latest for reproducible deploys.
FROM rocker/shiny:4.4.2

# --- System libraries required by the R dependencies -------------------------
# libpq-dev          -> RPostgres
# libgdal/proj/geos  -> sf / leaflet (map tab, shapefile export)
# libudunits2-dev    -> sf units
# fontconfig/freetype/harfbuzz/fribidi/png/tiff/jpeg -> ragg/textshaping/svglite
#                       (kableExtra, ggplot2, plotly rendering)
# zip                -> utils::zip(), used by the CSV and shapefile downloads
#                       in mod_results_display. Not a compile-time dependency:
#                       it is the runtime binary R shells out to.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    zip \
  && rm -rf /var/lib/apt/lists/*

# --- Install the CafriplotsR package and its dependencies --------------------
# pak resolves and installs dependencies using pre-built Linux binaries where
# available, keeping the build fast.
#
# Only HARD dependencies (Depends/Imports/LinkingTo) are installed - NOT
# Suggests. The query-plots app runs entirely on Imports (shiny, DT, sf,
# leaflet, plotly, writexl, shinyjs, shiny.i18n, pool, ...), and the Suggests
# set pulls in heavy/optional packages (BIOMASS, taxize, chromote, shinytest2)
# plus an Additional_repositories drat that pak cannot solve in this image.
WORKDIR /build
COPY . /build

RUN R -q -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform[['pkgType']], R.Version()[['os']], R.Version()[['arch']]))" \
 && R -q -e "pak::local_install('.', dependencies = c('Depends', 'Imports', 'LinkingTo'), upgrade = FALSE)"

# --- Place the served entry point --------------------------------------------
# shiny-server serves whatever lives in /srv/shiny-server. We expose this app
# at the root path. The installed package already ships the entry point at
# inst/app/query_plots/app.R; copy it to the serving root.
RUN rm -rf /srv/shiny-server/* \
 && cp /build/inst/app/query_plots/app.R /srv/shiny-server/app.R \
 && chown -R shiny:shiny /srv/shiny-server

# Tell the app it is served (skip stopApp/cleanup on per-user disconnect).
ENV CAFRI_SERVED=true
ENV CAFRI_LANGUAGE=fr

EXPOSE 3838

# rocker/shiny's default CMD launches shiny-server; keep it.
