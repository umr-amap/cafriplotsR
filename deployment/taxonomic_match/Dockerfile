# Dockerfile for the Taxonomic Name Standardization Shiny app (SSP Cloud).
#
# Build context = repository root, e.g.:
#   docker build -f deployment/taxonomic_match/Dockerfile -t <user>/cafri-taxomatch .
#
# Base image bundles R + shiny-server (listens on 3838). Pin a recent tag
# rather than :latest for reproducible deploys.
FROM rocker/shiny:4.4.2

# --- System libraries required by the R dependencies -------------------------
# libpq-dev          -> RPostgres
# libgdal/proj/geos  -> sf / leaflet
# libudunits2-dev    -> sf units
# fontconfig/freetype/harfbuzz/fribidi/png/tiff/jpeg -> ragg/textshaping/svglite
#                       (kableExtra, ggplot2, plotly rendering)
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
  && rm -rf /var/lib/apt/lists/*

# --- Install the CafriplotsR package and its dependencies --------------------
# pak resolves and installs Imports (and their system requirements) using
# pre-built Linux binaries where available, which keeps the build fast.
WORKDIR /build
COPY . /build

RUN R -q -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform[['pkgType']], R.Version()[['os']], R.Version()[['arch']]))" \
 && R -q -e "pak::local_install('.', dependencies = TRUE, upgrade = FALSE)"

# --- Place the served entry point --------------------------------------------
# shiny-server serves whatever lives in /srv/shiny-server. We expose this app
# at the root path. The installed package already ships the entry point at
# inst/app/taxonomic_match/app.R; copy it to the serving root.
RUN rm -rf /srv/shiny-server/* \
 && cp /build/inst/app/taxonomic_match/app.R /srv/shiny-server/app.R \
 && chown -R shiny:shiny /srv/shiny-server

# Tell the app it is served (skip stopApp/cleanup on per-user disconnect).
ENV CAFRI_SERVED=true
ENV CAFRI_LANGUAGE=fr

EXPOSE 3838

# rocker/shiny's default CMD launches shiny-server; keep it.
