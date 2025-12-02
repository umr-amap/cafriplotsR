# CafriplotsR <img src="man/figures/logo.png" align="right" height="139" />

> R package for managing and exploring the Central African forest plot database [cafriplot network](https://cafriplot.net/)

## Overview

`CafriplotsR` provides tools for querying a PostgreSQL database containing forest inventories 
data from Tropical Africa.
The package offers functions and shiny apps for (1) managing individual tree measurements on 
which either taxa or stem level traits _sensus largo_ measurements (or observations) can be 
aggregated, (2) standardizing taxonomic information en enrich with taxa level traits.   
The advantage of this package is allow managing inventories, traits and observations under 
the same taxonomic backbone, facilitating data integration, reproductibility in data analysis and
manipulation, data reusability.


**Key features:**
- Query plot data, individual tree measurements, and ecological features
- Access and aggregate species-level traits _sensus largo_
- Shiny app for standardize and correct your own list of taxonomic names

## Installation

```r
# Install from GitHub
install.packages(c("tidyverse", "dbplyr", "devtools"))
devtools::install_github("umr-amap/cafriplotsR", upgrade = "never")

```

In case of slow internet connection, the installation from github above may fail.
You may try to first launch this code line in the console, it will increase the time for trying to install :

```r
options(timeout = max(3000, getOption("timeout")))
```


**Note:** Access to the database is restricted and requires appropriate credentials.

## Package Logic & Access Control

The `CafriplotsR` package offers tools to **manipulate, export, visualize, standardize, and enrich** plant inventory data from Central Africa.

### Access Model

The package implements a **two-tier access system**:

1. **Plot inventories** (row-level security):
   - Each user has access to **their own plots**, controlled by database row-level 
   security policies
   - Policies define which specific plots each user can query and update
   - Ensures data providers maintain control over their contributed inventories
   - Some inventories are accessible to all users

2. **Species-level traits** (access across all users):
   - **All users** have read access to the taxa database
   - These data are grafted and aggregated to inventories
   

This design ensures data sovereignty for plot owners while enabling the research community 
to benefit from shared taxonomic and trait knowledge.


### Future Development

- **Species occurrence data**: Open access to occurrence records across Central Africa 
(not yet implemented). The RAINBIO database (only for shrub and trees) will be accessible and interoperable with inventories.

## Database Architecture

The package connects to two PostgreSQL databases:

1. **Main database** (`plots_transects`): Plot, subplot, and individual tree data
2. **Taxa database** (`rainbio`): Taxonomic information and species-level traits

### Quick Start

```r
library(CafriplotsR)

# Connect to databases
mydb <- call.mydb()
mydb_taxa <- call.mydb.taxa()

# Query plots
plots <- query_plots(id_plot = c(1, 2, 3))

# Query plots
plots <- query_plots(country = "GABON")

# Visualize database structure
get_database_fk(mydb)


```

## Core Functions

### Connection Management
- `call.mydb()` - Connect to main database
- `call.mydb.taxa()` - Connect to taxa database
- `cleanup_connections()` - Close all connections
- `db_diagnostic()` - Database connection diagnostics

### Data Querying
- `query_plots()` - Query plot metadata or individuals


## Documentation

- **Function help**: Use `?function_name` for detailed documentation
- **Changelog**: See [NEWS.md](NEWS.md) for version history and updates

## Recent Updates

See [NEWS.md](NEWS.md) for the latest changes, including:
- Breaking changes and migration guides
- New features and enhancements
- Bug fixes and improvements

## Package Metadata

- **Authors:** Gilles Dauby, Hugo Leblanc
- **Maintainer:** Gilles Dauby (gilles.dauby@ird.fr)
- **License:** GPL-2
- **Minimum R version:** 4.0

## Contributing

This package follows a git branching workflow:
- All code changes are made on feature branches
- Changes are documented in NEWS.md
- Pull requests are reviewed before merging to master


## Support

For issues, questions, or feature requests, contact the package maintainer.
