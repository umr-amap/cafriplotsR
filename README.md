# plotsdatabase

> R package for exploring and updating the Central African forest plot database [cafriplot network](https://cafriplot.net/)

## Overview

`plotsdatabase` provides tools for querying, analyzing, and updating a PostgreSQL database containing forest plot and transect data from Central Africa. The package offers comprehensive functions for working with individual tree measurements, taxonomic traits, and ecological features across multiple research plots.

**Key features:**
- Query plot data, individual tree measurements, and ecological features
- Access and aggregate species-level taxonomic traits
- Update database tables with new measurements and observations
- Resolve taxonomic synonyms and link trait data across hierarchy levels
- Visualize database structure and relationships

## Installation

```r
# Install from GitHub
install.packages(c("tidyverse", "dbplyr", "devtools"))
devtools::install_github("gdauby/bdd_plots_central_africa")

```

In case of slow internet connection, the installation from github above may fail.
You may try to first launch this code line in the console, it will increase the time for trying to install :

```r
options(timeout = max(3000, getOption("timeout")))
```


**Note:** Access to the database is restricted and requires appropriate credentials.

## Package Logic & Access Control

The `plotsdatabase` package offers tools to **manipulate, export, visualize, standardize, and enrich** forest inventory data with species-level traits.

### Access Model

The package implements a **two-tier access system**:

1. **Plot inventories** (row-level security):
   - Each user has access to **their own plots only**, controlled by database row-level security policies
   - Policies define which specific plots each user can query and update
   - Ensures data providers maintain control over their contributed inventories

2. **Species-level traits** (universal access):
   - **All users** have read access to the taxa database
   - Shared trait repository includes wood density, leaf traits, growth forms, ecological guilds, etc.
   - Users can graft these traits onto their own inventory data

### Future Development

- **Species occurrence data**: Universal access to occurrence records across Central Africa (not yet implemented)

This design ensures data sovereignty for plot owners while enabling the research community to benefit from shared taxonomic and trait knowledge.

## Database Architecture

The package connects to two PostgreSQL databases:

1. **Main database** (`plots_transects`): Plot, subplot, and individual tree data
2. **Taxa database** (`rainbio`): Taxonomic information and species-level traits (READ-ONLY)

### Quick Start

```r
library(plotsdatabase)

# Connect to databases
mydb <- call.mydb()
mydb_taxa <- call.mydb.taxa()

# Visualize database structure
get_database_fk(mydb)

# Query plots
plots <- query_plots(plot_ids = c(1, 2, 3))

# Query taxa-level traits
traits <- query_taxa_traits(
  idtax = c(12345, 67890),
  format = "wide",
  add_taxa_info = TRUE
)

# Query individual tree features
indiv <- query_individual_features(
  plot_ids = 1,
  trait_ids = c(1, 2),
  format = "wide"
)
```

## Core Functions

### Connection Management
- `call.mydb()` - Connect to main database
- `call.mydb.taxa()` - Connect to taxa database
- `cleanup_connections()` - Close all connections
- `db_diagnostic()` - Database connection diagnostics

### Data Querying
- `query_plots()` - Query plot metadata
- `query_individual_features()` - Query individual tree measurements
- `query_taxa_traits()` - Query species-level traits
- `query_plot_features()` - Query plot-level features


### Utilities
- `get_database_fk()` - Visualize database schema and relationships

## Documentation

- **Function help**: Use `?function_name` for detailed documentation
- **Changelog**: See [NEWS.md](NEWS.md) for version history and updates

## Recent Updates

See [NEWS.md](NEWS.md) for the latest changes, including:
- Breaking changes and migration guides
- New features and enhancements
- Bug fixes and improvements

## Package Metadata

- **Version:** 1.0
- **Authors:** Gilles Dauby, Hugo Leblanc
- **Maintainer:** Gilles Dauby (gilles.dauby@ird.fr)
- **License:** GPL-2
- **Minimum R version:** 4.0

## Contributing

This package follows a git branching workflow:
- All code changes are made on feature branches
- Changes are documented in NEWS.md
- Pull requests are reviewed before merging to master

See `CLAUDE.md` for development workflow details.

## Support

For issues, questions, or feature requests, contact the package maintainer.
