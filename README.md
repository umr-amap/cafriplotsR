# CafriplotsR <img src="man/figures/logo.png" align="right" height="139" />

[![codecov](https://codecov.io/gh/umr-amap/cafriplotsR/branch/master/graph/badge.svg)](https://codecov.io/gh/umr-amap/cafriplotsR)


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

---

**[Version française disponible ici / French version available here](README.fr.md)**

---

## Why CafriplotsR?

### The Challenge

Many researchers inventory woody vegetation in Central African forests (CAF), targeting diverse objectives: dynamics (mortality and growth), floristic and functional diversity, resource assessment and management, effects of both historical and contemporary disturbances, fauna-flora interactions, and more.

However, these initiatives and the research groups conducting them suffer from **insufficient visibility**:

- **Within the regional community**: Limited visibility among scientists and managers working on these forests restricts collaboration opportunities, experience sharing, protocol harmonization, and identification of complementarities in data and expertise.

- **At the international level**: This leads to the frequent claim that "we know almost nothing about Congo Basin forests." While there are indeed knowledge gaps compared to other major tropical forest blocks, asserting that our understanding relies solely on a handful of visible international initiatives is reductive.

### The Data Accessibility Problem

Unlike species occurrence data, which has become increasingly accessible (e.g., through GBIF), it remains difficult to get a comprehensive view of inventory data in Central Africa. This includes both recent inventories and 'historical' inventories dating back decades. These historical inventories sometimes exist only in paper format (when they haven't disappeared entirely!), yet they document plant biodiversity in localities that may have become inaccessible.

**Root causes include:**
1. Poor data archiving practices
2. Lack of resources to maintain accessibility beyond project lifetimes
3. Insufficient willingness to make data accessible

### The Integration Challenge

Another major difficulty is **combining different data types** (e.g., species conservation status, functional traits, etc.) with inventory data, even though this compilation is essential for investigating numerous research questions. These compilations are regularly performed, but the methods lack reproducibility. If 10 people were asked to perform such a compilation independently, we would likely get 10 different results, depending on:

- Data accessible at the time of compilation (which varies greatly with each person's context)
- How taxonomy is standardized between databases

### The CafriplotsR Solution

CafriplotsR addresses these challenges through **shared infrastructure and inventory data management** while **guaranteeing data sovereignty** for each user or research group.

**The package aims to:**
- Improve visibility of fieldwork conducted by different teams in Central Africa
- Facilitate data management (encoding, cleaning, consolidation, queries, etc.)
- Improve documentation and reproducibility of data processing
- Boost scientific collaborations in the region through controlled and deliberate data sharing

### How CafriplotsR Differs from Global Initiatives

While comparable to other data 'centralization' initiatives with global approaches (e.g., ForestPlots.net), CafriplotsR distinguishes itself through:

1. **Regional, not global**: Focuses on Central Africa—a geographic and human scale that enables interactions between actors involved in collecting, managing, and using this reference data

2. **Transparent multi-data management**: Manages different types of data associated with woody plant species (occurrences, traits, relevant attributes) in a transparent manner

3. **Data sovereignty over strict centralization**: Each user remains sovereign in managing their data. CafriplotsR aims to federate research groups involved in woody inventories in Central Africa, not to centralize control

### Next Steps

With the goal of making data import, management, and standardization accessible through interactive and user-friendly applications, the next development steps involve **co-construction** to identify and respond to the concrete needs of potential users in Central Africa.

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

## Herbarium Specimen Linking: Improving Data Quality Through Time

**A unique feature for long-term data quality improvement**

Field identifications in forest inventories, while valuable, often suffer from taxonomic uncertainty. Botanical specimens collected from the same individual trees and deposited in herbaria undergo expert taxonomic revision over time, resulting in more accurate identifications. However, this improved knowledge typically remains isolated in herbarium databases, disconnected from the ecological inventory data.

**The CafriplotsR solution: Formal specimen-individual links**

This package implements a **specimen linking system** that creates formal, persistent connections between:
- Individual trees in forest inventories (with their ecological measurements)
- Herbarium specimens collected from those same individuals (with their expert-revised taxonomy)

**Key advantages:**

1. **Automatic taxonomic updates**: When a specimen's identification is revised by taxonomists, the linked inventory individual automatically inherits the updated taxonomy. No manual re-identification needed.

2. **Improved data quality over time**: Your inventory data becomes progressively more accurate as specimen identifications are refined, without requiring field revisits or additional effort.

3. **Traceability**: Each inventory record maintains a clear link to its voucher specimen, providing scientific evidence and enabling verification.

4. **Taxonomic confidence**: Distinguish between field identifications (subject to uncertainty) and specimen-backed identifications (expert-verified).

5. **Data longevity**: Inventory data remains connected to the evolving taxonomic knowledge, ensuring long-term scientific value.

📖 **For detailed instructions on how to link specimens to individuals**, see the vignette: [Linking Herbarium Specimens to Inventory Individuals](vignettes/specimen_linking_workflow.Rmd)

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

- **Authors:** Gilles Dauby, Hugo Leblanc, Pierre Ploton
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

## Citation

To cite CafriplotsR in publications, use:

```r
citation("CafriplotsR")
```

Or manually:

> Dauby, G., Leblanc, H., & Ploton, P. (2024). CafriplotsR: Tools for Exploring, Managing and Standardizing Vegetation Inventories in Central Africa. R package version 1.8.0. https://umr-amap.github.io/cafriplotsR/

BibTeX entry:

```bibtex
@Manual{cafriplotsr,
  title = {CafriplotsR: Tools for Exploring, Managing and Standardizing Vegetation Inventories in Central Africa},
  author = {Gilles Dauby and Hugo Leblanc and Pierre Ploton},
  year = {2024},
  note = {R package version 1.8.0},
  url = {https://umr-amap.github.io/cafriplotsR/}
}
```
