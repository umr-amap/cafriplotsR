# Fetch plot data

Class for extracting metadata from database

## Methods

### Public methods

- [`PlotFetcher$new()`](#method-PlotFetcher-initialize)

- [`PlotFetcher$fetch_by_ids()`](#method-PlotFetcher-fetch_by_ids)

- [`PlotFetcher$fetch_with_filter()`](#method-PlotFetcher-fetch_with_filter)

- [`PlotFetcher$clone()`](#method-PlotFetcher-clone)

------------------------------------------------------------------------

### `PlotFetcher$new()`

Initialiser le fetcher

#### Usage

    PlotFetcher$new(connection)

#### Arguments

- `connection`:

  Connexion DBI

------------------------------------------------------------------------

### `PlotFetcher$fetch_by_ids()`

Récupérer plots par IDs

#### Usage

    PlotFetcher$fetch_by_ids(plot_ids)

#### Arguments

- `plot_ids`:

  Vecteur d'IDs de plots

------------------------------------------------------------------------

### `PlotFetcher$fetch_with_filter()`

Récupérer plots avec filtre (requête SQL)

#### Usage

    PlotFetcher$fetch_with_filter(query)

#### Arguments

- `query`:

  Requête SQL construite par PlotFilterBuilder

------------------------------------------------------------------------

### `PlotFetcher$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PlotFetcher$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
