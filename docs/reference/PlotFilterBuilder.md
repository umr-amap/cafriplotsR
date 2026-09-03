# Query builder for plot

Allow building progressively a SQL query to filter plots following
different criteria using the builder pattern

## Details

A filter whose value matches nothing in the database adds an
unsatisfiable condition, so the query returns no plots and warns. It
does not drop the condition: an unmatched country silently returning
every plot would look like a successful query.

## Methods

- `new(connection)`:

  Initialize the builder with a database connection.

  - `connection`: A DBI connection object to the database

- `filter_country(country, interactive = FALSE)`:

  Filter plots by country name(s).

  - `country`: Character vector of country name(s)

  - `interactive`: Logical. If TRUE, uses .link_table for fuzzy matching

- `filter_plot_name(plot_name, interactive = FALSE)`:

  Filter plots by plot name(s).

  - `plot_name`: Character vector of plot name(s)

  - `interactive`: Logical. If TRUE, uses .link_table for fuzzy matching

- `filter_method(method, interactive = FALSE)`:

  Filter plots by method(s).

  - `method`: Character vector of method name(s)

  - `interactive`: Logical. If TRUE, uses .link_table for fuzzy matching

- `filter_locality(locality_name)`:

  Filter plots by locality name(s).

  - `locality_name`: Character vector of locality name(s)

- `filter_features(feature_filters, exact_match = FALSE)`:

  Filter plots by their features – rows of `data_liste_sub_plots` typed
  by `subplotype_list`, not columns of `data_liste_plots`. Each named
  feature adds a subquery, so different features are combined with AND
  while the values of one feature are combined with OR.

  - `feature_filters`: Named list, names being feature types

  - `exact_match`: Logical. If TRUE, match values exactly rather than as
    substrings

- `build(operator = "AND")`:

  Build the final SQL query.

  - `operator`: Character. Join operator between conditions ("AND" or
    "OR"). Default is "AND"

  Returns a SQL query object

- `build_with_or()`:

  Build the SQL query with OR operator between conditions. Returns a SQL
  query object

- `add_custom_condition(condition, wrap_parentheses = TRUE)`:

  Add a custom SQL condition.

  - `condition`: Character. Raw SQL condition string

  - `wrap_parentheses`: Logical. If TRUE, wraps condition in parentheses

- `print_conditions()`:

  Display current filter conditions (for debugging).

## Methods

### Public methods

- [`PlotFilterBuilder$new()`](#method-PlotFilterBuilder-initialize)

- [`PlotFilterBuilder$filter_country()`](#method-PlotFilterBuilder-filter_country)

- [`PlotFilterBuilder$filter_plot_name()`](#method-PlotFilterBuilder-filter_plot_name)

- [`PlotFilterBuilder$filter_method()`](#method-PlotFilterBuilder-filter_method)

- [`PlotFilterBuilder$filter_locality()`](#method-PlotFilterBuilder-filter_locality)

- [`PlotFilterBuilder$filter_features()`](#method-PlotFilterBuilder-filter_features)

- [`PlotFilterBuilder$build()`](#method-PlotFilterBuilder-build)

- [`PlotFilterBuilder$build_with_or()`](#method-PlotFilterBuilder-build_with_or)

- [`PlotFilterBuilder$add_custom_condition()`](#method-PlotFilterBuilder-add_custom_condition)

- [`PlotFilterBuilder$print_conditions()`](#method-PlotFilterBuilder-print_conditions)

- [`PlotFilterBuilder$clone()`](#method-PlotFilterBuilder-clone)

------------------------------------------------------------------------

### `PlotFilterBuilder$new()`

#### Usage

    PlotFilterBuilder$new(connection)

------------------------------------------------------------------------

### `PlotFilterBuilder$filter_country()`

#### Usage

    PlotFilterBuilder$filter_country(country, interactive = FALSE)

------------------------------------------------------------------------

### `PlotFilterBuilder$filter_plot_name()`

#### Usage

    PlotFilterBuilder$filter_plot_name(
      plot_name,
      interactive = FALSE,
      exact_match = FALSE
    )

------------------------------------------------------------------------

### `PlotFilterBuilder$filter_method()`

#### Usage

    PlotFilterBuilder$filter_method(method, interactive = FALSE)

------------------------------------------------------------------------

### `PlotFilterBuilder$filter_locality()`

#### Usage

    PlotFilterBuilder$filter_locality(locality_name)

------------------------------------------------------------------------

### `PlotFilterBuilder$filter_features()`

#### Usage

    PlotFilterBuilder$filter_features(feature_filters, exact_match = FALSE)

------------------------------------------------------------------------

### `PlotFilterBuilder$build()`

#### Usage

    PlotFilterBuilder$build(operator = "AND")

------------------------------------------------------------------------

### `PlotFilterBuilder$build_with_or()`

#### Usage

    PlotFilterBuilder$build_with_or()

------------------------------------------------------------------------

### `PlotFilterBuilder$add_custom_condition()`

#### Usage

    PlotFilterBuilder$add_custom_condition(condition, wrap_parentheses = TRUE)

------------------------------------------------------------------------

### `PlotFilterBuilder$print_conditions()`

#### Usage

    PlotFilterBuilder$print_conditions()

------------------------------------------------------------------------

### `PlotFilterBuilder$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PlotFilterBuilder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
query <- PlotFilterBuilder$new(con)$
  filter_country("Gabon")$
  filter_method("transect")$
  build()
} # }
```
